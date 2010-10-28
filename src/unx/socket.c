/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (C): 1985-2002, University of Amsterdam

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

#ifdef __CYGWIN__
#define __USE_W32_SOCKETS
#define HAVE_WINSOCK 1
#endif

#include <md.h>				/* get HAVE_'s */
#if defined(HAVE_SOCKET) || defined(HAVE_WINSOCK)

#include <sys/types.h>
#include <memory.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_WINSOCK
#include "mswinsock.h"
#include <io.h>
#include <fcntl.h>
typedef size_t socklen_t;
#else /*HAVE_WINSOCK*/

#define HAVE_SYS_UN_H 1
#define UNIX_DOMAIN_SOCKETS 1
#include <sys/socket.h>			/* must be first to avoid send() */
#include <netdb.h>			/* conflict ... */
#include <netinet/in.h>
#ifdef HAVE_SYS_SOCKETVAR_H
#include <sys/socketvar.h>
#endif
#include <errno.h>
extern int errno;
#undef NAME_MAX				/* conflict */

#define SOCKET int
#ifndef HAVE_SOCKLEN_T
#define socklen_t size_t
#endif
#endif /*WINSOCK*/

#define create PCEcreate		/* avoid conflict */

#include <h/kernel.h>
#include <h/unix.h>
#include <h/interface.h>
#if defined(HAVE_SYS_TIME_H) && !defined(__USE_W32_SOCKETS)
#include <sys/time.h>
#endif
#ifdef HAVE_SYS_UN_H
#include <sys/un.h>
#endif
#include <signal.h>

#define MAX_UN_ADDRESS_LEN (sizeof(struct sockaddr_un) - sizeof(short))

#ifndef INVALID_SOCKET
#define INVALID_SOCKET -1
#endif

static status	closeSocket(Socket);

#ifdef HAVE_WINSOCK
#define NO_WINERR 1			/* there really isn't a way !!!! */
#ifdef NO_WINERR

typedef struct
{ int	 id;
  char  *description;
} wsock_err;

static wsock_err wsock_err_list[] =
{ { WSAEACCES, "Permission denied" },
  { WSAEADDRINUSE, "Address already in use" },
  { WSAEADDRNOTAVAIL, "Cannot assign requested address" },
  { WSAEAFNOSUPPORT, "Address family not supported by protocol family" },
  { WSAEALREADY, "Operation already in progress" },
  { WSAECONNABORTED, "Software caused connection abort" },
  { WSAECONNREFUSED, "Connection refused" },
  { WSAECONNRESET, "Connection reset by peer" },
  { WSAEDESTADDRREQ, "Destination address required" },
  { WSAEFAULT, "Bad address" },
  { WSAEHOSTDOWN, "Host is down" },
  { WSAEHOSTUNREACH, "No route to host" },
  { WSAEINPROGRESS, "Operation now in progress" },
  { WSAEINTR, "Interrupted function call" },
  { WSAEINVAL, "Invalid argument" },
  { WSAEISCONN, "Socket is already connected" },
  { WSAEMFILE, "Too many open files" },
  { WSAEMSGSIZE, "Message too long" },
  { WSAENETDOWN, "Network is down" },
  { WSAENETRESET, "Network dropped connection on reset" },
  { WSAENETUNREACH, "Network is unreachable" },
  { WSAENOBUFS, "No buffer space available" },
  { WSAENOPROTOOPT, "Bad protocol option" },
  { WSAENOTCONN, "Socket is not connected" },
  { WSAENOTSOCK, "Socket operation on non-socket" },
  { WSAEOPNOTSUPP, "Operation not supported" },
  { WSAEPFNOSUPPORT, "Protocol family not supported" },
  { WSAEPROCLIM, "Too many processes" },
  { WSAEPROTONOSUPPORT, "Protocol not supported" },
  { WSAEPROTOTYPE, "Protocol wrong type for socket" },
  { WSAESHUTDOWN, "Cannot send after socket shutdown" },
  { WSAESOCKTNOSUPPORT, "Socket type not supported" },
  { WSAETIMEDOUT, "Connection timed out" },
  { WSAEWOULDBLOCK, "Resource temporarily unavailable" },
  { WSAEDISCON, "Graceful shutdown in progress" },
  { 0, NULL }
};
#endif /*NO_WINERR*/


Name
SockError()
{
#ifdef NO_WINERR
  int err = WSAGetLastError();
  wsock_err *e = wsock_err_list;

  if ( !err )
    return (Name) NIL;

  for( ; e->description; e++ )
  { if ( e->id == err )
      return CtoName(e->description);
  }

  return WinStrError(err);
#else
  return WinStrError(WSAGetLastError());
#endif
}

#define SocketHandle(s) ((SOCKET)((s)->ws_ref))

#else /*HAVE_WINSOCK*/
#define SocketHandle(s) ((s->rdfd))
#define SockError() getOsErrorPce(PCE)
#endif /*HAVE_WINSOCK*/

static Chain	SocketChain;		/* Available open sockets */

static void
closeAllSockets(int status)
{ Socket s;

  for_chain(SocketChain, s, closeSocket(s));

#ifdef WINSOCK
  WSACleanup();
#endif
}


static void
sigPipeSocket(void)
{ errorPce(PCE, NAME_brokenPipe, 0);
}


static void
setupSockets(void)
{ static int initialised = 0;

  if ( !initialised )
  {
#ifdef HAVE_WINSOCK
    WSADATA data;
#ifdef USE_WINSOCK2
    WORD wversion = MAKEWORD(2, 0);
#else
    WORD wversion = MAKEWORD(1, 1);
#endif

    if ( WSAStartup(wversion, &data) != 0 )
      errorPce(NIL, NAME_socket, NAME_initialise, SockError());

    DEBUG(NAME_socket,
	  Cprintf("WSAStartup(): wVersion = %d.%d, wHighVersion = %d.%d\n",
		  data.wVersion >> 8, data.wVersion & 0xff,
		  data.wHighVersion >> 8, data.wHighVersion & 0xff);
	  Cprintf("Description: %s\n", data.szDescription);
	  Cprintf("Status:      %s\n", data.szSystemStatus);
	 );
#endif

    at_pce_exit(closeAllSockets, ATEXIT_FIFO);

#ifdef SIGPIPE
    hostAction(HOST_SIGNAL, SIGPIPE, sigPipeSocket);
#endif

    initialised++;
  }
}


static status
initialiseSocket(Socket s, Any address, Name domain)
{ setupSockets();

  initialiseStream((Stream) s, NIL, NIL, NIL, DEFAULT);
#ifdef HAVE_WINSOCK
  s->ws_ref = (WsRef) INVALID_SOCKET;
#endif

  if ( isDefault(domain) )
  { if ( instanceOfObject(address, ClassFile) )
      domain = NAME_unix;
    else if ( instanceOfObject(address, ClassTuple) || isInteger(address) )
      domain = NAME_inet;
    else
      return errorPce(s, NAME_noDomain);
  }

  assign(s, domain,	    domain);
  assign(s, address,	    address);
  assign(s, status, 	    NAME_idle);

  succeed;
}


static void
registerSocket(Socket s)		/* do not influence GC */
{ unsigned long flags = s->flags;
  unsigned long refs  = s->references;

  appendChain(SocketChain, s);
  s->flags      = flags;
  s->references = refs;
}


static void
unregisterSocket(Socket s)
{ unsigned long flags = s->flags;
  unsigned long refs  = s->references;

  addCodeReference(s);			/* avoid drop-out */
  deleteChain(SocketChain, s);

  s->flags      = flags;
  s->references = refs;
}


static status
registerClientSocket(Socket s, Socket client)
{ unsigned long flags = s->flags;
  unsigned long refs  = s->references;

  appendChain(s->clients, client);
  assign(client, master, s);
  s->flags      = flags;
  s->references = refs;

  succeed;
}


static status
unregisterClientSocket(Socket s, Socket client)
{ unsigned long flags = s->flags;
  unsigned long refs  = s->references;

  addCodeReference(s);			/* avoid drop-out */
  if ( notNil(s->clients) )
    deleteChain(s->clients, client);
  assign(client, master, NIL);
  s->flags      = flags;
  s->references = refs;

  succeed;
}


static status
unlinkSocket(Socket s)
{ return closeSocket(s);
}


static status
cloneSocket(Socket s, Socket clone)
{ clonePceSlots(s, clone);

  clone->rdfd = clone->wrfd = -1;
#ifdef HAVE_WINSOCK
  clone->ws_ref = (WsRef)INVALID_SOCKET;
#else
  clone->ws_ref = 0;
#endif
  clone->input_buffer = NULL;
  clone->input_allocated = s->input_p = 0;

  succeed;
}

		 /*******************************
		 *	     PRINT NAME		*
		 *******************************/

static StringObj
getPrintNameSocket(Socket s)
{ string tmp;
  StringObj sobj;
  Any av[3];
  Name fmt;
  int an;

  av[0] = getClassNameObject(s);

  if ( instanceOfObject(s->address, ClassTuple) )
  { Tuple t = (Tuple) s->address;

    av[1] = t->first;
    av[2] = t->second;
    an = 3;
    fmt = CtoName("%s(%s:%d)");
  } else
  { av[1] = get(s->address, NAME_printName, EAV);
    an = 2;
    fmt = CtoName("%s(%s)");
  }

  str_writefv(&tmp, (CharArray) fmt, an, av);
  sobj = StringToString(&tmp);
  str_unalloc(&tmp);

  answer(sobj);
}


		 /*******************************
		 *      CREATE/CONNECT/ETC	*
		 *******************************/

static status
createSocket(Socket s)
{ if ( SocketHandle(s) == INVALID_SOCKET )
  { int domain;

    closeSocket(s);

    if ( s->domain == NAME_unix )
    {
#ifdef UNIX_DOMAIN_SOCKETS
      domain = PF_UNIX;
#else
      return errorPce(s, NAME_noSocketDomain, NAME_unix);
#endif
    } else /*if ( s->domain == NAME_inet )*/
      domain = PF_INET;

#ifdef HAVE_WINSOCK
  { SOCKET mss;

    if ( (mss=socket(domain, SOCK_STREAM, 0)) == INVALID_SOCKET )
      return errorPce(s, NAME_socket, NAME_create, SockError());

    s->ws_ref = (WsRef) mss;
  }
#else
    if ( (s->rdfd = s->wrfd = socket(domain, SOCK_STREAM, 0)) < 0 )
      return errorPce(s, NAME_socket, NAME_create, SockError());
#endif
  }

  succeed;
}


#ifdef UNIX_DOMAIN_SOCKETS
static status
unix_address_socket(Socket s, struct sockaddr_un *address, unsigned int *len)
{ Name name = getOsNameFile((FileObj) s->address);
  char *path;

  if ( !name )
    fail;

  path = strName(name);
  address->sun_family = PF_UNIX;
  *len = strlen(path)+1;
  if ( *len > MAX_UN_ADDRESS_LEN )
    return errorPce(s, NAME_socket, NAME_address, CtoName("Name too long"));

  memcpy(address->sun_path, path, *len);
  *len += sizeof(address->sun_family);

  succeed;
}
#endif /*UNIX_DOMAIN_SOCKETS*/


static status
inet_address_socket(Socket s, struct sockaddr_in *address, int *len)
{ memset(address, 0, sizeof(*address));
  *len = sizeof(*address);
  address->sin_family = PF_INET;

  if ( instanceOfObject(s->address, ClassTuple) ) /* client */
  { CharArray hostname;
    Int port;
    struct hostent *hp;
    Tuple t = s->address;

    if ( !(hostname = checkType(t->first, TypeName, NIL)) )
      return errorPce(t->first, NAME_unexpectedType, TypeName);
    if ( !(port = checkType(t->second, TypeInt, NIL)) )
      return errorPce(t->second, NAME_unexpectedType, TypeInt);

    if ( !(hp = gethostbyname(strName(hostname))) )
      return errorPce(s, NAME_noHost, hostname);

    address->sin_port = htons((unsigned short)valInt(port));
    memcpy(&address->sin_addr, hp->h_addr, hp->h_length);
  } else if ( isInteger(s->address) )	/* server */
  { address->sin_port = htons((unsigned short)valInt(s->address));
    address->sin_addr.s_addr = htonl(INADDR_ANY);
  } else
    return errorPce(s->address, NAME_unexpectedType, CtoType("tuple"));

  succeed;
}


static status
bindSocket(Socket s, BoolObj reuse)
{ int rval;

  TRY(createSocket(s));

#ifdef UNIX_DOMAIN_SOCKETS
  if ( s->domain == NAME_unix )
  { struct sockaddr_un address;
    unsigned int len;
    TRY( unix_address_socket(s, &address, &len) );
    rval = bind(SocketHandle(s), (struct sockaddr *) &address, len);
  } else /*if ( s->domain == NAME_inet )*/
#endif
  { struct sockaddr_in address;
    int len;
    TRY( inet_address_socket(s, &address, &len) );

#if defined(SOL_SOCKET) && defined(SO_REUSEADDR)
    if ( reuse == ON )
    { int status = 1;

      DEBUG(NAME_socket, Cprintf("Setting SO_REUSEADDR\n"));
      if ( setsockopt(SocketHandle(s), SOL_SOCKET, SO_REUSEADDR,
		      (char *) &status, sizeof(int)) < 0 )
	return errorPce(s, NAME_socket, NAME_setsockopt, SockError());
    }
#endif

    if ( (rval = bind(SocketHandle(s), (struct sockaddr *) &address, len))==0 )
    { if ( s->address == ZERO ||
	   (instanceOfObject(s->address, ClassTuple) &&
	    ((Tuple)s->address)->second == ZERO) )
      { struct sockaddr_in addr;
#ifdef __WINDOWS__
	DWORD len = sizeof(addr);
#else
	socklen_t len = sizeof(addr);
#endif

	if ( getsockname(SocketHandle(s), (struct sockaddr *) &addr, &len) )
	{ return errorPce(s, NAME_socket, NAME_getsockname, SockError());
	} else
	{ if ( s->address == ZERO )
	    assign(s, address, toInt(ntohs(addr.sin_port)));
	  else
	    assign((Tuple)s->address, second, toInt(ntohs(addr.sin_port)));
	}
      }
    }
  }

  if ( rval != 0 )
    return errorPce(s, NAME_socket, NAME_bind, SockError());

  succeed;
}


status
acceptSocket(Socket s)
{ SOCKET id2;
  Socket s2;
  Any client_address;

#ifdef UNIX_DOMAIN_SOCKETS
  if ( s->domain == NAME_unix )
  { struct sockaddr_un address;
    socklen_t len = sizeof(address);

    if ( (id2=accept(SocketHandle(s), (struct sockaddr *) &address, &len))<0 )
      errorPce(s, NAME_socket, NAME_accept, SockError());

    client_address = s->address;
  } else /*if ( s->domain == NAME_inet )*/
#endif
  { struct sockaddr_in address;
#ifdef __WINDOWS__
    DWORD len = sizeof(address);
#else
    socklen_t len = sizeof(address);
#endif

    if ( (id2=accept(SocketHandle(s), (struct sockaddr *) &address, &len))<0 )
      errorPce(s, NAME_socket, NAME_accept, SockError());

    { struct hostent *hp;

      if ( (hp = gethostbyaddr((char *)&address.sin_addr,
			       sizeof(address.sin_addr),
			       AF_INET)) )
	client_address = newObject(ClassTuple,
				   CtoName(hp->h_name),
				   toInt(address.sin_port),
				   EAV);
      else
	client_address = NIL;
    }
  }

  if ( !(s2 = get(s, NAME_clone, EAV)) )
    return errorPce(s, NAME_failedToClone);

#ifdef HAVE_WINSOCK
  s2->ws_ref = (WsRef) id2;
  s2->rdfd = s2->wrfd = 0;		/* signal open status */
#else
  s2->rdfd = s2->wrfd = id2;
#endif
  assign(s2, input_message, s->input_message);
  assign(s2, status, NAME_accepted);
  registerClientSocket(s, s2);
  inputStream((Stream) s2, DEFAULT);

  if ( notNil(s->accept_message) )
    forwardReceiverCode(s->accept_message, s, s2, EAV);

  succeed;
}


static status
listenSocket(Socket s, Code accept_message, Int backlog, BoolObj reuse)
{ if ( isDefault(backlog) )
    backlog = toInt(5);

  TRY(bindSocket(s, reuse));

  if ( listen(SocketHandle(s), valInt(backlog)) )
    return errorPce(s, NAME_socket, NAME_listen, SockError());

  assign(s, status, NAME_listen);
  if ( notDefault(accept_message) )
    assign(s, accept_message, accept_message);
  assign(s, clients, newObject(ClassChain, EAV));
  registerSocket(s);

#ifndef RANDOM
#define RANDOM() rand()
#endif

  if ( notNil(s->authority) )
  { Int passwd = toInt(RANDOM());

    TRY(openFile(s->authority, NAME_write, DEFAULT, DEFAULT));

    if ( instanceOfObject(s->address, ClassFile) )
    { send(s->authority, NAME_format, CtoName("unix %s\n%d\n"),
	   getAbsolutePathFile(s->address));
    } else if ( instanceOfObject(s->address, ClassTuple) )
    { Tuple t = s->address;

      send(s->authority, NAME_format, CtoName("inet %s:%d\n%d\n"),
	   t->first, t->second, passwd, EAV);
    } else
    { send(s->authority, NAME_format, CtoName("inet %s:%d\n%d\n"),
	   getHostnamePce(PCE), s->address, passwd, EAV);
    }

    TRY(closeFile(s->authority));
  }

  openDisplay(CurrentDisplay(NIL));
  ws_listen_socket(s);

  succeed;
}


static status
connectSocket(Socket s)
{ int rval;

  if ( s->status == NAME_connected )
    succeed;

  TRY(createSocket(s));

#ifdef UNIX_DOMAIN_SOCKETS
  if ( s->domain == NAME_unix )
  { struct sockaddr_un address;
    unsigned int len;
    TRY( unix_address_socket(s, &address, &len) );
    rval = connect(SocketHandle(s), (struct sockaddr *) &address, len);
  } else /*if ( s->domain == NAME_inet )*/
#endif
  { struct sockaddr_in address;
    int len;
    TRY( inet_address_socket(s, &address, &len) );
    rval = connect(SocketHandle(s), (struct sockaddr *) &address, len);
  }

  if ( rval )
    return errorPce(s, NAME_socket, NAME_connect, SockError());

#ifdef HAVE_WINSOCK
  s->rdfd = s->wrfd = 0;		/* signal open status */
#endif

  assign(s, status, NAME_connected);
  registerSocket(s);

  openDisplay(CurrentDisplay(NIL));
  inputStream((Stream)s, DEFAULT);

  succeed;
}


static Any
getPeerNameSocket(Socket s)
{
#ifdef UNIX_DOMAIN_SOCKETS
  if ( s->domain == NAME_unix )
  { struct sockaddr_un *address;
    char buf[MAX_UN_ADDRESS_LEN + sizeof(address->sun_family)];
    socklen_t len;

    address = (struct sockaddr_un *) buf;
    len = sizeof(buf);

    if ( getpeername(SocketHandle(s), (struct sockaddr *) address, &len) < 0 )
    { errorPce(s, NAME_socket, NAME_peerName, SockError());
      fail;
    }

    answer(CtoName(address->sun_path));
  } else /* if ( s->domain = NAME_inet ) */
#endif /*UNIX_DOMAIN_SOCKETS*/
  { struct sockaddr_in address;
#ifdef __WINDOWS__
    DWORD len = sizeof(address);
#else
    socklen_t len = sizeof(address);
#endif
    int port;
    unsigned long addr;
    char aname[3*4+4];

    if ( getpeername(SocketHandle(s), (struct sockaddr *) &address, &len) < 0 )
    { errorPce(s, NAME_socket, NAME_peerName, SockError());
      fail;
    }

    port = address.sin_port;
    addr = htonl(address.sin_addr.s_addr); /* TBD */
    sprintf(aname, "%d.%d.%d.%d",
	    (int)((addr >> 24) & 0xff),
	    (int)((addr >> 16) & 0xff),
	    (int)((addr >>  8) & 0xff),
	    (int) (addr        & 0xff));

    answer(answerObject(ClassTuple, CtoName(aname), toInt(port), EAV));
  }
}


		 /*******************************
		 *	    EXCEPTIONS		*
		 *******************************/

static status
eofSocket(Socket s)
{ if ( s->status == NAME_accepted )
    freeObject(s);

  succeed;
}


static status
brokenPipeSocket(Socket s)
{ succeed;
}


static status
closeSocket(Socket s)
{ closeStream((Stream) s);

  if ( notNil(s->clients) )		/* destroy clients */
  { Socket client;

    for_chain(s->clients, client, closeSocket(client));
  }

  if ( notNil(s->master) )		/* delete client <->master link */
    unregisterClientSocket(s->master, s);

  if ( s->domain == NAME_unix &&	/* remove from file-system */
       s->status == NAME_listen )
    removeFile(s->address);

  assign(s, status, NAME_idle);
  unregisterSocket(s);

  succeed;
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_listen[] =
        { "accept_message=[code]*", "backlog=[{1..5}]", "reuse=[bool]" };
static char *T_initialise[] =
        { "address=file|tuple|int*", "domain=[{unix,inet}]" };

/* Instance Variables */

static vardecl var_socket[] =
{ IV(NAME_address, "file|tuple|int*", IV_GET,
     NAME_address, "Address for the connection end-point"),
  IV(NAME_domain, "{unix,inet}", IV_GET,
     NAME_address, "Domain for the connection"),
  IV(NAME_status, "{idle,listen,accepted,connected}", IV_GET,
     NAME_status, "Status of the associated socket"),
  IV(NAME_acceptMessage, "code*", IV_BOTH,
     NAME_connect, "Message to accept new client"),
  IV(NAME_clients, "chain*", IV_GET,
     NAME_server, "Chain with accepted connections"),
  IV(NAME_master, "socket*", IV_GET,
     NAME_server, "Socket I accepted a connection for"),
  IV(NAME_authority, "file*", IV_GET,
     NAME_authority, "Name of authority-file (if any)")
};

/* Send Methods */

static senddecl send_socket[] =
{ SM(NAME_initialise, 2, T_initialise, initialiseSocket,
     DEFAULT, "Create socket from address and domain"),
  SM(NAME_unlink, 0, NULL, unlinkSocket,
     DEFAULT, "Cleanup socket"),
  SM(NAME_connect, 0, NULL, connectSocket,
     NAME_connect, "Connect with server socket"),
  SM(NAME_listen, 3, T_listen, listenSocket,
     NAME_connect, "Listen for connection requests]"),
  SM(NAME_brokenPipe, 0, NULL, brokenPipeSocket,
     NAME_control, "Attempt to write on broken connection"),
  SM(NAME_endOfFile, 0, NULL, eofSocket,
     NAME_input, "EOF read"),
  SM(NAME_close, 0, NULL, closeSocket,
     NAME_output, "Close communication to socket")
};

/* Get Methods */

static getdecl get_socket[] =
{ GM(NAME_printName, 0, "string", NULL, getPrintNameSocket,
     DEFAULT, "returns <classname>(<address>)"),
  GM(NAME_peerName, 0, "name|tuple", NULL, getPeerNameSocket,
     NAME_address, "Description of the other-sides address")
};

/* Resources */

#define rc_socket NULL
/*
static classvardecl rc_socket[] =
{
};
*/

/* Class Declaration */

static Name socket_termnames[] = { NAME_address };

ClassDecl(socket_decls,
          var_socket, send_socket, get_socket, rc_socket,
          1, socket_termnames,
          "$Rev$");

status
makeClassSocket(Class class)
{ declareClass(class, &socket_decls);

  setCloneFunctionClass(class, cloneSocket);
  cloneStyleVariableClass(class, NAME_clients, NAME_nil);
  cloneStyleVariableClass(class, NAME_master, NAME_nil);

#ifdef HAVE_SOCKET
  featureClass(class, NAME_unixDomain, ON);
#endif
  featureClass(class, NAME_inetDomain, ON);

  SocketChain = globalObject(NAME_openSockets, ClassChain, EAV);

  succeed;
}

#endif /*HAVE_SOCKET*/
