/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  1995-2014, University of Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

#include "include.h"
#include <h/unix.h>
#include <h/interface.h>
#undef send
#define send sendPCE
#include <process.h>

#ifndef SD_RECEIVE
#define SD_RECEIVE 0
#define SD_SEND 1
#define SD_BOTH 2
#endif

#define WM_SOCKET	 (WM_WINEXIT+1)
#define WM_PROCESS_INPUT (WM_WINEXIT+2)
#define WM_PROCESS_EXIT  (WM_WINEXIT+3)

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module implements the low-level part of class socket, a subclass of
class stream built on top of the asynchronous WinSock layer. We need the
asynchronous one for two reasons. First,  unlike the X11 system, Windows
can't listen to a synchronous socket and   messages at the same time and
second it turns out their synchronous version is unstable at best.

Unfortunately it fits poorly in the stream design used for XPCE sockets
and the socket layer doesn't really behave as advertised.

One of the problems is discarding a   socket  due to ->free. We shutdown
the connection and XPCE requires us to  discard the object. The FD_CLOSE
resulting from the shutdown  however  comes   later.  The  docs say that
FD_CLOSE must respond with closesocket(), after which the socket becomes
invalid. We sometimes see FD_READ *after* closesocket()? Why?
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int 	ws_read_process(Process p, char *buf, int size, Real timeout);
static DWORD	process_thread(void *context);
static void	eof_process(Process p, int status);
extern Name	SockError(void);		/* TBD */

static int input_to_handle;

static Socket
findSocketObject(SOCKET sock)
{ Chain ch = getObjectFromReferencePce(PCE, NAME_openSockets);
  Cell cell;

  for_cell(cell, ch)
  { Socket s = cell->value;

    if ( (SOCKET) s->ws_ref == sock )
      return s;

    if ( notNil(s->clients) )
    { Cell cell2;

      for_cell(cell2, s->clients)
      { Socket s2 = cell2->value;

	if ( (SOCKET) s2->ws_ref == sock )
	  return s2;
      }
    }
  }

  return NIL;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Read pending data before calling closesocket(). See also comments at the
start of this file.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
discardInputSocket(SOCKET sock)
{ char buf[4*1024];
  int rval;

  do
  { rval = recv(sock, buf, sizeof(buf), 0);
  } while(rval > 0);
}

static status
handleInputSocket(Socket s)
{
#if 0					/* sockets are non-blocking now */
  u_long avail;

  while( (SOCKET)s->ws_ref != INVALID_SOCKET &&
	 ioctlsocket((SOCKET)s->ws_ref, FIONREAD, &avail) == 0 &&
	 avail > 0 )
  { DEBUG(NAME_stream, Cprintf("%s: %d bytes available\n", pp(s), avail));

    if ( !handleInputStream((Stream)s) )
      fail;

  }

  succeed;
#else
  return handleInputStream((Stream)s);
#endif
}


static int WINAPI
do_socket_wnd_proc(HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam)
{ switch( message )
  { case WM_SOCKET:
    { SOCKET sock = (SOCKET) wParam;
      int err     = WSAGETSELECTERROR(lParam);
      int evt     = WSAGETSELECTEVENT(lParam);
      Socket s    = findSocketObject(sock);

      if ( notNil(s) && err == NO_ERROR )
      { switch(evt)
	{ case FD_READ:
	    DEBUG(NAME_stream, Cprintf("Input available on %s\n", pp(s)));
	    handleInputSocket(s);
	    pceRedraw(FALSE);
	    break;
	  case FD_ACCEPT:
	    DEBUG(NAME_stream, Cprintf("Accept on %s\n", pp(s)));
	    acceptSocket(s);
	    pceRedraw(FALSE);		/* may have side-effects */
	    break;
	  case FD_CLOSE:
	    DEBUG(NAME_stream, Cprintf("Close on %s\n", pp(s)));
	    while(s->rdfd >= 0)
	      handleInputSocket(s);	/* handle input to end-of-file */
	    closesocket(sock);		/* really delete WinSock */
	    s->ws_ref = (WsRef) INVALID_SOCKET;
	    pceRedraw(FALSE);
	    break;
	}
      } else if ( isNil(s) )		/* socket has gone */
      { switch( evt )
	{ case FD_CLOSE:
	  { DEBUG(NAME_stream,
		  Cprintf("Discarding unknown SOCKET=%p\n", sock));
	    discardInputSocket(sock);
	    closesocket(sock);
	    break;
	  }
	  case FD_ACCEPT:
	    DEBUG(NAME_stream,
		  Cprintf("FD_ACCEPT on SOCKET=%p\n", sock));
	    break;
	  case FD_READ:
	    DEBUG(NAME_stream,
		  Cprintf("FD_READ on SOCKET=%p\n", sock));
	    break;
	}
      } else				/* an error occurred */
      { Name name = SockError();

	if ( isNil(name) )
	  name = NAME_unknown;

	errorPce(s, NAME_ioError, name);
      }

      return 0;
    }
    case WM_PROCESS_INPUT:
    { Process p = (Process) wParam;
      int avail = (int) lParam;

      DEBUG(NAME_process, Cprintf("Received WM_PROCESS_INPUT %s %d\n",
				  pp(p), avail));
      handleInputStream((Stream)p);
      input_to_handle = FALSE;
      pceRedraw(FALSE);
      return 0;
    }
    case WM_PROCESS_EXIT:
    { Process p = (Process) wParam;
      int status = (int) lParam;

      DEBUG(NAME_process, Cprintf("Received WM_PROCESS_EXIT %s %d\n",
				  pp(p), status));
      eof_process(p, status);
      pceRedraw(FALSE);
      return 0;
    }
#ifndef USE_RLC_FUNCTIONS
    case WM_RENDERALLFORMATS:
      ws_renderall();
      return 0;
    case WM_RENDERFORMAT:
      if ( ws_provide_selection(wParam) )
	return 0;
      break;
#endif /*USE_RLC_FUNCTIONS*/
  }

  return DefWindowProc(hwnd, message, wParam, lParam);
}


static int WINAPI
socket_wnd_proc(HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam)
{ int rval;

  pceMTLock();
  rval = do_socket_wnd_proc(hwnd, message, wParam, lParam);
  pceMTUnlock();

  return rval;
}


static char *
HiddenFrameClass()
{ static Name winclassname = NULL;
  static WNDCLASS wndClass;

  if ( !winclassname )
  { char buf[50];

    sprintf(buf, "PceSocketWin%d", (int)(intptr_t)PceHInstance);
    winclassname = CtoName(buf);

    wndClass.style		= 0;
    wndClass.lpfnWndProc	= (LPVOID) socket_wnd_proc;
    wndClass.cbClsExtra		= 0;
    wndClass.cbWndExtra		= 0;
    wndClass.hInstance		= PceHInstance;
    wndClass.hIcon		= NULL;
    wndClass.hCursor		= NULL;
    wndClass.hbrBackground	= GetStockObject(WHITE_BRUSH);
    wndClass.lpszMenuName	= NULL;
    wndClass.lpszClassName	= strName(winclassname);

    RegisterClass(&wndClass);
  }

  return strName(winclassname);
}


static HWND pce_hidden_window;

static void
destroyHiddenWindow(int rval)
{ if ( pce_hidden_window )
  { DestroyWindow(pce_hidden_window);
    pce_hidden_window = 0;
  }
}


HWND
PceHiddenWindow()
{ if ( !pce_hidden_window )
  { pce_hidden_window = CreateWindow(HiddenFrameClass(),
				     "XPCE hidden main window",
				     WS_POPUP,
				     0, 0, 32, 32,
				     NULL, NULL, PceHInstance, NULL);
    at_pce_exit(destroyHiddenWindow, ATEXIT_FIFO);
    assert(pce_hidden_window);
  }

  return pce_hidden_window;
}

		 /*******************************
		 *	   PUBLIC STUFF		*
		 *******************************/

void
ws_close_input_stream(Stream obj)
{ if ( instanceOfObject(obj, ClassSocket) )
  { SOCKET s = (SOCKET) obj->ws_ref;

    if ( s != INVALID_SOCKET )
    { WSAAsyncSelect(s, PceHiddenWindow(), 0, 0);
      shutdown(s, SD_RECEIVE);
    }
  } else				/* process */
  { HANDLE fd = (HANDLE) obj->rdfd;

    CloseHandle(fd);
  }

  obj->rdfd = -1;
}


void
ws_close_output_stream(Stream obj)
{ if ( instanceOfObject(obj, ClassSocket) )
  { SOCKET s = (SOCKET) obj->ws_ref;

    if ( s != INVALID_SOCKET )
    { shutdown(s, SD_SEND);
    }
  } else
  { HANDLE fd = (HANDLE) obj->wrfd;

    CloseHandle(fd);
  }

  obj->wrfd = -1;
}


void
ws_close_stream(Stream obj)
{
#if 0					/* sockets are deleted upon FD_CLOSE */
  if ( instanceOfObject(obj, ClassSocket) )
  { SOCKET s = (SOCKET) obj->ws_ref;

    if ( s != INVALID_SOCKET )
    { DEBUG(NAME_stream,
	    Cprintf("MS: [SOCKET=%p] closesocket(%s)\n", s, pp(obj)));
      closesocket(s);
      obj->ws_ref = (WsRef) INVALID_SOCKET;
    }
  } /* else { }*/
#endif
}


void
ws_listen_socket(Socket s)
{ SOCKET sock = (SOCKET) s->ws_ref;

  if ( sock != INVALID_SOCKET )
  { DEBUG(NAME_stream,
	  Cprintf("MS: [SOCKET=%p] listen on %s\n", sock, pp(s)));
    WSAAsyncSelect(sock, PceHiddenWindow(), WM_SOCKET, FD_ACCEPT|FD_CLOSE);
  }
}


void
ws_input_stream(Stream s)
{ HWND hwnd = PceHiddenWindow();    /* make sure to create in main thread */

  if ( instanceOfObject(s, ClassSocket) )
  { SOCKET sock = (SOCKET) s->ws_ref;

    if ( sock != INVALID_SOCKET )
    { DEBUG(NAME_stream,
	  Cprintf("MS: [SOCKET=%p] prepare input on %s\n", sock, pp(s)));
      if ( WSAAsyncSelect(sock, hwnd, WM_SOCKET, FD_READ|FD_CLOSE) )
	errorPce(s, NAME_socket, CtoName("WSAAsyncSelect()"), SockError());
      s->rdfd = 0;			/* signal open status */
    }
  } else
  { DWORD id;
    HANDLE h;

    if ( (h = CreateThread(NULL, 10240,
			   (LPTHREAD_START_ROUTINE)process_thread,
			   (LPVOID) s, 0, &id)) )
      CloseHandle(h);
    else
      Cprintf("%s: Failed to create wait-thread\n", pp(s));
  }

  DEBUG(NAME_stream,
	Cprintf("Registered %s for asynchronous input\n", pp(s)));
}


void
ws_no_input_stream(Stream obj)
{ if ( instanceOfObject(obj, ClassSocket) )
  { SOCKET s = (SOCKET) obj->ws_ref;

    if ( s != INVALID_SOCKET )
    { WSAAsyncSelect(s, PceHiddenWindow(), 0, 0);
    }
  }

  DEBUG(NAME_stream,
	Cprintf("Un-registered %s for asynchronous input\n", pp(obj)));
}


status
ws_write_stream_data(Stream s, void *data, int len)
{ if ( instanceOfObject(s, ClassSocket) )
  { char *str = data;
#undef send
    while( len > 0 )
    { int n = send((SOCKET)s->ws_ref, str, len, 0);

      if ( n < 0 )
      { if ( WSAGetLastError() == WSAEWOULDBLOCK )
	{ ws_dispatch(DEFAULT, DEFAULT);
	  continue;
	}

	return errorPce(s, NAME_ioError, SockError());
      }

      len -= n;
      str += n;
    }
#define send sendPCE
  } else				/* process */
  { DWORD written;
    HANDLE fd = (HANDLE) s->wrfd;

    if ( !WriteFile(fd, data, len, &written, NULL) )
      return errorPce(s, NAME_ioError, APIError());
  }

  succeed;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
On error, the return-value is -1,   unless  the error is WSAEWOULDBLOCK,
which can happen on Windows 9x due  to   a  bug  in select. Therefore we
return -2 and let the caller handle the problem.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
ws_read_stream_data(Stream s, void *data, int len, Real timeout)
{ if ( instanceOfObject(s, ClassSocket) )
  { SOCKET sock = (SOCKET) s->ws_ref;
    int rval;

    if ( notDefault(timeout) )
    { fd_set readfds;
      struct timeval to;
      double v = valReal(timeout);

      to.tv_sec  = (long)v;
      to.tv_usec = (long)(v * 1000000.0) % 1000000;

      FD_ZERO(&readfds);
      FD_SET(sock, &readfds);
      if ( select(0, &readfds, NULL, NULL, &to) == 0 )
	return -2;
    }

    if ( (rval = recv(sock, data, len, 0)) == SOCKET_ERROR )
    { if ( WSAGetLastError() == WSAEWOULDBLOCK )
	return -2;

      return -1;
    }

    return rval;
  } else if ( instanceOfObject(s, ClassProcess) )
  { return ws_read_process((Process)s, data, len, timeout);
  } else
  { Cprintf("%s: ws_read_stream_data() only on socket and process\n", pp(s));
    return -1;
  }
}


		 /*******************************
		 *	PROCESS ASYNC INPUT	*
		 *******************************/

static DWORD
process_thread(void *context)
{ Process p = (Process) context;
  DWORD avail;
  HWND hwnd = PceHiddenWindow();
  int peekok = FALSE;
  DWORD status;
  PROCESS_INFORMATION *pi = p->ws_ref;

  DEBUG(NAME_thread, Cprintf("%s: Starting process input thread\n", pp(p)));

  while( pi && p->rdfd > 0 && !onFlag(p, F_FREED|F_FREEING) &&
	 (peekok=PeekNamedPipe((HANDLE)p->rdfd, NULL, 0, NULL, &avail, NULL)) )
  { if ( avail )
    { input_to_handle = TRUE;
      DEBUG(NAME_thread, Cprintf("Posting WM_PROCESS_INPUT %s %d\n",
				  pp(p), avail));
      PostMessage(hwnd, WM_PROCESS_INPUT, (WPARAM)p, (LPARAM)avail);
      do
      { Sleep(0);			/* TBD: no global status */
      } while( input_to_handle );
    } else
    { if ( GetExitCodeProcess(pi->hProcess, &status) )
      { if ( status == STILL_ACTIVE )
	{ Sleep(100);			/* skip timeslice */
	  continue;
	} else
	{ break;
	}
      } else				/* why should this happen? */
      { status = (DWORD)-1;
	break;
      }
    }

    DEBUG(NAME_thread, Cprintf("."));
  }

  if ( !peekok )
  { DEBUG(NAME_thread,
	  Cprintf("PeekNamedPipe() failed: %s\n", pp(APIError())));
    if ( !pi || !GetExitCodeProcess(pi->hProcess, &status) )
      status = (DWORD) -1;
  }

  PostMessage(hwnd, WM_PROCESS_EXIT, (WPARAM)p, (LPARAM)status);

  DEBUG(NAME_thread, Cprintf("%s: Finished process input thread\n", pp(p)));

  return 0;
}


		 /*******************************
		 *	 PROCESS READLINE	*
		 *******************************/


static void
eof_process(Process p, int status)
{ closeInputProcess(p);
  send(p, NAME_exited, toInt(status), EAV);
}


static int
ws_read_process(Process p, char *buffer, int size, Real timeout)
{ if ( p->rdfd >= 0 )
  { DWORD avail;
    int peekok;
    long endtime;

    if ( isDefault(timeout) )
      endtime = 0;
    else
      endtime = mclock() + (long)(valReal(timeout)*1000.0);

    while( (peekok = PeekNamedPipe((HANDLE)p->rdfd, NULL, 0,
				   NULL, &avail, NULL)) &&
	   avail == 0 &&
	   (!endtime || mclock() < endtime) )
    { PROCESS_INFORMATION *pi = p->ws_ref;
      DWORD status;

      if ( GetExitCodeProcess(pi->hProcess, &status) )
      { if ( status == STILL_ACTIVE )
	{ ws_dispatch(DEFAULT, DEFAULT);
	} else
	{ eof_process((Process)p, status);
	  return 0;			/* end-of-file */
	}
      } else				/* why should this happen? */
      { eof_process((Process)p, -1);
	return -1;
      }
    }

    if ( peekok && avail )
    { int toread = min(size, avail);
      DWORD done;

      DEBUG(NAME_process, Cprintf("%s: %d bytes available\n", pp(p), avail));

      if ( ReadFile((HANDLE)p->rdfd, buffer, toread, &done, NULL) )
      { return done;
      } else
      { errorPce(p, NAME_ioError, APIError());
	eof_process((Process)p, -1);
	return -1;
      }
    } else				/* and this? */
    { DEBUG(NAME_process,
	    Cprintf("%s: peekok = %d, avail = %d\n", pp(p), peekok, avail));
      eof_process((Process) p, -1);
      return -1;
    }
  } else
  { errorPce(p, NAME_notOpen);
    return -1;
  }
}
