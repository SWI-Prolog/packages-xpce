/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  1985-2002, University of Amsterdam
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

#include <md.h>				/* get HAVE_'s */

#if defined(HAVE_SOCKET) || defined(HAVE_WINSOCK) || defined(HAVE_FORK)

#ifdef HAVE_WINSOCK
#include "mswinsock.h"
#define StreamError() SockError()
#else
#define StreamError() OsError()
#endif

#include <h/kernel.h>

#include <h/unix.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif

static status recordSeparatorStream(Stream s, Any sep);

#define OsError() getOsErrorPce(PCE)

status
initialiseStream(Stream s, Int rfd, Int wfd, Code input, Any sep)
{ s->rdfd = s->wrfd = -1;
  s->ws_ref = 0;
  s->input_buffer = NULL;
  s->input_allocated = s->input_p = 0;
  s->input_pending = NULL;
  s->input_pending_len = 0;

  if ( isDefault(rfd) )   rfd = NIL;
  if ( isDefault(wfd) )   wfd = NIL;
  if ( isDefault(input) ) input = NIL;
  if ( isDefault(sep) )   sep = newObject(ClassRegex, CtoName("\n"), EAV);

  if ( notNil(rfd) ) s->rdfd = valInt(rfd);
  if ( notNil(wfd) ) s->wrfd = valInt(wfd);

  assign(s, input_message, input);
  assign(s, encoding, getClassVariableValueObject(s, NAME_encoding));
  recordSeparatorStream(s, sep);

  succeed;
}


static status
unlinkStream(Stream s)
{ return closeStream(s);
}

		 /*******************************
		 *	    OPEN/CLOSE		*
		 *******************************/


status
closeStream(Stream s)
{ closeOutputStream(s);
  closeInputStream(s);

  ws_close_stream(s);

  succeed;
}


status
closeInputStream(Stream s)
{ if ( s->rdfd >= 0 )
  { DEBUG(NAME_stream, Cprintf("%s: Closing input\n", pp(s)));

    ws_close_input_stream(s);
    s->rdfd = -1;

    if ( s->input_buffer )
    { pceFree(s->input_buffer);
      s->input_buffer = NULL;
    }
    s->input_allocated = s->input_p = 0;
    if ( s->input_pending )
    { pceFree(s->input_pending);
      s->input_pending = NULL;
    }
    s->input_pending_len = 0;
  }

  succeed;
}


status
closeOutputStream(Stream s)
{ if ( s->wrfd >= 0 )
  { int input_too = (s->wrfd == s->rdfd);

    DEBUG(NAME_stream, Cprintf("%s: Closing output\n", pp(s)));

    ws_close_output_stream(s);
    s->wrfd = -1;
    if ( input_too )
      closeInputStream(s);
  }

  succeed;
}


status
inputStream(Stream s, Int fd)
{ if ( notDefault(fd) )
  { if ( isNil(fd) )
      closeInputStream(s);
    else
      s->rdfd = valInt(fd);		/* Unix only! */
  }

/*if ( notNil(s->input_message) )*/
    ws_input_stream(s);

  succeed;
}


		 /*******************************
		 *        HANDLE INPUT		*
		 *******************************/


#define BLOCKSIZE 1024
#define ALLOCSIZE 1024
#define MAX_PENDING 16			/* >= longest incomplete sequence */

#define Round(n, r) (((n) + (r) - 1) & ~((r)-1))

static IOENC
stream_encoding(Stream s)
{ IOENC enc = name_to_encoding(s->encoding);

  return enc == ENC_UNKNOWN ? ENC_UTF8 : enc;
}


/* Length of the prefix of data that holds only complete characters
 * for enc.  The remainder is kept until more data arrives.
 */

static size_t
complete_prefix(IOENC enc, const unsigned char *data, size_t len)
{ switch(enc)
  { case ENC_UTF8:
    { size_t i = len;

      for(size_t back=1; i > 0 && back <= 4; back++)
      { int c = data[--i];

	if ( (c&0xc0) != 0x80 )		/* not a continuation byte */
	{ size_t need = c >= 0xf0 && c < 0xf8 ? 4 :
			c >= 0xe0 && c < 0xf0 ? 3 :
			c >= 0xc0 && c < 0xe0 ? 2 : 1;

	  return back < need ? i : len;
	}
      }

      return len;
    }
    case ENC_UTF16BE:
    case ENC_UTF16LE:
    { size_t n = len & ~(size_t)1;

      if ( n >= 2 )
      { int u = ( enc == ENC_UTF16BE ? (data[n-2]<<8)|data[n-1]
				     : (data[n-1]<<8)|data[n-2] );
	if ( u >= 0xd800 && u <= 0xdbff )	/* high surrogate */
	  n -= 2;
      }

      return n;
    }
    case ENC_ANSI:
    { mbstate_t state;
      size_t i = 0;

      memset(&state, 0, sizeof(state));
      while( i < len )
      { size_t rc = mbrlen((const char *)data+i, len-i, &state);

	if ( rc == (size_t)-2 )
	  return i;
	if ( rc == (size_t)-1 || rc == 0 )
	{ memset(&state, 0, sizeof(state));
	  i++;
	} else
	  i += rc;
      }

      return len;
    }
    default:
      return len;
  }
}


static void
add_char_stream(Stream s, int c)
{ if ( s->input_p >= s->input_allocated )
  { s->input_allocated = Round(s->input_p + 1, ALLOCSIZE);
    s->input_buffer = pceRealloc(s->input_buffer,
				 s->input_allocated * sizeof(charW));
  }

  s->input_buffer[s->input_p++] = c;
}


/* Decode data using the stream's encoding and add it to the input
 * buffer.  An incomplete multibyte sequence at the end is saved in
 * input_pending and prepended to the next block.
 */

void
add_data_stream(Stream s, char *data, int len)
{ IOENC enc = stream_encoding(s);
  unsigned char *bytes = (unsigned char *)data;
  unsigned char *joined = NULL;
  size_t size = len;
  size_t n;

  if ( s->input_pending_len > 0 )
  { size = s->input_pending_len + len;
    joined = pceMalloc(size);
    memcpy(joined, s->input_pending, s->input_pending_len);
    memcpy(joined+s->input_pending_len, data, len);
    bytes = joined;
    s->input_pending_len = 0;
  }

  n = complete_prefix(enc, bytes, size);
  if ( size-n > MAX_PENDING )
    n = size;
  if ( n < size )
  { if ( !s->input_pending )
      s->input_pending = pceMalloc(MAX_PENDING);
    memcpy(s->input_pending, bytes+n, size-n);
    s->input_pending_len = size-n;
  }

  if ( enc == ENC_OCTET || enc == ENC_ISO_LATIN_1 )
  { for(size_t i=0; i<n; i++)
      add_char_stream(s, bytes[i]);
  } else if ( n > 0 )
  { IOSTREAM *fd = Sopen_string(NULL, (char *)bytes, n, "r");

    if ( fd )
    { int c;

      fd->encoding = enc;
      while( (c=Sgetcode(fd)) != EOF )
	add_char_stream(s, c);
      Sclose(fd);
    }
  }

  if ( joined )
    pceFree(joined);
}


static void
write_char(int c)
{ if ( c < 32 || (c >= 127 && c < 128+32) )
  { char buf[16];
    char *prt = buf;

    switch(c)
    { case '\t':
	prt = "\\t";
        break;
      case '\r':
	prt = "\\r";
	break;
      case '\n':
	prt = "\\n";
	break;
      case '\b':
	prt = "\\b";
	break;
      default:
	snprintf(buf, sizeof(buf), "<%d>", c);
    }

    Cprintf("%s", prt);
  } else
    Cputchar(c);
}


static void
write_buffer(const charW *buf, int size)
{ if ( size > 50 )
  { write_buffer(buf, 25);
    Cprintf(" ... ");
    write_buffer(buf + size - 25, 25);
  } else
  { for(int n=0; n<size; n++)
      write_char(buf[n]);
  }
}


static void
str_set_input_stream(PceString str, Stream s, size_t len)
{ str_inithdr(str, TRUE);
  str->s_size = (int)len;
  str->s_textW = s->input_buffer;
}


static void
dispatch_stream(Stream s, int size, int discard)
{ string q;
  AnswerMark mark;
  Any str;

  assert(size <= s->input_p);

  markAnswerStack(mark);
  str_set_input_stream(&q, s, size);
  str = StringToString(&q);
  if ( discard )
  { pceFree(s->input_buffer);
    s->input_buffer = NULL;
    s->input_allocated = s->input_p = 0;
  } else
  { memmove(s->input_buffer, &s->input_buffer[size],
	    (s->input_p - size) * sizeof(charW));
    s->input_p -= size;
  }

  DEBUG(NAME_input,
	{ Cprintf("Sending: %d characters, %s", size, pp(str));
	  Cprintf("\n\tLeft: %d characters, `", s->input_p);
	  write_buffer(s->input_buffer, s->input_p);
	  Cprintf("'\n");
	});

  if ( notNil(s->input_message) )
  { addCodeReference(s);
    assert(isProperObject(s));
    forwardReceiverCodev(s->input_message, s, 1, &str);
    assert(isProperObject(s));
    delCodeReference(s);
  }

  rewindAnswerStack(mark, NIL);
}


static void
dispatch_input_stream(Stream s)
{ while( !onFlag(s, F_FREED|F_FREEING) && s->input_buffer && s->input_p > 0 )
  { if ( isNil(s->record_separator) )
    { dispatch_stream(s, s->input_p, TRUE);

      return;
    }

    if ( isInteger(s->record_separator) )
    { int bsize = valInt(s->record_separator);

      if ( bsize <= s->input_p )
      {	dispatch_stream(s, bsize, FALSE);
	continue;
      }

      return;
    }

    if ( instanceOfObject(s->record_separator, ClassRegex) )
    { Regex re = s->record_separator;
      string str;

      str_set_input_stream(&str, s, s->input_p);
      if ( search_string_regex(re, &str) )
      { int size = valInt(getRegisterEndRegex(s->record_separator, ZERO));

	dispatch_stream(s, size, FALSE);
	continue;
      }
    }

    return;
  }
}



status
handleInputStream(Stream s)
{ char buf[BLOCKSIZE];
  int n;

  if ( onFlag(s, F_FREED|F_FREEING) )
    fail;

  if ( (n = ws_read_stream_data(s, buf, BLOCKSIZE, DEFAULT)) > 0 )
  { intptr_t here = s->input_p;

    add_data_stream(s, buf, n);

    DEBUG(NAME_input,
	  { Cprintf("Read (%d bytes, %d chars): `",
		    n, (int)(s->input_p-here));
	    write_buffer(&s->input_buffer[here], (int)(s->input_p-here));
	    Cprintf("'\n");
	  });

    if ( notNil(s->input_message) )
      dispatch_input_stream(s);
  } else if ( n != -2 )			/* Win 9x errornous WSAEWOULDBLOCK */
  {
    DEBUG(NAME_stream,
	  if ( n < 0 )
	    Cprintf("Read failed: %s\n", strName(StreamError()));
	  else
	    Cprintf("%s: Got 0 characters: EOF\n", pp(s));
	 );
    send(s, NAME_closeInput, EAV);
    send(s, NAME_endOfFile, EAV);
  }

  succeed;
}


		 /*******************************
		 *       OUTPUT HANDLING	*
		 *******************************/

/* Write str to s.  The Prolog stream layer encodes the text into a
 * memory buffer that is written in one go.  Doing the I/O ourselves
 * keeps I/O errors xpce errors rather than Prolog stream errors.
 */

static status
writeStringStream(Stream s, PceString str)
{ IOENC enc = stream_encoding(s);
  char tmp[4096];
  char *buf = tmp;
  size_t size = sizeof(tmp);
  IOSTREAM *fd;
  status rc = SUCCEED;

  if ( s->wrfd < 0 )
    return errorPce(s, NAME_notOpen);
  if ( str->s_size == 0 )
    succeed;
  if ( isstrA(str) && (enc == ENC_OCTET || enc == ENC_ISO_LATIN_1) )
    return ws_write_stream_data(s, str->s_textA, str->s_size);

  if ( !(fd = Sopenmem(&buf, &size, "wb")) )
    return errorPce(s, NAME_ioError, OsError());
  fd->encoding = enc;

  for(int i=0; i<str->s_size; i++)
  { int c = str_fetch(str, i);

    if ( Scanrepresent(c, fd) < 0 )
    { rc = errorPce(s, NAME_representation, NAME_encoding);
      break;
    }
    Sputcode(c, fd);
  }

  if ( Sclose(fd) < 0 && rc )
    rc = errorPce(s, NAME_ioError, OsError());
  if ( rc )
    rc = ws_write_stream_data(s, buf, (int)size);
  if ( buf != tmp )
    Sfree(buf);

  return rc;
}


static status
appendStream(Stream s, CharArray data)
{ return writeStringStream(s, &data->data);
}


static status
newlineStream(Stream s)
{ static char nl[] = "\n";
  string str;

  str_set_n_ascii(&str, 1, nl);
  return writeStringStream(s, &str);
}


static status
appendLineStream(Stream s, CharArray data)
{ if ( !appendStream(s, data) ||
       !newlineStream(s) )
    fail;

  succeed;
}


static status
formatStream(Stream s, CharArray fmt, int argc, Any *argv)
{ string tmp;
  status rc;

  str_writefv(&tmp, fmt, argc, argv);
  rc = writeStringStream(s, &tmp);
  str_unalloc(&tmp);

  return rc;
}


static status
waitStream(Stream s)
{ while( s->rdfd >= 0 )
    dispatchDisplayManager(TheDisplayManager(), DEFAULT, DEFAULT);

  succeed;
}

		 /*******************************
		 *	  INPUT HANDLING	*
		 *******************************/

static StringObj
getReadLineStream(Stream s, Real timeout)
{ int64_t epoch, tmo, left;
  bool use_timeout;

  if ( instanceOfObject(timeout, ClassReal) )
  { double v = valReal(timeout);

    if ( v < 0.0 )
      answer((StringObj)NIL);

    epoch = mclock();
    tmo = (unsigned long)(v * 1000.0);
    use_timeout = true;
  } else
  { use_timeout = false;
    epoch = tmo = 0L;		/* keep compiler happy */
  }

  while(s->rdfd >= 0)
  { if ( s->input_buffer )
    { charW *q;
      int n;

      DEBUG(NAME_stream, Cprintf("Scanning %d chars\n", s->input_p));
      for(n=s->input_p, q = s->input_buffer; n > 0; n--, q++)
      { if ( *q == '\n' )
	{ string str;
	  size_t len = (q-s->input_buffer)+1;
	  StringObj rval;

	  str_set_input_stream(&str, s, len);
	  rval = StringToString(&str);
	  memmove(s->input_buffer, &s->input_buffer[len],
		  (s->input_p - len) * sizeof(charW));
	  s->input_p -= len;

	  return rval;
	}
      }
      DEBUG(NAME_stream, Cprintf("No newline, reading\n"));
    }

    if ( use_timeout )
    { int64_t now = mclock();

      if ( now - epoch > tmo )
	answer((StringObj)NIL);
      left = tmo - (now - epoch);
    } else
      left = 0;				/* keep compiler happy */

    if ( !ws_dispatch(NULL, use_timeout ? toInt(left) : NIL) )
      return (StringObj) NIL;
  }

  fail;
}


static status
endOfFileStream(Stream s)
{ DEBUG(NAME_stream, Cprintf("Stream %s: end of output\n", pp(s)));

  succeed;
}


static status
recordSeparatorStream(Stream s, Any re)
{ if ( s->record_separator != re )
  { if ( isInteger(re) && valInt(re) > STR_MAX_SIZE )
      return errorPce(s, NAME_maxRecordSize, toInt(STR_MAX_SIZE));

    assign(s, record_separator, re);

    if ( instanceOfObject(re, ClassRegex) )
      compileRegex(re, ON);

    dispatch_input_stream(s);		/* handle possible pending data */
  }

  succeed;
}


static status
inputMessageStream(Stream s, Code msg)
{ if ( s->input_message != msg )
  { Code old = s->input_message;

    assign(s, input_message, msg);
    if ( isNil(old) && notNil(msg) )
    { ws_input_stream(s);
    } else if ( notNil(old) && isNil(msg) )
    { ws_no_input_stream(s);
    }
  }

  succeed;
}


		 /*******************************
		 *	      AS FILE		*
		 *******************************/

static status
writeAsFileStream(Stream s, Int where, CharArray txt)
{ if ( notDefault(where) )
    return errorPce(s, NAME_cannotSeekNonFile);

  return appendStream(s, txt);
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_format[] =
        { "format=char_array", "argument=any ..." };
static char *T_initialise[] =
        { "rfd=[int]", "wfd=[int]",
	  "input_message=[code]", "record_separator=[regex|int]" };
static char *T_writeAsFile[] =
        { "at=[int]", "text=char_array" };

/* Instance Variables */

#define var_stream XPCE_var_stream	/* AIX 3.2.5 conflict */

static vardecl var_stream[] =
{ SV(NAME_inputMessage, "code*", IV_GET|IV_STORE,
     inputMessageStream,
     NAME_input, "Forwarded on input from the stream"),
  SV(NAME_recordSeparator, "regex|int*", IV_GET|IV_STORE,
     recordSeparatorStream,
     NAME_input, "Regex that describes the record separator"),
  IV(NAME_encoding, "{octet,ascii,iso_latin_1,text,utf8,unicode_be,unicode_le}", IV_BOTH,
     NAME_encoding, "Encoding of the byte stream"),
  IV(NAME_wrfd, "alien:int", IV_NONE,
     NAME_internal, "File-handle to write to stream"),
  IV(NAME_rdfd, "alien:int", IV_NONE,
     NAME_internal, "File-handle to read from stream"),
  IV(NAME_rdstream, "alien:FILE *", IV_NONE,
     NAME_internal, "Stream used for <-read_line"),
  IV(NAME_wsRef, "alien:WsRef", IV_NONE,
     NAME_internal, "Window system synchronisation"),
  IV(NAME_inputBuffer, "alien:charW *", IV_NONE,
     NAME_internal, "Buffer for collecting decoded input"),
  IV(NAME_inputAllocated, "alien:int", IV_NONE,
     NAME_internal, "Allocated size of input_buffer"),
  IV(NAME_inputP, "alien:int", IV_NONE,
     NAME_internal, "Number of characters in input_buffer"),
  IV(NAME_inputPending, "alien:unsigned char *", IV_NONE,
     NAME_internal, "Incomplete multibyte sequence"),
  IV(NAME_inputPendingLen, "alien:int", IV_NONE,
     NAME_internal, "Number of bytes in input_pending")
};

/* Send Methods */

static senddecl send_stream[] =
{ SM(NAME_initialise, 4, T_initialise, initialiseStream,
     DEFAULT, "Create stream"),
  SM(NAME_unlink, 0, NULL, unlinkStream,
     DEFAULT, "Cleanup stream"),
  SM(NAME_wait, 0, NULL, waitStream,
     NAME_control, "Wait for the complete output"),
  SM(NAME_endOfFile, 0, NULL, endOfFileStream,
     NAME_input, "Send when end-of-file is reached"),
  SM(NAME_closeInput, 0, NULL, closeInputStream,
     NAME_open, "Close input section of stream"),
  SM(NAME_closeOutput, 0, NULL, closeOutputStream,
     NAME_open, "Close output section of stream"),
  SM(NAME_input, 1, "fd=[int]*", inputStream,
     NAME_open, "Enable input from file-descriptor"),
  SM(NAME_append, 1, "data=char_array", appendStream,
     NAME_output, "Send data to stream"),
  SM(NAME_appendLine, 1, "data=char_array", appendLineStream,
     NAME_output, "->append and ->newline"),
  SM(NAME_format, 2, T_format, formatStream,
     NAME_output, "Format arguments and send to stream"),
  SM(NAME_newline, 0, NULL, newlineStream,
     NAME_output, "Send a newline to the stream"),
  SM(NAME_writeAsFile, 2, T_writeAsFile, writeAsFileStream,
     NAME_stream, "Allow pce_open(Socket, append, Stream)")
};

/* Get Methods */

static getdecl get_stream[] =
{ GM(NAME_readLine, 1, "string*", "timeout=[real]", getReadLineStream,
     NAME_input, "Read line with optional timeout (seconds)")
};

/* Resources */

static classvardecl rc_stream[] =
{ RC(NAME_encoding, NULL, "utf8",
     "Default encoding of the byte stream")
};

/* Class Declaration */

ClassDecl(stream_decls,
          var_stream, send_stream, get_stream, rc_stream,
          0, NULL);

status
makeClassStream(Class class)
{ return declareClass(class, &stream_decls);
}

#else /*O_NO_PROCESS && O_NO_SOCKET*/

		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */


/* Instance Variables */

static vardecl var_stream[] =
{ IV(NAME_inputMessage, "code*", IV_BOTH,
     NAME_input, "Forwarded on input from the stream"),
  IV(NAME_recordSeparator, "regex|int*", IV_GET,
     NAME_input, "Regex that describes the record separator"),
  IV(NAME_encoding, "{octet,ascii,iso_latin_1,text,utf8,unicode_be,unicode_le}", IV_GET,
     NAME_encoding, "Encoding of the byte stream"),
  IV(NAME_wrfd, "alien:int", IV_NONE,
     NAME_internal, "File-handle to write to stream"),
  IV(NAME_rdfd, "alien:int", IV_NONE,
     NAME_internal, "File-handle to read from stream"),
  IV(NAME_rdstream, "alien:FILE *", IV_NONE,
     NAME_internal, "Stream used for <-read_line"),
  IV(NAME_wsRef, "alien:WsRef", IV_NONE,
     NAME_internal, "Window System synchronisation"),
  IV(NAME_inputBuffer, "alien:charW *", IV_NONE,
     NAME_internal, "Buffer for collecting decoded input"),
  IV(NAME_inputAllocated, "alien:int", IV_NONE,
     NAME_internal, "Allocated size of input_buffer"),
  IV(NAME_inputP, "alien:int", IV_NONE,
     NAME_internal, "Number of characters in input_buffer"),
  IV(NAME_inputPending, "alien:unsigned char *", IV_NONE,
     NAME_internal, "Incomplete multibyte sequence"),
  IV(NAME_inputPendingLen, "alien:int", IV_NONE,
     NAME_internal, "Number of bytes in input_pending")
};

/* Send Methods */

#define send_stream NULL
/*
static senddecl send_stream[] =
{
};
*/

/* Get Methods */

#define get_stream NULL
/*
static getdecl get_stream[] =
{
};
*/

/* Resources */

#define rc_stream NULL
/*
static classvardecl rc_stream[] =
{
};
*/

/* Class Declaration */

ClassDecl(stream_decls,
          var_stream, send_stream, get_stream, rc_stream,
          0, NULL);

status
makeClassStream(Class class)
{ return declareClass(class, &stream_decls);
}

#endif /*O_NO_PROCESS && O_NO_SOCKET*/
