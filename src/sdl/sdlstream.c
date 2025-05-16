/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org
    Copyright (c)  2025, SWI-Prolog Solutions b.v.
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

#include <sys/socket.h>
#include <h/kernel.h>
#include <h/unix.h>
#include <errno.h>
#include "sdlstream.h"
#include "sdlinput.h"
#ifdef HAVE_POLL
#include <poll.h>
#endif

#ifndef SHUT_RD
#define SHUT_RD 0
#define SHUT_WR 1
#endif

static FDWatch *
getStreamWatch(Stream s)
{ return s->ws_ref;
}

static void
setStreamWatch(Stream s, FDWatch *watch)
{ s->ws_ref = watch;
}


bool
sdl_stream_event(SDL_Event *event)
{ if ( event->type == MY_EVENT_FD_READY )
  { FDWatch *watch = event->user.data1;

    if ( event->user.code != FD_READY_DISPATCH )
      DEBUG(NAME_stream, Cprintf("Data on %d (code = %d)\n",
				 watch->fd, event->user.code));

    if ( event->user.code == FD_READY_STREAM_INPUT )
    { Stream s = event->user.data2;
      assert(instanceOfObject(s, ClassStream));
      pceMTLock(LOCK_PCE);
      DEBUG(NAME_stream, Cprintf("handleInputStream(%s)\n", pp(s)));
      bool rc = handleInputStream(s);
      pceMTUnlock(LOCK_PCE);
      if ( rc )
	processed_fd_watch(watch);
      else
	ws_no_input_stream(s);
    } else if ( event->user.code == FD_READY_STREAM_ACCEPT )
    { Socket s = event->user.data2;
      assert(instanceOfObject(s, ClassSocket));
      acceptSocket(s);
      processed_fd_watch(watch);
    }

    return true;
  }

  return false;
}

/**
 * Close the input side of the specified stream.
 *
 * @param s Pointer to the Stream object to close.
 */
void
ws_close_input_stream(Stream s)
{ if ( s->rdstream )
  { fclose(s->rdstream);
    s->rdstream = NULL;
  }

  if ( s->rdfd >= 0 )
  {
#ifdef HAVE_SHUTDOWN
    if ( instanceOfObject(s, ClassSocket) )
      shutdown(s->rdfd, SHUT_RD);
    else
#endif
      close(s->rdfd);
    s->rdfd = -1;
  }

  ws_no_input_stream(s);
}

/**
 * Close the output side of the specified stream.
 *
 * @param s Pointer to the Stream object to close.
 */
void
ws_close_output_stream(Stream s)
{ if ( s->wrfd >= 0 )
  {
#ifdef HAVE_SHUTDOWN
    if ( instanceOfObject(s, ClassSocket) )
      shutdown(s->wrfd, SHUT_WR);
/*    else		Seems we must do both to free the descriptor */
#endif
      close(s->wrfd);
    s->wrfd = -1;
  }
}

/**
 * Fully close the specified stream.
 *
 * @param s Pointer to the Stream object to close.
 */
void
ws_close_stream(Stream s)
{
}

/**
 * Prepare the stream to handle new input through messages.
 *
 * @param s Pointer to the Stream object.
 */
void
ws_input_stream(Stream s)
{ if ( s->rdfd >= 0 )
  { FDWatch *watch = add_fd_to_watch(s->rdfd, FD_READY_STREAM_INPUT, s);
    DEBUG(NAME_stream,
	  Cprintf("Registered %s for async input (fd=%d)\n",
		  pp(s), s->rdfd));
    setStreamWatch(s, watch);
  }
}

/**
 * Unprepare the stream.
 *
 * @param s Pointer to the Stream object.
 */
void
ws_no_input_stream(Stream s)
{ FDWatch *watch = getStreamWatch(s);
  if ( watch )
  { remove_fd_watch(watch);
    DEBUG(NAME_stream,
	  Cprintf("Un-registered %s for async input\n", pp(s)));
    setStreamWatch(s, NULL);
  }
}

/**
 * Begin listening on the specified socket for incoming connections or data.
 *
 * @param s Pointer to the Socket object to monitor.
 */
void
ws_listen_socket(Socket s)
{ FDWatch *watch = add_fd_to_watch(s->rdfd, FD_READY_STREAM_ACCEPT, s);
  setStreamWatch((Stream)s, watch);
  DEBUG(NAME_stream,
	  Cprintf("Registered %s for async listen (fd=%d)\n",
		  pp(s), s->rdfd));
}

/**
 * Write raw data to the specified stream.
 *
 * @param s Pointer to the Stream object.
 * @param data Pointer to the data buffer.
 * @param len Number of bytes to write.
 * @return SUCCEED on success; otherwise, FAIL.
 */
status
ws_write_stream_data(Stream s, void *data, int len)
{ if ( s->wrfd < 0 )
    return errorPce(s, NAME_notOpen);
  if ( write(s->wrfd, data, len) != len )
    return errorPce(s, NAME_ioError, getOsErrorPce(PCE));

  succeed;
}

/**
 * Read input  from stream s into  data, which is len  bytes long.  If
 * timeout is not DEFAULT, the read waits at most timeout milliseconds
 * before doing the system read.
 *
 * @param s Pointer to the Stream object.
 * @param data Pointer to the buffer to read into.
 * @param len Number of bytes to read.
 * @param timeout Timeout value in seconds.
 * @return -2: timeout; -1: error; 0: end-of-file; >0: bytes read
 */
int
ws_read_stream_data(Stream s, void *data, int len, Real timeout)
{ if ( s->rdfd < 0 )
  { errno = EINVAL;
    return -1;
  }

  if ( notDefault(timeout) )
  {
#ifdef HAVE_POLL
    double v = valReal(timeout);
    int to = (int)(v*1000.0);
    struct pollfd fds[1];

    fds[0].fd = s->rdfd;
    fds[0].events = POLLIN;
    if ( poll(fds, 1, to) == 0 )
      return -2;
#else
#ifndef __WINDOWS__
    if ( s->rdfd < FD_SETSIZE )
#endif
    { fd_set readfds;
      struct timeval to;
      double v = valReal(timeout);

      to.tv_sec  = (long)v;
      to.tv_usec = (long)(v * 1000000.0) % 1000000;

      FD_ZERO(&readfds);
      FD_SET(s->rdfd, &readfds);
      if ( select(s->rdfd+1, &readfds, NULL, NULL, &to) == 0 )
	return -2;
    }
#endif
  }

  return read(s->rdfd, data, len);
}

/**
 * Clean up after a completed process.
 *
 * @param p Pointer to the Process object to finalize.
 */
void
ws_done_process(Process p)
{
}
