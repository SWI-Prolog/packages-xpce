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

/*
 * Async FD Watcher Thread for SDL3
 * - Listens on file descriptors using poll()
 * - Injects SDL_EVENT_USER events into the SDL event loop when FDs are ready
 * - Accepts dynamic add/remove via a control pipe
 */

#include <h/kernel.h>
#include <SDL3/SDL.h>
#if HAVE_POLL
#include <poll.h>
#endif
#include <unistd.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <stdbool.h>
#include "sdlinput.h"
#ifdef __WINDOWS__
#include "../msw/mswin.h"
#endif

#ifdef __WINDOWS__
#define Pri_WAIT "%p"
#else
#define Pri_WAIT "%d"
#endif

#define MAX_FDS 64

#ifdef __WINDOWS__
static HANDLE handles[MAX_FDS];
static HANDLE control_event;
#else
static struct pollfd poll_fds[MAX_FDS];
static waitable_t control_pipe[2];
#endif
static int meta_id[MAX_FDS];
static FDWatch fd_meta[MAX_FDS] = {0};
static atomic_int watch_max = 0;
static SDL_Thread *watcher_thread = NULL;

static void lock_watch_userdata(FDWatch *watch);
static void unlock_watch_userdata(FDWatch *watch);

bool
cmp_and_set_watch(FDWatch *watch, watch_state old, watch_state new)
{ return atomic_compare_exchange_strong(&watch->state, &old, new);
}


static void
sdl_signal_watch(FDWatch *watch)
{ DEBUG(NAME_stream, Cprintf("Input on " Pri_WAIT "\n", watch->fd));
  if ( cmp_and_set_watch(watch, WATCH_ACTIVE, WATCH_PENDING) )
  { DEBUG(NAME_stream, Cprintf("Posting " Pri_WAIT "\n", watch->fd));
    SDL_Event ev = {0};
    ev.type = MY_EVENT_FD_READY;
    ev.user.code  = watch->code;
    ev.user.data1 = watch;
    ev.user.data2 = watch->userdata;
    lock_watch_userdata(watch);
    SDL_PushEvent(&ev);
#ifdef __WINDOWS__
    watch->pending = false;
#endif
  }
}

static void
releaseWatch(FDWatch *watch)
{
#ifdef __WINDOWS__
  if ( watch->hPipe )
  { HANDLE h;
    if ( (h=watch->overlapped.hEvent) )
    { watch->overlapped.hEvent = NULL;
      CloseHandle(watch->overlapped.hEvent);
    }
    if ( watch->buffer )
    { unalloc(PIPE_READ_CHUNK, watch->buffer);
      watch->buffer = NULL;
    }
  }
#endif
  watch->state = WATCH_FREE;
}

static int
poll_thread_fn(void *unused)
{ while (true)
  { int nfds = 1;		/* 0 is for our control pipe */
    bool removed = false;
    FDWatch *watch = fd_meta;

    for(int i=0; i<=watch_max; i++, watch++)
    { if ( watch->state == WATCH_ACTIVE )
      {
#ifdef __WINDOWS__
	if ( watch->hPipe )	/* A pipe/...: use overlapped I/O */
	{ if ( !watch->pending )
	  { memset(&watch->overlapped, 0, sizeof(watch->overlapped));
	    watch->overlapped.hEvent = watch->fd;
	    if ( ReadFile(watch->hPipe, watch->buffer, PIPE_READ_CHUNK,
			  NULL, &watch->overlapped) )
	    { DEBUG(NAME_stream, Cprintf("Pipe %d immediately ready\n", i));
	      sdl_signal_watch(watch);
	      continue;
	    } else if ( GetLastError() == ERROR_IO_PENDING )
	    { watch->pending = true;
	      DEBUG(NAME_stream, Cprintf("Pipe %d pending\n", i));
	    } else
	    { watch->last_error = GetLastError();
	      if ( watch->last_error != ERROR_BROKEN_PIPE )
		Cprintf("Pipe %d: error: %s\n", i,
			pp(WinStrError(watch->last_error)));
	      sdl_signal_watch(watch);
	    }
	  }
	  if ( watch->pending )
	    handles[nfds] = watch->fd;
	} else
	{ handles[nfds] = watch->fd; /* A normal waitable handle */
	}
#else
	poll_fds[nfds].fd = watch->fd;
        poll_fds[nfds].events = POLLIN;
        poll_fds[nfds].revents = 0;
#endif
        meta_id[nfds] = i;
        nfds++;
      } else if ( watch->state == WATCH_REMOVE )
      { releaseWatch(watch);
	removed = true;
      }
    }
    if ( removed )
    { for(int i=MAX_FDS-1; i>=0; i--)
      { if ( watch->state != WATCH_FREE )
	{ int expected = watch_max;
	  if ( i < watch_max )
	    atomic_compare_exchange_strong(&watch_max, &expected, i);
	  break;
	}
      }
    }

#ifdef __WINDOWS__
    //Cprintf("Waiting for %d handles\n", nfds);
    DWORD rc = WaitForMultipleObjects(nfds,
				      handles,
				      FALSE,
				      INFINITE);

    if ( rc >= WAIT_OBJECT_0 && rc < WAIT_OBJECT_0+nfds )
    { int i = rc-WAIT_OBJECT_0;
      if ( i == 0  )
      { //Cprintf("Control event\n");
	ResetEvent(control_event);
      } else
      { FDWatch *watch = &fd_meta[meta_id[i]];
	sdl_signal_watch(watch);
      }
    }
#else/*!__WINDOWS__*/
    int rc = poll(poll_fds, nfds, -1);
    if (rc < 0)
    { perror("poll");
      break;
    }

    if ( poll_fds[0].revents & POLLIN )
    { char buffer[128];
      read(control_pipe[0], buffer, sizeof(buffer));
    }

    for (int i = 1; i < nfds; ++i)
    { if ( (poll_fds[i].revents & POLLIN) )
      { FDWatch *watch = &fd_meta[meta_id[i]];
	sdl_signal_watch(watch);
      }
    }
#endif/*__WINDOWS__*/
  }

  return 0;
}

bool
start_fd_watcher_thread(void)
{
#if __WINDOWS__
  if ( !(control_event = CreateEvent(NULL, TRUE, FALSE, 0)) )
    return false;
  handles[0] = control_event;
#else
  if ( pipe(control_pipe) < 0 )
    return false;

  fcntl(control_pipe[0], F_SETFL, O_NONBLOCK);
  poll_fds[0].fd = control_pipe[0];
  poll_fds[0].events = POLLIN;
#endif

  watcher_thread = SDL_CreateThread(poll_thread_fn, "fd-watcher", NULL);
  return watcher_thread != NULL;
}


static void
signal_watcher(char c)
{
#ifdef __WINDOWS__
  SetEvent(control_event);
#else
  write(control_pipe[1], &c, 1);
#endif
}

static bool
claim_watch(FDWatch *watch)
{ if ( cmp_and_set_watch(watch, WATCH_FREE, WATCH_RESERVED) )
  { memset(&watch->fd, 0, sizeof(*watch)-offsetof(FDWatch, fd));
    DEBUG(NAME_stream, Cprintf("Claimed %d\n", watch-fd_meta));
    return true;
  }

  return false;
}

static FDWatch *
start_watch(FDWatch *watch, int32_t code, void *userdata)
{ int i = watch-fd_meta;

  watch->code     = code;
  watch->userdata = userdata;
  watch->state    = WATCH_ACTIVE;
  if ( i > watch_max )
    watch_max = i;
  signal_watcher('+');
  return watch;
}


FDWatch *
add_fd_to_watch(waitable_t fd, int32_t code, void *userdata)
{ FDWatch *watch = fd_meta;

  for(int i=0; i<MAX_FDS; i++, watch++)
  { if ( claim_watch(watch) )
    { watch->fd       = fd;
      return start_watch(watch, code, userdata);
    }
  }

  return NULL;
}

#ifdef __WINDOWS__
FDWatch *
add_pipe_to_watch(HANDLE hPipe, int32_t code, void *userdata)
{ FDWatch *watch = fd_meta;

  for(int i=0; i<MAX_FDS; i++, watch++)
  { if ( claim_watch(watch) )
    { watch->hPipe = hPipe;
      watch->fd = CreateEvent(NULL, TRUE, FALSE, NULL);
      watch->buffer = alloc(PIPE_READ_CHUNK);
      watch->last_error = ERROR_SUCCESS;
      return start_watch(watch, code, userdata);
    }
  }

  return NULL;
}

ssize_t
read_watch(FDWatch *watch, char *buffer, size_t size)
{ DWORD bytes;
  if ( watch->last_error != ERROR_SUCCESS )
  { return -1;
  } else if ( GetOverlappedResult(watch->hPipe, &watch->overlapped,
				  &bytes, FALSE) )
  { assert(bytes <= size);
    memcpy(buffer, watch->buffer, bytes);
    if ( bytes == 0 )
    { Cprintf("Got 0 bytes?\n");
      exit(1);
    }
    return bytes;
  } else
  { Cprintf("No overlapped result: %s\n", pp(WinStrError(GetLastError())));
    return -1;
  }
}



#endif/*__WINDOWS__*/
FDWatch *
add_socket_to_watch(socket_t fd, int32_t code, void *userdata)
{ FDWatch *watch = fd_meta;

  for(int i=0; i<MAX_FDS; i++, watch++)
  { if ( claim_watch(watch) )
    {
#ifdef __WINDOWS__
      watch->sock     = fd;
#else
      watch->fd       = fd;
#endif
      return start_watch(watch, code, userdata);
    }
  }

  return NULL;
}


void
remove_fd_watch(FDWatch *watch)
{ while ( watch &&
	  !(watch->state == WATCH_REMOVE || watch->state == WATCH_FREE) )
  { if ( cmp_and_set_watch(watch, WATCH_ACTIVE, WATCH_REMOVE) )
    { signal_watcher('-');
    } else if ( cmp_and_set_watch(watch, WATCH_PENDING, WATCH_REMOVE) )
    { unlock_watch_userdata(watch);
      signal_watcher('-');
    } else if ( cmp_and_set_watch(watch, WATCH_PROCESSING, WATCH_REMOVE) )
    { (void)0;			/* delete in processed_fd_watch */
    }
  }
}

void
processed_fd_watch(FDWatch *watch)
{ if ( watch )
  { if ( cmp_and_set_watch(watch, WATCH_PROCESSING, WATCH_ACTIVE) )
    { DEBUG(NAME_stream,
	    if ( watch->fd != 0 )
	      Cprintf("Re-enabling %d\n", watch->fd));
      unlock_watch_userdata(watch);
      signal_watcher('=');
    } else if ( watch->state == WATCH_REMOVE )
    { unlock_watch_userdata(watch);
      signal_watcher('-');
    }
  }
}

static void
lock_watch_userdata(FDWatch *watch)
{ switch(watch->code)
  { case FD_READY_STREAM_INPUT:
    case FD_READY_STREAM_ACCEPT:
    case FD_READY_TERMINAL:
      addCodeReference(watch->userdata);
      break;
    case FD_READY_DISPATCH:
      break;
  }
}

static void
unlock_watch_userdata(FDWatch *watch)
{ switch(watch->code)
  { case FD_READY_STREAM_INPUT:
    case FD_READY_STREAM_ACCEPT:
    case FD_READY_TERMINAL:
      delCodeReference(watch->userdata);
      break;
    case FD_READY_DISPATCH:
      break;
  }
}
