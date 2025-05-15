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
#include <poll.h>
#include <unistd.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <stdbool.h>
#include "sdlinput.h"

#define MAX_FDS 64
#define WATCH_FREE     0
#define WATCH_RESERVED 1
#define WATCH_ACTIVE   2
#define WATCH_PENDING  3
#define WATCH_REMOVE   4

static struct pollfd poll_fds[MAX_FDS];
static int meta_id[MAX_FDS];
static FDWatch fd_meta[MAX_FDS] = {0};
static int control_pipe[2];
static int watch_max = 0;
static SDL_Thread *watcher_thread = NULL;

static int
poll_thread_fn(void *unused)
{ while (true)
  { int nfds = 1;		/* 0 is for our control pipe */
    bool removed = false;

    for(int i=0; i<=watch_max; i++)
    { if ( fd_meta[i].state == WATCH_ACTIVE )
      { poll_fds[nfds].fd = fd_meta[i].fd;
	poll_fds[nfds].events = POLLIN;
	poll_fds[nfds].revents = 0;
	meta_id[nfds] = i;
	nfds++;
      } else if ( fd_meta[i].state == WATCH_REMOVE )
      { fd_meta[i].state = WATCH_FREE;
	removed = true;
      }
    }
    if ( removed )
    { for(int i=MAX_FDS-1; i>=0; i--)
      { if ( fd_meta[i].state != WATCH_FREE )
	{ int expected = watch_max;
	  if ( i < watch_max )
	    atomic_compare_exchange_strong(&watch_max, &expected, i);
	}
      }
    }

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

	DEBUG(NAME_stream, Cprintf("Input on %d\n", watch->fd));
	if ( watch->state == WATCH_ACTIVE )
	{ watch->state = WATCH_PENDING;
	  DEBUG(NAME_stream, Cprintf("Posting %d\n", watch->fd));
	  SDL_Event ev = {0};
	  ev.type = MY_EVENT_FD_READY;
	  ev.user.code  = watch->code;
	  ev.user.data1 = watch;
	  ev.user.data2 = watch->userdata;
	  SDL_PushEvent(&ev);
	}
      }
    }
  }

  return 0;
}

bool
start_fd_watcher_thread(void)
{ if ( pipe(control_pipe) < 0 )
    return false;

  fcntl(control_pipe[0], F_SETFL, O_NONBLOCK);
  poll_fds[0].fd = control_pipe[0];
  poll_fds[0].events = POLLIN;

  watcher_thread = SDL_CreateThread(poll_thread_fn, "fd-watcher", NULL);
  return watcher_thread != NULL;
}

FDWatch *
add_fd_to_watch(int fd, int32_t code, void *userdata)
{ for(int i=0; i<MAX_FDS; i++)
  { int free = WATCH_FREE;

    if ( atomic_compare_exchange_strong(
	   &fd_meta[i].state, &free, WATCH_RESERVED) )
    { FDWatch *watch = &fd_meta[i];
      watch->fd       = fd;
      watch->code     = code;
      watch->userdata = userdata;
      watch->state    = WATCH_ACTIVE;
      if ( i > watch_max )
	watch_max = i;
      write(control_pipe[1], "+", 1);
      return watch;
    }
  }

  return NULL;
}

void
remove_fd_watch(FDWatch *watch)
{ if ( watch )
  { watch->state = WATCH_REMOVE;
    write(control_pipe[1], "-", 1);
  }
}

void
processed_fd_watch(FDWatch *watch)
{ if ( watch )
  { DEBUG(NAME_stream,
	  if ( watch->fd != 0 )
	    Cprintf("Re-enabling %d\n", watch->fd));
    watch->state = WATCH_ACTIVE;
    write(control_pipe[1], "=", 1);
  }
}
