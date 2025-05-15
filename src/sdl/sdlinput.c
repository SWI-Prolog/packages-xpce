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

typedef struct
{ int   fd;			/* FD we are watching */
  int   code;			/* SDL3 event.user.code */
  void *userdata;		/* SDL3 event.user.data1 */
} FDWatch;

static struct pollfd poll_fds[MAX_FDS];
static FDWatch fd_meta[MAX_FDS];
static int nfds = 1; // 0 = control pipe, 1+ = user fds
static int control_pipe[2];
static SDL_Thread *watcher_thread = NULL;

// Message format: "+42:1234" to add fd 42 with userdata 1234
//                "-42" to remove fd 42
static void
handle_control_message(const char *msg)
{ if ( msg[0] == '+' )
  { int fd = 0;
    int32_t code = 0;
    size_t userdata = 0;
    sscanf(msg+1, "%d:%u:%zu", &fd, &code, &userdata);
    if ( nfds < MAX_FDS )
    { poll_fds[nfds].fd = fd;
      poll_fds[nfds].events = POLLIN;
      fd_meta[nfds].fd = fd;
      fd_meta[nfds].code     = code;
      fd_meta[nfds].userdata = (void*)userdata;
      nfds++;
    }
  } else if (msg[0] == '-')
  { int fd = 0;
    sscanf(msg+1, "%d", &fd);
    for (int i = 1; i < nfds; ++i)
    { if ( poll_fds[i].fd == fd )
      { // remove by swapping last
	poll_fds[i] = poll_fds[nfds-1];
	fd_meta[i] = fd_meta[nfds-1];
	nfds--;
	break;
      }
    }
  }
}

static int
poll_thread_fn(void *unused)
{ char buffer[128];

  while (true)
  { int rc = poll(poll_fds, nfds, -1);
    if (rc < 0)
    { perror("poll");
      break;
    }

    if ( poll_fds[0].revents & POLLIN )
    { ssize_t len = read(control_pipe[0], buffer, sizeof(buffer)-1);
      if ( len > 0 )
      { buffer[len] = '\0';
	handle_control_message(buffer);
      }
    }

    for (int i = 1; i < nfds; ++i)
    { if (poll_fds[i].revents & POLLIN) {
	SDL_Event ev = {0};
	ev.type = MY_EVENT_FD_READY;
	ev.user.code  = fd_meta[i].code;
	ev.user.data1 = (void*)(uintptr_t)fd_meta[i].fd;
	ev.user.data2 = fd_meta[i].userdata;
	SDL_PushEvent(&ev);
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
  nfds = 1;

  watcher_thread = SDL_CreateThread(poll_thread_fn, "fd-watcher", NULL);
  return watcher_thread != NULL;
}

void
add_fd_to_watch(int fd, int32_t code, void *userdata)
{ char msg[64];
  snprintf(msg, sizeof(msg), "+%d:%u:%zu", fd, code, (size_t)userdata);
  write(control_pipe[1], msg, strlen(msg));
}

void
remove_fd_from_watch(int fd)
{ char msg[32];
  snprintf(msg, sizeof(msg), "-%d", fd);
  write(control_pipe[1], msg, strlen(msg));
}
