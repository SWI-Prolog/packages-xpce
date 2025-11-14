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

#ifndef SDL_INPUT_H_INCLUDED
#define SDL_INPUT_H_INCLUDED
#include "sdluserevent.h"
#ifdef __STDC_NO_ATOMICS__
#define _Atomic(type) type
#else
#include <stdatomic.h>
#endif
#ifdef __WINDOWS__
#include <windows.h>
typedef HANDLE waitable_t;
typedef SOCKET socket_t;
#define NO_WAITABLE (NULL)
#define PIPE_READ_CHUNK 4096
#else
typedef int waitable_t;
typedef int socket_t;
#define NO_WAITABLE (-1)
#endif


typedef enum
{ WATCH_FREE = 0,		/* free to be allocated */
  WATCH_RESERVED,		/* Being installed */
  WATCH_ACTIVE,			/* Fully installed */
  WATCH_PENDING,		/* Sent SDL Event */
  WATCH_PROCESSING,		/* Processing SDL event */
  WATCH_REMOVE			/* Ready to be removed */
} watch_state;

typedef struct
{ _Atomic (watch_state)	state;	/* WATCH_* */
  waitable_t	 fd;		/* FD/HANDLE we are watching */
#ifdef __WINDOWS__
  socket_t       sock;		/* socket we are watching */
  HANDLE	 hPipe;		/* Pipe handle */
  OVERLAPPED	 overlapped;	/* ReadFile() overlapped struct */
  size_t	 buffer_size;	/* Size of the buffer */
  char		*buffer;	/* Overlapped data buffer */
  size_t	 queue_size;	/* write_watch() */
  char          *queue;		/* write_watch() */
  bool		 pending;	/* We started a ReadFile() */
  CRITICAL_SECTION lock;	/* Lock for async write */
  DWORD		 last_error;	/* ReadFile() failed with this code */
#endif
  fd_ready_codes code;		/* SDL3 event.user.code */
  Any		userdata;	/* SDL3 event.user.data2 */
} FDWatch;

bool cmp_and_set_watch(FDWatch *watch, watch_state old, watch_state new);

bool	 start_fd_watcher_thread(void);
FDWatch *add_fd_to_watch(waitable_t fd, int32_t code, void *userdata);
FDWatch *add_socket_to_watch(socket_t fd, int32_t code, void *userdata);
void	 remove_fd_watch(FDWatch *watch);
void	 processed_fd_watch(FDWatch *watch);

#ifdef __WINDOWS__
FDWatch *add_pipe_to_watch(HANDLE hPipe, int32_t code, void *userdata);
FDWatch *add_out_pipe_to_watch(HANDLE hPipe);
ssize_t  read_watch(FDWatch *watch, char *buffer, size_t size);
ssize_t	 write_watch(FDWatch *watch, const char *buffer, size_t size);
#endif

#endif /*SDL_INPUT_H_INCLUDED*/
