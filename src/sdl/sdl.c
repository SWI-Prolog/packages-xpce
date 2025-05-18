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

#include <h/kernel.h>
#include <h/graphics.h>
#include "sdl.h"
#include "sdlinput.h"

/**
 * Initialize the Raylib backend with the given command-line arguments.
 *
 * @param argc Argument count.
 * @param argv Argument vector.
 */
void
ws_initialise(int argc, char **argv)
{
}

static bool sdl_initialised_b = false;

bool
sdl_initialise(void)
{ if ( !sdl_initialised_b )
  { sdl_initialised_b = true;

    if ( !SDL_Init(SDL_INIT_EVENTS) )
      return errorPce(NIL, NAME_sdlInitialize);
    Cprintf("SDL_Init() on thread %d\n", PL_thread_self());
    ChangedFrames = globalObject(NAME_changedFrames, ClassChain, EAV);
    start_fd_watcher_thread();
  }

  return true;
}

bool
sdl_initialised(void)
{ return sdl_initialised_b;
}


/**
 * Retrieve the major version number of the SDL backend.
 *
 * @return Integer representing the version number.
 */
int
ws_version(void)
{ return 3;
}

/**
 * Retrieve the revision number of the Raylib backend.
 *
 * @return Integer representing the revision.
 */
int
ws_revision(void)
{ return 2;
}

/**
 * Show or hide the XPCE console window.
 *
 * @param how Name indicating how to show the console (e.g., open, iconify).
 * @return SUCCEED on success; otherwise, FAIL.
 */
status
ws_show_console(Name how)
{
    return SUCCEED;
}

/**
 * Set the label of the console window.
 *
 * @param label The CharArray containing the new label.
 * @return SUCCEED on success; otherwise, FAIL.
 */
status
ws_console_label(CharArray label)
{
    return SUCCEED;
}

/**
 * Get the default width of scrollbars.
 *
 * @return The default scrollbar width as an Int.
 */
Int
ws_default_scrollbar_width(void)
{
    return (Int)16;
}

/**
 * Retrieve the current username.
 *
 * @return A pointer to a string containing the username.
 */
char *
ws_user(void)
{
    return "user";
}

/**
 * Called when the xpce dispatch thread is terminated
 *
 * @return `TRUE` on success
 */

int
pceMTdetach(void)
{ return TRUE;
}

		 /*******************************
		 *   SEND TO SDL MAIN THREAD    *
		 *******************************/

typedef struct
{ Any receiver;
  Name selector;
  Class class;
  status status;
  int argc;
  const Any *argv;
} send_data_sync;

static void
sdl_in_main_sync_sendv(void *udata)
{ send_data_sync *data = udata;
  Any receiver    = data->receiver;
  Name selector   = data->selector;
  Class class     = data->class;
  int argc        = data->argc;
  const Any *argv = data->argv;
  data->status = vm_send(receiver, selector, class, argc, argv);
}

typedef struct
{ Any receiver;
  Name selector;
  Class class;
  status status;
  int argc;
  Any argv[VA_PCE_MAX_ARGS];
} send_data_async;

static void
sdl_in_main_async_sendv(void *udata)
{ send_data_async *data = udata;
  data->status = vm_send(data->receiver, data->selector,
			 data->class, data->argc, data->argv);
  for(int i=0; i<data->argc; i++)
  { if ( isObject(data->argv[i]) )
      delCodeReference(data->argv[i]);
  }
}

status
sdl_sendv(Any receiver, Name selector, bool sync, Class class,
	  int argc, const Any argv[])
{ if ( SDL_IsMainThread() )
  { return vm_send(receiver, selector, class, argc, argv);
  } else if ( sync )
  { send_data_sync data =
      { .receiver = receiver, .selector = selector,
        .class = class,
        .argc = argc, .argv = argv
      };
    if ( !SDL_RunOnMainThread(sdl_in_main_sync_sendv, &data, true) )
      Cprintf("SDL_RunOnMainThread(): %s\n", SDL_GetError());
    return data.status;
  } else
  { send_data_async *data = malloc(sizeof(*data));
    if ( !data )
      return errorPce(PCE, NAME_notEnoughMemory);
    data->receiver = receiver;
    data->selector = selector;
    data->class    = class;
    data->argc     = argc;
    for(int i=0; i<argc; i++)
    { data->argv[i] = argv[i];
      if ( isObject(data->argv[i]) )
	addCodeReference(data->argv[i]);
    }
    bool rc = SDL_RunOnMainThread(sdl_in_main_async_sendv, data, false);
    if ( !rc )
      Cprintf("SDL_RunOnMainThread(): %s\n", SDL_GetError());
    return rc;
  }
}

/**
 * Send  a message,  executing it  in the  SDL main  thread.  This  is
 * required  for methods  that call  SDL  functions that  may only  be
 * called in the main thread.
 */
status
sdl_send(Any receiver, Name selector, bool sync, ...)
{ va_list args;
  Any argv[VA_PCE_MAX_ARGS];
  int argc;

  va_start(args, sync);
  for(argc=0; (argv[argc] = va_arg(args, Any)) != NULL; argc++)
    assert(argc <= VA_PCE_MAX_ARGS);
  va_end(args);

  return sdl_sendv(receiver, selector, sync, NULL, argc, argv);
}
