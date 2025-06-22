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

static int sdl_main_thread = 0;

int
setPceThread(void)
{ if ( !sdl_main_thread )
  { sdl_main_thread = PL_thread_self();
    DEBUG(NAME_thread,
	  Cprintf("SDL_Init() on thread %d\n", sdl_main_thread));

    SDL_SetAppMetadataProperty(SDL_PROP_APP_METADATA_NAME_STRING, "xpce");
    SDL_SetAppMetadataProperty(SDL_PROP_APP_METADATA_VERSION_STRING, PCE_VERSION);
    SDL_SetAppMetadataProperty(SDL_PROP_APP_METADATA_IDENTIFIER_STRING, "org.swi_prolog.xpce");
    SDL_SetAppMetadataProperty(SDL_PROP_APP_METADATA_CREATOR_STRING, "Jan Wielemaker,Anjo Anjewierden");
    SDL_SetAppMetadataProperty(SDL_PROP_APP_METADATA_COPYRIGHT_STRING, "Copyright 1992-2007, University of Amsterdam");
    SDL_SetAppMetadataProperty(SDL_PROP_APP_METADATA_URL_STRING, "http://www.swi-prolog.org/packages/xpce/");

    if ( !SDL_Init(SDL_INIT_EVENTS|SDL_INIT_VIDEO) )
      return errorPce(NIL, NAME_sdlInitialize);
    ChangedFrames = globalObject(NAME_changedFrames, ClassChain, EAV);
    start_fd_watcher_thread();
    if ( !openDisplay(CurrentDisplay(NIL)) )
      return errorPce(CurrentDisplay(NIL), NAME_sdlInitialize);

    assign(PCE, window_system_version,  toInt(ws_version()));
    assign(PCE, window_system_revision, toInt(ws_revision()));
    assign(PCE, window_system_driver,   CtoName(ws_driver()));
  }

  return true;
}

bool
sdl_initialised(void)
{ return !!sdl_main_thread;
}


/**
 * Retrieve the major version number of the SDL backend.
 *
 * @return Integer representing the version number.
 */
int
ws_version(void)
{ return SDL_GetVersion() / 1000000;
}

/**
 * Retrieve the revision number of the Raylib backend.
 *
 * @return Integer representing the revision.
 */
int
ws_revision(void)
{ return (SDL_GetVersion() / 1000)%100;
}

const char *
ws_driver(void)
{ return SDL_GetCurrentVideoDriver();
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
#ifndef __WINDOWS__
Int
ws_default_scrollbar_width(void)
{ return toInt(16);
}
#endif

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


status
ws_open_url(PceString url)
{ const char *u = stringToUTF8(url, NULL);
  return SDL_OpenURL(u);
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
 *
 * @param sync defines whether the method is executed synchronously.  This
 * is an int rather than a bool because bool is not allowed as argument to
 * va_start().
 */
status
sdl_send(Any receiver, Name selector, int sync, ...)
{ va_list args;
  Any argv[VA_PCE_MAX_ARGS];
  int argc;

  va_start(args, sync);
  for(argc=0; (argv[argc] = va_arg(args, Any)) != NULL; argc++)
    assert(argc <= VA_PCE_MAX_ARGS);
  va_end(args);

  return sdl_sendv(receiver, selector, sync, NULL, argc, argv);
}
