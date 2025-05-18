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
{ if ( !SDL_Init(SDL_INIT_EVENTS) )
    errorPce(NIL, NAME_sdlInitialize);
  ChangedFrames = globalObject(NAME_changedFrames, ClassChain, EAV);
  start_fd_watcher_thread();
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
