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

#ifndef SDL_H
#define SDL_H
#include <SDL3/SDL.h>
#include <SWI-Prolog.h>		/* Debugging: PL_thread_self() */

#define O_HDP 1			/* High Density Display support */

#define ASSERT_SDL_MAIN()					 \
  do {								 \
    if ( !SDL_IsMainThread() ) {				 \
      Cprintf("Warning: %s:%d: Not called in SDL main thread\n", \
	      __FILE__, __LINE__);				 \
      not_on_sdl_main_thread();	/* Allow GDB breakpoint */	 \
    }								 \
  } while(0)

status	sdl_send(Any receiver, Name selector, int sync, ...);
bool	sdl_initialised(void);
void	not_on_sdl_main_thread(void);

void ws_initialise(int argc, char **argv);
int ws_version(void);
int ws_revision(void);
const char *ws_driver(void);
status ws_show_console(Name how);
status ws_console_label(CharArray label);
Int ws_default_scrollbar_width(void);
char *ws_user(void);
bool pceMTdetach(void);
status ws_open_url(PceString url);

#endif /* SDL_H */
