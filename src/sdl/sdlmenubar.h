/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org
    Copyright (c)  2026, SWI-Prolog Solutions b.v.
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

#ifndef SDLMENUBAR_H
#define SDLMENUBAR_H
#include <SDL3/SDL.h>

/* Native (MacOS) menu bar support.  The XPCE `menu_bar' object remains
 * the model; on MacOS it is not drawn.  Instead its `popup' members are
 * mirrored into the NSMenu that MacOS shows at the top of the screen
 * for the frame that has the input focus.  Selecting an item is routed
 * back through the normal `popup->execute' path, so no XPCE
 * application needs to change.
 *
 * The Cocoa half lives in sdlnsmenu.m; see sdlnsmenu.h for why it is a
 * separate file.  On other platforms all of this compiles away and the
 * menu_bar is drawn by RedrawAreaMenuBar() as before.
 */

#ifdef __APPLE__

bool	ws_has_native_menubar(MenuBar mb);
void	ws_menubar_changed(MenuBar mb);
void	ws_menubar_destroyed(MenuBar mb);
void	ws_menubar_activate_frame(FrameObj fr);
bool	ws_menubar_event(SDL_Event *ev);
bool	ws_menubar_key_equivalent(SDL_Event *ev);

#else /*__APPLE__*/

static inline bool ws_has_native_menubar(MenuBar mb)
{ (void)mb;
  return false;
}
static inline void ws_menubar_changed(MenuBar mb)
{ (void)mb;
}
static inline void ws_menubar_destroyed(MenuBar mb)
{ (void)mb;
}
static inline void ws_menubar_activate_frame(FrameObj fr)
{ (void)fr;
}
static inline bool ws_menubar_event(SDL_Event *ev)
{ (void)ev;
  return false;
}
static inline bool ws_menubar_key_equivalent(SDL_Event *ev)
{ (void)ev;
  return false;
}

#endif /*__APPLE__*/

#endif /* SDLMENUBAR_H */
