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

#ifndef RAYWINDOW_H
#define RAYWINDOW_H
#include "SDL3/SDL.h"
#include <cairo/cairo.h>

typedef struct
{ cairo_surface_t *backing;
  int w;
  int h;
} ws_window, *WsWindow;

status ws_created_window(PceWindow sw);
void ws_uncreate_window(PceWindow sw);
status ws_create_window(PceWindow sw, PceWindow parent);
void ws_manage_window(PceWindow sw);
void ws_unmanage_window(PceWindow sw);
void ws_reassociate_ws_window(PceWindow from, PceWindow to);
void ws_geometry_window(PceWindow sw, int x, int y, int w, int h, int pen);
void ws_topmost_window(PceWindow sw, BoolObj topmost);
void ws_grab_pointer_window(PceWindow sw, BoolObj val);
void ws_grab_keyboard_window(PceWindow sw, BoolObj val);
void ws_ungrab_all(void);
void ws_flash_area_window(PceWindow sw, int x, int y, int w, int h, int msecs);
void ws_flash_window(PceWindow sw, int msecs);
void ws_move_pointer(PceWindow sw, int x, int y);
void ws_window_cursor(PceWindow sw, CursorObj cursor);
void ws_window_background(PceWindow sw, Any c);
void ws_raise_window(PceWindow sw);
void ws_lower_window(PceWindow sw);
int ws_enable_window(PceWindow sw, int enable);
Int ws_window_thread(PceWindow sw);
int ws_delayed_redraw_window(PceWindow sw);

#endif /* RAYWINDOW_H */
