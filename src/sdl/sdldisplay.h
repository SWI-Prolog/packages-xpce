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

#ifndef RAYDISPLAY_H
#define RAYDISPLAY_H
#include <SDL3/SDL.h>

typedef struct
{ SDL_Window   *hidden_window;
  SDL_Renderer *hidden_renderer;
} ws_display, *WsDisplay;


void ws_flush_display(DisplayObj d);
void ws_synchronise_display(DisplayObj d);
void ws_bell_display(DisplayObj d, int volume);
void ws_get_size_display(DisplayObj d, int *w, int *h);
Name ws_get_visual_type_display(DisplayObj d);
int ws_depth_display(DisplayObj d);
int ws_resolution_display(DisplayObj d, int *rx, int *ry);
void ws_activate_screen_saver(DisplayObj d);
void ws_deactivate_screen_saver(DisplayObj d);
void ws_init_display(DisplayObj d);
status ws_legal_display_name(const char *s);
status ws_opened_display(DisplayObj d);
void ws_open_display(DisplayObj d);
void ws_quit_display(DisplayObj d);
status ws_init_graphics_display(DisplayObj d);
status ws_init_monitors_display(DisplayObj d);
void ws_foreground_display(DisplayObj d, Colour c);
void ws_background_display(DisplayObj d, Colour c);
void ws_draw_in_display(DisplayObj d, Graphical gr, BoolObj invert, BoolObj subtoo);
void ws_grab_server(DisplayObj d);
void ws_ungrab_server(DisplayObj d);
Int ws_display_connection_number(DisplayObj d);
status ws_events_queued_display(DisplayObj d);
status ws_pointer_location_display(DisplayObj d, int *x, int *y);
status ws_set_cutbuffer(DisplayObj d, int n, PceString s);
StringObj ws_get_cutbuffer(DisplayObj d, int n);
unsigned long ws_get_selection_timeout(void);
void ws_set_selection_timeout(unsigned long time);
Any ws_get_selection(DisplayObj d, Name which, Name target);
void ws_disown_selection(DisplayObj d, Name selection);
status ws_own_selection(DisplayObj d, Name selection, Name type);
Name ws_window_manager(DisplayObj d);
void ws_synchronous(DisplayObj d);
void ws_asynchronous(DisplayObj d);
status ws_postscript_display(DisplayObj d, int iscolor);
Image ws_grab_image_display(DisplayObj d, int x, int y, int width, int height);

#endif /* RAYDISPLAY_H */
