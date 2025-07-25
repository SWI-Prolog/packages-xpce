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
#include <cairo/cairo.h>

typedef struct
{ SDL_DisplayID    id;
  SDL_Window      *hidden_window;
  SDL_Renderer    *hidden_renderer;
  cairo_surface_t *hidden_surface;
  cairo_t         *hidden_cairo;
} ws_display, *WsDisplay;

float ws_pixel_density_display(Any obj);
Name  ws_get_system_theme_display(DisplayObj d);

bool ws_init_displays(void);
DisplayObj dsp_id_to_display(SDL_DisplayID id);
bool sdl_display_event(SDL_Event *ev);
void ws_bell_display(DisplayObj d, int volume);
void ws_get_size_display(DisplayObj d, int *w, int *h);
int ws_depth_display(DisplayObj d);
bool ws_resolution_display(DisplayObj d, int *rx, int *ry);
void ws_activate_screen_saver(DisplayObj d);
void ws_deactivate_screen_saver(DisplayObj d);
void ws_close_display(DisplayObj d);
void ws_foreground_display(DisplayObj d, Colour c);
void ws_background_display(DisplayObj d, Colour c);
status ws_events_queued_display(DisplayObj d);
status ws_selection_display(DisplayObj d, Name which, StringObj data);
Any ws_get_selection(DisplayObj d, Name which, Name target);
status ws_own_selection(DisplayObj d, Name selection, Name type);
Name ws_window_manager(DisplayObj d);
void ws_synchronous(DisplayObj d);
void ws_asynchronous(DisplayObj d);
status ws_postscript_display(DisplayObj d, int iscolor);
Image ws_grab_image_display(DisplayObj d, int x, int y, int width, int height);

#endif /* RAYDISPLAY_H */
