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

#ifndef RAYFRAME_H
#define RAYFRAME_H
#include <SDL3/SDL.h>

typedef struct
{ SDL_Window   *ws_window;	/* The SDL3 Window handle */
  SDL_Renderer *ws_renderer;	/* The Window's renderer */
  SDL_WindowID  ws_id;		/* Integer identifier for the event */
  Uint64	 flash_end_ms;	/* SDL_GetTicks() deadline; 0 = not flashing */
  SDL_FRect	 flash_rect;	/* overlay area (renderer coords); zero = full frame */
#ifdef __WINDOWS__
  HWND		hwnd;		/* Windows handle */
#endif
} ws_frame, *WsFrame;

/* Functions shared with SDL backend */
FrameObj wsid_to_frame(SDL_WindowID id);
WsFrame sdl_frame(FrameObj fr, bool create);
bool ws_draw_frame(FrameObj fr);
Uint32 SDLCALL flash_end_callback(void *userdata, SDL_TimerID id, Uint32 interval);
bool sdl_frame_event(SDL_Event *ev);
void ws_redraw_changed_frames(void);
bool ws_window_frame_position(Any window, FrameObj fr, float *ox, float *oy);
status ws_enable_text_input(Graphical gr, BoolObj enable);

/* Functions shared with core */
status ws_created_frame(FrameObj fr);
void ws_uncreate_frame(FrameObj fr);
status ws_create_frame(FrameObj fr);
void ws_raise_frame(FrameObj fr);
status ws_frame_bb(FrameObj fr, int *x, int *y, int *w, int *h);
void ws_x_geometry_frame(FrameObj fr, Name spec, DisplayObj dsp);
status ws_geometry_frame(FrameObj fr, Int x, Int y, Int w, Int h, DisplayObj dsp);
void ws_busy_cursor_frame(FrameObj fr, CursorObj c);
void ws_frame_cursor(FrameObj fr, CursorObj cursor);
void ws_status_frame(FrameObj fr, Name status);
void ws_set_label_frame(FrameObj fr);
Image ws_image_of_frame(FrameObj fr);

#endif /* RAYFRAME_H */
