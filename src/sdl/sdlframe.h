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
} ws_frame, *WsFrame;

/* Functions shared with DSL backend */
FrameObj wsid_to_frame(SDL_WindowID id);
bool sdl_frame_event(SDL_Event *ev);
void ws_redraw_changed_frames(void);

/* Functions shared with core */
status ws_created_frame(FrameObj fr);
void ws_uncreate_frame(FrameObj fr);
status ws_create_frame(FrameObj fr);
void ws_realise_frame(FrameObj fr);
PceWindow ws_window_holding_point_frame(FrameObj fr);
void ws_raise_frame(FrameObj fr);
void ws_lower_frame(FrameObj fr);
status ws_attach_wm_prototols_frame(FrameObj fr);
status setDndAwareFrame(FrameObj fr);
void ws_frame_cursor(FrameObj fr, CursorObj cursor);
void ws_grab_frame_pointer(FrameObj fr, BoolObj grab, CursorObj cursor);
status ws_frame_bb(FrameObj fr, int *x, int *y, int *w, int *h);
void ws_x_geometry_frame(FrameObj fr, Name spec, Monitor mon);
void ws_geometry_frame(FrameObj fr, Int x, Int y, Int w, Int h, Monitor mon);
void ws_border_frame(FrameObj fr, int b);
void ws_busy_cursor_frame(FrameObj fr, CursorObj c);
void ws_frame_background(FrameObj fr, Any c);
void ws_set_icon_frame(FrameObj fr);
void ws_set_icon_label_frame(FrameObj fr);
void ws_set_icon_position_frame(FrameObj fr, int x, int y);
status ws_get_icon_position_frame(FrameObj fr, int *x, int *y);
void ws_enable_modal(FrameObj fr, BoolObj val);
void ws_status_frame(FrameObj fr, Name status);
void ws_topmost_frame(FrameObj fr, BoolObj topmost);
void ws_set_label_frame(FrameObj fr);
Image ws_image_of_frame(FrameObj fr);
void ws_transient_frame(FrameObj fr, FrameObj fr2);
status ws_postscript_frame(FrameObj fr, int iscolor);
Int ws_frame_thread(FrameObj fr);
int ws_enable_frame(FrameObj fr, int enable);

#endif /* RAYFRAME_H */
