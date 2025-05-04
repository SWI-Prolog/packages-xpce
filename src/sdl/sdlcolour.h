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

#ifndef RAYCOLOUR_H
#define RAYCOLOUR_H
#include <SDL3/SDL.h>

typedef struct
{ Uint8 r;
  Uint8 g;
  Uint8 b;
  Uint8 a;
} sdl_color;

typedef union
{ sdl_color color;
  uint32_t  asint;
} cvt_color;

static inline WsRef
color2wsref(uint32_t i)
{ return (void*)(intptr_t)i;
}

static inline sdl_color
wsref2color(WsRef r)
{ cvt_color cvt = { .asint = (int32_t)(intptr_t)r };
  return cvt.color;
}


status ws_create_colour(Colour c, DisplayObj d);
void ws_uncreate_colour(Colour c, DisplayObj d);
status ws_colour_name(DisplayObj d, Name name);
Colour ws_pixel_to_colour(DisplayObj d, unsigned long pixel);
void ws_colour_cube(ColourMap cm, int size);
void ws_colour_map_colours(ColourMap cm);
status ws_create_colour_map(ColourMap cm, DisplayObj d);
status ws_uncreate_colour_map(ColourMap cm, DisplayObj d);
status ws_unlink_colour_map(ColourMap cm);

#endif /* RAYCOLOUR_H */
