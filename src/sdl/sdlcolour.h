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
#include <assert.h>

/* Windows API Emulation */
typedef uint32_t COLORRGBA;
#define RGBA(r,g,b,a) ((((COLORRGBA)a)<<24)| \
		       (((COLORRGBA)r)<<16)| \
		       (((COLORRGBA)g)<<8) | \
			((COLORRGBA)b))
#define RGB(r,g,b) RGBA(r,g,b,255)
#define GetRValue(rgb) (((rgb)>>16)&0xff)
#define GetGValue(rgb) (((rgb)>> 8)&0xff)
#define GetBValue(rgb) (((rgb)>> 0)&0xff)
#define GetAValue(rgb) (((rgb)>>24)&0xff)

#define WS_COLOR_CREATED (((uintptr_t)1) << 32)

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
{ static_assert(sizeof(uintptr_t) > sizeof(uint32_t),
		"Assumes 64 bits");
  return (void*)(((uintptr_t)i)|WS_COLOR_CREATED);
}

static inline SDL_Color
wsref2SDL_Color(WsRef r)
{ COLORRGBA rgb = (COLORRGBA)(intptr_t)r;
  SDL_Color c = { .r = GetRValue(rgb),
		  .g = GetGValue(rgb),
		  .b = GetBValue(rgb),
		  .a = GetAValue(rgb)
                };
  return c;
}

static inline SDL_Color
pceColour2SDL_Color(Colour c)
{ WsRef r = c->ws_ref;
  if ( r == NULL )
  { ws_create_colour(c, DEFAULT);
    r = c->ws_ref;
    if ( !r )
    { Cprintf("Failed to get colour from %s\n", pp(c));
      SDL_Color replace = {.b=255, .a=255};
      return replace;
    }
  }

  return wsref2SDL_Color(r);
}

Int getNamedRGB(Name name);

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
