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
#define ColorRValue(rgb) (((rgb)>>16)&0xff)
#define ColorGValue(rgb) (((rgb)>> 8)&0xff)
#define ColorBValue(rgb) (((rgb)>> 0)&0xff)
#define ColorAValue(rgb) (((rgb)>>24)&0xff)

#define WS_COLOR_CREATED (((uintptr_t)1) << 32)

status ws_named_colour(Colour c);

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

static inline SDL_Color
rgba2SDL_Color(COLORRGBA rgb)
{ SDL_Color c = { .r = ColorRValue(rgb),
		  .g = ColorGValue(rgb),
		  .b = ColorBValue(rgb),
		  .a = ColorAValue(rgb)
                };
  return c;
}

static inline SDL_Color
pceColour2SDL_Color(Colour c)
{ ws_named_colour(c);

  return rgba2SDL_Color(valInt(c->rgba));
}

Int getNamedRGB(Name name);

Colour ws_pixel_to_colour(DisplayObj d, unsigned long pixel);

#endif /* RAYCOLOUR_H */
