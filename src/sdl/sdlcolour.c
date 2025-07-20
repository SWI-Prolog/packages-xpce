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

#include <h/kernel.h>
#include <h/graphics.h>
#include "sdlcolour.h"
#ifdef __WINDOWS__
#include <msw/mscolour.h>
#else
#define ws_system_colours(cn) (void)0
#endif

static HashTable ColourNames;		/* name --> rgb (packed in Int) */

#include "csscolours.c"			/* get X11/CSS colour names */

static Name	canonical_colour_name(Name in);

Int
getNamedRGB(Name name)
{ Int Rgb;

  HashTable ht = LoadColourNames();

  if ( (Rgb = getMemberHashTable(ht, name)) ||
       (Rgb = getMemberHashTable(ht, canonical_colour_name(name))) )
    return Rgb;

  fail;
}

/**
 * If a colour is a named colour, fill its rgba.
 *
 * @param c Pointer to the Colour object to be created.
 * @return SUCCEED on successful creation; otherwise, FAIL.
 */
status
ws_named_colour(Colour c)
{ if ( isDefault(c->rgba) )
  { if ( c->kind == NAME_named )
    { Int Rgb = getNamedRGB(c->name);

      if ( Rgb )
      { assign(c, rgba, Rgb);
	succeed;
      }
    }

    Cprintf("%s: not named or no existing name (using grey50)\n", pp(c));
    assign(c, rgba, RGBA(127,127,127,255));

    fail;
  }

  succeed;
}

static Name
canonical_colour_name(Name in)
{ char *s = strName(in);
  char buf[100];
  int left = sizeof(buf);
  char *q = buf;
  int changed = 0;

  for( ; *s && --left > 0; s++, q++ )
  { if ( *s == ' ' )
    { *q = '_';
      changed++;
    } else if ( isupper(*s) )
    { *q = tolower(*s);
      changed++;
    } else
      *q = *s;
  }

  if ( left && changed )
  { *q = EOS;
    return CtoKeyword(buf);
  }

  return in;
}

/**
 * Convert a pixel value to its corresponding Colour object on the
 * specified display.
 *
 * @param d Pointer to the DisplayObj representing the display context.
 * @param pixel The pixel value to be converted.
 * @return Pointer to the corresponding Colour object; NULL if conversion fails.
 */
Colour
ws_pixel_to_colour(DisplayObj d, unsigned long pixel)
{
    return NULL;
}
