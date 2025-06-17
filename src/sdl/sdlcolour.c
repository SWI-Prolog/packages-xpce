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

static HashTable ColourNames;		/* name --> rgb (packed in Int) */

#define ws_system_colours(d) (void)0

#include "../msw/xcolours.c"	/* get x11_colours */

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
 * Create a native color resource associated with the specified Colour
 * object on the given display.
 *
 * @param c Pointer to the Colour object to be created.
 * @param d Pointer to the DisplayObj representing the display context.
 * @return SUCCEED on successful creation; otherwise, FAIL.
 */
status
ws_create_colour(Colour c, DisplayObj d)
{ (void)d;

  if ( c->kind == NAME_named )
  { Int Rgb = getNamedRGB(c->name);

    if ( Rgb )
    { COLORRGBA rgb = (COLORRGBA) valInt(Rgb);
      int r = ColorRValue(rgb) * 257;
      int g = ColorGValue(rgb) * 257;
      int b = ColorBValue(rgb) * 257;

      assign(c, red,   toInt(r));
      assign(c, green, toInt(g));
      assign(c, blue,  toInt(b));

      c->ws_ref = color2wsref(rgb);
    } else
      fail;
  } else
  { COLORRGBA rgb = RGBA(valInt(c->red)/256,
			 valInt(c->green)/256,
			 valInt(c->blue)/256,
			 255);

    c->ws_ref = color2wsref(rgb);
  }

  succeed;
}

/**
 * Destroy the native color resource associated with the specified Colour object on the given display.
 *
 * @param c Pointer to the Colour object to be destroyed.
 * @param d Pointer to the DisplayObj representing the display context.
 */
void
ws_uncreate_colour(Colour c, DisplayObj d)
{ (void)d;

  c->ws_ref = NULL;
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
 * Retrieve a color by its name from the specified display.
 *
 * @param d Pointer to the DisplayObj representing the display context.
 * @param name Pointer to the Name object representing the color name.
 * @return SUCCEED if the color is found; otherwise, FAIL.
 */
status
ws_colour_name(DisplayObj d, Name name)
{ HashTable ht = LoadColourNames();

  return ( getMemberHashTable(ht, name) ||
	   getMemberHashTable(ht, canonical_colour_name(name)) );
}

/**
 * Convert a pixel value to its corresponding Colour object on the specified display.
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

/**
 * Generate a color cube with the specified size in the given ColourMap.
 *
 * @param cm Pointer to the ColourMap object where the color cube will be created.
 * @param size The size of the color cube to be generated.
 */
void
ws_colour_cube(ColourMap cm, int size)
{
}

/**
 * Map all colors in the specified ColourMap to their native representations.
 *
 * @param cm Pointer to the ColourMap object to be mapped.
 */
void
ws_colour_map_colours(ColourMap cm)
{
}

/**
 * Create a native color map associated with the specified ColourMap object on the given display.
 *
 * @param cm Pointer to the ColourMap object to be created.
 * @param d Pointer to the DisplayObj representing the display context.
 * @return SUCCEED on successful creation; otherwise, FAIL.
 */
status
ws_create_colour_map(ColourMap cm, DisplayObj d)
{
    return SUCCEED;
}

/**
 * Destroy the native color map associated with the specified ColourMap object on the given display.
 *
 * @param cm Pointer to the ColourMap object to be destroyed.
 * @param d Pointer to the DisplayObj representing the display context.
 * @return SUCCEED on successful destruction; otherwise, FAIL.
 */
status
ws_uncreate_colour_map(ColourMap cm, DisplayObj d)
{
    return SUCCEED;
}

/**
 * Unlink the specified ColourMap from its associated display or resources.
 *
 * @param cm Pointer to the ColourMap object to be unlinked.
 * @return SUCCEED on successful unlinking; otherwise, FAIL.
 */
status
ws_unlink_colour_map(ColourMap cm)
{
    return SUCCEED;
}
