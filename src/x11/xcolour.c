/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  1985-2002, University of Amsterdam
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
#include "include.h"

#define XBRIGHT ((1<<16)-1)

static char *
x_colour_name(Name name)
{ static char buf[200];
  char *e = &buf[sizeof(buf)-1];
  char *s, *q;

  for( s = strName(name), q = buf; *s && q<e; s++, q++ )
  { if ( *s == '_' || *s == syntax.word_separator )
      *q = ' ';
    else
      *q = tolower(*s);
  }
  *q = EOS;

  return buf;
}


status
ws_create_colour(Colour c, DisplayObj d)
{ DisplayWsXref r = d->ws_ref;
  Display *display = r->display_xref;
  XColor exact;

  if ( isDefault(c->red) || isDefault(c->green) || isDefault(c->blue) )
  { XColor *screen = alloc(sizeof(XColor));

    if ( XAllocNamedColor(display, r->colour_map, x_colour_name(c->name),
			  screen, &exact) )
    { assign(c, red,   toInt(exact.red));
      assign(c, green, toInt(exact.green));
      assign(c, blue,  toInt(exact.blue));

      return registerXrefObject(c, d, (XtPointer) screen);
    } else
      XParseColor(display, r->colour_map, x_colour_name(c->name), &exact);
  } else
  { exact.red   = valInt(c->red);
    exact.green = valInt(c->green);
    exact.blue  = valInt(c->blue);

    if ( XAllocColor(display, r->colour_map, &exact) )
    { XColor *color = alloc(sizeof(XColor));

      *color = exact;
      return registerXrefObject(c, d, (XtPointer) color);
    }
  }

  if ( allocNearestColour(display, r->colour_map, r->depth,
			  get(d, NAME_visualType, EAV),
			  &exact) )
  { XColor *color = alloc(sizeof(XColor));

    *color = exact;
    assign(c, red,   toInt(exact.red));
    assign(c, green, toInt(exact.green));
    assign(c, blue,  toInt(exact.blue));

    errorPce(c, NAME_replacedColour);

    return registerXrefObject(c, d, (XtPointer) color);
  }

  return errorPce(c, NAME_xOpen, d);
}


void
ws_uncreate_colour(Colour c, DisplayObj d)
{ Xref xr;

  while( (xr = unregisterXrefObject(c, d)) )
  { DisplayWsXref r = xr->display->ws_ref;
    XColor *xc = xr->xref;

    XFreeColors(r->display_xref, r->colour_map, &xc->pixel, 1, 0);
  }
}


status
ws_colour_name(DisplayObj d, Name name)
{ XColor edr, sdr;
  DisplayWsXref r;

  openDisplay(d);
  r = d->ws_ref;
  if ( XLookupColor(r->display_xref, r->colour_map, x_colour_name(name),
		    &edr, &sdr) )
    succeed;

  fail;
}


Colour
ws_pixel_to_colour(DisplayObj d, unsigned long pixel)
{ for_hash_table(ColourTable, s,
		 { Colour c = s->value;
		   XColor *color = (XColor *) getExistingXrefObject(c, d);

		   if ( color && color->pixel == pixel )
		     answer(c);
		 });

  fail;
}

		 /*******************************
		 *	     COLOURMAPS		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ColourMap handling functions (TBD)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
ws_colour_cube(ColourMap cm, int size)
{
}


void
ws_colour_map_colours(ColourMap cm)
{
}


status
ws_create_colour_map(ColourMap cm, DisplayObj d)
{ fail;
}


status
ws_uncreate_colour_map(ColourMap cm, DisplayObj d)
{ fail;
}


status
ws_unlink_colour_map(ColourMap cm)
{ succeed;
}
