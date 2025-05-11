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
#include <stdbool.h>
#include "sdlfont.h"
#include "sdldisplay.h"

static bool   ttf_initialized = false;
static double font_scale = 1.0;

/**
 * Create a native font resource associated with the specified FontObj
 * on the given display.
 *
 * @param f Pointer to the FontObj to be created.
 * @param d Pointer to the DisplayObj representing the display context.
 * @return SUCCEED on successful creation; otherwise, FAIL.
 */
status
ws_create_font(FontObj f, DisplayObj d)
{ if ( isDefault(d) )
    d = CurrentDisplay(NIL);

  if ( !ttf_initialized )
  { if ( !openDisplay(d) )
      fail;

    ttf_initialized = true;
    Real r = getClassVariableValueObject(f, NAME_scale);
    if ( r )
      font_scale = valReal(r);
  }

  if ( f->ws_ref )		/* already done */
    succeed;

  cairo_font_face_t *face = cairo_toy_font_face_create(
    "monospace",
    CAIRO_FONT_SLANT_NORMAL,
    CAIRO_FONT_WEIGHT_NORMAL);
  if ( !face )
    fail;

  WsDisplay wsd = d->ws_ref;
  cairo_t *cr = wsd->hidden_cairo;
  cairo_set_font_face(cr, face);
  cairo_set_font_size(cr, (double)valInt(f->points)*font_scale);
  cairo_scaled_font_t *ttf = cairo_get_scaled_font(cr);
  ttf = cairo_scaled_font_reference(ttf);
  cairo_font_extents_t extents;
  cairo_text_extents_t xextents, wextents;
  cairo_font_extents(cr, &extents);
  cairo_text_extents(cr, "x", &xextents);
  cairo_text_extents(cr, "w", &wextents);

  WsFont wsf = alloc(sizeof(ws_font));
  memset(wsf, 0, sizeof(ws_font));
  wsf->font    = ttf;
  wsf->ascent  = extents.ascent;
  wsf->descent = extents.descent;
  wsf->height  = extents.height;
  f->ws_ref = wsf;
  assign(f, ex, toInt(xextents.height));
  assign(f, fixed_width, xextents.width==wextents.width ? ON : OFF);

  succeed;
}

/**
 * Destroy the native font resource associated with the specified
 * FontObj on the given display.
 *
 * @param f Pointer to the FontObj to be destroyed.
 * @param d Pointer to the DisplayObj representing the display context.
 */
void
ws_destroy_font(FontObj f, DisplayObj d)
{ WsFont wsf = f->ws_ref;
  if ( wsf )
  { f->ws_ref = NULL;
    cairo_scaled_font_destroy(wsf->font);
    unalloc(sizeof(ws_font), wsf);
  }
}

/**
 * Initialize or enumerate the system fonts available on the specified display.
 *
 * @param d Pointer to the DisplayObj representing the display context.
 * @return SUCCEED if fonts were successfully enumerated or loaded; otherwise, FAIL.
 */
status
ws_system_fonts(DisplayObj d)
{
    return SUCCEED;
}
