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
#include <math.h>
#include "sdlfont.h"
#include "sdldisplay.h"

static bool ttf_initialized = false;
static PangoFontMap *fontmap;	/* Per surface type (screen, PDF, ... */
static PangoContext *context;	/* Per DPI and fontmap */
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
    fontmap = pango_cairo_font_map_get_default();
    context = pango_font_map_create_context(fontmap);
    g_object_ref(context);
    Real r = getClassVariableValueObject(f, NAME_scale);
    if ( r )
      font_scale = valReal(r);
  }

  if ( f->ws_ref )		/* already done */
    succeed;

  PangoFontDescription *desc = pango_font_description_new();
  PangoStyle   slant = PANGO_STYLE_NORMAL;
  PangoWeight weight = PANGO_WEIGHT_NORMAL;
  const char *family = "sans";

  if ( f->style == NAME_bold )
    weight = PANGO_WEIGHT_BOLD;
  else if ( f->style == NAME_oblique )
    slant = PANGO_STYLE_ITALIC;

  if ( f->family == NAME_courier || f->family == NAME_screen )
    family = "monospace";
  else if ( f->family == NAME_times )
    family = "serif";
  else
    family = nameToUTF8(f->family);

  pango_font_description_set_family(desc, family);
  pango_font_description_set_style(desc, slant);
  pango_font_description_set_weight(desc, weight);

  PangoFont *pf = pango_font_map_load_font(fontmap, context, desc);
  PangoFontMetrics *metrics = pango_font_get_metrics(font, NULL);

  assign(f, fixed_width, pango_font_metrics_get_is_monospace(metrics) ? ON : OFF);

  WsFont wsf = alloc(sizeof(ws_font));
  memset(wsf, 0, sizeof(ws_font));
  wsf->font    = desc;
  wsf->ascent  = pango_font_metrics_get_ascent(metrics)/PANGO_SCALE;
  wsf->descent = pango_font_metrics_get_descent(metrics)/PANGO_SCALE;
  wsf->height  = wsf->ascent + wsf->descent;
  f->ws_ref = wsf;
  assign(f, ex, toInt(wsf->height/2)); /* approximation */
  pango_font_metrics_unref(metrics);

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
    pango_font_description_free(wsf->font);
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
