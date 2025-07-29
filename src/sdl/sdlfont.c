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

#define P2D(i) ((i)/(double)PANGO_SCALE)

static void clean_width_cache(charwidth_cache *wcache);

static status
ws_init_fonts(void)
{ if ( !ttf_initialized )
  { ttf_initialized = true;
    fontmap = pango_cairo_font_map_get_default();
    // fontmap = pango_ft2_font_map_new();
    context = pango_font_map_create_context(fontmap);
    pango_cairo_context_set_resolution(context, 96.0); /* TBD: Get from SDL */
    g_object_ref(context);
    Real r = getClassVariableValueClass(ClassFont, NAME_scale);
    if ( r )
      font_scale = valReal(r);
  }

  succeed;
}

#define TRUST_PANGO_METRICS 0

#if !TRUST_PANGO_METRICS

static void
dynamic_metrics(FontObj f)
{ WsFont wsf = f->ws_ref;
  PangoGlyphString *glyphs = pango_glyph_string_new();
  PangoAnalysis analysis = {0};
  PangoRectangle ink;
  const char *sample = "blpqgyÉÂ";

  analysis.font = wsf->font;
  pango_shape(sample, -1, &analysis, glyphs);

  int max_ascent = 0;
  int max_descent = 0;

  for (int i = 0; i < glyphs->num_glyphs; ++i)
  { PangoGlyphInfo *gi = &glyphs->glyphs[i];
    pango_font_get_glyph_extents(wsf->font, gi->glyph, &ink, NULL);

    int a = -ink.y;
    int d = (ink.y + ink.height);

    if ( a > max_ascent ) max_ascent = a;
    if ( d > max_descent ) max_descent = d;
  }

  DEBUG(NAME_font,
	Cprintf("%s: Ascent = %.1f; descent = %.1f\n",
		pp(f), P2D(max_ascent), P2D(max_descent)));

  pango_glyph_string_free(glyphs);
  wsf->ascent  = P2D(max_ascent)  + 1.0;
  wsf->descent = P2D(max_descent) + 1.0;
}

#endif/*TRUST_PANGO_METRICS*/

typedef struct
{ PangoWeight pango;
  Name        name;
} weight_name;

static const weight_name weight_names[] =
{ { PANGO_WEIGHT_THIN,       NAME_thin },
  { PANGO_WEIGHT_ULTRALIGHT, NAME_ultralight },
  { PANGO_WEIGHT_LIGHT,      NAME_light },
  { PANGO_WEIGHT_SEMILIGHT,  NAME_semilight },
  { PANGO_WEIGHT_BOOK,       NAME_book },
  { PANGO_WEIGHT_NORMAL,     NAME_normal },
  { PANGO_WEIGHT_MEDIUM,     NAME_medium },
  { PANGO_WEIGHT_BOLD,       NAME_bold },
  { PANGO_WEIGHT_ULTRABOLD,  NAME_ultrabold },
  { PANGO_WEIGHT_HEAVY,      NAME_heavy },
  { PANGO_WEIGHT_ULTRAHEAVY, NAME_ultraheavy },
  { 0, (Name)NULL }
};


/**
 * Create a native font resource associated with the specified FontObj
 * on the given display.
 *
 * @param f Pointer to the FontObj to be created.
 * @param d Pointer to the DisplayObj representing the display context.
 * @return SUCCEED on successful creation; otherwise, FAIL.
 */
status
ws_create_font(FontObj f)
{ BoolObj fixed = DEFAULT;

  if ( f->ws_ref )		/* already done */
    succeed;

  ws_init_fonts();

  PangoFontDescription *desc = pango_font_description_new();
  PangoStyle   slant = PANGO_STYLE_NORMAL;
  PangoWeight weight = PANGO_WEIGHT_NORMAL;
  const char *family;

  fixed = OFF;
  if ( f->style == NAME_normal )
    slant = PANGO_STYLE_NORMAL;
  else if ( f->style == NAME_italic )
    slant = PANGO_STYLE_ITALIC;
  else if ( f->style == NAME_oblique )
    slant = PANGO_STYLE_OBLIQUE;

  if ( isName(f->weight) )
  { weight = PANGO_WEIGHT_NORMAL;
    for(const weight_name *wn = weight_names; wn->name; wn++)
    { if ( wn->name == f->weight )
      { weight = wn->pango;
	break;
      }
    }
  } else
  { weight = valInt(f->weight);
  }

  Name fam = getMemberHashTable(FontFamilyTable, f->family);
  if ( !fam )
    fam = f->family;
  family = nameToUTF8(fam);

  DEBUG(NAME_font, Cprintf("Creating %s using Pango font %s\n",
			   pp(f), family));
  pango_font_description_set_family(desc, family);
  pango_font_description_set_style(desc, slant);
  pango_font_description_set_weight(desc, weight);
  pango_font_description_set_size(desc, valNum(f->points)*PANGO_SCALE*font_scale);

  PangoFont *pf = pango_font_map_load_font(fontmap, context, desc);
  PangoFontMetrics *metrics = pango_font_get_metrics(pf, NULL);
  PangoLayout *layout = pango_layout_new(context);
  pango_layout_set_font_description(layout, desc);

  if ( notDefault(fixed) )
    assign(f, fixed_width, fixed);

  WsFont wsf = alloc(sizeof(ws_font));
  memset(wsf, 0, sizeof(ws_font));
  wsf->references = 1;
  wsf->font       = pf;
  wsf->desc       = desc;
  wsf->layout     = layout;
  f->ws_ref = wsf;

#if TRUST_PANGO_METRICS
  wsf->ascent  = pango_font_metrics_get_ascent(metrics)/(double)PANGO_SCALE;
  wsf->descent = pango_font_metrics_get_descent(metrics)/(double)PANGO_SCALE;
#else
  dynamic_metrics(f);
#endif

  wsf->height  = wsf->ascent + wsf->descent;
  assign(f, ex, toNum(wsf->height/2.0)); /* approximated height of "x" */
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
ws_destroy_font(FontObj f)
{ WsFont wsf = f->ws_ref;
  if ( wsf )
  { f->ws_ref = NULL;
    if ( --wsf->references == 0 )
    { clean_width_cache(&wsf->wcache);
      g_object_unref(wsf->font);
      pango_font_description_free(wsf->desc);
      g_object_unref(wsf->layout);
      unalloc(sizeof(ws_font), wsf);
    }
  }
}

		 /*******************************
		 *            CACHE             *
		 *******************************/

static void
clean_width_cache(charwidth_cache *wcache)
{ for(int page=0; page<256; page++)
  { float *wpage;
    if ( (wpage=wcache->pages[page]) )
    { wcache->pages[page] = NULL;
      unalloc(256*sizeof(*wpage), wpage);
    }
  }
}

bool
s_cwidth(uint32_t c, FontObj font, float *wp)
{ WsFont wsf = font->ws_ref;
  if ( wsf && c <= 0xffff )
  { int page = c/256;
    int idx  = c%256;
    float *wpage = wsf->wcache.pages[page];
    if ( wpage )
    { float w = wpage[idx];
      if ( w >= 0.0 )
      { *wp = w;
	return true;
      }
    }
  }

  return false;
}

bool
s_setcwidth(uint32_t c, FontObj font, float w)
{ WsFont wsf = font->ws_ref;
  if ( wsf && c <= 0xffff )
  { int page = c/256;
    int idx  = c%256;
    float *wpage = wsf->wcache.pages[page];
    if ( !wpage )
    { wpage = wsf->wcache.pages[page] = alloc(256*sizeof(*wpage));
      if ( wpage )
      { for(int i=0; i<256; i++)
	  wpage[i] = -1.0;
      }
    }
    if ( wpage )
    { wpage[idx] = w;
      return true;
    }
  }
  return false;
}

Any
ws_get_pango_property(FontObj f, Name property)
{ WsFont wsf = ws_get_font(f);
  PangoFontDescription *desc = pango_font_describe(wsf->font);
  Any rval = NULL;

  if ( property == NAME_description )
  { char *pname = pango_font_description_to_string(desc);
    rval = UTF8ToName(pname);
    g_free(pname);
  } else if ( property == NAME_family )
  { const char *family = pango_font_description_get_family(desc);
    rval = UTF8ToName(family);
  } else if ( property == NAME_style )
  { PangoStyle style = pango_font_description_get_style(desc);
    switch(style)
    { case PANGO_STYLE_NORMAL:  rval = NAME_normal;  break;
      case PANGO_STYLE_OBLIQUE: rval = NAME_oblique; break;
      case PANGO_STYLE_ITALIC:  rval = NAME_italic;  break;
      default: break;
    }
  } else if ( property == NAME_weight )
  { PangoWeight weight = pango_font_description_get_weight(desc);
    for(const weight_name *wn = weight_names; wn->name; wn++)
    { if ( wn->pango == weight )
      { rval = wn->name;
	break;
      }
    }
    if ( !rval )
      rval = toInt(weight);
  } else if ( property == NAME_size )
  { int size = pango_font_description_get_size(desc);
    bool size_is_absolute = pango_font_description_get_size_is_absolute(desc);

    if ( size_is_absolute )
    { rval = toInt(size);
    } else
    { rval = toNum(P2D(size));
    }
  }
  pango_font_description_free(desc);

  return rval;
}



		 /*******************************
		 *         LIST FONTS           *
		 *******************************/

/**
 * Get information about all available fonts.
 * @return a sheet mapping family names to their properties.  Each
 * family is a sheet with the properties
 *   - monospace: @on
 *     Present if this is a monospace font
 *   - variable: @on
 *     Present if this is a variable font
 *   - faces: sheet
 *     This sheet maps face nams to a face description sheet with
 *     properties
 *     - synthesized: @on
 *       Present if the face is synthesized.
 *     - description: a `name` holding the Pango description
 */

Sheet
ws_font_families(BoolObj mono)
{ Sheet xfonts = answerObject(ClassSheet, EAV);
  PangoFontFamily **families;
  int n_families;

  ws_init_fonts();

  pango_font_map_list_families(fontmap, &families, &n_families);

  for(int i = 0; i < n_families; i++)
  { PangoFontFamily *family = families[i];
    bool is_monospace = pango_font_family_is_monospace(family);

    if ( notDefault(mono) && isOn(mono) != is_monospace )
      continue;

    const char *family_name = pango_font_family_get_name(family);
#if PANGO_VERSION_CHECK(1,44,0)
    bool is_variable = pango_font_family_is_variable(family);
#endif

    Sheet xfaces = newObject(ClassSheet, EAV);
    Sheet xfam = newObject(ClassSheet, EAV);
    valueSheet(xfam, NAME_faces, xfaces);
    valueSheet(xfonts, UTF8ToName(family_name), xfam);

    if ( is_monospace )
      valueSheet(xfam, NAME_monospace, ON);
#if PANGO_VERSION_CHECK(1,44,0)
    if ( is_variable )
      valueSheet(xfam, NAME_variable, ON);
#endif

    PangoFontFace **faces;
    int n_faces;
    pango_font_family_list_faces(family, &faces, &n_faces);

    for(int j = 0; j < n_faces; j++)
    { const char *face_name = pango_font_face_get_face_name(faces[j]);
      bool is_synthesized = pango_font_face_is_synthesized(faces[j]);
      PangoFontDescription *desc = pango_font_face_describe(faces[j]);
      gchar *desc_str = pango_font_description_to_string(desc);

      Sheet xface = newObject(ClassSheet, EAV);
      valueSheet(xfaces, UTF8ToName(face_name), xface);
      valueSheet(xface, NAME_description, UTF8ToName(desc_str));
      if ( is_synthesized )
	valueSheet(xface, NAME_synthesized, ON);

      g_free(desc_str);
      pango_font_description_free(desc);
    }

    g_free(faces);
  }

  g_free(families);
  answer(xfonts);
}
