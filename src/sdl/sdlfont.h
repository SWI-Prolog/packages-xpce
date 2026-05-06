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

#ifndef SDLFONT_H
#define SDLFONT_H
#include <cairo/cairo.h>
#include <pango/pango.h>
#include <pango/pangocairo.h>

/* Pango fixed point to Cairo double dimensions*/
#define P2D(i) ((i)/(double)PANGO_SCALE)

typedef struct
{ float *pages[256];
} charwidth_cache;

typedef struct
{ int first;
  int last;
  bool computed;
} font_domain;

typedef struct
{ PangoFont *font;
  PangoFontDescription *desc;
  PangoLayout *layout;		/* Should be per display/surface type */
  double ul_thickness;		/* Underline thinkness */
  double ul_position;		/* Underline position */
  charwidth_cache wcache;
  font_domain dom_main;		/* Cached domain of primary font */
  font_domain dom_family;	/* Cached domain over fontset/fallbacks */
  int references;		/* for cloning */
} ws_font, *WsFont;

status ws_create_font(FontObj f);
void   ws_destroy_font(FontObj f);
bool   s_cwidth(uint32_t c, FontObj font, float *wp);
bool   s_setcwidth(uint32_t c, FontObj font, float w);
bool   s_has_char_family(FontObj f, unsigned int c);
void   f_domain(FontObj f, bool family, int *a, int *z);
Sheet  ws_font_families(BoolObj mono);
Any    ws_get_pango_property(FontObj f, Name property);

static inline WsFont
ws_get_font(FontObj f)
{ WsFont wsf = f->ws_ref;
  if ( !wsf && !ws_create_font(f) )
    return NULL;
  return f->ws_ref;
}

static inline WsFont
ws_clone_ws_font(WsFont wsf)
{ wsf->references++;
  return wsf;
}

static inline PangoLayout *
cairo_font(FontObj f)
{ WsFont wsf = ws_get_font(f);
  return wsf ? wsf->layout : NULL;
}

		 /*******************************
		 *        FONT FAMILIES         *
		 *******************************/

#ifndef MONO_FAMILY
#ifdef __WINDOWS__
#define MONO_FAMILY "Consolas,Courier New,Cascadia Mono," \
		    "MS Gothic,Malgun Gothic,NSimSun," \
		    "Segoe UI Symbol,Segoe UI Emoji,monospace"
#elif __APPLE__
#define MONO_FAMILY "Menlo,monospace"
#else
#define MONO_FAMILY "Noto Sans Mono,DejaVu Sans Mono,monospace"
#endif
#endif

#ifndef SANS_FAMILY
#ifdef __WINDOWS__
#define SANS_FAMILY "Segoe UI,Verdana," \
		    "Yu Gothic UI,Malgun Gothic,Microsoft YaHei UI," \
		    "Segoe UI Symbol,Segoe UI Emoji,sans"
#elif __APPLE__
#define SANS_FAMILY "sans"
#else
#define SANS_FAMILY "Noto Sans,DejaVu Sans,sans"
#endif
#endif

#ifndef SERIF_FAMILY
#ifdef __WINDOWS__
#define SERIF_FAMILY "Georgia,Times New Roman," \
		     "MS Mincho,Batang,SimSun," \
		     "Segoe UI Symbol,serif"
#elif __APPLE__
#define SERIF_FAMILY "serif"
#else
#define SERIF_FAMILY "Noto Serif,DejaVu Serif,serif"
#endif
#endif

#endif /* SDLFONT_H */
