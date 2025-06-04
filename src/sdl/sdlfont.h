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

typedef struct
{ PangoFontDescription *font;
  PangoLayout *layout;		/* Should be per display/surface type */
  double ascent;
  double descent;
  double height;
} ws_font, *WsFont;

static inline WsFont
ws_get_font(FontObj f)
{ WsFont wsf = f->ws_ref;
  if ( !wsf && !ws_create_font(f, DEFAULT) )
    return NULL;
  return f->ws_ref;
}

static inline PangoLayout *
cairo_font(FontObj f)
{ WsFont wsf = ws_get_font(f);
  return wsf ? wsf->layout : NULL;
}

status ws_create_font(FontObj f, DisplayObj d);
void ws_destroy_font(FontObj f, DisplayObj d);
status ws_system_fonts(DisplayObj d);

#endif /* SDLFONT_H */
