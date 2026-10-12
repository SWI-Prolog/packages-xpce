/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org
    Copyright (c)  2026, SWI-Prolog Solutions b.v.
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

/* Class `markup_text' displays (read-only) text formatted using Pango
 * markup, e.g., "Some <b>bold</b> text".  Unlike class text, it can
 * mix fonts and colours, and it uses Pango for wrapping, alignment,
 * justification and ellipsizing.  The layout itself is computed by the
 * backend: see r_markup() in sdldraw.c.
 */

static status markupMarkupText(MarkupText t, CharArray markup);

static void
markup_spec_text(MarkupText t, markup_spec *m)
{ m->markup	  = t->markup;
  m->font	  = t->font;
  m->width	  = isNil(t->width) ? 0.0 : valNum(t->width);
  m->alignment	  = t->alignment;
  m->justify	  = (t->justify == ON);
  m->ellipsize	  = t->ellipsize;
  m->line_spacing = valNum(t->line_spacing);
}


static status
initialiseMarkupText(MarkupText t, CharArray markup, Int width, FontObj font)
{ initialiseGraphical(t, ZERO, ZERO, ZERO, ZERO);

  if ( isDefault(font) )
    font = getClassVariableValueObject(t, NAME_font);

  assign(t, markup,	  CtoString(""));
  assign(t, font,	  font);
  assign(t, width,	  isDefault(width) ? NIL : width);
  assign(t, alignment,	  NAME_left);
  assign(t, justify,	  OFF);
  assign(t, ellipsize,	  NAME_none);
  assign(t, line_spacing, ZERO);

  return markupMarkupText(t, markup);
}


static status
computeMarkupText(MarkupText t)
{ if ( notNil(t->request_compute) )
  { markup_spec m;
    double w = 0.0, h = 0.0;

    markup_spec_text(t, &m);
    r_markup_extents(&m, &w, &h);
    CHANGING_GRAPHICAL(t,
		       assign(t->area, w, toNum(w));
		       assign(t->area, h, toNum(h));
		       changedEntireImageGraphical(t));

    assign(t, request_compute, NIL);
  }

  succeed;
}


static status
RedrawAreaMarkupText(MarkupText t, Area a)
{ int x, y, w, h;
  markup_spec m;

  initialiseDeviceGraphical(t, &x, &y, &w, &h);
  markup_spec_text(t, &m);
  r_markup(&m, x, y);

  return RedrawAreaGraphical(t, a);
}


static status
markupMarkupText(MarkupText t, CharArray markup)
{ StringObj error;

  if ( !ws_parse_markup(markup, NULL, &error) )
    return errorPce(t, NAME_syntaxError, error);

  assign(t, markup, markup);
  return requestComputeGraphical(t, DEFAULT);
}


/* Setters for the slots that change the layout.
 */

#define LAYOUT_SETTER(func, slot, type) \
static status \
func(MarkupText t, type value) \
{ if ( t->slot != value ) \
  { assign(t, slot, value); \
    requestComputeGraphical(t, DEFAULT); \
  } \
  succeed; \
}

LAYOUT_SETTER(fontMarkupText,	     font,	   FontObj)
LAYOUT_SETTER(widthMarkupText,	     width,	   Int)
LAYOUT_SETTER(alignmentMarkupText,   alignment,	   Name)
LAYOUT_SETTER(justifyMarkupText,     justify,	   BoolObj)
LAYOUT_SETTER(ellipsizeMarkupText,   ellipsize,	   Name)
LAYOUT_SETTER(lineSpacingMarkupText, line_spacing, Num)


static status
geometryMarkupText(MarkupText t, Int x, Int y, Int w, Int h)
{ if ( notDefault(w) && w != t->width )
  { assign(t, width, w);
    requestComputeGraphical(t, DEFAULT);
  }

  return geometryGraphical(t, x, y, DEFAULT, DEFAULT);
}


static StringObj
getTextMarkupText(MarkupText t)
{ StringObj plain;

  if ( ws_parse_markup(t->markup, &plain, NULL) )
    answer(plain);

  fail;
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

static char *T_initialise[] =
	{ "markup=char_array", "width=[int]*", "font=[font]" };
static char *T_geometry[] =
	{ "x=[int]", "y=[int]", "width=[int]", "height=[int]" };

static vardecl var_markup_text[] =
{ SV(NAME_markup, "char_array", IV_GET|IV_STORE, markupMarkupText,
     NAME_contents, "Text with Pango markup"),
  SV(NAME_font, "font", IV_GET|IV_STORE, fontMarkupText,
     NAME_appearance, "Font for text without a font in the markup"),
  SV(NAME_width, "int*", IV_GET|IV_STORE, widthMarkupText,
     NAME_layout, "Wrap lines at this width (@nil: do not wrap)"),
  SV(NAME_alignment, "{left,center,right}", IV_GET|IV_STORE,
     alignmentMarkupText,
     NAME_layout, "Alignment of the lines"),
  SV(NAME_justify, "bool", IV_GET|IV_STORE, justifyMarkupText,
     NAME_layout, "Stretch wrapped lines to the width"),
  SV(NAME_ellipsize, "{none,start,middle,end}", IV_GET|IV_STORE,
     ellipsizeMarkupText,
     NAME_layout, "Where to put `...' if a line does not fit the width"),
  SV(NAME_lineSpacing, "num", IV_GET|IV_STORE, lineSpacingMarkupText,
     NAME_layout, "Distance between baselines as factor of the font's (0: font's)")
};

static senddecl send_markup_text[] =
{ SM(NAME_initialise, 3, T_initialise, initialiseMarkupText,
     DEFAULT, "Create from markup, wrap width and font"),
  SM(NAME_compute, 0, NULL, computeMarkupText,
     DEFAULT, "Compute the size"),
  SM(NAME_geometry, 4, T_geometry, geometryMarkupText,
     DEFAULT, "Move; a width sets <-width (the height is computed)")
};

static getdecl get_markup_text[] =
{ GM(NAME_text, 0, "string", NULL, getTextMarkupText,
     NAME_contents, "The text without the markup")
};

static classvardecl rc_markup_text[] =
{ RC(NAME_font, "font", "normal",
     "Default font")
};

static Name markup_text_termnames[] = { NAME_markup, NAME_width };

ClassDecl(markup_text_decls,
	  var_markup_text, send_markup_text, get_markup_text, rc_markup_text,
	  2, markup_text_termnames);


status
makeClassMarkupText(Class class)
{ declareClass(class, &markup_text_decls);
  setRedrawFunctionClass(class, RedrawAreaMarkupText);

  succeed;
}
