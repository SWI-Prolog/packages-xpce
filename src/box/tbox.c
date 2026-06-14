/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org/projects/xpce/
    Copyright (c)  1999-2025, University of Amsterdam
			      SWI-Prolog Solutions b.v.
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

#include "boxes.h"

static status computeTBox(TBox tb);

status
initialiseTBox(TBox tb, CharArray text, Style style)
{ obtainClassVariablesObject(tb);

  assign(tb, text,  text);
  if ( notDefault(style) )
    assign(tb, style, style);

  return computeTBox(tb);
}

		 /*******************************
		 *	      COMPUTE		*
		 *******************************/

FontObj
getFontTBox(TBox tb)
{ if ( notDefault(tb->style->font) )
    answer(tb->style->font);

  answer(getClassVariableValueObject(tb, NAME_font));
}


static status
computeTBox(TBox tb)
{ FontObj f = getFontTBox(tb);

  assign(tb, width,   getWidthFont(f, tb->text));
  assign(tb, ascent,  getAscentFont(f));
  assign(tb, descent, getDescentFont(f));

  succeed;
}


HBox
getSpaceHBoxFont(FontObj f)
{ return answerObject(ClassHBox,
		      getAdvanceFont(f, (CharArray)name_space),
		      getAscentFont(f),
		      getDescentFont(f),
		      findGlobal(NAME_spaceRubber),
		      EAV);
}


static HBox
getSpaceTBox(TBox tb)
{ FontObj f = getFontTBox(tb);

  return getSpaceHBoxFont(f);
}


		 /*******************************
		 *	      REDRAW		*
		 *******************************/

void
drawTBox(TBox tb, int x, int y, int w, parline const *line)
{ drawTBoxSel(tb, x, y, w, line, 0, 0, NIL);
}


static void
print_substring(PceString s, int offset, int len, int x, int y, FontObj f)
{ if ( len <= 0 )
    return;
  if ( isstrA(s) )
    s_printA(s->s_textA + offset, len, x, y, f);
  else
    s_printW(s->s_textW + offset, len, x, y, f);
}


void
drawTBoxSel(TBox tb, int x, int y, int w, parline const *line,
	    int sel_from, int sel_to, Style sel_style)
{ FontObj f = getFontTBox(tb);
  Style s = tb->style;
  Colour old_colour = NULL;
  PceString str = &tb->text->data;
  int len = str->s_size;

  if ( notDefault(s->colour) )
    old_colour = r_colour(s->colour);

  int ly = y - line->ascent;
  int lh = line->ascent + line->descent;

  if ( notDefault(s->background) )
  { Colour obg = r_background(s->background);
    r_clear(x, ly, w, lh);
    r_background(obg);
  } else
  { r_clear(x, ly, w, lh);
  }

  if ( sel_from < 0 )    sel_from = 0;
  if ( sel_to   > len )  sel_to   = len;

  if ( notNil(sel_style) && sel_from < sel_to )
  { int sx    = (int)str_advance(str, 0, sel_from, f);
    int sel_w = (int)str_advance(str, sel_from, sel_to, f);

    if ( notDefault(sel_style->background) )
      r_fill(x+sx, ly, sel_w, lh, sel_style->background);

    print_substring(str, 0, sel_from, x, y, f);

    Any sel_fg = NULL;
    if ( notDefault(sel_style->colour) )
      sel_fg = r_colour(sel_style->colour);
    print_substring(str, sel_from, sel_to - sel_from, x+sx, y, f);
    if ( sel_fg )
      r_colour(sel_fg);

    if ( sel_to < len )
      print_substring(str, sel_to, len - sel_to, x+sx+sel_w, y, f);
  } else
  { s_print_aligned(str, x, y, f);
  }

  if ( s->underline != OFF && notDefault(s->underline) )
    r_underline(f, x, y, w, s->underline, NAME_none);

  if ( old_colour )
    r_colour(old_colour);
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declaractions */

static char *T_initialise[] =
        { "text=char_array", "style=[style]" };

/* Instance Variables */

static vardecl var_tbox[] =
{ IV(NAME_text, "char_array", IV_GET,
     NAME_content, "Represented text"),
  IV(NAME_style, "style", IV_GET,
     NAME_appearance, "Appearance of the text")
};

/* Send Methods */

static senddecl send_tbox[] =
{ SM(NAME_initialise, 2, T_initialise, initialiseTBox,
     DEFAULT, "Create tbox from text and style")
};

/* Get Methods */

static getdecl get_tbox[] =
{ GM(NAME_space, 0, "hbox", NULL, getSpaceTBox,
     NAME_layout, "Yield hbox for a space compatible with tbox")
};

/* Resources */

static classvardecl rc_tbox[] =
{ RC(NAME_style, "style", "style()", "Appearance of text"),
  RC(NAME_font,  "font",  "normal",  "Font if <-style has no font")
};

/* Class Declaration */

static Name tbox_termnames[] = { NAME_text, NAME_style };

ClassDecl(tbox_decls,
          var_tbox, send_tbox, get_tbox, rc_tbox,
          2, tbox_termnames);


status
makeClassTBox(Class class)
{ return declareClass(class, &tbox_decls);
}
