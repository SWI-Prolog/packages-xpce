/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  1999-2011, University of Amsterdam
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

static FontObj
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
drawTBox(TBox tb, int x, int y, int w)
{ FontObj f = getFontTBox(tb);
  Style s = tb->style;
  Colour old_colour = NULL;

  if ( notDefault(s->colour) )
    old_colour = r_colour(s->colour);

  s_print_aligned(&tb->text->data, x, y, f);
  if ( s->attributes & TXT_UNDERLINED )
  { r_thickness(1);
    r_dash(NAME_none);
    r_line(x, y+1, x+w, y+1);
  }

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
          2, tbox_termnames,
          "$Rev$");


status
makeClassTBox(Class class)
{ return declareClass(class, &tbox_decls);
}
