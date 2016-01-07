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
#include <h/unix.h>
#include <h/text.h>

static status	highlightStyle(Style s, BoolObj on);
static status	underlineStyle(Style s, BoolObj on);
static status	greyStyle(Style s, BoolObj on);
static status	hiddenStyle(Style s, BoolObj on);

static status
initialiseStyle(Style s, Image icon, FontObj font, Colour colour,
		BoolObj highlight, BoolObj underline, BoolObj bold, BoolObj grey,
		Any background, BoolObj hidden, Int lm, Int rm)
{ if ( isDefault(icon) ) icon = NIL;
  if ( isDefault(lm) )   lm = ZERO;
  if ( isDefault(rm) )   rm = ZERO;

  assign(s, font,         font);
  assign(s, icon,         icon);
  assign(s, colour,       colour);
  assign(s, background,	  background);
  assign(s, left_margin,  lm);
  assign(s, right_margin, rm);
  s->attributes = 0;

  if ( notDefault(highlight) ) highlightStyle(s, highlight);
  if ( notDefault(underline) ) underlineStyle(s, underline);
  if ( notDefault(bold) )      boldStyle(s, bold);
  if ( notDefault(grey) )      greyStyle(s, grey);
  if ( notDefault(hidden) )    hiddenStyle(s, hidden);

  succeed;
}


static status
storeStyle(Style s, FileObj file)
{ storeSlotsObject(s, file);
  storeIntFile(file, toInt(s->attributes));

  succeed;
}


static status
loadStyle(Style s, IOSTREAM *fd, ClassDef def)
{ loadSlotsObject(s, fd, def);
  s->attributes = loadWord(fd);
  if ( s->font == NIL )			/* prior version 10 */
    assign(s, font, DEFAULT);
  if ( s->colour == NIL )
    assign(s, colour, DEFAULT);

  succeed;
}


static status
attribute_style(Style s, unsigned char att, BoolObj on)
{ if ( on == ON )
  { if ( s->attributes && att )
      succeed;
    s->attributes |= att;
  } else
  { if ( !(s->attributes && att) )
      succeed;
    s->attributes &= ~att;
  }

  succeed;
}


static status
highlightStyle(Style s, BoolObj on)
{ return attribute_style(s, TXT_HIGHLIGHTED, on);
}


static status
underlineStyle(Style s, BoolObj on)
{ return attribute_style(s, TXT_UNDERLINED, on);
}


static status
greyStyle(Style s, BoolObj on)
{ return attribute_style(s, TXT_GREYED, on);
}


status
boldStyle(Style s, BoolObj on)
{ return attribute_style(s, TXT_BOLDEN, on);
}


static status
hiddenStyle(Style s, BoolObj on)
{ return attribute_style(s, TXT_HIDDEN, on);
}


static BoolObj
get_attribute_style(Style s, unsigned char att)
{ return (s->attributes & att) ? ON : OFF;
}


static BoolObj
getHighlightStyle(Style s)
{ return get_attribute_style(s, TXT_HIGHLIGHTED);
}


static BoolObj
getUnderlineStyle(Style s)
{ return get_attribute_style(s, TXT_UNDERLINED);
}


static BoolObj
getGreyStyle(Style s)
{ return get_attribute_style(s, TXT_GREYED);
}


static BoolObj
getBoldStyle(Style s)
{ return get_attribute_style(s, TXT_BOLDEN);
}


static BoolObj
getHiddenStyle(Style s)
{ return get_attribute_style(s, TXT_HIDDEN);
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_initialise[] =
        { "icon=[image]*", "font=[font]", "colour=[colour]", "highlight=[bool]", "underline=[bool]", "bold=[bool]", "grey=[bool]", "background=[colour|pixmap|elevation]", "hidden=[bool]", "left_margin=[int]", "right_margin=[int]" };

/* Instance Variables */

static vardecl var_style[] =
{ IV(NAME_font, "[font]", IV_BOTH,
     NAME_appearance, "Font of characters"),
  IV(NAME_colour, "[colour]", IV_BOTH,
     NAME_appearance, "Colour of the characters"),
  IV(NAME_background, "[colour|pixmap|elevation]", IV_BOTH,
     NAME_appearance, "Background for the characters"),
  IV(NAME_icon, "image*", IV_BOTH,
     NAME_appearance, "Image for annotation margin"),
  IV(NAME_leftMargin, "int", IV_BOTH,
     NAME_appearance, "Left margin for wrapping (relative to current)"),
  IV(NAME_rightMargin, "int", IV_BOTH,
     NAME_appearance, "Right margin for wrapping (relative to current)"),
  IV(NAME_attributes, "alien:long", IV_NONE,
     NAME_appearance, "Character attributes")
};

/* Send Methods */

static senddecl send_style[] =
{ SM(NAME_initialise, 11, T_initialise, initialiseStyle,
     DEFAULT, "Create from icon, font, colour and attributes"),
  SM(NAME_bold, 1, "bool", boldStyle,
     NAME_appearance, "Bold text"),
  SM(NAME_grey, 1, "bool", greyStyle,
     NAME_appearance, "Greyed-out text"),
  SM(NAME_hidden, 1, "bool", hiddenStyle,
     NAME_appearance, "Make text invisible"),
  SM(NAME_highlight, 1, "bool", highlightStyle,
     NAME_appearance, "Inverse video"),
  SM(NAME_underline, 1, "bool", underlineStyle,
     NAME_appearance, "Underlined text")
};

/* Get Methods */

static getdecl get_style[] =
{ GM(NAME_bold, 0, "bool", NULL, getBoldStyle,
     NAME_appearance, "Boolean to indicate bold"),
  GM(NAME_grey, 0, "bool", NULL, getGreyStyle,
     NAME_appearance, "Boolean to indicate grey"),
  GM(NAME_hidden, 0, "bool", NULL, getHiddenStyle,
     NAME_appearance, "Boolean to indicate invisible text"),
  GM(NAME_highlight, 0, "bool", NULL, getHighlightStyle,
     NAME_appearance, "Boolean to indicate inverse video"),
  GM(NAME_underline, 0, "bool", NULL, getUnderlineStyle,
     NAME_appearance, "Boolean to indicate underline")
};

/* Resources */

#define rc_style NULL
/*
static classvardecl rc_style[] =
{
};
*/

/* Class Declaration */

static Name style_termnames[] = { NAME_icon, NAME_font };

ClassDecl(style_decls,
          var_style, send_style, get_style, rc_style,
          2, style_termnames,
          "$Rev$");

status
makeClassStyle(Class class)
{ declareClass(class, &style_decls);
  setLoadStoreFunctionClass(class, loadStyle, storeStyle);

  succeed;
}

