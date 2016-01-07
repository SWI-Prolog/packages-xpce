/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org/packages/xpce/
    Copyright (c)  1985-2005, University of Amsterdam
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

static status	defaultPostScriptFont(FontObj f);
static Int	getPointsFont(FontObj f);

static Name
fontName(Name family, Name style, Int points)
{ string s;
  Any av[3];
  Name rc;

  av[0] = family;
  av[1] = style;
  av[2] = points;

  str_writefv(&s, (CharArray)CtoTempString("%s_%s_%d"), 3, av);

  rc = StringToName(&s);
  str_unalloc(&s);

  return rc;
}


static status
initialiseFont(FontObj f, Name family, Name style, Int points, Name xname)
{ Name name = fontName(family, style, points);

  assign(f, family,      family);
  assign(f, style,       style);
  assign(f, points,      points);
  assign(f, fixed_width, DEFAULT);
  assign(f, iswide,	 DEFAULT);
  assign(f, x_name,      xname);

  defaultPostScriptFont(f);

  protectObject(f);
  newAssoc(name, f);

  return appendHashTable(FontTable, name, f);
}


static FontObj
getLookupFont(Class class, Name family, Name style, Int points)
{ Name name = fontName(family, style, points);
  FontObj f2;

  makeBuiltinFonts();
  if ( (f2 = getMemberHashTable(FontTable, name)) )
    answer(f2);

  fail;
}


static FontObj
getConvertFont(Class class, Name name)
{ char *s = strName(name);

  makeBuiltinFonts();

  if ( s[0] == '@' )
  { Name ref_name;

    for(s++; *s == ' ' || *s == '\t'; s++)
      ;
    ref_name = CtoKeyword(s);

    answer(getMemberHashTable(FontTable, ref_name));
  } else
  { DisplayObj d = CurrentDisplay(NIL);
    FontObj f;
    Name fn = (syntax.uppercase ? CtoKeyword(s) : name);

    if ( d && (f = getMemberHashTable(d->font_table, fn)) )
    { answer(f);
    } else
    { for_hash_table(FontTable, sy,
		     { FontObj f = sy->value;
		       if ( f->x_name == fn ) /* case? */
			 answer(f);
		     })
    }
  }

  fail;
}


status
replaceFont(FontObj f, DisplayObj d)
{ FontObj nofont;
  void *wsref;

  if ( !(nofont = getClassVariableValueObject(d, NAME_noFont)) )
    errorPce(f, NAME_noDefaultFont);

  if ( !(wsref = getXrefObject(nofont, d)) )
    fail;

  errorPce(f, NAME_replacedFont, nofont);
  registerXrefObject(f, d, wsref);

  assign(f, fixed_width, nofont->fixed_width);

  succeed;
}


static int XopenNesting = 0;

static status
XopenFont(FontObj f, DisplayObj d)
{ if ( isDefault(d) )
    d = CurrentDisplay(f);

  makeBuiltinFonts();

  if ( !ws_create_font(f, d) )
  { errorPce(f, NAME_noRelatedXFont);
    if ( XopenNesting <= 2 )
    { status rc;

      XopenNesting++;
      rc = replaceFont(f, d);
      XopenNesting--;
      if ( rc )
	succeed;
    }

    fail;
  }

  succeed;
}


static status
XcloseFont(FontObj f, DisplayObj d)
{ ws_destroy_font(f, d);

  succeed;
}


status
makeBuiltinFonts(void)
{ DisplayObj d;
  static int done = FALSE;

  if ( done )
    succeed;
  done = TRUE;

  if ( (d = CurrentDisplay(NIL)) &&
       send(d, NAME_loadFonts, EAV) &&	/* XPCE predefined fonts */
       ws_system_fonts(d) &&		/* Window-system fonts */
       send(d, NAME_loadFontAliases, NAME_systemFonts, EAV) )
  { send(d, NAME_loadFontAliases, NAME_userFonts, EAV);
    succeed;
  }

  fail;
}

		/********************************
		*          POSTSCRIPT		*
		********************************/

static status
defaultPostScriptFont(FontObj f)
{ char buf[LINESIZE];

  if ( f->family == NAME_helvetica )
  { strcpy(buf, "Helvetica");

    if ( f->style == NAME_bold )
      strcat(buf, "-Bold");
    else if ( f->style == NAME_oblique )
      strcat(buf, "-Oblique");
  } else if ( f->family == NAME_times )
  { strcpy(buf, "Times");

    if ( f->style == NAME_bold )
      strcat(buf, "-Bold");
    else if ( f->style == NAME_italic )
      strcat(buf, "-Italic");
    else /*if ( f->style == NAME_roman )*/
      strcat(buf, "-Roman");
  } else if ( f->style == NAME_ansiVar )
  { strcpy(buf, "Helvetica");
  } else				/* default */
  { strcpy(buf, "Courier");

    if ( f->style == NAME_bold )
      strcat(buf, "-Bold");
    else if ( f->style == NAME_oblique )
      strcat(buf, "-Oblique");
  }

  assign(f, postscript_size, getPointsFont(f));
  assign(f, postscript_font, CtoName(buf));

  succeed;
}


		/********************************
		*           GET INFO		*
		********************************/


Int
getWidthFont(FontObj f, CharArray txt)
{ if ( isDefault(txt) )
    txt = (CharArray) NAME_x;

  d_ensure_display();			/* TBD */

  answer(toInt(str_width(&txt->data, 0, txt->data.s_size, f)));
}


Int
getAdvanceFont(FontObj f, CharArray txt)
{ d_ensure_display();			/* TBD */

  return toInt(str_advance(&txt->data, 0, txt->data.s_size, f));
}


Int
getExFont(FontObj f)
{ if ( isNil(f->ex) )
    assign(f, ex, toInt(c_width('x', f)));

  answer(f->ex);
}


Int
getHeightFont(FontObj f)
{ d_ensure_display();

  answer(toInt(s_height(f)));
}


Int
getAscentFont(FontObj f)
{ d_ensure_display();

  answer(toInt(s_ascent(f)));
}


Int
getDescentFont(FontObj f)
{ d_ensure_display();

  answer(toInt(s_descent(f)));
}


static Size
getSizeFont(FontObj f)
{ answer(answerObject(ClassSize, getExFont(f), getHeightFont(f), EAV));
}


BoolObj
getFixedWidthFont(FontObj f)
{ if ( isDefault(f->fixed_width) )
  { getXrefObject(f, CurrentDisplay(NIL));

    if ( c_width('x', f) == c_width('W', f) )
      assign(f, fixed_width, ON);
    else
      assign(f, fixed_width, OFF);
  }

  answer(f->fixed_width);
}


BoolObj
getB16Font(FontObj f)
{ if ( isDefault(f->iswide) )
    XopenFont(f, CurrentDisplay(NIL));

  answer(f->iswide);
}


static status
memberFont(FontObj f, Int chr)
{ d_ensure_display();

  if ( s_has_char(f, valInt(chr)) )
    succeed;

  fail;
}


static Int
getDefaultCharacterFont(FontObj f)
{ d_ensure_display();

  answer(toInt(s_default_char(f)));
}


static Tuple
getDomainFont(FontObj f, Name which)
{ int a, z;

  if ( isDefault(which) )
    which = NAME_x;

  f_domain(f, which, &a, &z);
  return answerObject(ClassTuple, toInt(a), toInt(z), EAV);
}


static Int
getPointsFont(FontObj f)
{ if ( notDefault(f->points) )
    answer(f->points);

  answer(getHeightFont(f));
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_initialise[] =
        { "family=name", "style=name", "points=[int]", "x_name=[name]" };
static char *T_lookup[] =
        { "name", "name", "[int]" };

/* Instance Variables */

static vardecl var_font[] =
{ IV(NAME_family, "name", IV_GET,
     NAME_name, "Family the font belongs to (times, etc.)"),
  IV(NAME_style, "name", IV_GET,
     NAME_name, "Style of the font (bold, italic, etc.)"),
  IV(NAME_points, "[int]", IV_NONE,
     NAME_name, "Point-size of the font"),
  IV(NAME_ex, "int*", IV_NONE,
     NAME_dimension, "Width of the letter `x' in this font"),
  IV(NAME_xName, "[name]", IV_GET,
     NAME_x, "Window-system name for the font"),
  IV(NAME_fixedWidth, "[bool]", IV_NONE,
     NAME_property, "If @off, font is proportional"),
  IV(NAME_b16, "[bool]", IV_NONE,
     NAME_property, "If @on, font is a 16-bit font"),
  IV(NAME_postscriptFont, "name", IV_BOTH,
     NAME_postscript, "PostScript-name of the font"),
  IV(NAME_postscriptSize, "int", IV_BOTH,
     NAME_postscript, "PostScript point-size of the font")
};

/* Send Methods */

static senddecl send_font[] =
{ SM(NAME_initialise, 4, T_initialise, initialiseFont,
     DEFAULT, "Create from fam, style, points, name"),
  SM(NAME_member, 1, "char", memberFont,
     NAME_set, "Test if font defines character"),
  SM(NAME_Xclose, 1, "display", XcloseFont,
     NAME_x, "Destroy associated window-system resources"),
  SM(NAME_Xopen, 1, "display", XopenFont,
     NAME_x, "Open the associated window-system resources")
};

/* Get Methods */

static getdecl get_font[] =
{ GM(NAME_points, 0, "int", NULL, getPointsFont,
     DEFAULT, "Specified point-size or <-height"),
  GM(NAME_convert, 1, "font", "name", getConvertFont,
     NAME_conversion, "Convert logical font-name and @family_style_points"),
  GM(NAME_ascent, 0, "int", NULL, getAscentFont,
     NAME_dimension, "Highest point above baseline"),
  GM(NAME_descent, 0, "int", NULL, getDescentFont,
     NAME_dimension, "Lowest point below baseline"),
  GM(NAME_ex, 0, "int", NULL, getExFont,
     NAME_dimension, "Width of the letter `x'"),
  GM(NAME_height, 0, "int", NULL, getHeightFont,
     NAME_dimension, "Height of highest character in font"),
  GM(NAME_size, 0, "size", NULL, getSizeFont,
     NAME_dimension, "New size from <-width and <-height"),
  GM(NAME_width, 1, "int", "[char_array]", getWidthFont,
     NAME_dimension, "Width of string (default \"x\")"),
  GM(NAME_advance, 1, "int", "char_array", getAdvanceFont,
     NAME_dimension, "X-origin advancement of string"),
  GM(NAME_b16, 0, "bool", NULL, getB16Font,
     NAME_encoding, "Boolean to indicate font is 16-bits"),
  GM(NAME_lookup, 3, "font", T_lookup, getLookupFont,
     NAME_oms, "Lookup in @fonts table"),
  GM(NAME_defaultCharacter, 0, "char", NULL, getDefaultCharacterFont,
     NAME_property, "Character painted for non-existing entries"),
  GM(NAME_domain, 1, "tuple", "[{x,y}]", getDomainFont,
     NAME_property, "Range of valid characters"),
  GM(NAME_fixedWidth, 0, "bool", NULL, getFixedWidthFont,
     NAME_property, "Boolean to indicate font is fixed-width")
};

/* Resources */

static classvardecl rc_font[] =
{ RC(NAME_scale, "real", UXWIN("1.0", "1.4"),
     "Multiplication factor for all fonts")
};

/* Class Declaration */

static Name font_termnames[] = { NAME_family, NAME_style, NAME_points };

ClassDecl(font_decls,
          var_font, send_font, get_font, rc_font,
          3, font_termnames,
          "$Rev$");


status
makeClassFont(Class class)
{ declareClass(class, &font_decls);

  saveStyleClass(class, NAME_external);
  cloneStyleClass(class, NAME_none);

  FontTable = globalObject(NAME_fonts, ClassHashTable, toInt(101), EAV);

  succeed;
}
