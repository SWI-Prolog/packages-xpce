/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org/packages/xpce/
    Copyright (c)  1985-2025, University of Amsterdam
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

#include <h/kernel.h>
#include <h/graphics.h>

static status	defaultPostScriptFont(FontObj f);
static Int	getPointsFont(FontObj f);
static void	attach_font_families(Class class);
static status	loadFonts(void);
static status	loadFontAliases(Name res);
static status	fontAlias(Name name, FontObj font, BoolObj force);

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
  { FontObj f;
    Name fn = (syntax.uppercase ? CtoKeyword(s) : name);

    if ( (f=getMemberHashTable(FontAliasTable, fn)) )
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

  if ( XopenNesting > 1 )
    fail;

  XopenNesting++;
  if ( !ws_create_font(f, d) )
  { status rc;

    errorPce(f, NAME_noRelatedXFont);
    rc = replaceFont(f, d);
    XopenNesting--;

    return rc;
  }

  XopenNesting--;
  succeed;
}


static status
XcloseFont(FontObj f, DisplayObj d)
{ ws_destroy_font(f, d);

  succeed;
}


status
makeBuiltinFonts(void)
{ static int done = FALSE;

  if ( done )
    succeed;
  done = TRUE;

  if ( loadFonts() &&			/* XPCE predefined fonts */
       loadFontAliases(NAME_systemFonts) )
  { loadFontAliases(NAME_userFonts);
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

  answer(toNum(str_width(&txt->data, 0, txt->data.s_size, f)));
}


Int
getAdvanceFont(FontObj f, CharArray txt)
{ d_ensure_display();			/* TBD */

  return toNum(str_advance(&txt->data, 0, txt->data.s_size, f));
}


Int
getExFont(FontObj f)
{ if ( !isInteger(f->ex) )
    XopenFont(f, CurrentDisplay(NIL));

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
    XopenFont(f, CurrentDisplay(NIL));

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


		/********************************
		*          FONT TABLES		*
		********************************/

static status
loadFontFamily(Name fam)
{ if ( !getClassVariableClass(ClassFont, fam) )
    attach_class_variable(ClassFont, fam, "chain", "[]", "Font family set");

  if ( !getClassVariableValueClass(ClassFont, fam) )
    return errorPce(ClassFont, NAME_noFontsInFamily, fam);

  succeed;
}


static status
loadFonts(void)
{ Chain fams;
  static bool done = false;

  if ( done )
    succeed;
  done = true;

  if ( (fams = getClassVariableValueClass(ClassFont, NAME_fontFamilies)) )
  { Cell cell;

    for_cell(cell, fams)
      loadFontFamily(cell->value);
  }

  succeed;
}


static status
loadFontAliases(Name res)
{ Chain ch = getClassVariableValueClass(ClassFont, res);

  if ( ch )
  { Cell cell;
    Type type_font = nameToType(NAME_font);

    for_cell(cell, ch)
    { Name name;
      FontObj font;
      Any n, f;

      if ( instanceOfObject(cell->value, ClassBinding) )
      { Binding b = cell->value;
	n = b->name;
	f = b->value;
      } else if ( instanceOfObject(cell->value, ClassTuple) )
      { Tuple t = cell->value;
	n = t->first;
	f = t->second;
      } else if ( instanceOfObject(cell->value, ClassAttribute) )
      { Attribute a = cell->value;
	n = a->name;
	f = a->value;
      } else
      { errorPce(cell->value, NAME_unexpectedType,
		 CtoType(":=|tuple|attribute"));
	continue;
      }

      if ( !(name = checkType(n, TypeName, ClassFont)) ||
	   !(font = checkType(f, type_font, ClassFont)) )
	errorPce(ClassFont, NAME_badFontAlias, n, f);
      else
	fontAlias(name, font, OFF);
    }

    succeed;
  }

  fail;
}


static status
fontAlias(Name name, FontObj font, BoolObj force)
{ if ( force == ON || !getMemberHashTable(FontAliasTable, name) )
    appendHashTable(FontAliasTable, name, font);

  succeed;
}


/* Implements class(font)->list_fonts */
static status
listFonts(Class class, BoolObj mono)
{ return ws_list_fonts(mono);
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
     NAME_postscript, "PostScript point-size of the font"),
  IV(NAME_wsRef, "alien:WsRef", IV_NONE,
     NAME_storage, "Window system handle")
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
{ RC(NAME_scale, "real",  "1.0",
     "Multiplication factor for all fonts"),
  RC(NAME_systemFonts, "chain",
     "[ normal    := font(helvetica, roman, 12),\n"
     "  bold      := font(helvetica, bold, 12),\n"
     "  italic    := font(helvetica, oblique, 12),\n"
     "  small     := font(helvetica, roman, 10),\n"
     "  large     := font(helvetica, roman, 14),\n"
     "  boldlarge := font(helvetica, bold, 14),\n"
     "  huge      := font(helvetica, roman, 18),\n"
     "  boldhuge  := font(helvetica, bold, 18),\n"
     "  fixed     := font(courier, roman, 12),\n"
     "  tt        := font(courier, roman, 12),\n"
     "  boldtt    := font(courier, bold, 12)\n"
     "]",
     "Predefined font-aliases"),
  RC(NAME_noFont, "font", "normal",
     "Replacement for undefined fonts")
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

  FontTable      = globalObject(NAME_fonts, ClassHashTable, toInt(32), EAV);
  FontAliasTable = globalObject(NAME_fontAliases, ClassHashTable, toInt(16), EAV);

  attach_font_families(class);
  /* Create a class method */
  sendMethodObject(
    class,
    createSendMethod(NAME_listFonts,
		     newObject(ClassVector, CtoName("[bool]"), EAV),
		     CtoString("List Pango font families"),
		     listFonts));

  succeed;
}

		 /*******************************
		 *        DEFAULT FONTS         *
		 *******************************/

#define PFONT(n, p, x) { n, p, XNAME(x) }
#define ENDFONTLIST    { NULL, 0, NULL }

typedef struct
{ Name style;
  int  points;
  char *xname;
} fontdef, *FontDef;

#define XNAME(x) x

/* @screen_<Style>_<Points> */
static fontdef screen_fonts[] =
{ PFONT(NAME_roman, 10, ""),
  PFONT(NAME_roman, 12, ""),
  PFONT(NAME_roman, 14, ""),
  PFONT(NAME_roman, 16, ""),
  PFONT(NAME_bold,  10, ""),
  PFONT(NAME_bold,  12, ""),
  PFONT(NAME_bold,  14, ""),
  PFONT(NAME_bold,  16, ""),
  ENDFONTLIST
};

/* @courier_<Style>_<Points> */
static fontdef courier_fonts[] =
{ PFONT(NAME_roman,   10, ""),
  PFONT(NAME_roman,   12, ""),
  PFONT(NAME_roman,   14, ""),
  PFONT(NAME_roman,   18, ""),
  PFONT(NAME_roman,   24, ""),
  PFONT(NAME_bold,    10, ""),
  PFONT(NAME_bold,    12, ""),
  PFONT(NAME_bold,    14, ""),
  PFONT(NAME_bold,    18, ""),
  PFONT(NAME_bold,    24, ""),
  PFONT(NAME_oblique, 10, ""),
  PFONT(NAME_oblique, 12, ""),
  PFONT(NAME_oblique, 14, ""),
  PFONT(NAME_oblique, 18, ""),
  PFONT(NAME_oblique, 24, ""),
  ENDFONTLIST
};


static fontdef helvetica_fonts[] =
{ PFONT(NAME_bold,    10, ""),
  PFONT(NAME_bold,    12, ""),
  PFONT(NAME_bold,    14, ""),
  PFONT(NAME_bold,    18, ""),
  PFONT(NAME_bold,    24, ""),
  PFONT(NAME_roman,   10, ""),
  PFONT(NAME_roman,   12, ""),
  PFONT(NAME_roman,   14, ""),
  PFONT(NAME_roman,   18, ""),
  PFONT(NAME_roman,   24, ""),
  PFONT(NAME_oblique, 10, ""),
  PFONT(NAME_oblique, 12, ""),
  PFONT(NAME_oblique, 14, ""),
  PFONT(NAME_oblique, 18, ""),
  PFONT(NAME_oblique, 24, ""),
  ENDFONTLIST
};


static fontdef times_fonts[] =
{ PFONT(NAME_roman,  10, ""),
  PFONT(NAME_roman,  12, ""),
  PFONT(NAME_roman,  14, ""),
  PFONT(NAME_roman,  18, ""),
  PFONT(NAME_roman,  24, ""),
  PFONT(NAME_bold,   10, ""),
  PFONT(NAME_bold,   12, ""),
  PFONT(NAME_bold,   14, ""),
  PFONT(NAME_bold,   18, ""),
  PFONT(NAME_bold,   24, ""),
  PFONT(NAME_italic, 10, ""),
  PFONT(NAME_italic, 12, ""),
  PFONT(NAME_italic, 14, ""),
  PFONT(NAME_italic, 18, ""),
  PFONT(NAME_italic, 24, ""),
  ENDFONTLIST
};


static char *
default_font_list(Name fam, FontDef defs)
{ char buf[10240];
  char *s = buf;

#define LEFT() (sizeof(buf)-(s-buf)-1)

  *s++ = '[';

  while(defs->style)
  {
    if ( defs->xname && defs->xname[0] )
    { snprintf(s, LEFT(),
	       "font(%s, %s, %d, \"%s\")",
	      strName(fam),
	      strName(defs->style),
	      defs->points,
	      defs->xname);
    } else
    { snprintf(s, LEFT(),
	      "font(%s, %s, %d)",
	      strName(fam),
	      strName(defs->style),
	      defs->points);
    }
    s += strlen(s);
    defs++;
    if ( defs->style && LEFT() >= 2 )
      strcpy(s, ",\n");
    s += strlen(s);
  }

  if ( LEFT() > 1 )
    *s++ = ']';
  *s = EOS;

  assert(LEFT() > 0);

  return save_string(buf);
}


static void
attach_fonts(Class class, char *res, Name fam, FontDef defs)
{ attach_class_variable(class, CtoName(res), "chain",
			default_font_list(fam, defs),
			"Font family set");
}


static void
attach_font_families(Class class)
{ attach_class_variable(class, NAME_fontFamilies,  "chain",
			"[screen_fonts,courier_fonts,"
			"helvetica_fonts,times_fonts]",
			"Predefined font families");

  attach_fonts(class, "courier_fonts",   NAME_courier,   courier_fonts);
  attach_fonts(class, "helvetica_fonts", NAME_helvetica, helvetica_fonts);
  attach_fonts(class, "times_fonts",     NAME_times,     times_fonts);
  attach_fonts(class, "screen_fonts",    NAME_screen,    screen_fonts);
}
