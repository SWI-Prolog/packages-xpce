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

static Int	getPointsFont(FontObj f);
static status	loadFontFamilies(void);
static status	loadFontAliases(Name res);
static status	fontAlias(Name name, FontObj font, BoolObj force);

static Name
fontName(Name family, Name style, Int points, Name weight)
{ string s;
  Any av[4];
  Name rc;

  if ( (weight == NAME_normal || weight == toInt(400)) ||
       (style == NAME_bold && (weight == NAME_bold || weight == toInt(700))) )
  { av[0] = family;
    av[1] = style;
    av[2] = points;

    str_writefv(&s, (CharArray)CtoTempString("%s_%s_%.2f"), 3, av);
  } else
  { av[0] = family;
    av[1] = weight;
    av[2] = style;
    av[3] = points;

    str_writefv(&s, (CharArray)CtoTempString("%s_%s_%s_%.2f"), 4, av);
  }

  str_downcase(&s, 0, s.s_size);
  str_translate(&s, ' ', '_');
  if ( str_fetch(&s, s.s_size-1) == '0' &&
       str_fetch(&s, s.s_size-2) == '0' )
    s.s_size -= 3;
  else
    str_store(&s, s.s_size-3, '_');


  rc = StringToName(&s);
  str_unalloc(&s);

  return rc;
}


static status
initialiseFont(FontObj f, Name family, Name style, Int points, Name weight)
{ if ( isDefault(weight) )
    weight = style == NAME_bold ? NAME_bold : NAME_normal;

  assign(f, family,      family);
  assign(f, style,       style);
  assign(f, weight,	 weight);
  assign(f, points,      points);
  assign(f, ascent,      DEFAULT);
  assign(f, descent,     DEFAULT);
  assign(f, fixed_width, DEFAULT);

  protectObject(f);		/* Still needed? */

  Name name = fontName(family, style, points, weight);
  newAssoc(name, f);

  return appendHashTable(FontTable, name, f);
}


static FontObj
getLookupFont(Class class, Name family, Name style, Int points, Name weight)
{ if ( isDefault(weight) )
    weight = style == NAME_bold ? NAME_bold : NAME_normal;
  Name name = fontName(family, style, points, weight);
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
    }
  }

  fail;
}

FontObj
getCopyFont(FontObj f)
{ FontObj copy = allocObject(classOfObject(f), FALSE);

  ws_create_font(f);
  assign(copy, family,      f->family);
  assign(copy, style,       f->style);
  assign(copy, weight,	    f->weight);
  assign(copy, points,      f->points);
  assign(copy, ascent,      f->ascent);
  assign(copy, descent,     f->descent);
  assign(copy, fixed_width, f->fixed_width);
  copy->ws_ref = ws_clone_ws_font(f->ws_ref);

  answer(copy);
}

static FontObj
getRescaleFont(FontObj f, Num scale)
{ double pts = valNum(f->points)*valNum(scale);

  return answerObject(getClassObject(f),
		      f->family, f->style, toNum(pts), f->weight, EAV);
}

status
replaceFont(FontObj f)
{ FontObj nofont;
  void *wsref;

  if ( !(nofont = getClassVariableValueClass(ClassFont, NAME_noFont)) )
    errorPce(f, NAME_noDefaultFont);

  if ( !(wsref = ws_get_font(nofont)) )
    fail;

  errorPce(f, NAME_replacedFont, nofont);
  f->ws_ref = ws_clone_ws_font(wsref);

  assign(f, fixed_width, nofont->fixed_width);
  assign(f, ex,          nofont->ex);

  succeed;
}


static status
unlinkFont(FontObj f)
{ ws_destroy_font(f);

  succeed;
}


status
makeBuiltinFonts(void)
{ static bool done = false;

  if ( done )
    succeed;
  done = true;

  if ( loadFontFamilies() &&	/* Family -> Pango Family */
       loadFontAliases(NAME_systemFonts) )
  { loadFontAliases(NAME_userFonts);
    succeed;
  }

  fail;
}

		/********************************
		*           GET INFO		*
		********************************/


Num
getWidthFont(FontObj f, CharArray txt)
{ if ( isDefault(txt) )
    txt = (CharArray) NAME_x;

  d_ensure_display();			/* TBD */

  answer(toNum(str_width(&txt->data, 0, txt->data.s_size, f)));
}


Num
getAdvanceFont(FontObj f, CharArray txt)
{ return toNum(str_advance(&txt->data, 0, txt->data.s_size, f));
}


Num
getExFont(FontObj f)
{ ws_create_font(f);

  answer(f->ex);
}

Num
getAvgCharWidthFont(FontObj f)
{ if ( !isInteger(f->avg_char_width) )
  { const char *sample =
      "0123456789"
      "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
      "abcdefghijklmnopqrstuvwxyz";
    size_t len = strlen(sample);
    double avg = str_advance_utf8(sample, len, f)/(double)len;
    assign(f, avg_char_width, toNum(avg));
  }

  answer(f->avg_char_width);
}

Num
getHeightFont(FontObj f)
{ ws_create_font(f);
  answer(toNum(valNum(f->ascent)+valNum(f->descent)));
}


Num
getAscentFont(FontObj f)
{ ws_create_font(f);
  answer(f->ascent);
}


Num
getDescentFont(FontObj f)
{ ws_create_font(f);
  answer(f->descent);
}


static Size
getSizeFont(FontObj f)
{ answer(answerObject(ClassSize, getExFont(f), getHeightFont(f), EAV));
}


BoolObj
getFixedWidthFont(FontObj f)
{ if ( isDefault(f->fixed_width) )
  { ws_create_font(f);

    if ( isDefault(f->fixed_width) )
    { BoolObj fixed = ( getAdvanceFont(f, (CharArray)NAME_i) ==
			getAdvanceFont(f, (CharArray)NAME_w) ) ? ON : OFF;
      assign(f, fixed_width, fixed);
    }
  }

  answer(f->fixed_width);
}


static status
memberFont(FontObj f, Int chr)
{ if ( s_has_char(f, valInt(chr)) )
    succeed;

  fail;
}


static Int
getDefaultCharacterFont(FontObj f)
{ answer(toInt(s_default_char(f)));
}


static Tuple
getDomainFont(FontObj f, Name which)
{ int a, z;

  if ( isDefault(which) )
    which = NAME_x;

  f_domain(f, which, &a, &z);
  return answerObject(ClassTuple, toInt(a), toInt(z), EAV);
}


static Num
getPointsFont(FontObj f)
{ if ( notDefault(f->points) )
    answer(f->points);

  answer(getHeightFont(f));
}

static Any
getPangoPropertyFont(FontObj f, Name property)
{ return ws_get_pango_property(f, property);
}


		/********************************
		*          FONT TABLES		*
		********************************/

static status
getPair(Any obj, Any *key, Any *value)
{ if ( instanceOfObject(obj, ClassBinding) )
  { Binding b = obj;
    *key   = b->name;
    *value = b->value;
  } else if ( instanceOfObject(obj, ClassTuple) )
  { Tuple t = obj;
    *key   = t->first;
    *value = t->second;
  } else if ( instanceOfObject(obj, ClassAttribute) )
  { Attribute a = obj;
    *key = a->name;
    *value = a->value;
  } else
  { errorPce(obj, NAME_unexpectedType,
	     CtoType(":=|tuple|attribute"));
    fail;
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

      if ( !getPair(cell->value, &n, &f) )
	continue;

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


static status
loadFontFamilies(void)
{ Chain ch = getClassVariableValueClass(ClassFont, NAME_pangoFamilies);

  if ( ch )
  { Cell cell;

    for_cell(cell, ch)
    { Name name;
      Name pname;
      Any n, f;

      if ( !getPair(cell->value, &n, &f) )
	continue;

      if ( !(name  = checkType(n, TypeName, ClassFont)) ||
	   !(pname = checkType(f, TypeName, ClassFont)) )
      { errorPce(ClassFont, NAME_badPangoFamily, n, f);
      } else
      { if ( !getMemberHashTable(FontFamilyTable, name) )
	  appendHashTable(FontFamilyTable, name, pname);
      }
    }

    succeed;
  }

  fail;
}


/* Implements class(font)<-font_families */
static Sheet
getFontFamilies(Class class, BoolObj mono)
{ answer(ws_font_families(mono));
}

		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

#define WEIGHT_NAMES "{thin,ultralight,light,semilight,book,normal,medium,bold,ultrabold,heavy,ultraheavy}"
#define WEIGHT_TYPE WEIGHT_NAMES "|100..1000"
static char *T_initialise[] =
        { "family=name", "style=name", "points=[int]",
	  "weight=["WEIGHT_TYPE"]" };

/* Instance Variables */

static vardecl var_font[] =
{ IV(NAME_family, "name", IV_GET,
     NAME_name, "Family the font belongs to (times, etc.)"),
  IV(NAME_style, "{normal,italic,oblique,bold}", IV_GET,
     NAME_name, "Style of the font"),
  IV(NAME_weight, WEIGHT_TYPE, IV_GET,
     NAME_name, "Weight of the font"),
  IV(NAME_points, "[num]", IV_NONE,
     NAME_name, "Point-size of the font"),
  IV(NAME_ascent, "[num]", IV_NONE,
     NAME_dimension, "Height above baseline"),
  IV(NAME_descent, "[num]", IV_NONE,
     NAME_dimension, "Depth below baseline"),
  IV(NAME_ex, "num*", IV_NONE,
     NAME_dimension, "Height of the letter `x' in this font"),
  IV(NAME_avgCharWidth, "num*", IV_NONE,
     NAME_dimension, "Average char width"),
  IV(NAME_fixedWidth, "[bool]", IV_NONE,
     NAME_dimension, "If @off, font is proportional"),
  IV(NAME_wsRef, "alien:WsRef", IV_NONE,
     NAME_storage, "Window system handle")
};

/* Send Methods */

static senddecl send_font[] =
{ SM(NAME_initialise, 4, T_initialise, initialiseFont,
     DEFAULT, "Create from fam, style, points, weigth"),
  SM(NAME_unlink, 0, NULL, unlinkFont,
     DEFAULT, "Destroy the font"),
  SM(NAME_member, 1, "char", memberFont,
     NAME_set, "Test if font defines character")
};

/* Get Methods */

static getdecl get_font[] =
{ GM(NAME_points, 0, "num", NULL, getPointsFont,
     DEFAULT, "Specified point-size or <-height"),
  GM(NAME_convert, 1, "font", "name", getConvertFont,
     NAME_conversion, "Convert logical font-name and @family_style_points"),
  GM(NAME_ascent, 0, "num", NULL, getAscentFont,
     NAME_dimension, "Highest point above baseline"),
  GM(NAME_descent, 0, "num", NULL, getDescentFont,
     NAME_dimension, "Lowest point below baseline"),
  GM(NAME_ex, 0, "num", NULL, getExFont,
     NAME_dimension, "Height of the letter `x'"),
  GM(NAME_avgCharWidth, 0, "num", NULL, getAvgCharWidthFont,
     NAME_dimension, "Average char width"),
  GM(NAME_height, 0, "num", NULL, getHeightFont,
     NAME_dimension, "Height of highest character in font"),
  GM(NAME_size, 0, "size", NULL, getSizeFont,
     NAME_dimension, "New size from <-width and <-height"),
  GM(NAME_width, 1, "num", "[char_array]", getWidthFont,
     NAME_dimension, "Width of string (default \"x\")"),
  GM(NAME_rescale, 1, "font", "num", getRescaleFont,
     NAME_dimension, "Get scaled version of font"),
  GM(NAME_advance, 1, "num", "char_array", getAdvanceFont,
     NAME_dimension, "X-origin advancement of string"),
  GM(NAME_lookup, 4, "font", T_initialise, getLookupFont,
     NAME_oms, "Lookup in @fonts table"),
  GM(NAME_defaultCharacter, 0, "char", NULL, getDefaultCharacterFont,
     NAME_property, "Character painted for non-existing entries"),
  GM(NAME_domain, 1, "tuple", "[{x,y}]", getDomainFont,
     NAME_property, "Range of valid characters"),
  GM(NAME_fixedWidth, 0, "bool", NULL, getFixedWidthFont,
     NAME_property, "Boolean to indicate font is fixed-width"),
  GM(NAME_pangoProperty, 1, "{description,family,style,weight,size}",
     "int|name", getPangoPropertyFont,
     NAME_property, "Property of the Pango font"),
};

/* Resources */

static classvardecl rc_font[] =
{ RC(NAME_scale, "real",  "1.0",
     "Multiplication factor for all fonts"),
  RC(NAME_systemFonts, "chain",
     "[ normal    := font(sans, normal, 12),\n"
     "  bold      := font(sans, bold,   12),\n"
     "  italic    := font(sans, italic, 12),\n"
     "  small     := font(sans, normal, 10),\n"
     "  large     := font(sans, normal, 14),\n"
     "  boldlarge := font(sans, bold,   14),\n"
     "  huge      := font(sans, normal, 18),\n"
     "  boldhuge  := font(sans, bold,   18),\n"
     "  fixed     := font(mono, normal, 12),\n"
     "  tt        := font(mono, normal, 12),\n"
     "  boldtt    := font(mono, bold,   12)\n"
     "]",
     "Predefined font-aliases"),
  RC(NAME_pangoFamilies, "chain",
     "[ mono      := '"MONO_FAMILY"',\n"
     "  sans      := '"SANS_FAMILY"',\n"
     "  serif     := '"SERIF_FAMILY"',\n"
     "  screen    := '"MONO_FAMILY"',\n" /* Backward compatibility */
     "  helvetica := '"SANS_FAMILY"',\n"
     "  times     := '"SERIF_FAMILY"'\n"
     "]",
     "Mapping from generic family to Pango family"),
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

  FontTable       = globalObject(NAME_fonts, ClassHashTable, EAV);
  FontAliasTable  = globalObject(NAME_fontAliases, ClassHashTable, EAV);
  FontFamilyTable = globalObject(NAME_fontFamilies, ClassHashTable, EAV);

  /* Create a class method */
  getMethodObject(
    class,
    createGetMethod(NAME_fontFamilies,
		    toType(NAME_sheet),
		    newObject(ClassVector, CtoName("[monospace=bool]"), EAV),
		    CtoString("Get Pango font families"),
		    (void*)getFontFamilies));

  succeed;
}
