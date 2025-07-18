/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog-org/projects/xpce/
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

static status
toRBG(Int *r, Int *g, Int *b, Name model)
{ if ( isDefault(*r) || isDefault(*g) || isDefault(*b) )
    fail;

  if ( model == NAME_hsv )
  { int	ih = valInt(*r) % 360;
    int is = valInt(*g);
    int iv = valInt(*b);
    float R,G,B;

    if ( is > 100 )
      return errorPce(*g, NAME_unexpectedType, CtoType("0..100"));
    if ( iv > 100 )
      return errorPce(*g, NAME_unexpectedType, CtoType("0..100"));

    if ( ih < 0 )
      ih += 360;

    HSVToRGB((float)ih/360.0, (float)is/100.0, (float)iv/100.0,
	     &R, &G, &B);
    *r = toInt((int)(R*255+0.5));
    *g = toInt((int)(G*255+0.5));
    *b = toInt((int)(B*255+0.5));
  }

  succeed;
}


static Name
defcolourname(Int r, Int g, Int b)
{ if ( notDefault(r) && notDefault(g) && notDefault(b) )
  { char buf[50];

    sprintf(buf, "#%02x%02x%02x",
	    (unsigned int)valInt(r),
	    (unsigned int)valInt(g),
	    (unsigned int)valInt(b));

    return CtoName(buf);
  }

  fail;
}


static status
initialiseColour(Colour c, Name name, Int r, Int g, Int b, Name model)
{ if ( notDefault(name) )
    assign(c, name, name);

  if ( isDefault(r) && isDefault(g) && isDefault(b) )
  { assign(c, kind, NAME_named);
    assign(c, rgba, DEFAULT);
  } else if ( notDefault(r) && notDefault(g) && notDefault(b) )
  { assign(c, kind, NAME_rgb);

    if ( !toRBG(&r, &g, &b, model) )
      fail;

    if ( isDefault(name) )
    { name = defcolourname(r, g, b);
      assign(c, name, name);
    }
    COLORRGBA rgba = RGBA(valInt(r), valInt(g), valInt(b), 255);
    assign(c, rgba, toInt(rgba));
  } else
    return errorPce(c, NAME_instantiationFault,
		    getMethodFromFunction((Any(*)())initialiseColour));

  appendHashTable(ColourTable, c->name, c);

  succeed;
}


static status
unlinkColour(Colour c)
{ deleteHashTable(ColourTable, c->name);

  succeed;
}


static Colour
getLookupColour(Class class, Name name, Int r, Int g, Int b, Name model)
{ if ( isDefault(name) && notDefault(r) && notDefault(g) && notDefault(b) )
  { if ( !toRBG(&r, &g, &b, model) )
      fail;

    name = defcolourname(r, g, b);
  }

  if ( name )
    answer(getMemberHashTable(ColourTable, name));

  fail;
}


static Name
getStorageReferenceColour(Colour c)
{ if ( c->kind == NAME_named )
    answer(c->name);
  else
    answer(defcolourname(getRedColour(c),
			 getGreenColour(c),
			 getBlueColour(c)));
}


static status
equalColour(Colour c1, Colour c2)
{ if ( c1 == c2 )
    succeed;
  if ( instanceOfObject(c1, ClassColour) &&
       instanceOfObject(c2, ClassColour) )
  { if ( c1->name == c2->name )
      succeed;

    if ( c1->rgba == c2->rgba )
      succeed;
  }

  fail;
}


static status
storeColour(Colour c, FileObj file)
{ return storeSlotsObject(c, file);
}


static status
loadColour(Colour c, IOSTREAM *fd, ClassDef def)
{ TRY( loadSlotsObject(c, fd, def) );

  if ( c->kind == NAME_named )
    assign(c, rgba, DEFAULT);

  succeed;
}


static int
take_hex(char *s, int digits)
{ unsigned int v = 0;

  for(; digits-- > 0; s++)
  { if ( *s >= '0' && *s <= '9' )
      v = v * 16 + *s - '0';
    else if ( *s >= 'a' && *s <= 'f' )
      v = v * 16 + *s - 'a' + 10;
    else if ( *s >= 'A' && *s <= 'F' )
      v = v * 16 + *s - 'A' + 10;
    else
      return -1;			/* error */
  }

  return v;
}


static Colour
getConvertColour(Class class, Name name)
{ Colour c;
  char *s;

  if ( (c = getMemberHashTable(ColourTable, name)) )
    answer(c);

  if ( (s=strName(name))[0] == '#' )
  { int r, g, b;
    int dgs = 0;
    size_t l = strlen(s);

    if ( l == 4 )
      dgs = 1;
    else if ( l == 7 )
      dgs = 2;
    else if ( l == 13 )
      dgs = 4;

    if ( dgs )
    { s++;				/* skip # */
      r = take_hex(s, dgs); s+= dgs;
      g = take_hex(s, dgs); s+= dgs;
      b = take_hex(s, dgs);

      if ( r >= 0 && g >= 0 && b >= 0 )
      { if ( dgs == 1 )
	{ r = r*16 + r;
	  g = g*16 + g;
	  b = b*16 + b;
	} else if ( dgs == 4 )
	{ r /= 256;
	  g /= 256;
	  b /= 256;
	}

	answer(answerObject(ClassColour, name,
			    toInt(r), toInt(g), toInt(b), EAV));
      }
    }

    fail;
  }

  answer(answerObject(ClassColour, name, EAV));
}

Int
getRedColour(Colour c)
{ if ( isDefault(c->rgba) )
    ws_named_colour(c);

  return toInt(ColorRValue(valInt(c->rgba)));
}


Int
getGreenColour(Colour c)
{ if ( isDefault(c->rgba) )
    ws_named_colour(c);

  return toInt(ColorGValue(valInt(c->rgba)));
}


Int
getBlueColour(Colour c)
{ if ( isDefault(c->rgba) )
    ws_named_colour(c);

  return toInt(ColorBValue(valInt(c->rgba)));
}


static status
get_hsv_colour(Colour c, float *h, float *s, float *v)
{ if ( isDefault(c->rgba) )
    ws_named_colour(c);

  float r = (float)ColorRValue(valInt(c->rgba))/255.0;
  float g = (float)ColorGValue(valInt(c->rgba))/255.0;
  float b = (float)ColorBValue(valInt(c->rgba))/255.0;


  RGBToHSV(r, g, b,
	   h, s, v);

  succeed;
}


static Int
getHueColour(Colour c)
{ float h, s, v;

  TRY(get_hsv_colour(c, &h, &s, &v));

  return toNum(h*360.0);
}


static Int
getSaturationColour(Colour c)
{ float h, s, v;

  TRY(get_hsv_colour(c, &h, &s, &v));

  return toNum(s*100.0);
}


static Int
getValueColour(Colour c)
{ float h, s, v;

  TRY(get_hsv_colour(c, &h, &s, &v));

  return toNum(v*100.0);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
We store derived colours in a chain  associated with the main colour, so
they remain in existence as long as the main colour.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static Colour
associateColour(Colour c, Int r, Int g, Int b)
{ Name name;
  Colour c2;
  Chain ch;

  name = defcolourname(r, g, b);
  if ( !(c2=getMemberHashTable(ColourTable, name)) )
    c2 = newObject(ClassColour, name, r, g, b, EAV);

  if ( !(ch=getAttributeObject(c, NAME_associates)) )
    attributeObject(c, NAME_associates, newObject(ClassChain, c2, EAV));
  else
    addChain(ch, c2);

  answer(c2);
}



Colour
getHiliteColour(Colour c, Real h)
{ float hf;

  if ( isDefault(h) )
    h = getClassVariableValueObject(c, NAME_hiliteFactor);
  hf = h ? valReal(h) : 0.9;

  if ( isDefault(c->rgba) )
    ws_named_colour(c);

  COLORRGBA rgb = valInt(c->rgba);
  int r = ColorRValue(rgb);
  int g = ColorGValue(rgb);
  int b = ColorBValue(rgb);

  r = r + (int)((float)(255 - r) * hf);
  g = g + (int)((float)(255 - g) * hf);
  b = b + (int)((float)(255 - b) * hf);

  return associateColour(c, toInt(r), toInt(g), toInt(b));
}


Colour
getReduceColour(Colour c, Real re)
{ float rf;

  if ( isDefault(re) )
    re = getClassVariableValueObject(c, NAME_reduceFactor);
  rf = re ? valReal(re) : 0.6;

  if ( isDefault(c->rgba) )
    ws_named_colour(c);

  COLORRGBA rgb = valInt(c->rgba);
  int r = ColorRValue(rgb);
  int g = ColorGValue(rgb);
  int b = ColorBValue(rgb);

  r = (int)((float)r * rf);
  g = (int)((float)g * rf);
  b = (int)((float)b * rf);

  return associateColour(c, toInt(r), toInt(g), toInt(b));
}


static Int
getIntensityColour(Colour c)
{ if ( isDefault(c->rgba) )
    ws_named_colour(c);

  COLORRGBA rgb = valInt(c->rgba);
  int r = ColorRValue(rgb);
  int g = ColorGValue(rgb);
  int b = ColorBValue(rgb);

  answer(toInt((r*20 + g*32 + b*18)/(20+32+18)));
}

		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_lookup[] =
	{ "[name|int]",
	  "red=[0..360]", "green=[0..255]", "blue=[0..255]",
	  "model=[{rgb,hsv}]" };
static char *T_initialise[] =
	{ "name=[name]",
	  "red=[0..360]", "green=[0..255]", "blue=[0..255]",
	  "model=[{rgb,hsv}]" };

/* Instance Variables */

static vardecl var_colour[] =
{ IV(NAME_name, "name|int", IV_GET,
     NAME_name, "Name of the colour"),
  IV(NAME_kind, "{named,rgb}", IV_GET,
     NAME_kind, "From colour-name database or user-defined"),
  IV(NAME_rgba, "int", IV_GET,
     NAME_colour, "Encoded RGBA tuple")
};

/* Send Methods */

static senddecl send_colour[] =
{ SM(NAME_initialise, 5, T_initialise, initialiseColour,
     DEFAULT, "Create from name and optional rgb"),
  SM(NAME_unlink, 0, NULL, unlinkColour,
     DEFAULT, "Deallocate the colour object"),
  SM(NAME_equal, 1, "colour", equalColour,
     DEFAULT, "Test if colours have equal RGB")
};

/* Get Methods */

static getdecl get_colour[] =
{ GM(NAME_hilite, 1, "colour", "factor=[0.0..1.0]", getHiliteColour,
     NAME_3d, "Hilited version of the colour"),
  GM(NAME_reduce, 1, "colour", "factor=[0.0..1.0]", getReduceColour,
     NAME_3d, "Reduced version of the colour"),
  GM(NAME_convert, 1, "colour", "name", getConvertColour,
     NAME_conversion, "Convert X-colour name"),
  GM(NAME_storageReference, 0, "name", NULL, getStorageReferenceColour,
     NAME_file, "Description name for ->save_in_file"),
  GM(NAME_intensity, 0, "0..65535", NULL, getIntensityColour,
     NAME_grey, "Total light intensity of the colour"),
  GM(NAME_lookup, 5, "colour", T_lookup, getLookupColour,
     NAME_oms, "Lookup in @colours table"),
  GM(NAME_red, 0, "0..255", NULL, getRedColour,
     NAME_colour, "RGB red component"),
  GM(NAME_green, 0, "0..255", NULL, getGreenColour,
     NAME_colour, "RGB red component"),
  GM(NAME_blue, 0, "0..255", NULL, getBlueColour,
     NAME_colour, "RGB red component"),
  GM(NAME_hue, 0, "0..360", NULL, getHueColour,
     NAME_colour, "Hue from the HSV-model"),
  GM(NAME_saturnation, 0, "0..100", NULL, getSaturationColour,
     NAME_colour, "Saturnation from the HSV-model"),
  GM(NAME_value, 0, "0..100", NULL, getValueColour,
     NAME_colour, "Value from the HSV-model")
};

/* Resources */

static classvardecl rc_colour[] =
{ RC(NAME_hiliteFactor, "real", "0.9",
     "Default factor for <-hilite'd colour"),
  RC(NAME_reduceFactor, "real", "0.6",
     "Default factor for <-reduce'd colour")
};

/* Class Declaration */

static Name colour_termnames[] = { NAME_name };

ClassDecl(colour_decls,
	  var_colour, send_colour, get_colour, rc_colour,
	  1, colour_termnames,
	  "$Rev$");


status
makeClassColour(Class class)
{ declareClass(class, &colour_decls);

  setLoadStoreFunctionClass(class, loadColour, storeColour);
  cloneStyleClass(class, NAME_none);

  ColourTable = globalObject(NAME_colours, ClassHashTable, toInt(32), EAV);
  assign(ColourTable, refer, NAME_none);

  succeed;
}
