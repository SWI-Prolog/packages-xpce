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
#define _USE_MATH_DEFINES
#include <math.h>

static Int getAlphaColour(Colour c);

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
defcolourname(Int r, Int g, Int b, Int a)
{ if ( notDefault(r) && notDefault(g) && notDefault(b) )
  { char buf[50];

    if ( isDefault(a) || a == toInt(255) )
      sprintf(buf, "#%02x%02x%02x",
	      (unsigned int)valInt(r),
	      (unsigned int)valInt(g),
	      (unsigned int)valInt(b));
    else
      sprintf(buf, "#%02x%02x%02x%02x",
	      (unsigned int)valInt(r),
	      (unsigned int)valInt(g),
	      (unsigned int)valInt(b),
	      (unsigned int)valInt(a));

    return CtoName(buf);
  }

  fail;
}


static status
initialiseColour(Colour c, Name name, Int r, Int g, Int b, Int a, Name model)
{ if ( notDefault(name) )
    assign(c, name, name);

  if ( isDefault(a) )
    a = toInt(255);

  if ( isDefault(r) && isDefault(g) && isDefault(b) )
  { assign(c, kind, NAME_named);
    if ( a == toInt(255) )
    { assign(c, rgba, DEFAULT);
    } else
    { ws_named_colour(c);
      assign(c, rgba, toInt(valInt(c->rgba)|(valInt(a)<<24)));
    }
  } else if ( notDefault(r) && notDefault(g) && notDefault(b) )
  { assign(c, kind, NAME_rgb);

    if ( !toRBG(&r, &g, &b, model) )
      fail;

    if ( isDefault(name) )
    { name = defcolourname(r, g, b, a);
      assign(c, name, name);
    }
    COLORRGBA rgba = RGBA(valInt(r), valInt(g), valInt(b), valInt(a));
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
getLookupColour(Class class, Name name, Int r, Int g, Int b, Int a, Name model)
{ if ( isDefault(a) )
    a = toInt(255);

  if ( isDefault(name) && notDefault(r) && notDefault(g) && notDefault(b) )
  { if ( !toRBG(&r, &g, &b, model) )
      fail;

    name = defcolourname(r, g, b, a);
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
			 getBlueColour(c),
			 getAlphaColour(c)));
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

  if ( c->kind == NAME_named && !isInteger(c->rgba) )
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
  { int r, g, b, a = 255;
    int dgs = 0;
    size_t l = strlen(s);
    bool has_alpha = false;

    switch(l-1)
    { case 4:			/* #RGBA */
	has_alpha = true;
      case 3:			/* #RGB */
	dgs = 1;
	break;
      case 8:			/* #RRGGBBAA */
	has_alpha = true;
      case 6:			/* #RRGGBB */
	dgs = 2;
	break;
      default:
	fail;
    }

    s++;				/* skip # */
    r = take_hex(s, dgs); s+= dgs;
    g = take_hex(s, dgs); s+= dgs;
    b = take_hex(s, dgs); s+= dgs;
    if ( has_alpha )
      a = take_hex(s, dgs);

    if ( r >= 0 && g >= 0 && b >= 0 && a >= 0 )
    { if ( dgs == 1 )
      { r = r*16 + r;
	g = g*16 + g;
	b = b*16 + b;
	if ( has_alpha )
	  a = b*16 + a;
      }

      answer(answerObject(ClassColour, name,
			  toInt(r), toInt(g), toInt(b), toInt(a), EAV));
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

static Int
getAlphaColour(Colour c)
{ if ( isDefault(c->rgba) )
    ws_named_colour(c);

  return toInt(ColorAValue(valInt(c->rgba)));
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
associateColour(Colour c, Int r, Int g, Int b, Int a)
{ Name name;
  Colour c2;
  Chain ch;

  name = defcolourname(r, g, b, a);
  if ( !(c2=getMemberHashTable(ColourTable, name)) )
    c2 = newObject(ClassColour, name, r, g, b, a, EAV);

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

  return associateColour(c, toInt(r), toInt(g), toInt(b), getAlphaColour(c));
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

  return associateColour(c, toInt(r), toInt(g), toInt(b), getAlphaColour(c));
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
		 *       COLOUR DISTANCE        *
		 *******************************/

// Forward declarations
static void rgb_to_lab(double r, double g, double b,
                       double *L, double *a, double *b_);
static double cie_de2000(double L1, double a1, double b1,
                         double L2, double a2, double b2);

// Convert 8-bit RGB to CIE Lab
static void rgb_to_lab(double r, double g, double b,
                       double *L, double *a, double *bp)
{ // Normalize and linearize sRGB
  double rs = r / 255.0, gs = g / 255.0, bs = b / 255.0;
  double R = (rs <= 0.04045) ? rs / 12.92 : pow((rs + 0.055)/1.055, 2.4);
  double G = (gs <= 0.04045) ? gs / 12.92 : pow((gs + 0.055)/1.055, 2.4);
  double B = (bs <= 0.04045) ? bs / 12.92 : pow((bs + 0.055)/1.055, 2.4);

  // Convert to XYZ (D65)
  double X = R*0.4124564 + G*0.3575761 + B*0.1804375;
  double Y = R*0.2126729 + G*0.7151522 + B*0.0721750;
  double Z = R*0.0193339 + G*0.1191920 + B*0.9503041;

  // Normalize to reference white
  X /= 0.95047; Y /= 1.00000; Z /= 1.08883;

  // XYZ to Lab helper
  double fx = (X > 0.008856) ? cbrt(X) : (7.787 * X + 16.0/116.0);
  double fy = (Y > 0.008856) ? cbrt(Y) : (7.787 * Y + 16.0/116.0);
  double fz = (Z > 0.008856) ? cbrt(Z) : (7.787 * Z + 16.0/116.0);

  *L  =  (116.0 * fy) - 16.0;
  *a  =  500.0 * (fx - fy);
  *bp =  200.0 * (fy - fz);
}

/* Copied from https://stackoverflow.com/questions/6630599/are-there-known-implementations-of-the-ciede2000-or-cie94-delta-e-color-differen

   Code is in the public domain.

However, the results are dubious.   The same post provides test cases.  Translated to C:

typedef struct
{ double l1, a1, b1, l2, a2, b2, r;
} test;
#define T(L1, A1, B1, L2, A2, B2, R) {L1, A1, B1, L2, A2, B2, R}

test tests[] =
{
T(53.0, 0.65, 0.15, 33.0, -0.45, -0.1, 20.03112),
T(42.0, -0.3, 0.1, 74.0, -0.2, -0.15, 32.001118),
T(12.0, -1.0, -0.45, 32.0, 0.3, 0.9, 20.084782),
T(94.0, -0.1, -0.55, 77.0, 0.5, 0.45, 17.03928),
T(75.0, -0.8, 0.35, 46.0, -0.6, -0.85, 29.02483),
T(83.0, -0.65, -0.7, 67.0, 0.75, 0.0, 16.074173),
T(70.0, -0.7, 0.9, 54.0, 0.35, -0.95, 16.13608),
T(81.0, 0.45, -0.8, 53.0, -0.35, 0.05, 28.023375),
T(40.0, -0.2, -0.65, 25.0, -1.0, 0.8, 15.088856),
T(66.0, 0.85, -0.7, 93.0, 0.55, 0.15, 27.014244),
T(44.0, -0.5, 0.5, 23.0, -0.9, 0.5, 21.00363),
T(67.0, 0.4, 0.25, 42.0, -0.25, 0.6, 25.010727),
T(32.0, 0.6, 0.55, 86.0, 0.0, 0.25, 54.003925),
T(96.0, -0.15, -0.9, 87.0, 0.25, -0.3, 9.027307),
T(100.0, -0.6, 0.3, 61.0, -0.25, -0.75, 39.015385),
T(2.0, -0.2, -0.65, 73.0, -0.3, 0.65, 71.01173),
T(74.0, 0.1, -0.65, 96.0, -0.5, 0.8, 22.05474),
T(22.0, -0.3, -0.85, 64.0, -0.65, -0.95, 42.0015),
T(73.0, -0.35, 0.3, 38.0, 0.25, -1.0, 35.02875),
T(91.0, 0.6, 0.45, 82.0, -0.25, 0.2, 9.042115),
T(0, 0, 0, 0, 0, 0, 0)
};


int main(void)
{ for(test *t = tests; t->l1 > 0; t++)
  { const double delta_e = cie_de2000(t->l1, t->a1, t->b1, t->l2, t->a2, t->b2);

    printf("l_1 = %g; a_1 = %g; b_1 = %g; l_2 = %g; a_2 = %g; b_2 = %g -> delta_e = %g (exp %g)\n",
            t->l1, t->a1, t->b1, t->l2, t->a2, t->b2, delta_e, t->r);
  }
  return 0;
}
*/

static double
cie_de2000(const double l_1, const double a_1, const double b_1,
	   const double l_2, const double a_2, const double b_2)
{ // Working in C with the CIEDE2000 color-difference formula.
  // k_l, k_c, k_h are parametric factors to be adjusted according to
  // different viewing parameters such as textures, backgrounds...
  const double k_l = 1.0;
  const double k_c = 1.0;
  const double k_h = 1.0;
  double n = (hypot(a_1, b_1) + hypot(a_2, b_2)) * 0.5;
  n = n * n * n * n * n * n * n;
  // A factor involving chroma raised to the power of 7 designed to make
  // the influence of chroma on the total color difference more accurate.
  n = 1.0 + 0.5 * (1.0 - sqrt(n / (n + 6103515625.0)));
  // hypot calculates the Euclidean distance while avoiding overflow/underflow.
  const double c_1 = hypot(a_1 * n, b_1);
  const double c_2 = hypot(a_2 * n, b_2);
  // atan2 is preferred over atan because it accurately computes the angle of
  // a point (x, y) in all quadrants, handling the signs of both coordinates.
  double h_1 = atan2(b_1, a_1 * n);
  double h_2 = atan2(b_2, a_2 * n);
  if (h_1 < 0.0)
    h_1 += 2.0 * M_PI;
  if (h_2 < 0.0)
    h_2 += 2.0 * M_PI;
  n = fabs(h_2 - h_1);
  // Cross-implementation consistent rounding.
  if (M_PI - 1E-14 < n && n < M_PI + 1E-14)
    n = M_PI;
  // When the hue angles lie in different quadrants, the straightforward
  // average can produce a mean that incorrectly suggests a hue angle in
  // the wrong quadrant, the next lines handle this issue.
  double h_m = (h_1 + h_2) * 0.5;
  double h_d = (h_2 - h_1) * 0.5;
  if (M_PI < n)
  { if (0.0 < h_d)
      h_d -= M_PI;
    else
      h_d += M_PI;
    h_m += M_PI;
  }
  const double p = 36.0 * h_m - 55.0 * M_PI;
  n = (c_1 + c_2) * 0.5;
  n = n * n * n * n * n * n * n;
  // The hue rotation correction term is designed to account for the
  // non-linear behavior of hue differences in the blue region.
  const double r_t = -2.0 * sqrt(n / (n + 6103515625.0))
                          * sin(M_PI / 3.0 * exp(p * p / (-25.0 * M_PI * M_PI)));
  n = (l_1 + l_2) * 0.5;
  n = (n - 50.0) * (n - 50.0);
  // Lightness.
  const double l = (l_2 - l_1) / (k_l * (1.0 + 0.015 * n / sqrt(20.0 + n)));
  // These coefficients adjust the impact of different harmonic
  // components on the hue difference calculation.
  const double t = 1.0 + 0.24 * sin(2.0 * h_m + M_PI / 2.0)
                     + 0.32 * sin(3.0 * h_m + 8.0 * M_PI / 15.0)
                     - 0.17 * sin(h_m + M_PI / 3.0)
                     - 0.20 * sin(4.0 * h_m + 3.0 * M_PI / 20.0);
  n = c_1 + c_2;
  // Hue.
  const double h = 2.0 * sqrt(c_1 * c_2) * sin(h_d) / (k_h * (1.0 + 0.0075 * n * t));
  // Chroma.
  const double c = (c_2 - c_1) / (k_c * (1.0 + 0.0225 * n));
  // Returning the square root ensures that the result represents
  // the "true" geometric distance in the color space.
  return sqrt(l * l + h * h + c * c + c * h * r_t);
}

// Public API: Compute DeltaE between two RGB colors
static double
deltaE_rgb(uint8_t r1, uint8_t g1, uint8_t b1,
	   uint8_t r2, uint8_t g2, uint8_t b2)
{ double L1,a1,b_1, L2,a2,b_2;
  rgb_to_lab(r1,g1,b1, &L1,&a1,&b_1);
  rgb_to_lab(r2,g2,b2, &L2,&a2,&b_2);
  return cie_de2000(L1,a1,b_1, L2,a2,b_2);
}

static Int
getDistanceColour(Colour me, Any to)
{ ws_named_colour(me);
  int me_rgb = valInt(me->rgba) & 0xffffff;
  int to_rgb;

  if ( instanceOfObject(to, ClassColour) )
  { Colour toc = to;
    ws_named_colour(toc);
    to_rgb = valInt(toc->rgba) & 0xffffff;
  } else
    to_rgb = valInt(to) & 0xffffff;

  uint8_t r1 = ColorRValue(me_rgb);
  uint8_t g1 = ColorGValue(me_rgb);
  uint8_t b1 = ColorBValue(me_rgb);

  uint8_t r2 = ColorRValue(to_rgb);
  uint8_t g2 = ColorGValue(to_rgb);
  uint8_t b2 = ColorBValue(to_rgb);

  return toNum(deltaE_rgb(r1, g1, b1, r2, g2, b2));
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_lookup[] =
	{ "[name|int]",
	  "red=[0..360]", "green=[0..255]", "blue=[0..255]",
	  "alpha=[0..255]", "model=[{rgb,hsv}]" };
static char *T_initialise[] =
	{ "name=[name]",
	  "red=[0..360]", "green=[0..255]", "blue=[0..255]",
	  "alpha=[0..255]", "model=[{rgb,hsv}]" };

/* Instance Variables */

static vardecl var_colour[] =
{ IV(NAME_name, "name|int", IV_GET,
     NAME_name, "Name of the colour"),
  IV(NAME_kind, "{named,rgb}", IV_GET,
     NAME_kind, "From colour-name database or user-defined"),
  IV(NAME_rgba, "[int]", IV_GET,
     NAME_colour, "Encoded RGBA tuple")
};

/* Send Methods */

static senddecl send_colour[] =
{ SM(NAME_initialise, 6, T_initialise, initialiseColour,
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
  GM(NAME_intensity, 0, "0..255", NULL, getIntensityColour,
     NAME_grey, "Total light intensity of the colour"),
  GM(NAME_distance, 1, "int", "colour|int", getDistanceColour,
     NAME_grey, "Similarity using CIEDE2000 (ΔE₀₀)"),
  GM(NAME_lookup, 6, "colour", T_lookup, getLookupColour,
     NAME_oms, "Lookup in @colours table"),
  GM(NAME_red, 0, "0..255", NULL, getRedColour,
     NAME_colour, "RGB red component"),
  GM(NAME_green, 0, "0..255", NULL, getGreenColour,
     NAME_colour, "RGB red component"),
  GM(NAME_blue, 0, "0..255", NULL, getBlueColour,
     NAME_colour, "RGB red component"),
  GM(NAME_alpha, 0, "0..255", NULL, getAlphaColour,
     NAME_colour, "RGBA alpha component"),
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

  WHITE_COLOUR  = newObject(ClassColour, NAME_white,  EAV);
  GREY25_COLOUR = newObject(ClassColour, NAME_grey25, EAV);
  GREY50_COLOUR = newObject(ClassColour, NAME_grey50, EAV);
  BLACK_COLOUR  = newObject(ClassColour, NAME_black,  EAV);

  succeed;
}
