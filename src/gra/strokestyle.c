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
#include <math.h>

/* Class `stroke_style' describes how lines are drawn, beyond their
 * thickness (graphical <-pen) and colour.  It may be used wherever a
 * texture_name is accepted as graphical <-texture.  The Cairo backend
 * applies it in r_dash() (sdldraw.c).
 */

static status
initialiseStrokeStyle(StrokeStyle s, Name cap, Name join,
		      Any dash, Any paint)
{ assign(s, cap,	 isDefault(cap)  ? NAME_butt  : cap);
  assign(s, join,	 isDefault(join) ? NAME_miter : join);
  assign(s, miter_limit, toNum(10.0));
  assign(s, dash,	 isDefault(dash) ? NIL : dash);
  assign(s, dash_offset, ZERO);
  assign(s, paint,	 isDefault(paint) ? NIL : paint);

  if ( instanceOfObject(s->dash, ClassVector) )
    return dashStrokeStyle(s, s->dash);

  succeed;
}


/* A dash vector must hold non-negative numbers that are not all zero.
 */

status
dashStrokeStyle(StrokeStyle s, Any dash)
{ if ( instanceOfObject(dash, ClassVector) )
  { Vector v = dash;
    bool nonzero = false;

    for(int i=0; i<valInt(v->size); i++)
    { Any e = v->elements[i];

      if ( !isNum(e) || valNum(e) < 0.0 )
	return errorPce(e, NAME_unexpectedType, nameToType(NAME_num));
      if ( valNum(e) > 0.0 )
	nonzero = true;
    }
    if ( !nonzero )
      return errorPce(s, NAME_badDash, v);
  }

  assign(s, dash, dash);
  succeed;
}


/* Round and square caps extend a line by half the pen beyond its end
 * points.  Return how much this adds to the area of a graphical.
 */

int
capExtentGraphical(Graphical gr)
{ if ( instanceOfObject(gr->texture, ClassStrokeStyle) )
  { StrokeStyle s = gr->texture;

    if ( s->cap != NAME_butt )
      return (int)ceil(valNum(gr->pen)/2.0);
  }

  return 0;
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

static char *T_initialise[] =
	{ "cap=[{butt,round,square}]",
	  "join=[{miter,round,bevel}]",
	  "dash=[texture_name|vector*]",
	  "paint=[colour|gradient*]"
	};

static vardecl var_stroke_style[] =
{ IV(NAME_cap, "{butt,round,square}", IV_BOTH,
     NAME_appearance, "Shape of the ends of a line"),
  IV(NAME_join, "{miter,round,bevel}", IV_BOTH,
     NAME_appearance, "Shape of the corners of a line"),
  IV(NAME_miterLimit, "num", IV_BOTH,
     NAME_appearance, "Bevel miter joins longer than this times the pen"),
  SV(NAME_dash, "texture_name|vector*", IV_GET|IV_STORE, dashStrokeStyle,
     NAME_appearance, "Named dash pattern or lengths of dashes and gaps"),
  IV(NAME_dashOffset, "num", IV_BOTH,
     NAME_appearance, "Distance into the dash pattern at the start"),
  IV(NAME_paint, "colour|gradient*", IV_BOTH,
     NAME_appearance, "Paint for the line (@nil: graphical <-colour)")
};

static senddecl send_stroke_style[] =
{ SM(NAME_initialise, 4, T_initialise, initialiseStrokeStyle,
     DEFAULT, "Create from cap, join, dash pattern and paint")
};

#define get_stroke_style NULL
#define rc_stroke_style  NULL

static Name stroke_style_termnames[] =
	{ NAME_cap, NAME_join, NAME_dash, NAME_paint };

ClassDecl(stroke_style_decls,
	  var_stroke_style, send_stroke_style, get_stroke_style,
	  rc_stroke_style, 4, stroke_style_termnames);


status
makeClassStrokeStyle(Class class)
{ return declareClass(class, &stroke_style_decls);
}
