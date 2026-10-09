/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org/packages/xpce/
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

/* Class shadow describes a drop shadow as CSS `box-shadow' does: the
 * shape of a graphical, moved by <-x_offset and <-y_offset, grown by
 * <-spread, blurred over <-blur pixels and painted in <-colour, which
 * normally has an alpha.  It is drawn outside the graphical's <-area,
 * which keeps its size.  See r_drop_shadow() and
 * get_extension_margin_graphical().
 */

static status
initialiseShadow(Shadow s, Int dx, Int dy, Int blur, Colour colour,
		 Int spread)
{ if ( notDefault(dx) )     assign(s, x_offset, dx);
  if ( notDefault(dy) )     assign(s, y_offset, dy);
  if ( notDefault(blur) )   assign(s, blur,     blur);
  if ( notDefault(colour) ) assign(s, colour,   colour);
  if ( notDefault(spread) ) assign(s, spread,   spread);

  return obtainClassVariablesObject(s);
}


/* An integer N is a soft shadow N pixels to the bottom right, blurred
 * over 2N pixels, in the default colour.  This is what the old hard
 * black shadow of N pixels becomes.  See also shadowGraphical(), which
 * maps 0 to no shadow.
 */

static Shadow
getConvertShadow(Any receiver, Any val)
{ Int i;

  if ( (i = toInteger(val)) && i != ZERO )
    answer(answerObject(ClassShadow, i, i, mul(i, TWO), DEFAULT, ZERO, EAV));

  fail;
}


/* The shadow object of the <-shadow slot of a graphical or NULL.  The
 * slot of a box or ellipse saved before class shadow existed holds an
 * integer.
 */

Shadow
toShadow(Any val)
{ if ( instanceOfObject(val, ClassShadow) )
    return val;
  if ( isInteger(val) && val != ZERO )
    return getConvertShadow(NIL, val);

  return NULL;
}


/* How far the shadow reaches beyond the area of the shape */

int
extentShadow(Shadow s)
{ int dx = abs((int)valInt(s->x_offset));
  int dy = abs((int)valInt(s->y_offset));

  return max(dx, dy) + valInt(s->blur) + max(0, (int)valInt(s->spread)) + 1;
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

static char *T_initialise[] =
{ "x_offset=[int]", "y_offset=[int]", "blur=[0..]", "colour=[colour]",
  "spread=[int]" };

static vardecl var_shadow[] =
{ IV(NAME_xOffset, "int", IV_GET,
     NAME_appearance, "Horizontal distance to the shape"),
  IV(NAME_yOffset, "int", IV_GET,
     NAME_appearance, "Vertical distance to the shape"),
  IV(NAME_blur, "0..", IV_GET,
     NAME_appearance, "Width of the blurred edge"),
  IV(NAME_colour, "colour", IV_GET,
     NAME_appearance, "Colour, normally with an alpha"),
  IV(NAME_spread, "int", IV_GET,
     NAME_appearance, "How much larger than the shape it is")
};

static senddecl send_shadow[] =
{ SM(NAME_initialise, 5, T_initialise, initialiseShadow,
     DEFAULT, "Create from offset, blur, colour and spread")
};

static getdecl get_shadow[] =
{ GM(NAME_convert, 1, "shadow", "int", getConvertShadow,
     DEFAULT, "Convert int N to a soft shadow N to the bottom right")
};

static classvardecl rc_shadow[] =
{ RC(NAME_xOffset, "int", "0",
     "Default horizontal distance to the shape"),
  RC(NAME_yOffset, "int", "3",
     "Default vertical distance to the shape"),
  RC(NAME_blur, "0..", "8",
     "Default width of the blurred edge"),
  RC(NAME_colour, "colour", "colour(@default, 0, 0, 0, 80)",
     "Default colour"),
  RC(NAME_spread, "int", "0",
     "Default growth of the shape")
};

static Name shadow_termnames[] =
	{ NAME_xOffset, NAME_yOffset, NAME_blur, NAME_colour, NAME_spread };

ClassDecl(shadow_decls,
          var_shadow, send_shadow, get_shadow, rc_shadow,
          5, shadow_termnames);


status
makeClassShadow(Class class)
{ return declareClass(class, &shadow_decls);
}
