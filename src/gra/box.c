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
#include <h/graphics.h>

static status
initialiseBox(Box b, Int w, Int h)
{ initialiseGraphical(b, ZERO, ZERO, w, h);
  assign(b, radius,	  ZERO);
  assign(b, shadow,	  ZERO);
/*assign(b, fill_pattern, NIL);
  assign(b, fill_offset,  NIL);
*/

  succeed;
}


static status
RedrawAreaBox(Box b, Area a)
{ int x, y, w, h;
  fill_state state;

  initialiseDeviceGraphical(b, &x, &y, &w, &h);

  r_filloffset(b->fill_offset, x, y, &state);
  r_thickness(valInt(b->pen));
  r_dash(b->texture);
  r_shadow_box(x, y, w, h,
	       valInt(b->radius), valInt(b->shadow), b->fill_pattern);
  r_fillrestore(&state);

  return RedrawAreaGraphical(b, a);
}


static status
radiusBox(Box b, Int r)
{ if (r != b->radius)
  { CHANGING_GRAPHICAL(b, assign(b, radius, r);
		          changedEntireImageGraphical(b));
  }

  succeed;
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_initialise[] =
        { "width=[int]", "height=[int]" };

/* Instance Variables */

static vardecl var_box[] =
{ SV(NAME_radius, "int", IV_GET|IV_STORE, radiusBox,
     NAME_appearance, "Rounding radius for corners"),
  SV(NAME_shadow, "int", IV_GET|IV_STORE, shadowGraphical,
     NAME_appearance, "Shadow at bottom-right of box"),
  SV(NAME_fillPattern, "image|colour*", IV_GET|IV_STORE, fillPatternGraphical,
     NAME_appearance, "Fill pattern for internals"),
  SV(NAME_fillOffset, "point*", IV_GET|IV_STORE, fillOffsetGraphical,
     NAME_appearance, "Offset for using <-fill_pattern")
};

/* Send Methods */

static senddecl send_box[] =
{ SM(NAME_initialise, 2, T_initialise, initialiseBox,
     DEFAULT, "Create box from width and height"),
  SM(NAME_DrawPostScript, 1, "{head,body}", drawPostScriptBox,
     NAME_postscript, "Create PostScript")
};

/* Get Methods */

#define get_box NULL
/*
static getdecl get_box[] =
{
};
*/

/* Resources */

#define rc_box NULL
/*
static classvardecl rc_box[] =
{
};
*/

/* Class Declaration */

static Name box_termnames[] = { NAME_width, NAME_height };

ClassDecl(box_decls,
          var_box, send_box, get_box, rc_box,
          2, box_termnames,
          "$Rev$");


status
makeClassBox(Class class)
{ declareClass(class, &box_decls);

  cloneStyleVariableClass(class, NAME_fillPattern, NAME_reference);
  setRedrawFunctionClass(class, RedrawAreaBox);

  succeed;
}
