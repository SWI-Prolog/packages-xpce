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
#include "math.h"

static status	drawArrow(double x1, double y1, double x2, double y2,
			  double x3, double y3,
			  Any fill, double pen, Name texture, Name style);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Arrows are defined in terms of the point at the tip the length of the arrow
head and the width of the two wings.  The direction of the arrow is
determined by a reference point on an imaginery line through the reference
point and the tip:


		 -     |\
		 |     |  \
		 |     |    \
	wing >=  |     |      \    <= tip (point)
		 |     |      /
		 |     |    /
		 |     |  /
		 -     |/

		       |------|    <= length

Although arrows are a sub-class of graphical this feature (in particular the
area of the graphical) should not be used.  Changing the area of an arrow
has no well-defined meaning.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static status pointsArrow(Arrow a, Int tx, Int ty, Int rx, Int ry);

static status
initialiseArrow(Arrow a, Int length, Int wing, Name style, Any fill)
{ initialiseGraphical(a, ZERO, ZERO, ONE, ONE);

  if ( notDefault(length) )	assign(a, length, length);
  if ( notDefault(wing) )	assign(a, wing, wing);
  if ( notDefault(style) )	assign(a, style, style);
  if ( notDefault(fill) )	assign(a, fill_pattern, fill);

  assign(a, tip,       newObject(ClassPoint, toInt(10), toInt(10), EAV));
  assign(a, reference, newObject(ClassPoint, EAV));
  assign(a, left,      newObject(ClassPoint, EAV));
  assign(a, right,     newObject(ClassPoint, EAV));

  obtainClassVariablesObject(a);

  if ( notNil(a->fill_pattern) )
    assign(a, pen, ZERO);

  requestComputeGraphical(a, DEFAULT);

  succeed;
}


static status
computeArrow(Arrow a)
{ if ( notNil(a->request_compute) )
  { double x1, y1, x2, y2;
    double x, y, w, h;
    double sx, sy, rx, ry;
    double xdiff, ydiff;
    double cdl1, sdl1, cl2, sl2;
    double l1, l2, d;
    double sin_theta, cos_theta;
    bool changed = false;

    x1 = valNum(a->reference->x);
    y1 = valNum(a->reference->y);
    x2 = valNum(a->tip->x);
    y2 = valNum(a->tip->y);

    l1 = valNum(a->length);
    l2 = valNum(a->wing)/2.0;

    xdiff = x2 - x1;
    ydiff = y2 - y1;

    d = sqrt(xdiff*xdiff + ydiff*ydiff);
    if (d < 0.0000001)
    { cos_theta = 1.0;
      sin_theta = 0.0;
    } else
    { cos_theta = xdiff / d;
      sin_theta = ydiff / d;
    }

    cdl1 = cos_theta * (d-l1);
    sdl1 = sin_theta * (d-l1);
    cl2  = cos_theta * l2;
    sl2  = sin_theta * l2;

    sx = x1 + cdl1 - sl2;
    sy = y1 + sdl1 + cl2;
    rx = x1 + cdl1 + sl2;
    ry = y1 + sdl1 - cl2;

    if ( a->left->x != toNum(sx) )
    { assign(a->left, x, toNum(sx));
      changed = true;
    }
    if ( a->left->y != toNum(sy) )
    { assign(a->left, y, toNum(sy));
      changed = true;
    }
    if ( a->right->x != toNum(rx) )
    { assign(a->right, x, toNum(rx));
      changed = true;
    }
    if ( a->right->y != toNum(ry) )
    { assign(a->right, y, toNum(ry));
      changed = true;
    }

    x = min(x2, min(sx, rx));
    y = min(y2, min(sy, ry));
    w = max(x2, max(sx, rx)) - x + 1;
    h = max(y2, max(sy, ry)) - y + 1;

    CHANGING_GRAPHICAL(a,
		       { setArea(a->area,
				 toInt(x), toInt(y), toInt(w), toInt(h));
			 if ( changed )
			   changedEntireImageGraphical(a);
		       });

    assign(a, request_compute, NIL);
  }

  succeed;
}


static status
geometryArrow(Arrow a, Int x, Int y, Int w, Int h)
{ if ( notDefault(x) || notDefault(y) )
  { double dx, dy;

    ComputeGraphical(a);
    dx = valNum(x)-valNum(a->area->x);
    dy = valNum(y)-valNum(a->area->y);

    pointsArrow(a, toNum(valNum(a->tip->x)+dx),
		   toNum(valNum(a->tip->y)+dy),
		   toNum(valNum(a->reference->x)+dx),
		   toNum(valNum(a->reference->y)+dy));
  }

  succeed;
}


static status
RedrawAreaArrow(Arrow a, Area area)
{ drawArrow(valNum(a->left->x),  valNum(a->left->y),
	    valNum(a->tip->x),   valNum(a->tip->y),
	    valNum(a->right->x), valNum(a->right->y),
	    a->fill_pattern,
	    valNum(a->pen),
	    a->texture,
	    a->style);

  return RedrawAreaGraphical(a, area);
}


static status
drawArrow(double x1, double y1, double x2, double y2, double x3, double y3,
	  Any fill, double pen, Name texture, Name style)
{ fpoint pts[3];

  /* TBD: Cairo can paint the line and fill from the same path */
  if ( notNil(fill) )
  { pts[0].x = x1;
    pts[0].y = y1;
    pts[1].x = x2;
    pts[1].y = y2;
    pts[2].x = x3;
    pts[2].y = y3;

    r_fillpattern(fill, NAME_foreground);
    r_fill_polygon(pts, 3);
  }

  if ( pen > 0 )
  { r_dash(texture);
    r_thickness(pen);
    r_line(x1, y1, x2, y2);
    r_line(x2, y2, x3, y3);
    if ( style == NAME_closed )
      r_line(x3, y3, x1, y1);
  }

  succeed;
}


static status
tipArrow(Arrow a, Point p)
{ return pointsArrow(a, p->x, p->y, DEFAULT, DEFAULT);
}

static status
tipXArrow(Arrow a, Int x)
{ return pointsArrow(a, x, DEFAULT, DEFAULT, DEFAULT);
}

static status
tipYArrow(Arrow a, Int y)
{ return pointsArrow(a, DEFAULT, y, DEFAULT, DEFAULT);
}


static status
referenceArrow(Arrow a, Point p)
{ return pointsArrow(a, DEFAULT, DEFAULT, p->x, p->y);
}

static status
referenceXArrow(Arrow a, Int x)
{ return pointsArrow(a, DEFAULT, DEFAULT, x, DEFAULT);
}

static status
referenceYArrow(Arrow a, Int y)
{ return pointsArrow(a, DEFAULT, DEFAULT, DEFAULT, y);
}


static Int
getTipXArrow(Arrow a)
{ answer(a->tip->x);
}

static Int
getTipYArrow(Arrow a)
{ answer(a->tip->y);
}

static Int
getReferenceXArrow(Arrow a)
{ answer(a->reference->x);
}


static Int
getReferenceYArrow(Arrow a)
{ answer(a->reference->y);
}


static status
styleArrow(Arrow a, Name style)
{ if ( a->style != style )
  { CHANGING_GRAPHICAL(a,
	assign(a, style, style);
	changedEntireImageGraphical(a));
  }
  succeed;
}


static status
lengthArrow(Arrow a, Int l)
{ if ( a->length != l )
  { assign(a, length, l);
    requestComputeGraphical(a, DEFAULT);
  }
  succeed;
}


static status
wingArrow(Arrow a, Int w)
{ if ( a->wing != w )
  { assign(a, wing, w);
    requestComputeGraphical(a, DEFAULT);
  }
  succeed;
}


static status
pointsArrow(Arrow a, Int tx, Int ty, Int rx, Int ry)
{ Point tip = a->tip;
  Point ref = a->reference;

  if (isDefault(tx)) tx = tip->x;
  if (isDefault(ty)) ty = tip->y;
  if (isDefault(rx)) rx = ref->x;
  if (isDefault(ry)) ry = ref->y;

  if (tx != tip->x || ty != tip->y || rx != ref->x || ry != ref->y)
  { assign(tip, x, tx);
    assign(tip, y, ty);
    assign(ref, x, rx);
    assign(ref, y, ry);
    requestComputeGraphical(a, DEFAULT);
  }

  succeed;
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declaractions */

static char *T_initialise[] =
        { "length=[int]", "wing=[int]",
	  "style=[{open,closed}]", "fill=[image|colour]*"
	};
static char *T_points[] =
        { "tip_x=[int]", "tip_y=[int]",
	  "reference_x=[int]", "reference_y=[int]"
	};
static char *T_geometry[] =
	{ "x=[int]", "y=[int]", "width=[int]", "height=[int]" };


/* Instance Variables */

static vardecl var_arrow[] =
{ SV(NAME_tip, "point", IV_GET|IV_STORE, tipArrow,
     NAME_area, "Tip of the arrow"),
  SV(NAME_reference, "point", IV_GET|IV_STORE, referenceArrow,
     NAME_area, "Where arrow points to"),
  SV(NAME_length, "int", IV_GET|IV_STORE, lengthArrow,
     NAME_area, "Distance tip to base"),
  SV(NAME_wing, "int", IV_GET|IV_STORE, wingArrow,
     NAME_area, "Length of base"),
  SV(NAME_fillPattern, TYPE_FILL, IV_GET|IV_STORE, fillPatternGraphical,
     NAME_appearance, "How it is filled"),
  SV(NAME_style, "{open,closed}", IV_GET|IV_STORE, styleArrow,
     NAME_appearance, "If `closed', the triangle is closed"),
  IV(NAME_left, "point", IV_NONE,
     NAME_internal, "Left-end of base"),
  IV(NAME_right, "point", IV_NONE,
     NAME_internal, "Right-end of base")
};

/* Send Methods */

static senddecl send_arrow[] =
{ SM(NAME_compute, 0, NULL, computeArrow,
     DEFAULT, "Compute <-tip, <-left and <-right"),
  SM(NAME_geometry, 4, T_geometry, geometryArrow,
     DEFAULT, "Move arrow"),
  SM(NAME_initialise, 4, T_initialise, initialiseArrow,
     DEFAULT, "Create from length and wing"),
  SM(NAME_points, 4, T_points, pointsArrow,
     NAME_area, "Set XY of tip and reference"),
  SM(NAME_referenceX, 1, "int", referenceXArrow,
     NAME_area, "Set X of reference"),
  SM(NAME_referenceY, 1, "int", referenceYArrow,
     NAME_area, "Set Y of reference"),
  SM(NAME_tipX, 1, "int", tipXArrow,
     NAME_area, "Set X of tip"),
  SM(NAME_tipY, 1, "int", tipYArrow,
     NAME_area, "Set Y of tip"),
  SM(NAME_DrawPostScript, 1, "{head,body}", drawPostScriptArrow,
     NAME_postscript, "Create PostScript")
};

/* Get Methods */

static getdecl get_arrow[] =
{ GM(NAME_referenceX, 0, "int", NULL, getReferenceXArrow,
     NAME_area, "X of reference point"),
  GM(NAME_referenceY, 0, "int", NULL, getReferenceYArrow,
     NAME_area, "Y of reference point"),
  GM(NAME_tipX, 0, "int", NULL, getTipXArrow,
     NAME_area, "X of tip point"),
  GM(NAME_tipY, 0, "int", NULL, getTipYArrow,
     NAME_area, "Y of tip  point")
};

/* Resources */

static classvardecl rc_arrow[] =
{ RC(NAME_fillPattern, TYPE_FILL, "foreground",
     "Fill pattern for the triangle"),
  RC(NAME_length, "int", "2.5mm",
     "Distance tip to base (2.5mm)"),
  RC(NAME_style, "{open,closed}", "closed",
     "Whether or not the wing is closed"),
  RC(NAME_selectionHandles, RC_REFINE, "sides",
     NULL),
  RC(NAME_wing, "int", "1.75mm",
     "Width of wing (1.75mm)")
};

/* Class Declaration */

static Name arrow_termnames[] = { NAME_length, NAME_wing };

ClassDecl(arrow_decls,
          var_arrow, send_arrow, get_arrow, rc_arrow,
          2, arrow_termnames,
          "$Rev$");


status
makeClassArrow(Class class)
{ declareClass(class, &arrow_decls);

  cloneStyleVariableClass(class, NAME_fillPattern, NAME_reference);
  setRedrawFunctionClass(class, RedrawAreaArrow);

  succeed;
}
