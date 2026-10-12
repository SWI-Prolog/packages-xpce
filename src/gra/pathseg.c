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

/* Path segments.  A path's <-points is a chain of points.  A plain
 * point is a straight line to it from the previous point.  The classes
 * below are points too, so everything that works on the points of a
 * path keeps working, but they say how the path gets to them:
 *
 *   curve_point  A Bezier curve with one (quadratic) or two (cubic)
 *		  control points.
 *   arc_point	  An elliptical arc, specified as in SVG.
 *   move_point	  Jump there without drawing, starting a sub-path.
 *
 * Control points are in the same coordinate system as the points
 * themselves, i.e., relative to the path's <-offset.  The drawing,
 * bounding box and hit testing are in sdldraw.c (r_path() and
 * friends).
 */

		 /*******************************
		 *	     CURVE POINT	*
		 *******************************/

static status
initialiseCurvePoint(CurvePoint p, Int x, Int y, Point c1, Point c2)
{ initialisePoint((Point)p, x, y);
  assign(p, control1, c1);
  assign(p, control2, isDefault(c2) ? NIL : c2);

  succeed;
}


static char *T_initialiseCurvePoint[] =
	{ "x=int", "y=int", "control1=point", "control2=[point]*" };

static vardecl var_curve_point[] =
{ IV(NAME_control1, "point", IV_BOTH,
     NAME_dimension, "(First) control point"),
  IV(NAME_control2, "point*", IV_BOTH,
     NAME_dimension, "Second control point (@nil: quadratic curve)")
};

static senddecl send_curve_point[] =
{ SM(NAME_initialise, 4, T_initialiseCurvePoint, initialiseCurvePoint,
     DEFAULT, "Create from end point and control point(s)")
};

#define get_curve_point NULL
#define rc_curve_point NULL

static Name curve_point_termnames[] =
	{ NAME_x, NAME_y, NAME_control1, NAME_control2 };

ClassDecl(curve_point_decls,
	  var_curve_point, send_curve_point, get_curve_point, rc_curve_point,
	  4, curve_point_termnames);

status
makeClassCurvePoint(Class class)
{ return declareClass(class, &curve_point_decls);
}

		 /*******************************
		 *	      ARC POINT		*
		 *******************************/

static status
initialiseArcPoint(ArcPoint p, Int x, Int y, Num rx, Num ry,
		   Num rotation, BoolObj large, BoolObj sweep)
{ initialisePoint((Point)p, x, y);
  assign(p, rx,	      rx);
  assign(p, ry,	      isDefault(ry) ? rx : ry);
  assign(p, rotation, isDefault(rotation) ? ZERO : rotation);
  assign(p, large,    isDefault(large) ? OFF : large);
  assign(p, sweep,    isDefault(sweep) ? ON : sweep);

  succeed;
}


static char *T_initialiseArcPoint[] =
	{ "x=int", "y=int", "rx=num", "ry=[num]", "rotation=[num]",
	  "large=[bool]", "sweep=[bool]" };

static vardecl var_arc_point[] =
{ IV(NAME_rx, "num", IV_BOTH,
     NAME_dimension, "Horizontal radius of the ellipse"),
  IV(NAME_ry, "num", IV_BOTH,
     NAME_dimension, "Vertical radius of the ellipse"),
  IV(NAME_rotation, "num", IV_BOTH,
     NAME_dimension, "Rotation of the ellipse's x-axis in degrees"),
  IV(NAME_large, "bool", IV_BOTH,
     NAME_dimension, "@on: the arc of more than 180 degrees"),
  IV(NAME_sweep, "bool", IV_BOTH,
     NAME_dimension, "@on: clockwise (positive angles), @off: anti-clockwise")
};

static senddecl send_arc_point[] =
{ SM(NAME_initialise, 7, T_initialiseArcPoint, initialiseArcPoint,
     DEFAULT, "Create from end point and ellipse (as SVG `A')")
};

#define get_arc_point NULL
#define rc_arc_point NULL

static Name arc_point_termnames[] =
	{ NAME_x, NAME_y, NAME_rx, NAME_ry, NAME_rotation,
	  NAME_large, NAME_sweep };

ClassDecl(arc_point_decls,
	  var_arc_point, send_arc_point, get_arc_point, rc_arc_point,
	  7, arc_point_termnames);

status
makeClassArcPoint(Class class)
{ return declareClass(class, &arc_point_decls);
}

		 /*******************************
		 *	     MOVE POINT		*
		 *******************************/

static status
initialiseMovePoint(MovePoint p, Int x, Int y, BoolObj closed)
{ initialisePoint((Point)p, x, y);
  assign(p, closed, isDefault(closed) ? NIL : closed);

  succeed;
}


static char *T_initialiseMovePoint[] =
	{ "x=[int]", "y=[int]", "closed=[bool]*" };

static vardecl var_move_point[] =
{ IV(NAME_closed, "bool*", IV_BOTH,
     NAME_appearance, "Close the sub-path started here (@nil: as path)")
};

static senddecl send_move_point[] =
{ SM(NAME_initialise, 3, T_initialiseMovePoint, initialiseMovePoint,
     DEFAULT, "Create from position and whether the sub-path is closed")
};

#define get_move_point NULL
#define rc_move_point NULL

static Name move_point_termnames[] = { NAME_x, NAME_y, NAME_closed };

ClassDecl(move_point_decls,
	  var_move_point, send_move_point, get_move_point, rc_move_point,
	  3, move_point_termnames);

status
makeClassMovePoint(Class class)
{ return declareClass(class, &move_point_decls);
}
