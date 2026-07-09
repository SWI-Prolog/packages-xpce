/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi.prolog.org/projects/xpce/
    Copyright (c)  1985-2026, University of Amsterdam
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

/* Test the normalised ellipse-space distance of (px, py) from the
 * centre of the axis-aligned bounding box (ax, ay, aw, ah).  A return
 * value <= 1 means the point is inside the ellipse inscribed in that
 * box; larger values are outside.  Semi-axes below 1 are clamped to
 * avoid a division by zero on degenerate 0-width shapes.
 *
 * Shared with class circle via proto.h.
 */
double
ellipseNormDistance(int ax, int ay, int aw, int ah, int px, int py)
{ double rx = aw / 2.0;
  double ry = ah / 2.0;
  double cx = ax + rx;
  double cy = ay + ry;
  double dx, dy;

  if ( rx < 1 ) rx = 1;
  if ( ry < 1 ) ry = 1;
  dx = (px - cx) / rx;
  dy = (py - cy) / ry;
  return sqrt(dx*dx + dy*dy);
}


static status
initialiseEllipse(EllipseObj e, Int w, Int h)
{ initialiseGraphical(e, ZERO, ZERO, w, h);
  assign(e, shadow, ZERO);
  assign(e, fill, NIL);

  succeed;
}


static status
RedrawAreaEllipse(EllipseObj e, Area a)
{ int x, y, w, h;

  initialiseDeviceGraphical(e, &x, &y, &w, &h);
  NormaliseArea(x, y, w, h);
  r_dash(e->texture);

  if ( e->shadow != ZERO )
  { int shadow = valInt(e->shadow);
    Any fill = e->fill;

    if ( isNil(fill) )
      fill = NAME_background;

    if ( shadow > w ) shadow = w;
    if ( shadow > h ) shadow = h;

    r_thickness(0.0);
    r_ellipse(x+shadow, y+shadow, w-shadow, h-shadow, BLACK_COLOUR);
    r_thickness(valNum(e->pen));
    r_colour(DEFAULT);
    r_ellipse(x, y, w-shadow, h-shadow, fill);
  } else
  { r_thickness(valNum(e->pen));
    r_ellipse(x, y, w, h, e->fill);
  }

  return RedrawAreaGraphical(e, a);
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_initialise[] =
        { "width=[int]", "height=[int]" };

/* Instance Variables */

static vardecl var_ellipse[] =
{ SV(NAME_shadow, "int", IV_GET|IV_STORE, shadowGraphical,
     NAME_appearance, "Shadow painted below/right"),
  SV(NAME_fill, TYPE_FILL, IV_GET|IV_STORE, fillGraphical,
     NAME_appearance, "Fill pattern for internals")
};

static status
insideEllipse(EllipseObj e, Int xc, Int yc)
{ Area a = e->area;
  return ellipseNormDistance(valInt(a->x), valInt(a->y),
			       valInt(a->w), valInt(a->h),
			       valInt(xc), valInt(yc)) <= 1.0;
}


/* Event hit: inside the ellipse, or within event_tolerance pixels of
 * its outline (implemented as "inside a slightly larger ellipse").
 */
static status
inEventAreaEllipse(EllipseObj e, Int xc, Int yc)
{ static int evtol = -1;
  Area a = e->area;
  int ax = valInt(a->x), ay = valInt(a->y);
  int aw = valInt(a->w), ah = valInt(a->h);
  int px = valInt(xc), py = valInt(yc);

  if ( evtol < 0 )
  { Int v = getClassVariableValueObject(e, NAME_eventTolerance);
    evtol = (v ? valInt(v) : 5);
  }

  return ellipseNormDistance(ax - evtol, ay - evtol,
			       aw + 2*evtol, ah + 2*evtol,
			       px, py) <= 1.0;
}


/* Send Methods */

static char *T_inside[] =
	{ "x=int", "y=int" };

static senddecl send_ellipse[] =
{ SM(NAME_initialise, 2, T_initialise, initialiseEllipse,
     DEFAULT, "Create ellipse from width and height"),
  SM(NAME_inside, 2, T_inside, insideEllipse,
     NAME_event, "Test whether (X,Y) is inside the ellipse")
};

/* Get Methods */

#define get_ellipse NULL
/*
static getdecl get_ellipse[] =
{
};
*/

/* Resources */

static classvardecl rc_ellipse[] =
{ RC(NAME_selectionHandles, RC_REFINE, "sides",
     NULL)
};

/* Class Declaration */

static Name ellipse_termnames[] = { NAME_width, NAME_height };

ClassDecl(ellipse_decls,
          var_ellipse, send_ellipse, get_ellipse, rc_ellipse,
          2, ellipse_termnames);


status
makeClassEllipse(Class class)
{ declareClass(class, &ellipse_decls);

  cloneStyleVariableClass(class, NAME_fill, NAME_reference);
  setRedrawFunctionClass(class, RedrawAreaEllipse);
  setInEventAreaFunctionClass(class, inEventAreaEllipse);

  succeed;
}

