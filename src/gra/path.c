/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org/packages/xpce/
    Copyright (c)  1985-2026, University of Amsterdam
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
#include <math.h>
#include <ctype.h>
#include <stdarg.h>

static void	smooth_path(Path p);
static void	curve_fit(float *x, float *y, int ii,
			  float *u, float *v, int jj, int intervals);
static void	points_to_path(Path p, float *x, float *y, int n);
static status   computeBoundingBoxPath(Path);
static status	insidePath(Path p, Int xc, Int yc);

static status
initialisePath(Path p, Name kind, Int radius_or_interval, Chain points)
{ if ( isDefault(kind) )
    kind = NAME_poly;

  assign(p, radius, ZERO);		/* defaults */
  assign(p, intervals, getClassVariableValueObject(p, NAME_intervals));

  if ( kind == NAME_poly )
  { if ( notDefault(radius_or_interval) )
      assign(p, radius, radius_or_interval);
  } else /*if ( kind == NAME_smooth )*/
  { if ( notDefault(radius_or_interval) )
      assign(p, intervals, radius_or_interval);
  }

  initialiseGraphical(p, ZERO, ZERO, ZERO, ZERO);
  assign(p, offset,	   newObject(ClassPoint, EAV));
  assign(p, kind,          kind);
  assign(p, points,        newObject(ClassChain, EAV));
  assign(p, interpolation, NIL);
  assign(p, fill,  NIL);
  assign(p, closed,	   OFF);
  assign(p, fill_rule,	   NAME_winding);
  assign(p, event_area,	   NAME_auto);

  if ( notDefault(points) )
    send(p, NAME_points, points, EAV);

  succeed;
}

/* The points we draw: the interpolated ones for a smooth path.  The
 * radius only applies to `poly' paths.
 */

static Chain
path_chain(Path p)
{ return p->kind == NAME_smooth ? p->interpolation : p->points;
}

static double
path_radius(Path p)
{ return p->kind == NAME_smooth ? 0.0 : valNum(p->radius);
}

static bool
path_drawable(Path p)
{ Chain ch = path_chain(p);

  return notNil(ch) && valInt(ch->size) >= 2;
}

		 /*******************************
		 *	      ARROWS		*
		 *******************************/

static status
adjustArrowPath(Path p, Graphical arrow, bool first)
{ double tx, ty, rx, ry;

  if ( notNil(arrow) && path_drawable(p) &&
       r_path_end(path_chain(p), path_radius(p), p->closed == ON, first,
		  &tx, &ty, &rx, &ry) )
  { double ox = valNum(p->offset->x);
    double oy = valNum(p->offset->y);
    Any av[4];

    av[0] = toNum(tx+ox);
    av[1] = toNum(ty+oy);
    av[2] = toNum(rx+ox);
    av[3] = toNum(ry+oy);

    if ( qadSendv(arrow, NAME_points, 4, av) )
    { assign(arrow, displayed, ON);	/* else RedrawArea() skips it */
      return ComputeGraphical(arrow);
    }
  }

  fail;
}


status
adjustFirstArrowPath(Path p)
{ return adjustArrowPath(p, p->first_arrow, true);
}


status
adjustSecondArrowPath(Path p)
{ return adjustArrowPath(p, p->second_arrow, false);
}


		/********************************
		*             REDRAW		*
		********************************/

static status
RedrawAreaPath(Path p, Area a)
{ if ( valInt(getSizeChain(p->points)) >= 2 )
  { int x, y, w, h;
    double ox, oy;

    initialiseDeviceGraphical(p, &x, &y, &w, &h);
    ox = x - valNum(p->area->x) + valNum(p->offset->x);
    oy = y - valNum(p->area->y) + valNum(p->offset->y);

    r_thickness(valNum(p->pen));
    r_dash(p->texture);
    if ( path_drawable(p) )
      r_path(path_chain(p), ox, oy, path_radius(p), p->closed == ON,
	     p->fill, p->fill_rule);

    if ( notNil(p->mark) )
    { Cell cell;
      Image i = p->mark;
      int iw = valInt(i->size->w);
      int ih = valInt(i->size->h);
      int iw2 = (iw+1)/2;
      int ih2 = (ih+1)/2;

      for_cell(cell, p->points)
      { Point pt = cell->value;

	r_image(i, 0, 0,
		(int)rint(valNum(pt->x) + ox) - iw2,
		(int)rint(valNum(pt->y) + oy) - ih2,
		iw, ih);
      }
    }

    if ( adjustFirstArrowPath(p) )
      RedrawArea(p->first_arrow, a);
    if ( adjustSecondArrowPath(p) )
      RedrawArea(p->second_arrow, a);
  }

  return RedrawAreaGraphical(p, a);
}


static status
paintSelectedPath(Path p)
{ PceWindow sw = getWindowGraphical((Graphical) p);

  if ( sw && sw->selection_feedback == (Any) NAME_handles )
  { int x, y, w, h;
    int ox, oy;
    Cell cell;

    initialiseDeviceGraphical(p, &x, &y, &w, &h);
    ox = x - valInt(p->area->x) + valInt(p->offset->x);
    oy = y - valInt(p->area->y) + valInt(p->offset->y);

    for_cell(cell, p->points)
    { Point pt = cell->value;
      int bx = valInt(pt->x);
      int by = valInt(pt->y);

      r_fill(bx-2+ox, by-2+oy, 5, 5, NAME_foreground);
    }

    succeed;
  } else
    return paintSelectedGraphical((Graphical)p);
}


static status
computePath(Path p)
{ if ( notNil(p->request_compute) )
  { CHANGING_GRAPHICAL(p,
		       if ( p->kind == NAME_smooth )
			 smooth_path(p);
		       else
			 assign(p, interpolation, NIL);
		       computeBoundingBoxPath(p);
		       changedEntireImageGraphical(p));

    assign(p, request_compute, NIL);
  }

  succeed;
}


/* The area is the box around the points, extended for the marks,
 * selection handles and pen as it always was, joined with what Cairo
 * says the drawing covers.  The latter is needed for curves and arcs,
 * which leave the box of their points, and for mitered corners.
 */

static status
computeBoundingBoxPath(Path p)
{ Cell cell;
  Chain points = path_chain(p);
  double minx = HUGE_VAL, miny = HUGE_VAL, maxx = -HUGE_VAL, maxy = -HUGE_VAL;

  if ( isNil(points) )
    points = p->points;

  for_cell(cell, points)
  { Point p = cell->value;
    double px = valNum(p->x);
    double py = valNum(p->y);

    if ( px < minx ) minx = px;
    if ( px > maxx ) maxx = px;
    if ( py < miny ) miny = py;
    if ( py > maxy ) maxy = py;
  }

  if ( notNil(p->mark) || p->selected == ON )
  { int mw=0, mh=0;

    if ( notNil(p->mark) )
    { mw = valInt(p->mark->size->w);
      mh = valInt(p->mark->size->h);
    }
    if ( p->selected == ON )		/* selection bubbles */
    { mw = max(mw, 5);
      mh = max(mh, 5);
    }

    minx -= (mw+1)/2;
    maxx += (mw+1)/2;
    miny -= (mh+1)/2;
    maxy += (mh+1)/2;
  }

  if ( maxx >= minx && maxy >= miny )
  { double pen  = valNum(p->pen);
    double pens = floor(pen/2.0);
    double pena = pen - pens;
    double x1, y1, x2, y2;

    minx -= pens; maxx += pena;
    miny -= pens; maxy += pena;

    if ( path_drawable(p) &&
	 r_path_extents(points, path_radius(p), p->closed == ON,
			pen, p->texture, &x1, &y1, &x2, &y2) )
    { if ( x1 < minx ) minx = x1;
      if ( y1 < miny ) miny = y1;
      if ( x2 > maxx ) maxx = x2;
      if ( y2 > maxy ) maxy = y2;
    }

    minx = floor(minx + valNum(p->offset->x));
    miny = floor(miny + valNum(p->offset->y));
    maxx = ceil(maxx + valNum(p->offset->x));
    maxy = ceil(maxy + valNum(p->offset->y));

    assign(p->area, x, toNum(minx));
    assign(p->area, y, toNum(miny));
    assign(p->area, w, toNum(maxx - minx));
    assign(p->area, h, toNum(maxy - miny));
  } else
    clearArea(p->area);

  if ( adjustFirstArrowPath(p) )
    unionNormalisedArea(p->area, p->first_arrow->area);
  if ( adjustSecondArrowPath(p) )
    unionNormalisedArea(p->area, p->second_arrow->area);

  succeed;
}

		/********************************
		*           SMOOTHING		*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The routine smooth_path() will  create a chain  of points describing a
smooth curve and assign this to  p->interpolation.  For this purpose it
uses the routine curve_fit.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
smooth_path(Path p)
{ float *x, *y, *u, *v;
  int npoints = valInt(getSizeChain(p->points));
  int ipoints;				/* interpolation points */
  Cell cell;
  int intervals = valInt(p->intervals);
  int px = 1000000, py = 10000000;

  if ( p->closed == ON )
    npoints++;

  x = (float *) alloca((npoints + 1) * sizeof(float));
  y = (float *) alloca((npoints + 1) * sizeof(float));

  npoints = 0;
  for_cell(cell, p->points)
  { Point pos = cell->value;

    if ( valInt(pos->x) != px || valInt(pos->y) != py )
    { px = valInt(pos->x);
      py = valInt(pos->y);

      x[npoints+1] = (float) px;
      y[npoints+1] = (float) py;

      npoints++;
    }
  }

  if ( p->closed == ON && notNil(p->points->head) ) /* Close the path? */
  { Point pos = (Point) p->points->head->value;

    if ( valInt(pos->x) != px || valInt(pos->y) != py )
    { px = valInt(pos->x);
      py = valInt(pos->y);

      x[npoints+1] = (float) px;
      y[npoints+1] = (float) py;

      npoints++;
    }
  }

  if ( npoints < 2 || intervals < 1 )
  { if ( notNil(p->interpolation) )
      clearChain(p->interpolation);
    else
      assign(p, interpolation, newObject(ClassChain, EAV));

    return;
  }

  ipoints = (npoints - 1) * intervals + 1;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The routine curvefit  was adapted from  FORTRAN code, in FORTRAN array
indices run from 1 to n, in C they run from 0 to n-1.  Hence the extra
space reserved below.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  u = (float *) alloca((ipoints + 1) * sizeof(float));
  v = (float *) alloca((ipoints + 1) * sizeof(float));

  curve_fit(x, y, npoints, u, v, ipoints, intervals);
  points_to_path(p, u, v, ipoints);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The algorithm used in the routine curvefit is based on the algorithm
described in

    A new method of interpolation and smooth curve fitting
    based on local procedures.
    by : Hiroshi Akima, ESSA Research Laboratories, Boulder, Colorado.
    [ Journal of the ACM, Vol. 17, October 1970, pp. 589-602 ]

A FORTRAN implementation of this algorithm is described in

    Algorithm 433 -- Interpolation and smooth curve fitting
    based on local procedures.
    by : Hiroshi Akima.
    [ CACM, Vol. 15, October 1972, pp. 914-918 ]

The implementation used in the routine curvefit is a direct
translation of the above.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
curve_fit(float *x, float *y, int ii, float *u, float *v, int jj, int intervals)
{ int l0 = ii, lm1 = ii-1, m0 = intervals, mm1 = intervals - 1, n0 = jj;
  int i, j, k, k5;
  float p1, r, rm;
  float q1, q2, q3;
  float a1, a2 = 0.0, a3 = 0.0, a4 = 0.0;
  float b1, b2 = 0.0, b3 = 0.0, b4 = 0.0;
  float x2 = 0.0, x3 = 0.0, x4 = 0.0, x5 = 0.0;
  float y2 = 0.0, y3 = 0.0, y4 = 0.0, y5 = 0.0;
  float sin2 = 0.0, sin3 = 0.0;
  float cos2 = 0.0, cos3 = 0.0;

					/* fill the exact points */
  k =n0+m0;
  i = l0+1;
  for (j=1;j<=l0;j++)
  { k -= m0;            i--;
    u[k] = x[i];        v[k] = y[i];
    DEBUG(NAME_path, Cprintf("(%f %f) at %d (from %d)\n", u[k], v[k], k, i));
  }
  rm = 1.0/(float)m0;   k5 = m0+1;

  for (i = 1; i<=l0 ; i++)
  { if (i > 1)
    { x2 = x3; y2 = y3; x3 = x4;
      y3 = y4; x4 = x5; y4 = y5;
      a1 = a2; b1 = b2; a2 = a3;
      b2 = b3; a3 = a4; b3 = b4;
      if (i >= lm1)
      { a4 = a3+a3-a2;
        b4 = b3+b3-b2;
      } else
      { k5 +=m0;
        x5 = u[k5]; y5 = v[k5];
        a4 = x5-x4; b4 = y5-y4;
      }
      cos2 = cos3; sin2 = sin3;
    } else
    { x3 = u[1]; y3 = v[1];
      x4 = u[m0+1]; y4 = v[m0+1];
      a3 = x4-x3; b3 = y4-y3;
      if (l0 != 2)
      { k5 += m0;
        x5 = u[k5]; y5 = v[k5];
        a4 = x5-x4; b4 = y5-y4;
      } else
      { a4 = a3; y4 = y3;
      }
      a2 = a3+a3-a4; a1 = a2+a2-a3;
      b2 = b3+b3-b4; b1 = b2+b2-b3;
    }

    q2 = fabs(a3*b4-a4*b3);
    q3 = fabs(a1*b2-a2*b1);
    if (q2+q3 == 0.0)
    { q2 = sqrt(a3*a3+b3*b3);
      q3 = sqrt(a2*a2+b2*b2);
    }

    cos3 = q2*a2+q3*a3;
    sin3 = q2*b2+q3*b3;
    r = cos3*cos3+sin3*sin3;

    if (r != 0.0)
    { r = sqrt(r);
      cos3 = cos3/r;
      sin3 = sin3/r;
    }

    if (i-1 > 0)
    { r = sqrt(a2*a2+b2*b2);
      DEBUG(NAME_path, Cprintf("a2 = %f; b2 = %f --> r = %f\n", a2, b2, r));
      DEBUG(NAME_path, Cprintf("cos2 = %f, cos3 = %f, sin2 = %f, sin3 = %f\n",
			       cos2, cos3, sin2, sin3));
      p1 = r*cos2;
      a1 = 3.0*a2-r*(cos2+cos2+cos3);
      b1 = a2-p1-a1;
      q1 = r*sin2;
      q2 = 3.0*b2-r*(sin2+sin2+sin3);
      q3 = b2-q1-q2;
      r = 0.0;
      DEBUG(NAME_path, Cprintf("p1=%f, a1=%f, b1=%f, q1=%f, q2=%f, q3=%f\n",
			       p1, a1, b1, q1, q2, q3));
      for (j=1;j<=mm1;j++)
      { k++;
        r +=rm;
        u[k] = x2+r*(p1+r*(a1+r*b1));
        v[k] = y2+r*(q1+r*(q2+r*q3));
	DEBUG(NAME_path, Cprintf("k = %d, r=%f, u[k] = %f, v[k] = %f\n",
				 k, r, u[k], v[k]));
      }
      k++;
    }
  }
}

static void
points_to_path(Path p, float *x, float *y, int n)
{ int i, ix, iy, X, Y;

  if ( isNil(p->interpolation) )
    assign(p, interpolation, newObject(ClassChain, EAV));
  else
    clearChain(p->interpolation);

  X = Y = 1000000;
  for ( i=1; i<=n; i++ )
  { ix = rfloat(x[i]);
    iy = rfloat(y[i]);

    if (ix == X && iy == Y)
      continue;
    X = ix, Y = iy;

    appendChain(p->interpolation,
		newObject(ClassPoint, toInt(X), toInt(Y), EAV));
  }
}

		/********************************
		*            EDITTING		*
		********************************/

static status
clearPath(Path p)
{ clearChain(p->points);
  return requestComputeGraphical(p, DEFAULT);
}


static status
pointsPath(Path p, Chain points)
{ if ( p->points != points )
  { Cell cell;
    Type TypePoint = nameToType(NAME_point);

    for_cell(cell, points)
    { if ( !instanceOfObject(cell->value, ClassPoint) )
      { Point pt = checkType(cell->value, TypePoint, p);

	if ( pt )
	  cellValueChain(points, PointerToInt(cell), pt);
	else
	  return errorPce(cell->value, NAME_unexpectedType, TypePoint);
      }
    }

    assign(p, points, points);
    requestComputeGraphical(p, DEFAULT);
  }

  succeed;
}


static status
appendPath(Path p, Point pt)
{ appendChain(p->points, pt);
  return requestComputeGraphical(p, DEFAULT);
}


static status
deletePath(Path p, Point pt)
{ if ( deleteChain(p->points, pt) )
    return requestComputeGraphical(p, DEFAULT);

  fail;
}


static status
insertPath(Path p, Point pt, Point after)
{ if ( insertAfterChain(p->points, pt, after) )
    return requestComputeGraphical(p, DEFAULT);

  fail;
}


static status
intervalsPath(Path p, Int i)
{ if ( p->intervals != i )
  { assign(p, intervals, i);
    requestComputeGraphical(p, DEFAULT);
  }

  succeed;
}


static status
closedPath(Path p, BoolObj val)
{ if ( val != p->closed )
  { assign(p, closed, val);
    return requestComputeGraphical(p, DEFAULT);
  }

  succeed;
}


static status
markPath(Path p, Image mark)
{ if ( mark != p->mark )
  { assign(p, mark, mark);
    return requestComputeGraphical(p, DEFAULT);
  }

  succeed;
}


static status
radiusPath(Path p, Int r)
{ if ( r != p->radius )
  { CHANGING_GRAPHICAL(p,
	assign(p, radius, r);
	changedEntireImageGraphical(p));
  }

  succeed;
}


static status
fillRulePath(Path p, Name rule)
{ if ( p->fill_rule != rule )
  { CHANGING_GRAPHICAL(p,
	assign(p, fill_rule, rule);
	changedEntireImageGraphical(p));
  }

  succeed;
}


static status
kindPath(Path p, Name kind)
{ if ( p->kind == kind )
    succeed;

  assign(p, kind, kind);
  requestComputeGraphical(p, DEFAULT);

  succeed;
}


static status
setPointPath(Path p, Point pt, Int x, Int y)
{ setPoint(pt, x, y);

  return requestComputeGraphical(p, DEFAULT);
}


static Any
getRadiusOrIntervalPath(Path p)
{ answer(p->kind == NAME_smooth ? (Any) p->intervals
				: (Any) p->radius);
}


		 /*******************************
		 *	   TRANSFORMATION	*
		 *******************************/

static void
transformPathCoord(Point pt, double sx, double tx, double sy, double ty)
{ assign(pt, x, toNum(valNum(pt->x)*sx + tx));
  assign(pt, y, toNum(valNum(pt->y)*sy + ty));
}


/* Scale the ellipse of an arc by sx,sy.  If the scaling is not uniform
 * the image of a rotated ellipse is an ellipse with other axes and
 * rotation, which we find from the singular value decomposition of
 * diag(sx,sy)*R(rotation)*diag(rx,ry).  Mirroring reverses the sweep.
 */

static void
transformArcPoint(ArcPoint ap, double sx, double sy)
{ double rx = valNum(ap->rx);
  double ry = valNum(ap->ry);

  if ( sx == sy && sx > 0.0 )
  { rx *= sx;
    ry *= sx;
  } else
  { double phi = valNum(ap->rotation)*M_PI/180.0;
    double a =  sx*cos(phi)*rx, b = -sx*sin(phi)*ry;
    double c =  sy*sin(phi)*rx, d =  sy*cos(phi)*ry;
    double E = (a+d)/2.0, F = (a-d)/2.0;
    double G = (c+b)/2.0, H = (c-b)/2.0;
    double Q = hypot(E, H), R = hypot(F, G);

    rx = Q+R;
    ry = fabs(Q-R);
    phi = (atan2(H, E) + atan2(G, F))/2.0;
    assign(ap, rotation, toNum(phi*180.0/M_PI));
    if ( sx*sy < 0.0 )
      assign(ap, sweep, ap->sweep == ON ? OFF : ON);
  }

  assign(ap, rx, toNum(rx));
  assign(ap, ry, toNum(ry));
}


/* Map all coordinates of the points to x' = sx*x+tx, y' = sy*y+ty.
 * This includes the control points of curves and the ellipses of arcs.
 */

static void
transformPathPoints(Chain points, double sx, double tx, double sy, double ty)
{ Cell cell;

  for_cell(cell, points)
  { Point pt = cell->value;

    transformPathCoord(pt, sx, tx, sy, ty);
    if ( instanceOfObject(pt, ClassCurvePoint) )
    { CurvePoint cp = (CurvePoint)pt;

      transformPathCoord(cp->control1, sx, tx, sy, ty);
      if ( notNil(cp->control2) )
	transformPathCoord(cp->control2, sx, tx, sy, ty);
    } else if ( instanceOfObject(pt, ClassArcPoint) &&
		(sx != 1.0 || sy != 1.0) )
    { transformArcPoint((ArcPoint)pt, sx, sy);
    }
  }
}


static void
translatePath(Path p, double dx, double dy)
{ transformPathPoints(p->points, 1.0, dx, 1.0, dy);
  if ( notNil(p->interpolation) )
    transformPathPoints(p->interpolation, 1.0, dx, 1.0, dy);
}


static status
geometryPath(Path p, Int x, Int y, Int w, Int h)
{ double ox, oy;
  Int ow, oh;

  ComputeGraphical(p);
  ox = valNum(p->area->x);
  oy = valNum(p->area->y);
  ow = p->area->w;
  oh = p->area->h;

  CHANGING_GRAPHICAL(p,
        if ( ow == ZERO || oh == ZERO )	/* empty path */
	{ setArea(p->area, x, y, ow, oh);
	} else
	{ setArea(p->area, x, y, w, h);
	  double ax = valNum(p->area->x);
	  double ay = valNum(p->area->y);
	  double ooffx = valNum(p->offset->x);
	  double ooffy = valNum(p->offset->y);
	  double offx = ooffx + ax - ox;
	  double offy = ooffy + ay - oy;
	  double xf = valNum(p->area->w) / valNum(ow);
	  double yf = valNum(p->area->h) / valNum(oh);

	  assign(p->offset, x, toNum(offx));
	  assign(p->offset, y, toNum(offy));

	  if ( xf != 1.0 || yf != 1.0 )	/* only the offset moves otherwise */
	  { transformPathPoints(p->points,
				xf, ax - (ox-ooffx)*xf - offx,
				yf, ay - (oy-ooffy)*yf - offy);
	    if ( p->kind == NAME_smooth && notNil(p->interpolation) )
	      smooth_path(p);
	  }
	});

  succeed;
}


static status
relativeMovePath(Path p,  Point diff, Name method)
{ Int dx = diff->x;
  Int dy = diff->y;

  if ( dx != ZERO || dy != ZERO )
  { CHANGING_GRAPHICAL(p,
		       { assign(p->area, x, add(p->area->x, dx));
			 assign(p->area, y, add(p->area->y, dy));

			 if ( method == NAME_points )
			   translatePath(p, valNum(dx), valNum(dy));
			 else
			   offsetPoint(p->offset, dx, dy);
		       });
  }

  succeed;
}


static status
referencePath(Path p, Point r)
{ Area a = p->area;
  double rx, ry;

  if ( isDefault(r) )
  { rx = valNum(a->x);
    ry = valNum(a->y);
  } else
  { rx = valNum(r->x);
    ry = valNum(r->y);
  }

  double dx = valNum(p->offset->x) - rx;
  double dy = valNum(p->offset->y) - ry;

  if ( dx != 0.0 || dy != 0.0 )
  { assign(p->offset, x, toNum(rx));
    assign(p->offset, y, toNum(ry));
    translatePath(p, dx, dy);
  }

  succeed;
}


static status
resizePath(Path p, Num xfactor, Num yfactor, Point origin)
{ float xf, yf;
  int ox = valInt(p->area->x);
  int oy = valInt(p->area->y);
  double offx = valNum(p->offset->x);
  double offy = valNum(p->offset->y);

  init_resize_graphical(p, xfactor, yfactor, origin, &xf, &yf, &ox, &oy);
  if ( xf == 1.0 && yf == 1.0 )
    succeed;

  transformPathPoints(p->points,
		      xf, ox + (offx-ox)*xf - offx,
		      yf, oy + (offy-oy)*yf - offy);

  return requestComputeGraphical(p, DEFAULT);
}


static Point
getStartPath(Path p)
{ answer(getHeadChain(p->points));
}


static Point
getEndPath(Path p)
{ answer(getTailChain(p->points));
}


static Point
getPointPath(Path p, Point pos, Int dist)
{ Point rval = NIL;
  int bestd = 0;			/* make gcc happy */
  Cell cell;

  if ( instanceOfObject(pos, ClassEvent) && notNil(p->device) )
  { pos = getPositionEvent((EventObj) pos, (Graphical) p->device);
    minusPoint(pos, p->offset);
  }

  if ( isDefault(dist) )
    dist = toInt(10);			/* TBD */

  for_cell(cell, p->points)
  { Point pt = cell->value;
    Int d = getDistancePoint(pt, pos);

    if ( valInt(d) < valInt(dist) && (isNil(rval) || valInt(d) < bestd) )
    { rval = pt;
      bestd = valInt(d);
    }
  }

  if ( notNil(rval) )
    answer(rval);

  fail;
}


static Point
getSegmentPath(Path p, Point pos, Int accept)
{ Point rval = NIL;
  int besth;				/* worst accepted */
  Cell cell;
  Point p0 = NIL;
  int d0 = 0;				/* keep gcc happy */

  if ( isDefault(accept) )
    besth = 100;
  else
    besth = valInt(accept);

  if ( instanceOfObject(pos, ClassEvent) && notNil(p->device) )
  { pos = getPositionEvent((EventObj) pos, (Graphical) p->device);
    minusPoint(pos, p->offset);
  }

  for_cell(cell, p->points)
  { if ( isNil(p0) )
    { p0 = cell->value;
      d0 = valInt(getDistancePoint(p0, pos));
    } else
    { Point p1 = cell->value;
      int   d1 = valInt(getDistancePoint(p1, pos));
      int   dt = max(1, valInt(getDistancePoint(p0, p1)));
      int    h = (1000 * (d0 + d1 - dt)) / dt;

      DEBUG(NAME_path,
	    writef("p0 = %N; p1 = %N; d0 = %d; d1 = %d; h = %d\n",
		   p0, p1, toInt(d0), toInt(d1), toInt(h)));

      if ( h < besth )
      { besth = h;
	rval = p0;
      }

      p0 = p1;
      d0 = d1;
    }
  }

  if ( notNil(rval) )
    answer(rval);

  fail;
}


static Int
getDistancePath(Path p, Any to)
{ ComputeGraphical(p);

  if ( instanceOfObject(to, ClassEvent) && notNil(p->device) )
  { to = getPositionEvent((EventObj) to, (Graphical) p->device);
    minusPoint(to, p->offset);
  }

  if ( instanceOfObject(to, ClassPoint) )
  { Point pt = to;
    Chain ch = path_chain(p);

    if ( isNil(ch) || ch->size == ZERO )	/* no points */
      fail;
    if ( ch->size == ONE )		/* one point? */
      answer(getDistancePoint(to, getHeadChain(ch)));

    double d = r_path_distance(ch, path_radius(p), p->closed == ON,
			       valNum(pt->x), valNum(pt->y));
    answer(toInt((intptr_t)rint(d)));
  } else
    return getDistanceArea(p->area, ((Graphical)to)->area);
}


		 /*******************************
		 *	   SVG PATH DATA	*
		 *******************************/

/* Read and write the `d' attribute of an SVG <path> element (SVG 1.1,
 * section 8.3).  All commands are supported.  Relative commands and
 * the shorthands H, V, S and T are converted to absolute points, so
 * <-svg_data does not reproduce the input literally.
 */

typedef struct svg_parser
{ const char *s;			/* current position */
  const char *error;			/* error message */
} svg_parser;

static void
svg_skip_sep(svg_parser *p)
{ while( *p->s == ' ' || *p->s == '\t' || *p->s == '\n' ||
	 *p->s == '\r' || *p->s == '\f' || *p->s == ',' )
    p->s++;
}

static bool
svg_number_ahead(svg_parser *p)
{ svg_skip_sep(p);
  int c = *p->s;

  return (c >= '0' && c <= '9') || c == '-' || c == '+' || c == '.';
}

static bool
svg_number(svg_parser *p, double *v)
{ char *e;

  svg_skip_sep(p);
  *v = strtod(p->s, &e);
  if ( e == p->s )
  { p->error = "number expected";
    return false;
  }
  p->s = e;
  return true;
}

static bool
svg_flag(svg_parser *p, bool *v)	/* flags may be written as "01" */
{ svg_skip_sep(p);
  if ( *p->s == '0' || *p->s == '1' )
  { *v = (*p->s == '1');
    p->s++;
    return true;
  }
  p->error = "flag (0 or 1) expected";
  return false;
}

static bool
svg_numbers(svg_parser *p, int n, double *v)
{ for(int i=0; i<n; i++)
  { if ( !svg_number(p, &v[i]) )
      return false;
  }
  return true;
}

static Point
svg_point(double x, double y)
{ return newObject(ClassPoint, toNum(x), toNum(y), EAV);
}

static status
svgDataPath(Path p, CharArray data)
{ const char *text = charArrayToUTF8(data);
  svg_parser parser = { .s = text };
  svg_parser *ps = &parser;
  Chain points = newObject(ClassChain, EAV);
  double cx = 0.0, cy = 0.0;		/* current point */
  double sx = 0.0, sy = 0.0;		/* start of the sub-path */
  double lcx = 0.0, lcy = 0.0;		/* last control point */
  int cmd = 0, last = 0;
  bool first_closed = false;
  bool in_subpath = false;		/* drawing after a moveto */
  MovePoint subpath = NULL;		/* move_point of this sub-path */
  int subpaths = 0;

  for(;;)
  { svg_skip_sep(ps);
    if ( !*ps->s )
      break;

    if ( isalpha((unsigned char)*ps->s) )
    { cmd = *ps->s++;
    } else if ( !cmd )
    { ps->error = "command expected";
      goto error;
    } else if ( cmd == 'M' || cmd == 'm' )
    { cmd = (cmd == 'M' ? 'L' : 'l');	/* implicit lineto after moveto */
    } else if ( cmd == 'Z' || cmd == 'z' )
    { ps->error = "command expected";
      goto error;
    }

    bool rel = islower(cmd);
    double bx = rel ? cx : 0.0;
    double by = rel ? cy : 0.0;
    double v[7];
    int ucmd = toupper(cmd);

    if ( ucmd != 'M' && ucmd != 'Z' && !in_subpath )
    { if ( subpaths == 0 )
      { ps->error = "path must start with a moveto";
	goto error;
      }
      subpath = newObject(ClassMovePoint, toNum(cx), toNum(cy), EAV);
      appendChain(points, subpath);	/* drawing on after a closepath */
      subpaths++;
      in_subpath = true;
    }

    switch(ucmd)
    { case 'M':
	if ( !svg_numbers(ps, 2, v) )
	  goto error;
	cx = sx = v[0]+bx;
	cy = sy = v[1]+by;
	if ( subpaths++ == 0 )
	{ appendChain(points, svg_point(cx, cy));
	} else
	{ subpath = newObject(ClassMovePoint, toNum(cx), toNum(cy), EAV);
	  appendChain(points, subpath);
	}
	in_subpath = true;
	break;
      case 'L':
	if ( !svg_numbers(ps, 2, v) )
	  goto error;
	cx = v[0]+bx;
	cy = v[1]+by;
	appendChain(points, svg_point(cx, cy));
	break;
      case 'H':
	if ( !svg_numbers(ps, 1, v) )
	  goto error;
	cx = v[0]+bx;
	appendChain(points, svg_point(cx, cy));
	break;
      case 'V':
	if ( !svg_numbers(ps, 1, v) )
	  goto error;
	cy = v[0]+by;
	appendChain(points, svg_point(cx, cy));
	break;
      case 'C':
      case 'S':
      { double c1x, c1y;

	if ( ucmd == 'C' )
	{ if ( !svg_numbers(ps, 6, v) )
	    goto error;
	  c1x = v[0]+bx; c1y = v[1]+by;
	  memmove(v, v+2, 4*sizeof(double));
	} else
	{ if ( !svg_numbers(ps, 4, v) )
	    goto error;
	  if ( last == 'C' || last == 'S' )	/* reflect last control */
	  { c1x = 2*cx-lcx; c1y = 2*cy-lcy;
	  } else
	  { c1x = cx; c1y = cy;
	  }
	}
	lcx = v[0]+bx; lcy = v[1]+by;
	cx = v[2]+bx;  cy = v[3]+by;
	appendChain(points,
		    newObject(ClassCurvePoint, toNum(cx), toNum(cy),
			      svg_point(c1x, c1y), svg_point(lcx, lcy), EAV));
	break;
      }
      case 'Q':
      case 'T':
	if ( ucmd == 'Q' )
	{ if ( !svg_numbers(ps, 4, v) )
	    goto error;
	  lcx = v[0]+bx; lcy = v[1]+by;
	  cx = v[2]+bx;  cy = v[3]+by;
	} else
	{ if ( !svg_numbers(ps, 2, v) )
	    goto error;
	  if ( last == 'Q' || last == 'T' )
	  { lcx = 2*cx-lcx; lcy = 2*cy-lcy;
	  } else
	  { lcx = cx; lcy = cy;
	  }
	  cx = v[0]+bx; cy = v[1]+by;
	}
	appendChain(points,
		    newObject(ClassCurvePoint, toNum(cx), toNum(cy),
			      svg_point(lcx, lcy), NIL, EAV));
	break;
      case 'A':
      { bool large, sweep;

	if ( !svg_numbers(ps, 3, v) ||
	     !svg_flag(ps, &large) || !svg_flag(ps, &sweep) ||
	     !svg_numbers(ps, 2, v+3) )
	  goto error;
	cx = v[3]+bx;
	cy = v[4]+by;
	appendChain(points,
		    newObject(ClassArcPoint, toNum(cx), toNum(cy),
			      toNum(v[0]), toNum(v[1]), toNum(v[2]),
			      large ? ON : OFF, sweep ? ON : OFF, EAV));
	break;
      }
      case 'Z':
	if ( subpath )
	  assign(subpath, closed, ON);
	else
	  first_closed = true;
	cx = sx; cy = sy;
	in_subpath = false;
	break;
      default:
	ps->error = "unknown command";
	goto error;
    }

    last = ucmd;
    if ( ucmd == 'Z' && svg_number_ahead(ps) )
    { ps->error = "command expected";
      goto error;
    }
  }

  if ( subpaths == 0 )
  { ps->error = "path must start with a moveto";
    goto error;
  }

  /* The path's <-closed is that of the first sub-path.  Only the move
   * points that differ from it keep an explicit <-closed.
   */
  Cell cell;
  for_cell(cell, points)
  { if ( instanceOfObject(cell->value, ClassMovePoint) )
    { MovePoint mp = cell->value;
      bool closed = (mp->closed == ON);

      assign(mp, closed, closed == first_closed ? (BoolObj)NIL
					      : (closed ? ON : OFF));
    }
  }

  assign(p, kind, NAME_poly);
  assign(p, closed, first_closed ? ON : OFF);
  return pointsPath(p, points);

error:
  { char msg[100];

    snprintf(msg, sizeof(msg), "%s at offset %d", ps->error,
	     (int)(ps->s - text));
    return errorPce(p, NAME_syntaxError, CtoName(msg));
  }
}


typedef struct svg_buffer
{ char  *data;
  size_t size;
  size_t allocated;
} svg_buffer;

static void
svg_printf(svg_buffer *b, const char *fmt, ...)
{ va_list args;

  for(;;)
  { size_t left = b->allocated - b->size;
    int n;

    va_start(args, fmt);
    n = vsnprintf(b->data+b->size, left, fmt, args);
    va_end(args);

    if ( n >= 0 && (size_t)n < left )
    { b->size += n;
      return;
    }
    b->allocated = b->allocated*2 + n + 1;
    b->data = pceRealloc(b->data, b->allocated);
  }
}

static void
svg_xy(svg_buffer *b, Point pt)
{ svg_printf(b, " %.15g %.15g", valNum(pt->x), valNum(pt->y));
}

static StringObj
getSvgDataPath(Path p)
{ svg_buffer b = { .data = pceMalloc(256), .allocated = 256 };
  bool closed = false;
  bool first = true;
  Cell cell;

  b.data[0] = EOS;
  for_cell(cell, p->points)
  { Point pt = cell->value;

    if ( first || instanceOfObject(pt, ClassMovePoint) )
    { if ( !first )
	svg_printf(&b, "%sM", closed ? " Z " : " ");
      else
	svg_printf(&b, "M");
      closed = (p->closed == ON);
      if ( instanceOfObject(pt, ClassMovePoint) &&
	   notNil(((MovePoint)pt)->closed) )
	closed = (((MovePoint)pt)->closed == ON);
      svg_xy(&b, pt);
      first = false;
    } else if ( instanceOfObject(pt, ClassCurvePoint) )
    { CurvePoint cp = (CurvePoint)pt;

      svg_printf(&b, isNil(cp->control2) ? " Q" : " C");
      svg_xy(&b, cp->control1);
      if ( notNil(cp->control2) )
	svg_xy(&b, cp->control2);
      svg_xy(&b, pt);
    } else if ( instanceOfObject(pt, ClassArcPoint) )
    { ArcPoint ap = (ArcPoint)pt;

      svg_printf(&b, " A %.15g %.15g %.15g %d %d",
		 valNum(ap->rx), valNum(ap->ry), valNum(ap->rotation),
		 ap->large == ON, ap->sweep == ON);
      svg_xy(&b, pt);
    } else
    { svg_printf(&b, " L");
      svg_xy(&b, pt);
    }
  }
  if ( closed )
    svg_printf(&b, " Z");

  StringObj s = CtoString(b.data);
  pceFree(b.data);

  answer(s);
}


		 /*******************************
		 *	    SAVE/LOAD		*
		 *******************************/

static status
initialiseNewSlotPath(Path p, Variable var)
{ if ( var->name == NAME_offset )
    assign(p, offset, newObject(ClassPoint, EAV));
  else if ( var->name == NAME_fillRule )
    assign(p, fill_rule, NAME_winding);
  else if ( var->name == NAME_eventArea )
    assign(p, event_area, NAME_auto);
  else
    initialiseNewSlotGraphical((Graphical) p, var);

  succeed;
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_resize[] =
        { "factor_x=num", "factor_y=[num]", "origin=[point]" };
static char *T_initialise[] =
        { "kind=[{poly,smooth}]",
	  "radius_or_interval=[int]",
	  "points=[chain]"
	};
static char *T_point[] =
        { "near=point|event", "max_distance=[int]" };
static char *T_setPoint[] =
        { "point=point", "x=[int]", "y=[int]" };
static char *T_insert[] =
        { "point", "point*" };
static char *T_geometry[] =
        { "x=[int]", "y=[int]", "width=[int]", "height=[int]" };
static char *T_relativeMove[] =
	{ "diff=point", "how=[{offset,points}]" };
static char *T_segment[] =
	{ "near=point|event", "accept=[0..]" };
static char *T_inside[] =
	{ "x=int", "y=int" };

/* Instance Variables */

static vardecl var_path[] =
{ IV(NAME_offset, "point", IV_GET,
     NAME_dimension, "Offset to origin"),
  SV(NAME_kind, "{poly,smooth}", IV_GET|IV_STORE, kindPath,
     NAME_interpolation, "Whether path is interpolated over points"),
  SV(NAME_radius, "int", IV_GET|IV_STORE, radiusPath,
     NAME_appearance, "Rounding radius for `poly' type"),
  SV(NAME_intervals, "0..100", IV_GET|IV_STORE, intervalsPath,
     NAME_interpolation, "Interpolation intervals between points"),
  SV(NAME_points, "chain", IV_GET|IV_STORE, pointsPath,
     NAME_points, "Chain of points"),
  SV(NAME_fill, TYPE_FILL, IV_GET|IV_STORE, fillGraphical,
     NAME_appearance, "Fill pattern"),
  SV(NAME_mark, "image*", IV_GET|IV_STORE, markPath,
     NAME_appearance, "Mark used for points"),
  SV(NAME_closed, "bool", IV_GET|IV_STORE, closedPath,
     NAME_appearance, "Draw line from last to first point"),
  IV(NAME_interpolation, "chain*", IV_GET,
     NAME_interpolation, "Derived interpolated points"),
  SV(NAME_fillRule, "{winding,even_odd}", IV_GET|IV_STORE, fillRulePath,
     NAME_appearance, "Which parts of a self-intersecting path are inside"),
  IV(NAME_eventArea, "{auto,painted,area,outline}", IV_BOTH,
     NAME_event, "Which part of the path accepts events")
};

/* Send Methods */

static senddecl send_path[] =
{ SM(NAME_compute, 0, NULL, computePath,
     DEFAULT, "Recompute interpolation and area"),
  SM(NAME_geometry, 4, T_geometry, geometryPath,
     DEFAULT, "Move and/or resize the path"),
  SM(NAME_relativeMove, 2, T_relativeMove, relativeMovePath,
     NAME_area, "Move path relative to current position"),
  SM(NAME_initialise, 3, T_initialise, initialisePath,
     DEFAULT, "Create from kind and intervals/radius"),
  SM(NAME_resize, 3, T_resize, resizePath,
     DEFAULT, "Resize path with specified factor"),
  SM(NAME_paintSelected, 0, NULL, paintSelectedPath,
     NAME_appearance, "Paint inverted drops on control-points"),
  SM(NAME_reference, 1, "reference=[point]", referencePath,
     NAME_area, "Move <-offset while retaining points"),
  SM(NAME_initialiseNewSlot, 1, "variable", initialiseNewSlotPath,
     NAME_compatibility, "Initialise <-offset, <-fill_rule and <-event_area"),
  SM(NAME_append, 1, "point", appendPath,
     NAME_points, "Append a point"),
  SM(NAME_clear, 0, NULL, clearPath,
     NAME_points, "Delete all points"),
  SM(NAME_delete, 1, "point", deletePath,
     NAME_points, "Delete a point"),
  SM(NAME_insert, 2, T_insert, insertPath,
     NAME_points, "Insert after 2nd argument (@nil: prepend)"),
  SM(NAME_setPoint, 3, T_setPoint, setPointPath,
     NAME_points, "Move (member) point to (X, Y)"),
  SM(NAME_inside, 2, T_inside, insidePath,
     NAME_event, "Test whether (X,Y) lies inside the polygon"),
  SM(NAME_svgData, 1, "char_array", svgDataPath,
     NAME_points, "Set the points from SVG path data (`d' attribute)")
};

/* Get Methods */

static getdecl get_path[] =
{ GM(NAME_point, 2, "point", T_point, getPointPath,
     NAME_event, "Find closest point"),
  GM(NAME_segment, 2, "point", T_segment, getSegmentPath,
     NAME_event, "Return start-point of closest line-segment"),
  GM(NAME_distance, 1, "int", "point|event|graphical", getDistancePath,
     NAME_compute, "Closest distance to line"),
  GM(NAME_radiusOrInterval, 0, "int", NULL, getRadiusOrIntervalPath,
     NAME_term, "Radius (`poly') or intervals (`smooth')"),
  GM(NAME_end, 0, "point", NULL, getEndPath,
     NAME_tip, "End-point of path"),
  GM(NAME_start, 0, "point", NULL, getStartPath,
     NAME_tip, "Start-point of path"),
  GM(NAME_svgData, 0, "string", NULL, getSvgDataPath,
     NAME_points, "The points as SVG path data (`d' attribute)")
};

/* Resources */

static classvardecl rc_path[] =
{ RC(NAME_intervals, "int", "10",
     "Number of interpolated points"),
  RC(NAME_selectionHandles, RC_REFINE, "@nil",
     NULL)
};

/* Class Declaration */

static Name path_termnames[] = { NAME_kind, NAME_radiusOrInterval };

ClassDecl(path_decls,
          var_path, send_path, get_path, rc_path,
          2, path_termnames);

		 /*******************************
		 *	HIT-TEST FOR EVENTS	*
		 *******************************/

/* Public send method: test whether (X, Y) — expressed in the path's
 * device coordinate system — is inside the area the path encloses,
 * using its <-fill_rule.  The path's `<-closed' flag is not consulted,
 * so an "open" polygon can still be treated as a closed region for
 * hit-testing purposes.  Fails silently if there are fewer than two
 * points.  Two points only enclose an area if one is a curve or arc.
 */
static status
insidePath(Path p, Int xc, Int yc)
{ if ( !path_drawable(p) )
    fail;

  return r_path_hit(path_chain(p), path_radius(p), p->closed == ON, p->fill_rule,
		    true, 0.0,
		    valNum(xc) - valNum(p->offset->x),
		    valNum(yc) - valNum(p->offset->y));
}


/* Hit-test for events.  <-event_area selects what is hit:
 *
 *   - auto:    the area if the path is closed or filled, as well as
 *		anything within <-event_tolerance of the line.
 *   - painted: exactly what is painted: the fill if the path is filled
 *		and the line as wide as <-pen.  Use this for shapes that
 *		touch, such as the tiles of a chart.
 *   - area:    the area enclosed by the path, filled or not.
 *   - outline: anything within <-event_tolerance of the line.
 */

static status
inEventAreaPath(Path p, Int xc, Int yc)
{ static int evtol = -1;
  bool area;
  double tolerance;

  if ( evtol < 0 )
  { Int v = getClassVariableValueObject(p, NAME_eventTolerance);
    evtol = (v ? valInt(v) : 5);
  }

  if ( !path_drawable(p) )
    fail;

  if ( p->event_area == NAME_painted )
  { area = notNil(p->fill);
    tolerance = valNum(p->pen)/2.0;
  } else if ( p->event_area == NAME_area )
  { area = true;
    tolerance = 0.0;
  } else if ( p->event_area == NAME_outline )
  { area = false;
    tolerance = (double)evtol;
  } else				/* auto */
  { area = (p->closed == ON || notNil(p->fill));
    tolerance = (double)evtol;
  }

  return r_path_hit(path_chain(p), path_radius(p), p->closed == ON,
		    p->fill_rule, area, tolerance,
		    valNum(xc) - valNum(p->offset->x),
		    valNum(yc) - valNum(p->offset->y));
}


status
makeClassPath(Class class)
{ declareClass(class, &path_decls);

  cloneStyleVariableClass(class, NAME_fill, NAME_reference);
  setRedrawFunctionClass(class, RedrawAreaPath);
  setInEventAreaFunctionClass(class, inEventAreaPath);

  succeed;
}
