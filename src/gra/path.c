/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (C): 1985-2002, University of Amsterdam

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

#include <h/kernel.h>
#include <h/graphics.h>
#include <math.h>

static void	smooth_path(Path p);
static void	curve_fit(float *x, float *y, int ii,
			  float *u, float *v, int jj, int intervals);
static void	points_to_path(Path p, float *x, float *y, int n);
static status   computeBoundingBoxPath(Path);

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
  assign(p, fill_pattern,  NIL);
  assign(p, closed,	   OFF);

  if ( notDefault(points) )
    send(p, NAME_points, points, EAV);

  succeed;
}

		 /*******************************
		 *	      ARROWS		*
		 *******************************/

status
adjustFirstArrowPath(Path p)
{ if ( notNil(p->first_arrow) )
  { Chain points = (p->kind == NAME_smooth ? p->interpolation : p->points);

    if ( valInt(getSizeChain(points)) >= 2 )
    { Any av[4];
      Point tip = getHeadChain(points);
      Point ref = getNth1Chain(points, TWO);

      av[0] = add(tip->x, p->offset->x);
      av[1] = add(tip->y, p->offset->y);
      av[2] = add(ref->x, p->offset->x);
      av[3] = add(ref->y, p->offset->y);

      if ( qadSendv(p->first_arrow, NAME_points, 4, av) )
	return ComputeGraphical(p->first_arrow);
    }
  }

  fail;
}


status
adjustSecondArrowPath(Path p)
{ if ( notNil(p->second_arrow) )
  { Chain points = (p->kind == NAME_smooth ? p->interpolation : p->points);
    int size;

    if ( (size=valInt(getSizeChain(points))) >= 2 )
    { Any av[4];
      Point tip = getTailChain(points);
      Point ref = getNth1Chain(points, toInt(size-1));

      av[0] = add(tip->x, p->offset->x);
      av[1] = add(tip->y, p->offset->y);
      av[2] = add(ref->x, p->offset->x);
      av[3] = add(ref->y, p->offset->y);

      if ( qadSendv(p->second_arrow, NAME_points, 4, av) )
	return ComputeGraphical(p->second_arrow);
    }
  }

  fail;
}


		/********************************
		*             REDRAW		*
		********************************/

static status
RedrawAreaPath(Path p, Area a)
{ if ( valInt(getSizeChain(p->points)) >= 2 )
  { int x, y, w, h;
    int ox, oy;
    Chain points;

    initialiseDeviceGraphical(p, &x, &y, &w, &h);
    ox = x - valInt(p->area->x) + valInt(p->offset->x);
    oy = y - valInt(p->area->y) + valInt(p->offset->y);

    r_thickness(valInt(p->pen));
    r_dash(p->texture);
    if ( p->kind == NAME_smooth )
    { points = p->interpolation;
      r_path(points, ox, oy, 0, FALSE, p->fill_pattern);
    } else
    { points = p->points;
      r_path(points, ox, oy,
	     valInt(p->radius), p->closed == ON, p->fill_pattern);
    }

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
		valInt(pt->x) - iw2 + ox,
		valInt(pt->y) - ih2 + oy,
		iw, ih, ON);
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

      r_complement(bx-2+ox, by-2+oy, 5, 5);
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


static status
computeBoundingBoxPath(Path p)
{ Cell cell;
  Chain points = (p->kind == NAME_smooth ? p->interpolation : p->points);
  int minx = 1000000, miny = 1000000, maxx = -1000000, maxy = -10000000;

  for_cell(cell, points)
  { Point p = cell->value;
    int px = valInt(p->x);
    int py = valInt(p->y);

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
  { int pens = valInt(p->pen) / 2;
    int pena = (valInt(p->pen) % 2 == 0 ? pens : pens + 1);

    minx -= pens; maxx += pena;
    miny -= pens; maxy += pena;

    assign(p->area, x, toInt(minx + valInt(p->offset->x)));
    assign(p->area, y, toInt(miny + valInt(p->offset->y)));
    assign(p->area, w, toInt(maxx - minx));
    assign(p->area, h, toInt(maxy - miny));
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


static status
geometryPath(Path p, Int x, Int y, Int w, Int h)
{ int ox, ax, offx, ooffx;
  int oy, ay, offy, ooffy;
  Int ow, oh;
  float xf, yf;
  Cell cell;

  ComputeGraphical(p);
  ox = valInt(p->area->x);
  oy = valInt(p->area->y);
  ow = p->area->w;
  oh = p->area->h;

  CHANGING_GRAPHICAL(p,
        if ( ow == ZERO || oh == ZERO )	/* empty path */
	{ setArea(p->area, x, y, ow, oh);
	} else
	{ setArea(p->area, x, y, w, h);
	  ax = valInt(p->area->x);
	  ay = valInt(p->area->y);
	  ooffx = valInt(p->offset->x);
	  ooffy = valInt(p->offset->y);
	  offx = ooffx + ax - ox;
	  offy = ooffy + ay - oy;
	  xf = (float) valInt(p->area->w) / (float) valInt(ow);
	  yf = (float) valInt(p->area->h) / (float) valInt(oh);

	  assign(p->offset, x, toInt(offx));
	  assign(p->offset, y, toInt(offy));

	  for_cell(cell, p->points)
	  { Point pt = cell->value;
	    int nx = ax + rfloat((float) (valInt(pt->x)-ox+ooffx) * xf) - offx;
	    int ny = ay + rfloat((float) (valInt(pt->y)-oy+ooffy) * yf) - offy;

	    assign(pt, x, toInt(nx));
	    assign(pt, y, toInt(ny));
	  }

	  if ( p->kind == NAME_smooth && notNil(p->interpolation) )
	  { if ( xf == 1.0 && yf == 1.0 )
	    { Int dx = toInt(ax - ox - (offx - ooffx));
	      Int dy = toInt(ay - oy - (offy - ooffy));

	      for_cell(cell, p->interpolation)
		offsetPoint(cell->value, dx, dy);
	    } else
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
			 { Cell cell;

			   for_cell(cell, p->points)
			     offsetPoint(cell->value, dx, dy);
			   if ( notNil(p->interpolation) )
			     for_cell(cell, p->interpolation)
			       offsetPoint(cell->value, dx, dy);
			 } else
			 { offsetPoint(p->offset, dx, dy);
			 }
		       });
  }

  succeed;
}


static status
referencePath(Path p, Point r)
{ Int rx, ry, dx, dy;
  Area a = p->area;

  if ( isDefault(r) )
  { rx = a->x;
    ry = a->y;
  } else
  { rx = r->x;
    ry = r->y;
  }

  dx = sub(p->offset->x, rx);
  dy = sub(p->offset->y, ry);

  if ( dx || dy )
  { Cell cell;

    offsetPoint(p->offset, neg(dx), neg(dy));

    for_cell(cell, p->points)
      offsetPoint(cell->value, dx, dy);
    if ( notNil(p->interpolation) )
      for_cell(cell, p->interpolation)
	offsetPoint(cell->value, dx, dy);
  }

  succeed;
}


static status
resizePath(Path p, Real xfactor, Real yfactor, Point origin)
{ float xf, yf;
  int ox = valInt(p->area->x);
  int oy = valInt(p->area->y);
  int offx = valInt(p->offset->x);
  int offy = valInt(p->offset->y);
  Cell cell;

  init_resize_graphical(p, xfactor, yfactor, origin, &xf, &yf, &ox, &oy);
  if ( xf == 1.0 && yf == 1.0 )
    succeed;

  for_cell(cell, p->points)
  { Point pt = cell->value;
    int nx = ox + rfloat((float) (valInt(pt->x)+offx-ox) * xf) - offx;
    int ny = oy + rfloat((float) (valInt(pt->y)+offy-oy) * yf) - offy;

    assign(pt, x, toInt(nx));
    assign(pt, y, toInt(ny));
  }

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
  { Point p0 = NIL;
    Point pt = to;
    int bestd = INT_MAX;
    Chain ch = (p->kind == NAME_smooth ? p->interpolation : p->points);
    Cell cell;
    int tx = valInt(pt->x);
    int ty = valInt(pt->y);

    if ( ch->size == ZERO )		/* no points */
      fail;
    if ( ch->size == ONE )		/* one point? */
      answer(getDistancePoint(to, getHeadChain(ch)));

    for_cell(cell, ch)
    { if ( isNil(p0) )
      { p0 = cell->value;
      } else
      { Point p1 = cell->value;
	int   dl = distanceLineToPoint(valInt(p0->x), valInt(p0->y),
				       valInt(p1->x), valInt(p1->y),
				       tx, ty, FALSE);
	/*Cprintf("dl = %d\n", dl);*/
	bestd = min(bestd, dl);
	p0 = p1;
      }
    }

    answer(toInt(bestd));
  } else
    return getDistanceArea(p->area, ((Graphical)to)->area);
}








		 /*******************************
		 *	    SAVE/LOAD		*
		 *******************************/

static status
initialiseNewSlotPath(Path p, Variable var)
{ if ( var->name == NAME_offset )
    assign(p, offset, newObject(ClassPoint, EAV));
  else
    initialiseNewSlotGraphical((Graphical) p, var);

  succeed;
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_resize[] =
        { "factor_x=real", "factor_y=[real]", "origin=[point]" };
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
  SV(NAME_fillPattern, "image|colour*", IV_GET|IV_STORE, fillPatternGraphical,
     NAME_appearance, "Fill pattern"),
  SV(NAME_mark, "image*", IV_GET|IV_STORE, markPath,
     NAME_appearance, "Mark used for points"),
  SV(NAME_closed, "bool", IV_GET|IV_STORE, closedPath,
     NAME_appearance, "Draw line from last to first point"),
  IV(NAME_interpolation, "chain*", IV_GET,
     NAME_interpolation, "Derived interpolated points")
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
     NAME_compatibility, "Initialise <-offset"),
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
  SM(NAME_DrawPostScript, 1, "{head,body}", drawPostScriptPath,
     NAME_postscript, "Create PostScript")
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
     NAME_tip, "Start-point of path")
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
          2, path_termnames,
          "$Rev$");

status
makeClassPath(Class class)
{ declareClass(class, &path_decls);

  cloneStyleVariableClass(class, NAME_fillPattern, NAME_reference);
  setRedrawFunctionClass(class, RedrawAreaPath);

  succeed;
}
