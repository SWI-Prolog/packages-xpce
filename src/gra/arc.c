/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org/projects/xpce/
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
#ifndef M_PI
#define M_PI (3.14159265358979323846)
#endif

#define COS(x) cos(((x) * M_PI) / 180.0)
#define SIN(x) sin(((x) * M_PI) / 180.0)

static status
initialiseArc(ArcObj a, Int radius, Int start_angle, Int size_angle)
{ initialiseJoint((Joint) a, ZERO, ZERO, ZERO, ZERO, DEFAULT);

  if ( isDefault(radius) )
    radius = getClassVariableValueObject(a, NAME_radius);
  if ( isDefault(start_angle) )
    start_angle = toNum(0.0);
  if ( isDefault(size_angle) )
    size_angle = toNum(90.0);

  assign(a, size,	  newObject(ClassSize, radius, radius, EAV));
  assign(a, position,	  newObject(ClassPoint, EAV));
  assign(a, start_angle,  start_angle);
  assign(a, size_angle,	  size_angle);
  assign(a, close,	  NAME_none);

  return requestComputeGraphical(a, DEFAULT);
}


void
points_arc(ArcObj a, double *sx, double *sy, double *ex, double *ey)
{ double cx = valInt(a->position->x);
  double cy = valInt(a->position->y);
  double aw = valInt(a->size->w);
  double ah = valInt(a->size->h);
  double start = valNum(a->start_angle);
  double size  = valNum(a->size_angle);

  if ( sx )
    *sx = cx + aw * COS(start);
  if ( sy )
    *sy = cy - ah * SIN(start);
  if ( ex )
    *ex = cx + aw * COS(start + size);
  if ( ey )
    *ey = cy - ah * SIN(start + size);
}


/* Compute (tip, reference) for an arrow attached to an arc end.
 *
 * The tip sits on the arc at the endpoint.  The reference is placed at
 * the arc point that is at chord-distance arrow-><-length from the tip
 * — looking forward into the arc for is_first, backward for the second.
 * That makes the arrow's wing line align with the local chord rather
 * than the tangent, so the wing's midpoint lands on (or very close to)
 * the arc, eliminating the visible offset between arc and wing centre.
 *
 * For an ellipse (aw != ah) the chord-distance is computed against the
 * mean radius — exact for circles, a usable approximation otherwise.
 * Falls back to the historic tangent direction when the arc is too
 * small to fit the arrow.
 */
static void
arc_arrow_endpoints(ArcObj a, Arrow ar, bool is_first,
		    double *tip_x, double *tip_y,
		    double *ref_x, double *ref_y)
{ double cx = valInt(a->position->x);
  double cy = valInt(a->position->y);
  double aw = valInt(a->size->w);
  double ah = valInt(a->size->h);
  double start = valNum(a->start_angle);
  double size  = valNum(a->size_angle);
  double tip_angle = is_first ? start : start + size;
  double l1 = valNum(ar->length);
  double r_avg = (aw + ah) / 2.0;

  *tip_x = cx + aw * COS(tip_angle);
  *tip_y = cy - ah * SIN(tip_angle);

  if ( r_avg > 0.0 && l1 > 0.0 && l1 < 2.0*r_avg )
  { double sign = (size >= 0.0) ? 1.0 : -1.0;
    double delta = 2.0 * asin(l1 / (2.0 * r_avg)) * 180.0 / M_PI;
    double ref_angle = is_first
		     ? tip_angle + sign * delta
		     : tip_angle - sign * delta;
    *ref_x = cx + aw * COS(ref_angle);
    *ref_y = cy - ah * SIN(ref_angle);
  } else
  { if ( (is_first && size >= 0.0) || (!is_first && size < 0.0) )
    { *ref_x = *tip_x + (*tip_y - cy);
      *ref_y = *tip_y - (*tip_x - cx);
    } else
    { *ref_x = *tip_x - (*tip_y - cy);
      *ref_y = *tip_y + (*tip_x - cx);
    }
  }
}


static status
RedrawAreaArc(ArcObj a, Area area)
{ int x, y, w, h;
  double aw = valInt(a->size->w);
  double ah = valInt(a->size->h);
  double cx, cy;

  initialiseDeviceGraphical(a, &x, &y, &w, &h);

  cx = valInt(a->position->x);
  cy = valInt(a->position->y);

  r_thickness(valInt(a->pen));
  r_dash(a->texture);

  r_arc(cx - aw, cy - ah,
	2*aw, 2*ah,
	valNum(a->start_angle), valNum(a->size_angle),
	a->close,
	a->fill);

  if ( notNil(a->first_arrow) )
  { Any av[4];
    double tx, ty, rx, ry;

    arc_arrow_endpoints(a, (Arrow)a->first_arrow, true, &tx, &ty, &rx, &ry);
    av[0] = toNum(tx); av[1] = toNum(ty);
    av[2] = toNum(rx); av[3] = toNum(ry);

    if ( qadSendv(a->first_arrow, NAME_points, 4, av) )
    { assign(a->first_arrow, displayed, ON);
      ComputeGraphical(a->first_arrow);
      RedrawArea(a->first_arrow, area);
    }
  }
  if ( notNil(a->second_arrow) )
  { Any av[4];
    double tx, ty, rx, ry;

    arc_arrow_endpoints(a, (Arrow)a->second_arrow, false, &tx, &ty, &rx, &ry);
    av[0] = toNum(tx); av[1] = toNum(ty);
    av[2] = toNum(rx); av[3] = toNum(ry);

    if ( qadSendv(a->second_arrow, NAME_points, 4, av) )
    { assign(a->second_arrow, displayed, ON);
      ComputeGraphical(a->second_arrow);
      RedrawArea(a->second_arrow, area);
    }
  }

  return RedrawAreaGraphical(a, area);
}


static status
angleInArc(ArcObj a, double angle)
{ double start = valNum(a->start_angle);
  double size  = valNum(a->size_angle);

  if ( size < 0 )
  { start += size;
    size = -size;
  }
  start = fmod(start, 360.0);
  if ( start < 0 )
    start += 360.0;

  if ( (angle >= start && angle <= start + size) ||
       (angle <  start && angle <= start + size - 360.0) )
    succeed;

  fail;
}


static void
includeArrowsInAreaArc(ArcObj a)
{ Any av[4];
  double tx, ty, rx, ry;

  if ( notNil(a->first_arrow) )
  { arc_arrow_endpoints(a, (Arrow)a->first_arrow, true, &tx, &ty, &rx, &ry);
    av[0] = toNum(tx); av[1] = toNum(ty);
    av[2] = toNum(rx); av[3] = toNum(ry);

    if ( qadSendv(a->first_arrow, NAME_points, 4, av) )
    { ComputeGraphical(a->first_arrow);
      unionNormalisedArea(a->area, a->first_arrow->area);
    }
  }
  if ( notNil(a->second_arrow) )
  { arc_arrow_endpoints(a, (Arrow)a->second_arrow, false, &tx, &ty, &rx, &ry);
    av[0] = toNum(tx); av[1] = toNum(ty);
    av[2] = toNum(rx); av[3] = toNum(ry);

    if ( qadSendv(a->second_arrow, NAME_points, 4, av) )
    { ComputeGraphical(a->second_arrow);
      unionNormalisedArea(a->area, a->second_arrow->area);
    }
  }
}


static status
computeArc(ArcObj a)
{ if ( notNil(a->request_compute) )
  { double minx, miny, maxx, maxy;
    double sx, sy, ex, ey;
    int px = valInt(a->position->x);
    int py = valInt(a->position->y);
    int sw = valInt(a->size->w);
    int sh = valInt(a->size->h);
    int iminx, iminy, imaxx, imaxy;

    points_arc(a, &sx, &sy, &ex, &ey);
    minx = min(sx, ex);
    maxx = max(sx, ex);
    miny = min(sy, ey);
    maxy = max(sy, ey);

    if ( angleInArc(a, 0.0) )
      maxx = max(maxx, (double)(px + sw));
    if ( angleInArc(a, 90.0) )
      miny = min(miny, (double)(py - sh));
    if ( angleInArc(a, 180.0) )
      minx = min(minx, (double)(px - sw));
    if ( angleInArc(a, 270.0) )
      maxy = max(maxy, (double)(py + sh));

    if ( a->close == NAME_pieSlice ||
	 (a->close == NAME_none && notNil(a->fill)) )
    { maxx = max(maxx, (double)px);
      minx = min(minx, (double)px);
      miny = min(miny, (double)py);
      maxy = max(maxy, (double)py);
    }

    /* Snap to enclosing integer area with a 1-pixel margin. */
    iminx = (int)floor(minx) - 1;
    iminy = (int)floor(miny) - 1;
    imaxx = (int)ceil(maxx)  + 1;
    imaxy = (int)ceil(maxy)  + 1;

    if ( a->selected == ON )		/* account for selection blobs */
    { iminx -= 3;
      iminy -= 3;
      imaxx += 3;
      imaxy += 3;
    }

    CHANGING_GRAPHICAL(a,
		       { setArea(a->area, toInt(iminx), toInt(iminy),
					  toInt(imaxx-iminx),
					  toInt(imaxy-iminy));
			 includeArrowsInAreaArc(a);
		         changedEntireImageGraphical(a);
		       });

    assign(a, request_compute, NIL);
  }

  succeed;
}


static status
geometryArc(ArcObj a, Int x, Int y, Int w, Int h)
{ Int ox, oy;

  ox = isDefault(x) ? ZERO : sub(x, a->area->x);
  oy = isDefault(y) ? ZERO : sub(y, a->area->y);

  CHANGING_GRAPHICAL(a,
		     offsetPoint(a->position, ox, oy);
		     requestComputeGraphical(a, DEFAULT));

  succeed;
}


static status
setArc(ArcObj a, Int x, Int y, Int radius, double start, double size)
{ int changed = 0;

  if ( a->position->x != x || a->position->y != y )
  { setPoint(a->position, x, y);
    changed++;
  }
  if ( a->size->w != radius || a->size->h != radius )
  { setSize(a->size, radius, radius);
    changed++;
  }

  if ( valNum(a->start_angle) != start || valNum(a->size_angle) != size )
  { assign(a, start_angle, toNum(start));
    assign(a, size_angle,  toNum(size));
    changed++;
  }

  if ( changed )
    requestComputeGraphical(a, DEFAULT);

  succeed;
}


static status
radiusArc(ArcObj a, Int r)
{ if ( a->size->w != r || a->size->h != r )
  { setSize(a->size, r, r);
    requestComputeGraphical(a, DEFAULT);
  }

  succeed;
}


static status
positionArc(ArcObj a, Point pos)
{ if ( !equalPoint(a->position, pos) )
  { copyPoint(a->position, pos);
    requestComputeGraphical(a, DEFAULT);
  }

  succeed;
}


static status
resizeArc(ArcObj a, Real xfactor, Real yfactor, Point origin)
{ float xf, yf;
  int ox = valInt(a->position->x);
  int oy = valInt(a->position->y);
  int nx, ny, nw, nh;

  init_resize_graphical(a, xfactor, yfactor, origin, &xf, &yf, &ox, &oy);
  if ( xf == 1.0 && yf == 1.0 )
    succeed;

  nx = ox + rfloat((float) (valInt(a->position->x)-ox) * xf);
  ny = oy + rfloat((float) (valInt(a->position->y)-oy) * yf);
  nw = rfloat((float) valInt(a->size->w) * xf);
  nh = rfloat((float) valInt(a->size->h) * yf);

  setSize(a->size, toInt(nw), toInt(nh));
  setPoint(a->position, toInt(nx), toInt(ny));

  return requestComputeGraphical(a, DEFAULT);
}


static Int
getRadiusArc(ArcObj a)
{ answer(a->size->w);
}


static Point
getStartArc(ArcObj a)
{ double sx, sy;

  points_arc(a, &sx, &sy, NULL, NULL);
  answer(answerObject(ClassPoint, toNum(sx), toNum(sy), EAV));
}


static Point
getEndArc(ArcObj a)
{ double ex, ey;

  points_arc(a, NULL, NULL, &ex, &ey);
  answer(answerObject(ClassPoint, toNum(ex), toNum(ey), EAV));
}


static status
sizeArc(ArcObj a, Size sz)
{ if ( !equalSize(a->size, sz) )
  { copySize(a->size, sz);
    requestComputeGraphical(a, DEFAULT);
  }

  succeed;
}


static status
startAngleArc(ArcObj a, Int s)
{ if ( valNum(a->start_angle) != valNum(s) )
  { assign(a, start_angle, s);
    requestComputeGraphical(a, DEFAULT);
  }

  succeed;
}


static status
sizeAngleArc(ArcObj a, Int e)
{ if ( valNum(a->size_angle) != valNum(e) )
  { assign(a, size_angle, e);
    requestComputeGraphical(a, DEFAULT);
  }

  succeed;
}


static status
endAngleArc(ArcObj a, Int e)
{ double size = valNum(e) - valNum(a->start_angle);
  if ( size < 0.0 )
    size += 360.0;

  if ( valNum(a->size_angle) != size )
  { assign(a, size_angle, toNum(size));
    requestComputeGraphical(a, DEFAULT);
  }

  succeed;
}


static status
closeArc(ArcObj a, Name how)
{ if ( a->close != how )
  { assign(a, close, how);
    requestComputeGraphical(a, DEFAULT);
  }

  succeed;
}



		/********************************
		*        SPECIFICATIONS		*
		********************************/

static status
connectAngleArc(ArcObj a, Line l1, Line l2)
{ Point is;

  if ( !(is = getIntersectionLine(l1, l2)) )
    fail;				/* no intersection */

  positionArc(a, is);
  startAngleArc(a, toNum(valReal(getAngleLine(l1, is))));
  endAngleArc(a,   toNum(valReal(getAngleLine(l2, is))));
  doneObject(is);

  succeed;
}


static status
pointsArc(ArcObj a, Int Sx, Int Sy, Int Ex, Int Ey, Int D)
{ int sx, sy, ex, ey, d;
  int cx, cy;
  int dx, dy;
  int l, dl;
  int radius;
  float start, end, size;

  sx = valInt(Sx), sy = valInt(Sy), ex = valInt(Ex), ey = valInt(Ey);
  d = valInt(D);
  DEBUG(NAME_arc, Cprintf("ArcObj %d,%d --> %d,%d (%d)\n", sx, sy, ex, ey, d));

  cx = (sx + ex + 1)/2;			/* center of segment line */
  cy = (sy + ey + 1)/2;

  dx = ex - sx;
  dy = ey - sy;
  l = isqrt(dx*dx + dy*dy);
  dl = (l*l)/(8*d) - d/2;
  dx = (dx * dl)/l;
  dy = (dy * dl)/l;

  cx -= dy;				/* center of circle */
  cy += dx;
  radius = isqrt((cx-sx)*(cx-sx) + (cy-sy)*(cy-sy));
  DEBUG(NAME_arc, Cprintf("\tcircle from %d,%d, radius %d\n", cx, cy, radius));

  if ( ex != cx || ey != cy )
  { start = atan2((float)(cy-ey), (float)(ex-cx));
    if ( start < 0.0 )
      start = 2.0 * M_PI + start;
    start = (start * 180.0) / M_PI;

    end = atan2((float)(cy-sy), (float)(sx-cx));
    if ( end < 0.0 )
      end = 2.0 * M_PI + end;
    end = (end * 180.0) / M_PI;
  } else
  { start = end = 0.0;
  }
  DEBUG(NAME_arc, Cprintf("\t%d --> %d degrees\n",
			  (int) ((start * 360.0)/(2.0 * M_PI)),
			  (int) ((end * 360.0)/(2.0 * M_PI))));

  if ( d < 0 )
  { float x = end;
    end = start;
    start = x;
  }

  size = end - start;
  if ( size < 0.0 )
    size += 360.0;

  if ( d > 0 )
  { start += size;
    size = -size;
  }

  return setArc(a, toInt(cx), toInt(cy), toInt(radius), start, size);
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declaractions */

static char *T_connectAngle[] =
        { "line", "line" };
static char *T_initialise[] =
        { "radius=[int]", "start=[num]", "size=[num]" };
static char *T_resize[] =
        { "real", "[real]", "[point]" };
static char *T_points[] =
        { "start_x=int", "start_y=int", "end_x=int", "end_y=int", "curvature=int" };
static char *T_geometry[] =
        { "x=[int]", "y=[int]", "width=[int]", "height=[int]" };

/* Instance Variables */

static vardecl var_arc[] =
{ SV(NAME_position, "point", IV_GET|IV_STORE, positionArc,
     NAME_area, "Position of the arc"),
  SV(NAME_size, "size", IV_GET|IV_STORE, sizeArc,
     NAME_area, "Size of the ellipse I'm part of"),
  SV(NAME_startAngle, "num", IV_GET|IV_STORE, startAngleArc,
     NAME_pie, "Start angle (degrees)"),
  SV(NAME_sizeAngle, "num", IV_GET|IV_STORE, sizeAngleArc,
     NAME_pie, "Size (degrees)"),
  SV(NAME_close, "{none,pie_slice,chord}", IV_GET|IV_STORE, closeArc,
     NAME_appearance, "How the arc is closed"),
  SV(NAME_fill, TYPE_FILL, IV_GET|IV_STORE, fillGraphical,
     NAME_appearance, "Fill pattern for the slice")
};

/* Send Methods */

static senddecl send_arc[] =
{ SM(NAME_compute, 0, NULL, computeArc,
     DEFAULT, "Compute the bounding box area"),
  SM(NAME_geometry, 4, T_geometry, geometryArc,
     DEFAULT, "Only move the arc"),
  SM(NAME_initialise, 3, T_initialise, initialiseArc,
     DEFAULT, "Create ArcObj from radius, start_angle and size_angle (degrees)"),
  SM(NAME_resize, 3, T_resize, resizeArc,
     DEFAULT, "Resize arc with specified factor"),
  SM(NAME_connectAngle, 2, T_connectAngle, connectAngleArc,
     NAME_area, "Connect both lines with an angle"),
  SM(NAME_radius, 1, "int", radiusArc,
     NAME_dimension, "->width and ->height"),
  SM(NAME_endAngle, 1, "num", endAngleArc,
     NAME_pie, "Set ->size_angle to argument - <-start_angle"),
  SM(NAME_points, 5, T_points, pointsArc,
     NAME_tip, "ArcObj between two points")
};

/* Get Methods */

static getdecl get_arc[] =
{ GM(NAME_radius, 0, "int", NULL, getRadiusArc,
     NAME_area, "Equivalent to <-width"),
  GM(NAME_end, 0, "point", NULL, getEndArc,
     NAME_tip, "End position of arc"),
  GM(NAME_start, 0, "point", NULL, getStartArc,
     NAME_tip, "Start position of arc")
};

/* Resources */

static classvardecl rc_arc[] =
{ RC(NAME_radius, "int", "30",
     "Default radius")
};

/* Class Declaration */

static Name arc_termnames[] = { NAME_radius, NAME_startAngle, NAME_sizeAngle };

ClassDecl(arc_decls,
          var_arc, send_arc, get_arc, rc_arc,
          3, arc_termnames);


status
makeClassArc(Class class)
{ declareClass(class, &arc_decls);

  cloneStyleVariableClass(class, NAME_fill, NAME_reference);
  setRedrawFunctionClass(class, RedrawAreaArc);

  succeed;
}
