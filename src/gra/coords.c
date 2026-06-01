/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org/projects/xpce/
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

/* Coordinate mapping that is aware of figure->transform.
 *
 * The fundamental walk goes UP the device chain from a graphical or
 * device toward its containing window, composing at each step:
 *
 *   1. (optional) the device's figure->transform on the input point
 *   2. the device's `offset' as a translation
 *
 * For a graphical, an initial translation by gr->area->{x,y} maps from
 * gr's local coord (relative to its own area origin) into gr->device's
 * children-coord system, before that loop runs.
 *
 * All math is performed in double, then rounded outward (floor/ceil
 * with a near-integer snap) when an integer AABB is requested.  See
 * gra/transform.c for the snap helper.
 */

#include <h/kernel.h>
#include <h/graphics.h>
#include <math.h>
#include <float.h>

typedef struct
{ double xx, xy;	/* 2x2 linear part */
  double yx, yy;
  double tx, ty;	/* translation */
} aff;


static void
aff_identity(aff *a)
{ a->xx = 1.0; a->xy = 0.0;
  a->yx = 0.0; a->yy = 1.0;
  a->tx = 0.0; a->ty = 0.0;
}


/* W := T(dx, dy) * W
 *
 * Pre-multiplying by a pure translation only shifts the translation
 * column.
 */

static void
aff_pre_translate(aff *W, double dx, double dy)
{ W->tx += dx;
  W->ty += dy;
}


/* W := W * T(dx, dy)
 *
 * Post-multiplying by a translation moves the translation column
 * through W's linear part.
 */

static void
aff_post_translate(aff *W, double dx, double dy)
{ W->tx += W->xx*dx + W->xy*dy;
  W->ty += W->yx*dx + W->yy*dy;
}


/* W := M * W, where M comes from a Transform xpce object.
 *
 * Pre-multiplying by an affine M:
 *
 *   new.linear      = M.linear * W.linear
 *   new.translation = M.linear * W.translation + M.translation
 */

static void
aff_pre_compose_transform(aff *W, Transform t)
{ double mxx = valNum(t->xx), mxy = valNum(t->xy);
  double myx = valNum(t->yx), myy = valNum(t->yy);
  double mtx = valNum(t->tx), mty = valNum(t->ty);

  double xx = mxx*W->xx + mxy*W->yx;
  double xy = mxx*W->xy + mxy*W->yy;
  double yx = myx*W->xx + myy*W->yx;
  double yy = myx*W->xy + myy*W->yy;
  double tx = mxx*W->tx + mxy*W->ty + mtx;
  double ty = myx*W->tx + myy*W->ty + mty;

  W->xx = xx; W->xy = xy;
  W->yx = yx; W->yy = yy;
  W->tx = tx; W->ty = ty;
}


static void
aff_apply(const aff *W, double x, double y, double *ox, double *oy)
{ *ox = W->xx*x + W->xy*y + W->tx;
  *oy = W->yx*x + W->yy*y + W->ty;
}


static bool
aff_invert(const aff *W, aff *inv)
{ double det = W->xx*W->yy - W->xy*W->yx;

  if ( fabs(det) < 1e-300 )
    return false;

  inv->xx =  W->yy / det;
  inv->xy = -W->xy / det;
  inv->yx = -W->yx / det;
  inv->yy =  W->xx / det;
  inv->tx = (W->xy*W->ty - W->yy*W->tx) / det;
  inv->ty = (W->yx*W->tx - W->xx*W->ty) / det;
  return true;
}


/* Compose the affine that maps a point in `dev`'s children-coord system
 * (pre-transform of dev) to the window's children-coord system.  Walks
 * dev, dev->device, ... up to (and including) the enclosing window.
 *
 * Returns false if `dev` is not displayed in any window.
 */

static bool
compose_device_to_window(Device dev, aff *out)
{ aff W;
  aff_identity(&W);

  for( ; notNil(dev); dev = dev->device )
  { /* The window terminates the walk and its own dev->offset is not
     * part of the painting / event coordinate chain (scroll_offset is
     * handled separately by callers).
     */
    if ( instanceOfObject(dev, ClassWindow) )
    { *out = W;
      return true;
    }
    if ( instanceOfObject(dev, ClassFigure) )
    { Figure f = (Figure) dev;
      if ( notNil(f->transform) && !transformIsIdentity(f->transform) )
	aff_pre_compose_transform(&W, f->transform);
    }
    aff_pre_translate(&W,
		      (double)valInt(dev->offset->x),
		      (double)valInt(dev->offset->y));
  }

  return false;
}


/* Compose the affine for a graphical: the gr->area translation maps
 * gr-local to gr->device's children-coord (the rightmost factor in the
 * composition), then the device-chain walk takes us up to window.
 */

static bool
compose_graphical_to_window(Graphical gr, aff *out)
{ if ( !compose_device_to_window(gr->device, out) )
    return false;
  aff_post_translate(out,
		     (double)valInt(gr->area->x),
		     (double)valInt(gr->area->y));
  return true;
}


		 /*******************************
		 *	  PUBLIC API		*
		 *******************************/

/* Map a point in `dev`'s children-coord system to the window-coord
 * system.  Returns false if dev has no window ancestor.
 */
bool
deviceLocalToWindowCoord(Device dev, double lx, double ly,
			 double *wx, double *wy)
{ aff W;

  if ( !compose_device_to_window(dev, &W) )
    return false;
  aff_apply(&W, lx, ly, wx, wy);
  return true;
}


/* Inverse: map a point in window-coord into `dev`'s children-coord
 * system.  Fails if dev has no window ancestor or the composed
 * transform is singular.
 */
bool
windowToDeviceLocalCoord(Device dev, double wx, double wy,
			 double *lx, double *ly)
{ aff W, inv;

  if ( !compose_device_to_window(dev, &W) )
    return false;
  if ( !aff_invert(&W, &inv) )
    return false;
  aff_apply(&inv, wx, wy, lx, ly);
  return true;
}


/* Map a point in `gr`'s local coord (relative to gr->area origin) into
 * `*target_io`'s children-coord system, walking through ancestor
 * figure->transforms.
 *
 * `*target_io` selects the stopping point:
 *
 *   - DEFAULT or NIL: walk until reaching a window (or the root).
 *     `*target_io` is updated to the stopping device.
 *   - a specific device: walk until reaching that device.  Returns
 *     false if it isn't an ancestor of gr.
 *
 * The integer fast-path is not used here — callers that need it should
 * check hasTransformInDeviceChain / deviceChainHasTransform first.
 */
bool
graphicalToDeviceCoord(Graphical gr, Device *target_io,
		       double lx, double ly,
		       double *ox, double *oy)
{ aff W;
  Device target = NULL;

  if ( target_io && notDefault(*target_io) && notNil(*target_io) )
    target = *target_io;

  aff_identity(&W);
  aff_post_translate(&W,
		     (double)valInt(gr->area->x),
		     (double)valInt(gr->area->y));

  Graphical cur = gr;
  while ( notNil(cur->device) &&
	  !instanceOfObject(cur->device, ClassWindow) &&
	  (target == NULL || cur->device != target) )
  { Device d = cur->device;
    if ( instanceOfObject(d, ClassFigure) )
    { Figure f = (Figure) d;
      if ( notNil(f->transform) && !transformIsIdentity(f->transform) )
	aff_pre_compose_transform(&W, f->transform);
    }
    aff_pre_translate(&W,
		      (double)valInt(d->offset->x),
		      (double)valInt(d->offset->y));
    cur = (Graphical) d;
  }

  if ( target && cur->device != target )
    return false;

  if ( target_io )
    *target_io = cur->device;

  aff_apply(&W, lx, ly, ox, oy);
  return true;
}


/* Map an integer area in `gr`'s local coord into `*target_io`'s
 * children-coord, taking the outward-rounded AABB.  Same target
 * semantics as graphicalToDeviceCoord.
 */
bool
graphicalToDeviceAreaAABB(Graphical gr, Device *target_io,
			  int lx, int ly, int lw, int lh,
			  int *ox, int *oy, int *ow, int *oh)
{ double x0 = (double)lx, y0 = (double)ly;
  double x1 = x0 + (double)lw, y1 = y0 + (double)lh;
  double cx[4] = { x0, x1, x1, x0 };
  double cy[4] = { y0, y0, y1, y1 };
  double minx =  DBL_MAX, miny =  DBL_MAX;
  double maxx = -DBL_MAX, maxy = -DBL_MAX;
  Device probe = (target_io ? *target_io : NIL);

  for(int i=0; i<4; i++)
  { Device t = probe;
    double nx, ny;
    if ( !graphicalToDeviceCoord(gr, &t, cx[i], cy[i], &nx, &ny) )
      return false;
    if ( i == 0 && target_io )
      *target_io = t;
    if ( nx < minx ) minx = nx;
    if ( ny < miny ) miny = ny;
    if ( nx > maxx ) maxx = nx;
    if ( ny > maxy ) maxy = ny;
  }
  *ox = (int)floor(transformSnapInt(minx));
  *oy = (int)floor(transformSnapInt(miny));
  *ow = (int)ceil (transformSnapInt(maxx)) - *ox;
  *oh = (int)ceil (transformSnapInt(maxy)) - *oy;
  return true;
}


/* Map a point in `gr`'s local coord (relative to gr->area origin) to
 * the window-coord system.
 */
bool
graphicalToWindowCoord(Graphical gr, double lx, double ly,
		       double *wx, double *wy)
{ aff W;

  if ( !compose_graphical_to_window(gr, &W) )
    return false;
  aff_apply(&W, lx, ly, wx, wy);
  return true;
}


/* Inverse: window-coord to gr-local.  Fails if the composed transform
 * is singular (e.g. scale by 0) or gr is not displayed.
 */
bool
windowToGraphicalCoord(Graphical gr, double wx, double wy,
		       double *lx, double *ly)
{ aff W, inv;

  if ( !compose_graphical_to_window(gr, &W) )
    return false;
  if ( !aff_invert(&W, &inv) )
    return false;
  aff_apply(&inv, wx, wy, lx, ly);
  return true;
}


/* Map an integer rect in `dev`'s children-coord to a window-coord
 * AABB, rounded outward (floor min, ceil max with float-dust snap).
 * Returns false if `dev` has no window ancestor or the composition is
 * singular.  Used by damage propagation in graphical.c.
 */
bool
deviceLocalAreaToWindowAABB(Device dev,
			    int lx, int ly, int lw, int lh,
			    int *wx, int *wy, int *ww, int *wh)
{ aff W;

  if ( !compose_device_to_window(dev, &W) )
    return false;

  double x0 = (double)lx, y0 = (double)ly;
  double x1 = x0 + (double)lw, y1 = y0 + (double)lh;
  double cx[4] = { x0, x1, x1, x0 };
  double cy[4] = { y0, y0, y1, y1 };
  double minx =  DBL_MAX, miny =  DBL_MAX;
  double maxx = -DBL_MAX, maxy = -DBL_MAX;

  for(int i=0; i<4; i++)
  { double nx, ny;
    aff_apply(&W, cx[i], cy[i], &nx, &ny);
    if ( nx < minx ) minx = nx;
    if ( ny < miny ) miny = ny;
    if ( nx > maxx ) maxx = nx;
    if ( ny > maxy ) maxy = ny;
  }

  int ix  = (int)floor(transformSnapInt(minx));
  int iy  = (int)floor(transformSnapInt(miny));
  int ix1 = (int)ceil (transformSnapInt(maxx));
  int iy1 = (int)ceil (transformSnapInt(maxy));
  *wx = ix;
  *wy = iy;
  *ww = ix1 - ix;
  *wh = iy1 - iy;
  return true;
}


/* Map an Area in `gr`-local coords to the window-coord AABB.  Writes
 * floats; caller decides how to round.
 */
bool
graphicalToWindowArea(Graphical gr, Area in, double bbox[4])
{ aff W;

  if ( !compose_graphical_to_window(gr, &W) )
    return false;

  double x0 = (double)valInt(in->x);
  double y0 = (double)valInt(in->y);
  double x1 = x0 + (double)valInt(in->w);
  double y1 = y0 + (double)valInt(in->h);
  double cx[4] = { x0, x1, x1, x0 };
  double cy[4] = { y0, y0, y1, y1 };

  bbox[0] = bbox[1] =  DBL_MAX;
  bbox[2] = bbox[3] = -DBL_MAX;
  for(int i=0; i<4; i++)
  { double nx, ny;
    aff_apply(&W, cx[i], cy[i], &nx, &ny);
    if ( nx < bbox[0] ) bbox[0] = nx;
    if ( ny < bbox[1] ) bbox[1] = ny;
    if ( nx > bbox[2] ) bbox[2] = nx;
    if ( ny > bbox[3] ) bbox[3] = ny;
  }
  return true;
}


/* Fast-path predicate: does the device chain starting at `dev`
 * (inclusive) up to the enclosing window carry a non-identity
 * figure->transform?
 */
bool
hasTransformInDeviceChain(Device dev)
{ for( ; notNil(dev); dev = dev->device )
  { if ( instanceOfObject(dev, ClassWindow) )
      break;
    if ( instanceOfObject(dev, ClassFigure) )
    { Figure f = (Figure) dev;
      if ( notNil(f->transform) && !transformIsIdentity(f->transform) )
	return true;
    }
  }
  return false;
}


/* Convenience wrapper for graphicals: checks gr->device upward, so
 * that gr's own transform (if gr is itself a figure) does NOT matter
 * — only ancestor transforms affect mapping a gr-local coordinate.
 */
bool
deviceChainHasTransform(Graphical gr)
{ return hasTransformInDeviceChain(gr->device);
}
