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

		/********************************
		*            CREATE		*
		********************************/

status
initialiseFigure(Figure f)
{ initialiseDevice((Device) f);
  assign(f, background, NIL);
  assign(f, pen,        ZERO);
  assign(f, border,	ZERO);
  assign(f, radius,	ZERO);
  assign(f, elevation,	NIL);
  assign(f, transform,	NIL);
  assign(f, local_area,	newObject(ClassArea, EAV));
  assign(f, status,     NAME_allActive);

  succeed;
}

		/********************************
		*             REDRAW		*
		********************************/

Any
RedrawBoxFigure(Figure f, Area area)
{ Any rval = NIL;

  if ( f->pen != ZERO || notNil(f->background) || notNil(f->elevation) )
  { int x, y, w, h;

    initialiseDeviceGraphical(f, &x, &y, &w, &h);
    if ( f->pen == ZERO && f->radius == ZERO && isNil(f->elevation) )
    { r_fill(x, y, w, h, f->background);
      rval = f->background;
    } else
    { r_thickness(valInt(f->pen));
      r_dash(f->texture);

      if ( notNil(f->elevation) )
      { Elevation e = f->elevation;
	if ( e->kind == NAME_shadow )
	{ r_shadow_box(x, y, w, h,
		       valInt(f->radius), valInt(e->height), f->background);
	} else
	{ r_3d_box(x, y, w, h, valInt(f->radius), e, true);
	}
	rval = f->elevation->background;
      } else
      { r_box(x, y, w, h, valInt(f->radius), f->background);
	rval = f->background;
      }
    }
  }

  return rval;
}


/* Paint the children of f with f->transform applied.  Mirrors the body
 * of RedrawAreaDevice between Enter/Exit, with cairo carrying the affine
 * via r_push_transform/r_pop_transform around the children loop.
 *
 * After EnterRedrawAreaDevice the incoming `area' is in the figure's
 * POST-transform local coord (where the rotated pixel AABB lives), but
 * the children's own `area' slots live in PRE-transform local coord.
 * RedrawArea's overlap test would then mis-clip the children — visible
 * as text disappearing at ±180° rotation or small scales.  We replace
 * `area' with the figure's local_area for the children loop, which is
 * always in pre-transform coord and encloses every child; cairo's clip
 * (and the dirty-rect filtering done by changed_window) still keeps
 * the paint area minimal.  Exit restores the original area.
 */

static void
RedrawTransformedChildren(Figure f, Area area)
{ device_draw_context ctx;
  Device dev = (Device) f;

  if ( EnterRedrawAreaDevice(dev, area, &ctx) )
  { Cell cell;
    r_transform_save saved;
    Area la = f->local_area;

    qassign(area, x, la->x);
    qassign(area, y, la->y);
    qassign(area, w, la->w);
    qassign(area, h, la->h);

    if ( notNil(dev->layout_manager) )
      qadSendv(dev->layout_manager, NAME_redrawBackground, 1, (Any*)&area);

    r_push_transform(f->transform, &saved);
    for_cell(cell, dev->graphicals)
      RedrawArea(cell->value, area);
    r_pop_transform(&saved);

    if ( notNil(dev->layout_manager) )
      qadSendv(dev->layout_manager, NAME_redrawForeground, 1, (Any*)&area);

    ExitRedrawAreaDevice(dev, area, &ctx);
  }

  RedrawAreaGraphical((Graphical) f, area);
}


static status
RedrawAreaFigure(Figure f, Area area)
{ Any bg, obg;
  bool transformed = ( notNil(f->transform) &&
		       !transformIsIdentity(f->transform) );

  if ( notNil(bg = RedrawBoxFigure(f, area)) )
    obg = r_background(bg);
  else
    obg = NULL;

  if ( transformed )
    RedrawTransformedChildren(f, area);
  else
    RedrawAreaDevice((Device) f, area);

  if ( obg )
    r_background(obg);

  succeed;
}


		 /*******************************
		 *	     OUTLINE		*
		 *******************************/

static status
computeBoundingBoxFigure(Figure f)
{ if ( f->badBoundingBox == ON )
  { Area a = f->area;
    Int ox = a->x, oy = a->y, ow = a->w, oh = a->h;
    bool transformed = ( notNil(f->transform) &&
			 !transformIsIdentity(f->transform) );

    computeBoundingBoxDevice((Device) f);

    /* f->area now holds the un-transformed children union, translated
     * by dev->offset (and clipped by clip_area in local coords).
     * Snapshot the local-coord bbox into f->local_area; later phases
     * (events, damage) need it.
     */
    int offx = valInt(f->offset->x);
    int offy = valInt(f->offset->y);
    qassign(f->local_area, x, toInt(valInt(f->area->x) - offx));
    qassign(f->local_area, y, toInt(valInt(f->area->y) - offy));
    qassign(f->local_area, w, f->area->w);
    qassign(f->local_area, h, f->area->h);

    if ( transformed )
    { /* Project local_area through the transform, then re-apply offset
       * to land back in parent coords.
       */
      transformAreaToIntAABB(f->transform, f->local_area, f->area);
      qassign(f->area, x, toInt(valInt(f->area->x) + offx));
      qassign(f->area, y, toInt(valInt(f->area->y) + offy));
    }

    if ( f->border != ZERO )
      increaseArea(f->area, f->border);
    if ( f->pen != ZERO )
      increaseArea(f->area, f->pen);

    if ( ox != a->x || oy != a->y || ow != a->w || oh != a->h )
      changedAreaGraphical((Graphical)f, ox, oy, ow, oh);
  }

  succeed;
}


static status
computeFigure(Figure f)
{ if ( notNil(f->request_compute) )
  { if ( f->pen != ZERO || notNil(f->background) )
    { CHANGING_GRAPHICAL(f, { computeGraphicalsDevice((Device) f);
			      computeLayoutDevice((Device) f);
			      computeBoundingBoxFigure(f);
			    });
    } else
    { computeGraphicalsDevice((Device) f);
      computeLayoutDevice((Device) f);
      computeBoundingBoxFigure(f);
    }

    assign(f, request_compute, NIL);
  }

  succeed;
}


		/********************************
		*           ATTRIBUTES		*
		********************************/


static status
statusFigure(Figure f, Name stat)
{ Cell cell;

  if ( stat == NAME_allActive )
  { for_cell(cell, f->graphicals)
      DisplayedGraphical(cell->value, ON);
  } else
  { for_cell(cell, f->graphicals)
    { Graphical gr = cell->value;

      DisplayedGraphical(gr, gr->name == stat ? ON : OFF);
    }
    assign(f, status, stat);
  }

  requestComputeDevice((Device) f, DEFAULT);

  succeed;
}


static status
nextStatusFigure(Figure f)
{ Cell cell;

  if ( f->status == NAME_allActive)
    fail;

  for_cell(cell, f->graphicals)
  { Graphical gr = cell->value;

    if ( gr->name == f->status )
    { Graphical gr2;

      if ( notNil(cell->next) )
        gr2 = (Graphical) cell->next->value;
      else
	gr2 = (Graphical) f->graphicals->head->value;

      return statusFigure(f, gr2->name);
    }
  }

  fail;
}


static status
backgroundFigure(Figure f, Image bg)
{ if ( f->background != bg )
  { CHANGING_GRAPHICAL(f,
		       assign(f, background, bg);
		       if ( notNil(f->elevation) )
		         assign(f, elevation, getModifyElevation(f->elevation, NAME_colour,
								 isNil(bg) ? DEFAULT : bg));
		       changedEntireImageGraphical(f));
  }

  succeed;
}


static status
clipAreaFigure(Device f, Area a)
{ assign(f, badBoundingBox, ON);
  assign(f, clip_area, a);
  requestComputeDevice(f, DEFAULT);

  succeed;
}


static Area
getClipAreaFigure(Figure f)
{ answer(f->clip_area);
}


static status
elevationFigure(Figure f, Elevation e)
{ return assignGraphical(f, NAME_elevation, e);
}


/* Attach an optional 2D affine transform to the figure's contents.
 * @nil restores the un-transformed default.
 *
 * Unlike a "pure" slot setter we always invalidate here, even when the
 * passed transform is the same object as the current one: callers
 * typically modify a single transform in place (e.g. driven by a
 * slider) and re-assign it to trigger repaint.
 */

static status
transformFigure(Figure f, Transform t)
{ CHANGING_GRAPHICAL(f,
		     assign(f, transform, t);
		     requestComputeDevice((Device) f, DEFAULT);
		     changedEntireImageGraphical(f));

  succeed;
}


/* Compose an operation into the figure's transform.  If the figure has
 * no transform yet (the @nil default), one is created lazily and set to
 * the identity before the operation is applied.  After composing the
 * figure is invalidated so that bounding box, damage and repaint pick
 * up the new transform on the next round.
 */

static Transform
get_or_make_transform(Figure f)
{ if ( isNil(f->transform) )
  { Transform t = newObject(ClassTransform, EAV);
    assign(f, transform, t);
  }
  return f->transform;
}

static void
transform_changed(Figure f)
{ requestComputeDevice((Device) f, DEFAULT);
  changedEntireImageGraphical(f);
}

static status
translateFigure(Figure f, Num dx, Num dy)
{ Transform t = get_or_make_transform(f);
  CHANGING_GRAPHICAL(f,
		     translateTransform(t, dx, dy);
		     transform_changed(f));
  succeed;
}

static status
scaleFigureMethod(Figure f, Num sx, Num sy)
{ Transform t = get_or_make_transform(f);
  CHANGING_GRAPHICAL(f,
		     scaleTransform(t, sx, sy);
		     transform_changed(f));
  succeed;
}

static status
rotateFigure(Figure f, Num degrees)
{ Transform t = get_or_make_transform(f);
  CHANGING_GRAPHICAL(f,
		     rotateTransform(t, degrees);
		     transform_changed(f));
  succeed;
}

static status
shearFigureMethod(Figure f, Num kx, Num ky)
{ Transform t = get_or_make_transform(f);
  CHANGING_GRAPHICAL(f,
		     shearTransform(t, kx, ky);
		     transform_changed(f));
  succeed;
}


static status
shadowFigure(Figure f, Int shadow)
{ return elevationFigure(f, shadow == ZERO ?
			      NIL :
			      newObject(ClassElevation, NIL,
					shadow, /* height */
					isNil(f->background) ? DEFAULT
							     : f->background,
					DEFAULT, DEFAULT, /* edge colours */
					NAME_shadow, EAV));
}


static Int
getShadowFigure(Figure f)
{ if ( notNil(f->elevation) )
    answer(f->elevation->height);

  answer(ZERO);
}


static status
radiusFigure(Figure f, Num radius)
{ return assignGraphical(f, NAME_radius, radius);
}


static status
borderFigure(Figure f, Num border)
{ if ( f->border != border )
  { assign(f, border, border);
    requestComputeDevice((Device) f, DEFAULT);
  }

  succeed;
}


static status
displayFigure(Figure f, Graphical gr, Point pos)
{ if ( notDefault(pos) )
    setGraphical(gr, pos->x, pos->y, DEFAULT, DEFAULT);

  TRY( DeviceGraphical(gr, (Device) f) );
  return DisplayedGraphical(gr,
			    (f->status == NAME_allActive ||
			     f->status == gr->name) ? ON : OFF);
}


static status
convertOldSlotFigure(Figure f, Name slot, Any value)
{ if ( slot == NAME_shadow )
    shadowFigure(f, value);

  succeed;
}


status
makeClassFigure(Class class)
{ sourceClass(class, makeClassFigure, __FILE__);

  localClass(class, NAME_status, NAME_visibility, "name", NAME_get,
	     "Name of visible graphical (or all_active)");
  localClass(class, NAME_background, NAME_appearance,
	     TYPE_FILL, NAME_get,
	     "Fill pattern used as background");
  localClass(class, NAME_border, NAME_appearance, "num", NAME_get,
	     "Border around graphicals");
  localClass(class, NAME_radius, NAME_appearance, "num", NAME_get,
	     "Radius of outline");
  localClass(class, NAME_elevation, NAME_appearance, "elevation*", NAME_get,
	     "Elevation from background");
  localClass(class, NAME_transform, NAME_appearance, "transform*", NAME_get,
	     "Optional 2D affine transform applied to contents");
  localClass(class, NAME_localArea, NAME_area, "area", NAME_get,
	     "Children bbox in figure-local coords (before transform/offset)");

  setRedrawFunctionClass(class, RedrawAreaFigure);

  storeMethod(class, NAME_status,     statusFigure);
  storeMethod(class, NAME_background, backgroundFigure);
  storeMethod(class, NAME_clipArea,   clipAreaFigure);
  storeMethod(class, NAME_border,     borderFigure);
  storeMethod(class, NAME_radius,     radiusFigure);
  storeMethod(class, NAME_elevation,  elevationFigure);
  storeMethod(class, NAME_transform,  transformFigure);

  sendMethod(class, NAME_initialise, DEFAULT, 0,
	     "Create figure",
	     initialiseFigure);
  sendMethod(class, NAME_compute, NAME_update, 0,
	     "Recompute figure (handle <-border)",
	     computeFigure);
  sendMethod(class, NAME_nextStatus, NAME_visibility, 0,
	     "Make next in <-graphicals visible",
	     nextStatusFigure);
  sendMethod(class, NAME_display, NAME_organisation, 2, "graphical", "[point]",
	     "Display graphical at point",
	     displayFigure);
  sendMethod(class, NAME_shadow, NAME_appearance, 1, "0..",
	     "Attach `shadow' elevation object",
	     shadowFigure);
  sendMethod(class, NAME_translate, NAME_calculate, 2,
	     "dx=num", "dy=num",
	     "Compose translate(dx,dy) into the figure's transform",
	     translateFigure);
  sendMethod(class, NAME_scale, NAME_calculate, 2,
	     "sx=num", "sy=[num]",
	     "Compose scale(sx,sy) into the figure's transform",
	     scaleFigureMethod);
  sendMethod(class, NAME_rotate, NAME_calculate, 1,
	     "degrees=num",
	     "Compose rotate(degrees) into the figure's transform",
	     rotateFigure);
  sendMethod(class, NAME_shear, NAME_calculate, 2,
	     "kx=num", "ky=num",
	     "Compose shear(kx,ky) into the figure's transform",
	     shearFigureMethod);
  sendMethod(class, NAME_convertOldSlot, NAME_compatibility, 2,
	     "slot=name", "value=any",
	     "Translate old shadow into elevation",
	     convertOldSlotFigure);

  getMethod(class, NAME_clipArea, NAME_scroll, "area", 0,
	    "Clip area associated with figure",
	    getClipAreaFigure);
  getMethod(class, NAME_shadow, NAME_compatibility, "0..", 0,
	    "Read `elevation <-height'",
	    getShadowFigure);

  succeed;
}

