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

/* A `transform' represents a 2D affine transformation:
 *
 *     [ x' ]   [ xx  xy ] [ x ]   [ tx ]
 *     [    ] = [        ] [   ] + [    ]
 *     [ y' ]   [ yx  yy ] [ y ]   [ ty ]
 *
 * Composition follows cairo's convention: when a method like ->scale
 * or ->rotate is applied, the new transform is `self * Op', so that
 * `Op' is applied to a point first and `self' afterwards.  Reading a
 * sequence
 *
 *   send(T, scale, 2), send(T, rotate, 30)
 *
 * therefore means: rotate the input, then scale the rotated result.
 */

#include <h/kernel.h>
#include <h/graphics.h>
#include <math.h>
#include <float.h>

#define TXX(t) valNum((t)->xx)
#define TXY(t) valNum((t)->xy)
#define TYX(t) valNum((t)->yx)
#define TYY(t) valNum((t)->yy)
#define TTX(t) valNum((t)->tx)
#define TTY(t) valNum((t)->ty)

static void
set_transform(Transform t,
	      double xx, double xy, double yx, double yy,
	      double tx, double ty)
{ assign(t, xx, toNum(xx));
  assign(t, xy, toNum(xy));
  assign(t, yx, toNum(yx));
  assign(t, yy, toNum(yy));
  assign(t, tx, toNum(tx));
  assign(t, ty, toNum(ty));
}


static double
determinantTransform(Transform t)
{ return TXX(t)*TYY(t) - TXY(t)*TYX(t);
}


/* Snap a double to the nearest integer when within float-roundoff distance.
 * Used when converting transformed coordinates to integer area corners so
 * that exact rotations (90/180/270) of integer-aligned areas don't grow by
 * one pixel from cos/sin dust.
 */
static double
snap_to_int(double v)
{ double r = round(v);
  if ( fabs(v - r) < 1e-9 * fmax(1.0, fabs(v)) )
    return r;
  return v;
}


		 /*******************************
		 *	CONSTRUCT/IDENTITY	*
		 *******************************/

static status
initialiseTransform(Transform t,
		    Num xx, Num xy, Num yx, Num yy, Num tx, Num ty)
{ set_transform(t,
		isDefault(xx) ? 1.0 : valNum(xx),
		isDefault(xy) ? 0.0 : valNum(xy),
		isDefault(yx) ? 0.0 : valNum(yx),
		isDefault(yy) ? 1.0 : valNum(yy),
		isDefault(tx) ? 0.0 : valNum(tx),
		isDefault(ty) ? 0.0 : valNum(ty));

  succeed;
}


static status
identityTransform(Transform t)
{ set_transform(t, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0);

  succeed;
}


static status
copyTransform(Transform t, Transform from)
{ set_transform(t,
		TXX(from), TXY(from), TYX(from), TYY(from),
		TTX(from), TTY(from));

  succeed;
}


static status
setTransform(Transform t,
	     Num xx, Num xy, Num yx, Num yy, Num tx, Num ty)
{ set_transform(t,
		valNum(xx), valNum(xy), valNum(yx), valNum(yy),
		valNum(tx), valNum(ty));

  succeed;
}


		 /*******************************
		 *	    OPERATIONS		*
		 *******************************/

static status
translateTransform(Transform t, Num dx, Num dy)
{ double dxv = valNum(dx);
  double dyv = valNum(dy);
  /* self := self * Translate(dx,dy): linear part unchanged */
  assign(t, tx, toNum(TXX(t)*dxv + TXY(t)*dyv + TTX(t)));
  assign(t, ty, toNum(TYX(t)*dxv + TYY(t)*dyv + TTY(t)));

  succeed;
}


static status
scaleTransform(Transform t, Num sx, Num sy)
{ double sxv = valNum(sx);
  double syv = isDefault(sy) ? sxv : valNum(sy);
  /* self := self * Scale(sx,sy): translation unchanged */
  assign(t, xx, toNum(TXX(t)*sxv));
  assign(t, xy, toNum(TXY(t)*syv));
  assign(t, yx, toNum(TYX(t)*sxv));
  assign(t, yy, toNum(TYY(t)*syv));

  succeed;
}


static status
rotateTransform(Transform t, Num degrees)
{ double rad = valNum(degrees) * M_PI / 180.0;
  double c = cos(rad), s = sin(rad);
  double xx = TXX(t), xy = TXY(t), yx = TYX(t), yy = TYY(t);
  /* self := self * Rotate(rad), R = [c -s; s c] */
  assign(t, xx, toNum( xx*c + xy*s));
  assign(t, xy, toNum(-xx*s + xy*c));
  assign(t, yx, toNum( yx*c + yy*s));
  assign(t, yy, toNum(-yx*s + yy*c));

  succeed;
}


static status
shearTransform(Transform t, Num kx, Num ky)
{ double kxv = valNum(kx);
  double kyv = valNum(ky);
  double xx = TXX(t), xy = TXY(t), yx = TYX(t), yy = TYY(t);
  /* self := self * Shear, Shear = [1 kx; ky 1] */
  assign(t, xx, toNum(xx       + xy*kyv));
  assign(t, xy, toNum(xx*kxv   + xy));
  assign(t, yx, toNum(yx       + yy*kyv));
  assign(t, yy, toNum(yx*kxv   + yy));

  succeed;
}


static status
composeTransform(Transform t, Transform o)
{ double axx = TXX(t), axy = TXY(t),
         ayx = TYX(t), ayy = TYY(t),
         atx = TTX(t), aty = TTY(t);
  double bxx = TXX(o), bxy = TXY(o),
         byx = TYX(o), byy = TYY(o),
         btx = TTX(o), bty = TTY(o);

  set_transform(t,
		axx*bxx + axy*byx,
		axx*bxy + axy*byy,
		ayx*bxx + ayy*byx,
		ayx*bxy + ayy*byy,
		axx*btx + axy*bty + atx,
		ayx*btx + ayy*bty + aty);

  succeed;
}


static status
invertTransform(Transform t)
{ double det = determinantTransform(t);

  if ( fabs(det) < 1e-300 )
    fail;

  double xx = TXX(t), xy = TXY(t),
         yx = TYX(t), yy = TYY(t),
         tx = TTX(t), ty = TTY(t);

  set_transform(t,
		 yy/det,
		-xy/det,
		-yx/det,
		 xx/det,
		(xy*ty - yy*tx)/det,
		(yx*tx - xx*ty)/det);

  succeed;
}


		 /*******************************
		 *	     GETTERS		*
		 *******************************/

static Num
getDeterminantTransform(Transform t)
{ answer(toNum(determinantTransform(t)));
}


static Transform
getCopyTransform(Transform t)
{ answer(answerObject(classOfObject(t),
		      t->xx, t->xy, t->yx, t->yy, t->tx, t->ty, EAV));
}


static Transform
getInverseTransform(Transform t)
{ Transform copy = getCopyTransform(t);

  if ( !invertTransform(copy) )
    fail;

  answer(copy);
}


static Any
getApplyTransform(Transform t, Any obj)
{ if ( instanceOfObject(obj, ClassPoint) )
  { Point p = obj;
    double px = (double)valInt(p->x);
    double py = (double)valInt(p->y);
    double nx = snap_to_int(TXX(t)*px + TXY(t)*py + TTX(t));
    double ny = snap_to_int(TYX(t)*px + TYY(t)*py + TTY(t));
    /* Slot is still Int; round.  Will become exact when Point migrates to Num. */
    answer(answerObject(ClassPoint,
			toInt((intptr_t)floor(nx + 0.5)),
			toInt((intptr_t)floor(ny + 0.5)), EAV));
  } else if ( instanceOfObject(obj, ClassArea) )
  { Area a = obj;
    double bbox[4];

    transformAreaAABB(t, a, bbox);
    intptr_t ix = (intptr_t)floor(snap_to_int(bbox[0]));
    intptr_t iy = (intptr_t)floor(snap_to_int(bbox[1]));
    intptr_t iw = (intptr_t)ceil (snap_to_int(bbox[2])) - ix;
    intptr_t ih = (intptr_t)ceil (snap_to_int(bbox[3])) - iy;
    answer(answerObject(ClassArea,
			toInt(ix), toInt(iy), toInt(iw), toInt(ih), EAV));
  }

  fail;
}


		 /*******************************
		 *	  C-SIDE HELPERS	*
		 *******************************/

bool
transformIsIdentity(Transform t)
{ return TXX(t) == 1.0 && TXY(t) == 0.0 &&
         TYX(t) == 0.0 && TYY(t) == 1.0 &&
         TTX(t) == 0.0 && TTY(t) == 0.0;
}


void
transformPoint(Transform t, double x, double y, double *ox, double *oy)
{ *ox = TXX(t)*x + TXY(t)*y + TTX(t);
  *oy = TYX(t)*x + TYY(t)*y + TTY(t);
}


bool
inverseTransformPoint(Transform t, double x, double y,
		      double *ox, double *oy)
{ double det = determinantTransform(t);

  if ( fabs(det) < 1e-300 )
    return false;

  double rx = x - TTX(t);
  double ry = y - TTY(t);
  *ox = ( TYY(t)*rx - TXY(t)*ry) / det;
  *oy = (-TYX(t)*rx + TXX(t)*ry) / det;

  return true;
}


void
transformAreaAABB(Transform t, Area in, double bbox[4])
{ double x0 = (double)valInt(in->x);
  double y0 = (double)valInt(in->y);
  double x1 = x0 + (double)valInt(in->w);
  double y1 = y0 + (double)valInt(in->h);
  double cx[4] = { x0, x1, x1, x0 };
  double cy[4] = { y0, y0, y1, y1 };

  bbox[0] = bbox[1] =  DBL_MAX;
  bbox[2] = bbox[3] = -DBL_MAX;
  for(int i=0; i<4; i++)
  { double nx, ny;
    transformPoint(t, cx[i], cy[i], &nx, &ny);
    if ( nx < bbox[0] ) bbox[0] = nx;
    if ( ny < bbox[1] ) bbox[1] = ny;
    if ( nx > bbox[2] ) bbox[2] = nx;
    if ( ny > bbox[3] ) bbox[3] = ny;
  }
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Argument-type vectors */

static char *T_initialise[] =
{ "xx=[num]", "xy=[num]", "yx=[num]", "yy=[num]",
  "tx=[num]", "ty=[num]"
};
static char *T_set[] =
{ "xx=num", "xy=num", "yx=num", "yy=num",
  "tx=num", "ty=num"
};
static char *T_dxdy[]	= { "dx=num", "dy=num" };
static char *T_sxsy[]	= { "sx=num", "sy=[num]" };
static char *T_kxky[]	= { "kx=num", "ky=num" };

/* Instance variables */

static vardecl var_transform[] =
{ IV(NAME_xx, "num", IV_BOTH,
     NAME_dimension, "X-scale (top-left of 2x2 linear part)"),
  IV(NAME_xy, "num", IV_BOTH,
     NAME_dimension, "Y-to-X shear (top-right of 2x2)"),
  IV(NAME_yx, "num", IV_BOTH,
     NAME_dimension, "X-to-Y shear (bottom-left of 2x2)"),
  IV(NAME_yy, "num", IV_BOTH,
     NAME_dimension, "Y-scale (bottom-right of 2x2)"),
  IV(NAME_tx, "num", IV_BOTH,
     NAME_dimension, "X translation"),
  IV(NAME_ty, "num", IV_BOTH,
     NAME_dimension, "Y translation")
};

/* Send methods */

static senddecl send_transform[] =
{ SM(NAME_initialise, 6, T_initialise, initialiseTransform,
     DEFAULT, "Create transform; defaults to identity"),
  SM(NAME_set, 6, T_set, setTransform,
     NAME_dimension, "Set all six coefficients"),
  SM(NAME_copy, 1, "from=transform", copyTransform,
     NAME_copy, "Copy coefficients from argument transform"),
  SM(NAME_identity, 0, NULL, identityTransform,
     NAME_calculate, "Reset to the identity transform"),
  SM(NAME_translate, 2, T_dxdy, translateTransform,
     NAME_calculate, "Pre-translate: self := self * translate(dx,dy)"),
  SM(NAME_scale, 2, T_sxsy, scaleTransform,
     NAME_calculate, "Pre-scale: self := self * scale(sx,sy); uniform if sy omitted"),
  SM(NAME_rotate, 1, "degrees=num", rotateTransform,
     NAME_calculate, "Pre-rotate by given angle in degrees"),
  SM(NAME_shear, 2, T_kxky, shearTransform,
     NAME_calculate, "Pre-shear with factors kx (along x) and ky (along y)"),
  SM(NAME_compose, 1, "transform", composeTransform,
     NAME_calculate, "self := self * argument"),
  SM(NAME_invert, 0, NULL, invertTransform,
     NAME_calculate, "Invert in place; fails if singular")
};

/* Get methods */

static getdecl get_transform[] =
{ GM(NAME_copy, 0, "transform", NULL, getCopyTransform,
     NAME_copy, "Independent copy of this transform"),
  GM(NAME_inverse, 0, "transform", NULL, getInverseTransform,
     NAME_calculate, "New transform that is the inverse; fails if singular"),
  GM(NAME_determinant, 0, "num", NULL, getDeterminantTransform,
     NAME_calculate, "Determinant of the 2x2 linear part"),
  GM(NAME_apply, 1, "point|area", "point|area", getApplyTransform,
     NAME_calculate, "Map a point or the AABB of an area through this transform")
};

#define rc_transform NULL

static Name transform_termnames[] =
{ NAME_xx, NAME_xy, NAME_yx, NAME_yy, NAME_tx, NAME_ty };

ClassDecl(transform_decls,
	  var_transform, send_transform, get_transform, rc_transform,
	  6, transform_termnames);

status
makeClassTransform(Class class)
{ return declareClass(class, &transform_decls);
}
