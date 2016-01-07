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
#include <h/arith.h>

static status
initialiseSpatial(Spatial s,
		  Equation xFrom, Equation yFrom,
		  Equation xTo, Equation yTo,
		  Equation wTo, Equation hTo)
{ assign(s, xFrom, isDefault(xFrom) ? (Equation) NIL : xFrom);
  assign(s, yFrom, isDefault(yFrom) ? (Equation) NIL : yFrom);
  assign(s, xTo, isDefault(xTo) ? (Equation) NIL : xTo);
  assign(s, yTo, isDefault(yTo) ? (Equation) NIL : yTo);
  assign(s, wTo, isDefault(wTo) ? (Equation) NIL : wTo);
  assign(s, hTo, isDefault(hTo) ? (Equation) NIL : hTo);

  succeed;
}


static inline Int
_getVar(Equation e, Var var, va_list args) /* Var, Int, Var, Int, ... */
{ int argc, i;
  Var vars[FWD_PCE_MAX_ARGS];
  Any vals[FWD_PCE_MAX_ARGS];
  Any savd[FWD_PCE_MAX_ARGS];
  numeric_value v;

  for(argc = 0; (vars[argc] = va_arg(args, Var)) != NULL; argc++)
  { assert(argc <= FWD_PCE_MAX_ARGS);
    assert(instanceOfObject(vars[argc], ClassVar));

    vals[argc] = va_arg(args, Expression);
    assert(vals[argc] != NULL);
  }

  for(i=0; i<argc; i++)
  { savd[i] = vars[i]->value;
    setVar(vars[i], vals[i]);
  }

  evaluateEquation(e, var, &v);

  for(i=0; i<argc; i++)
    setVar(vars[i], savd[i]);

  return ar_int_result(e, &v);
}


static Int
getVar(Equation e, Var var, ...)
{ va_list args;
  Int rval;

  va_start(args, var);
  rval = _getVar(e, var, args);
  va_end(args);

  return rval;
}


#define CALC(var, rel, how, def) TRY(var = (notNil(rel) ? how : def))

static status
forwardsSpatial(Spatial s, Any from, Any to)
{ Int xref, yref;
  Int tX, tY, tW, tH;
  Area f, t;

  TRY(f = get(from, NAME_area, EAV));
  TRY(t = get(to, NAME_area, EAV));

  CALC(xref,s->xFrom,getVar(s->xFrom,VarXref,VarX,f->x,VarW,f->w, EAV),f->x);
  CALC(yref,s->yFrom,getVar(s->yFrom,VarYref,VarY,f->y,VarH,f->h, EAV),f->y);
  CALC(tW, s->wTo, getVar(s->wTo, VarW2, VarW, f->w, EAV), t->w);
  CALC(tH, s->hTo, getVar(s->hTo, VarH2, VarH, f->h, EAV), t->h);
  CALC(tX, s->xTo,getVar(s->xTo, VarX, VarXref, xref, VarW, tW, EAV), t->x);
  CALC(tY, s->yTo,getVar(s->yTo, VarY, VarYref, yref, VarH, tH, EAV), t->y);

  DEBUG(NAME_spatial,
	Cprintf("%s->f: (%s,%s) -- %ld,%ld,%ld,%ld ==> (%ld, %ld, %ld, %ld)\n",
		pp(s), pp(from), pp(to),
		valInt(f->x), valInt(f->y), valInt(f->w), valInt(f->h),
		valInt(tX), valInt(tY), valInt(tW), valInt(tH)));

  if (t->x != tX || t->y != tY || t->w != tW || t->h != tH)
    return send(to, NAME_set, tX, tY, tW, tH, EAV);

  succeed;
}


static status
backwardsSpatial(Spatial s, Any from, Any to)
{ Int xref, yref;
  Int fW, fH, fX, fY;

  Area f, t;

  TRY(f = get(from, NAME_area, EAV));
  TRY(t = get(to, NAME_area, EAV));

  CALC(xref, s->xTo, getVar(s->xTo,VarXref,VarX,t->x,VarW,t->w,EAV), t->x);
  CALC(yref, s->yTo, getVar(s->yTo,VarYref,VarY,t->y,VarH,t->h,EAV), t->y);
  CALC(fW, s->wTo, getVar(s->wTo,VarW,VarW2,t->w,EAV), f->w);
  CALC(fH, s->hTo, getVar(s->hTo,VarH,VarH2,t->h,EAV), f->h);
  CALC(fX, s->xTo, getVar(s->xFrom,VarX,VarXref,xref,VarW,f->w,EAV), f->x);
  CALC(fY, s->yTo, getVar(s->yFrom,VarY,VarYref,yref,VarH,f->h,EAV), f->y);

  DEBUG(NAME_spatial,
	Cprintf("%s->b: (%s,%s) -- %ld,%ld,%ld,%ld ==> (%ld, %ld, %ld, %ld)\n",
		pp(s), pp(from), pp(to),
		valInt(t->x), valInt(t->y), valInt(t->w), valInt(t->h),
		valInt(fX), valInt(fY), valInt(fW), valInt(fH)));

  if (f->x != fX || f->y != fY || f->w != fW || f->h != fH)
    return send(from, NAME_set, fX, fY, fW, fH, EAV);

  succeed;
}


static status
createSpatial(Spatial s, Any from, Any to)
{ if (isNil(from) || isNil(to))
    succeed;
  return forwardsSpatial(s, from, to);
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_create[] =
        { "from=graphical*", "to=graphical*" };
static char *T_fromAgraphical_toAgraphical[] =
        { "from=graphical", "to=graphical" };
static char *T_initialise[] =
        { "x1=[=]*", "y1=[=]*", "x2=[=]*", "y2=[=]*", "width=[=]*", "height=[=]*" };

/* Instance Variables */

static vardecl var_spatial[] =
{ IV(NAME_xFrom, "=*", IV_BOTH,
     NAME_position, "X of reference at `from' (XYHW -> xref)"),
  IV(NAME_yFrom, "=*", IV_BOTH,
     NAME_position, "Y of reference at `from' (XYHW -> yref)"),
  IV(NAME_xTo, "=*", IV_BOTH,
     NAME_position, "X of reference at `to' (XYHW -> xref)"),
  IV(NAME_yTo, "=*", IV_BOTH,
     NAME_position, "Y of reference at `to' (XYHW -> yref)"),
  IV(NAME_wTo, "=*", IV_BOTH,
     NAME_dimension, "Equation between `w' and `w2'"),
  IV(NAME_hTo, "=*", IV_BOTH,
     NAME_dimension, "Equation between `h' and `h2'")
};

/* Send Methods */

static senddecl send_spatial[] =
{ SM(NAME_backwards, 2, T_fromAgraphical_toAgraphical, backwardsSpatial,
     DEFAULT, "Maintain after `from' has changed"),
  SM(NAME_create, 2, T_create, createSpatial,
     DEFAULT, "Establish spatial relation"),
  SM(NAME_forwards, 2, T_fromAgraphical_toAgraphical, forwardsSpatial,
     DEFAULT, "Maintain after `to' has changed"),
  SM(NAME_initialise, 6, T_initialise, initialiseSpatial,
     DEFAULT, "Create from equations x1, y1, x2, y2, w, h")
};

/* Get Methods */

#define get_spatial NULL
/*
static getdecl get_spatial[] =
{
};
*/

/* Resources */

#define rc_spatial NULL
/*
static classvardecl rc_spatial[] =
{
};
*/

/* Class Declaration */

static Name spatial_termnames[] =
	{ NAME_xFrom, NAME_yFrom, NAME_xTo, NAME_yTo, NAME_wTo, NAME_hTo };

ClassDecl(spatial_decls,
          var_spatial, send_spatial, get_spatial, rc_spatial,
          6, spatial_termnames,
          "$Rev$");

status
makeClassSpatial(Class class)
{ return declareClass(class, &spatial_decls);
}

