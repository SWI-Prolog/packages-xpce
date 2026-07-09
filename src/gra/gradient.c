/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
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

/* Class `gradient' — a linear or radial colour gradient usable as a
 * graphical <-fill.  The Cairo backend consumes the slots directly
 * (see pce_cairo_set_source_fill in sdldraw.c).
 *
 * Linear:  p0 and p1 are the start and end points of the axis; r0
 *          and r1 are ignored (typically @default).
 * Radial:  p0 is the start-circle center and r0 its radius; p1 is
 *          the end-circle center and r1 its radius.
 *
 * `stops' is a chain of `tuple(fraction, colour)' pairs, `fraction'
 * in 0..1.
 */

static status
initialiseGradient(Gradient g, Name kind, Point p0, Point p1,
		   Num r0, Num r1, Chain stops)
{ if ( kind != NAME_linear && kind != NAME_radial )
    return errorPce(g, NAME_unexpectedType, kind);

  assign(g, kind,  kind);
  assign(g, p0,    p0);
  assign(g, p1,    p1);
  assign(g, r0,    isDefault(r0) ? (Num)NIL : r0);
  assign(g, r1,    isDefault(r1) ? (Num)NIL : r1);
  assign(g, stops, isDefault(stops) ? newObject(ClassChain, EAV) : stops);

  succeed;
}


static status
stopsGradient(Gradient g, Chain stops)
{ assign(g, stops, stops);
  succeed;
}


static status
addStopGradient(Gradient g, Num frac, Colour c)
{ Tuple t = newObject(ClassTuple, frac, c, EAV);
  appendChain(g->stops, t);
  succeed;
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

static char *T_initialise[] =
	{ "kind={linear,radial}",
	  "p0=point", "p1=point",
	  "r0=[num]*", "r1=[num]*",
	  "stops=[chain]"
	};
static char *T_addStop[] =
	{ "fraction=num", "colour=colour" };

static vardecl var_gradient[] =
{ IV(NAME_kind, "{linear,radial}", IV_GET,
     NAME_kind, "Linear or radial gradient"),
  IV(NAME_p0, "point", IV_GET,
     NAME_dimension, "Linear start / radial start-circle center"),
  IV(NAME_p1, "point", IV_GET,
     NAME_dimension, "Linear end / radial end-circle center"),
  IV(NAME_r0, "num*", IV_GET,
     NAME_dimension, "Radial start-circle radius (@nil for linear)"),
  IV(NAME_r1, "num*", IV_GET,
     NAME_dimension, "Radial end-circle radius (@nil for linear)"),
  SV(NAME_stops, "chain", IV_GET|IV_STORE, stopsGradient,
     NAME_appearance, "Chain of tuple(fraction, colour) stops")
};

static senddecl send_gradient[] =
{ SM(NAME_initialise, 6, T_initialise, initialiseGradient,
     DEFAULT, "Create linear/radial gradient with two points and stops"),
  SM(NAME_addStop, 2, T_addStop, addStopGradient,
     NAME_appearance, "Append a (fraction, colour) stop")
};

#define get_gradient NULL
#define rc_gradient  NULL

static Name gradient_termnames[] = { NAME_kind };

ClassDecl(gradient_decls,
	  var_gradient, send_gradient, get_gradient, rc_gradient,
	  1, gradient_termnames);


status
makeClassGradient(Class class)
{ declareClass(class, &gradient_decls);

  succeed;
}
