/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  1999-2011, University of Amsterdam
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

#include "boxes.h"

status
initialiseHBox(HBox hb, Int width, Int ascent, Int descent, Rubber rubber)
{ if ( isDefault(rubber) )		/* resource? */
    rubber = NIL;
  if ( isDefault(width) )
    width = ZERO;
  if ( isDefault(ascent) )
    ascent = ZERO;
  if ( isDefault(descent) )
    descent = ZERO;

  assign(hb, width,   width);
  assign(hb, ascent,  ascent);
  assign(hb, descent, descent);
  assign(hb, rubber, rubber);

  succeed;
}

		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declaractions */

static char *T_initialise[] =
        { "width=[int]",
	  "ascent=[int]", "descent=[int]",
	  "rubber=[rubber]*"
	};

/* Instance Variables */

static vardecl var_hbox[] =
{ IV(NAME_width,  "int", IV_GET,
     NAME_dimension, "Natural width of content"),
  IV(NAME_ascent, "0..", IV_GET,
     NAME_dimension, "Height above baseline"),
  IV(NAME_descent, "0..", IV_GET,
     NAME_dimension, "Depth below baseline"),
  IV(NAME_rubber, "rubber*", IV_GET,
     NAME_layout, "Stretch/shrinkability")
};

/* Send Methods */

static senddecl send_hbox[] =
{ SM(NAME_initialise, 4, T_initialise, initialiseHBox,
     DEFAULT, "Create hbox from dimensions")
};

/* Get Methods */

#define get_hbox NULL
/*
static getdecl get_hbox[] =
{
};
*/

/* Resources */

#define rc_hbox NULL
/*
static classvardecl rc_hbox[] =
{
};
*/

/* Class Declaration */

static Name hbox_termnames[] = \
	{ NAME_width, NAME_ascent, NAME_descent, NAME_rubber };

ClassDecl(hbox_decls,
          var_hbox, send_hbox, get_hbox, rc_hbox,
          4, hbox_termnames,
          "$Rev$");


status
makeClassHBox(Class class)
{ return declareClass(class, &hbox_decls);
}

