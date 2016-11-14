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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Rubber levels:

	0	not used (fixed?)
	1	Spacing
	2	hfil (left/center/right alignment)
	3	hfill
	4	hfilll
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static status
initialiseRubber(Rubber r, Int level, Int stretch, Int shrink, Name linebreak)
{ if ( isDefault(level) )
    level = ONE;
  if ( isDefault(stretch) )		/* resource? */
    stretch = ZERO;
  if ( isDefault(shrink) )
    shrink = ZERO;
  if ( isDefault(linebreak) )
    linebreak = NIL;

  assign(r, stretch,   stretch);
  assign(r, shrink,    shrink);
  assign(r, linebreak, linebreak);
  assign(r, level,     level);
  assign(r, natural,   DEFAULT);

  succeed;
}

		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declaractions */

static char *T_initialise[] =
        { "level=[1..]",
	  "stretch=[0..]",
	  "shrink=[0..]",
	  "linebreak=[{allow,force}]*"
	};

/* Instance Variables */

static vardecl var_rubber[] =
{ IV(NAME_stretch,  "0..", IV_GET,
     NAME_rubber, "Ease to get bigger"),
  IV(NAME_shrink, "0..", IV_GET,
     NAME_rubber, "Ease to get smaller"),
  IV(NAME_level, "1..", IV_GET,
     NAME_rubber, "Level of the rubber (TeX hfil/hfill/hfilll)"),
  IV(NAME_natural, "[int]", IV_BOTH,
     NAME_dimension, "Natrual size"),
  IV(NAME_minimum, "int*", IV_BOTH,
     NAME_dimension, "Minimum size"),
  IV(NAME_maximum, "int*", IV_BOTH,
     NAME_dimension, "Maximum size"),
  IV(NAME_linebreak, "{allow,force}*", IV_GET,
     NAME_layout, "Can be use this box as a linebreak")
};

/* Send Methods */

static senddecl send_rubber[] =
{ SM(NAME_initialise, 4, T_initialise, initialiseRubber,
     DEFAULT, "Create rubber from stretch, shrink and linebreak")
};

/* Get Methods */

#define get_rubber NULL
/*
static getdecl get_rubber[] =
{
};
*/

/* Resources */

#define rc_rubber NULL
/*
static classvardecl rc_rubber[] =
{
};
*/

/* Class Declaration */

static Name rubber_termnames[] = { NAME_stretch, NAME_shrink };

ClassDecl(rubber_decls,
          var_rubber, send_rubber, get_rubber, rc_rubber,
          2, rubber_termnames,
          "$Rev$");


status
makeClassRubber(Class class)
{ declareClass(class, &rubber_decls);

  globalObject(NAME_spaceRubber,
	       ClassRubber,
	       toInt(1),
	       toInt(100),
	       toInt(1),
	       NAME_allow,
	       EAV);

  succeed;
}
