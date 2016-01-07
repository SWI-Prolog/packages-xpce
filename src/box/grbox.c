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

static status
initialiseGrBox(GrBox grb, Graphical gr,
		Any align,		/* left, right or @nil */
		Rubber rubber)
{ if ( isDefault(align) )
    align = NAME_center;
  if ( isDefault(rubber) )
    rubber = NIL;

  assign(grb, graphical, gr);
  assign(grb, alignment, align);
  assign(grb, rubber,    rubber);

  ComputeGraphical(gr);
  assign(grb, width, gr->area->w);
  computeAscentDescentGrBox(grb);

  succeed;
}


		 /*******************************
		 *	      COMPUTE		*
		 *******************************/

status
computeGrBox(GrBox grb)
{ Graphical gr = grb->graphical;

  ComputeGraphical(gr);
  if ( isNil(grb->rubber) ||
       ( grb->rubber->stretch == ZERO &&
	 grb->rubber->shrink  == ZERO
       ) )
  { DEBUG(NAME_grbox,
	  Cprintf("%s width %d --> %d\n",
		  pp(grb), valInt(grb->width), valInt(gr->area->w)));
    assign(grb, width, gr->area->w);	/* TBD */
  } else
  { DEBUG(NAME_grbox,
	  Cprintf("%s IGNORING width %d --> %d\n",
		  pp(grb), valInt(grb->width), valInt(gr->area->w)));
  }
  computeAscentDescentGrBox(grb);

  succeed;
}


status
computeAscentDescentGrBox(GrBox grb)
{ Graphical gr = grb->graphical;
  int h, ascent, descent;

  ComputeGraphical(gr);
  h = valInt(gr->area->h);

  if ( grb->alignment == NAME_top )
    ascent = 0;
  else if ( grb->alignment == NAME_bottom )
    ascent = h;
  else
    ascent = h/2;

  descent = h-ascent;
  if ( grb->ascent  != toInt(ascent) ||
       grb->descent != toInt(descent) )
  { assign(grb, ascent,  toInt(ascent));
    assign(grb, descent, toInt(descent));

    succeed;				/* changed */
  } else
    fail;				/* no change */
}


static status
alignmentGrBox(GrBox grb, Any alignment)
{ if ( grb->alignment != alignment )
  { assign(grb, alignment, alignment);
    computeAscentDescentGrBox(grb);
  }

  succeed;
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declaractions */

static char *T_initialise[] =
        { "graphical=graphical",
	  "alignment=[{top,center,bottom,left,right}]",
	  "rubber=[rubber]*"
	};

/* Instance Variables */

static vardecl var_grbox[] =
{ IV(NAME_graphical, "graphical", IV_GET,
     NAME_content, "Represented graphical object"),
  SV(NAME_alignment, "{top,center,bottom,left,right}", IV_GET|IV_STORE,
     alignmentGrBox,
     NAME_layout, "Alignment in paragraph")
};

/* Send Methods */

static senddecl send_grbox[] =
{ SM(NAME_initialise, 3, T_initialise, initialiseGrBox,
     DEFAULT, "Create grbox from graphical, alignment and rubber"),
  SM(NAME_compute, 0, NULL, computeGrBox,
     NAME_update, "Compute <-graphical and update dimensions")
};

/* Get Methods */

#define get_grbox NULL
/*
static getdecl get_grbox[] =
{
};
*/

/* Resources */

#define rc_grbox NULL
/*
static classvardecl rc_grbox[] =
{
};
*/

/* Class Declaration */

static Name grbox_termnames[] = { NAME_graphical };

ClassDecl(grbox_decls,
          var_grbox, send_grbox, get_grbox, rc_grbox,
          1, grbox_termnames,
          "$Rev$");


status
makeClassGrBox(Class class)
{ declareClass(class, &grbox_decls);
  delegateClass(class, NAME_graphical);

  succeed;
}

