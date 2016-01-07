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
#include <h/graphics.h>

static status arrowsJoint(Joint, Name);

status
initialiseJoint(Joint jt, Int x, Int y, Int w, Int h, Name arrows)
{ initialiseGraphical(jt, x, y, w, h);

  if ( notDefault(arrows) )
    arrowsJoint(jt, arrows);

  succeed;
}


status
copyJoint(Joint jt1, Joint jt2)
{ copyGraphical(jt1, jt2);

  return setArrowsJoint(jt1, jt2->first_arrow, jt2->second_arrow);
}


		/********************************
		*            ARROWS		*
		********************************/

static Graphical
getDefaultArrowJoint(Joint jt)
{ answer(newObject(ClassArrow, EAV));
}


status
setArrowsJoint(Joint jt, Graphical first, Graphical second)
{ if ( isDefault(first)  ) first  = jt->first_arrow;
  if ( isDefault(second) ) second = jt->second_arrow;

  if ( jt->first_arrow == first && jt->second_arrow == second )
    succeed;

  CHANGING_GRAPHICAL(jt,
		     assign(jt, first_arrow, first);
		     assign(jt, second_arrow, second);
		     requestComputeGraphical(jt, DEFAULT);
		     changedEntireImageGraphical(jt));

  succeed;
}


static status
firstArrowJoint(Joint jt, Graphical arrow)
{ return setArrowsJoint(jt, arrow, DEFAULT);
}


static status
secondArrowJoint(Joint jt, Graphical arrow)
{ return setArrowsJoint(jt, DEFAULT, arrow);
}


static Graphical
initArrowJoint(Joint jt)
{ Any rval;

  if ( !(rval = qadGetv(jt, NAME_defaultArrow, 0, NULL)) )
    rval = NIL;

  return rval;
}


static status
arrowsJoint(Joint jt, Name arrows)
{ Graphical first, second;

  if ( equalName(arrows, NAME_none) )
  { first  = NIL;
    second = NIL;
  } else if ( arrows == NAME_first )
  { first = (notNil(jt->first_arrow) ? jt->first_arrow : initArrowJoint(jt));
    second = NIL;
  } else if ( arrows == NAME_second )
  { first = NIL;
    second = (notNil(jt->second_arrow) ? jt->second_arrow
				       : initArrowJoint(jt));
  } else if ( arrows == NAME_both )
  { first = (notNil(jt->first_arrow) ? jt->first_arrow : initArrowJoint(jt));
    second = (notNil(jt->second_arrow) ? jt->second_arrow
	      			       : initArrowJoint(jt));
  } else
    fail;

  return setArrowsJoint(jt, first, second);
}


static Name
getArrowsJoint(Joint jt)
{ if ( notNil(jt->first_arrow) )
  { if ( notNil(jt->second_arrow) )
      return NAME_both;
    else
      return NAME_first;
  } else
  { if ( notNil(jt->second_arrow) )
      return NAME_second;
    else
      return NAME_none;
  }
}

		 /*******************************
		 *	      SELECTED		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The selection blobs of joints often exceed the area.  Enlarge it.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static status
selectedJoint(Joint jt, BoolObj selected)
{ if ( jt->selected != selected )
  { CHANGING_GRAPHICAL(jt,
		       assign(jt, selected, selected);
		       requestComputeGraphical(jt, DEFAULT);
		       changedEntireImageGraphical(jt));
  }

  succeed;
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_initialise[] =
        { "x=[int]", "y=[int]", "width=[int]",
	  "height=[int]", "arrows=[{none,first,second,both}]" };

/* Instance Variables */

static vardecl var_joint[] =
{ SV(NAME_firstArrow, "graphical*", IV_GET|IV_STORE, firstArrowJoint,
     NAME_appearance, "Arrow on start-point"),
  SV(NAME_secondArrow, "graphical*", IV_GET|IV_STORE, secondArrowJoint,
     NAME_appearance, "Arrow on end-point")
};

/* Send Methods */

static senddecl send_joint[] =
{ SM(NAME_initialise, 5, T_initialise, initialiseJoint,
     DEFAULT, "Create joint with bounding-box and arrows"),
  SM(NAME_arrows, 1, "arrows={none,first,second,both}", arrowsJoint,
     NAME_appearance, "Default arrows on {none,first,second,both}"),
  SM(NAME_selected, 1, "bool", selectedJoint,
     NAME_selection, "If @on, I'm selected")
};

/* Get Methods */

static getdecl get_joint[] =
{ GM(NAME_arrows, 0, "arrows={none,first,second,both}", NULL, getArrowsJoint,
     NAME_appearance, "Which arrows are defined"),
  GM(NAME_defaultArrow, 0, "graphical", NULL, getDefaultArrowJoint,
     NAME_appearance, "Create default arrow for ->arrows")
};

/* Resources */

#define rc_joint NULL
/*
static classvardecl rc_joint[] =
{
};
*/

/* Class Declaration */

static Name joint_termnames[] = { NAME_arrows };

ClassDecl(joint_decls,
          var_joint, send_joint, get_joint, rc_joint,
          1, joint_termnames,
          "$Rev$");


status
makeClassJoint(Class class)
{ return declareClass(class, &joint_decls);
}
