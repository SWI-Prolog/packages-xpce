/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  1995-2011, University of Amsterdam
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

static status
initialiseMoveOutlineGesture(MoveOutlineGesture g,
			     Name button, Modifier modifier)
{ initialiseMoveGesture((MoveGesture) g, button, modifier);
  obtainClassVariablesObject(g);
  assign(g, outline, newObject(ClassBox, EAV));
  send(g->outline, NAME_texture,
       getClassVariableValueObject(g, NAME_texture), EAV);
  send(g->outline, NAME_recogniser,
       newObject(ClassMoveGesture, g->button, g->modifier, EAV), EAV);

  succeed;
}


		/********************************
		*       GESTURE BEHAVIOUR	*
		********************************/

static status
initiateMoveOutlineGesture(MoveOutlineGesture g, EventObj ev)
{ Graphical gr = ev->receiver;

  if ( !instanceOfObject(gr, ClassGraphical) )
    fail;
  send(g->outline, NAME_area, gr->area, EAV);
  send(gr->device, NAME_display, g->outline, EAV);
  postEvent(ev, (Graphical) g->outline, DEFAULT);

  succeed;
}


static status
dragMoveOutlineGesture(MoveOutlineGesture g, EventObj ev)
{ postEvent(ev, (Graphical) g->outline, DEFAULT);

  succeed;
}


static status
terminateMoveOutlineGesture(MoveOutlineGesture g, EventObj ev)
{ Area a;

  send(g, NAME_drag, ev, EAV);

  a = g->outline->area;
  send(ev->receiver, NAME_doSet, a->x, a->y, a->w, a->h, EAV);
  send(g->outline, NAME_device, NIL, EAV);

  succeed;
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_initialise[] =
        { "button=[button_name]", "modifier=[modifier]" };

/* Instance Variables */

static vardecl var_moveOutlineGesture[] =
{ IV(NAME_outline, "box", IV_GET,
     NAME_feedback, "The outline moved")
};

/* Send Methods */

static senddecl send_moveOutlineGesture[] =
{ SM(NAME_drag, 1, "event", dragMoveOutlineGesture,
     DEFAULT, "Drag outline to next position"),
  SM(NAME_initialise, 2, T_initialise, initialiseMoveOutlineGesture,
     DEFAULT, "Create from button and modifier"),
  SM(NAME_initiate, 1, "event", initiateMoveOutlineGesture,
     DEFAULT, "Display outline and change cursor"),
  SM(NAME_terminate, 1, "event", terminateMoveOutlineGesture,
     DEFAULT, "Move object and undisplay outline")
};

/* Get Methods */

#define get_moveOutlineGesture NULL
/*
static getdecl get_moveOutlineGesture[] =
{
};
*/

/* Resources */

static classvardecl rc_moveOutlineGesture[] =
{ RC(NAME_button, "button_name", "middle",
     "Active on which button (middle)"),
  RC(NAME_cursor, "cursor", "fleur",
     "Cursor while active"),
  RC(NAME_texture, "texture_name", "dotted",
     "Texture of the outline box")
};

/* Class Declaration */

static Name moveOutlineGesture_termnames[] = { NAME_button, NAME_modifier };

ClassDecl(moveOutlineGesture_decls,
          var_moveOutlineGesture, send_moveOutlineGesture,
	  get_moveOutlineGesture, rc_moveOutlineGesture,
          2, moveOutlineGesture_termnames,
          "$Rev$");

status
makeClassMoveOutlineGesture(Class class)
{ return declareClass(class, &moveOutlineGesture_decls);
}

