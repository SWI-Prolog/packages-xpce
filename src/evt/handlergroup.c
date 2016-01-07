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

static status appendHandlerGroup(HandlerGroup, Recogniser);

static status
initialiseHandlerGroupv(HandlerGroup h, int argc, Any *argv)
{ int i;

  assign(h, members, newObject(ClassChain, EAV));
  assign(h, active, ON);

  for (i=0; i<argc; i++)
    appendHandlerGroup(h, argv[i]);

  succeed;
}


static status
eventHandlerGroup(HandlerGroup h, EventObj ev)
{ Cell cell;

  if ( h->active == OFF )
    fail;

  for_cell(cell, h->members)
  { if ( qadSendv(cell->value, NAME_event, 1, (Any *)&ev) )
      succeed;
  }

  fail;
}


static status
appendHandlerGroup(HandlerGroup h, Recogniser r)
{ return appendChain(h->members, r);
}


static status
deleteHandlerGroup(HandlerGroup h, Recogniser r)
{ return deleteChain(h->members, r);
}


static Int
getArityHandlerGroup(HandlerGroup h)
{ answer(getSizeChain(h->members));
}


static Any
getArgHandlerGroup(HandlerGroup h, Int n)
{ extern Any getNth1Chain(Chain ch, Int index);
  answer(getNth1Chain(h->members, n));
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */


/* Instance Variables */

static vardecl var_handlerGroup[] =
{ IV(NAME_members, "chain", IV_GET,
     NAME_list, "Members of the collection")
};

/* Send Methods */

static senddecl send_handlerGroup[] =
{ SM(NAME_initialise, 1, "member=recogniser ...", initialiseHandlerGroupv,
     DEFAULT, "Create from given recognisers"),
  SM(NAME_event, 1, "event", eventHandlerGroup,
     NAME_event, "Process an event"),
  SM(NAME_append, 1, "recogniser", appendHandlerGroup,
     NAME_list, "Append a recogniser"),
  SM(NAME_delete, 1, "recogniser", deleteHandlerGroup,
     NAME_list, "Delete first occurrence of recogniser")
};

/* Get Methods */

static getdecl get_handlerGroup[] =
{ GM(NAME_Arg, 1, "recogniser", "int", getArgHandlerGroup,
     DEFAULT, "Nth-1 argument or term description"),
  GM(NAME_Arity, 0, "int", NULL, getArityHandlerGroup,
     DEFAULT, "Arity of term description")
};

/* Resources */

#define rc_handlerGroup NULL
/*
static classvardecl rc_handlerGroup[] =
{
};
*/

/* Class Declaration */

ClassDecl(handlerGroup_decls,
          var_handlerGroup, send_handlerGroup,
	  get_handlerGroup, rc_handlerGroup,
          ARGC_UNKNOWN, NULL,
          "$Rev$");

status
makeClassHandlerGroup(Class class)
{ return declareClass(class, &handlerGroup_decls);
}
