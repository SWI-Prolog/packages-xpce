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

status
initialiseBehaviour(Behaviour b, Name name, Any ctx)
{ initialiseProgramObject(b);

  if ( isDefault(ctx) )
    ctx = NIL;

  assign(b, name, name);
  assign(b, context, ctx);

  succeed;
}

		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declaractions */

static char *T_initialise[] =
        { "name=name", "context=[class|object*]" };

/* Instance Variables */

static vardecl var_behaviour[] =
{ IV(NAME_name, "name", IV_GET,
     NAME_name, "Selector of this behaviour"),
  IV(NAME_context, "class|object*", IV_GET,
     NAME_whole, "Definition context of this method")
};

/* Send Methods */

static senddecl send_behaviour[] =
{ SM(NAME_initialise, 2, T_initialise, initialiseBehaviour,
     DEFAULT, "Create from <-name and <-context")
};

/* Get Methods */

#define get_behaviour NULL
/*
static getdecl get_behaviour[] =
{
};
*/

/* Resources */

#define rc_behaviour NULL
/*
static classvardecl rc_behaviour[] =
{
};
*/

/* Class Declaration */

static Name behaviour_termnames[] = { NAME_name, NAME_context };

ClassDecl(behaviour_decls,
          var_behaviour, send_behaviour, get_behaviour, rc_behaviour,
          2, behaviour_termnames,
          "$Rev$");


status
makeClassBehaviour(Class class)
{ declareClass(class, &behaviour_decls);
  cloneStyleVariableClass(class, NAME_context, NAME_reference);

  succeed;
}

