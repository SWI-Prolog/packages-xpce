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

static status
initialiseAndv(And a, int argc, Any *argv)
{ int n;

  initialiseCode((Code) a);
  assign(a, members, newObject(ClassChain, EAV));

  for(n=0; n<argc; n++)
    appendChain(a->members, argv[n]);

  succeed;
}


		/********************************
		*           EXECUTION		*
		********************************/

static status
ExecuteAnd(And a)
{ Cell cell;

  for_cell(cell, a->members)
    TRY(executeCode(cell->value));

  succeed;
}

		/********************************
		*      TERM REPRESENTATION	*
		********************************/

static Int
getArityAnd(And a)
{ answer(getArityChain(a->members));
}


static Any
getArgAnd(And a, Int n)
{ answer(getArgChain(a->members, n));
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */


/* Instance Variables */

static vardecl var_and[] =
{ IV(NAME_members, "chain", IV_GET,
     NAME_statement, "Tests that must succeed")
};

/* Send Methods */

static senddecl send_and[] =
{ SM(NAME_Execute, 0, NULL, ExecuteAnd,
     DEFAULT, "Evaluate and"),
  SM(NAME_initialise, 1, "test=code ...", initialiseAndv,
     DEFAULT, "Create and from tests")
};

/* Get Methods */

static getdecl get_and[] =
{ GM(NAME_Arg, 1, "code", "int", getArgAnd,
     DEFAULT, "Nth-1 argument for term description"),
  GM(NAME_Arity, 0, "int", NULL, getArityAnd,
     DEFAULT, "Arity for term description")
};

/* Resources */

#define rc_and NULL
/*
static classvardecl rc_and[] =
{
};
*/

/* Class Declaration */

ClassDecl(and_decls,
          var_and, send_and, get_and, rc_and,
          ARGC_UNKNOWN, NULL,
          "$Rev$");

status
makeClassAnd(Class class)
{ declareClass(class, &and_decls);
  delegateClass(class, NAME_members);

  succeed;
}

