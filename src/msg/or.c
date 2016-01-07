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
initialiseOrv(Or or, int argc, Any *argv)
{ int n;

  initialiseCode((Code) or);
  assign(or, members,   newObject(ClassChain, EAV));

  for(n=0; n<argc; n++)
    appendChain(or->members, argv[n]);

  succeed;
}


		/********************************
		*           EXECUTION		*
		********************************/

static status
ExecuteOr(Or or)
{ Cell cell;

  for_cell(cell, or->members)
  { if ( executeCode(cell->value) != FAIL )
      succeed;
  }

  fail;
}

		/********************************
		*      TERM REPRESENTATION	*
		********************************/

static Int
getArityOr(Or or)
{ answer(getArityChain(or->members));
}


static Any
getArgOr(Or or, Int n)
{ answer(getArgChain(or->members, n));
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */


/* Instance Variables */

static vardecl var_or[] =
{ IV(NAME_members, "chain", IV_GET,
     NAME_statement, "One of these must succeed")
};

/* Send Methods */

static senddecl send_or[] =
{ SM(NAME_Execute, 0, NULL, ExecuteOr,
     DEFAULT, "Evaluate tests until one succeeds"),
  SM(NAME_initialise, 1, "test=code ...", initialiseOrv,
     DEFAULT, "Create from tests")
};

/* Get Methods */

static getdecl get_or[] =
{ GM(NAME_Arg, 1, "code", "int", getArgOr,
     DEFAULT, "Nth-1 argument for term description"),
  GM(NAME_Arity, 0, "int", NULL, getArityOr,
     DEFAULT, "Arity for term description")
};

/* Resources */

#define rc_or NULL
/*
static classvardecl rc_or[] =
{
};
*/

/* Class Declaration */

ClassDecl(or_decls,
          var_or, send_or, get_or, rc_or,
          ARGC_UNKNOWN, NULL,
          "$Rev$");

status
makeClassOr(Class class)
{ declareClass(class, &or_decls);
  delegateClass(class, NAME_members);

  succeed;
}

