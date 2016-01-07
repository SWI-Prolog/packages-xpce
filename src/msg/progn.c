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
initialisePrognv(Progn p, int argc, Any *argv)
{ int n;

  initialiseFunction((Function) p);
  assign(p, members, newObject(ClassChain, EAV));

  for(n=0; n<argc; n++)
    appendChain(p->members, argv[n]);

  succeed;
}



		/********************************
		*           EXECUTION		*
		********************************/

static Any
getExecuteProgn(Progn p)
{ Cell cell;
  Any rval = FAIL;

  if ( emptyChain(p->members) )
  { errorPce(p, NAME_lastIsNoFunction);
    fail;
  }

  withLocalVars(for_cell(cell, p->members)
	      { if ( notNil(cell->next) )
		{ if ( !instanceOfObject(cell->value, ClassCode) )
		  { errorPce(cell->value, NAME_cannotExecute);
		    break;
		  }

		  if ( !executeCode(cell->value) )
		    break;

		  continue;
		}

		rval = expandCodeArgument(cell->value);
	      });

  answer(rval);
}


static status
appendProgn(Progn p, Code statement)
{ return appendChain(p->members, statement);
}


		/********************************
		*      TERM REPRESENTATION	*
		********************************/

static Int
getArityProgn(Progn p)
{ answer(getArityChain(p->members));
}


static Any
getArgProgn(Progn p, Int n)
{ answer(getArgChain(p->members, n));
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */


/* Instance Variables */

static vardecl var_progn[] =
{ IV(NAME_members, "chain", IV_GET,
     NAME_statement, "Statements and return function")
};

/* Send Methods */

static senddecl send_progn[] =
{ SM(NAME_initialise, 1, "statement=code|any ...", initialisePrognv,
     DEFAULT, "Create progn from statements and return"),
  SM(NAME_append, 1, "code|any", appendProgn,
     NAME_list, "Append a statement")
};

/* Get Methods */

static getdecl get_progn[] =
{ GM(NAME_Arg, 1, "code", "int", getArgProgn,
     DEFAULT, "Nth-1 argument for term description"),
  GM(NAME_Arity, 0, "int", NULL, getArityProgn,
     DEFAULT, "Arity for term description"),
  GM(NAME_Execute, 0, "unchecked", NULL, getExecuteProgn,
     DEFAULT, "Execute the progn")
};

/* Resources */

#define rc_progn NULL
/*
static classvardecl rc_progn[] =
{
};
*/

/* Class Declaration */

ClassDecl(progn_decls,
          var_progn, send_progn, get_progn, rc_progn,
          ARGC_UNKNOWN, NULL,
          "$Rev$");

status
makeClassProgn(Class class)
{ return declareClass(class, &progn_decls);
}

