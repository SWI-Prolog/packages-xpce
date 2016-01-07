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
initialiseNotv(Not n, Code arg)
{ initialiseCode((Code) n);
  assign(n, argument, arg);

  succeed;
}


		/********************************
		*           EXECUTION		*
		********************************/

static status
ExecuteNot(Not n)
{ if ( executeCode(n->argument) != FAIL )
    fail;

  succeed;
}

		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */


/* Instance Variables */

static vardecl var_not[] =
{ IV(NAME_argument, "code", IV_BOTH,
     NAME_statement, "Test to negate")
};

/* Send Methods */

static senddecl send_not[] =
{ SM(NAME_Execute, 0, NULL, ExecuteNot,
     DEFAULT, "Evaluate argument test and negate result"),
  SM(NAME_initialise, 1, "test=code", initialiseNotv,
     DEFAULT, "Create from test")
};

/* Get Methods */

#define get_not NULL
/*
static getdecl get_not[] =
{
};
*/

/* Resources */

#define rc_not NULL
/*
static classvardecl rc_not[] =
{
};
*/

/* Class Declaration */

static Name not_termnames[] = { NAME_argument };

ClassDecl(not_decls,
          var_not, send_not, get_not, rc_not,
          1, not_termnames,
          "$Rev$");


status
makeClassNot(Class class)
{ return declareClass(class, &not_decls);
}

