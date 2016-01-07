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
initialiseIf(If i, Code cond, Code if_true, Code if_false)
{ initialiseCode((Code) i);

  if ( isDefault(if_true) )  if_true  = NIL;
  if ( isDefault(if_false) ) if_false = NIL;

  assign(i, condition, cond);
  assign(i, then_branch, if_true);
  assign(i, else_branch, if_false);

  succeed;
}


static status
ExecuteIf(If i)
{ if ( executeCode(i->condition) )
  { if ( notNil(i->then_branch) )
      return executeCode(i->then_branch) ? SUCCEED : FAIL;
    succeed;
  } else
  { if ( notNil(i->else_branch) )
      return executeCode(i->else_branch) ? SUCCEED : FAIL;
    succeed;
  }
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_initialise[] =
        { "condition=code", "then=[code]*", "else=[code]*" };

/* Instance Variables */

static vardecl var_if[] =
{ IV(NAME_condition, "code", IV_BOTH,
     NAME_statement, "Condition to be tested"),
  IV(NAME_then, "code*", IV_BOTH,
     NAME_statement, "Executed if condition is true"),
  IV(NAME_else, "code*", IV_BOTH,
     NAME_statement, "Executed if condition is false")
};

/* Send Methods */

static senddecl send_if[] =
{ SM(NAME_Execute, 0, NULL, ExecuteIf,
     DEFAULT, "Test condition and branch"),
  SM(NAME_initialise, 3, T_initialise, initialiseIf,
     DEFAULT, "Create from condition, if- and else")
};

/* Get Methods */

#define get_if NULL
/*
static getdecl get_if[] =
{
};
*/

/* Resources */

#define rc_if NULL
/*
static classvardecl rc_if[] =
{
};
*/

/* Class Declaration */

static Name if_termnames[] = { NAME_condition, NAME_then, NAME_else };

ClassDecl(if_decls,
          var_if, send_if, get_if, rc_if,
          3, if_termnames,
          "$Rev$");

status
makeClassIf(Class class)
{ return declareClass(class, &if_decls);
}


