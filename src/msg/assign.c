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
initialiseAssignment(Assignment b, Var var, Any value, Name scope)
{ initialiseCode((Code) b);

  if ( isDefault(scope) )
    scope = NAME_local;

  assign(b, var,   var);
  assign(b, value, value);
  assign(b, scope, scope);

  succeed;
}


static status
ExecuteAssignment(Assignment b)
{ Any val;

  TRY(val = expandCodeArgument(b->value));
  return assignVar(b->var, val, b->scope);
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_initialise[] =
        { "variable=var", "value=any|function", "scope=[{local,outer,global}]" };

/* Instance Variables */

static vardecl var_assign[] =
{ IV(NAME_var, "var", IV_BOTH,
     NAME_storage, "Variable to be bound"),
  IV(NAME_value, "any|function", IV_BOTH,
     NAME_storage, "Value to give to the assignment"),
  IV(NAME_scope, "{local,outer,global}", IV_BOTH,
     NAME_scope, "Scope of assignment")
};

/* Send Methods */

static senddecl send_assign[] =
{ SM(NAME_Execute, 0, NULL, ExecuteAssignment,
     DEFAULT, "Bind the variable"),
  SM(NAME_initialise, 3, T_initialise, initialiseAssignment,
     DEFAULT, "Create assignment from var and value")
};

/* Get Methods */

#define get_assign NULL
/*
static getdecl get_assign[] =
{
};
*/

/* Resources */

#define rc_assign NULL
/*
static classvardecl rc_assign[] =
{
};
*/

/* Class Declaration */

static Name assign_termnames[] = { NAME_var, NAME_value, NAME_scope };

ClassDecl(assign_decls,
          var_assign, send_assign, get_assign, rc_assign,
          3, assign_termnames,
          "$Rev$");

status
makeClassAssign(Class class)
{ return declareClass(class, &assign_decls);
}

