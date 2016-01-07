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
initialiseWhen(When w, Code cond, Function when_true, Function when_false)
{ initialiseFunction((Function) w);

  assign(w, condition, cond);
  assign(w, then_branch, when_true);
  assign(w, else_branch, when_false);

  succeed;
}


static Any
getExecuteWhen(When w)
{ if ( executeCode(w->condition) )
    return expandCodeArgument(w->then_branch);
  else
    return expandCodeArgument(w->else_branch);
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_initialise[] =
        { "condition=code", "then=any|function", "else=any|function" };

/* Instance Variables */

static vardecl var_when[] =
{ IV(NAME_condition, "code", IV_BOTH,
     NAME_statement, "Condition to be tested"),
  IV(NAME_then, "any|function", IV_BOTH,
     NAME_statement, "Executed if condition is true"),
  IV(NAME_else, "any|function", IV_BOTH,
     NAME_statement, "Executed if condition is false")
};

/* Send Methods */

static senddecl send_when[] =
{ SM(NAME_initialise, 3, T_initialise, initialiseWhen,
     DEFAULT, "Create from condition, when- and else"),
  SM(NAME_unlink, 0, NULL, succeedObject,
     DEFAULT, "temp; just to trap")
};

/* Get Methods */

static getdecl get_when[] =
{ GM(NAME_Execute, 0, "unchecked", NULL, getExecuteWhen,
     DEFAULT, "Test condition and evaluate <-then or <-else")
};

/* Resources */

#define rc_when NULL
/*
static classvardecl rc_when[] =
{
};
*/

/* Class Declaration */

static Name when_termnames[] = { NAME_condition, NAME_then, NAME_else };

ClassDecl(when_decls,
          var_when, send_when, get_when, rc_when,
          3, when_termnames,
          "$Rev$");

status
makeClassWhen(Class class)
{ return declareClass(class, &when_decls);
}


