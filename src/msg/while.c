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
initialiseWhile(While w, Code cond, Code body)
{ initialiseCode((Code) w);

  if ( isDefault(body) )
    body = NIL;

  assign(w, condition, cond);
  assign(w, body,      body);

  succeed;
}


static status
ExecuteWhile(While w)
{ while ( executeCode(w->condition) )
  { if ( notNil(w->body) )
    { TRY( executeCode(w->body) );
    }
  }

  succeed;
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_initialise[] =
        { "condition=code", "statement=[code]*" };

/* Instance Variables */

static vardecl var_while[] =
{ IV(NAME_condition, "code", IV_BOTH,
     NAME_statement, "Condition to be tested"),
  IV(NAME_body, "code*", IV_BOTH,
     NAME_statement, "Statement to execute")
};

/* Send Methods */

static senddecl send_while[] =
{ SM(NAME_Execute, 0, NULL, ExecuteWhile,
     DEFAULT, "Execute body until test fails"),
  SM(NAME_initialise, 2, T_initialise, initialiseWhile,
     DEFAULT, "Create from condition and statement")
};

/* Get Methods */

#define get_while NULL
/*
static getdecl get_while[] =
{
};
*/

/* Resources */

#define rc_while NULL
/*
static classvardecl rc_while[] =
{
};
*/

/* Class Declaration */

static Name while_termnames[] = { NAME_condition, NAME_body };

ClassDecl(while_decls,
          var_while, send_while, get_while, rc_while,
          2, while_termnames,
          "$Rev$");

status
makeClassWhile(Class class)
{ return declareClass(class, &while_decls);
}



