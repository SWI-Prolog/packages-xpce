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
initialiseEqual(Equal eq, Any left, Any right)
{ assign(eq, left,  left);
  assign(eq, right, right);

  return initialiseCode((Code) eq);
}


static status
ExecuteEqual(Equal eq)
{ Any left  = expandCodeArgument(eq->left);
  Any right = expandCodeArgument(eq->right);

  if ( left == FAIL || right == FAIL )
    fail;

  if ( left == right )			/* left ->equal: right? */
    succeed;

  fail;
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_initialise[] =
        { "left=any|function", "right=any|function" };

/* Instance Variables */

static vardecl var_equal[] =
{ IV(NAME_left, "any|function", IV_BOTH,
     NAME_operant, "Left-hand side"),
  IV(NAME_right, "any|function", IV_BOTH,
     NAME_operant, "Right-hand side")
};

/* Send Methods */

static senddecl send_equal[] =
{ SM(NAME_Execute, 0, NULL, ExecuteEqual,
     DEFAULT, "Evaluate both sides and test on equal"),
  SM(NAME_initialise, 2, T_initialise, initialiseEqual,
     DEFAULT, "Create from left- and right-hand")
};

/* Get Methods */

#define get_equal NULL
/*
static getdecl get_equal[] =
{
};
*/

/* Resources */

#define rc_equal NULL
/*
static classvardecl rc_equal[] =
{
};
*/

/* Class Declaration */

static Name equal_termnames[] = { NAME_left, NAME_right };

ClassDecl(equal_decls,
          var_equal, send_equal, get_equal, rc_equal,
          2, equal_termnames,
          "$Rev$");

status
makeClassEqual(Class class)
{ return declareClass(class, &equal_decls);
}


