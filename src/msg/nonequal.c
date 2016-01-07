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
initialiseNonEqual(NonEqual c, Any left, Any right)
{ assign(c, left,  left);
  assign(c, right, right);

  return initialiseCode((Code) c);
}


static status
ExecuteNonEqual(NonEqual c)
{ Any left  = expandCodeArgument(c->left);
  Any right = expandCodeArgument(c->right);

  if ( left == FAIL || right == FAIL )
    fail;

  if ( left != right )
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

static vardecl var_nonEqual[] =
{ IV(NAME_left, "any|function", IV_BOTH,
     NAME_operant, "Left-hand side"),
  IV(NAME_right, "any|function", IV_BOTH,
     NAME_operant, "Right-hand side")
};

/* Send Methods */

static senddecl send_nonEqual[] =
{ SM(NAME_Execute, 0, NULL, ExecuteNonEqual,
     DEFAULT, "Evaluate both sides and test on non-equal"),
  SM(NAME_initialise, 2, T_initialise, initialiseNonEqual,
     DEFAULT, "Create from left- and right-hand")
};

/* Get Methods */

#define get_nonEqual NULL
/*
static getdecl get_nonEqual[] =
{
};
*/

/* Resources */

#define rc_nonEqual NULL
/*
static classvardecl rc_nonEqual[] =
{
};
*/

/* Class Declaration */

static Name nonEqual_termnames[] = { NAME_left, NAME_right };

ClassDecl(nonEqual_decls,
          var_nonEqual, send_nonEqual, get_nonEqual, rc_nonEqual,
          2, nonEqual_termnames,
          "$Rev$");


status
makeClassNonEqual(Class class)
{ return declareClass(class, &nonEqual_decls);
}

