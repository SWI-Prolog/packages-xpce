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
#include <h/lang.h>

static status	kindOperator(Operator o, Name kind);

static status
initialiseOperator(Operator o, Name name, Int priority, Name kind)
{ assign(o, name, name);
  assign(o, priority, priority);

  return kindOperator(o, kind);
}


static status
kindOperator(Operator o, Name kind)
{ int lp, rp, p = valInt(o->priority);

  if ( kind == NAME_xf )
    lp = p-1, rp = 0;
  else if ( kind == NAME_yf )
    lp = p, rp = 0;
  else if ( kind == NAME_fx )
    lp = 0, rp = p-1;
  else if ( kind == NAME_fy )
    lp = 0, rp = p;
  else if ( kind == NAME_xfx )
    lp = rp = p-1;
  else if ( kind == NAME_xfy )
    lp = p-1, rp = p;
  else /* if ( kind == NAME_yfx ) */
    lp = p, rp = p-1;

  assign(o, left_priority, toInt(lp));
  assign(o, right_priority, toInt(rp));

  succeed;
}


static Name
getKindOperator(Operator o)
{ Int lp = o->left_priority;
  Int rp = o->right_priority;
  Int p  = o->priority;

  if ( lp == ZERO )
    answer(rp == p ? NAME_fy : NAME_fx );
  if ( rp == ZERO )
    answer(lp == p ? NAME_yf : NAME_xf);
  if ( rp == p )
    answer(NAME_xfy);
  else
    answer(lp == p ? NAME_yfx : NAME_xfx);
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_initialise[] =
        { "name=name", "priority=0..1200", "kind={xf,yf,xfx,xfy,yfx,fy,fx}" };

/* Instance Variables */

static vardecl var_operator[] =
{ IV(NAME_name, "name", IV_GET,
     NAME_name, "Name of the operator"),
  IV(NAME_priority, "int", IV_GET,
     NAME_internal, "Priority of the operator"),
  IV(NAME_leftPriority, "int", IV_GET,
     NAME_internal, "Max priority of left-hand operant"),
  IV(NAME_rightPriority, "int", IV_GET,
     NAME_internal, "Max priority of right-hand operant")
};

/* Send Methods */

static senddecl send_operator[] =
{ SM(NAME_initialise, 3, T_initialise, initialiseOperator,
     DEFAULT, "Initialise"),
  SM(NAME_kind, 1, "kind={xf,yf,xfx,xfy,yfx,fy,fx}", kindOperator,
     NAME_syntax, "Define associativity of the operator")
};

/* Get Methods */

static getdecl get_operator[] =
{ GM(NAME_kind, 0, "kind={xf,yf,xfx,xfy,yfx,fy,fx}", NULL, getKindOperator,
     NAME_syntax, "associativity of the operator")
};

/* Resources */

#define rc_operator NULL
/*
static classvardecl rc_operator[] =
{
};
*/

/* Class Declaration */

static Name operator_termnames[] = { NAME_name, NAME_priority, NAME_kind };

ClassDecl(operator_decls,
          var_operator, send_operator, get_operator, rc_operator,
          3, operator_termnames,
          "$Rev$");

status
makeClassOperator(Class class)
{ return declareClass(class, &operator_decls);
}
