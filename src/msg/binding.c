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
initialiseBinding(Binding att, Any name, Any value)
{ assign(att, name, name);
  assign(att, value, value);

  setFlag(att, F_ISBINDING);

  succeed;
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_initialise[] =
        { "name=name", "value=any|function" };

/* Instance Variables */

static vardecl locals_binding[] =
{ IV(NAME_name, "name", IV_BOTH,
     NAME_argument, "Name of the binding"),
  IV(NAME_value, "any|function", IV_BOTH,
     NAME_value, "Value of the binding")
};

/* Send Methods */

static senddecl send_binding[] =
{ SM(NAME_initialise, 2, T_initialise, initialiseBinding,
     DEFAULT, "Create binding from name and value")
};

/* Get Methods */

#define get_binding NULL
/*
static getdecl get_binding[] =
{
};
*/

/* Resources */

#define rc_binding NULL
/*
static classvardecl rc_binding[] =
{
};
*/

/* Class Declaration */

static Name binding_termnames[] = { NAME_name, NAME_value };

ClassDecl(binding_decls,
          locals_binding, send_binding, get_binding, rc_binding,
          2, binding_termnames,
          "$Rev$");

status
makeClassBinding(Class class)
{ return declareClass(class, &binding_decls);
}

