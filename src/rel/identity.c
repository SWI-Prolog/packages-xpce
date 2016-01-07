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

static status	forwardsIdentity(Identity id, Any from, Any to);

static status
initialiseIdentity(Identity id, Name from, Name to)
{ assign(id, from, from);
  assign(id, to, isDefault(to) ? from : to);

  succeed;
}


static status
createIdentity(Identity id, Any from, Any to)
{ if (isNil(from) || isNil(to))
    succeed;
  return forwardsIdentity(id, from, to);
}


static status
forwardsIdentity(Identity id, Any from, Any to)
{ Any value;
  status rval;

  TRY(value = get(from, id->from, EAV));
  rval = send(to, id->to, value, EAV);
  if ( isObject(value) )
    doneObject(value);

  return rval;
}


static status
backwardsIdentity(Identity id, Any from, Any to)
{ Any value;
  status rval;

  TRY(value = get(to, id->to, EAV));
  rval = send(from, id->from, value, EAV);
  if ( isObject(value) )
    doneObject(value);

  return rval;
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_create[] =
        { "from=object*", "to=object*" };
static char *T_fromAobject_toAobject[] =
        { "from=object", "to=object" };
static char *T_initialise[] =
        { "selector1=name", "selector2=[name]" };

/* Instance Variables */

static vardecl var_identity[] =
{ IV(NAME_from, "selector1=name", IV_BOTH,
     NAME_selector, "Attribute at the `From' side"),
  IV(NAME_to, "selector2=name", IV_BOTH,
     NAME_selector, "Attribute at the `To' side")
};

/* Send Methods */

static senddecl send_identity[] =
{ SM(NAME_backwards, 2, T_fromAobject_toAobject, backwardsIdentity,
     DEFAULT, "Update after `from' object has changed"),
  SM(NAME_create, 2, T_create, createIdentity,
     DEFAULT, "Update after instantiation"),
  SM(NAME_forwards, 2, T_fromAobject_toAobject, forwardsIdentity,
     DEFAULT, "Update after `to' object has changed"),
  SM(NAME_initialise, 2, T_initialise, initialiseIdentity,
     DEFAULT, "Create from attribute names")
};

/* Get Methods */

#define get_identity NULL
/*
static getdecl get_identity[] =
{
};
*/

/* Resources */

#define rc_identity NULL
/*
static classvardecl rc_identity[] =
{
};
*/

/* Class Declaration */

static Name identity_termnames[] = { NAME_from, NAME_to };

ClassDecl(identity_decls,
          var_identity, send_identity, get_identity, rc_identity,
          2, identity_termnames,
          "$Rev$");

status
makeClassIdentity(Class class)
{ return declareClass(class, &identity_decls);
}

