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
createRelation(Relation r, Any from, Any to)
{ return send(r, NAME_forwards, from, to, EAV);
}

static status
ignoreReleation(Relation r, Any from, Any to)
{ succeed;
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_fromAobject_toAobject[] =
        { "from=object", "to=object" };

/* Instance Variables */

#define var_relation NULL
/*
vardecl var_relation[] =
{
};
*/

/* Send Methods */

static senddecl send_relation[] =
{ SM(NAME_backwards, 2, T_fromAobject_toAobject, ignoreReleation,
     NAME_constraint, "Called to update after a change of `to'"),
  SM(NAME_create, 2, T_fromAobject_toAobject, createRelation,
     NAME_constraint, "Called to initiate the relation"),
  SM(NAME_forwards, 2, T_fromAobject_toAobject, ignoreReleation,
     NAME_constraint, "Called to update after a change of `from'")
};

/* Get Methods */

#define get_relation NULL
/*
static getdecl get_relation[] =
{
};
*/

/* Resources */

#define rc_relation NULL
/*
static classvardecl rc_relation[] =
{
};
*/

/* Class Declaration */

ClassDecl(relation_decls,
          var_relation, send_relation, get_relation, rc_relation,
          0, NULL,
          "$Rev$");

status
makeClassRelation(Class class)
{ return declareClass(class, &relation_decls);
}
