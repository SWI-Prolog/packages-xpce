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
initialiseBool(BoolObj b)
{ return errorPce(classOfObject(b), NAME_cannotCreateInstances);
}


static status
unlinkBool(BoolObj b)
{ fail;					/* should not happen! */
}


static BoolObj
getConvertBool(Class class, Any obj)
{ answer(toBool(obj));
}


static BoolObj
getNegateBool(BoolObj b)
{ answer(b == ON ? OFF : ON);
}

		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declaractions */


/* Instance Variables */

#define var_bool NULL
/*
static vardecl var_bool[] =
{
};
*/

/* Send Methods */

static senddecl send_bool[] =
{ SM(NAME_initialise, 0, NULL, initialiseBool,
     DEFAULT, "Create bool (cannot be created)"),
  SM(NAME_unlink, 0, NULL, unlinkBool,
     DEFAULT, "Destroy boolean (cannot be done)")
};

/* Get Methods */

static getdecl get_bool[] =
{ GM(NAME_convert, 1, "bool", "any", getConvertBool,
     DEFAULT, "Converts true, false and integer"),
  GM(NAME_negate, 0, "bool", NULL, getNegateBool,
     NAME_calculate, "Maps @on <-> @off")
};

/* Resources */

#define rc_bool NULL
/*
static classvardecl rc_bool[] =
{
};
*/

/* Class Declaration */

static Name bool_termnames[] = { NAME_self };

ClassDecl(bool_decls,
          var_bool, send_bool, get_bool, rc_bool,
          1, bool_termnames,
          "$Rev$");


status
makeClassBool(Class class)
{ declareClass(class, &bool_decls);

  saveStyleClass(class, NAME_external);
  cloneStyleClass(class, NAME_none);

  ON->class = OFF->class = class;
  newAssoc(NAME_on,  ON);
  newAssoc(NAME_off, OFF);

  ON->name = OFF->name = NIL;
  ON->summary = OFF->summary = NIL;
  assign(ON, name, NAME_on);
  assign(OFF, name, NAME_off);
  assign(ON, summary, staticCtoString("Boolean true"));
  assign(OFF, summary, staticCtoString("Boolean false"));

  succeed;
}

