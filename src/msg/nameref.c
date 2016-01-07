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

typedef struct assoc *Assoc;

NewClass(assoc)
  ABSTRACT_CODE
  Name	reference;			/* reference of the object */
  Any	object;				/* Object to reference */
End;


static status
initialiseAssoc(Assoc a, Name name, Any object)
{ initialiseCode((Code) a);

  assign(a, reference, name);
  assign(a, object,    object);

  succeed;
}


static status
ExecuteAssoc(Assoc a)
{ return nameReferenceObject(a->object, a->reference);
}

		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_initialise[] =
        { "reference=name", "object=object|function" };

/* Instance Variables */

static vardecl var_assoc[] =
{ IV(NAME_reference, "name", IV_BOTH,
     NAME_reference, "Reference to give to the object"),
  IV(NAME_object, "object|function", IV_BOTH,
     NAME_reference, "Object to assign reference")
};

/* Send Methods */

static senddecl send_assoc[] =
{ SM(NAME_Execute, 0, NULL, ExecuteAssoc,
     DEFAULT, "Assign the reference"),
  SM(NAME_initialise, 2, T_initialise, initialiseAssoc,
     DEFAULT, "Create from reference and object")
};

/* Get Methods */

#define get_assoc NULL
/*
static getdecl get_assoc[] =
{
};
*/

/* Resources */

#define rc_assoc NULL
/*
static classvardecl rc_assoc[] =
{
};
*/

/* Class Declaration */

static Name assoc_termnames[] = { NAME_reference, NAME_object };

ClassDecl(assoc_decls,
          var_assoc, send_assoc, get_assoc, rc_assoc,
          2, assoc_termnames,
          "$Rev$");


status
makeClassAssoc(Class class)
{ return declareClass(class, &assoc_decls);
}

