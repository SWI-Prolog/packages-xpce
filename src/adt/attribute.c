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
#include <h/trace.h>

static status
initialiseAttribute(Attribute att, Any name, Any value)
{ initialiseProgramObject(att);

  assign(att, name, name);
  assign(att, value, value);

  succeed;
}


static Attribute
getConvertAttribute(Class class, Any name)
{ answer(answerObject(ClassAttribute, name, NIL, EAV));
}


static status
sendAttribute(Attribute att, Any rec, Any value)
{ assign(att, value, value);

  succeed;
}


static Any
getAttribute(Attribute att, Any rec)
{ return att->value;
}


		/********************************
		*            TRACING		*
		********************************/

static Type
getArgumentTypeAttribute(Attribute att, Int n)
{ if ( isDefault(n) || n == ONE )
    answer(TypeAny);

  fail;
}

		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declaractions */

static char *T_send[] =
        { "context=object", "value=any" };
static char *T_initialise[] =
        { "name=any", "value=any" };

/* Instance Variables */

static vardecl var_attribute[] =
{ IV(NAME_name, "any", IV_BOTH,
     NAME_storage, "Name of the attribute"),
  IV(NAME_value, "any", IV_BOTH,
     NAME_storage, "Value of the attribute")
};

/* Send Methods */

static senddecl send_attribute[] =
{ SM(NAME_initialise, 2, T_initialise, initialiseAttribute,
     DEFAULT, "Create attribute from name and value"),
  SM(NAME_send, 2, T_send, sendAttribute,
     NAME_execute, "Invoke (write) object-attribute")
};

/* Get Methods */

static getdecl get_attribute[] =
{ GM(NAME_convert, 1, "attribute", "any", getConvertAttribute,
     DEFAULT, "Converts name to attribute(name, @nil)"),
  GM(NAME_get, 1, "any", "object", getAttribute,
     NAME_execute, "Invoke (read) object-attribute"),
  GM(NAME_argumentType, 1, "type", "index=[int]", getArgumentTypeAttribute,
     NAME_meta, "Type of n-th1 argument")
};

/* Resources */

#define rc_attribute NULL
/*
static classvardecl rc_attribute[] =
{
};
*/

/* Class Declaration */

static Name attribute_termnames[] = { NAME_name, NAME_value };

ClassDecl(attribute_decls,
          var_attribute, send_attribute, get_attribute, rc_attribute,
          2, attribute_termnames,
          "$Rev$");


status
makeClassAttribute(Class class)
{ declareClass(class, &attribute_decls);

  succeed;
}

