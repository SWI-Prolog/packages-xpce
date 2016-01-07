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

#define INLINE_UTILITIES 1
#include <h/kernel.h>


GetMethod
createGetMethod(Name name, Type rtype, Vector types,
		StringObj doc, Func action)
{ GetMethod m = alloc(sizeof(struct get_method));

  initHeaderObj(m, ClassGetMethod);
  m->return_type = (Type) NIL;
  assign(m, return_type, rtype);
  createMethod((Method) m, name, types, doc, action);

  return m;
}


status
initialiseGetMethod(GetMethod m, Name name, Type rtype,
		    Vector types, Function msg,
		    StringObj doc, SourceLocation loc, Name group)
{ if ( isDefault(rtype) )
    rtype = TypeUnchecked;

  TRY(initialiseMethod((Method) m, name, types, (Code) msg, doc, loc, group));
  assign(m, return_type, rtype);

  succeed;
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declaractions */

static char *T_initialise[] =
        { "name=name", "return=[type]", "types=[vector]",
	  "implementation=function|c_pointer",
	  "summary=[string]*",
	  "source=[source_location]*", "group=[name]*" };
static char *T_get[] =
        { "receiver=object", "argument=unchecked ..." };

/* Instance Variables */

static vardecl var_getMethod[] =
{ IV(NAME_returnType, "type", IV_GET,
     NAME_type, "Type of value returned")
};

/* Send Methods */

static senddecl send_getMethod[] =
{ SM(NAME_initialise, 7, T_initialise, initialiseGetMethod,
     DEFAULT, "->selector, return_type, types, msg, doc, location")
};

/* Get Methods */

static getdecl get_getMethod[] =
{ GM(NAME_get, 2, "value=unchecked", T_get, getGetGetMethod,
     NAME_execute, "Invoke get-method")
};

/* Resources */

#define rc_getMethod NULL
/*
static classvardecl rc_getMethod[] =
{
};
*/

/* Class Declaration */

static Name getMethod_termnames[] = { NAME_name, NAME_returnType, NAME_types,
				      NAME_summary
				    };

ClassDecl(getMethod_decls,
          var_getMethod, send_getMethod, get_getMethod, rc_getMethod,
          4, getMethod_termnames,
          "$Rev$");


status
makeClassGetMethod(Class class)
{ return declareClass(class, &getMethod_decls);
}
