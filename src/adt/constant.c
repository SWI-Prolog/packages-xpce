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
initialiseConstant(Constant c, Name name, StringObj summary)
{ protectObject(c);

  assign(c, name, name);
  if ( notDefault(summary) )
    assign(c, summary, summary);

  succeed;
}


static void
mkconstant(Class class, Constant c, Name name, const char *summary)
{ initHeaderObj(c, class);
  setProtectedObj(c);
  clearCreatingObj(c);

  assign(c, name,    name);
  assign(c, summary, staticCtoString(summary));

  newAssoc(name, c);
}


status
makeClassConstant(Class class)
{ localClass(class, NAME_name, NAME_name, "name", NAME_both,
	     "Name of the constant");
  localClass(class, NAME_summary, NAME_manual, "string*", NAME_both,
	     "Short description");

  sourceClass(class, makeClassConstant, __FILE__, "$Revision$");
  termClass(class, "constant", 1, NAME_self);
  saveStyleClass(class, NAME_external);
  cloneStyleClass(class, NAME_none);

  sendMethod(class, NAME_initialise, DEFAULT, 2, "name=name", "summary=string",
	     "Create constant",
	     initialiseConstant);

  mkconstant(class,
	     NIL,
	     NAME_nil,
	     "Representation of not-filled, nothing");
  mkconstant(class,
	     DEFAULT,
	     NAME_default,
	     "Representation of default/optional");
  mkconstant(class,
	     CLASSDEFAULT,
	     NAME_classDefault,
	     "Use class-variable value");

  assign(class, no_created, toInt(3));

  succeed;
}

