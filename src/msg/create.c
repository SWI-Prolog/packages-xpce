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
initialiseCreatev(Create c, Class class, int argc, const Any argv[])
{ initialiseFunction((Function) c);

  assign(c, c_class, class);
  if ( argc > 0 )
    assign(c, arguments, newObjectv(ClassCodeVector, argc, argv));

  succeed;
}


static Int
getArityCreate(Create c)
{ if ( isNil(c->arguments) )
    answer(ONE);
  else
    answer(add(c->arguments->size, ONE));
}


static Any
getArgCreate(Create c, Int arg)
{ int n = valInt(arg);

  switch(n)
  { case 1:	answer(c->c_class);
    default:	if ( n < 1 || n > valInt(getArityCreate(c)) )
		  fail;
                answer(c->arguments->elements[n-2]);
  }
}


static status
argumentCreate(Create c, Int n, Any val)
{ if ( valInt(n) < 1 )
    fail;

  if ( isNil(c->arguments) )
    assign(c, arguments, newObject(ClassVector, EAV));

  return elementVector(c->arguments, n, val);
}


static Any
getArgumentCreate(Create c, Int n)
{ if ( valInt(n) < 1 )
    fail;

  if ( isNil(c->arguments) )
    fail;

  return getElementVector(c->arguments, n);
}


static Any
getExecuteCreate(Create c)
{ if ( !instanceOfObject(c->c_class, ClassClass) )
  { Class class;

    if ( !(class = getConvertClass(ClassClass, c->c_class)) )
    { errorPce(c, NAME_noClass, class);
      fail;
    }
    assign(c, c_class, class);
  }

  if ( isNil(c->arguments) )
    return answerObjectv(c->c_class, 0, NULL);
  else
  { int n;
    int argc = valInt(c->arguments->size);
    ArgVector(argv, argc);
    Any *elms = c->arguments->elements;

    for(n = 0; n < argc; n++)
      TRY(argv[n] = expandCodeArgument(elms[n]));

    return answerObjectv(c->c_class, argc, argv);
  }
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_initialise[] =
        { "class=name|class", "argument=any|function ..." };
static char *T_argument[] =
        { "index=int", "value=any|function" };

/* Instance Variables */

static vardecl var_create[] =
{ IV(NAME_class, "name|class", IV_BOTH,
     NAME_class, "Class (name) to create instance of"),
  IV(NAME_argument, "code_vector*", IV_BOTH,
     NAME_argument, "Arguments used to create instance")
};

/* Send Methods */

static senddecl send_create[] =
{ SM(NAME_initialise, 2, T_initialise, initialiseCreatev,
     DEFAULT, "Create from class (name) and arguments"),
  SM(NAME_argument, 2, T_argument, argumentCreate,
     NAME_argument, "Set nth-1 argument")
};

/* Get Methods */

static getdecl get_create[] =
{ GM(NAME_Arg, 1, "any|function", "int", getArgCreate,
     DEFAULT, "Nth-1 argument for term description"),
  GM(NAME_Arity, 0, "int", NULL, getArityCreate,
     DEFAULT, "Arity for term description"),
  GM(NAME_Execute, 0, "unchecked", NULL, getExecuteCreate,
     DEFAULT, "Create instance and return it"),
  GM(NAME_argument, 1, "value=any|function", "index=int", getArgumentCreate,
     NAME_argument, "Nth-1 argument")
};

/* Resources */

#define rc_create NULL
/*
static classvardecl rc_create[] =
{
};
*/

/* Class Declaration */

ClassDecl(create_decls,
          var_create, send_create, get_create, rc_create,
          ARGC_UNKNOWN, NULL,
          "$Rev$");

status
makeClassCreate(Class class)
{ return declareClass(class, &create_decls);
}


