/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  1985-2025, University of Amsterdam
			      SWI-Prolog Solutions b.v.
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

status
initialiseFunction(Function f)
{ setFlag(f, F_ACTIVE|F_NOTANY);

  return initialiseCode((Code) f);
}


static Function
getConvertFunction(Class class, QuoteFunction q)
{ answer(q->function);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
				FORWARDING
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

Any
getForwardFunctionv(Function f, int argc, const Any argv[])
{ Any rval;

  withArgs(argc, argv, rval = getExecuteFunction(f));

  return rval;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		      FORWARDING WITH PUSH OF @RECEIVER

The test of `if ( RECEIVER->value != receiver )' is dubious: we should
check whether the message actually is send to @receiver
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

Any
getForwardReceiverFunctionv(Function f, Any receiver,
			    int argc, const Any argv[])
{ if ( getValueVar(RECEIVER) != receiver )
  { Class rclass = classOfObject(receiver);
    Any rval;

    withReceiver(receiver, rclass,
		 rval = getForwardFunctionv(f, argc, argv));

    return rval;
  } else
    return getForwardFunctionv(f, argc, argv);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			  ARGLIST FUNCTION INVOKATION
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

Any
getForwardFunction(Function f, ...)
{ va_list args;
  Any argv[VA_PCE_MAX_ARGS];
  int argc;

  va_start(args, f);
  for(argc=0; (argv[argc] = va_arg(args, Any)) != NULL; argc++)
    assert(argc <= VA_PCE_MAX_ARGS);
  va_end(args);

  return getForwardFunctionv(f, argc, argv);
}


Any
getForwardReceiverFunction(Function f, Any receiver, ...)
{ va_list args;
  Any argv[VA_PCE_MAX_ARGS];
  int argc;

  va_start(args, receiver);
  for(argc=0; (argv[argc] = va_arg(args, Any)) != NULL; argc++)
    assert(argc <= VA_PCE_MAX_ARGS);
  va_end(args);

  return getForwardReceiverFunctionv(f, receiver, argc, argv);
}

static Any
getNoExecuteFunction(Function f)
{ errorPce(f, NAME_cannotExecute);

  fail;
}


Any
getSendMethodFunction(Function f, Name selector)
{ Behaviour b;

  if ( (b = getMemberHashTable(classOfObject(f)->send_table,
			       selector)) &&
       notNil(b) &&
       isAClass(b->context, ClassFunction) )
    answer(b);

  fail;
}


Any
getGetMethodFunction(Function f, Name selector)
{ Behaviour b;

  if ( (b = getMemberHashTable(classOfObject(f)->get_table,
			       selector)) &&
       notNil(b) &&
       isAClass(b->context, ClassFunction) )
    answer(b);

  fail;
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_Error[] =
        { "error=error", "context=any ..." };
static char *T_convertLoadedObject[] =
        { "int", "int" };
static char *T_sendSuper[] =
        { "selector=name", "argument=unchecked ..." };

/* Instance Variables */

#define var_function NULL
/*
vardecl var_function[] =
{
};
*/

/* Send Methods */

static senddecl send_function[] =
{ SM(NAME_Check, 1, "[bool]", CheckObject,
     DEFAULT, "Check obtainer instead of result"),
  SM(NAME_Free, 0, NULL, freeObject,
     DEFAULT, "Free obtainer instead of result"),
#ifndef O_RUNTIME
  SM(NAME_Inspect, 1, "bool", inspectObject,
     DEFAULT, "Inspect obtainer instead of result"),
#endif
  SM(NAME_InstanceOf, 1, "class", instanceOfObject,
     DEFAULT, "Test class of obtainer instead of result"),
  SM(NAME_convertLoadedObject, 2, T_convertLoadedObject, convertLoadedObjectObject,
     DEFAULT, "Called by File <-object if conversion might be required"),
  SM(NAME_initialise, 0, NULL, initialiseFunction,
     DEFAULT, "Initialise function"),
  SM(NAME_initialiseNewSlot, 1, "variable", initialiseNewSlotProgramObject,
     DEFAULT, "Initialise <-dflags"),
  SM(NAME_SameReference, 1, "to=any|function", sameReferenceObject,
     NAME_function, "Test equivalence instead of result"),
  SM(NAME_sendSuper, 2, T_sendSuper, sendSuperObject,
     NAME_programming, "Send using method of super-class"),
  SM(NAME_NameReference, 1, "name*", nameReferenceObject,
     NAME_reference, "Change named (atomic) reference"),
  SM(NAME_Error, 2, T_Error, errorObjectv,
     NAME_report, "Initiate an error")
};

/* Get Methods */

static getdecl get_function[] =
{ GM(NAME_Arg, 1, "unchecked", "int", getArgObject,
     DEFAULT, "Nth-1 argument of term instead of result's"),
  GM(NAME_Arity, 0, "int", NULL, getArityObject,
     DEFAULT, "Arity of term instead of result's"),
  GM(NAME_Class, 0, "class", NULL, getClassObject,
     DEFAULT, "Class instead of result's"),
  GM(NAME_ClassName, 0, "name", NULL, getClassNameObject,
     DEFAULT, "Class name instead of result's"),
  GM(NAME_Flags, 0, "name", NULL, getFlagsObject,
     DEFAULT, "Flags instead of result's"),
#ifndef O_RUNTIME
  GM(NAME_Inspect, 0, "bool", NULL, getInspectObject,
     DEFAULT, "Inspect instead of result's"),
  GM(NAME_ManId, 0, "name", NULL, getManIdObject,
     DEFAULT, "Manual identifier instead of result's"),
#endif
  GM(NAME_References, 0, "int", NULL, getReferencesObject,
     DEFAULT, "Reference count instead of result's"),
  GM(NAME_Slot, 1, "any", "name|int", getSlotObject,
     DEFAULT, "Slot value instead of result's"),
  GM(NAME_convert, 1, "function", "quote=quote_function", getConvertFunction,
     DEFAULT, "Convert quoted function to quoted value"),
  GM(NAME_functor, 0, "name", NULL, getFunctorObject,
     DEFAULT, "Functor of term instead of result's"),
  GM(NAME_storageReference, 0, "any", NULL, getFailObject,
     DEFAULT, "Description name for ->save_in_file"),
  GM(NAME_Execute, 0, "unchecked", NULL, getNoExecuteFunction,
     NAME_execute, "Execute and return value"),
  GM(NAME_Forward, 1, "unchecked", "any ...", getForwardFunctionv,
     NAME_execute, "Push @arg1, ...; execute and return value"),
  GM(NAME_execute, 0, "unchecked", NULL, getExecuteFunction,
     NAME_execute, "Execute the function object (yielding value)"),
  GM(NAME_forward, 1, "unchecked", "any ...", getForwardFunctionv,
     NAME_execute, "Push @arg1, ...; execute and return value")
};

/* Resources */

#define rc_function NULL
/*
static classvardecl rc_function[] =
{
};
*/

/* Class Declaration */

ClassDecl(function_decls,
          var_function, send_function, get_function, rc_function,
          0, NULL,
          "$Rev$");

status
makeClassFunction(Class class)
{ return declareClass(class, &function_decls);
}
