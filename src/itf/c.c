/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (C): 1985-2002, University of Amsterdam

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

#include <h/kernel.h>
#include "c.h"

typedef struct c_obj* CObj;

NewClass(c_obj)
  ABSTRACT_HOST				/* Class host */
End;


static status
initialiseC(CObj h)
{ initialiseHost((Host)h, CtoName("C"));
  assign(h, language, NAME_c);

  succeed;
}


static status
callCv(CObj host, CPointer function, int argc, Any *argv)
{ status rval;
  SendFunc f = (SendFunc) function->pointer;
  int n;

  for(n=0; n<argc; n++)
    if ( isObject(argv[n]) )
      addCodeReference(argv[n]);

  switch(argc)
  { case 0: rval = (*f)(); break;
    case 1: rval = (*f)(argv[0]); break;
    case 2: rval = (*f)(argv[0], argv[1]); break;
    case 3: rval = (*f)(argv[0], argv[1], argv[2]); break;
    case 4: rval = (*f)(argv[0], argv[1], argv[2], argv[3]); break;
    case 5: rval = (*f)(argv[0], argv[1], argv[2], argv[3], argv[4]); break;
    case 6: rval = (*f)(argv[0], argv[1], argv[2], argv[3], argv[4],
		        argv[5]); break;
    case 7: rval = (*f)(argv[0], argv[1], argv[2], argv[3], argv[4],
		        argv[5], argv[6]); break;
    case 8: rval = (*f)(argv[0], argv[1], argv[2], argv[3], argv[4],
		        argv[5], argv[6], argv[7]); break;
    case 9: rval = (*f)(argv[0], argv[1], argv[2], argv[3], argv[4],
		        argv[5], argv[6], argv[7], argv[8]); break;
    default:
      rval = errorPce(host, NAME_tooManyArguments, argc);
  }

  for(n=0; n<argc; n++)
    if ( isObject(argv[n]) && !isFreedObj(argv[n]) )
      delCodeReference(argv[n]);

  return rval ? SUCCEED : FAIL;
}


static Any
getCallCv(CObj host, CPointer function, int argc, Any *argv)
{ Any rval;
  GetFunc f = function->pointer;
  int n;

  for(n=0; n<argc; n++)
    if ( isObject(argv[n]) )
      addCodeReference(argv[n]);

  switch(argc)
  { case 0: rval = (*f)(); break;
    case 1: rval = (*f)(argv[0]); break;
    case 2: rval = (*f)(argv[0], argv[1]); break;
    case 3: rval = (*f)(argv[0], argv[1], argv[2]); break;
    case 4: rval = (*f)(argv[0], argv[1], argv[2], argv[3]); break;
    case 5: rval = (*f)(argv[0], argv[1], argv[2], argv[3], argv[4]); break;
    case 6: rval = (*f)(argv[0], argv[1], argv[2], argv[3], argv[4],
		        argv[5]); break;
    case 7: rval = (*f)(argv[0], argv[1], argv[2], argv[3], argv[4],
		        argv[5], argv[6]); break;
    case 8: rval = (*f)(argv[0], argv[1], argv[2], argv[3], argv[4],
		        argv[5], argv[6], argv[7]); break;
    case 9: rval = (*f)(argv[0], argv[1], argv[2], argv[3], argv[4],
		        argv[5], argv[6], argv[7], argv[8]); break;
    default:
      errorPce(host, NAME_tooManyArguments, argc);
      rval = FAIL;
  }

  for(n=0; n<argc; n++)
    if ( isObject(argv[n]) && !isFreedObj(argv[n]) )
      delCodeReference(argv[n]);

  return rval;
}


status
makeClassC(Class class)
{ sourceClass(class, makeClassC, __FILE__, "$Revision$");

  sendMethod(class, NAME_initialise, DEFAULT, 0,
	     "Create C interface",
	     initialiseC);
  sendMethod(class, NAME_call, NAME_callback, 2,
	     "c_pointer", "unchecked ...",
	     "Invoke a C-function send_method",
	     callCv);

  getMethod(class, NAME_call, NAME_callback, "unchecked", 2,
	    "c_pointer", "unchecked ...",
	    "Invoke a C-function get_method",
	    getCallCv);

  initClass(class);

  succeed;
}


		 /*******************************
		 *	  PUBLIC FUNCTIONS	*
		 *******************************/

void
XPCE_initialise()
{ if ( !XPCE_initialised )
    pceInitialise(0, NULL, 0, NULL);
}

					/* C ---> XPCE */

XPCE_Object
XPCE_to_string(char *text)
{ if ( text )
    return CtoString(text);

  fail;
}


XPCE_Object
XPCE_to_tmp_char_array(char *text)
{ if ( text )
    return CtoScratchCharArray(text);

  fail;
}


void
XPCE_done_tmp_char_array(XPCE_Object ca)
{ if ( ca )
    doneScratchCharArray(ca);
}


XPCE_Object
XPCE_to_name(char *text)
{ if ( text )
  { XPCE_initialise();			/* Bah, not other way? */
    return CtoName(text);
  }

  fail;
}


XPCE_Object
XPCE_to_integer(long value)
{ if ( value >= PCE_MIN_INT && value <= PCE_MAX_INT )
    return toInt(value);

  errorPce(PCE, NAME_intOutOfRange);
  fail;
}


XPCE_Object
XPCE_to_real(float value)
{ return CtoReal(value);
}


XPCE_Object
XPCE_to_pointer(void *ptr)
{ return answerObjectv(ClassCPointer, 1, &ptr);
}


XPCE_Object
XPCE_to_object(XPCE_Object name)
{ if ( isName(name) )
    return getObjectFromReferencePce(PCE, name);

  fail;
}


XPCE_Object
XPCE_to_class(XPCE_Object name)
{ if ( isName(name) )
    return getConvertClass(ClassClass, name);

  fail;
}

					/* XPCE ---> C */

char *
XPCE_charp_of(XPCE_Object string)
{ return toCharp(string);
}


long
XPCE_int_of(XPCE_Object integer)
{ Int i;

  if ( isInteger(integer) )
    return valInt(integer);
  else if ( (i = toInteger(integer)) )
    return valInt(i);

  errorPce(TypeInt, NAME_cannotConvert, integer);
  return 0L;
}


float
XPCE_float_of(XPCE_Object real)
{ Real r;

  if ( (r = toReal(getConvertReal(ClassReal, real))) )
    return valReal(r);

  errorPce(CtoType("real"), NAME_cannotConvert, real);
  return 0.0;	/*NaN;*/
}


void *
XPCE_pointer_of(XPCE_Object cptr)
{ if ( instanceOfObject(cptr, ClassCPointer) )
  { CPointer ptr = (CPointer)cptr;

    return ptr->pointer;
  }

  return NULL;
}


		 /*******************************
		 *	       VMI		*
		 *******************************/


#define CHECKARGV { int n; for(n=argc; --n>=0; ) if ( !argv[n] ) fail; }

XPCE_status
XPCE_sendv(XPCE_Object receiver, XPCE_Object selector,
	   int argc, const XPCE_Object argv[])
{ if ( receiver )
  { CHECKARGV;
    return vm_send(receiver, selector, NULL, argc, argv);
  }
  fail;
}


XPCE_Object
XPCE_getv(XPCE_Object receiver, XPCE_Object selector,
	   int argc, const XPCE_Object argv[])
{ if ( receiver )
  { CHECKARGV;
    return vm_get(receiver, selector, NULL, argc, argv);
  }
  fail;
}


XPCE_Object
XPCE_newv(XPCE_Object class, const XPCE_Object name,
	  int argc, const XPCE_Object argv[])
{ XPCE_Object rval;

  XPCE_initialise();
  CHECKARGV;
  rval = createObjectv(name ? name : (Name) NIL, class, argc, argv);
  if ( rval )
    pushAnswerObject(rval);

  return rval;
}



XPCE_status
XPCE_free(XPCE_Object object)
{ return freeObject(object);
}


					/* va-arg versions */

XPCE_status
XPCE_send(XPCE_Object receiver, XPCE_Object selector, ...)
{ va_list args;
  Any argv[VA_PCE_MAX_ARGS];
  int argc;

  va_start(args, selector);
  for(argc=0; (argv[argc] = va_arg(args, Any)) != XPCE_END; argc++)
  { if ( argc > VA_PCE_MAX_ARGS )
    { va_end(args);
      return errorPce(receiver, NAME_badCArgList, CtoName("->"), selector);
    }
  }
  va_end(args);

  return XPCE_sendv(receiver, selector, argc, argv);
}


XPCE_Object
XPCE_get(XPCE_Object receiver, XPCE_Object selector, ...)
{ va_list args;
  Any argv[VA_PCE_MAX_ARGS];
  int argc;

  va_start(args, selector);
  for(argc=0; (argv[argc] = va_arg(args, Any)) != XPCE_END; argc++)
  { if ( argc > VA_PCE_MAX_ARGS )
    { va_end(args);
      errorPce(receiver, NAME_badCArgList, CtoName("<-"), selector);
      fail;
    }
  }
  va_end(args);

  return XPCE_getv(receiver, selector, argc, argv);
}


XPCE_Object
XPCE_new(XPCE_Object class, const XPCE_Object name, ...)
{ va_list args;
  Any argv[VA_PCE_MAX_ARGS];
  int argc;

  va_start(args, name);
  for(argc=0; (argv[argc] = va_arg(args, Any)) != XPCE_END; argc++)
  { if ( argc > VA_PCE_MAX_ARGS )
    { va_end(args);
      errorPce(class, NAME_badCArgList, CtoName("<-"), NAME_instance);
      fail;
    }
  }
  va_end(args);

  return XPCE_newv(class, name, argc, argv);
}


		 /*******************************
		 *	      FUNCALL		*
		 *******************************/

XPCE_Object
XPCE_CHost(void)
{ static XPCE_Object me = NULL;

  if ( !me )
  { me = globalObject(NAME_c, ClassC, EAV);
    protectObject(me);
  }

  return me;
}


XPCE_Object
XPCE_callv(XPCE_Procedure function, int argc, const XPCE_Object argv[])
{ ArgVector(av, argc+3);
  int i;

  av[0] = XPCE_CHost();
  av[1] = NAME_call;
  av[2] = CtoCPointer(function);
  for(i=0; i<argc; i++)
    av[i+3] = argv[i];

  return answerObjectv(ClassMessage, argc+3, av);
}


XPCE_Object
XPCE_funcallv(XPCE_Function function, int argc, const XPCE_Object argv[])
{ ArgVector(av, argc+3);
  int i;

  av[0] = XPCE_CHost();
  av[1] = NAME_call;
  av[2] = CtoCPointer(function);
  for(i=0; i<argc; i++)
    av[i+3] = argv[i];

  return answerObjectv(ClassObtain, argc+3, av);
}


XPCE_Object
XPCE_call(XPCE_Procedure function, ...)
{ va_list args;
  Any argv[VA_PCE_MAX_ARGS];
  int argc;

  va_start(args, function);
  for(argc=0; (argv[argc] = va_arg(args, Any)) != XPCE_END; argc++)
  { if ( argc > VA_PCE_MAX_ARGS )
    { va_end(args);
      errorPce(XPCE_CHost(), NAME_badCArgList, CtoName("->"), NAME_call);
      fail;
    }
  }
  va_end(args);

  return XPCE_callv(function, argc, argv);
}


XPCE_Object				/* ? */
XPCE_funcall(XPCE_Function function, ...)
{ va_list args;
  Any argv[VA_PCE_MAX_ARGS];
  int argc;

  va_start(args, function);
  for(argc=0; (argv[argc] = va_arg(args, Any)) != XPCE_END; argc++)
  { if ( argc > VA_PCE_MAX_ARGS )
    { va_end(args);
      errorPce(XPCE_CHost(), NAME_badCArgList, CtoName("<-"), NAME_call);
      fail;
    }
  }
  va_end(args);

  return XPCE_funcallv(function, argc, argv);
}

		 /*******************************
		 *	     CLASSES		*
		 *******************************/

XPCE_Object
XPCE_defclass(XPCE_Object name, XPCE_Object super, XPCE_Object summary,
	      XPCE_Procedure makefunc)
{ if ( name && super && summary && makefunc )
  { Class class;

    if ( (class = defineClass(name, super, summary, (SendFunc)makefunc)) )
    { numberTreeClass(ClassObject, 0);
      answer(class);
    }
  }

  fail;
}


XPCE_Object
XPCE_defcxxclass(XPCE_Object name, XPCE_Object super, XPCE_Object summary,
		 XPCE_Procedure makefunc)
{ if ( name && super && summary && makefunc )
  { Class class;

    if ( (class = defineClass(name, super, summary, (SendFunc)makefunc)) )
    { setDFlag(class, D_CXX);
      assign(class, creator, name_cxx);
      numberTreeClass(ClassObject, 0);
      answer(class);
    }
  }

  fail;
}



XPCE_Object				/* class */
XPCE_makeclass(XPCE_Object name, XPCE_Object super, XPCE_Object summary)
{ Class class, superclass;

  if ( !(superclass = getConvertClass(ClassClass, super)) )
  { errorPce(name, NAME_noSuperClass, super);
    fail;
  }

  TRY(class = newObject(superclass->class, name, superclass, EAV));

  if ( instanceOfObject(summary, ClassCharArray) )
    assign(class, summary, summary);

  return class;
}


XPCE_Variable
XPCE_defvar(XPCE_Object class,
	    XPCE_Object name, XPCE_Object group, XPCE_Object summary,
	    XPCE_Object type, XPCE_Object access, XPCE_Object initial)
{ Variable var;

  if ( !instanceOfObject(summary, ClassCharArray) )
    summary = DEFAULT;
  if ( !instanceOfObject(group, ClassCharArray) )
    group = DEFAULT;
  if ( !validPceDatum(initial) )
    initial = NIL;
  if ( !(type = checkType(type, TypeType, NIL)) )
    type = TypeAny;

  var = newObject(ClassObjOfVariable, name, type, access, summary, group, EAV);
  initialValueVariable(var, initial);
  TRY(instanceVariableClass(class, var));

  return var;
}


static XPCE_Object
NoCode()
{ static XPCE_Object me = NULL;

  if ( !me )
  { me = newObject(ClassAnd, EAV);
    protectObject(me);
  }

  return me;
}


XPCE_status
XPCE_defsendmethodv(XPCE_Object class,
		    XPCE_Object name, XPCE_Object group, XPCE_Object summary,
		    XPCE_Procedure implementation,
		    int argc, const XPCE_Object types[])
{ SendMethod method;

  if ( !instanceOfObject(summary, ClassCharArray) )
    summary = DEFAULT;
  if ( !instanceOfObject(group, ClassCharArray) )
    group = DEFAULT;

  method = newObject(ClassSendMethod, name,
		     newObjectv(ClassVector, argc, types),
		     NoCode(),		/* hack to avoid type-conflict */
		     summary, DEFAULT, group, EAV);
  assign(method, message, NIL);
  setDFlag(method, D_CXX);
  method->function = (SendFunc)implementation;

  return sendMethodClass(class, method);
}


XPCE_status
XPCE_defgetmethodv(XPCE_Object class,
		   XPCE_Object name, XPCE_Object group, XPCE_Object summary,
		   XPCE_Object return_type, XPCE_Function implementation,
		   int argc, const XPCE_Object types[])
{ GetMethod method;

  if ( !instanceOfObject(summary, ClassCharArray) )
    summary = DEFAULT;
  if ( !instanceOfObject(group, ClassCharArray) )
    group = DEFAULT;

  method = newObject(ClassGetMethod, name, return_type,
		     newObjectv(ClassVector, argc, types),
		     Arg(1),		/* hack to avoid type-conflict */
		     summary, DEFAULT, group, EAV);
  assign(method, message, NIL);
  setDFlag(method, D_CXX);
  method->function = (Func)implementation;

  return getMethodClass(class, method);
}


XPCE_status
XPCE_store(XPCE_Object in, XPCE_Variable var, XPCE_Object value)
{ if ( !in || !value )
    fail;

  return sendVariable(var, in, value);
}


XPCE_Object
XPCE_fetch(XPCE_Object in, XPCE_Variable var)
{ if ( !in )
    fail;

  return getGetVariable(var, in);
}

		 /*******************************
		 *	       CHAINS		*
		 *******************************/

XPCE_Object
XPCE_chain_head(XPCE_Object chain)
{ if ( instanceOfObject(chain, ClassChain) )
  { Chain ch = chain;

    return (XPCE_Object) (notNil(ch->head) ? ch->head : NULL);
  }

  fail;					/* error? */
}


XPCE_Object
XPCE_next_cell(XPCE_Object cell)
{ if ( cell )
  { XPCE_Object next = ((Cell)cell)->next;

    return notNil(next) ? next : NULL;
  }

  fail;
}


XPCE_Object
XPCE_cell_value(XPCE_Object cell)
{ if ( cell )
    return ((Cell)cell)->value;

  fail;
}

		 /*******************************
		 *	       GLOBALS		*
		 *******************************/

Any XPCE_on;
Any XPCE_off;
Any XPCE_nil;
Any XPCE_default;
Any XPCE_arg1;
Any XPCE_arg2;
Any XPCE_arg3;
Any XPCE_arg4;
Any XPCE_arg5;
Any XPCE_arg6;
Any XPCE_arg7;
Any XPCE_arg8;
Any XPCE_arg9;
Any XPCE_arg10;
Any XPCE_event;
Any XPCE_receiver;
Any XPCE_pce;
Any XPCE_display;

void
initCGlobals()
{ XPCE_on	= ON;
  XPCE_off	= OFF;
  XPCE_nil	= NIL;
  XPCE_default	= DEFAULT;
  XPCE_arg1	= Arg(1);
  XPCE_arg2	= Arg(2);
  XPCE_arg3	= Arg(3);
  XPCE_arg4	= Arg(4);
  XPCE_arg5	= Arg(5);
  XPCE_arg6	= Arg(6);
  XPCE_arg7	= Arg(7);
  XPCE_arg8	= Arg(8);
  XPCE_arg9	= Arg(9);
  XPCE_arg10	= Arg(10);
  XPCE_event	= EVENT;
  XPCE_receiver = RECEIVER;
  XPCE_pce	= PCE;
  XPCE_display  = findGlobal(NAME_display);
}

		 /*******************************
		 *	    C++ SUPPORT		*
		 *******************************/

#if O_CPLUSPLUS

XPCE_Object
XPCE_callCPlusPlusMethodv(XPCE_Procedure function, void *obj,
			  int argc, const XPCE_Object argv[])
{ ArgVector(av, argc+4);
  int i;

  av[0] = XPCE_CHost();
  av[1] = NAME_callCPlusPlusMethod;
  av[2] = CtoCPointer(function);
  av[3] = CtoCPointer(obj);
  for(i=0; i<argc; i++)
    av[i+4] = argv[i];

  return answerObjectv(ClassMessage, argc+4, av);
}


XPCE_Object
XPCE_funcallCPlusPlusMethodv(XPCE_Function function, void *obj,
			     int argc, const XPCE_Object argv[])
{ ArgVector(av, argc+4);
  int i;

  av[0] = XPCE_CHost();
  av[1] = NAME_callCPlusPlusMethod;
  av[2] = CtoCPointer(function);
  av[3] = CtoCPointer(obj);
  for(i=0; i<argc; i++)
    av[i+4] = argv[i];

  return answerObjectv(ClassObtain, argc+4, av);
}


XPCE_Object
XPCE_callCPlusPlusv(XPCE_Procedure f, int argc, const XPCE_Object argv[])
{ ArgVector(av, argc+3);
  int i;

  av[0] = XPCE_CHost();
  av[1] = NAME_callCPlusPlus;
  av[2] = CtoCPointer(f);
  for(i=0; i<argc; i++)
    av[i+3] = argv[i];

  return answerObjectv(ClassMessage, argc+3, av);
}


XPCE_Object
XPCE_funcallCPlusPlusv(XPCE_Function f, int argc, const XPCE_Object argv[])
{ ArgVector(av, argc+3);
  int i;

  av[0] = XPCE_CHost();
  av[1] = NAME_callCPlusPlus;
  av[2] = CtoCPointer(f);
  for(i=0; i<argc; i++)
    av[i+3] = argv[i];

  return answerObjectv(ClassObtain, argc+3, av);
}


#endif /*O_CPLUSPLUS*/
