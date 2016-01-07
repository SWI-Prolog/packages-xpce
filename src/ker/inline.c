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

#ifdef PCE_INCLUDED
#if O_INLINE && INLINE_UTILITIES
#define INLINE static inline
#define USE_INLINE 1
#endif
#else
#include <h/kernel.h>
#define INLINE
#define USE_INLINE 1
#endif

#include <h/trace.h>

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This file defines various time-critical general purpose-functions.  Time
critical modules may wish to include this file in the following way:
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#if USE_INLINE

		/********************************
		*     OBJECT MANIPULATIONS	*
		********************************/

INLINE status instanceOfObject(const Any obj, const Class super) PURE_FUNCTION;
INLINE status objectIsInstanceOf(const Any obj, const Class super) PURE_FUNCTION;

INLINE status
instanceOfObject(const Any obj, const Class super)
{ if ( isObject(obj) )
  { Class class = classOfObject(obj);

    return class == super || (class->tree_index >= super->tree_index &&
			      class->tree_index <  super->neighbour_index);
  }

  fail;
}


INLINE status
objectIsInstanceOf(const Any obj, const Class super)
{ const Class class = classOfObject(obj);

  return class == super || (class->tree_index >= super->tree_index &&
			    class->tree_index <  super->neighbour_index);
}


INLINE status
isProperObject(const Any obj)
{ return (obj && isAddress(obj) && hasObjectMagic(obj));
}


		/********************************
		*           HASHTABLES		*
		********************************/


INLINE Any
getMemberHashTable(const HashTable ht, const Any name)
{ int hashkey = hashKey(name, ht->buckets);
  Symbol s = &ht->symbols[hashkey];

  COUNT(hash_lookups++);

  for(;;)
  { if ( s->name == name )
      return s->value;
    if ( !s->name )
      fail;
    COUNT(hash_cmp_failed++);
    if ( ++hashkey == ht->buckets )
    { hashkey = 0;
      s = ht->symbols;
    } else
      s++;
  }

  fail;
}


		 /*******************************
		 *     REFERENCES FROM CODE	*
		 *******************************/

INLINE void
unallocObject(Any obj)
{ unalloc(valInt(classOfObject(obj)->instance_size), obj);
}


INLINE void
addCodeReference(Any obj)
{ Instance i = obj;

  i->references += ONE_CODE_REF;
}


INLINE void
delCodeReference(Any obj)
{ Instance i = obj;

  i->references -= ONE_CODE_REF;
  checkDeferredUnalloc(i);
}

		/********************************
		*             CODE		*
		********************************/

INLINE status
executeCode(Code c)
{ Class cl = classOfObject(c);
  status rval;

  addCodeReference(c);
  FixSendFunctionClass(cl, NAME_Execute);
  if ( onDFlag(c, D_SERVICE) )
  { ServiceMode(PCE_EXEC_SERVICE, rval = (*cl->send_function)(c));
  } else
    rval = (*cl->send_function)(c);
  delCodeReference(c);

  return rval;
}


INLINE status
forwardBlockv(Block b, int argc, const Any argv[])
{ status rval;

  if ( isNil(b->parameters) )
  { withArgs(argc, argv, rval = executeCode((Code) b));
  } else
  { withLocalVars({ int i;
		    Var *vars = (Var *) b->parameters->elements;
		    int nvars = valInt(b->parameters->size);

		    for(i=0; i<argc; i++)
		    { if ( i < nvars )
			assignVar(vars[i], argv[i], DEFAULT);
		      else
			assignVar(Arg(i-nvars+1), argv[i], DEFAULT);
		    }
		    rval = executeCode((Code) b);
		  });
  }

  return rval;
}


INLINE status
forwardCodev(Code c, int argc, const Any argv[])
{ status rval;

/*if ( instanceOfObject(c, ClassBlock) )*/
  if ( c->class == ClassBlock )
    return forwardBlockv((Block) c, argc, argv);

  withArgs(argc, argv, rval = executeCode(c));

  return rval;
}


		/********************************
		*            FUNCTIONS		*
		********************************/

INLINE Any
getExecuteFunction(Function f)
{ Class cl = classOfObject(f);
  Any rval;

  addCodeReference(f);
  FixGetFunctionClass(cl, NAME_Execute);
  if ( onDFlag(f, D_SERVICE) )
  { ServiceMode(PCE_EXEC_SERVICE, rval = (*cl->get_function)(f));
  } else
    rval = (*cl->get_function)(f);
  delCodeReference(f);

  return rval;
}


INLINE Any
expandCodeArgument(Any arg)
{ if ( isFunction(arg) )
    return getExecuteFunction(arg);

  return arg;
}


		/********************************
		*           CLASSES		*
		********************************/

#define RealiseClass(class) if ( (class)->realised != ON ) realiseClass(class)

INLINE Any
getSendMethodClass(Class class, Name name)
{ Any rval;

  RealiseClass(class);
  if ( !(rval = getMemberHashTable(class->send_table, name)) )
    rval = getResolveSendMethodClass(class, name);

  if ( notNil(rval) )
    answer(rval);

  fail;
}


INLINE Any
getGetMethodClass(Class class, Name name)
{ Any rval;

  RealiseClass(class);
  if ( !(rval = getMemberHashTable(class->get_table, name)) )
    rval = getResolveGetMethodClass(class, name);

  if ( notNil(rval) )
    answer(rval);

  fail;
}

		 /*******************************
		 *	      TYPES		*
		 *******************************/

INLINE Any
checkType(const Any val, const Type t, const Any ctx)
{ if ( validateType(t, val, ctx) )
    return val;

  return getTranslateType(t, val, ctx);
}


INLINE Name
checkSelector(Any sel)
{ if ( isName(sel) )
    return sel;

  return checkType(sel, TypeName, NIL);
}

#else /*USE_INLINE*/

COMMON(void)	unallocObject(Any obj);
COMMON(void)	addCodeReference(Any obj);
COMMON(void)	delCodeReference(Any obj);
COMMON(status)	instanceOfObject(const Any, const Class) PURE_FUNCTION;
COMMON(status)  objectIsInstanceOf(const Any obj, const Class super) PURE_FUNCTION;
COMMON(status)	isProperObject(const Any);
COMMON(Any)	getSendMethodClass(Class, Name);
COMMON(Any)	getGetMethodClass(Class, Name);
COMMON(Any)	getMemberHashTable(const HashTable, const Any);
COMMON(status)	executeCode(Code);
COMMON(Any)	getExecuteFunction(Function);
COMMON(status)	forwardCodev(Code, int, const Any[]);
COMMON(status)	forwardBlockv(Block, int, const Any[]);
COMMON(Any)	expandCodeArgument(Any);
COMMON(Any)	checkType(const Any val, const Type t, const Any ctx);
COMMON(Name)	checkSelector(Any sel);

/* Donot write below this line */
#endif /*USE_INLINE*/
