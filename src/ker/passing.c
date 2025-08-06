/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org/packages/xpce/
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

#ifdef __WINDOWS__
#define USE_WIN32_CRITICAL_SECTION
#endif

#define _GNU_SOURCE 1			/* for recursive mutexes */
#define INLINE_UTILITIES 1
#include <h/kernel.h>
#include <h/trace.h>
#include <itf/c.h>

					/* Win32 native locking */
#ifdef USE_WIN32_CRITICAL_SECTION
#define SYS_THREAD_T      DWORD
#define SYS_LOCK_T        CRITICAL_SECTION
#define SYS_THREAD_SELF() GetCurrentThreadId()
#define SYS_LOCK(l)	  EnterCriticalSection(l)
#define SYS_TRYLOCK(l)	  TryEnterCriticalSection(l)
#define SYS_UNLOCK(l)	  LeaveCriticalSection(l)

#define RECURSIVE_MUTEX_INIT { 0 }

#else /*USE_WIN32_CRITICAL_SECTION*/
#include <pthread.h>

#define SYS_THREAD_T      pthread_t
#define SYS_LOCK_T        pthread_mutex_t
#define SYS_THREAD_SELF() pthread_self()
#define SYS_LOCK(l)	  pthread_mutex_lock(l)
#define SYS_TRYLOCK(l)	  (pthread_mutex_trylock(l) == 0)
#define SYS_UNLOCK(l)	  pthread_mutex_unlock(l)

#define RECURSIVE_MUTEX_INIT { 0, 0, PTHREAD_MUTEX_INITIALIZER }

#endif /*USE_WIN32_CRITICAL_SECTION*/

typedef struct _mutex_t
{ SYS_THREAD_T	owner;
  int		count;
  SYS_LOCK_T	lock;
} recursive_mutex_t;

static recursive_mutex_t mutex = RECURSIVE_MUTEX_INIT;

bool
pceMTinit(void)
{ XPCE_mt = TRUE;
#ifdef USE_WIN32_CRITICAL_SECTION
  InitializeCriticalSection(&mutex.lock);
#endif
  return true;
}

/* A Prolog thread has terminated */
void
pceMTdetach(void)
{ destroyPceThreadData();
}

static inline void
LOCK(void)
{ if ( XPCE_mt )
  { SYS_THREAD_T self = SYS_THREAD_SELF();

    if ( mutex.owner != self )
    { SYS_LOCK(&(mutex.lock));
      mutex.owner = self;
      mutex.count = 1;
    } else
      mutex.count++;
  }
}


static inline void
UNLOCK(void)
{ if ( XPCE_mt )
  { SYS_THREAD_T self = SYS_THREAD_SELF();

    if ( mutex.owner == self )
    { if ( --mutex.count == 0 )
      { mutex.owner = 0;
	SYS_UNLOCK(&(mutex.lock));
      }
    } else
      assert(0);
  }
}

/* Public API */
void
pceMTLock()
{ LOCK();
}

void
pceMTUnlock()
{ UNLOCK();
}

bool
pceMTTryLock(void)
{ if ( XPCE_mt )
  { SYS_THREAD_T self = SYS_THREAD_SELF();

    if ( mutex.owner != self )
    { if ( !SYS_TRYLOCK(&mutex.lock) )
	return false;

      mutex.owner = self;
      mutex.count = 1;
    } else
      mutex.count++;
  }

  return true;
}

/**
 * Unlock/relock when we do a callback to Prolog.  This provides
 * similar multi-threading as using the Python GIL.
 */

int
pceMTUnlockAll(void)
{ int rc = 0;

  if ( XPCE_mt )
  { SYS_THREAD_T self = SYS_THREAD_SELF();

    if ( mutex.owner == self )
    { assert(mutex.count);
      rc = mutex.count;
      mutex.owner = 0;
      mutex.count = 0;
      SYS_UNLOCK(&(mutex.lock));
    }
  }

  return rc;
}

void
pceMTRelock(int count)
{ if ( XPCE_mt && count )
  { SYS_THREAD_T self = SYS_THREAD_SELF();

    assert(mutex.owner != self);
    SYS_LOCK(&(mutex.lock));
    mutex.owner = self;
    mutex.count = count;
  }
}


#define pushGoal(g) { LOCK(); \
		      (g)->parent   = CurrentGoal; \
		      CurrentGoal = g; \
		    }
#define popGoal(g)  { CurrentGoal = (g)->parent; \
		      UNLOCK(); \
		    }

int
pceSetErrorGoal(PceGoal g, int err, ...)
{ va_list args;

  if ( g->errcode != PCE_ERR_OK )
    fail;
  g->errcode = err;

  va_start(args, err);
  switch(err)
  { case PCE_ERR_OK:
    case PCE_ERR_NO_BEHAVIOUR:
    case PCE_ERR_TOO_MANY_ARGS:
      break;
    case PCE_ERR_ARGTYPE:
    case PCE_ERR_RETTYPE:
    case PCE_ERR_ANONARG_AFTER_NAMED:
      g->errc1 = va_arg(args, PceObject); /* argument value */
      break;
    case PCE_ERR_NO_NAMED_ARGUMENT:
      g->errc1 = va_arg(args, PceObject); /* argument name */
      break;
    case PCE_ERR_MISSING_ARGUMENT:
      g->errc1 = va_arg(args, PceObject); /* index of missing arg */
      break;
    case PCE_ERR_CODE_AS_GETMETHOD:
      g->errc1 = va_arg(args, PceObject); /* offending code object */
      break;
    case PCE_ERR_PERMISSION:
      g->errc1 = va_arg(args, PceObject); /* offending operation */
      break;
    case PCE_ERR_FUNCTION_FAILED:
      g->errc1 = va_arg(args, PceObject); /* failing function */
      break;
    case PCE_ERR_ERROR:
      break;
  }
  va_end(args);

  fail;
}


static Method
getMethodMethodList(Any list, Name sel)
{ if ( instanceOfObject(list, ClassMethod) )
  { Method m = list;

    if ( m->name == sel )
      answer(m);

    fail;
  } else if ( instanceOfObject(list, ClassChain) )
  { Chain ch = list;
    Cell cell;
    Method m;

    for_cell(cell, ch)
    { if ( (m = getMethodMethodList(cell->value, sel)) )
	answer(m);
    }

    fail;
  } else
  { errorPce(list, NAME_unexpectedType, CtoType("method|chain"));
    fail;
  }
}


static Method
getCatchAllMethodGoal(PceGoal g)
{ Class cl = g->class;
  Method m, *mp;

  if ( !cl )
    cl = classOfObject(g->receiver);

  mp = ((g->flags & PCE_GF_SEND) ? (Method *)&cl->send_catch_all
				 : (Method *)&cl->get_catch_all);
  m = *mp;

  if ( isDefault(m) )
  { if ( g->flags & PCE_GF_SEND )
      m = getSendMethodClass(cl, NAME_catchAll);
    else
      m = getGetMethodClass(cl, NAME_catchAll);

    if ( m )
    { setDFlag(m, D_TYPENOWARN);
      assignField((Instance)cl, (Any *)mp, m);
    } else
      assignField((Instance)cl, (Any *)mp, NIL);
  }

  if ( notNil(m) )
    return m;

  return NULL;
}


static status
resolveImplementationGoal(PceGoal g)
{ Any m;
  Any obj = g->receiver;
  int issend = (g->flags & PCE_GF_SEND);

  if ( isInteger(obj) )
    g->receiver = obj = answerObject(ClassNumber, obj, EAV);

  if ( !g->class )
  { if ( onFlag(obj, F_ACTIVE|F_ATTRIBUTE|F_SENDMETHOD|F_GETMETHOD) )
    { while( isFunction(obj) )
      { m = (issend ? getSendMethodFunction(obj, g->selector)
		    : getGetMethodFunction(obj, g->selector));

	if ( m )
	{ g->implementation = m;
	  succeed;
	}

	if ( (obj = getExecuteFunction((Function) obj)) )
	{ if ( isInteger(obj) )
	    obj = answerObject(ClassNumber, obj, EAV);
	  g->receiver = obj;
	} else
	  return pceSetErrorGoal(g, PCE_ERR_FUNCTION_FAILED, obj);
      }

      if ( onFlag(obj, F_SENDMETHOD|F_GETMETHOD) )
      { Chain ch;

	if ( issend )
	  ch = getAllSendMethodsObject(obj, OFF);
	else
	  ch = getAllGetMethodsObject(obj, OFF);

	if ( ch && (m = getMethodMethodList(ch, g->selector)) )
	{ g->implementation = m;
	  succeed;
	}
      }

      if ( onFlag(obj, F_ATTRIBUTE) )
      { Chain ch = getAllAttributesObject(obj, ON);
	Cell cell;

	for_cell(cell, ch)
	{ Attribute att = cell->value;

	  if ( att->name == g->selector )
	  { g->implementation = att;
	    succeed;
	  }
	}
      }
    }

    g->class = classOfObject(obj);
  }

  if ( issend )
    m = getSendMethodClass(g->class, g->selector);
  else
    m = getGetMethodClass(g->class, g->selector);

  if ( m )
  { g->implementation = m;
    succeed;
  } else
  { Chain delegate = g->class->delegate;
    Cell cell;
    Class old = g->class;

    for_cell(cell, delegate)
    { Variable var = cell->value;
      Any val;

      if ( (val = getGetVariable(var, obj)) )
      { g->receiver = val;
	g->class    = NULL;

	if ( resolveImplementationGoal(g) && !(g->flags & PCE_GF_CATCHALL) )
	  succeed;
	g->flags &= ~PCE_GF_CATCHALL;
	g->errcode = PCE_ERR_OK;
      }
    }

    g->class    = old;
    g->receiver = obj;
  }

  if ( (m=getCatchAllMethodGoal(g)) )
  { g->flags |= PCE_GF_CATCHALL;
    g->implementation = m;

    succeed;
  }

  g->implementation = NIL;		/* so isProperGoal() succeeds */
  g->errcode = PCE_ERR_NO_BEHAVIOUR;	/* cause this need not be fatal */
/*return pceSetErrorGoal(g, PCE_ERR_NO_BEHAVIOUR);*/
  fail;
}


status
pceResolveImplementation(PceGoal g)
{ g->va_allocated = 0;
  g->va_type      = NULL;
  g->argn	  = 0;

  if ( !resolveImplementationGoal(g) )
    fail;

  pushGoal(g);

  if ( objectIsInstanceOf(g->implementation, ClassMethod) )
  { Method m = g->implementation;

    g->argc  = valInt(m->types->size);
    g->types = (PceType *)m->types->elements;
    if ( g->argc > 0 && g->types[g->argc-1]->vector == ON )
    { g->va_type = g->types[g->argc-1];
      g->argc--;
      g->va_argc = 0;
    }

    if ( g->flags & PCE_GF_GET )
    { GetMethod gm = (GetMethod)m;
      g->return_type = gm->return_type;
    }

    if ( onDFlag(m, D_HOSTMETHOD) )
      g->flags |= PCE_GF_HOST;
  } else				/* TBD: reorganise hierarchy! */
  { if ( g->flags & PCE_GF_SEND )
    { g->argc = 1;
      if ( objectIsInstanceOf(g->implementation, ClassObjOfVariable) )
      { Variable v = g->implementation;

	g->types = &v->type;
      } else if ( objectIsInstanceOf(g->implementation, ClassClassVariable) )
      { ClassVariable cv = g->implementation;

	g->types = &cv->type;
      } else				/* Attribute */
      { g->types = &TypeAny;
      }
    } else
    { g->argc = 0;
    }
  }

  succeed;
}


void
pceInitArgumentsGoal(PceGoal g)
{ int an = g->argc;
  PceObject *ap;

  if ( an <= PCE_GOAL_DIRECT_ARGS )
    ap = g->_av;
  else
  { ap = alloc(an*sizeof(PceObject));
    g->flags |= PCE_GF_ALLOCATED;
  }

  g->argv = ap;

  while( --an >= 0 )
    *ap++ = NULL;

  if ( (g->flags & PCE_GF_CATCHALL) && !(g->flags & PCE_GF_HOSTARGS) )
    pcePushArgument(g, g->selector);
}


void
pceVaAddArgGoal(PceGoal g, Any value)
{ if ( g->va_argc >= g->va_allocated )
  { if ( g->va_allocated )
    { int nsize = g->va_allocated*2;
      Any *nav  = alloc(nsize * sizeof(Any));

      cpdata(nav, g->va_argv, Any, g->va_allocated);
      unalloc(g->va_allocated*sizeof(Any), g->va_argv);
      g->va_argv = nav;
      g->va_allocated = nsize;
    } else
    { g->va_allocated = 8;
      g->va_argv = alloc(g->va_allocated * sizeof(Any));
      g->flags |= PCE_GF_VA_ALLOCATED;
    }
  }

  g->va_argv[g->va_argc++] = value;
}


void
pcePushGoal(PceGoal g)
{ pushGoal(g);
}


void
pceFreeGoal(PceGoal g)
{ if ( g == CurrentGoal )
  { popGoal(g);

    if ( g->flags & (PCE_GF_ALLOCATED|PCE_GF_VA_ALLOCATED) )
    { if ( g->flags & PCE_GF_ALLOCATED )
	unalloc(g->argc * sizeof(Any), g->argv);
      if ( g->flags & PCE_GF_VA_ALLOCATED )
	unalloc(g->va_allocated*sizeof(Any), g->va_argv);
    }
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
pcePushArgument(PceGoal g, Any argument)
    Push anonymous argument.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
pcePushArgument(PceGoal g, Any arg)
{ if ( g->argn >= 0 )
  { if ( g->argn < g->argc )
    { Type t = g->types[g->argn];
      Any  v = checkType(arg, t, g->receiver);

      if ( v )
      { g->argv[g->argn++] = v;
	succeed;
      } else
      { err_argtype:
	if ( offDFlag(g->implementation, D_TYPENOWARN) )
	  pceSetErrorGoal(g, PCE_ERR_ARGTYPE, arg);

	fail;
      }
    } else
    { if ( g->va_type )
      { Any v = checkType(arg, g->va_type, g->receiver);

	if ( v )
	{ pceVaAddArgGoal(g, v);
	  succeed;
	} else
	  goto err_argtype;
      } else
      { if ( offDFlag(g->implementation, D_TYPENOWARN) )
	  pceSetErrorGoal(g, PCE_ERR_TOO_MANY_ARGS);

	fail;
      }
    }
  } else
    return pceSetErrorGoal(g, PCE_ERR_ANONARG_AFTER_NAMED, arg);
}


int
pcePushNamedArgument(PceGoal g, PceName name, Any arg)
{ int i;

  if ( !name )
    return pcePushArgument(g, arg);

  if ( g->argn >= g->argc && g->va_type )
    return pcePushArgument(g, answerObject(ClassBinding, name, arg, EAV));

  for(i=0; i<g->argc; i++)
  { if ( g->types[i]->argument_name == name )
    { Any v = checkType(arg, g->types[i], g->receiver);

      g->argn = -1;

      if ( v )
      { g->argv[i] = v;
	succeed;
      } else
      { if ( offDFlag(g->implementation, D_TYPENOWARN) )
	{ g->argn  = i;
	  pceSetErrorGoal(g, PCE_ERR_ARGTYPE, arg);
	}
	fail;
      }
    }
  }

  pceSetErrorGoal(g, PCE_ERR_NO_NAMED_ARGUMENT, name);

  return FALSE;
}


static inline int
fillDefaultsGoal(PceGoal g)
{ int n = g->argc;
  int i;

  for(i=0; i<n; i++)
  { if ( !g->argv[i] )
    { PceObject val;

      if ( (val = checkType(DEFAULT, g->types[i], g->receiver)) )
	g->argv[i] = val;
      else
      {	if ( offDFlag(g->implementation, D_TYPENOWARN) )
	  pceSetErrorGoal(g, PCE_ERR_MISSING_ARGUMENT, toInt(i));

	fail;
      }
    }
  }

  succeed;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
status pceExecuteGoal(g)
    Execute the goal.  For get-goals, the return-value is stored in g->rval.
    Success/failure of the goal is indicated using the return value of this
    function.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifdef O_RUNTIME
#define DEBUGGER(g)
#else
#define DEBUGGER(g) if ( PCEdebugging ) g
#endif

typedef Any (*VAGetFunc0)(Any r, int vaac, Any *vaav);
typedef Any (*VAGetFunc1)(Any r, Any, int vaac, Any *vaav);
typedef Any (*VAGetFunc2)(Any r, Any, Any, int vaac, Any *vaav);
typedef Any (*VAGetFunc3)(Any r, Any, Any, Any, int vaac, Any *vaav);
typedef Any (*VAGetFunc4)(Any r, Any, Any, Any, Any, int vaac, Any *vaav);
typedef Any (*VAGetFunc5)(Any r, Any, Any, Any, Any, Any, int vaac, Any *vaav);
typedef Any (*VAGetFunc6)(Any r, Any, Any, Any, Any, Any, Any, int vaac, Any *vaav);
typedef Any (*VAGetFunc7)(Any r, Any, Any, Any, Any, Any, Any, Any, int vaac, Any *vaav);
typedef Any (*VAGetFunc8)(Any r, Any, Any, Any, Any, Any, Any, Any, Any, int vaac, Any *vaav);
typedef Any (*VAGetFunc9)(Any r, Any, Any, Any, Any, Any, Any, Any, Any, Any, int vaac, Any *vaav);
typedef Any (*VAGetFunc10)(Any r, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, int vaac, Any *vaav);

typedef Any (*GetFunc0)(Any r);
typedef Any (*GetFunc1)(Any r, Any);
typedef Any (*GetFunc2)(Any r, Any, Any);
typedef Any (*GetFunc3)(Any r, Any, Any, Any);
typedef Any (*GetFunc4)(Any r, Any, Any, Any, Any);
typedef Any (*GetFunc5)(Any r, Any, Any, Any, Any, Any);
typedef Any (*GetFunc6)(Any r, Any, Any, Any, Any, Any, Any);
typedef Any (*GetFunc7)(Any r, Any, Any, Any, Any, Any, Any, Any);
typedef Any (*GetFunc8)(Any r, Any, Any, Any, Any, Any, Any, Any, Any);
typedef Any (*GetFunc9)(Any r, Any, Any, Any, Any, Any, Any, Any, Any, Any);
typedef Any (*GetFunc10)(Any r, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any);
typedef Any (*GetFunc11)(Any r, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any);

typedef status (*VASendFunc0)(Any r, int vaac, Any *vaav);
typedef status (*VASendFunc1)(Any r, Any, int vaac, Any *vaav);
typedef status (*VASendFunc2)(Any r, Any, Any, int vaac, Any *vaav);
typedef status (*VASendFunc3)(Any r, Any, Any, Any, int vaac, Any *vaav);
typedef status (*VASendFunc4)(Any r, Any, Any, Any, Any, int vaac, Any *vaav);
typedef status (*VASendFunc5)(Any r, Any, Any, Any, Any, Any, int vaac, Any *vaav);
typedef status (*VASendFunc6)(Any r, Any, Any, Any, Any, Any, Any, int vaac, Any *vaav);
typedef status (*VASendFunc7)(Any r, Any, Any, Any, Any, Any, Any, Any, int vaac, Any *vaav);
typedef status (*VASendFunc8)(Any r, Any, Any, Any, Any, Any, Any, Any, Any, int vaac, Any *vaav);
typedef status (*VASendFunc9)(Any r, Any, Any, Any, Any, Any, Any, Any, Any, Any, int vaac, Any *vaav);
typedef status (*VASendFunc10)(Any r, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, int vaac, Any *vaav);

typedef status (*SendFunc0)(Any r);
typedef status (*SendFunc1)(Any r, Any);
typedef status (*SendFunc2)(Any r, Any, Any);
typedef status (*SendFunc3)(Any r, Any, Any, Any);
typedef status (*SendFunc4)(Any r, Any, Any, Any, Any);
typedef status (*SendFunc5)(Any r, Any, Any, Any, Any, Any);
typedef status (*SendFunc6)(Any r, Any, Any, Any, Any, Any, Any);
typedef status (*SendFunc7)(Any r, Any, Any, Any, Any, Any, Any, Any);
typedef status (*SendFunc8)(Any r, Any, Any, Any, Any, Any, Any, Any, Any);
typedef status (*SendFunc9)(Any r, Any, Any, Any, Any, Any, Any, Any, Any, Any);
typedef status (*SendFunc10)(Any r, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any);
typedef status (*SendFunc11)(Any r, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any);

status
pceExecuteGoal(PceGoal g)
{ if ( !fillDefaultsGoal(g) )
  { pceReportErrorGoal(g);
    fail;
  }

  if ( objectIsInstanceOf(g->implementation, ClassMethod) )
  { status rval;
    Method m = g->implementation;
    void *prof_node;

    DEBUGGER(pcePrintEnterGoal(g));
    if ( PceProfile.call )
      prof_node = (*PceProfile.call)(g->implementation,
				     PceProfile.handle);
    else
      prof_node = NULL;

    if ( m->function )				/* C-function implemented */
    { Any r      = g->receiver;
      Func f     = m->function;
      Any *a     = g->argv;

      if ( g->flags & PCE_GF_GET )		/* Get method */
      { Any fval;

	if ( g->va_type )
	{ int  vaac = g->va_argc;
	  Any *vaav = g->va_argv;

	  switch(g->argc)
	  { case 0:
	      fval = (*(VAGetFunc0)f)(r, vaac, vaav);
	      break;
	    case 1:
	      fval = (*(VAGetFunc1)f)(r, a[0], vaac, vaav);
	      break;
	    case 2:
	      fval = (*(VAGetFunc2)f)(r, a[0], a[1], vaac, vaav);
	      break;
	    case 3:
	      fval = (*(VAGetFunc3)f)(r, a[0], a[1], a[2], vaac, vaav);
	      break;
	    case 4:
	      fval = (*(VAGetFunc4)f)(r, a[0], a[1], a[2], a[3], vaac, vaav);
	      break;
	    case 5:
	      fval = (*(VAGetFunc5)f)(r, a[0], a[1], a[2], a[3], a[4], vaac, vaav);
	      break;
	    case 6:
	      fval = (*(VAGetFunc6)f)(r, a[0], a[1], a[2], a[3], a[4], a[5], vaac, vaav);
	      break;
	    case 7:
	      fval = (*(VAGetFunc7)f)(r, a[0], a[1], a[2], a[3], a[4], a[5],
				     a[6], vaac, vaav);
	      break;
	    case 8:
	      fval = (*(VAGetFunc8)f)(r, a[0], a[1], a[2], a[3], a[4], a[5],
				     a[6], a[7], vaac, vaav);
	      break;
	    case 9:
	      fval = (*(VAGetFunc9)f)(r, a[0], a[1], a[2], a[3], a[4], a[5],
				     a[6], a[7], a[8], vaac, vaav);
	      break;
	    case 10:
	      fval = (*(VAGetFunc10)f)(r, a[0], a[1], a[2], a[3], a[4], a[5],
				      a[6], a[7], a[8], a[9], vaac, vaav);
	      break;
	    default:
	      fval = (Any)FAIL;
	      assert(0);
	  }
	} else
	{ switch(g->argc)
	  { case 0:
	      fval = (*(GetFunc0)f)(r);
	      break;
	    case 1:
	      fval = (*(GetFunc1)f)(r, a[0]);
	      break;
	    case 2:
	      fval = (*(GetFunc2)f)(r, a[0], a[1]);
	      break;
	    case 3:
	      fval = (*(GetFunc3)f)(r, a[0], a[1], a[2]);
	      break;
	    case 4:
	      fval = (*(GetFunc4)f)(r, a[0], a[1], a[2], a[3]);
	      break;
	    case 5:
	      fval = (*(GetFunc5)f)(r, a[0], a[1], a[2], a[3], a[4]);
	      break;
	    case 6:
	      fval = (*(GetFunc6)f)(r, a[0], a[1], a[2], a[3], a[4], a[5]);
	      break;
	    case 7:
	      fval = (*(GetFunc7)f)(r, a[0], a[1], a[2], a[3], a[4], a[5], a[6]);
	      break;
	    case 8:
	      fval = (*(GetFunc8)f)(r, a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7]);
	      break;
	    case 9:
	      fval = (*(GetFunc9)f)(r, a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7],
				    a[8]);
	      break;
	    case 10:
	      fval = (*(GetFunc10)f)(r, a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7],
				     a[8], a[9]);
	      break;
	    case 11:
	      fval = (*(GetFunc11)f)(r, a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7],
				     a[8], a[9], a[10]);
	      break;
	    default:
	      fval = (Any)FAIL;
	      assert(0);
	  }
	}
	g->rval = fval;
	rval = !!fval;
      } else					/* Send method */
      {	if ( g->va_type )
	{ int  vaac = g->va_argc;
	  Any *vaav = g->va_argv;

	  switch(g->argc)
	  { case 0:
	      rval = (*(VASendFunc0)f)(r, vaac, vaav);
	      break;
	    case 1:
	      rval = (*(VASendFunc1)f)(r, a[0], vaac, vaav);
	      break;
	    case 2:
	      rval = (*(VASendFunc2)f)(r, a[0], a[1], vaac, vaav);
	      break;
	    case 3:
	      rval = (*(VASendFunc3)f)(r, a[0], a[1], a[2], vaac, vaav);
	      break;
	    case 4:
	      rval = (*(VASendFunc4)f)(r, a[0], a[1], a[2], a[3], vaac, vaav);
	      break;
	    case 5:
	      rval = (*(VASendFunc5)f)(r, a[0], a[1], a[2], a[3], a[4], vaac, vaav);
	      break;
	    case 6:
	      rval = (*(VASendFunc6)f)(r, a[0], a[1], a[2], a[3], a[4], a[5], vaac, vaav);
	      break;
	    case 7:
	      rval = (*(VASendFunc7)f)(r, a[0], a[1], a[2], a[3], a[4], a[5],
				     a[6], vaac, vaav);
	      break;
	    case 8:
	      rval = (*(VASendFunc8)f)(r, a[0], a[1], a[2], a[3], a[4], a[5],
				     a[6], a[7], vaac, vaav);
	      break;
	    case 9:
	      rval = (*(VASendFunc9)f)(r, a[0], a[1], a[2], a[3], a[4], a[5],
				     a[6], a[7], a[8], vaac, vaav);
	      break;
	    case 10:
	      rval = (*(VASendFunc10)f)(r, a[0], a[1], a[2], a[3], a[4], a[5],
				      a[6], a[7], a[8], a[9], vaac, vaav);
	      break;
	    default:
	      rval = FAIL;
	      assert(0);
	  }
	} else
	{ switch(g->argc)
	  { case 0:
	      rval = (*(SendFunc0)f)(r);
	      break;
	    case 1:
	      rval = (*(SendFunc1)f)(r, a[0]);
	      break;
	    case 2:
	      rval = (*(SendFunc2)f)(r, a[0], a[1]);
	      break;
	    case 3:
	      rval = (*(SendFunc3)f)(r, a[0], a[1], a[2]);
	      break;
	    case 4:
	      rval = (*(SendFunc4)f)(r, a[0], a[1], a[2], a[3]);
	      break;
	    case 5:
	      rval = (*(SendFunc5)f)(r, a[0], a[1], a[2], a[3], a[4]);
	      break;
	    case 6:
	      rval = (*(SendFunc6)f)(r, a[0], a[1], a[2], a[3], a[4], a[5]);
	      break;
	    case 7:
	      rval = (*(SendFunc7)f)(r, a[0], a[1], a[2], a[3], a[4], a[5], a[6]);
	      break;
	    case 8:
	      rval = (*(SendFunc8)f)(r, a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7]);
	      break;
	    case 9:
	      rval = (*(SendFunc9)f)(r, a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7],
				    a[8]);
	      break;
	    case 10:
	      rval = (*(SendFunc10)f)(r, a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7],
				     a[8], a[9]);
	      break;
	    case 11:
	      rval = (*(SendFunc11)f)(r, a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7],
				     a[8], a[9], a[10]);
	      break;
	    default:
	      rval = FAIL;
	      assert(0);
	  }
	}
      }
					/* end of function-implementation */
    } else				/* not a C-function */
    { if ( objectIsInstanceOf(m->message, ClassCode) )
      {					/* A function object */
	if ( objectIsInstanceOf(m->message, ClassFunction) )
	{ Any fval;
	  Function f = (Function)m->message;

	  if ( g->va_type )
	  { Any cv = createCodeVectorv(g->va_argc, g->va_argv);
	    ArgVector(av, g->argc+1);
	    int i;

	    for(i=0; i<g->argc; i++)
	      av[i] = g->argv[i];
	    av[i] = cv;
	    addCodeReference(cv);

	    withReceiver(g->receiver, m->context,
			 fval = getForwardFunctionv(f, i, av));

	    delCodeReference(cv);
	    doneCodeVector(cv);
	  } else
	  { withReceiver(g->receiver, m->context,
			 fval = getForwardFunctionv(f, g->argc, g->argv));
	  }

	  if ( fval )
	  { if ( g->flags & PCE_GF_GET )
	      g->rval = fval;
	    rval = SUCCEED;
	  } else
	    rval = FAIL;
	} else				/* a procedure (code) object */
	{ if ( g->flags & PCE_GF_GET )
	  { pceSetErrorGoal(g, PCE_ERR_CODE_AS_GETMETHOD, m->message);
	    rval = FAIL;
	    goto out;
	  }

	  if ( g->va_type )
	  { Any cv = createCodeVectorv(g->va_argc, g->va_argv);
	    ArgVector(av, g->argc+1);
	    int i;

	    for(i=0; i<g->argc; i++)
	      av[i] = g->argv[i];
	    av[i] = cv;
	    addCodeReference(cv);

	    withReceiver(g->receiver, m->context,
			 rval = forwardCodev(m->message, i, av));

	    delCodeReference(cv);
	    doneCodeVector(cv);
	  } else
	  { withReceiver(g->receiver, m->context,
			 rval = forwardCodev(m->message, g->argc, g->argv));
	  }
	}
      } else
	rval = hostCall(g);
    }

out:
    if ( prof_node && PceProfile.exit )
      (*PceProfile.exit)(prof_node);
    DEBUGGER(pcePrintReturnGoal(g, rval));
    return rval;
					/* end of method-implemtation */

  } else if ( objectIsInstanceOf(g->implementation, ClassObjOfVariable) )
  { Variable var = g->implementation;
    Any *field = &(((Instance)g->receiver)->slots[valInt(var->offset)]);

    if ( g->flags & PCE_GF_SEND )
      assignField(g->receiver, field, g->argv[0]);
    else
    { if ( isClassDefault(*field) )
      { Any v = getGetVariable(var, g->receiver);

	if ( v )
	  g->rval = v;
	else
	{ DEBUGGER(pcePrintReturnGoal(g, FAIL));
	  fail;
	}
      } else
	g->rval = *field;

      DEBUGGER(pcePrintReturnGoal(g, SUCCEED));
    }

    succeed;
  } else if ( objectIsInstanceOf(g->implementation, ClassClassVariable) )
  { ClassVariable cv = g->implementation;

    if ( g->flags & PCE_GF_SEND )
    { return pceSetErrorGoal(g, PCE_ERR_PERMISSION, NAME_write);
    } else
    { g->rval =  getValueClassVariable(cv);

      DEBUGGER(pcePrintReturnGoal(g, SUCCEED));
      succeed;
    }
  } else if ( objectIsInstanceOf(g->implementation, ClassAttribute) )
  { Attribute a = g->implementation;

    DEBUGGER(pcePrintReturnGoal(g, SUCCEED));
    if ( g->flags & PCE_GF_SEND )
      assign(a, value, g->argv[0]);
    else
      g->rval = a->value;

    succeed;
  }

  assert(0);
  fail;
}


static int
getNamedArgument(Any obj, Name *an, Any *av)
{ if ( isObject(obj) && onFlag(obj, F_ISBINDING) )
  { Binding b = obj;

    *an = b->name;
    *av = b->value;

    succeed;
  }

  fail;
}


void
pceReportErrorGoal(PceGoal g)
{ int pushed;

  if ( g->flags & PCE_GF_THROW )	/* already an exception pending! */
    return;

  if ( CurrentGoal != g )		/* if there is no implementation */
  { pushGoal(g);
    pushed = TRUE;
  } else
    pushed = FALSE;

  switch(g->errcode)
  { case PCE_ERR_OK:
      break;
    case PCE_ERR_NO_BEHAVIOUR:
    { Name arrow = ((g->flags & PCE_GF_SEND) ? CtoName("->") : CtoName("<-"));

      g->argc    = 0;			/* make the goal argument sane */
      g->va_type = 0;
      errorPce(g->receiver, NAME_noBehaviour, arrow, g->selector);
      break;
    }
    case PCE_ERR_ARGTYPE:
    { int an = g->argn;
      Type t = g->types[an];

      errorTypeMismatch(g->receiver, g->implementation, an+1, t, g->errc1);
      break;
    }
    case PCE_ERR_RETTYPE:
      errorPce(g->implementation, NAME_badReturnValue,
	       g->errc1, g->return_type);
      break;
    case PCE_ERR_TOO_MANY_ARGS:
      errorPce(g->implementation, NAME_argumentCount, toInt(g->argc));
      break;
    case PCE_ERR_ANONARG_AFTER_NAMED:
      errorPce(g->implementation, NAME_unboundAfterBoundArgument);
      break;
    case PCE_ERR_NO_NAMED_ARGUMENT:
      errorPce(g->implementation, NAME_noNamedArgument, g->errc1);
      break;
    case PCE_ERR_MISSING_ARGUMENT:
    { int an = valInt(g->errc1);
      Type t = g->types[an];
      Name argname;

      if ( instanceOfObject(g->implementation, ClassObjOfVariable) )
      { Variable v = g->implementation;
	argname = v->name;
      } else
      {	argname = t->argument_name;
	if ( isNil(argname) )
	  argname = CtoName("?");
      }

      errorPce(g->implementation, NAME_missingArgument,
	       toInt(an+1), argname, getNameType(t));

      break;
    }
    case PCE_ERR_FUNCTION_FAILED:	/* this is not (yet) reported */
      break;
    case PCE_ERR_ERROR:
      break;
    default:
      Cprintf("Unknown error: %d\n", g->errcode);
  }

  if ( pushed )
    popGoal(g);
}


status
vm_send(Any receiver, Name selector, Class class, int argc, const Any argv[])
{ pce_goal g;

  g.va_argc  = 0;
  g.flags    = PCE_GF_SEND;
  g.receiver = receiver;
  g.class    = class;
  g.selector = selector;
  g.errcode  = PCE_ERR_OK;

  if ( pceResolveImplementation(&g) )
  { int i;
    status rval;

    pceInitArgumentsGoal(&g);
    for(i=0; i<argc; i++)
    { Name an;
      Any av;

      if ( getNamedArgument(argv[i], &an, &av) )
      { if ( !pcePushNamedArgument(&g, an, av) )
	{ if ( g.errcode == PCE_ERR_NO_NAMED_ARGUMENT )
	  { if ( pcePushArgument(&g, argv[i]) )
	    { g.errcode = PCE_ERR_OK;
	      continue;
	    }
	    pceSetErrorGoal(&g, PCE_ERR_NO_NAMED_ARGUMENT, an);
	  }
	  goto error;
	}
      } else
      { if ( !pcePushArgument(&g, argv[i]) )
	  goto error;
      }
    }
    rval = pceExecuteGoal(&g);
    pceFreeGoal(&g);
    return rval;
  }

error:
  pceReportErrorGoal(&g);
  pceFreeGoal(&g);

  fail;
}


Any
vm_get(Any receiver, Name selector, Class class, int argc, const Any argv[])
{ pce_goal g;

  g.va_argc  = 0;
  g.flags    = PCE_GF_GET;
  g.receiver = receiver;
  g.class    = class;
  g.selector = selector;
  g.errcode  = PCE_ERR_OK;

  if ( pceResolveImplementation(&g) )
  { int i;
    status rval;

    pceInitArgumentsGoal(&g);
    for(i=0; i<argc; i++)
    { Name an;
      Any av;

      if ( getNamedArgument(argv[i], &an, &av) )
      { if ( !pcePushNamedArgument(&g, an, av) )
	  goto error;
      } else
      { if ( !pcePushArgument(&g, argv[i]) )
	  goto error;
      }
    }
    rval = pceExecuteGoal(&g);
    pceFreeGoal(&g);
    if ( rval )
      return g.rval;
    fail;

  error:
    pceFreeGoal(&g);
  }

  pceReportErrorGoal(&g);

  fail;
}


status
sendSendMethod(SendMethod sm, Any receiver, int argc, const Any argv[])
{ pce_goal g;
  int i;
  status rval;

				/* this is pceResolveImplementation() */
  g.selector       = sm->name;
  g.va_allocated   = 0;
  g.va_argc        = 0;
  g.argn	   = 0;
  g.flags	   = PCE_GF_SEND;
  g.receiver       = receiver;
  g.implementation = sm;
  g.errcode        = PCE_ERR_OK;

  if ( onDFlag(sm, D_HOSTMETHOD) )
    g.flags |= PCE_GF_HOST;

  pushGoal(&g);

  g.argc  = valInt(sm->types->size);
  g.types = (PceType *)sm->types->elements;
  if ( g.argc > 0 && g.types[g.argc-1]->vector == ON )
  { g.va_type = g.types[g.argc-1];
    g.argc--;
    g.va_argc = 0;
  } else
  { g.va_type = NULL;
  }

					/* and this is as vm_send() */
  pceInitArgumentsGoal(&g);
  for(i=0; i<argc; i++)
  { Name an;
    Any av;

    if ( getNamedArgument(argv[i], &an, &av) )
    { if ( !pcePushNamedArgument(&g, an, av) )
	goto error;
    } else
    { if ( !pcePushArgument(&g, argv[i]) )
	goto error;
    }
  }
  rval = pceExecuteGoal(&g);
  pceFreeGoal(&g);
  return rval;

error:
  popGoal(&g);
  pceReportErrorGoal(&g);

  fail;
}


Any
getGetGetMethod(GetMethod gm, Any receiver, int argc, const Any argv[])
{ pce_goal g;
  int i;
  status rval;

					/* this is pceResolveSend() */
  g.selector       = gm->name;
  g.va_allocated   = 0;
  g.va_argc        = 0;
  g.argn	   = 0;
  g.flags	   = PCE_GF_GET;
  g.receiver       = receiver;
  g.implementation = gm;
  g.errcode        = PCE_ERR_OK;
  g.return_type	   = gm->return_type;

  if ( onDFlag(gm, D_HOSTMETHOD) )
    g.flags |= PCE_GF_HOST;

  g.argc  = valInt(gm->types->size);
  g.types = (PceType *)gm->types->elements;
  if ( g.argc > 0 && g.types[g.argc-1]->vector == ON )
  { g.va_type = g.types[g.argc-1];
    g.argc--;
    g.va_argc = 0;
  } else
  { g.va_type = NULL;
  }

					/* and this is as vm_get() */
  pceInitArgumentsGoal(&g);
  for(i=0; i<argc; i++)
  { Name an;
    Any av;

    if ( getNamedArgument(argv[i], &an, &av) )
    { if ( !pcePushNamedArgument(&g, an, av) )
	goto error;
    } else
    { if ( !pcePushArgument(&g, argv[i]) )
	goto error;
    }
  }
  rval = pceExecuteGoal(&g);
  pceFreeGoal(&g);
  if ( rval )
    return g.rval;
  fail;

error:
  pceReportErrorGoal(&g);

  fail;
}

		 /*******************************
		 *     HOST-CALLING SUPPORT	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
pceGetArgumentTypeGoal()
    Deternimes type and argument location for the next argument.  Location
    -1 indicates the argument must be placed in the variable-argument list.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


int
pceGetArgumentTypeGoal(PceGoal g, PceName name, PceType *type, int *ai)
{ if ( name )
  { int i;

    if ( g->argn >= g->argc && g->va_type )
    { *type = g->va_type;
      *ai   = -1;			/* Means use vararg list */
      succeed;
    }

    g->argn = -1;

    for(i=0; i<g->argc; i++)
    { if ( g->types[i]->argument_name == name )
      { *type = g->types[i];
        *ai = i;

        succeed;
      }
    }

    return pceSetErrorGoal(g, PCE_ERR_NO_NAMED_ARGUMENT, name);
  }

  if ( g->argn >= 0 )
  { if ( g->argn < g->argc )
    { *type = g->types[g->argn];
      *ai   = g->argn++;

      succeed;
    } else
    { if ( g->va_type )
      { *type = g->types[g->argn];
        *ai   = -1;

        succeed;
      } else
      { if ( offDFlag(g->implementation, D_TYPENOWARN) )
	  pceSetErrorGoal(g, PCE_ERR_TOO_MANY_ARGS);

	fail;
      }
    }
  } else
    return pceSetErrorGoal(g, PCE_ERR_ANONARG_AFTER_NAMED, NIL);
}


#undef sendv
status
sendv(Any receiver, Name selector, int argc, Any *argv)
{ return vm_send(receiver, selector, NULL, argc, argv);
}


typedef status (*SendFunc0)(Any r);
typedef status (*SendFunc1)(Any r, Any);
typedef status (*SendFunc2)(Any r, Any, Any);
typedef status (*SendFunc3)(Any r, Any, Any, Any);
typedef status (*SendFunc4)(Any r, Any, Any, Any, Any);
typedef status (*SendFunc5)(Any r, Any, Any, Any, Any, Any);
typedef status (*SendFunc6)(Any r, Any, Any, Any, Any, Any, Any);

status					/* QuickAndDirtySend */
qadSendv(Any r, Name selector, int ac, Any *av)
{ SendMethod implementation = getSendMethodClass(classOfObject(r), selector);
  SendFunc f;

  if ( instanceOfObject(implementation, ClassSendMethod) &&
       (f=implementation->function) &&
       offDFlag(implementation, D_CXX|D_TRACE|D_BREAK))
  { switch(ac)
    { case 0: return (*(SendFunc0)f)(r);
      case 1: return (*(SendFunc1)f)(r, av[0]);
      case 2: return (*(SendFunc2)f)(r, av[0],av[1]);
      case 3: return (*(SendFunc3)f)(r, av[0],av[1],av[2]);
      case 4: return (*(SendFunc4)f)(r, av[0],av[1],av[2],av[3]);
      case 5: return (*(SendFunc5)f)(r, av[0],av[1],av[2],av[3],av[4]);
      case 6: return (*(SendFunc6)f)(r, av[0],av[1],av[2],av[3],av[4],av[5]);
    }
  }

  return vm_send(r, selector, classOfObject(r), ac, av);
}


#undef getv
Any
getv(Any receiver, Name selector, int argc, Any *argv)
{ return vm_get(receiver, selector, NULL, argc, argv);
}

Any					/* QuickAndDirtyGet */
qadGetv(Any r, Name selector, int ac, Any *av)
{ GetMethod implementation = getGetMethodClass(classOfObject(r), selector);
  Func f;

  if ( instanceOfObject(implementation, ClassGetMethod) &&
       (f=implementation->function) &&
       offDFlag(implementation, D_CXX|D_TRACE|D_BREAK) )
  { switch(ac)
    { case 0: return (*(GetFunc0)f)(r);
      case 1: return (*(GetFunc1)f)(r, av[0]);
      case 2: return (*(GetFunc2)f)(r, av[0],av[1]);
      case 3: return (*(GetFunc3)f)(r, av[0],av[1],av[2]);
      case 4: return (*(GetFunc4)f)(r, av[0],av[1],av[2],av[3]);
      case 5: return (*(GetFunc5)f)(r, av[0],av[1],av[2],av[3],av[4]);
      case 6: return (*(GetFunc6)f)(r, av[0],av[1],av[2],av[3],av[4],av[5]);
    }
  }

  return vm_get(r, selector, classOfObject(r), ac, av);
}


		/********************************
		*        VARARG VERSIONS	*
		********************************/

status
send(Any receiver, Name selector, ...)
{ va_list args;
  Any argv[VA_PCE_MAX_ARGS];
  int argc;

  va_start(args, selector);
  for(argc=0; (argv[argc] = va_arg(args, Any)) != NULL; argc++)
    assert(argc <= VA_PCE_MAX_ARGS);
  va_end(args);

  return vm_send(receiver, selector, NULL, argc, argv);
}


Any
get(Any receiver, Name selector, ...)
{ va_list args;
  Any argv[VA_PCE_MAX_ARGS];
  int argc;

  va_start(args, selector);
  for(argc=0; (argv[argc] = va_arg(args, Any)) != NULL; argc++)
    assert(argc <= VA_PCE_MAX_ARGS);
  va_end(args);

  return vm_get(receiver, selector, NULL, argc, argv);
}

		 /*******************************
		 *     PUBLIC RESOLVE SUPPORT   *
		 *******************************/

Any
resolveSendMethodObject(Any obj, Class class, Name sel, Any *receiver)
{ pce_goal g;

  g.receiver = obj;
  g.class    = class;
  g.selector = sel;
  g.flags    = PCE_GF_SEND;
  g.errcode  = PCE_ERR_OK;

  if ( resolveImplementationGoal(&g) && !(g.flags & PCE_GF_CATCHALL) )
  { *receiver = g.receiver;
    return g.implementation;
  }

  fail;
}


Any
resolveGetMethodObject(Any obj, Class class, Name sel, Any *receiver)
{ pce_goal g;

  g.receiver = obj;
  g.class    = class;
  g.selector = sel;
  g.flags    = PCE_GF_GET;
  g.errcode  = PCE_ERR_OK;

  if ( resolveImplementationGoal(&g) && !(g.flags & PCE_GF_CATCHALL) )
  { *receiver = g.receiver;
    return g.implementation;
  }

  fail;
}
