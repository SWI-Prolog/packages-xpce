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
#include <h/trace.h>
#include <h/interface.h>
#include <h/graphics.h>
#include <h/unix.h>
#include "stub.h"
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#if !defined(FD_ZERO) && HAVE_SELECT
#include <sys/select.h>
#endif
#ifdef HAVE_CONIO_H			/* AIX 4.1 requires this */
#include <conio.h>
#endif
#ifdef HAVE_BSTRING_H
#include <bstring.h>
#endif

		/********************************
		*           C --> PCE		*
		********************************/

Any
cToPceInteger(intptr_t i)
{ Int n = toInt(i);

  if ( valInt(n) != i )
  { errorPce(PCE, NAME_intRange);
    fail;
  }

  return n;
}


Any
cToPceReal(double f)
{ return CtoReal(f);
}


Any
cToPceStringA(Name assoc, const char *s, size_t len, int translate)
{ Any str;
  string ss;
  Any c;

  str_set_n_ascii(&ss, len, (char *)s);
  c = StringToScratchCharArray(&ss);

  if ( translate )
    str = pceNew(assoc, ClassString, 1, &c);
  else
  { Any av[2];

    av[0] = name_procent_s;
    av[1] = c;
    str = pceNew(assoc, ClassString, 2, av);
  }
  doneScratchCharArray(c);

  return str;
}


Any
cToPceStringW(Name assoc, const wchar_t *s, size_t len, int translate)
{ Any str;
  string ss;
  Any c;

  str_set_n_wchar(&ss, len, (wchar_t*)s);
  c = StringToScratchCharArray(&ss);

  if ( translate )
    str = pceNew(assoc, ClassString, 1, &c);
  else
  { Any av[2];

    av[0] = name_procent_s;
    av[1] = c;
    str = pceNew(assoc, ClassString, 2, av);
  }
  doneScratchCharArray(c);

  return str;
}


Any
cToPceName(const char *text)
{ if ( text )
  { string s;

    str_set_n_ascii(&s, strlen(text), (char *)text);

    return StringToName(&s);
  } else
    fail;
}


Any
cToPceName_nA(const char *text, size_t len)
{ if ( text )
  { string s;

    str_set_n_ascii(&s, len, (char *)text);

    return StringToName(&s);
  } else
    fail;
}


Any
cToPceName_nW(const wchar_t *text, size_t len)
{ return WCToName(text, len);
}


Any
cToPcePointer(void *ptr)
{ CPointer p = answerObjectv(ClassCPointer, 0, NULL);

  p->pointer = ptr;

  return p;
}


void *
pcePointerToC(PceObject obj)
{ if ( instanceOfObject(obj, ClassCPointer) )
  { CPointer ptr = (CPointer)obj;

    return ptr->pointer;
  }

  return PCE_NO_POINTER;
}


Any
cToPceAssoc(const char *s)
{ return getObjectFromReferencePce(PCE, CtoName(s));
}


PceObject
pceObjectFromName(PceName name)
{ return findGlobal(name);
}


Any
cToPceReference(uintptr_t val)
{ Instance rval = longToPointer(val);

  if ( rval &&
       validAddress(rval) &&
       (rval->flags & (OBJ_MAGIC_MASK|F_FREED)) == OBJ_MAGIC )
    answer(rval);

  fail;
}


int
pceExistsReference(uintptr_t ref)
{ Any addr = longToPointer(ref);

  if ( !isProperObject(addr) || isFreedObj(addr) )
    return PCE_FAIL;

  return PCE_SUCCEED;
}


char *
pcePPReference(PceObject ref)
{ if ( isInteger(ref) )
  { Any addr = longToPointer(valInt(ref));
    char *rval = pp(addr);

    if ( rval[0] != '@' )
    { char tmp[256];
      sprintf(tmp, "@%" PRIdPTR, valInt(ref));
      return save_string(tmp);
    } else
      return rval;
  } else if ( isName(ref) )
  { Any addr;

    if ( !(addr = getObjectAssoc(ref)) )
    { char tmp[256];

      sprintf(tmp, "@%s", strName(ref));
      return save_string(tmp);
    } else
      return pp(addr);
  } else
    return save_string("invalid reference");
}


int
pceExistsAssoc(PceName assoc)
{ Any addr;

  if ( !(addr = getObjectAssoc(assoc)) )
    return PCE_FAIL;
  if ( !isProperObject(addr) || isFreedObj(addr) )
    return PCE_FAIL;

  return PCE_SUCCEED;
}


PceObject
cToPceTmpCharArray(const char *s)
{ return CtoScratchCharArray(s);
}


void
donePceTmpCharArray(Any ca)
{ doneScratchCharArray(ca);
}

		 /*******************************
		 *		GC		*
		 *******************************/

export void
_markAnswerStack(AnswerMark *mark)
{ *mark = AnswerStack->index;
}


		/********************************
		*           TYPE TEST		*
		********************************/

status
pceInstanceOf(Any obj, Any classspec)
{ Class class;

  if ( (class = checkType(classspec, TypeClass, NIL)) )
    return instanceOfObject(obj, class);

  errorPce(CtoName(pp(classspec)), NAME_unexpectedType, TypeClass);
  fail;
}


PceClass
nameToExistingClass(PceName Name)
{ return getMemberHashTable(classTable,	Name);
}


PceClass
pceClassOfObject(PceObject obj)
{ if ( isObject(obj) )
    return classOfObject(obj);

  fail;
}


int
pceReferencesOfObject(PceObject obj)
{ if ( isObject(obj) )
    return refsObject(obj);

  return -1;
}


int
pceFreeObject(PceObject obj)
{ if ( isObject(obj) )
    return freeObject(obj);

  fail;
}


void
pceSendMethod(PceClass class,
	      const char *name,
	      const char *group,
	      int argc,
	      ...)
{ Name n, g;
  va_list args;

  va_start(args, argc);

  n = cToPceName(name);
  g = group ? cToPceName(group) : (Name)DEFAULT;
  sendMethodv(class, n, g, argc, args);
  va_end(args);
}


void
pceGetMethod(PceClass class,
	     const char *name,
	     const char *group,
	     const char *rtype,
	     int argc,
	     ...)
{ Name n, g;
  va_list args;

  va_start(args, argc);

  n = cToPceName(name);
  g = group ? cToPceName(group) : (Name)DEFAULT;
  getMethodv(class, n, g, rtype, argc, args);
  va_end(args);
}


		/********************************
		*           PCE --> C		*
		********************************/

int
pceToCReference(Any obj, PceCValue *rval)
{ assert(isObject(obj));

  if ( onFlag(obj, F_ASSOC) )
  { rval->itf_symbol = getMemberHashTable(ObjectToITFTable, obj);
    return PCE_ASSOC;
  } else
  { rval->integer = valInt(PointerToInt(obj));
    return PCE_REFERENCE;
  }
}


int
pceToC(Any obj, PceCValue *rval)
{ if ( isInteger(obj) )
  { rval->integer = valInt((Int) obj);
    return PCE_INTEGER;
  }

  assert(obj);

  if ( onFlag(obj, F_ASSOC|F_ISNAME|F_ISREAL|F_ISHOSTDATA) )
  { if ( onFlag(obj, F_ASSOC) )
    { rval->itf_symbol = getMemberHashTable(ObjectToITFTable, obj);
      return PCE_ASSOC;
    }
    if ( onFlag(obj, F_ISNAME) )
    { rval->itf_symbol = getITFSymbolName(obj);
      return PCE_NAME;
    }
    if ( onFlag(obj, F_ISHOSTDATA) )
    { rval->pointer = ((HostData)obj)->handle;
      return PCE_HOSTDATA;
    }
    { rval->real = valReal(obj);
      return PCE_REAL;
    }
  } else
  { rval->integer = PointerToCInt(obj);
    return PCE_REFERENCE;
  }
}


int
pceIsString(Any val)
{ return instanceOfObject(val, ClassString) ? TRUE : FALSE;
}


char *
pceCharArrayToCA(Any val, size_t *len)
{ if ( instanceOfObject(val, ClassCharArray) )
  { CharArray ca = val;

    if ( isstrA(&ca->data) )
    { if ( len )
	*len = ca->data.s_size;

      return (char*)ca->data.s_textA;
    }
  }

  return NULL;
}


wchar_t *
pceCharArrayToCW(Any val, size_t *len)
{ if ( instanceOfObject(val, ClassCharArray) )
  { CharArray ca = val;

    if ( isstrW(&ca->data) )
    { if ( len )
	*len = ca->data.s_size;

      return ca->data.s_textW;
    }
  }

  return NULL;
}


int
pceObject(Any obj)
{ return isObject(obj) ? PCE_SUCCEED : PCE_FAIL;
}

		 /*******************************
		 *	      METHOD		*
		 *******************************/

static void
convert_trace_flags(PceMethod m, int *flags)
{ static struct dflagmap
  { int internal;
    int external;
  } staticmap[] =
  { { D_TRACE_ENTER, PCE_METHOD_INFO_TRACE_ENTER },
    { D_TRACE_EXIT,  PCE_METHOD_INFO_TRACE_EXIT },
    { D_TRACE_FAIL,  PCE_METHOD_INFO_TRACE_FAIL },
    { D_BREAK_ENTER, PCE_METHOD_INFO_BREAK_ENTER },
    { D_BREAK_EXIT,  PCE_METHOD_INFO_BREAK_EXIT },
    { D_BREAK_FAIL,  PCE_METHOD_INFO_BREAK_FAIL },
    { 0, 0 }
  };
  struct dflagmap *map = staticmap;

  for( ; map->internal; map++ )
  { if ( onDFlag(m, map->internal) )
      *flags |= map->external;
  }
}


int
pceGetMethodInfo(PceMethod m, pce_method_info *info)
{ if ( onDFlag(m, D_HOSTMETHOD) )
  { CPointer p = (CPointer)m->message;

    info->handle = p->pointer;
    if ( DebuggingProgramObject(m, D_TRACE|D_BREAK) )
      convert_trace_flags(m, &info->flags);

    if ( !(m->flags & PCE_METHOD_INFO_HANDLE_ONLY) )
    { info->name    = m->name;
      info->context = ((Class)m->context)->name;
      info->argc    = valInt(m->types->size);
      info->types   = (PceType*)m->types->elements;
    }

    succeed;
  }

  fail;
}


		/********************************
		*          SYMBOL-TABLE		*
		********************************/

PceITFSymbol
getITFSymbolName(Name name)
{ if ( onFlag(name, F_ITFNAME) )
    return getMemberHashTable(NameToITFTable, name);
  else
  { PceITFSymbol symbol = newSymbol(NULL, name);

    setFlag(name, F_ITFNAME);
    appendHashTable(NameToITFTable, name, symbol);

    return symbol;
  }
}


PceITFSymbol
pceLookupHandle(int n, hostHandle handle)
{ return getMemberHashTable(HandleToITFTables[n], handle);
}


void
pceRegisterName(int n, hostHandle handle, Name name)
{ PceITFSymbol symbol = getITFSymbolName(name);

  symbol->handle[n] = handle;
  appendHashTable(HandleToITFTables[n], handle, symbol);
}


void
pceRegisterAssoc(int n, hostHandle handle, Any obj)
{ if ( (isObject(obj) && onFlag(obj, F_ASSOC)) )
  { PceITFSymbol symbol = getMemberHashTable(ObjectToITFTable, obj);
    symbol->handle[n] = handle;
    appendHashTable(HandleToITFTables[n], handle, symbol);
  } else
  { PceITFSymbol symbol = newSymbol(obj, NULL);
    symbol->handle[n] = handle;

    if ( isObject(obj) )
      setFlag(obj, F_ASSOC);
    appendHashTable(HandleToITFTables[n], handle, symbol);
    appendHashTable(ObjectToITFTable, obj, symbol);
  }
}


		/********************************
		*  VIRTUAL MACHINE INSTRUCTIONS	*
		********************************/

Any
pceNew(Name assoc, Any class, int argc, Any *argv)
{ Any rval;

  if ( (rval = createObjectv(assoc, class, argc, argv)) )
    pushAnswerObject(rval);

  return rval;
}


status
pceSend(Any receiver, Name classname, Name selector, int argc, Any *argv)
{ Class cl;

  if ( classname )
  { if ( !(cl = getMemberHashTable(classTable, classname)) )
      return errorPce(receiver, NAME_noClass, classname);
    if ( !instanceOfObject(receiver, cl) )
      return errorPce(receiver, NAME_noSuperClassOf, classname);
  } else
    cl = NULL;

  return vm_send(receiver, selector, cl, argc, argv);
}


Any
pceGet(Any receiver, Name classname, Name selector, int argc, Any *argv)
{ Class cl;

  if ( classname )
  { if ( !(cl = getMemberHashTable(classTable, classname)) )
    { errorPce(receiver, NAME_noClass, classname);
      fail;
    }
    if ( !instanceOfObject(receiver, cl) )
    { errorPce(receiver, NAME_noSuperClassOf, classname);
      fail;
    }
  } else
    cl = NULL;

  return vm_get(receiver, selector, cl, argc, argv);
}


		/********************************
		*       EVENT DISPATCHING	*
		********************************/

#ifndef FD_ZERO
#define FD_ZERO(x)	{(x)->fds_bits[0] = 0;}
#define FD_SET(n, x)	{(x)->fds_bits[0] |= 1<<(n); }
#endif

#if !defined(HAVE_SELECT) && defined(HAVE_CONIO_H)
#include <conio.h>
#endif

int
pceDispatch(IOSTREAM *input, int time)
{ if ( DispatchEvents != NULL )
  { int rval;

    rval = (*DispatchEvents)(input, time);

    return (rval == SUCCEED ? PCE_DISPATCH_INPUT : PCE_DISPATCH_TIMEOUT);
  } else
  {
#ifndef HAVE_SELECT
    ws_dispatch(input, toInt(time));
    return PCE_DISPATCH_TIMEOUT;
#else
    int fd = Sfileno(input);
    if ( fd >= 0 )
    { if ( time > 0 )
      { struct timeval timeout;
	fd_set readfds;

	timeout.tv_sec = time / 1000;
	timeout.tv_usec = (time % 1000) * 1000;

	FD_ZERO(&readfds);
	FD_SET(fd, &readfds);
	if ( select(fd+1, &readfds, NULL, NULL, &timeout) > 0 )
	  return PCE_DISPATCH_INPUT;
	else
	  return PCE_DISPATCH_TIMEOUT;
      } else
      { fd_set readfds;
	FD_ZERO(&readfds);
	FD_SET(fd, &readfds);
	select(fd+1, &readfds, NULL, NULL, NULL);
	return PCE_DISPATCH_INPUT;
      }
    } else
    { return PCE_DISPATCH_INPUT;
    }
#endif /*HAVE_SELECT*/
  }
}


void
pceRedraw(int sync)
{ if ( sync )
  { static DisplayObj d = NULL;

    if ( !d && !(d = CurrentDisplay(NIL)) )
      return;

    synchroniseDisplay(d);
  } else
  { static DisplayManager dm = NULL;

    if ( !dm && !(dm = getObjectAssoc(NAME_displayManager)) )
      return;

    RedrawDisplayManager(dm);
  }
}

		/********************************
		*           DEBUGGING		*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
pceExecuteMode()
	returns PCE_EXEC_USER is the goal is to be processed in `user'
        space, and PCE_EXEC_SERVICE otherwise.  goals of the latter type
	are not supposed to be visible in the host tracer.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
pceExecuteMode(void)
{ return ServiceMode;
}


void
pceReset(void)
{ resetPce(PCE);
}


void
pceWriteCurrentGoal(void)
{
#ifndef O_RUNTIME
  writeGoal(CurrentGoal);
#endif
}


void
pceWriteErrorGoal(void)
{
#ifndef O_RUNTIME
  writeErrorGoal();
#endif
}

		 /*******************************
		 *	    DLL CALLBACK	*
		 *******************************/

static void
outOfMemory(void)
{ static int nesting = 0;

  if ( nesting++ > 2 )
    abort();
  Cprintf("Out of memory: ");
  Cprintf("%s", strName(getOsErrorPce(PCE)));
  hostAction(HOST_RECOVER_FROM_FATAL_ERROR);
  nesting--;
}


static void *
pce_malloc(size_t bytes)
{ void *mem;

  if ( !(mem = malloc(bytes)) )
    outOfMemory();

  return mem;
}


static void *
pce_realloc(void *old, size_t bytes)
{ void *mem;

  if ( !(mem = realloc(old, bytes)) )
    outOfMemory();

  return mem;
}


pce_callback_functions TheCallbackFunctions =
{ Stub__HostSend,			/* hostSend() */
  Stub__HostGet,			/* hostGet() */
  Stub__HostCall,			/* hostCallProc() */
  Stub__HostQuery,			/* hostQuery() */
  Stub__HostActionv,			/* hostActionv() */
  Stub__vCprintf,			/* console IO */
  Stub__Cputchar,			/* print single character */
  Stub__Cflush,				/* flush console output */
  Stub__Cgetline,			/* read line from console */
  pce_malloc,				/* malloc */
  pce_realloc,				/* realloc */
  free					/* free */
};


void
pceRegisterCallbacks(pce_callback_functions *fs)
{ void **new = (void **)fs;
  void **old = (void **)&TheCallbackFunctions;
  int i = sizeof(TheCallbackFunctions)/sizeof(void *);

  for( ; i-- > 0; old++, new++)
  { if ( *new )
      *old = *new;
  }
}


int
hostSend(PceObject host, PceName selector, int argc, PceObject argv[])
{ if ( TheCallbackFunctions.hostSend )
    return (*TheCallbackFunctions.hostSend)(host, selector, argc, argv);

  return FAIL;
}


PceObject
hostGet(PceObject host, PceName selector, int argc, PceObject argv[])
{ if ( TheCallbackFunctions.hostGet )
    return (*TheCallbackFunctions.hostGet)(host, selector, argc, argv);

  return FAIL;
}


int
hostQuery(int what, PceCValue *value)
{ if ( TheCallbackFunctions.hostQuery )
    return (*TheCallbackFunctions.hostQuery)(what, value);

  return FAIL;
}


int
hostAction(int what, ...)
{ if ( TheCallbackFunctions.hostActionv )
  { va_list args;
    int rval;

    va_start(args, what);
    rval = (*TheCallbackFunctions.hostActionv)(what, args);
    va_end(args);
    return rval;
  }

  return FAIL;
}


void
Cprintf(const char *fmt, ...)
{ if ( TheCallbackFunctions.vCprintf )
  { va_list args;

    va_start(args, fmt);
    (*TheCallbackFunctions.vCprintf)(fmt, args);
    va_end(args);
  }
}


void
Cvprintf(const char *fmt, va_list args)
{ if ( TheCallbackFunctions.vCprintf )
    (*TheCallbackFunctions.vCprintf)(fmt, args);
}


int
Cputchar(int chr)
{ if ( TheCallbackFunctions.Cputchar )
    return (*TheCallbackFunctions.Cputchar)(chr);
  else
  { Cprintf("%c", chr);
    return chr;
  }
}


int
Cputstr(PceString s)
{ if ( TheCallbackFunctions.Cputchar )
  { int i;

    for(i=0; i<s->s_size; i++)
    { (*TheCallbackFunctions.Cputchar)(str_fetch(s, i));
    }

    return s->s_size;
  } else if ( isstrA(s) )
  { Cprintf("%s", s->s_textA);

    return s->s_size;
  } else
    return 0;
}


void
Cflush()
{ if ( TheCallbackFunctions.Cflush )
    (*TheCallbackFunctions.Cflush)();
}


char *
Cgetline(char *line, int size)
{ if ( TheCallbackFunctions.Cgetline )
    return (*TheCallbackFunctions.Cgetline)(line, size);
  else
  { size = 0;				/* signal end-of-file */
    line[0] = '\0';
    return NULL;
  }
}

pce_profile_hooks PceProfile =
{ NULL,					/* call */
  NULL,					/* exit */
  NULL					/* handle */
};

int
pceSetProfileHooks(pce_profile_hooks *hooks)
{ PceProfile = *hooks;			/* structure copy */

  return TRUE;
}


		 /*******************************
		 *	 MEMORY ALLOCATION	*
		 *******************************/

#undef pceMalloc
#undef pceRealloc
#undef pceFree

void *
pceMalloc(size_t size)
{ return (*TheCallbackFunctions.malloc)(size);
}


void *
pceRealloc(void *ptr, size_t size)
{ return (*TheCallbackFunctions.realloc)(ptr, size);
}


void
pceFree(void *ptr)
{ (*TheCallbackFunctions.free)(ptr);
}

		 /*******************************
		 *	 INTERFACE ALLOC	*
		 *******************************/

void *
pceAlloc(int bytes)
{ return alloc(bytes);
}


void
pceUnAlloc(int bytes, void *p)
{ unalloc(bytes, p);
}


		 /*******************************
		 *	    COLLECTIONS		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function to help foreign-code enumerating the elements of XPCE chains
and vectors.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
pceEnumElements(PceObject collection,
		int (*enumfunc)(PceObject, void *),
		void *closure)
{ if ( instanceOfObject(collection, ClassChain) )
  { Chain ch = collection;
    PceObject e;

    for_chain(ch, e,
	      if ( !(*enumfunc)(e, closure) )
	        fail;
	     );
    succeed;
  }

  if ( instanceOfObject(collection, ClassVector) )
  { Vector v = collection;
    PceObject e;

    for_vector(v, e,
	       if ( !(*enumfunc)(e, closure) )
	         fail;
	      );
    succeed;
  }

  assert(0);
  fail;
}
