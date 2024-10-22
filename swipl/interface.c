/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org/packages/xpce/
    Copyright (c)  2011-2024, University of Amsterdam
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
#define HAVE_MALLOC_H 1
#define HAVE_SIGNAL_H 1
#include "../src/md.h"
#else
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#endif

#define SWI 1			/* SWI-Prolog version 2.5! and up */
#define O_SHAREDLIBRARY 1

#ifdef SWI
#include <SWI-Stream.h>			/* get intptr_t, etc first */
#include <SWI-Prolog.h>
#endif

#define DEBUG(g) ((void)0)

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdarg.h>
#include <assert.h>
#include <limits.h>
#include <sys/types.h>
#include <ctype.h>
#include <h/interface.h>
#include <string.h>
#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif
#ifdef HAVE_SIGNAL_H
#include <signal.h>
#endif
#ifdef HAVE_ALLOCA_H
#include <alloca.h>
#endif

#ifdef _REENTRANT
#define LOCK()   pceMTLock(LOCK_PCE)
#define UNLOCK() pceMTUnlock(LOCK_PCE)
#else
#define LOCK()
#define UNLOCK()
#endif

#ifdef __WINDOWS__
#include <windows.h>
#endif

#ifdef __GNUC__
#define TermVector(name, size)  term_t name[size]
#define ObjectVector(name, size) PceObject name[size]
#else
#define TermVector(name, size) \
	term_t *name = (term_t *) alloca(size * sizeof(term_t))
#define ObjectVector(name, size) \
	PceObject *name = (PceObject *) alloca(size * sizeof(term_t))
#endif

#ifndef TRUE
#define TRUE 1
#define FALSE 0
#endif

#ifndef EOS
#define EOS '\0'
#endif

#define NULLATOM ((atom_t)0)

#define ARG_ERROR	0
#define ARG_OK		1
#define ARG_HOSTDATA	2

#ifdef __GNUC__
#define DynamicVector(name, type, size)	type name[size]
#else
#define DynamicVector(name, type, size)	type *name = \
				  (type *) alloca((size)*sizeof(type))
#endif

typedef int (*OnExitFunction)(int rc);	/* HOST_ONEXIT function */

		 /*******************************
		 *    PROLOG ELEMENTARY TYPES	*
		 *******************************/

#ifdef SICSTUS
#include <sicstus/sicstus.h>

typedef unsigned long	atom_t;
typedef SP_term_ref	term_t;
typedef int		foreign_t;
typedef atom_t		Module;
typedef SP_pred_ref	Predicate;
typedef SP_term_ref	Fid;

#define AtomFromString(s)	SP_atom_from_string(s)
#define ModuleFromAtom(a)	((Module) (a))
#define ModuleName(m)		((atom_t) (m))
#endif /*SICSTUS*/

#ifdef SWI

typedef record_t	Record;
typedef module_t	Module;
typedef predicate_t	Predicate;
typedef functor_t	Functor;
typedef fid_t		Fid;

#define AtomFromString(s)	PL_new_atom(s)
#define ModuleFromAtom(a)	PL_new_module(a)
#define ModuleName(m)		PL_module_name(m)
#endif /*SWI*/

typedef struct
{ PL_atomic_t	method_id;		/* Identifier of the method */
  functor_t	functor;		/* Functor for the arguments */
  int		flags;			/* debugging, etc */
  int		argc;			/* #arguments */
} prolog_call_data;


		 /*******************************
		 *	    PROTOTYPES		*
		 *******************************/

static PceObject	termToObject(term_t t, PceType type,
				     atom_t assoc, int new);
static prolog_call_data *get_pcd(PceObject method);
static int		put_object(term_t t, PceObject obj);
static int		put_trace_info(term_t id, prolog_call_data *pm);
       foreign_t	pl_pce_init(term_t home, term_t appdir);
static Module		pceContextModule();
static void		makeClassProlog();
static term_t		getTermHandle(PceObject hd);
static PceType		cToPceType(const char *name);


static foreign_t	pl_new(term_t assoc, term_t descr);
static foreign_t	pl_send(term_t rec, term_t msg);
static foreign_t	pl_send_class(term_t rec, term_t cl, term_t msg);
static foreign_t	pl_get(term_t rec, term_t msg, term_t ret);
static foreign_t	pl_get_class(term_t rec, term_t cl, term_t msg, term_t r);
static foreign_t	pl_object1(term_t ref);
static foreign_t	pl_object2(term_t ref, term_t description);
static foreign_t	pl_pce_method_implementation(term_t id, term_t msg);
static foreign_t	pl_pce_open(term_t t, term_t mode, term_t plhandle);
static foreign_t	pl_pce_postscript_stream(term_t ps);

extern install_t	install_pcecall(void);


		 /*******************************
		 *	      PROFILER		*
		 *******************************/

static int prof_active;			/* activated */
static PL_prof_type_t pceProfType;	/* registration */
static int pce_initialised = FALSE;	/* library is initialiased */

		 /*******************************
		 *	       CONTEXT		*
		 *******************************/

static Module	 DefaultModule;		/* For module handling */
static PceObject DefaultContext;	/* Executing context */

		 /*******************************
		 *	     CONSTANTS		*
		 *******************************/

static atom_t ATOM_append;		/* "append" */
static atom_t ATOM_argument;		/* "argument" */
static atom_t ATOM_argument_count;	/* "argument_count" */
static atom_t ATOM_assign;		/* ":=" */
static atom_t ATOM_badIntegerReference;	/* "bad_integer_reference" */
static atom_t ATOM_badList;		/* "bad_list" */
static atom_t ATOM_badObjectDescription;	/* "bad_object_description" */
static atom_t ATOM_badReference;		/* "bad_reference" */
static atom_t ATOM_badSelector;		/* "bad_selector" */
static atom_t ATOM_badStringArgument;	/* "bad_string_argument" */
static atom_t ATOM_behaviour;		/* "behaviour" */
static atom_t ATOM_context;		/* "context" */
static atom_t ATOM_default;		/* "default" */

#ifndef ATOM_dot
static atom_t ATOM_dot;			/* "." */
#endif
static atom_t ATOM_domain_error;		/* "domain_error" */
static atom_t ATOM_error;			/* "error" */
static atom_t ATOM_existence_error;	/* "existence_error" */
static atom_t ATOM_get;			/* "get" */
static atom_t ATOM_initialisation;	/* "initialisation" */
static atom_t ATOM_instantiation_error;	/* "instantiation_error" */
static atom_t ATOM_io_mode;		/* "io_mode" */
static atom_t ATOM_module;		/* ":" */
static atom_t ATOM_named_argument;	/* "named_argument" */
static atom_t ATOM_named_reference;	/* "named_reference" */
static atom_t ATOM_new;			/* "new" */
static atom_t ATOM_object;		/* "object" */
static atom_t ATOM_open;		/* "open" */
static atom_t ATOM_pce;			/* "pce" */
static atom_t ATOM_permission_error;	/* "permission_error" */
static atom_t ATOM_procedure;		/* "procedure" */
static atom_t ATOM_proper_list;		/* "proper_list" */
static atom_t ATOM_read;		/* "read" */
static atom_t ATOM_ref;			/* "@" */
static atom_t ATOM_send;		/* "send" */
static atom_t ATOM_slash;		/* "/" */
static atom_t ATOM_spy;			/* "spy" */
static atom_t ATOM_string;		/* "string" */
static atom_t ATOM_trace;		/* "trace" */
static atom_t ATOM_true;		/* "true" */
static atom_t ATOM_type_error;		/* "type_error" */
static atom_t ATOM_unknownReference;	/* "unknown_reference" */
static atom_t ATOM_update;		/* "update" */
static atom_t ATOM_user;		/* "user" */
static atom_t ATOM_write;		/* "write" */
static atom_t ATOM_prolog;		/* "prolog" */
static atom_t ATOM_class;		/* "class" */
static atom_t ATOM_unwind;		/* "unwind" */

static Module MODULE_user;		/* Handle for user-module */

static void
initPrologConstants()
{ ATOM_append			= AtomFromString("append");
  ATOM_argument			= AtomFromString("argument");
  ATOM_argument_count		= AtomFromString("argument_count");
  ATOM_assign			= AtomFromString(":=");
  ATOM_badIntegerReference	= AtomFromString("bad_integer_reference");
  ATOM_badList			= AtomFromString("bad_list");
  ATOM_badObjectDescription	= AtomFromString("bad_object_description");
  ATOM_badReference		= AtomFromString("bad_reference");
  ATOM_badSelector		= AtomFromString("bad_selector");
  ATOM_badStringArgument	= AtomFromString("bad_string_argument");
  ATOM_behaviour		= AtomFromString("behaviour");
  ATOM_context			= AtomFromString("context");
  ATOM_default			= AtomFromString("default");
#ifndef ATOM_dot
  ATOM_dot			= AtomFromString(".");
#endif
  ATOM_domain_error		= AtomFromString("domain_error");
  ATOM_error			= AtomFromString("error");
  ATOM_existence_error		= AtomFromString("existence_error");
  ATOM_get			= AtomFromString("get");
  ATOM_initialisation		= AtomFromString("initialisation");
  ATOM_instantiation_error	= AtomFromString("instantiation_error");
  ATOM_io_mode			= AtomFromString("io_mode");
  ATOM_module			= AtomFromString(":");
  ATOM_named_argument		= AtomFromString("named_argument");
  ATOM_named_reference		= AtomFromString("named_reference");
  ATOM_new			= AtomFromString("new");
  ATOM_object			= AtomFromString("object");
  ATOM_open			= AtomFromString("open");
  ATOM_pce			= AtomFromString("pce");
  ATOM_permission_error		= AtomFromString("permission_error");
  ATOM_procedure		= AtomFromString("procedure");
  ATOM_proper_list		= AtomFromString("proper_list");
  ATOM_read			= AtomFromString("read");
  ATOM_ref			= AtomFromString("@");
  ATOM_send			= AtomFromString("send");
  ATOM_slash			= AtomFromString("/");
  ATOM_spy			= AtomFromString("spy");
  ATOM_string			= AtomFromString("string");
  ATOM_trace			= AtomFromString("trace");
  ATOM_true			= AtomFromString("true");
  ATOM_type_error		= AtomFromString("type_error");
  ATOM_unknownReference		= AtomFromString("unknown_reference");
  ATOM_update			= AtomFromString("update");
  ATOM_user			= AtomFromString("user");
  ATOM_write			= AtomFromString("write");
  ATOM_prolog			= AtomFromString("prolog");
  ATOM_class			= AtomFromString("class");
  ATOM_unwind			= AtomFromString("unwind");

  MODULE_user			= ModuleFromAtom(ATOM_user);
}

static PceObject	NIL;		/* @nil */
static PceObject	DEFAULT;	/* @default */
static PceObject	PROLOG;		/* @prolog */
static PceClass		ClassBinding;	/* class(:=) */
static PceClass		ClassProlog;	/* class(prolog_term, host_data) */
static PceType		ClassType;	/* class(type) */
static PceType		TypeProlog;	/* prolog_term|atomic */
static PceType		TypePrologTerm;	/* type representing above */
static PceType		TypeInt;	/* Int type */
static PceType		TypeReal;	/* real type */
static PceName		NAME_functor;	/* "functor" */
static PceName		NAME_Arity;	/* "_arity" */
static PceName		NAME_Arg;	/* "_arg" */
static PceName		NAME_user;	/* "user" */
static PceName		NAME_includes;	/* "includes" */
static PceName		NAME_chain;	/* "chain" */
static PceName		NAME_vector;	/* "vector" */
static PceName		NAME_codeVector;/* "code_vector" */

#define cToPceName(s) cToPceName_nA(s, strlen(s))

static void
initPceConstants()
{ NAME_functor	  = cToPceName("functor");
  NAME_Arity	  = cToPceName("_arity");
  NAME_Arg	  = cToPceName("_arg");
  NAME_user       = cToPceName("user");
  NAME_includes   = cToPceName("includes");
  NAME_chain      = cToPceName("chain");
  NAME_vector     = cToPceName("vector");
  NAME_codeVector = cToPceName("code_vector");

  NIL		  = cToPceAssoc("nil");
  DEFAULT	  = cToPceAssoc("default");
  PROLOG	  = cToPceAssoc("host");

  ClassBinding    = cToPceAssoc(":=_class");	/* not so nice! */
  ClassType       = cToPceAssoc("type_class");	/* not so nice! */
  assert(ClassBinding);

  TypeInt	  = cToPceType("int");
  TypeReal	  = cToPceType("real");

  makeClassProlog();
}


static PceType
cToPceType(const char *name)
{ PceObject av[1];
  PceType t;

  av[0] = cToPceName(name);
  t = pceNew(NIL, ClassType, 1, av);
  assert(t);

  return t;
}


		 /*******************************
		 *	   SICSTUS GLUE		*
		 *******************************/

#ifdef SICSTUS
#define GET_FUNCTOR_BUG 1

#define FUNCTOR_pce1		ATOM_pce, 1
#define	FUNCTOR_pce2		ATOM_pce, 2
#define	FUNCTOR_pce3		ATOM_pce, 3
#define	FUNCTOR_ref1		ATOM_ref, 1
#define	FUNCTOR_new1		ATOM_new, 1
#define	FUNCTOR_string1		ATOM_string, 1
#define	FUNCTOR_module2		ATOM_module, 2
#define FUNCTOR_namearg		ATOM_assign, 2
#define FUNCTOR_error2		ATOM_error, 2
#define FUNCTOR_domain_error2	ATOM_domain_error, 2

#define initHostConstants()

#define AtomCharp(a)		SP_string_from_atom((a))
#define GetInteger(a, i)	SP_get_integer((a), (i))
#define GetAtom(a, n)		SP_get_atom((a), (n))
#define GetFloat(a, f)		SP_get_float((a), (f))
#define PL_new_term_ref()	SP_new_term_ref()
#ifdef GET_FUNCTOR_BUG
#define GetNameArity(t, n, a)	((SP_is_compound(t) || SP_is_atom(t)) && \
				 SP_get_functor((t), (n), (a)))
#else
#define GetNameArity(t, n, a)	SP_get_functor((t), (n), (a))
#endif
#define IsVar(t)		SP_is_variable(t)
#define GetArg(n, t, a)		SP_get_arg((n), (t), (a))
#define PutFunctor(t, n, a)	SP_put_functor((t), (n), (a))
#define PL_put_atom_chars(t, s)	SP_put_string((t), (s))
#define PL_put_integer(t, i)	SP_put_integer((t), (i))
#define PL_put_float(t, f)	SP_put_float((t), (f))
#define PL_unify(t1, t2)	SP_unify((t1), (t2))
#define PL_put_atom(t, a)	SP_put_atom((t), (a))
#define PutTerm(t, f)		SP_put_term((t), (f))
#define FindPredicate(n, a, m)	SP_pred(n, a, m)
#define OpenForeign()		SP_new_term_refs(0)
#define CloseForeign(fid)	SP_reset_term_refs(fid)
#define InstallPredicate(n, a, f, flags)

#if defined(__WINDOWS__)
#define PROLOG_INSTALL_DISPATCH_FUNCTION(f) {}
#else
#define PROLOG_INSTALL_DISPATCH_FUNCTION(f) \
				{ SP_read_hook = f; }
#endif

#define PROLOG_INSTALL_RESET_FUNCTION(f) \
				{ SP_set_reinit_hook(f); }

static int
GetChars(term_t t, char **s, unsigned int *len)
{ if ( SP_get_string(t, s) ||
       SP_get_list_chars(t, s) ||
       SP_get_number_chars(t, s) )
  { *len = strlen(*s);
    return TRUE;
  }

  return FALSE;
}


#ifndef SWI

static int
StripModuleTag(term_t t, atom_t *module, term_t p)
{ atom_t name;
  size_t arity;
  term_t a = PL_new_term_ref();

  PutTerm(p, t);

  while( GetNameArity(p, &name, &arity) &&
	 name == ATOM_module && arity == 2 )
  { atom_t m;

    QGetArg(1, p, a);
    if ( GetAtom(a, &m) )
    { *module = m;
      QGetArg(2, p, p);
    } else
      break;
  }

  return TRUE;
}

#endif /*~SWI*/

static void
UndefinedPredicate(atom_t pred, int arity, atom_t module)
{ term_t et = PL_new_term_ref();			/* existence_error(G, 0, ...) */
  term_t goal = PL_new_term_ref();		/* foo(_, _) */
  term_t fa = PL_new_term_ref();			/* foo/2 */
  term_t culprit = PL_new_term_ref();		/* Module:foo/2 */
  term_t zero = PL_new_term_ref();		/* 0 */
  term_t id = PL_new_term_ref();			/* procedure */
  term_t name = PL_new_term_ref();		/* foo */
  term_t ar = PL_new_term_ref();			/* 2 */
  term_t m = PL_new_term_ref();			/* Module */

  PutFunctor(goal, pred, arity);
  PL_put_integer(zero, 0);
  PL_put_atom(id, ATOM_procedure);
  PL_put_atom(name, pred);
  PL_put_atom(m, module);
  PL_put_integer(ar, arity);
  PL_cons_functor(fa, ATOM_slash, 2, name, ar);
  PL_cons_functor(culprit, ATOM_module, 2, m, fa);
  PL_cons_functor(et, ATOM_existence_error, 5, goal, zero, id, culprit, zero);

  SP_raise_exception(et);
}

#endif /*SICSTUS*/

		 /*******************************
		 *	   SWI-Prolog GLUE	*
		 *******************************/

#ifdef SWI
#define O_STRING 1
#define HAVE_XPCEREF 1			/* _PL_put/get/unify_xpce_reference */

static Functor	FUNCTOR_pce1;
static Functor	FUNCTOR_pce2;
static Functor	FUNCTOR_pce3;
static Functor	FUNCTOR_context2;
static Functor	FUNCTOR_ref1;
static Functor	FUNCTOR_new1;
static Functor	FUNCTOR_string1;
static Functor	FUNCTOR_module2;
static Functor  FUNCTOR_spy1;
static Functor  FUNCTOR_trace1;
static Functor  FUNCTOR_namearg;
static Functor  FUNCTOR_error2;
static Functor  FUNCTOR_send2;
static Functor  FUNCTOR_get2;
static Functor  FUNCTOR_existence_error2;
static Functor  FUNCTOR_permission_error3;
static Functor  FUNCTOR_type_error2;
static Functor  FUNCTOR_domain_error2;
static Functor  FUNCTOR_behaviour1;
static Functor  FUNCTOR_unwind1;
static predicate_t PREDICATE_send_implementation;
static predicate_t PREDICATE_get_implementation;

static PL_dispatch_hook_t	old_dispatch_hook;

#define AtomCharp(a)		PL_atom_chars((a))
#if SIZEOF_VOIDP == 8 && SIZEOF_VOIDP != SIZEOF_LONG
#define GetInteger(a, i)	PL_get_int64((a), (i))
#else
#define GetInteger(a, i)	PL_get_intptr((a), (intptr_t*)(i))
#endif
#define GetAtom(a, n)		PL_get_atom((a), (n))
#define GetFloat(a, f)		PL_get_float((a), (f))
#define CopyTerm(t)		PL_copy_term_ref(t)
#define GetNameArity(t, n, a)	PL_get_name_arity((t), (n), (a))
#define IsVar(t)		PL_is_variable(t)
#define GetArg(n, t, a)		PL_get_arg((n), (t), (a))
#define QGetArg(n, t, a)	_PL_get_arg((n), (t), (a))
#define IsFunctor(t, f)		PL_is_functor((t), (f))
#define PutFunctor(t, n, a)	PL_put_functor((t), PL_new_functor((n), (a)))
#define PutTerm(t, f)		PL_put_term((t), (f))
#define UnifyAtom(t, a)		PL_unify_atom((t), (a))
#define UnifyFloat(t, a)	PL_unify_float((t), (a))
#if SIZEOF_VOIDP == 8 && SIZEOF_VOIDP != SIZEOF_LONG
#define UnifyInteger(t, a)	PL_unify_int64((t), (a))
#define PutInteger(t, a)	PL_put_int64((t), (a))
#else
#define UnifyInteger(t, a)	PL_unify_integer((t), (a))
#define PutInteger(t, a)	PL_put_integer((t), (a))
#endif
#define PutVar(t)		PL_put_variable((t))
#define StripModuleTag(t, m, p)	PL_strip_module((t), (m), (p))
#define FindPredicate(n, a, m)	PL_pred(PL_new_functor(n, a), m)
#define OpenForeign()		PL_open_foreign_frame()
#define CloseForeign(fid)	PL_close_foreign_frame(fid)
#define DebugMode		(pceExecuteMode() == PCE_EXEC_USER \
					? PL_Q_NORMAL : PL_Q_NODEBUG)

#if defined(__WINDOWS__)
#define PROLOG_INSTALL_DISPATCH_FUNCTION(f) {}
#else
#define PROLOG_INSTALL_DISPATCH_FUNCTION(f) \
	(old_dispatch_hook = PL_dispatch_hook(f))
#endif

#define PROLOG_INSTALL_RESET_FUNCTION(f) \
				{ PL_abort_hook(f); }

#define PROLOG_DISPATCH_INPUT   PL_DISPATCH_INPUT
#define PROLOG_DISPATCH_TIMEOUT PL_DISPATCH_TIMEOUT

static void
initHostConstants()
{ FUNCTOR_behaviour1        = PL_new_functor(ATOM_behaviour, 1);
  FUNCTOR_error2	    = PL_new_functor(ATOM_error, 2);
  FUNCTOR_existence_error2  = PL_new_functor(ATOM_existence_error, 2);
  FUNCTOR_get2		    = PL_new_functor(ATOM_get, 2);
  FUNCTOR_module2	    = PL_new_functor(ATOM_module, 2);
  FUNCTOR_namearg	    = PL_new_functor(ATOM_assign, 2);
  FUNCTOR_context2	    = PL_new_functor(ATOM_context, 2);
  FUNCTOR_pce1		    = PL_new_functor(ATOM_pce, 1);
  FUNCTOR_pce2		    = PL_new_functor(ATOM_pce, 2);
  FUNCTOR_pce3		    = PL_new_functor(ATOM_pce, 3);
  FUNCTOR_permission_error3 = PL_new_functor(ATOM_permission_error, 3);
  FUNCTOR_ref1		    = PL_new_functor(ATOM_ref, 1);
  FUNCTOR_new1		    = PL_new_functor(ATOM_new, 1);
  FUNCTOR_send2		    = PL_new_functor(ATOM_send, 2);
  FUNCTOR_spy1		    = PL_new_functor(ATOM_spy, 1);
  FUNCTOR_string1	    = PL_new_functor(ATOM_string, 1);
  FUNCTOR_trace1	    = PL_new_functor(ATOM_trace, 1);
  FUNCTOR_type_error2       = PL_new_functor(ATOM_type_error, 2);
  FUNCTOR_domain_error2     = PL_new_functor(ATOM_domain_error, 2);
  FUNCTOR_unwind1	    = PL_new_functor(ATOM_unwind, 1);

  PREDICATE_send_implementation = PL_predicate("send_implementation", 3,
					       "pce_principal");
  PREDICATE_get_implementation = PL_predicate("get_implementation", 4,
					       "pce_principal");
}


#ifdef __WINDOWS__
#include <console.h>

static RlcUpdateHook old_update_hook;

static void *
getConsoleFunction(const char *name)
{ HMODULE hconsole;

  if ( (hconsole=GetModuleHandle("plterm")) )
  { return GetProcAddress(hconsole, name);
  }

  return NULL;
}


RlcUpdateHook
indirect_rlc_update_hook(RlcUpdateHook hook)
{ RlcUpdateHook (*sethook)(RlcUpdateHook new);

  if ( (sethook = getConsoleFunction("rlc_update_hook")) )
    return (*sethook)(hook);

  return NULL;
}


static HWND
indirect_rlc_hwnd()
{ HWND (*f)(void *console);

  if ( (f = getConsoleFunction("rlc_hwnd")) )
    return (*f)(NULL);

  return 0;
}


#define PROLOG_ITF_INIT() \
	{ }
#define PROLOG_INSTALL_REDRAW_FUNCTION(f) \
	{ old_update_hook = indirect_rlc_update_hook(f); }
#ifndef O_SHAREDLIBRARY
#define O_SHAREDLIBRARY
#endif
#endif

#ifdef O_SHAREDLIBRARY
#ifndef PROLOG_ITF_INIT
#define PROLOG_ITF_INIT() { }
#endif
#define PROLOG_ONEXIT(f)	{ exitpce_hook = (OnExitFunction) f; }

static OnExitFunction		exitpce_hook;

install_t
install_pl2xpce(void)
{ if ( pce_initialised )
    return;
  pce_initialised = TRUE;

  PL_register_foreign("pce_init", 2,
		      pl_pce_init, PL_FA_TRANSPARENT);
  PL_register_foreign("send", 2,
		      pl_send, PL_FA_TRANSPARENT);
  PL_register_foreign("get", 3,
		      pl_get, PL_FA_TRANSPARENT);
  PL_register_foreign("send_class", 3,
		      pl_send_class, PL_FA_TRANSPARENT);
  PL_register_foreign("get_class", 4,
		      pl_get_class, PL_FA_TRANSPARENT);
  PL_register_foreign("object", 1,
		      pl_object1, 0);
  PL_register_foreign("object", 2,
		      pl_object2, 0);
  PL_register_foreign("new", 2,
		      pl_new, PL_FA_TRANSPARENT);
  PL_register_foreign("pce_method_implementation", 2,
		      pl_pce_method_implementation, 0);
  PL_register_foreign("pce_open", 3,
		      pl_pce_open, 0);
  PL_register_foreign("pce_postscript_stream", 1,
		      pl_pce_postscript_stream, 0);

#ifndef __WINDOWS__
  PL_license("lgplv2+", "xpce (drag&drop library by Paul Sheer)");
#endif

  install_pcecall();
}

install_t
uninstall_pl2xpce(void)
{ if ( !pce_initialised )
    return;
  pce_initialised = FALSE;

  DEBUG(Sdprintf("Removing hooks (%p and %p)\n",
		 old_dispatch_hook, old_update_hook));

  PL_dispatch_hook(old_dispatch_hook);
#ifdef __WINDOWS__
  indirect_rlc_update_hook(old_update_hook);
#endif
  if ( exitpce_hook )
    (*exitpce_hook)(0);
}

#endif /*O_SHAREDLIBRARY*/

#endif /*SWI*/

		 /*******************************
		 *         EXCEPTIONS		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
XPCE calls raising errors are mapped into Prolog exceptions if the error
is directly related to an interface call.   The  error term is compliant
with the normal Prolog error-exceptions: error(Kind, Context).

Defined error Kinds:

	type_error(pce(Type), Actual)
	type_error(pce(too_many_arguments), N)
	existence_error(object, Ref)
+	existence_error(behaviour, send(Ref, Selector))
+	existence_error(behaviour, get(Ref, Selector))
	existence_error(named_argument,  Name)
	existence_error(argument, ArgN)

Defined context terms

	send(Obj, Msg)
	get(Obj, Msg)
	behaviour(Impl)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define EX_GOAL				1 /* goal, receiver, message */
#define EX_BAD_INTEGER_OBJECT_REF	2 /* <integer> */
#define EX_BAD_ATOM_OBJECT_REF		3 /* <atom> */
#define EX_BAD_OBJECT_REF		4 /* <term> */
#define EX_TYPE				5 /* <type-name>, <term> */
#define EX_INSTANTIATION		6 /* <nothing> */
#define EX_DOMAIN			7 /* <domain-name>, <term> */
#define EX_PERMISSION			8 /* op, type, obj, msg */
#define EX_TOO_MANY_ARGUMENTS		9
#define EX_EXISTENCE		       10 /* <type>, <term> */

static int
put_goal_context(term_t ctx, PceGoal g, va_list args)
{ if ( g->flags & (PCE_GF_SEND|PCE_GF_GET) )
  { term_t rec = va_arg(args, term_t);
    term_t msg = va_arg(args, term_t);

    if ( g->flags & PCE_GF_SEND )
      return PL_cons_functor(ctx, FUNCTOR_send2, rec, msg);
    else
      return PL_cons_functor(ctx, FUNCTOR_get2, rec, msg);
  } else				/* new/2 */
  { term_t descr = va_arg(args, term_t);

    return PL_cons_functor(ctx, FUNCTOR_new1, descr);
  }
}


static int
add_list(PceObject e, void *closure)
{ term_t tail = ((term_t *)closure)[0];
  term_t head = ((term_t *)closure)[1];
  term_t tmp  = ((term_t *)closure)[2];

  return ( PL_unify_list(tail, head, tail) &&
	   put_object(tmp, e) &&
	   PL_unify(head, tmp)
	 );
}


static int
ThrowException(int id, ...)
{ va_list args;
  fid_t fid;
  term_t et;
  term_t err;
  term_t ctx;

  if ( !(fid = PL_open_foreign_frame()) )
    return FALSE;

  et  = PL_new_term_ref();	/* the error term */
  err = PL_new_term_ref();	/* the 1-st argument */
  ctx = PL_new_term_ref();	/* the 2-nd (context) argument */

  va_start(args, id);
  switch(id)
  { case EX_GOAL:			/* goal, receiver, message */
    { PceGoal g = va_arg(args, PceGoal);

      switch( g->errcode )
      { case PCE_ERR_ERROR:
	{ term_t a1 = PL_new_term_ref();
	  term_t a2 = PL_new_term_ref();
	  term_t l[3];
	  l[0] = PL_copy_term_ref(a2);
	  l[1] = PL_new_term_ref();
	  l[2] = PL_new_term_ref();

	  if ( !put_object(a1, g->errc1) || /* error->id */
	       !pceEnumElements(g->errc2, add_list, (void *)l) ||
	       !PL_unify_nil(l[0]) )	/* the tail */
	    goto error;

	  if ( !PL_cons_functor(err, FUNCTOR_pce2, a1, a2) ||
	       !put_goal_context(ctx, g, args) )
	    goto error;
	  break;
	}
	default:
	  assert(0);
      }
      break;
    }
    case EX_BAD_INTEGER_OBJECT_REF:		/* , <integer> */
    { intptr_t ref = va_arg(args, intptr_t);
      char *descr = pcePPReference(cToPceInteger(ref));
      term_t a1 = PL_new_term_ref();
      term_t a2 = PL_new_term_ref();
      term_t na = PL_new_term_ref();

      PL_put_atom(a1, ATOM_object);
      if ( !PL_cons_functor(a1, FUNCTOR_pce1, a1) ||
	   !PL_put_int64(a2, ref) ||
	   !PL_cons_functor(a2, FUNCTOR_ref1, a2) ||
	   !PL_cons_functor(err, FUNCTOR_existence_error2, a1, a2) )
	goto error;

      if ( descr[0] == '@' )
      { char *s;

	for(s=&descr[1]; *s && isdigit(*s&0xff); s++)
	  ;
	if ( *s )
	{ if ( !PL_put_atom_chars(ctx, descr) || /* context(_, Message) */
	       !PL_cons_functor(ctx, FUNCTOR_context2, na, ctx) )
	    goto error;
	}
      }

      break;
    }
    case EX_BAD_ATOM_OBJECT_REF:		/* , <name> */
    { atom_t ref = va_arg(args, atom_t);
      term_t a1 = PL_new_term_ref();
      term_t a2 = PL_new_term_ref();

      PL_put_atom(a1, ATOM_object);
      PL_put_atom(a2, ref);
      if ( !PL_cons_functor(a1, FUNCTOR_pce1, a1) ||
	   !PL_cons_functor(a2, FUNCTOR_ref1, a2) ||
	   !PL_cons_functor(err, FUNCTOR_existence_error2, a1, a2) )
	goto error;
      break;
    }
    case EX_BAD_OBJECT_REF:			/* not @<name-or-int> */
    { term_t ref = va_arg(args, term_t);
      term_t a1  = PL_new_term_ref();

      PL_put_atom(a1, ATOM_object);
      if ( !PL_cons_functor(a1, FUNCTOR_pce1, a1) ||
	   !PL_cons_functor(err, FUNCTOR_type_error2, a1, ref) )
	goto error;
      break;
    }
    case EX_TYPE:				/* type-name, arg */
    { term_t a1 = PL_new_term_ref();
      atom_t tn = va_arg(args, atom_t);
      term_t v  = va_arg(args, term_t);

      if ( PL_is_variable(v) )
	goto ex_instantiation;

      PL_put_atom(a1, tn);
      if ( !PL_cons_functor(a1, FUNCTOR_pce1, a1) ||
	   !PL_cons_functor(err, FUNCTOR_type_error2, a1, v) )
	goto error;
      break;
    }
    case EX_EXISTENCE:				/* type-name, arg */
    { term_t a1 = PL_new_term_ref();
      atom_t tn = va_arg(args, atom_t);
      term_t v  = va_arg(args, term_t);

      if ( PL_is_variable(v) )
	goto ex_instantiation;

      PL_put_atom(a1, tn);
      if ( !PL_cons_functor(a1, FUNCTOR_pce1, a1) ||
	   !PL_cons_functor(err, FUNCTOR_existence_error2, a1, v) )
	goto error;
      break;
    }
    case EX_INSTANTIATION:			/* No arguments */
    ex_instantiation:
    { PL_put_atom(err, ATOM_instantiation_error);

      break;
    }
    case EX_DOMAIN:				/* domain-name, arg */
    { term_t a1 = PL_new_term_ref();
      atom_t tn = va_arg(args, atom_t);
      term_t v  = va_arg(args, term_t);

      PL_put_atom(a1, tn);
      if ( !PL_cons_functor(err, FUNCTOR_domain_error2, a1, v) )
	goto error;
      break;
    }
    case EX_PERMISSION:
    { term_t a1 = PL_new_term_ref();
      term_t a2 = PL_new_term_ref();
      term_t a3 = PL_new_term_ref();
      atom_t op = va_arg(args, atom_t);
      atom_t tp = va_arg(args, atom_t);
      PceObject obj = va_arg(args, PceObject);
      atom_t msg = va_arg(args, atom_t);

      PL_put_atom(a1, op);
      PL_put_atom(a2, tp);
      if ( !put_object(a3, obj) ||
	   !PL_cons_functor(err, FUNCTOR_permission_error3, a1, a2, a3) )
	goto error;

      PutVar(a1);
      PL_put_atom(a2, msg);
      if ( !PL_cons_functor(ctx, FUNCTOR_context2, a1, a2) )
	goto error;
      break;
    }
    default:
      assert(0);
  }
  va_end(args);

  if ( !PL_cons_functor(et, FUNCTOR_error2, err, ctx) )
    goto error;

  return PL_raise_exception(et);

error:
  va_end(args);
  PL_close_foreign_frame(fid);
  return FALSE;
}


		 /*******************************
		 *	     PRIMITIVES		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PceName	atomToName(atom_t a)
	Translate a Prolog atom into an XPCE name object.  This version
	uses the XPCE handle table to cache direct associations between
	XPCE names and Prolog atoms.

PceObject referenceToObject(term_t a)
	a is the argument to @/1.  Translate to an XPCE object or raise
	an error.  This function too caches using the
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#include "table.c"

static atom_t
nameToAtom(PceName name)
{ size_t len;
  const char *textA;
  const wchar_t *textW;

  if ( (textA = pceCharArrayToCA(name, &len)) )
    return PL_new_atom_nchars(len, textA);
  else if ( (textW = pceCharArrayToCW(name, &len)) )
    return PL_new_atom_wchars(len, textW);
  else
    return (atom_t)0;
}


static __inline PceName
atomToAssoc(atom_t a)
{ return a ? atomToName(a) : NIL;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Get an XPCE object-reference from a Prolog term we already know to be of
the form @/1.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
get_object_from_refterm(term_t t, PceObject *obj)
{ term_t a = PL_new_term_ref();
  PceObject o;
  intptr_t r;
  atom_t name;

  _PL_get_arg(1, t, a);

  if ( PL_get_intptr(a, &r) )
  { if ( (o = cToPceReference(r)) )
    { *obj = o;

      return TRUE;
    }

    return ThrowException(EX_BAD_INTEGER_OBJECT_REF, r);
  }

  if ( GetAtom(a, &name) )
  { if ( (o = pceObjectFromName(atomToName(name))) )
    { *obj = o;

      return TRUE;
    }

    return ThrowException(EX_BAD_ATOM_OBJECT_REF, name);
  }

  return ThrowException(EX_BAD_OBJECT_REF, t);
}


static int
unifyReferenceArg(term_t t, int type, PceCValue value)
{ term_t t2 = PL_new_term_ref();	/* Exploit SWI-Prolog PL_unify-* */

  if ( type == PCE_REFERENCE )
  { if ( !PutInteger(t2, value.integer) )
      return FALSE;
  } else
  { PceITFSymbol symbol = value.itf_symbol;

    PL_put_atom(t2, CachedNameToAtom(symbol->name));
  }

  return PL_unify(t, t2);
}


static int
unifyReference(term_t t, int type, PceCValue value)
{
#ifdef HAVE_XPCEREF
  xpceref_t r;

  if ( type == PCE_REFERENCE )
  { r.type = PL_INTEGER;
    r.value.i = value.integer;
  } else
  { PceITFSymbol symbol = value.itf_symbol;

    r.type = PL_ATOM;
    r.value.a = CachedNameToAtom(symbol->name);
  }
  return _PL_unify_xpce_reference(t, &r);

#else /*HAVE_XPCEREF*/

  term_t t2 = PL_new_term_ref();
  term_t r  = PL_new_term_ref();

  if ( type == PCE_REFERENCE )
  { PL_put_integer(t2, value.integer);
  } else
  { PceITFSymbol symbol = value.itf_symbol;

    PL_put_atom(t2, CachedNameToAtom(symbol->name));
  }
  PL_cons_functor(r, FUNCTOR_ref1, t2);

  return PL_unify(t, r);
#endif /*HAVE_XPCEREF*/
}


		 /*******************************
		 *	   TERM-TO-OBJECT	*
		 *******************************/

static PceObject
do_new(term_t ref, term_t t)
{ PceObject rval;

  if ( IsVar(ref) )
  { if ( (rval = termToObject(t, NULL, NULLATOM, TRUE)) )
    { PceCValue value;
      int type = pceToCReference(rval, &value);

      if ( unifyReference(ref, type, value) )
	return rval;
    }

    return PCE_FAIL;
  } else if ( IsFunctor(ref, FUNCTOR_ref1) )
  { term_t a = PL_new_term_ref();
    atom_t assoc;

    QGetArg(1, ref, a);

    if ( !GetAtom(a, &assoc) )		/* new(@foo, ...) */
    { if ( IsVar(a) )
	assoc = 0;			/* new(@X, ...) */
      else
	goto error;
    }

    if ( (rval = termToObject(t, NULL, assoc, TRUE)) )
    { PceCValue value;
      int type = pceToCReference(rval, &value);

      if ( unifyReferenceArg(a, type, value) )
	return rval;
    }

    return PCE_FAIL;
  }

error:
  ThrowException(EX_TYPE, ATOM_named_reference, ref);
  return PCE_FAIL;
}

		 /*******************************
		 *      HOST DATA HANDLES	*
		 *******************************/

typedef struct _host_stack_entry
{ PceObject handle;
  struct _host_stack_entry *previous;
} host_stack_entry, *HostStackEntry;

static HostStackEntry host_handle_stack;

static __inline PceObject
pushHostHandle(PceObject h)
{ HostStackEntry e = pceAlloc(sizeof(*e));

  e->handle   = h;
  e->previous = host_handle_stack;
  host_handle_stack = e;

  return h;
}


static __inline void
rewindHostHandles(HostStackEntry top)
{ if ( top != host_handle_stack )
  { HostStackEntry p, e = host_handle_stack;

    for( ; e && e != top; e = p )
    { p = e->previous;

      if ( !freeHostData(e->handle) )
      { term_t t = getTermHandle(e->handle);
	Record r = PL_record(t);

	assert((((uintptr_t)r & 0x1L) == 0L));
	setHostDataHandle(e->handle, r);
      }

      pceUnAlloc(sizeof(*e), e);
    }

    host_handle_stack = top;
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
term_t handles appear in two formats: as direct handles to Prolog terms and
as handles to the Prolog recorded database.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static PceObject
makeTermHandle(term_t t)
{ term_t copy = PL_copy_term_ref(t);
  void *h = (void *)(((uintptr_t)copy<<1) | 0x1L);

  return pushHostHandle(CtoHostData(ClassProlog, h, 0));
}


static PceObject
makeRecordedTermHandle(term_t t)
{ Record r = PL_record(t);

  assert((((uintptr_t)r & 0x1L) == 0L));
  return CtoHostData(ClassProlog, r, PCE_ANSWER);
}


static term_t
getTermHandle(PceObject hd)
{ void *h;

  if ( (h = getHostDataHandle(hd)) )
  { uintptr_t l = (uintptr_t)h;

    if ( l & 1 )
      return (term_t)(l>>1);
    else
    { term_t t = PL_new_term_ref();

      PL_recorded(h, t);
      return t;
    }
  }

  return 0;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Translate a message argument into an XPCE  object.

Returns  FALSE  and  raises  an  exception    of  the  the  argument  is
@<bad-reference>
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
get_object_arg(term_t t, PceObject* obj)
{ term_value_t val;

  switch(PL_get_term_value(t, &val))
  { case PL_ATOM:
      *obj = atomToName(val.a);
      return TRUE;
    case PL_INTEGER:
      if ( val.i >= PCE_MIN_INT && val.i <= PCE_MAX_INT )
	*obj = cToPceInteger((intptr_t)val.i);
      else
	*obj = cToPceReal((double)val.i);
      return TRUE;
    case PL_FLOAT:
      *obj = cToPceReal(val.f);
      return TRUE;
    case PL_TERM:			/* @reference */
      if ( val.t.name == ATOM_ref && val.t.arity == 1 )
	return get_object_from_refterm(t, obj);

      if ( val.t.name == ATOM_assign && val.t.arity == 2 )
      { term_t a = PL_new_term_ref();
	atom_t an;

	QGetArg(1, t, a);
	if ( GetAtom(a, &an) )
	{ PceObject av[2];

	  QGetArg(2, t, a);
	  av[0] = atomToName(an);
	  if ( !get_object_arg(a, &av[1]) )
	    return FALSE;

	  *obj = pceNew(NIL, ClassBinding, 2, av);

	  return TRUE;
	}
      }
    /*FALLTHROUGH*/
    default:
      *obj = makeTermHandle(t);
      return TRUE;
  }
}


static int
get_typed_object(PceGoal g, term_t t, PceType type, PceObject* rval)
{ PceObject obj = PCE_FAIL, obj2;
  term_value_t val;

  switch(PL_get_term_value(t, &val))
  { case PL_ATOM:
      obj = atomToName(val.a);
      break;
    case PL_INTEGER:
      if ( val.i >= PCE_MIN_INT && val.i <= PCE_MAX_INT )
	obj = cToPceInteger(val.i);
      else
	obj = cToPceReal((double)val.i);
      break;
    case PL_FLOAT:
      obj = cToPceReal(val.f);
      break;
    case PL_TERM:			/* @reference */
      if ( val.t.name == ATOM_ref && val.t.arity == 1 )
	get_object_from_refterm(t, &obj);
      break;
  }

  if ( !obj )
  { if ( pceIncludesHostDataType(type, ClassProlog) )
    { *rval = makeTermHandle(t);
      return TRUE;
    }

    if ( !(obj = termToObject(t, type, NULLATOM, FALSE)) )
      return pceSetErrorGoal(g, PCE_ERR_ARGTYPE, makeTermHandle(t));
  }

  if ( (obj2 = pceCheckType(g, type, obj)) )
  { *rval = obj2;
    return TRUE;
  }

  return pceSetErrorGoal(g, PCE_ERR_ARGTYPE, makeTermHandle(t));
}


static __inline PceObject
termToReceiver(term_t t)
{ return termToObject(t, NULL, NULLATOM, FALSE);
}


static int
get_answer_object(PceGoal g, term_t t, PceType type, PceObject *rval)
{ PceObject obj = PCE_FAIL, obj2;
  term_value_t val;

  switch(PL_get_term_value(t, &val))
  { case PL_ATOM:
      obj = atomToName(val.a);
      break;
    case PL_INTEGER:
      if ( val.i >= PCE_MIN_INT && val.i <= PCE_MAX_INT )
	obj = cToPceInteger(val.i);
      else
	obj = cToPceReal((double)val.i);
      break;
    case PL_FLOAT:
      obj = cToPceReal(val.f);
      break;
    case PL_TERM:			/* @reference */
      if ( val.t.name == ATOM_ref && val.t.arity == 1 )
	get_object_from_refterm(t, &obj);
      break;
  }

  if ( !obj )
  { if ( pceIncludesHostDataType(type, ClassProlog) )
    { *rval = makeRecordedTermHandle(t);
      return TRUE;
    }

    if ( !(obj = termToObject(t, type, NULLATOM, FALSE)) )
      return pceSetErrorGoal(g, PCE_ERR_RETTYPE, makeRecordedTermHandle(t));
  }

  if ( (obj2 = pceCheckType(g, type, obj)) )
  { *rval = obj2;
    return TRUE;
  }

  return pceSetErrorGoal(g, PCE_ERR_RETTYPE, makeRecordedTermHandle(t));
}



		 /*******************************
		 *	   CLASS PROLOG		*
		 *******************************/

static int
unlinkProlog(PceObject hd)
{ void *h = getHostDataHandle(hd);

  if ( !((uintptr_t)h & 0x1) )
  { /*Sdprintf("Erasing recorded Prolog term\n");*/
    PL_erase(h);			/* This is a record */
  }

  return PCE_SUCCEED;
}


static PceObject
getPrintNameProlog(PceObject hd)
{ char *buffer = NULL;
  size_t  size   = 0;
  PceObject rval;
  IOSTREAM *s;

  s = Sopenmem(&buffer, &size, "w");
  s->encoding = ENC_WCHAR;
  PL_write_term(s, getTermHandle(hd), 1200, 0);
  Sflush(s);
  rval = cToPceStringW(NIL, (wchar_t *)buffer, size/sizeof(wchar_t), FALSE);
  Sclose(s);

  if ( buffer )
    Sfree(buffer);

  return rval;
}


static int
equalProlog(PceObject p1, PceObject p2)
{ term_t t1 = getTermHandle(p1);
  term_t t2 = getTermHandle(p2);

  if ( !(t2 = getTermHandle(p2)) )
  { atom_t a = nameToAtom(p2);

    if ( a )
    { t2 = PL_new_term_ref();
      PL_put_atom(t2, a);
    } else
      return PCE_FAIL;
  }

  if ( PL_compare(t1, t2) == 0 )
    return PCE_SUCCEED;

  return PCE_FAIL;
}


static void
makeClassProlog()
{ PceObject av[4];
  PceObject supers;

  av[0] = cToPceName("prolog_term");
  av[1] = cToPceName("host_data");
  ClassProlog = pceNew(NIL, cToPceName("class"), 2, av);

  av[0] = cToPceName("none");
  pceSend(ClassProlog, NULL, cToPceName("clone_style"), 1, av);
  pceSendMethod(ClassProlog,			/* The class */
		"unlink",			/* Name of the method */
		NULL,				/* Group */
		0,				/* # arguments */
		"Discard associated term",	/* Summary */
		unlinkProlog);			/* Function */
  pceGetMethod (ClassProlog,			/* The class */
		"print_name",			/* Name of the method */
		NULL,				/* Group */
		"string",			/* Return type */
		0,				/* # arguments */
		"Discard associated term",	/* Summary */
		getPrintNameProlog);		/* Function */

  /* type(prolog, atomic, @default, chain(type(prolog_term))) */

  av[0] = cToPceName("prolog_term");
  av[1] = cToPceName("type");
  TypePrologTerm = pceGet(cToPceAssoc("pce"), NULL, cToPceName("convert"),
			  2, av);
  av[0] = TypePrologTerm;
  supers = pceNew(NIL, cToPceName("chain"), 1, av);

  av[0] = cToPceName("prolog");
  av[1] = cToPceName("atomic");
  av[2] = DEFAULT;
  av[3] = supers;
  TypeProlog = pceNew(NIL, cToPceName("type"), 4, av);

  assert(TypeProlog);

  pceSendMethod(ClassProlog,			/* The class */
		"equal",			/* Name of the method */
		NULL,				/* Group */
		1,				/* # arguments */
		"prolog",			/* Type arg1 */
		"Test equality (==)",		/* Summary */
		equalProlog);			/* Function */
}





		 /*******************************
		 *	PROLOG --> XPCE		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
termToObject(term_t t, PceType targettype, atom_t assoc, int new)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


static PceObject
termToObject(term_t t, PceType type, atom_t assoc, int new)
{ atom_t functor;
  size_t arity;

  DEBUG(Sdprintf("termToObject(");
	PL_write_term(Soutput, t, 1200, 0);
	Sdprintf(")\n"));

  if ( GetNameArity(t, &functor, &arity) )
  {					/* Just an atom */
    if ( arity == 0 )
    { PceName name = atomToName(functor);

      return (new ? pceNew(atomToAssoc(assoc), name, 0, NULL) : name);
    }
					/* @Ref */
    if ( functor == ATOM_ref && arity == 1 )
    { PceObject rval;

      if ( get_object_from_refterm(t, &rval) )
	return rval;

      return PCE_FAIL;
    }
					/* new/[1,2] */
    if ( functor == ATOM_new )
    { if ( arity == 1 )			/* new(chain) */
      { term_t a = PL_new_term_ref();

	QGetArg(1, t, a);
	return termToObject(a, NULL, NULLATOM, TRUE);
      }
      if ( arity == 2 )			/* new(B, box) */
      { term_t r = PL_new_term_ref();
	term_t n = PL_new_term_ref();

	QGetArg(1, t, r);
	QGetArg(2, t, n);
	return do_new(r, n);
      }
    }

					/* string(hello) */
    if ( functor == ATOM_string && arity == 1 )
    { char *s;
      wchar_t *sW;
      size_t len;
      term_t a = PL_new_term_ref();
      PceName pceassoc = atomToAssoc(assoc);

      QGetArg(1, t, a);
      if ( PL_get_nchars(a, &len, &s, CVT_ALL) )
	return cToPceStringA(pceassoc, s, len, FALSE);
      else if ( PL_get_wchars(a, &len, &sW, CVT_ALL) )
	return cToPceStringW(pceassoc, sW, len, FALSE);

      ThrowException(EX_TYPE, ATOM_string, t);
      return PCE_FAIL;
    }

					/* prolog(term_t) */
    if ( functor == ATOM_prolog && arity == 1 )
    { PceObject h;
      term_t a = PL_new_term_ref();
      double f;
      intptr_t r;

      QGetArg(1, t, a);

      if ( GetInteger(a, &r) )		/* pass atoms, ints and floats */
	return cToPceInteger(r);	/* as xpce objects anyhow */
      if ( GetFloat(t, &f) )
	return cToPceReal(f);

      h = makeTermHandle(a);		/* real terms */
      makeAnyHostData(h);		/* make acceptable to any/object */

      return h;
    }
					/* A list */
    if ( functor == ATOM_dot && arity == 2 )
    { term_t tail = CopyTerm(t);
      term_t head = PL_new_term_ref();
      int argsallocated = 16;
      int argc = 0;
      PceObject *argv = alloca(argsallocated*sizeof(PceObject));
      PceName classname = NAME_codeVector;

      while ( PL_get_list(tail, head, tail) )
      { PceObject a;

	if ( get_object_arg(head, &a) )
	{ if ( argc >= argsallocated )
	  { PceObject tmp = alloca(2*argsallocated*sizeof(PceObject));
	    memcpy(tmp, argv, argsallocated*sizeof(PceObject));
	    argv = tmp;
	    argsallocated *= 2;
	  }
	  argv[argc++] = a;
	} else
	  return PCE_FAIL;
      }

      if ( !PL_get_nil(tail) )
      { ThrowException(EX_TYPE, ATOM_proper_list, t);
	return PCE_FAIL;
      }

      if ( type )
      { if ( pceSend(type, NULL, NAME_includes,
		     1, (PceObject *)&NAME_chain) )
	  classname = NAME_chain;
	else if ( pceSend(type, NULL, NAME_includes, 1,
			  (PceObject *)&NAME_vector) )
	  classname = NAME_vector;
      }

      return pceNew(NIL, classname, argc, argv);
    }

					/* Class(...Args...) */
    { PceName name = atomToName(functor);
      ArgVector(argv, arity);
      term_t a = PL_new_term_ref();
      int n;

      for(n=0 ; n < arity; n++ )
      { QGetArg(n+1, t, a);
	if ( !get_object_arg(a, &argv[n]) )
	  return PCE_FAIL;
      }

      return pceNew(atomToAssoc(assoc), name, (int)arity, argv);
    }
  } else				/* not a term */
  { double f;
    intptr_t r;

					/* PL_get_integer() translates */
					/* `whole' floats to integers */
    if ( PL_is_integer(t) && GetInteger(t, &r) )
    { if ( new )
	goto type_error;

      return cToPceInteger(r);
    }

#ifdef O_STRING
  { char *s;
    wchar_t *w;
    size_t len;

    if ( PL_get_string(t, &s, &len) )	/* string object (if supported) */
      return cToPceStringA(atomToAssoc(assoc), s, len, FALSE);
    if ( PL_get_wchars(t, &len, &w, CVT_STRING) )
      return cToPceStringW(atomToAssoc(assoc), w, len, FALSE);
  }
#endif

    if ( GetFloat(t, &f) )		/* floating point number */
      return cToPceReal(f);

    if ( PL_get_nil(t) )
    { PceName classname = NAME_codeVector;

      if ( type )
      { if ( pceSend(type, NULL, NAME_includes,
		     1, (PceObject *)&NAME_chain) )
	  classname = NAME_chain;
	else if ( pceSend(type, NULL, NAME_includes, 1,
			  (PceObject *)&NAME_vector) )
	  classname = NAME_vector;
      }
      return pceNew(NIL, classname, 0, NULL);
    }
					/* anything else */
  type_error:
    ThrowException(EX_TYPE, ATOM_object, t);
    return PCE_FAIL;
  }
}


		 /*******************************
		 *	  OBJECT-TO-TERM	*
		 *******************************/

static int
unifyObject(term_t t, PceObject obj, int top)
{ PceCValue value;
  int pcetype;
  term_t tmpt;

  switch( (pcetype = pceToC(obj, &value)) )
  { case PCE_INTEGER:			/* integer */
      return UnifyInteger(t, value.integer);
    case PCE_REAL:			/* float (real object) */
      return UnifyFloat(t, value.real);
    case PCE_NAME:			/* name */
    { size_t len;
      const char *textA;
      const wchar_t *textW;

      if ( (textA = pceCharArrayToCA(obj, &len)) )
	return PL_unify_atom_nchars(t, len, textA);
      else if ( (textW = pceCharArrayToCW(obj, &len)) )
	return PL_unify_wchars(t, PL_ATOM, len, textW);
      else
      { assert(0);
	return FALSE;
      }
    }
    case PCE_HOSTDATA:
      return PL_unify(t, getTermHandle(obj)); /* TBD: avoid redoing this */
    case PCE_REFERENCE:
    case PCE_ASSOC:
      if ( !top )
      { atom_t n;
	size_t a;

	if ( IsVar(t) )			/* get(R, S, Var) */
	  return unifyReference(t, pcetype, value);

					/* get(R, S, @something) */
	if ( GetNameArity(t, &n, &a) && n == ATOM_ref && a == 1 )
	{ tmpt = PL_new_term_ref();

	  QGetArg(1, t, tmpt);
	  return unifyReferenceArg(tmpt, pcetype, value);
	}
      }
  }

  if ( pceIsString(obj) )	/* string: handle special */
  { const char *textA;
    const wchar_t *textW;
    size_t len;
    term_t a = PL_new_term_ref();

    if ( (textA = pceCharArrayToCA(obj, &len)) )
    { PL_put_atom_nchars(a, len, textA);
    } else if ( (textW = pceCharArrayToCW(obj, &len)) )
    { if ( !PL_unify_wchars(a, PL_ATOM, len, textW) )
	return FALSE;
    } else
    { return FALSE;
    }

    return PL_unify_term(t,
			 PL_FUNCTOR, FUNCTOR_string1,
			   PL_TERM, a);
  }

  { atom_t name;
    size_t n, arity;
    atom_t pname;			/* name of Pce object */
    size_t parity;			/* its `arity' */
    PceObject got;			/* temp variable */
    term_t at = PL_new_term_ref();

    if ( !(got = pceGet(obj, NULL, NAME_functor, 0, NULL)) ||
	 !(pname = nameToAtom(got)) )
      return FALSE;
    if ( !(got = pceGet(obj, NULL, NAME_Arity, 0, NULL)) ||
	 pceToC(got, &value) != PCE_INTEGER )
      return FALSE;
    parity = value.integer;

    if ( GetNameArity(t, &name, &arity) )
    { if ( name != pname || arity != parity )
	return FALSE;
      for(n=1; n<=arity; n++)
      { PceObject pcen = cToPceInteger(n);

	if ( (got = pceGet(obj, NULL, NAME_Arg, 1, &pcen)) )
	{ QGetArg(n, t, at);

	  if ( !unifyObject(at, got, FALSE) )
	    return FALSE;
	} else
	  return FALSE;
      }

      return TRUE;
    } else if ( IsVar(t) )
    { term_t t2 = PL_new_term_ref();

      if ( !PutFunctor(t2, pname, parity) )
	return FALSE;
      for(n=1; n<=parity; n++)
      { PceObject pcen = cToPceInteger(n);

	if ( (got = pceGet(obj, NULL, NAME_Arg, 1, &pcen)) )
	{ QGetArg(n, t2, at);

	  if ( !unifyObject(at, got, FALSE) )
	    return FALSE;
	} else
	  return FALSE;
      }
      return PL_unify(t, t2);
    } else
      return FALSE;
  }
}


		 /*******************************
		 *	  VMI FUNCTIONS		*
		 *******************************/

static __inline Module
PushDefaultModule()
{ Module odm = DefaultModule;

  DefaultModule = 0;
  return odm;
}

#define PopDefaultModule(o)	(DefaultModule = (o))

					/* NEW */
static foreign_t
pl_new(term_t assoc, term_t descr)
{ AnswerMark mark;
  PceObject obj;
  term_t d = PL_new_term_ref();
  Module odm;
  pce_goal goal;
  HostStackEntry hmark;

  LOCK();
  odm		      =	PushDefaultModule();
  hmark               = host_handle_stack;
  goal.flags	      =	PCE_GF_CATCH;
  goal.errcode	      =	PCE_ERR_OK;
  goal.argc	      =	0;
  goal.receiver	      =	NIL;
  goal.implementation =	NIL;
  pcePushGoal(&goal);

  if ( !StripModuleTag(descr, &DefaultModule, d) )
    return FALSE;
  markAnswerStack(mark);
  obj = do_new(assoc, d);
  rewindAnswerStack(mark, obj);
  rewindHostHandles(hmark);
  PopDefaultModule(odm);

  if ( !obj && (goal.flags & PCE_GF_THROW) )
    ThrowException(EX_GOAL, &goal, descr);

  pceFreeGoal(&goal);
  UNLOCK();

  return obj ? TRUE : FALSE;
}


static __inline int
get_pce_class(term_t t, PceClass *cl)
{ if ( t )
  { atom_t a;

    if ( GetAtom(t, &a) )
    { PceClass class = nameToExistingClass(atomToName(a));

      if ( class )
      { *cl = class;
	return TRUE;
      }
    }

    return FALSE;
    assert(0);				/* Raise exception */
  }

  *cl = NULL;
  return TRUE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Put @default into the argument term
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
put_default(PceGoal g, int n, term_t t)
{ PceObject v = pceCheckType(g, g->types[n], DEFAULT);

  if ( v == DEFAULT )			/* pass @default */
  { PL_put_atom(t, ATOM_default);
    return PL_cons_functor(t, FUNCTOR_ref1, t);
  } else if ( v )
  { return put_object(t, v);		/* some converted object */
  } else
    return pceSetErrorGoal(g, PCE_ERR_MISSING_ARGUMENT, cToPceInteger(n));
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
put_prolog_argument()
    Does Prolog-to-Prolog conversion of arguments.  If the type has a
    natural Prolog counterpart, the reference is passed directly.  Otherwise
    it is translated to XPCE for conversion.  Atoms, Names and floats have
    these natural counterparts.  See alse get_object_arg().
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
put_prolog_argument(PceGoal g, term_t t, PceType type, term_t f)
{ PceObject obj;
  term_value_t val;
					/* --> :prolog */
  if ( pceIncludesHostDataType(type, ClassProlog) )
    return PutTerm(t, f);

  switch(PL_get_term_value(f, &val))
  { case PL_ATOM:
      if ( pceCheckNameType(type, AtomCharp(val.a)) )
      { PL_put_atom(t, val.a);
	return TRUE;
      }
      break;
    case PL_INTEGER:
      if ( pceCheckIntType(type, val.i) )
	return PL_put_int64(t, val.i);
      break;
    case PL_FLOAT:
      if ( pceCheckFloatType(type, val.f) )
	return PutTerm(t, f);
      break;
    case PL_TERM:
      if ( val.t.name == ATOM_ref && val.t.arity == 1 )
      { PceObject obj2;

	if ( !get_object_from_refterm(f, &obj) )
	{ g->errcode = PCE_ERR_OK;	/* TBD: Should be something else  */
	  return FALSE;
	}

	if ( (obj2 = pceCheckType(g, type, obj)) )
	{ if ( obj2 == obj )
	  { if ( !PutTerm(t, f) )
	      return FALSE;
	  } else
	  { put_object(t, obj2);
	  }

	  return TRUE;
	}
      }
  }

					/* anything else */
  if ( (obj = termToObject(f, type, NULLATOM, FALSE)) )
  { PceObject obj2;

    if ( (obj2 = pceCheckType(g, type, obj)) )
    { put_object(t, obj2);
      return TRUE;
    }

    return pceSetErrorGoal(g, PCE_ERR_ARGTYPE, obj);
  }

  return pceSetErrorGoal(g, PCE_ERR_ARGTYPE, NIL);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
stripArgName(?t, PceName *name)
    if t is <atom> := <value>, put value into <t> and <atom> as an XPCE
    name object into name.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static __inline void
stripArgName(term_t t, PceName *name)
{ if ( IsFunctor(t, FUNCTOR_namearg) )	/* Name := Value */
  { term_t a = PL_new_term_ref();
    atom_t an;

    QGetArg(1, t, a);
    if ( GetAtom(a, &an) )
    { *name = atomToName(an);
      QGetArg(2, t, t);
    }
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
invoke(term_t receiver, term_t class, term_t selector, term_t return)
    This function is the central code fore invoking both XPCE send- and
    get-methods.  `Class' and `return' may be 0 to indicate these arguments
    don't care.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
invoke(term_t rec, term_t cl, term_t msg, term_t ret)
{ int rval = FALSE;
  AnswerMark mark;
  PceObject receiver;
  Module odm;
  pce_goal goal;
  HostStackEntry hmark;
  fid_t fid = 0;

  LOCK();
  odm = PushDefaultModule();
  hmark = host_handle_stack;

  goal.flags = (ret ? PCE_GF_GET : PCE_GF_SEND)|PCE_GF_CATCH;
  goal.errcode = PCE_ERR_OK;
  goal.rval  = NIL;			/* for rewindAnswerStack() */

  markAnswerStack(mark);
  receiver = termToReceiver(rec);

  if ( receiver )
  { atom_t name;
    size_t  arity;
    PceClass class;

    if ( !get_pce_class(cl, &class) )
    { rval = ThrowException(EX_EXISTENCE, ATOM_class, cl);
      goto out;
    }
    if ( !StripModuleTag(msg, &DefaultModule, msg) )
      goto out;
    if ( GetNameArity(msg, &name, &arity) )
    { PceName selector = atomToName(name);
      term_t arg;

      if ( arity > 0 )
	arg = PL_new_term_ref();
      else
	arg = 0;			/* make compiler happy */

      goal.receiver = receiver;
      goal.class    = class;
      goal.selector = selector;

      if ( pceResolveImplementation(&goal) )
      { if ( goal.flags & PCE_GF_HOST )
	{ prolog_call_data *pcd = get_pcd(goal.implementation);
				/* Implemented in Prolog */
	  term_t av, mav;
	  term_t tmp, tmp2, tail;
	  int n;
	  void *prof_node;

	  fid = PL_open_foreign_frame();
	  av  = PL_new_term_refs(4);
	  mav = PL_new_term_refs(pcd->argc);

	  goal.flags |= PCE_GF_HOSTARGS;
	  pceInitArgumentsGoal(&goal);

	  if ( goal.va_type )
	  {				/* used by PrologWriteGoalArgs() */
	    goal.host_closure = (void *)(mav+pcd->argc-1);
	    tail = PL_copy_term_ref(mav+pcd->argc-1);
	    tmp  = PL_new_term_ref();
	    tmp2 = PL_new_term_ref();
	  } else
	    tmp = tmp2 = tail = 0;

					/* push method identifier */
	  if ( (pcd->flags & (PCE_METHOD_INFO_TRACE|PCE_METHOD_INFO_BREAK)) )
	  { if ( !put_trace_info(av+0, pcd) )
	      goto plerror;
	  } else
	  { _PL_put_atomic(av+0, pcd->method_id);
	  }

	  if ( goal.flags & PCE_GF_CATCHALL )
	  { goal.argn++;
	    PL_put_atom(mav, CachedNameToAtom(goal.selector));
	    goal.argv[0] = (PceObject)mav;
	  }

	  for(n=0; n<arity; n++)
	  { PceName name = NULL;
	    PceType type;
	    int i;

	    QGetArg(n+1, msg, arg);
	    stripArgName(arg, &name);
	    if ( !pceGetArgumentTypeGoal(&goal, name, &type, &i) )
	      goto plerror;
	    if ( !put_prolog_argument(&goal,
				      i < 0 ? tmp : (mav+i),
				      type,
				      arg) )
	    { if ( goal.errcode == PCE_ERR_ARGTYPE )
	      { goal.argn = (i<0 ? goal.argc : i);
		goal.errc1 = makeTermHandle(arg);
	      }

	      goto plerror;
	    }
	    if ( i < 0 )
	    { if ( !PL_unify_list(tail, tmp2, tail) ||
		   !PL_unify(tmp2, tmp) )
	      { rval = FALSE;
		goto out;
	      }
	    } else
	      goal.argv[i] = (PceObject)(mav+i);
	  }
	  if ( tail )
	  { if ( !PL_unify_nil(tail) )
	    { rval = FALSE;
	      goto out;
	    }
	  }

	  for(n=0; n<goal.argc; n++)
	  { if ( !goal.argv[n] && !put_default(&goal, n, mav+n) )
	      goto plerror;
	  }

	  if ( !PL_cons_functor_v(av+1, pcd->functor, mav) ||
	       !put_object(av+2, goal.receiver) )
	  { rval = FALSE;
	    goto out;
	  }

	  if ( prof_active )
	    prof_node = PL_prof_call(goal.implementation, &pceProfType);
	  else
	    prof_node = NULL;
	  if ( ret )
	  { rval = PL_call_predicate(MODULE_user,
				     DebugMode|PL_Q_PASS_EXCEPTION,
				     PREDICATE_get_implementation, av);
	    if ( rval )
	    { if ( IsFunctor(av+3, FUNCTOR_ref1) )
	      { if ( !get_object_from_refterm(av+3, &goal.rval) )
		{ rval = FALSE;
		  goto out;
		}

		if ( !PL_unify(ret, av+3) )
		  rval = unifyObject(ret, goal.rval, FALSE);
	      } else
		rval = PL_unify(ret, av+3);
	    }
	  } else
	  { rval = PL_call_predicate(MODULE_user,
				     DebugMode|PL_Q_PASS_EXCEPTION,
				     PREDICATE_send_implementation, av);
	  }
	  if ( prof_node )
	    PL_prof_exit(prof_node);

	  goto out;
	plerror:
	  pceReportErrorGoal(&goal);
	  goto out;
	} else				/* Implemented in XPCE itself */
	{ int n;

	  pceInitArgumentsGoal(&goal);
	  for(n=0; n<arity; n++)
	  { PceObject value = NULL;
	    PceName name = NULL;
	    PceType type;
	    int i;

	    QGetArg(n+1, msg, arg);
	    stripArgName(arg, &name);
	    if ( !pceGetArgumentTypeGoal(&goal, name, &type, &i) )
	      goto error;
	    if ( !get_typed_object(&goal, arg, type, &value) )
	    { if ( goal.errcode == PCE_ERR_ARGTYPE )
		goal.argn = (i < 0 ? goal.argc : i);
	      goto error;
	    }
	    if ( i >= 0 )
	      goal.argv[i] = value;
	    else
	      pceVaAddArgGoal(&goal, value);
	  }
	  rval = pceExecuteGoal(&goal);

	  if ( ret && rval )
	    rval = unifyObject(ret, goal.rval, FALSE);

	  goto out;
	error:
	  pceReportErrorGoal(&goal);
	  goto out;
	}
      } else /* no implementation */
	pceReportErrorGoal(&goal);
    }
  }
out:
  if ( goal.flags & PCE_GF_THROW )
    rval = ThrowException(EX_GOAL, &goal, rec, msg);
  rewindHostHandles(hmark);
  rewindAnswerStack(mark, goal.rval);
  PopDefaultModule(odm);
  pceFreeGoal(&goal);
  if ( fid )
    PL_close_foreign_frame(fid);
  UNLOCK();

  return rval;
}


static foreign_t
pl_send(term_t rec, term_t msg)
{ return invoke(rec, 0, msg, 0);
}


static foreign_t
pl_send_class(term_t rec, term_t cl, term_t msg)
{ return invoke(rec, cl, msg, 0);
}


static foreign_t
pl_get(term_t rec, term_t msg, term_t ret)
{ return invoke(rec, 0, msg, ret);
}


static foreign_t
pl_get_class(term_t rec, term_t cl, term_t msg, term_t ret)
{ return invoke(rec, cl, msg, ret);
}



		 /*******************************
		 *	    OBJECT/[1,2]	*
		 *******************************/

static foreign_t
pl_object1(term_t ref)
{ atom_t name;
  size_t arity;

  if ( GetNameArity(ref, &name, &arity) &&
       name == ATOM_ref &&
       arity == 1 )
  { term_t a = PL_new_term_ref();
    atom_t refname;
    intptr_t refi;

    QGetArg(1, ref, a);
    if ( GetAtom(a, &refname) )
      return pceExistsAssoc(atomToName(refname));
    else if ( GetInteger(a, &refi) )
      return pceExistsReference(refi);
  }

  return FALSE;
}


static foreign_t
pl_object2(term_t ref, term_t description)
{ PceObject obj;
  int rval;

  LOCK();
  if ( (obj = termToObject(ref, NULL, NULLATOM, FALSE)) )
    rval = unifyObject(description, obj, TRUE);
  else
    rval = FALSE;
  UNLOCK();

  return rval;
}


		 /*******************************
		 *	  PROLOG SEND/GET	*
		 *******************************/

#ifdef SWI
#define prolog_exception(r) (r)
#else
static int
prolog_exception(r)
int r;
{ switch(r)
  { case SP_SUCCESS:
      return TRUE;
    case SP_FAILURE:
      return FALSE;
    case SP_ERROR:
    default:
    { SP_term_ref t = SP_new_term_ref();
      SP_pred_ref p = SP_predicate("print_exception", 1, "pce_host");

      SP_exception_term(t);
      (void) SP_query(p, t);
    }
  }
}
#endif

static int
PrologSend(PceObject prolog, PceObject sel, int argc, PceObject *argv)
{ Fid fid;
  Module m;
  PceCValue value;
  Predicate pred = NULL;
  term_t goal = 0;
  int rval;

  if ( !pce_initialised )
    return FALSE;

  fid = OpenForeign();
  m = pceContextModule();

  switch(pceToC(sel, &value))
  { case PCE_NAME:
    { PceITFSymbol symbol = value.itf_symbol;
      pred = FindPredicate(nameToAtom(symbol->name), argc, m);
      break;
    }
    case PCE_HOSTDATA:
      goal = getTermHandle(sel);
      break;
    default:
      assert(0);			/* should not be passed */
  }

#ifdef SWI
  if ( pred )
  { term_t terms = PL_new_term_refs(argc);
    qid_t qid;
    int i;

    for(i=0; i<argc; i++)
      put_object(terms+i, argv[i]);

    qid  = PL_open_query(m, DebugMode|PL_Q_PASS_EXCEPTION, pred, terms);
    rval = PL_next_solution(qid);
    PL_cut_query(qid);
  } else
  { if ( argc > 0 )
      rval = FALSE;			/* TBD */
    rval = PL_call(goal, m);
  }
#else /*~SWI*/
  if ( pred )
  { SP_term_ref *terms = alloca(sizeof(SP_term_ref) * argc);
    SP_qid qid;

    for(i=0; i<argc; i++)
    { terms[i] = SP_new_term_ref();
      SP_put_variable(terms[i]);
      put_object(terms[i], argv[i]);
    }

    if ( (qid = SP_open_query_array(pred, terms)) )
    { rval = prolog_exception(SP_next_solution(qid));
      SP_close_query(qid);
    } else
      return ThrowException(EX_OPEN_QUERY, name, argc);
  } else
  { atom_t name, module;

    split_selector(sel, &name, &module);
    UndefinedPredicate(name, argc, module);
    rval = FALSE;
  }
#endif /*SWI*/

  CloseForeign(fid);
  return rval;
}


static PceObject
PrologGet(PceObject prolog, PceObject sel, int argc, PceObject *argv)
{ Fid fid;
  Module m;
  atom_t name;
  Predicate pred;
  int i;
  PceObject obj;

  if ( !pce_initialised )
    return PCE_FAIL;

  fid = OpenForeign();
  m = pceContextModule();
  name = nameToAtom(sel);
  pred = FindPredicate(name, argc+1, m);

#ifdef SWI
{ term_t terms = PL_new_term_refs(argc+1);
  qid_t qid;
  int rval;

  for(i=0; i<argc; i++)
  { if ( !unifyObject(terms+i, argv[i], FALSE) )
    { obj = PCE_FAIL;
      goto out;
    }
  }

  qid  = PL_open_query(m, DebugMode, pred, terms);
  rval = PL_next_solution(qid);
  PL_cut_query(qid);
  if ( rval )
    obj = termToObject(terms+argc, NULL, NULLATOM, FALSE);
  else
    obj = PCE_FAIL;
}
#else /*~SWI*/

  if ( pred )
  { SP_term_ref *terms = alloca(sizeof(SP_term_ref) * (argc+1));
    SP_qid qid;

    for(i=0; i<argc; i++)
    { terms[i] = SP_new_term_ref();
      SP_put_variable(terms[i]);
      if ( !unifyObject(terms[i], argv[i], FALSE) )
      { obj = PCE_FAIL;
	goto out;
      }
    }
    terms[argc] = SP_new_term_ref();
    SP_put_variable(terms[argc]);	/* trailing variable for result */

    if ( (qid = SP_open_query_array(pred, terms)) )
    { if ( prolog_exception(SP_next_solution(qid)) )
	obj = termToAnswer(terms[argc]);
      else
	obj = PCE_FAIL;
      SP_close_query(qid);
    } else
    { ThrowException(EX_OPEN_QUERY, name, argc+1);
      obj = PCE_FAIL;
    }
  } else
  { atom_t name, module;

    split_selector(sel, &name, &module);
    UndefinedPredicate(name, argc+1, module);
    obj = PCE_FAIL;
  }

#endif /*SWI*/

out:
  CloseForeign(fid);
  return obj;
}


static int
put_object(term_t t, PceObject obj)
{ PceCValue value;
  int pcetype;
  atom_t avalue;

  switch( pcetype = pceToC(obj, &value) )
  { case PCE_REFERENCE:
    {
#ifdef HAVE_XPCEREF
      return _PL_put_xpce_reference_i(t, value.integer);
#else
      term_t t2;

      return ( (t2 = PL_new_term_ref()) &&
	       PL_put_int64(t2, value.integer) &&
	       PL_cons_functor(t, FUNCTOR_ref1, t2) );
#endif
      break;
    }
    case PCE_ASSOC:
    { PceITFSymbol symbol = value.itf_symbol;

      avalue = CachedNameToAtom(symbol->name);

#ifdef HAVE_XPCEREF
      return _PL_put_xpce_reference_a(t, avalue);
#else
      { term_t t2;

	return ( (t2=PL_new_term_ref()) &&
		 PL_put_atom(t2, avalue) &&
		 PL_cons_functor(t, FUNCTOR_ref1, t2)
	       );
      }
#endif

      break;
    }
    case PCE_HOSTDATA:
      return PutTerm(t, getTermHandle(obj));	/* TBD: Use saved handle */
    case PCE_INTEGER:
      return PL_put_int64(t, value.integer);
    case PCE_NAME:
      { PceITFSymbol symbol = value.itf_symbol;

	avalue = nameToAtom(symbol->name);
      }
      PL_put_atom(t, avalue);
      return TRUE;

      break;
    case PCE_REAL:
      return PL_put_float(t, value.real);

      break;
    default:
      assert(0);
      return FALSE;
  }
}


static foreign_t
pl_pce_method_implementation(term_t id, term_t msg)
{ prolog_call_data *pcd = pceAlloc(sizeof(prolog_call_data));

  memset(pcd, 0, sizeof(*pcd));

  if ( PL_is_atomic(id) )
  { pcd->method_id = _PL_get_atomic(id);
  } else
  { return PL_warning("pce_method_implementation/2: type error");
  }

  return unifyObject(msg, cToPcePointer(pcd), FALSE);
}


static prolog_call_data *
get_pcd(PceObject method)
{ pce_method_info m;

  m.flags = PCE_METHOD_INFO_HANDLE_ONLY;
  if ( pceGetMethodInfo(method, &m) )
  { prolog_call_data *pcd = m.handle;

    if ( !pcd->functor )
    { m.flags = 0;

      pceGetMethodInfo(method, &m);

      pcd->functor = PL_new_functor(nameToAtom(m.name), m.argc);
      pcd->argc    = m.argc;
    }

    pcd->flags = m.flags;

    return pcd;
  }

  return NULL;
}


static int
put_trace_info(term_t id, prolog_call_data *pm)
{ term_t a = PL_new_term_ref();
  functor_t f;

  _PL_put_atomic(a, pm->method_id);
  if ( (pm->flags & PCE_METHOD_INFO_BREAK) )
    f = FUNCTOR_spy1;
  else /*if ( (pm->flags & PCE_METHOD_INFO_TRACE) )*/
    f = FUNCTOR_trace1;

  return PL_cons_functor(id, f, a);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Call a Prolog-defined implementation from  XPCE.   Note  that send/2 and
get/3 route methods defined in  Prolog   directly  back  to Prolog. This
definition only comes into action if something   in  XPCE calls a method
defined in Prolog.

NOTE: if the  return-type accepts prolog, we  return a term-reference.
Is this ok?  Who is ensuring the consistency?  Should  we throw it out
of  the context  immediately? It  is  returned to  C-code from  inside
XPCE. Who says me what happens to it?  - - - - - - - - - - - - - - - -
- - - - - - - - - - - - - - - - - - - - - */

static int
PrologCall(PceGoal goal)
{ prolog_call_data *pcd;

  if ( !pce_initialised )
    return FALSE;

  if ( (pcd = get_pcd(goal->implementation)) )
  { fid_t fid;

    if ( (fid=PL_open_foreign_frame()) )
    { term_t av  = PL_new_term_refs(4);
      term_t mav = PL_new_term_refs(pcd->argc);
      int rval = PCE_FAIL, n;
					  /* push method identifier */
      if ( (pcd->flags & (PCE_METHOD_INFO_TRACE|PCE_METHOD_INFO_BREAK)) )
      { if ( !put_trace_info(av+0, pcd) )
	  goto error;
      } else
	_PL_put_atomic(av+0, pcd->method_id);

      for(n=0; n<goal->argc; n++)		/* push normal arguments */
      { if ( !put_object(mav+n, goal->argv[n]) )
	  goto error;
      }
      if ( goal->va_argc >= 0 )		/* push varargs excess args in list */
      { term_t l = mav+n;
	term_t tmp = PL_new_term_ref();

	PL_put_nil(l);
	for(n=goal->va_argc; --n >= 0; )
	{ if ( !put_object(tmp, goal->va_argv[n]) ||
	       !PL_cons_list(l, tmp, l) )
	    goto error;
	}
      }
					  /* push @receiver */
      if ( !PL_cons_functor_v(av+1, pcd->functor, mav) ||
	   !put_object(av+2, goal->receiver) )
	goto error;

      if ( goal->flags & PCE_GF_SEND )
      { rval = PL_call_predicate(MODULE_user, DebugMode|PL_Q_PASS_EXCEPTION,
				 PREDICATE_send_implementation, av);
      } else
      { rval = PL_call_predicate(MODULE_user, DebugMode|PL_Q_PASS_EXCEPTION,
				 PREDICATE_get_implementation, av);
	if ( rval )
	{ if ( !get_answer_object(goal, av+3, goal->return_type, &goal->rval) )
	  { pceReportErrorGoal(goal);
	    rval = PCE_FAIL;
	  }
	}
      }

      term_t ex;
      if ( !rval && (ex=PL_exception(0)) )
      { bool rc;

	if ( PL_is_functor(ex, FUNCTOR_unwind1) )
	{ PL_close_foreign_frame(fid);
	  return PCE_FAIL;
	}

	rc = PL_print_message(ATOM_error, PL_TERM, ex);
	(void)rc;
	PL_clear_exception();
      }

    error:
      PL_discard_foreign_frame(fid);
      return rval;
    }
  }

  return PCE_FAIL;
}


static PceObject
getPrologContext(PceObject receiver)
{ if ( receiver == PROLOG )
  { atom_t mname;

    if ( DefaultModule )
    { mname = ModuleName(DefaultModule);

      return atomToName(mname);
    }

    return NAME_user;
  }

  return NIL;
}


static PceObject
setPrologContext(PceObject context)
{ PceObject old = DefaultContext;

  DefaultContext = context;

  return old;
}


static Module
pceContextModule()
{ atom_t mname;

  if ( DefaultContext && (mname = nameToAtom(DefaultContext)) )
    return ModuleFromAtom(mname);

  return MODULE_user;
}


		 /*******************************
		 *	 STREAM CONNECTION	*
		 *******************************/

#ifdef SWI
#define fdFromHandle(h) ((int)((intptr_t)(h)))

static ssize_t
Swrite_pce(void *handle, char *buf, size_t size)
{ return pceWrite(fdFromHandle(handle), buf, size);
}


static ssize_t
Sread_pce(void *handle, char *buf, size_t size)
{ return pceRead(fdFromHandle(handle), buf, size);
}


static long
Sseek_pce(void *handle, long offset, int whence)
{ return pceSeek(fdFromHandle(handle), offset, whence);
}


static int
Sclose_pce(void *handle)
{ return pceClose(fdFromHandle(handle));
}


static int
Scontrol_pce(void *handle, int cmd, void *closure)
{ switch(cmd)
  { case SIO_FLUSHOUTPUT:
      return 0;
    case SIO_SETENCODING:
      return pceControl(fdFromHandle(handle), PCE_SETENCODING, closure);
  }

  return -1;
}


IOFUNCTIONS pceFunctions =
{ Sread_pce,
  Swrite_pce,
  Sseek_pce,
  Sclose_pce,
  Scontrol_pce
};


static foreign_t
pl_pce_open(term_t t, term_t mode, term_t plhandle)
{ PceObject obj;
  IOENC enc;

  if ( (obj = termToReceiver(t)) )
  { int flags, sflags = SIO_LBUF|SIO_RECORDPOS;
    int handle;
    atom_t m;

    if ( GetAtom(mode, &m) )
    { if ( m == ATOM_read )
      { flags = PCE_RDONLY;
	sflags |= SIO_INPUT;
      } else if ( m == ATOM_write )
      { flags = PCE_WRONLY|PCE_TRUNC;
	sflags |= SIO_OUTPUT;
      } else if ( m == ATOM_append )
      { flags = PCE_WRONLY|PCE_APPEND;
	sflags |= SIO_OUTPUT;
      } else if ( m == ATOM_update )
      { flags = PCE_WRONLY;
	sflags |= SIO_OUTPUT;
      } else
	goto domain_error;
    } else
    { domain_error:

      return ThrowException(EX_DOMAIN, ATOM_io_mode, mode);
    }

    if ( (handle = pceOpen(obj, flags, (void *)&enc)) >= 0 )
    { IOSTREAM *s = Snew((void *)(intptr_t)handle, sflags, &pceFunctions);
      s->encoding = enc;
      if ( enc != ENC_OCTET )
	s->flags |= SIO_TEXT;

      return PL_open_stream(plhandle, s);
    } else
    { atom_t a = AtomFromString(pceOsError());

      return ThrowException(EX_PERMISSION,
			    ATOM_open, ATOM_object, obj,
			    a);
    }
  }

  PL_fail;
}


static foreign_t
pl_pce_postscript_stream(term_t ps)
{ IOSTREAM *s = pcePostScriptStream();

  if ( s )
    return PL_unify_stream(ps, s);

  return FALSE;
}

#endif /*SWI*/

		 /*******************************
		 *	  EVENT-DISPATCH	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
TIMEOUT defines  the number  of milliseconds  to  wait for  something to
happen in PCE.  If this value  is 0,  PCE  will wait indefinitely for an
event or input.

For linux this value is currently set to 250 because linux's select call
appears  not  to  be   broken  when   a   signal  (notably  SIGCHLD  in
unx-process.c) arrives.  This way pce will anyway  see the signal ...  A
better solution for signal handling is to be searched for (also avoiding
the possibility of reentrance at moments this is not allowed in PCE ...
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifndef __WINDOWS__
#ifndef PROLOG_DISPATCH_INPUT
#define PROLOG_DISPATCH_INPUT 1
#define PROLOG_DISPATCH_TIMEOUT 0
#endif

#ifndef TIMEOUT
#define TIMEOUT 250			/* just wait */
#endif

static int
pce_dispatch(int fd)
{ if ( pceDispatch(fd, TIMEOUT) == PCE_DISPATCH_INPUT )
    return PROLOG_DISPATCH_INPUT;

  return PROLOG_DISPATCH_TIMEOUT;
}
#endif /*__WINDOWS__*/

#ifdef SICSTUS

#ifdef __WINDOWS__

void *
pl_malloc(unsigned int size)
{ return SP_malloc(size);
}


void *
pl_realloc(void *ptr, unsigned int size)
{ return SP_realloc(ptr, size);
}


void
pl_free(void *ptr)
{ SP_free(ptr);
}

#else /* __WINDOWS__ */

#define pl_malloc SP_malloc
#define pl_realloc SP_realloc
#define pl_free SP_free

#endif /*__WINDOWS__*/


		 /*******************************
		 *	    CONSOLE I/O		*
		 *******************************/

void
pl_Cvprintf(const char *fmt, va_list args)
{ char buf[2048];			/* No SP_vprintf() in SICStus */
  char *s;

  vsprintf(buf, fmt, args);
  for(s=buf; *s; s++)
    SP_putc(*s);
}


static int
pl_Cputchar(int c)
{ SP_putc(c);

  return c;
}


static void
pl_Cflush(void)
{ SP_fflush(SP_stdout);
}


static char *
pl_Cgetline(char *buf, int size)
{ int c, n = 0;

  do
  { c = SP_getc();

    if ( c == EOF )
      return NULL;

    buf[n++] = c;
    if ( n == size );
      return buf;
  } while ( c != '\n' && c != '\r' );

  return buf;
}


		 /*******************************
		 *	HOST ACTION/QUERY	*
		 *******************************/

typedef void (*sighandler_t)(int);

static int
PrologAction(int action, va_list args)
{ switch(action)
  { case HOST_TRACE:
      SP_action(SP_ACTION_TRACE, NULL);
      return PCE_SUCCEED;
    case HOST_HALT:
      SP_action(SP_ACTION_HALT, NULL);
      return PCE_FAIL;				/* should not get here */
    case HOST_BREAK:
    { SP_pred_ref pred = SP_predicate("break", 0, "user");

      SP_query_cut_fail(pred);
      return PCE_SUCCEED;
    }
    case HOST_ABORT:
      SP_action(SP_ACTION_ABORT, NULL);
      return PCE_SUCCEED;
    case HOST_SIGNAL:
    { int sig = va_arg(args, int);
      sighandler_t func = va_arg(args, sighandler_t);

      signal(sig, func);		/* Not clear whether or not to SP_ */
      return PCE_SUCCEED;
    }
    case HOST_RECOVER_FROM_FATAL_ERROR:
      SP_action(SP_ACTION_ABORT, NULL);
      return PCE_FAIL;			/* could not abort: failure */
    case HOST_BACKTRACE:
    case HOST_ATEXIT:
    default:
      return PCE_FAIL;
  }
}


static int
PrologQuery(int what, PceCValue *value)
{ switch(what)
  {
#ifdef _CONSOLE_H_INCLUDED		/* Win32 console */
    case HOST_CONSOLE:
      if ( (value->pointer = indirect_rlc_hwnd()) )
	return PCE_SUCCEED;
      return PCE_FAIL;
#endif
    default:
      return PCE_FAIL;
  }
}

#endif /*SICSTUS*/

#ifdef SWI

/* No need to redirect allocation over SWI-Prolog.  In particular, we
   do not want to use Boehm-GC for XPCE as long as it has its own GC.

   Note that using Boehm-GC would be much better for XPCE; it is
   precisely the sort of application Boehm-GC is designed for.
*/

#define pl_malloc  NULL
#define pl_realloc NULL
#define pl_free    NULL


		 /*******************************
		 *	    CONSOLE I/O		*
		 *******************************/

#define XPCE_OUTPUT Suser_output	/* log in current console */
#define XPCE_INPUT Suser_input

void
pl_Cvprintf(const char *fmt, va_list args)
{ Svfprintf(XPCE_OUTPUT, fmt, args);
}


static int
pl_Cputchar(int c)
{ return Sputcode(c, XPCE_OUTPUT);
}


static void
pl_Cflush(void)
{ Sflush(XPCE_OUTPUT);
}


static char *
pl_Cgetline(char *buf, int size)
{ return Sfgets(buf, size, XPCE_INPUT);
}


		 /*******************************
		 *	HOST ACTION/QUERY	*
		 *******************************/

typedef void (*sighandler_t)(int);

static int
swi_halt_hook(int rc, void *closure)
{ OnExitFunction f = closure;

  return (*f)(rc);
}

static int
PrologAction(int action, va_list args)
{ switch(action)
  { case HOST_TRACE:
      PL_action(PL_ACTION_TRACE, NULL);
      return PCE_SUCCEED;
    case HOST_HALT:
      PL_action(PL_ACTION_HALT, NULL);
      return PCE_FAIL;				/* should not get here */
    case HOST_BREAK:
      PL_action(PL_ACTION_BREAK, NULL);
      return PCE_SUCCEED;
    case HOST_ABORT:
      PL_action(PL_ACTION_ABORT, NULL);
      return PCE_SUCCEED;
    case HOST_SIGNAL:
    { int sig = va_arg(args, int);
      sighandler_t func = va_arg(args, sighandler_t);

      PL_signal(sig, func);
      return PCE_SUCCEED;
    }
    case HOST_RECOVER_FROM_FATAL_ERROR:
      PL_action(PL_ACTION_ABORT, NULL);
      return PCE_FAIL;			/* could not abort: failure */
    case HOST_CHECK_INTERRUPT:
      PL_handle_signals();
      return PCE_SUCCEED;
    case HOST_BACKTRACE:
    { int frames = va_arg(args, int);
      PL_action(PL_ACTION_BACKTRACE, (void *)(intptr_t)frames);
      return PCE_SUCCEED;
    }
    case HOST_ATEXIT:
    { OnExitFunction f = va_arg(args, OnExitFunction);

      PL_on_halt(swi_halt_hook, f);

      return PCE_SUCCEED;
    }
    default:
      return PCE_FAIL;
  }
}


static int
PrologQuery(int what, PceCValue *value)
{ switch(what)
  {
#ifdef _CONSOLE_H_INCLUDED		/* Win32 console */
    case HOST_CONSOLE:
      if ( (value->pointer = indirect_rlc_hwnd()) )
	return PCE_SUCCEED;
      return PCE_FAIL;
#endif
    case HOST_ENCODING:
    { IOENC enc;

      enc = (IOENC)PL_query(PL_QUERY_ENCODING);
      value->integer = enc;

      return PCE_SUCCEED;
    }
    default:
      return PCE_FAIL;
  }
}

#endif /*SWI*/

		 /*******************************
		 *	     RESOURCE		*
		 *******************************/

static IOSTREAM *
PrologOpenResource(const char *name, const char *rc_class, const char *mode)
{ return PL_open_resource(pceContextModule(), name, rc_class, mode);
}


		 /*******************************
		 *	      PROFILING		*
		 *******************************/

static int
unify_prof_node(term_t t, void *impl)
{ return unifyObject(t, impl, FALSE);
}


static int
get_prof_node(term_t ref, void **impl)
{ atom_t name;
  size_t arity;

  if ( GetNameArity(ref, &name, &arity) &&
       name == ATOM_ref &&
       arity == 1 )
  { *impl = termToObject(ref, NULL, NULLATOM, FALSE);
    return TRUE;
  }

  return FALSE;
}


static void
prof_activate(int active)
{ pce_profile_hooks hooks;

  memset(&hooks, 0, sizeof(hooks));
  if ( active )
  { hooks.call   = (void*(*)(void*, void*))PL_prof_call;
    hooks.exit   = PL_prof_exit;
    hooks.handle = &pceProfType;
  }

  pceSetProfileHooks(&hooks);
  prof_active = active;
}

static void
registerProfiler()
{ pceProfType.unify    = unify_prof_node;
  pceProfType.get      = get_prof_node;
  pceProfType.activate = prof_activate;

  PL_register_profile_type(&pceProfType);
}


		 /*******************************
		 *	     TRANSLATE		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Translate a Prolog term. This can be  any   term  that is not handled by
get_object_arg(): a compound, string or variable.   If it is a compound,
it may be new(class) or new(Ref, Class), as well as a list.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static PceObject
PrologTranslate(PceObject hd, PceObject type)
{ term_t t;

  if ( (t = getTermHandle(hd)) )
    return termToObject(t, type, NULLATOM, FALSE);

  assert(0);
  return NULL;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Called by writeGoal() to print the arguments  of the goal handled by the
host-language. The non-vararg arguments are in g->argv[], represented as
Prolog terms.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
PrologWriteGoalArgs(PceGoal g)
{ int i, argn = 0;
  term_t l;

  for(i=0; i<g->argc; i++)
  { if ( argn++ )
      Sprintf(", ");
    if ( g->argv[i] )
      PL_write_term(Soutput, (term_t)g->argv[i], 999, PL_WRT_PORTRAY);
    else
      Sprintf("(nil)");
  }

  if ( g->va_type && (l = (term_t)g->host_closure) )
  { term_t tail = PL_copy_term_ref(l);
    term_t head = PL_new_term_ref();

    while( PL_get_list(tail, head, tail) )
    { if ( argn++ )
	Sprintf(", ");
      PL_write_term(Soutput, head, 999, PL_WRT_PORTRAY);
    }
  }

  return TRUE;
}



		 /*******************************
		 *	  SETUP CALLBACK	*
		 *******************************/

static pce_callback_functions callbackfunction =
{ PrologSend,
  PrologGet,
  PrologCall,
  PrologQuery,
  PrologAction,
  pl_Cvprintf,				/* Console output */
  pl_Cputchar,
  pl_Cflush,
  pl_Cgetline,
  pl_malloc,				/* Prolog memory management hooks */
  pl_realloc,
  pl_free,
  PrologOpenResource,			/* resource handling */
  getPrologContext,			/* (Module) context */
  setPrologContext,
  PrologTranslate,
  PrologWriteGoalArgs
};


#ifndef PROLOG_ARGC
#define PROLOG_ARGC() 0
#endif
#ifndef PROLOG_ARGV
#define PROLOG_ARGV() ((char **)NULL)
#endif
#ifndef PROLOG_INSTALL_REINIT_FUNCTION
#define PROLOG_INSTALL_REINIT_FUNCTION(x)
#endif
#ifndef PROLOG_ITF_INIT
#define PROLOG_ITF_INIT()
#endif
#ifndef PROLOG_INSTALL_CALLBACKS
#define PROLOG_INSTALL_CALLBACKS()
#endif
#ifndef PROLOG_INSTALL_DISPATCH_FUNCTION
#define PROLOG_INSTALL_DISPATCH_FUNCTION(f)
#endif
#ifndef PROLOG_INSTALL_RESET_FUNCTION
#define PROLOG_INSTALL_RESET_FUNCTION(f)
#endif
#ifndef PROLOG_INSTALL_REDRAW_FUNCTION
#define PROLOG_INSTALL_REDRAW_FUNCTION(f)
#endif


static void
do_reset(void)
{ pceReset();				/* reset XPCE itself */
  rewindHostHandles(NULL);		/* invalidate them all */
}


#ifdef __WINDOWS__
static void
do_redraw(void)
{ pceRedraw(FALSE);			/* FALSE: do not sync Xserver */
}
#endif

static int
hasThreadsProlog()
{ predicate_t pred = PL_predicate("current_prolog_flag", 2, "user");
  term_t av = PL_new_term_refs(2);

  PL_put_atom_chars(av+0, "threads");
  PL_put_atom_chars(av+1, "true");
  return PL_call_predicate(NULL, PL_Q_NORMAL, pred, av);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Call this function as Prolog is about to detach this thread, so XPCE can
delete all windows operating in this thread.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
detach_thread(void *closure)
{ pceMTdetach();
}


foreign_t
pl_pce_init(term_t Home, term_t AppDir)
{ char **argv;
  int argc;
  const char *home, *appdata;
  atom_t ahome, aappdata;
  static int initialised = FALSE;

  if ( GetAtom(Home, &ahome) )
    home = AtomCharp(ahome);
  else
    home = NULL;
  if ( GetAtom(AppDir, &aappdata) )
    appdata = AtomCharp(aappdata);
  else
    appdata = NULL;

  argc = PROLOG_ARGC();
  argv = PROLOG_ARGV();

  if ( !initialised )
  { PceObject plname;

    initialised = TRUE;

    if ( hasThreadsProlog() )
    { if ( pceMTinit() )
      { PL_thread_at_exit(detach_thread, NULL, TRUE);
      } else
      { Sdprintf("Warning: this version of XPCE is not compiled to support\n"
		 "Warning: multiple threads.\n");
      }
    }

    PROLOG_INSTALL_REINIT_FUNCTION(pl_pce_init);
    PROLOG_ITF_INIT();

    pceRegisterCallbacks(&callbackfunction);
    initNameAtomTable();
    if ( !pceInitialise(0, home, appdata, argc, argv) )
      return FALSE;

    initPceConstants();			/* get code used PCE constants */
    initPrologConstants();		/* Public prolog constants */
    initHostConstants();		/* Host-specific Prolog constants */
    registerProfiler();			/* hook the profilers */

    plname = cToPceName("prolog");
    pceSend(PROLOG, NULL, cToPceName("name_reference"), 1, &plname);
    PROLOG_INSTALL_DISPATCH_FUNCTION(pce_dispatch);
    PROLOG_INSTALL_RESET_FUNCTION(do_reset);
    PROLOG_INSTALL_REDRAW_FUNCTION(do_redraw);
  }

  return TRUE;
}
