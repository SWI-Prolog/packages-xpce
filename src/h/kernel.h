/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  1985-2007, University of Amsterdam
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

#define PCE_INCLUDED 1

#include <md.h>

#define O_NOX11RESOURCES 1		/* use own resource parser */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>
#ifdef __APPLE__			/* defines INT_MAX, so we must */
#include <time.h>			/* do that before we do it here */
#endif
#include <h/stream.h>			/* IOSTREAM interface */
#include <limits.h>

#ifndef INT_MAX
#define INT_MAX	    ((int)(((unsigned int)1<<(sizeof(int)*8-1))-1))
#define INT_MIN     (-(INT_MIN)-1)
#endif

#ifdef HAVE_DMALLOC_H
#include <dmalloc.h>			/* Use www.dmalloc.com debugger */
#endif
#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif

#ifdef HAVE_XOS_H
#include <xos.h>
#endif

#ifdef HAVE_SYS_FILE_H
#include <sys/file.h>
#endif

#ifndef GLOBAL
#define GLOBAL extern			/* global variables */
#define PUBLIC_GLOBAL extern		/* exported global variables */
#endif

#if defined(WIN32) || defined(__CYGWIN__)
#define __pce_export __declspec(dllexport)
#else
#define __pce_export extern
#endif

#ifndef export				/* WIN32 DLL export stuff */
#define export
#endif

		 /********************************
		 * AVOID ACCIDENTAL USE OF STDIO *
		 ********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The XPCE library should not use STDIO   to allow for embedding in window
environments. We undefine these  symbols  here   to  make  the  compiler
generate warnings on accidental use of them.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifndef __osf__
#undef stdin
#undef stdout
#undef stderr
#endif
#undef printf
#undef putchar
#undef getchar


		 /*******************************
		 *	SOME SYSTEM STUFF	*
		 *******************************/

#ifdef SYSLIB_H
#include SYSLIB_H
#endif

		/********************************
		*             LIMITS		*
		********************************/

#define PCE_MAX_RECURSION	1000	/* maximum send() recursion */
#define METHOD_MAX_ARGS		16	/* maximum # args for C-method */
#define FWD_PCE_MAX_ARGS	10	/* @arg1 ... @arg10 */
#define SCAN_MAX_ARGS		32	/* scanstr maximum arguments */
#define PCE_MAX_INT		((intptr_t)(((intptr_t)1<<(sizeof(Any)*8 - TAG_BITS-1))-1))
#define PCE_MIN_INT		(-(PCE_MAX_INT-1))
#ifndef INT_MAX
#define INT_MAX			((int)(((unsigned int)1<<(sizeof(int)*8-1))-1))
#define INT_MIN			(-(INT_MIN)-1)
#endif

#define LINESIZE		2048	/* maximum length of a line */
#define FORMATSIZE		10000	/* maximum length of a ->format */
#define BROWSER_LINE_WIDTH	256	/* maximum #chars of line in browser */

		/********************************
		*           OS STUFF		*
		********************************/

#ifndef SIGNAL_HANDLER_TYPE		/* type returned by signal-handler */
#define SIGNAL_HANDLER_TYPE void
#endif

		 /*******************************
		 *	       CLEANUP		*
		 *******************************/

#define ATEXIT_FILO	0x1
#define ATEXIT_FIFO	0x2

typedef void			(*atexit_function)(int status);

		 /*******************************
		 *	 VARARG FUNCTIONS	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Handling of send(Receiver, Method, ..., EAV),  etc. Note that EAV cannot
be just 0, as  on  some  machines   int  and  pointers  are  promoted to
different types when passed to a vararg function.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define VA_PCE_MAX_ARGS		10	/* send(), etc. */
#define EAV			((Any)0) /* End of the list */


		/********************************
		*       SAVING OBJECTS		*
		********************************/

#define SAVEMAGIC		"PCE version 4"
#define SAVEVERSION		18	/* last increment for 5.6.14 */

		/********************************
		*             ASSERTS		*
		********************************/

#undef assert
#ifdef NOASSERT
#define assert(expr) ((void)0)
#else
#define assert(expr) ((expr) ? (void)0 : \
			       (void)pceAssert(0,#expr,__FILE__,__LINE__))
#endif

		/********************************
		*        COMPILER STUFF		*
		********************************/

#if __STRICT_ANSI__
#undef TAGGED_LVALUE
#endif

#ifdef __GNUC__
# if !__STRICT_ANSI__			/* gcc -ansi */
#  ifndef O_INLINE
#   define O_INLINE 1
#  endif
#  define O_CONST_FUNCTION 1
# endif
# define Promote(type) int
#else
# define Promote(type) type
#endif

#ifdef HAVE_VISIBILITY_ATTRIBUTE
#define SO_LOCAL __attribute__((visibility("hidden")))
#else
#define SO_LOCAL
#endif

#if !O_INLINE
#define inline
#endif

#ifdef __GNUC__
#define PURE_FUNCTION __attribute__((pure))
#else
#define PURE_FUNCTION
#endif

#define forwards	static		/* Local functions */

#if __GNUC__ && !__STRICT_ANSI__
#define LocalArray(t, n, s)	t n[s]
#else
#define LocalArray(t, n, s)	t *n = (t *) alloca((s)*sizeof(t))
#endif

#define ArgVector(name, s)	LocalArray(Any, name, s)
#define CharBuf(name, s)	LocalArray(unsigned char, name, (s)+1)

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
cpdata(to, from, type, n)
    copies n objects of type type from `from' to `to'.  Does not deal with
    overlapping, but is much faster on relatively small data pieces then
    memcpy(to, from, (n)*sizeof(type)), to which it is equivalent.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#if 1
#define cpdata(to, from, type, n) memcpy(to, from, (n)*sizeof(type))
#else
#define cpdata(to, from, type, n) do \
	{ type *_t = (type*)(to)-1; \
	  type *_f = (type*)(from)-1; \
	  int   _i = (n); \
	  while(--_i >= 0) *++_t = *++_f; \
	} while(0)
#endif
#define setdata(to, val, type, n) do \
	{ type *_t = (to)-1; \
	  int   _i = (n); \
	  type  _v = (val); \
	  while(--_i >= 0) *++_t = _v; \
	} while(0)

#define NOTREACHED	assert(0)	/* should not get here */

#ifdef O_EXTRA_SYSTEM_TYPES
#include O_EXTRA_SYSTEM_TYPES
#endif

		 /*******************************
		 * OS-IDENTIFIERS (STRICT_ANSI) *
		 *******************************/

#ifndef __unix__
#if defined(_AIX) || defined(__APPLE__) || defined(__unix) || defined(__BEOS__) || defined(__NetBSD__)
#define __unix__ 1
#endif
#endif

#if defined(__unix__) && !defined(unix)
#define unix 1
#endif

		 /*******************************
		 *	 LIBRARY PROBLEMS	*
		 *******************************/

#ifndef StrTod
#define StrTod(s, e)	strtod(s, e)
#endif

		/********************************
		*         NAME CONFLICTS	*
		********************************/

#define	CtoInt(i)	toInt(i)		/* int --> Int */
#define CtoName(s)	(Name)cToPceName((s))	/* const char * --> Name */
#define CtoType(s)	nameToType(CtoName(s))  /* char * --> type object */
#define WCtoType(s)	nameToType(WCToName(s, -1)) /* wchar_t * --> type object */
#define	pp(x)		pcePP((Any)(x))		/* interface name */
#define get             getPCE			/* avoid common name-conflict */
#define send            sendPCE			/* same */
#define toString	toStringPCE		/* SWI-Prolog name-conflict */
#define valReal		valPceReal		/* and another */

		 /*******************************
		 *	    BASIC TYPES		*
		 *******************************/

typedef int			status;		/* FAIL, SUCCEED */
typedef void *			Any;		/* Arbitrary object */

typedef Any			Int;		/* ZERO, ONE, ... */
typedef Any			(*Func)();	/* Absolete GetFunc (TBD) */
typedef Any			(*GetFunc)();	/* GetMethod implementation */
typedef status			(*SendFunc)();	/* SendMethod implementation */
typedef void			(*VoidFunc)();

typedef void *			WsRef;		/* Window-system reference */
typedef struct xref *		Xref;		/* Entry in ws-table */

typedef struct classdef        *ClassDef;	/* See pce-save.c */
typedef struct dCell	      **DelegateList;   /* See msg-passing.c */

#include "types.h"

#define INVOKE_FUNC ((SendFunc)~0L)

		/********************************
		*	    POINTERS		*
		********************************/

#ifdef VARIABLE_POINTER_OFFSET
#undef POINTER_OFFSET
GLOBAL uintptr_t pce_data_pointer_offset;
#define POINTER_OFFSET pce_data_pointer_offset
#else
#ifndef POINTER_OFFSET
#define POINTER_OFFSET (0L)
#endif
#endif

#define PointerToCInt(p) (((uintptr_t)(p) - POINTER_OFFSET)/sizeof(void*))
#define PointerToInt(p)	 toInt(PointerToCInt(p))
#define longToPointer(i) ((Any) (i * sizeof(void*) + POINTER_OFFSET))
#define IntToPointer(i)  longToPointer(valInt(i))


		/********************************
		*           TAG MASKS		*
		********************************/

#define INT_MASK	0x00000001	/* 10 mask for Int (integers) */
#define MASK_MASK	0x00000001	/* 11 Mask Mask */
#define TAG_BITS	1		/* number of mask bits for INT */

#define MaskOf(obj)		((uintptr_t)(obj) & MASK_MASK)
#define UnMask(obj)		((uintptr_t)(obj) & ~MASK_MASK)


		/********************************
		*           EQUALITY		*
		********************************/

#define EQI(o1, o2)	((Any)(o1) == (Any)(o2))
#define EQ(o1, o2)	EQI(o1, o2)


		/********************************
		*             TYPES		*
		********************************/

#define ARGC_UNKNOWN		(-1)
#define ARGC_INHERIT		(-2)	/* classdecl */

		/********************************
		*           FUNCTIONS		*
		********************************/

#define isFunction(obj)		(isObject(obj) && onFlag(obj, F_ACTIVE))
#define isHostData(obj)		(isObject(obj) && onFlag(obj, F_ISHOSTDATA))


		/********************************
		*         PCE INTEGERS		*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PCE uses tagged integers rather than C   integers. The top TAG_BITS bits
hold the MASK whereas the remaining bits  hold the integer itself. A PCE
integer is declared as of type Int (for casting purposes). The following
test, conversion and computation macro's are provided.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/* TBD: INTPTR_FORMAT is available as PRTdPTR from <inttypes.h>.  Our
   include for fixed size integers is a bit flaky on various platforms
   though
*/

#ifndef INTPTR_FORMAT		/* printf format for intptr_t */
#if SIZEOF_INT == SIZEOF_VOIDP
#define INTPTR_FORMAT "%d"
#elif SIZEOF_LONG == SIZEOF_VOIDP
#define INTPTR_FORMAT "%ld"
#elif defined(__WIN64)
#define INTPTR_FORMAT "%I64d"
#else
#error "Config needs INTPTR_FORMAT"
#endif
#endif

#undef max
#undef min
#define max(a, b)	((a) > (b) ? (a) : (b))
#define min(a, b)	((a) < (b) ? (a) : (b))

#define isInteger(i)	((uintptr_t)(i) & INT_MASK)
#define toInt(i)	((Int)(((uintptr_t)(i)<<TAG_BITS)|INT_MASK))
#define valInt(i)	(((intptr_t)(i))>>TAG_BITS)
#define incrInt(i)	((i) = toInt(valInt(i)+1))
#define decrInt(i)	((i) = toInt(valInt(i)-1))
#define addInt(i, j)	((i) = toInt(valInt(i) + valInt(j)))
#define subInt(i, j)	((i) = toInt(valInt(i) - valInt(j)))
#define maxInt(i, j)	toInt(max(valInt(i), valInt(j)))
#define absInt(i)	(valInt(i) < 0 ? neg(i) : i)

#undef div
#define neg(i)		(toInt(-valInt(i)))
#define add(i, j)	(toInt(valInt(i) + valInt(j)))
#define sub(i, j)	(toInt(valInt(i) - valInt(j)))
#define div(i, j)	(toInt(valInt(i) / valInt(j)))
#define mul(i, j)	(toInt(valInt(i) * valInt(j)))
#define avg(i, j)	(toInt((valInt(i) + valInt(j))/2))
#define mid(i, j)	(toInt((valInt(i) + valInt(j)/2)))
#define dif(i, j)	(toInt((valInt(i) - valInt(j)/2)))
#define inc(i)		(toInt(valInt(i) + 1))
#define dec(i)		(toInt(valInt(i) - 1))
#define minInt(i)	(toInt(-valInt(i)))

#define ZERO		toInt(0)	/* PCE Int 0 */
#define ONE		toInt(1)	/* PCE Int 1 */
#define TWO		toInt(2)	/* PCE Int 2 */


		/********************************
		*          DFLAG VALUES		*
		********************************/


#define makeDFlag(n)		(1L << ((n) - 1 + TAG_BITS))
#define DFlags(obj)		(((ProgramObject)(obj))->dflags)
#ifndef TAGGED_LVALUE
#define setDFlag(obj, mask)     setDFlagProgramObject((obj), (mask))
#define clearDFlag(obj, mask)	clearDFlagProgramObject((obj), (mask))
#else
#define setDFlag(obj, mask)	(DFlags(obj) |= (mask))
#define clearDFlag(obj, mask)	(DFlags(obj) &= ~(mask))
#endif
#define onDFlag(obj, mask)	(DFlags(obj) & (mask))
#define offDFlag(obj, mask)	(!onDFlag(obj, mask))

					/* Debugging flags */
#define D_TRACE_ENTER	  makeDFlag(1)	/* Trace enter port of method */
#define D_TRACE_EXIT	  makeDFlag(2)	/* Trace exit port of method */
#define D_TRACE_FAIL	  makeDFlag(3)	/* Trace fail port of method */
#define D_TRACE		  (D_TRACE_ENTER|D_TRACE_EXIT|D_TRACE_FAIL)

#define D_BREAK_ENTER	  makeDFlag(4)	/* Break enter port of method */
#define D_BREAK_EXIT	  makeDFlag(5)	/* Break exit port of method */
#define D_BREAK_FAIL	  makeDFlag(6)	/* Break fail port of method */
#define D_BREAK		  (D_BREAK_ENTER|D_BREAK_EXIT|D_BREAK_FAIL)

#define D_SYSTEM	  makeDFlag(7) /* Generate system trace frame */

					/* Variable attributes */
#define D_SAVE_NORMAL	  makeDFlag(8) /* Save normally */
#define D_SAVE_NIL	  makeDFlag(9) /* Save as NIL */
#define D_SAVE		  (D_SAVE_NORMAL|D_SAVE_NIL)

#define D_CLONE_RECURSIVE makeDFlag(10) /* Clone object recursively */
#define D_CLONE_REFERENCE makeDFlag(11) /* Clone object reference */
#define D_CLONE_NIL	  makeDFlag(12) /* Cloned value is @nil */
#define D_CLONE_VALUE	  makeDFlag(13) /* Clone the plain PCE value */
#define D_CLONE_ALIEN	  makeDFlag(14) /* Clone alien values */
#define D_CLONE_REFCHAIN  makeDFlag(15) /* Value is a reference chain */
#define D_CLONE		  (D_CLONE_RECURSIVE|D_CLONE_REFERENCE|\
			   D_CLONE_NIL|D_CLONE_VALUE|D_CLONE_ALIEN|\
			   D_CLONE_REFCHAIN)

#define D_ALIEN		  makeDFlag(16) /* Variable is alien */
#define D_TYPENOWARN	  makeDFlag(17) /* Methods: donot warn */

					 /* Class attributes */
#define DC_LAZY_GET	  makeDFlag(18) /* bind get-behaviour lazy */
#define DC_LAZY_SEND	  makeDFlag(19) /* bind send-behaviour lazy */
#define D_CXX		  makeDFlag(20) /* C++ defined method/class */

					 /* ClassVariable attributes */
#define DCV_TEXTUAL	  makeDFlag(21) /* Default is textual */

					/* Method */
#define D_HOSTMETHOD	  makeDFlag(22) /* Implementation is in the host */
#define D_SERVICE	  makeDFlag(23)	/* Execute in `service' mode */


		/********************************
		*       CHAR_ARRAY, STRING	*
		********************************/

#include "str.h"			/* string type and friends */
#include "../txt/proto.h"		/* prototypes */

#define LocalString(name, iswide, size) \
  string _s_ ## name ## _hdr; \
  void  *_s_ ## name ## _buf = (void *)alloca(iswide ? (size) * sizeof(charW) \
						   : (size) * sizeof(charA)); \
  String name = fstr_inithdr(&_s_ ## name ## _hdr, iswide, _s_ ## name ## _buf, size)

		/********************************
		*         OBJECT HEADER		*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
[q]assign(obj, slot, value)
    assign() assigns a slot a value, qassign() does the same, but bypasses
    the object management system.  It should be used in very time-critical
    code where the value is constant (Int, Name, Constant).  Nowhere else!
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define assign(o, s, v)	assignField((Instance) (o), \
				    (Any *) &((o)->s), \
				    (Any) (v))
#define qassign(o, s, v) ((o)->s = (v))

#define makeFlag(n)		(1L << ((n) - 1))
#define setFlag(obj, mask)	(((Instance)(obj))->flags |= (mask))
#define clearFlag(obj, mask)	(((Instance)(obj))->flags &= ~(mask))
#define onFlag(obj, mask)	(((Instance)(obj))->flags & (mask))
#define offFlag(obj, mask)	(!onFlag(obj, mask))

#define	F_LOCKED		makeFlag(1)
#define	F_CREATING		makeFlag(2)
#define	F_FREED			makeFlag(3)
#define	F_FREEING		makeFlag(4)
#define	F_PROTECTED		makeFlag(5)
#define	F_ANSWER		makeFlag(6)
#define F_INSPECT		makeFlag(7)
#define F_ACTIVE		makeFlag(8)  /* Active object */
#define F_CONSTRAINT		makeFlag(9)  /* has constraints */
#define F_ATTRIBUTE		makeFlag(10) /* has attributes */
#define F_SENDMETHOD		makeFlag(11) /* has send-methods */
#define F_GETMETHOD		makeFlag(12) /* has get-methods */
#define F_HYPER			makeFlag(13) /* has hypers */
#define F_RECOGNISER		makeFlag(14) /* has recognisers */
#define F_ASSOC			makeFlag(15) /* has name-assoc */
#define F_ITFNAME		makeFlag(16) /* Name known to itf table */
#define F_SOLID			makeFlag(17) /* Solid graphical object */
#define F_OBTAIN_CLASSVARS	makeFlag(18) /* obtainClassVariablesObject() */
#define F_TEMPLATE_METHOD	makeFlag(19) /* method<-instantiate_template */
#define F_ISBINDING		makeFlag(20) /* instanceOf(x, ClassBinding) */
#define F_ISNAME		makeFlag(21) /* instanceOf(x, ClassName) */
#define F_ISREAL		makeFlag(22) /* instanceOf(x, ClassReal) */
#define F_ISHOSTDATA		makeFlag(23) /* instanceOf(x, ClassHostData) */
#define F_NOTANY		makeFlag(24) /* Not acceptable to any/object */

#define OBJ_MAGIC		((uintptr_t)(0x94L << 25))
#define OBJ_MAGIC_MASK		((uintptr_t)(0xfeL << 25))

#define hasObjectMagic(obj)	((((Instance)(obj))->flags&OBJ_MAGIC_MASK) == \
					OBJ_MAGIC)

#define initHeaderObj(obj, cl) \
  { (obj)->class	= (cl); \
    (obj)->flags        = F_CREATING|OBJ_MAGIC; \
    (obj)->references   = 0L; \
  }

#define classOfObject(obj)	(((Instance)(obj))->class)

#define	setProtectedObj(obj)	setFlag(obj, F_PROTECTED)
#define	clearProtectedObj(obj)	clearFlag(obj, F_PROTECTED)
#define	isProtectedObj(obj)	onFlag(obj, F_PROTECTED)
#define	setCreatingObj(obj)	setFlag(obj, F_CREATING)
#define	clearCreatingObj(obj)	clearFlag(obj, F_CREATING)
#define	isCreatingObj(obj)	onFlag(obj, F_CREATING)
#define	setAnswerObj(obj)	setFlag(obj, F_ANSWER)
#define	clearAnswerObj(obj)	clearFlag(obj, F_ANSWER)
#define	isAnswerObj(obj)	onFlag(obj, F_ANSWER)

#define ONE_CODE_REF		(1L<<20)

#define refsObject(obj)		(((Instance)obj)->references % ONE_CODE_REF)
#define codeRefsObject(obj)	(((Instance)obj)->references / ONE_CODE_REF)
#define noRefsObj(obj)		(((Instance)obj)->references == 0L)
#define addRefObj(obj)		(((Instance)obj)->references++)
#define delRefObj(obj)		(((Instance)obj)->references--)
#define lockObj(obj)		setFlag(obj, F_LOCKED)
#define unlockObj(obj)		clearFlag(obj, F_LOCKED)
#define lockedObj(obj)		onFlag(obj, F_LOCKED)
#define setFreedObj(obj)	setFlag(obj, F_FREED)
#define isFreedObj(obj)		onFlag(obj, F_FREED)
#define setFreeingObj(obj)	setFlag(obj, F_FREEING)
#define isFreeingObj(obj)	onFlag(obj, F_FREEING)
#define isVirginObj(o)		(noRefsObj(o) && \
				 !onFlag(o, F_LOCKED|F_PROTECTED|F_ANSWER))
#define freeableObj(o)		if ( isVirginObj(o) ) \
				  freeObject(o)
#define checkDeferredUnalloc(o)	if ( (((Instance)o)->references) <= 0 ) \
				  unreferencedObject(o)

#define GcProtect(o, g)		do { \
				addCodeReference(o); \
				g; \
				delCodeReference(o); } while(0)



		/********************************
		*            CONSTANTS		*
		********************************/

#define NIL		((Any)(&ConstantNil))
#define DEFAULT		((Any)(&ConstantDefault))
#define CLASSDEFAULT	((Any)(&ConstantClassDefault))
#define ON		(&BoolOn)
#define OFF		(&BoolOff)

#define isOn(val)	((BoolObj)(val) == ON)
#define isOff(val)	((BoolObj)(val) == OFF)
#define isBoolean(val)	((BoolObj)(val) == ON || (BoolObj)(val) == OFF)

#define isNil(o)	((Constant)(o) == NIL)
#define notNil(o)	((Constant)(o) != NIL)
#define isDefault(o)	((Constant)(o) == DEFAULT)
#define notDefault(o)	((Constant)(o) != DEFAULT)
#define isClassDefault(o)	((Constant)(o) == CLASSDEFAULT)
#define notClassDefault(o)	((Constant)(o) != CLASSDEFAULT)

#define TrueOrFalse(b)	(isOn(b) ? TRUE : FALSE)

#define nonObject(obj)	(MaskOf(obj) || !(obj))
#define isObject(obj)	(!nonObject(obj))

		/********************************
		*         CAREFUL CHECKERS	*
		********************************/

#define validAddress(a)	((uintptr_t)(a) >= allocBase && \
			 (uintptr_t)(a) < allocTop)
#define isAddress(a)	(validAddress(a) && \
			 !((uintptr_t)(a) & (sizeof(Any)-1)))
#define validPceDatum(x) (isInteger(x) || isProperObject(x))


#ifndef TRUE
#define TRUE		1	/* boolean truth value */
#define FALSE		0	/* boolean false value */
#endif

#define FAIL		0	/* message failed */
#define SUCCEED		1	/* message completed successful */

#define fail		return FAIL
#define succeed		return SUCCEED
#define answer(v)	return (v)

#define DONE(goal)	if ( (goal)  ) succeed
#define TRY(goal)	if ( !(goal) ) fail
#define EXISTS(object)	if ( isNil(object) ) fail


		/********************************
		*       CLASS STRUCTURES	*
		********************************/

#define OBJECT_HEADER \
  uintptr_t     flags;			/* general flag field */ \
  uintptr_t     references;		/* reference count */ \
  Class		class;			/* Associated class */

#define ABSTRACT_OBJECT

#define ABSTRACT_PROGRAM_OBJECT \
  uintptr_t	dflags;			/* Debugging flags */

#define ABSTRACT_RECOGNISER \
  BoolObj		active;			/* Does accept events? */

#define ABSTRACT_CODE \
  ABSTRACT_PROGRAM_OBJECT

#define ABSTRACT_FUNCTION  \
  ABSTRACT_CODE

#define ABSTRACT_BINARY_EXPRESSION \
  ABSTRACT_FUNCTION \
  Expression	left;			/* Left-hand side */ \
  Expression	right;			/* Right-hand side */

#define ABSTRACT_BINARY_CONDITION \
  ABSTRACT_CODE \
  Expression	left;			/* Left-hand side */ \
  Expression	right;			/* Right-hand side */

#define ABSTRACT_VISUAL

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
NewClass(class) privides the structure header for any class  that is a
subclass of class `object'.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define NewClass(x) \
  struct x \
  { OBJECT_HEADER \
    ABSTRACT_OBJECT
#define End \
  }

struct instance
{ OBJECT_HEADER				/* non-pce header */
  Any		slots[1];		/* array of slots. */
};

NewClass(object)
End;

NewClass(program_object)
  ABSTRACT_PROGRAM_OBJECT
End;

NewClass(vmi)
  ABSTRACT_PROGRAM_OBJECT
  Name		name;			/* Name of vmi */
End;

NewClass(c_pointer)
  void *	pointer;		/* the pointer value */
End;

NewClass(area)
  Int		x;			/* position and dimension */
  Int		y;
  Int		w;
  Int		h;
End;

NewClass(atable)
  Vector	keys;			/* bool vector stating key fields */
  Vector	names;			/* parameter names of the entries */
  Vector	tables;			/* hash tables */
End;

NewClass(tuple)
  Any		first;			/* first of tuple */
  Any		second;			/* second element of tuple */
End;

#define ABSTRACT_BEHAVIOUR \
  ABSTRACT_PROGRAM_OBJECT \
  Name		name;			/* Name of the behaviour */ \
  Any		context;		/* Object or class I belong too */

#define ABSTRACT_VARIABLE \
  ABSTRACT_BEHAVIOUR \
  Name		group;			/* Conceptual group */ \
  Name		access;			/* whether send/get may be used */ \
  Type		type;			/* type of contents */ \
  Int		offset;			/* offset from base (from 0) */ \
  StringObj	summary;		/* Summary for variable */ \
  Any		init_function;		/* Function to initialise */ \
  Any		alloc_value;		/* Allocate value of variable */

#ifndef O_RUNTIME
#define ABSTRACT_METHOD \
  ABSTRACT_BEHAVIOUR \
  Name		group;			/* Conceptual group */ \
  Vector	types;			/* type checking codes */ \
  StringObj	summary;		/* Summary of this method */ \
  SourceLocation source;		/* Location of def in sources */ \
  Code		message;		/* message implementing method */

#else /*O_RUNTIME*/

#define ABSTRACT_METHOD \
  ABSTRACT_BEHAVIOUR \
  Name		group;			/* Conceptual group */ \
  Vector	types;			/* type checking codes */ \
  StringObj	summary;		/* Summary of this method */ \
  Code		message;		/* message implementing method */
#endif/*O_RUNTIME*/

NewClass(behaviour)
  ABSTRACT_BEHAVIOUR
End;

NewClass(method)
  ABSTRACT_METHOD
  Func		function;		/* C-function implementing method */
End;

NewClass(send_method)
  ABSTRACT_METHOD
  SendFunc	function;		/* C-function implementing method */
End;

NewClass(get_method)
  ABSTRACT_METHOD
  GetFunc	function;		/* C-function implementing method */
  Type		return_type;		/* Type of returned value */
End;

NewClass(variable)
  ABSTRACT_VARIABLE
End;

NewClass(attribute)
  ABSTRACT_PROGRAM_OBJECT
  Any		name;			/* name of the attribute */
  Any		value;			/* value for the attribute */
End;

NewClass(class_variable)
  ABSTRACT_BEHAVIOUR
  Type		type;			/* Type of this variable */
  Any		value;			/* Value of the variable */
  Any		cv_default;		/* Default value */
  StringObj	summary;		/* Short documentation */
End;

NewClass(binding)
  Name		name;			/* name of the binding */
  Any		value;			/* Value of the binding */
End;

NewClass(error)
  Name		id;			/* Id of the error */
  Name		format;			/* Format of the error message */
  Name		kind;			/* {message,warning,error} */
  Name		feedback;		/* {inform,print} */
End;

NewClass(chain)
  Int		size;			/* # elements in the chain */
  Cell		head;			/* first element */
  Cell		tail;			/* last element */
  Cell		current;		/* current element */
End;

typedef struct instance_proto *InstanceProto;

struct instance_proto
{ int		size;			/* Size of the prototype (bytes) */
  struct object proto;			/* the proto itself */
};

		 /*******************************
		 *	CLASS AND LAZY STUFF	*
		 *******************************/

typedef struct _vardecl
{ Name		name;			/* name of the instance var */
  char         *type;			/* type */
  int		flags;			/* IV_<flag> bitwise or */
  void	       *context;		/* wrapper or function ptr */
  Name		group;			/* group identifier */
  char	       *summary;		/* documentation summary */
} vardecl;

typedef struct _senddecl
{ Name		name;			/* name of the method */
  int		arity;			/* arity thereof */
  void	       *types;			/* type or type-vector */
  SendFunc	function;		/* implementation */
  Name		group;			/* group id */
  char		*summary;		/* documentation summary */
} senddecl;

typedef struct _getdecl
{ Name		name;			/* name of the method */
  int		arity;			/* arity thereof */
  char	       *rtype;			/* return type */
  void	       *types;			/* type or type-vector */
  GetFunc	function;		/* implementation */
  Name		group;			/* group id */
  char	       *summary;		/* documentation summary */
} getdecl;

typedef struct _classvardecl
{ Name		name;			/* Name of the class-variable */
  char	       *type;			/* type description */
  char	       *value;			/* (default) value */
  char	       *summary;		/* documentation summary */
} classvardecl;

typedef struct _classdecl
{ vardecl      *variables;		/* Instance variables */
  senddecl     *send_methods;		/* Send methods of class */
  getdecl      *get_methods;		/* get methods of class */
  classvardecl *class_variables;	/* Variables of the class */
  int		nvar;			/* number of entries in tables */
  int		nsend;
  int		nget;
  int		nclassvars;
  int		term_arity;		/* Arity of term description */
  Name	       *term_names;		/* Array of term-names */
  char	       *source_file;		/* Name of the source-file */
  char	       *rcs_revision;		/* RCS version info */
} classdecl;

#define ClassDecl(name, vs, ss, gs, rs, ta, tn, rcs) \
	static classdecl name = \
	{ vs, ss, gs, rs, \
	  IVEntries(vs), SMEntries(ss), GMEntries(gs), RCEntries(rs), \
	  ta, tn, __FILE__, rcs \
	}

					/* Dont change IV_GET and IV_SEND */
#define IV_NONE		0x00		/* No access, nothing */
#define IV_GET		0x01		/* instance var get-access */
#define IV_SEND		0x02		/* instance var send-access */
#define IV_BOTH		(IV_GET|IV_SEND) /* convenience */
#define IV_SUPER	0x04		/* delegation variable */
#define IV_STORE	0x08		/* has store method */
#define IV_FETCH	0x10		/* has fetch method */
#define IV_REDEFINE	0x20		/* redefine existing variable */

#define RC_REFINE	(char *)(-1)	/* refinement of class-variable */

#define SM(n, a, t, f, g, s)	{ n, a, t, (SendFunc) f, (Name) g, s }
#define GM(n, a, r, t, f, g, s) { n, a, r, t, (GetFunc) f, (Name) g, s }
#define RC(n, t, d, s)		{ n, t, d, s }
#define IV(n, t, f, g, s)	{ n, t, f, NULL,        (Name) g, s }
#define SV(n, t, f, c, g, s)	{ n, t, f, (void *)(c), (Name) g, s }
#define IVEntries(l)		(sizeof(l) / sizeof(vardecl))
#define SMEntries(l)		(sizeof(l) / sizeof(senddecl))
#define GMEntries(l)		(sizeof(l) / sizeof(getdecl))
#define RCEntries(l)		(sizeof(l) / sizeof(classvardecl))
#define TNEntries(l)		(sizeof(l) / sizeof(Name))

#ifndef UXWIN
#ifdef WIN32_GRAPHICS
#define UXWIN(unx, win) win
#else
#define UXWIN(unx, win) unx
#endif
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE

If you  add/delete slots, do  not forget to  change PCE_CLASS_SLOTS in
pce-class.c
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

NewClass(class)
  ABSTRACT_PROGRAM_OBJECT \
  Name		name;			/* (2) name for this class */ \
  StringObj	summary;		/* Summary of the class */ \
  Name		creator;		/* Created from where? */ \
  Class		super_class;		/* (abstract) super-class */ \
  Chain		sub_classes;		/* list of sub-classes */
  Vector	instance_variables;	/* (7) local variables */
  Chain		send_methods;		/* send methods for this class */
  Chain		get_methods;		/* get methods for this class */
  Vector	term_names;		/* get method to obtain arguments */
  Chain		delegate;		/* variables I delegate to */
  Chain		class_variables;	/* Class variables of this class */
  Name		cloneStyle;		/* style of clone method */
  Name		saveStyle;		/* special save method */
  Sheet		features;		/* installed features */
  Int		no_created;		/* how many were created */
  Int		no_freed;		/* how many were freed */
  BoolObj		solid;			/* graphicals: OFF by default */
  Name		selection_style;	/* graphicals: feedback selected */
  Chain		handles;		/* graphicals only: connection pts */
  Int		instance_size;		/* Instance size in bytes */
  Int		slots;			/* # instance variables */
  SourceLocation source;		/* Source location */
  Name		rcs_revision;		/* Current rcs-revision of source */
  Chain		changed_messages;	/* Trap instance changes */
  Chain		created_messages;	/* Trap instance creation */
  Chain		freed_messages;		/* Trap instance destruction */
  BoolObj		un_answer;		/* Decide on slot assignment */

  Code		make_class_message;	/* Message to build the class */

  SendMethod	initialise_method;	/* Initialise instance */
  SendMethod	send_catch_all;		/* Catch failed sends */
  GetMethod	get_catch_all;		/* Catch failed gets */
  GetMethod	convert_method;		/* Convert to this type */
  GetMethod	lookup_method;		/* Reusable object-lookup */

  Code		resolve_method_message;	/* Lazy definition of methods */

  HashTable	send_table;		/* hash-table of send methods */
  HashTable	get_table;		/* hash-table of get methods */
  HashTable	local_table;		/* hash-table of instance variables */
  HashTable	class_variable_table;	/* hash-table of class-variables */
  HashTable	instances;		/* hash-table holding the instances */

  BoolObj		realised;		/* Class has been realised? */
  Name		init_variables;		/* How to initialise slots */

  InstanceProto	proto;			/* Prototype instance */
  intptr_t	tree_index;		/* Index in depth-first tree */
  intptr_t	neighbour_index;	/* Index of my neighbour */

  GetFunc	get_function;		/* `Get' on Code objects */
  SendFunc	send_function;		/* `Send' on Code objects */
  SendFunc	saveFunction;		/* function handling saveFile */
  SendFunc	loadFunction;		/* function handling loadFile */
  SendFunc	cloneFunction;		/* function to clone object */
  SendFunc	redrawFunction;		/* redraw a graphical */
  SendFunc	changedFunction;	/* Trap instance changes */
  SendFunc	in_event_area_function;	/* Test if event is in area */
  SendFunc	make_class_function;	/* makeClass function pointer */
  intptr_t	boot;			/* When booting: #pce-slots; else 0 */

  classdecl    *c_declarations;		/* Non-object declarations */
End;

NewClass(type)
  ABSTRACT_PROGRAM_OBJECT
  Name		kind;			/* Kind of type */
  Name		fullname;		/* Logical name of the type */
  Name		argument_name;		/* Name of the argument */
  Chain		supers;			/* Super-types */
  Any		context;		/* Context argument for functions */
  BoolObj	vector;			/* Method: vector of these */
  intptr_t	validate_function;	/* Function to check the type */
  Func		translate_function;	/* Function to convert the type */
End;

NewClass(constraint)
  Any	        from;			/* 'From' object of constraint */
  Any	        to;			/* 'To' object of constraint */
  Relation	relation;		/* relation they have */
  Name		locked;			/* locked fro further messages? */
End;

NewClass(date)
  union
  { intptr_t	date;			/* Unix view of time */
    Any		slot;			/* dummy */
  } date;
End;

NewClass(dict)
  Any		browser;		/* browser showing contents */
  Chain		members;		/* list of dict_items */
  HashTable	table;			/* hash table for associative lookup */
  Code		sort_by;		/* Sort cirterium */
End;

NewClass(dictitem)
  ABSTRACT_VISUAL
  Any		key;			/* key (often same as label) */
  CharArray	label;			/* label displayed in browser */
  Any		object;			/* associated object (often a sheet) */
  Name		style;			/* Display style */
  Int		index;			/* index number (0 upwards) */
  Dict		dict;			/* dict object in which item resides */
End;

NewClass(divide)
  ABSTRACT_BINARY_EXPRESSION
End;

NewClass(equation)
  ABSTRACT_BINARY_CONDITION
End;

NewClass(binary_expression)
  ABSTRACT_BINARY_EXPRESSION
End;

NewClass(binary_condition)
  ABSTRACT_BINARY_CONDITION
End;

NewClass(handle)
  Expression	xPosition;		/* X position of handle */
  Expression	yPosition;		/* Y position of handle */
  Name		kind;			/* Kind of handle */
  Name		name;			/* Logical nam of connection */
End;

NewClass(modifier)
  Name		shift;			/* {up,down,@default} */
  Name		control;		/* {up,down,@default} */
  Name		meta;			/* {up,down,@default} */
End;


#define ABSTRACT_GESTURE \
  ABSTRACT_RECOGNISER \
  Name		button;			/* {left,middle,right} */ \
  Modifier	modifier;		/* shift-control-meta */ \
  Code		condition;		/* Additional conditions */ \
  Name		status;			/* {inactive, ...} */ \
  Any		cursor;			/* Cursor while acitive */ \
  Name		drag_scroll;		/* Scroll when dragging out */ \
  Timer		drag_scroll_timer;	/* Associated timer */ \
  EventObj	drag_scroll_event;	/* Last event for drag-scroll */

NewClass(gesture)
  ABSTRACT_GESTURE
End;

NewClass(handler)
  ABSTRACT_RECOGNISER
  Name		event;			/* type of event handled by handler */
  Code		message;		/* message associated with handler */
  RegionObj	region;			/* region of the receiver */
End;

NewClass(handlergroup)
  ABSTRACT_RECOGNISER
  Chain		members;		/* Handlers of the group */
End;

#define ABSTRACT_HASH_TABLE \
  Name		refer;			/* Maintain references */ \
  Int		size;			/* # symbols in table */ \
  intptr_t	buckets;		/* # buckets in symbol-array */ \
  Symbol	symbols;		/* Symbol-array */

NewClass(hash_table)
  ABSTRACT_HASH_TABLE
End;

NewClass(chain_table)
  ABSTRACT_HASH_TABLE
End;

NewClass(hyper)
  ABSTRACT_PROGRAM_OBJECT
  Any		from;			/* first linked object */
  Any		to;			/* second linked object */
  Name		forward_name;		/* name of the link from <-from */
  Name		backward_name;		/* name of the link from <-to */
End;

NewClass(identity)
  Name		from;			/* selector of 'from' object */
  Name		to;			/* selector of 'to' object */
End;

NewClass(minus)
  ABSTRACT_BINARY_EXPRESSION
End;

#define ABSTRACT_SOURCE_SINK \
  Name		encoding;		/* used encoding */

#define ABSTRACT_CHAR_ARRAY \
  string	data;			/* the represented data */

NewClass(char_array)
  ABSTRACT_CHAR_ARRAY
End;

NewClass(name)
  ABSTRACT_CHAR_ARRAY
End;

NewClass(string)
  ABSTRACT_CHAR_ARRAY
End;

NewClass(source_sink)
  ABSTRACT_SOURCE_SINK
End;

NewClass(number)
  intptr_t	value;			/* value of the number */
End;

NewClass(pce)
#ifndef O_RUNTIME
  BoolObj		debugging;		/* debugging? (watching spy points) */
  BoolObj		trap_errors;		/* Trap tracer on errors */
#endif
  Name		last_error;		/* Last error occurred */
  Chain		catched_errors;		/* Stack of catched error-id's */
  BoolObj		catch_error_signals;	/* Catch Unix signals */

  Chain		exit_messages;		/* Called on exit */
  Sheet		exception_handlers;	/* exception-name --> code */

  Name		home;			/* Home directory */
  SourceSink	defaults;		/* Location to load defaults from */
  Directory	application_data;	/* User application data */

  Name		version;		/* Version number of PCE */
  Name		machine;		/* Architecture */
  Name		operating_system;	/* Name of operating system*/
  Name		window_system;		/* X or windows */
  Int		window_system_version;	/* Version of Xt library used */
  Int		window_system_revision;	/* Revision of Xt library used */
  Chain		features;		/* Installed features */
End;

NewClass(plus)
  ABSTRACT_BINARY_EXPRESSION
End;

NewClass(point)
  Int		x;			/* the x- and y-coordinates */
  Int		y;
End;

#define ABSTRACT_HOST \
  Name		language;		/* Prolog, Lisp, ... */ \
  Name		system;			/* host system we are connected to */ \
  BoolObj		callBack;		/* if @on can be called directly */ \
  Chain		messages;		/* messages waiting in queue */


NewClass(host)
  ABSTRACT_HOST
End;

NewClass(host_data)
  void *	handle;			/* the host handle */
End;

NewClass(real)
#if SIZEOF_VOIDP == SIZEOF_DOUBLE
#define REAL_IN_ONE 1
  double	value;			/* can store in one slot */
#else
  uintptr_t value1;			/* 1-st part of double */
  uintptr_t value2;			/* 2nd-part of double */
#endif
End;

NewClass(recogniser)
  ABSTRACT_RECOGNISER
End;

NewClass(region)
  Expression	x;			/* describe x of region */
  Expression	y;			/* describe y of region */
  Expression	w;			/* describe w of region */
  Expression	h;			/* describe h of region */
End;

NewClass(relation)			/* empty abstract super class */
End;

NewClass(size)
  Int		w;			/* width and height */
  Int		h;
End;

#define ABSTRACT_SHEET \
  Chain		attributes;		/* list of attributes */

NewClass(sheet)
  ABSTRACT_SHEET
End;

NewClass(source_location)
  Name		file_name;		/* Name of the file */
  Int		line_no;		/* Line of the source location */
End;

NewClass(spatial)
  Equation	xFrom;			/* X reference point of from */
  Equation	yFrom;			/* Y reference point of from */
  Equation	xTo;			/* X reference point of to */
  Equation	yTo;			/* Y reference point of to */
  Equation	wTo;			/* W of to */
  Equation	hTo;			/* H of to */
End;

NewClass(times)
  ABSTRACT_BINARY_EXPRESSION
End;

#define ABSTRACT_VECTOR \
  Int		offset;			/* index of element 0 of array */ \
  Int		size;			/* number of valid entries */ \
  Int		allocated;		/* # allocated cells */ \
  Any		*elements;		/* array of elements */

NewClass(vector)
  ABSTRACT_VECTOR
End;

NewClass(visual)
  ABSTRACT_VISUAL
End;


struct cell
{ Cell		next;			/* pointer to next cell */
  Any		value;			/* value pointer */
};


struct symbol
{ Any		name;			/* name entry of symbol */
  Any		value;			/* associated value with name */
};

#define ABSTRACT_CONSTANT \
  Name		name;			/* Name of the constant */ \
  StringObj	summary;		/* Summary description */

NewClass(constant)			/* @nil, @default */
  ABSTRACT_CONSTANT
End;

NewClass(bool)				/* @on, @off */
  ABSTRACT_CONSTANT
End;

NewClass(code)
  ABSTRACT_CODE
End;

NewClass(function)
  ABSTRACT_FUNCTION
End;

NewClass(quote_function)
  Function	function;		/* the function quoted */
End;

#define ABSTRACT_AND \
  ABSTRACT_CODE \
  Chain		members;		/* members of the and */

NewClass(and)
  ABSTRACT_AND
End;

NewClass(assignment)
  ABSTRACT_CODE
  Var		var;			/* Variable to bind */
  Any		value;			/* Value (or function) */
  Name		scope;			/* Local or global binding */
End;

NewClass(var)
  ABSTRACT_FUNCTION
  Name		name;			/* Name of the variable */
  Type		type;			/* Type of the variable */
  Any		value;			/* Current value of the variable */
  Any		global_value;		/* Initial or global value */
End;

NewClass(obtain)
  ABSTRACT_FUNCTION
  Any		receiver;		/* receiver of the message */
  Name		selector;		/* selector of the message */
  Vector	arguments;		/* argument vector of the message */
  Any		context;		/* Host context */
End;

NewClass(create_obj)
  ABSTRACT_FUNCTION
  Class		c_class;		/* Class to create instance from */
  Vector	arguments;		/* Initialisation arguments */
End;

NewClass(message)
  ABSTRACT_CODE
  Any		receiver;		/* receiver of the message */
  Name		selector;		/* selector of the message */
  Int		arg_count;		/* number of arguments */
  Vector	arguments;		/* argument vector of the message */
  Any		context;		/* Host context */
End;

NewClass(block)
  ABSTRACT_AND
  Vector	parameters;		/* formal-parameter-list */
End;

NewClass(if_obj)
  ABSTRACT_CODE
  Code		condition;		/* codition of the `if' */
  Code		then_branch;		/* if condition succeeds */
  Code		else_branch;		/* if condition fails */
End;

NewClass(while_obj)
  ABSTRACT_CODE
  Code		condition;		/* condition of the `while' */
  Code		body;			/* body of the `while' */
End;

NewClass(equal)				/* == */
  ABSTRACT_CODE
  Any		left;
  Any		right;
End;

NewClass(non_equal)			/* \== */
  ABSTRACT_CODE
  Any		left;
  Any		right;
End;

NewClass(or)
  ABSTRACT_CODE
  Chain		members;		/* members of the or */
End;

NewClass(not)
  ABSTRACT_CODE
       Code		argument;	/* Its argument */
End;

NewClass(progn)
  ABSTRACT_FUNCTION
  Chain			members;	/* statements */
End;

NewClass(when)
  ABSTRACT_FUNCTION
  Code		condition;		/* codition of the `when' */
  Function	then_branch;		/* value if condition succeeds */
  Function	else_branch;		/* value if condition fails */
End;

		 /*******************************
		 *	     CLASSES		*
		 *******************************/

struct class_definition
{ Name		name;			/* name of the class */
  Name		super;			/* Name of the super-class */
  SendFunc	makefunction;		/* Built the class */
  Class *	global;			/* Pointer to global class var */
  char *	summary;		/* Summary description */
};

		 /*******************************
		 *	       NAMES		*
		 *******************************/

#ifndef NO_BUILT_IN_DECL
extern struct name builtin_names[];	/* object-array of built-in's */
#endif
#include "names.ih"			/* #defines for code used names */

#define isName(name)	(isObject(name) && onFlag((name), F_ISNAME))
#define notName(name)	(!isName(name))
#define equalName(a, b) ((a) == (b))
#define strName(s)	((char *)((Name)(s))->data.s_textA)

#define getAppendName(n, s) \
	((Name) getAppendCharArray((CharArray)(n), (CharArray)(s)))


		/********************************
		*         FORWARDING		*
		********************************/

#define Arg(i)			(ARG[((i)-1)])
#define	setVar(v, val)		((v)->value = val)

typedef struct
{ Var	variable;
  Any	value;
} var_binding, *VarBinding;

#define BINDINGBLOCKSIZE 8

typedef struct var_environment * VarEnvironment;
typedef struct var_extension * VarExtension;

GLOBAL VarEnvironment varEnvironment;

struct var_environment
{ VarEnvironment parent;
  int		 size;
  var_binding	 bindings[BINDINGBLOCKSIZE];
  VarExtension   extension;
};


struct var_extension
{ int		 allocated;
  var_binding	 bindings[BINDINGBLOCKSIZE];
};


#define withLocalVars(code) \
  { struct var_environment _var_env; \
 \
    _var_env.size = 0; \
    _var_env.parent = varEnvironment; \
    _var_env.extension = NULL; \
    varEnvironment = &_var_env; \
 \
    code; \
 \
    popVarEnvironment(); \
  }


#define withArgs(ac, av, code) \
  { struct var_environment _var_env; \
    int _i; \
 \
    _var_env.parent = varEnvironment; \
    _var_env.extension = NULL; \
    varEnvironment = &_var_env; \
 \
    if ( ac <= BINDINGBLOCKSIZE ) \
    { Var *_v = &ARG[0]; \
      VarBinding _b = &_var_env.bindings[0]; \
      const Any *_val = (av); \
      for( _i=ac; --_i >= 0; _b++, _v++, _val++) \
      { _b->variable = *_v; \
	_b->value = _b->variable->value; \
	_b->variable->value = *_val; \
	if ( isObject(*_val) ) \
	  addCodeReference(*_val); \
      } \
      _var_env.size = (ac); \
    } else \
    { _var_env.size = 0; \
      for(_i=0; _i<ac; _i++) \
        assignVar(Arg(_i+1), (av)[_i], DEFAULT); \
    } \
 \
    code; \
 \
    popVarEnvironment(); \
  }

#define withReceiver(r, c, code) \
  { Any _rs = RECEIVER->value; \
    Any _rc = RECEIVER_CLASS->value; \
    RECEIVER->value = (r); \
    RECEIVER_CLASS->value = (c); \
    code; \
    RECEIVER_CLASS->value = _rc; \
    RECEIVER->value = _rs; \
  }


		/********************************
		*        INCREMENTAL GC		*
		********************************/

typedef struct to_cell *ToCell;		/* TemporaryObjectCell */

struct to_cell
{ ToCell	next;			/* Next of the stack */
  Any		value;			/* Object there */
  long		index;			/* Index of the mark */
};

GLOBAL ToCell	AnswerStack;		/* Stack of `answer objects' */
GLOBAL int	deferredUnalloced;	/* # deferred unallocs in ->free */

typedef intptr_t AnswerMark;

#define markAnswerStack(mark)	{(mark) = AnswerStack->index;}
#define rewindAnswerStack(mark, obj) \
	{ if ( (mark) != AnswerStack->index ) \
	    _rewindAnswerStack(&(mark), obj); }


		 /*******************************
		 *	 GLOBAL FUNCTIONS	*
		 *******************************/

#include "../ker/proto.h"
#include "../msg/proto.h"
#include "../adt/proto.h"
#include "../rel/proto.h"

#define getSubName(n, f, t) (Name)getSubCharArray((CharArray)(n), f, t)

					/* Interface callback stubs */
__pce_export void	Cprintf(const char *fmt, ...);
__pce_export void	Cvprintf(const char *fmt, va_list args);
__pce_export int	Cputchar(int chr);
__pce_export char *	Cgetline(char *line, int size);

					/* interface prototypes */
__pce_export Any	cToPceName(const char *text);
COMMON(CPointer) CtoCPointer(void *);
COMMON(status)	makeClassC(Class class);
COMMON(status)	makeClassRC(Class class);
COMMON(status)	makeClassCPointer(Class class);
COMMON(status)	initialiseHost(Host h, Name which);
COMMON(status)	makeClassHost(Class class);
COMMON(status)	makeClassHostData(Class class);
COMMON(status)	makeClassSourceSink(Class class);
COMMON(Host)	HostObject(void);
COMMON(int)	hostGetc(void);
COMMON(void)	pceWriteErrorGoal(void);
COMMON(int)	initPublicInterface(void);

COMMON(status)	initialiseSourceSink(SourceSink ss);
COMMON(status)	checkErrorSourceSink(SourceSink ss, IOSTREAM *fd);
COMMON(status)	initialiseSourceSink(SourceSink ss);
COMMON(IOSTREAM *)	Sopen_object(Any obj, const char *mode);
COMMON(status)	setStreamEncodingSourceSink(SourceSink ss, IOSTREAM *fd);
COMMON(Name)	encoding_to_name(IOENC encoding);

#if O_CPLUSPLUS
COMMON(status )	callCPlusPlusProc(void *f, int ac, const Any av[]);
COMMON(Any)	callCPlusPlusFunc(void *f, int ac, const Any av[]);
COMMON(status )	callCPlusPlusPceMethodProc(Any o, void *f,
					   int ac, const Any av[]);
COMMON(Any )	callCPlusPlusPceMethodFunc(Any o, void *f,
					   int ac, const Any av[]);
COMMON(status )	callCPlusPlusMethodProc(Any o, void *f,
					int ac, const Any av[]);
COMMON(Any )	callCPlusPlusMethodFunc(Any o, void *f,
					int ac, const Any av[]);
#endif
COMMON(void)	initCGlobals(void);

		/********************************
		*       GLOBAL VARIABLES	*
		********************************/

GLOBAL int	XPCE_initialised;	/* Is system initialised? */
GLOBAL Pce	PCE;			/* the one and only Pce object */
GLOBAL Host	HOST;			/* the one and only Host object */
GLOBAL SendFunc	DispatchEvents;		/* Dispatch function */
GLOBAL int	changedLevel;		/* Change forwarding levels */
GLOBAL HashTable ErrorTable;		/* @error_database */
GLOBAL int	XPCE_mt;		/* we are multi-threaded */

GLOBAL struct constant ConstantNil;	/* MUST be first! */
GLOBAL struct constant ConstantDefault;
GLOBAL struct constant ConstantClassDefault;
GLOBAL struct bool     BoolOn;
GLOBAL struct bool     BoolOff;

GLOBAL Var	RECEIVER;		/* @receiver */
GLOBAL Var	RECEIVER_CLASS;		/* @receiver_class */
GLOBAL Var	EVENT;			/* @event */
GLOBAL Var	SELECTOR;		/* @selector */
GLOBAL Var	REPORTEE;		/* @reportee */
GLOBAL Var	ARG[FWD_PCE_MAX_ARGS];  /* @arg1 ... */
GLOBAL Var	VarX;			/* x */
GLOBAL Var	VarY;			/* y */
GLOBAL Var	VarW;			/* w */
GLOBAL Var	VarH;			/* h */
GLOBAL Var	VarW2;			/* w2 */
GLOBAL Var	VarH2;			/* h2 */
GLOBAL Var	VarXref;		/* xref */
GLOBAL Var	VarYref;		/* yref */


GLOBAL HashTable classTable;		/* @classes (name --> class) */
GLOBAL HashTable TypeTable;		/* @types (name --> type) */

#define CTE_OK			0	/* CheckType success */
#define CTE_OBTAINER_FAILED	1	/* Obtainer failed */

GLOBAL int	CheckTypeError;		/* Why did checkType fail? */
GLOBAL int	restoreVersion;		/* Version of save file */
GLOBAL SourceSink LoadFile;		/* Current file for <-object */
GLOBAL char    *SaveMagic;		/* Magic string for saved objects */
GLOBAL int	inBoot;			/* is the system in the boot cycle? */
GLOBAL uintptr_t allocBase;		/* lowest allocated memory */
GLOBAL uintptr_t allocTop;		/* highest allocated memory */
#ifndef O_RUNTIME
GLOBAL int	PCEdebugging;		/* PCE->debugging == ON */
GLOBAL int	PCEdebugBoot;		/* Debug booting phase? */
GLOBAL Chain	PCEdebugSubjects;	/* Names of things we are debugging */
GLOBAL char    *symbolFile;		/* current symbol file */
#else
#define PCEdebugging FALSE
#endif /*O_RUNTIME*/
GLOBAL int	PCEargc;		/* main() argument count */
GLOBAL char   **PCEargv;		/* main() argument vector */
GLOBAL char    *(*getFunctionNameFromAddress)();
					/* stack trace (pce-debug.c) */

GLOBAL HashTable ObjectConstraintTable;	/* object-level constraints */
GLOBAL HashTable ObjectAttributeTable;	/* object-level attributes */
GLOBAL HashTable ObjectSendMethodTable;	/* object-level send_methods */
GLOBAL HashTable ObjectGetMethodTable;	/* object-level get_methods */
GLOBAL HashTable ObjectRecogniserTable;	/* object-level recognisers */
GLOBAL HashTable ObjectHyperTable;	/* object-level hypers */

GLOBAL Name	name_procent_s;		/* "%s" */
GLOBAL Name	name_cxx;		/* "C++" */
GLOBAL Name	name_nil;		/* "[]" */
GLOBAL Name	name_space;		/* " " */
GLOBAL Code	qsortCompareCode;	/* used by qsortCompareObjects() */
GLOBAL int	qsortReverse;		/* used by qsortCompareObjects() */

		 /*******************************
		 *	    GLOBAL TYPES	*
		 *******************************/

extern char *T_report[];		/* ->report: kind, format, args... */

		/********************************
		*        SET ITERATION		*
		********************************/

#define copyArgs(n, f, t) \
  { int _i; for(_i=0; _i < (n); _i++) (t)[_i] = (f)[_i]; }

#define for_chain(ch, val, code) \
  { intptr_t _i=0, _size  = valInt(ch->size); \
    Any *_array = (Any *)alloca((size_t)_size * sizeof(Any)); \
    Cell _cell = ch->head; \
	\
    for( ; notNil(_cell); _cell = _cell->next, _i++ ) \
    { _array[_i] = _cell->value; \
      if ( isObject(_array[_i]) ) addCodeReference(_array[_i]); \
    } \
    for(_i = 0; _i < _size; _i++) \
    { (val) = _array[_i]; \
      if ( nonObject(val) || !isFreedObj(val) ) \
      { code; \
      } \
      if ( isObject(val) ) delCodeReference(val); \
    } \
  }

#define for_vector(v, val, code) \
  { intptr_t _iv, _sizev = valInt((v)->size); \
    for(_iv = 0; _iv < _sizev; _iv++) \
    { val = (v)->elements[_iv]; \
      code; \
    } \
  }

#define for_vector_i(v, val, i, code) \
  { intptr_t _iv, _sizev = valInt((v)->size); \
    intptr_t _offv = valInt((v)->offset)+1; \
    for(_iv = 0; _iv < _sizev; _iv++) \
    { intptr_t i = _iv + _offv; \
      val = (v)->elements[_iv]; \
      code; \
    } \
  }


#define for_hash_table(ht, var, code) \
  { intptr_t _iht, _sizeht = (ht)->buckets; \
    for(_iht = 0; _iht < _sizeht; _iht++) \
    { Symbol var = &(ht)->symbols[_iht]; \
      if ( var->name != NULL ) \
      { code; \
      } \
    } \
  }


#define for_cell(c, ch)	for(c=(ch)->head; notNil(c); c=c->next)
#define for_cell_save(p, q, ch)	if (notNil(p=(ch)->head))\
		for(q=p->next; notNil(p); p=q, q=(isNil(q) ? q : q->next))

		/********************************
		*          EXPRESSIONS		*
		********************************/

#define LEFTHAND(e)	(((BinaryExpression)e)->left)
#define RIGHTHAND(e)	(((BinaryExpression)e)->right)


		/********************************
		*             AREAS		*
		********************************/

/* An area has an orientation defined as the point where the origin
 * of the area is:
 *
 *   northWest	    northEast
 *	-----------------
 *      |		|
 *      |		|
 *	-----------------
 *   southWest	    southEast
 */

#define OrientationArea(w, h)	(w>=0 ? (h>=0 ? NAME_northWest		\
					       : NAME_southWest)	\
				       : (h>=0 ? NAME_northEast		\
					       : NAME_southEast))


#define OrientateArea(x, y, w, h, d) \
  { if ( equalName(d, NAME_northWest) ) \
    { if (w < 0) x += w+1, w = -w; \
      if (h < 0) y += h+1, h = -h; \
    } else if ( equalName(d, NAME_southWest) ) \
    { if (w < 0) x += w+1, w = -w; \
      if (h > 0) y += h-1, h = -h; \
    } else if ( equalName(d, NAME_northEast) ) \
    { if (w > 0) x += w-1, w = -w; \
      if (h < 0) y += h+1, h = -h; \
    } else if ( equalName(d, NAME_southEast) ) \
    { if (w > 0) x += w-1, w = -w; \
      if (h > 0) y += h-1, h = -h; \
    } \
  }


/* Normalise the area given by the C integers x, y, w, h
 * such that w and h are always positive.
 */
#define NormaliseArea(x,y,w,h)	{ if (w < 0) x += w+1, w = -w; \
				  if (h < 0) y += h+1, h = -h; \
				}

#ifndef O_RUNTIME
#define DEBUGGING(subject)	( PCEdebugging && pceDebugging(subject) )
#define DEBUG(subject, goal)	{ if ( DEBUGGING(subject) ) \
				  { goal; \
				  } \
				}

#define DEBUG_BOOT(goal)	{ if ( PCEdebugBoot ) \
				  { goal; \
				  } \
				}
#else /*O_RUNTIME*/
#define DEBUG(subject, goal)
#define DEBUG_BOOT(goal)
#define tracePce(pce, how)
#endif

#ifndef O_RUNTIME
#define O_COUNT 0			/* we've had that */
#else
#define O_COUNT 0
#endif

		/********************************
		*             SYNTAX		*
		********************************/

#include <h/syntax.h>
#include <h/utf8.h>

		 /*******************************
		 *	 SPEEDUP MACROS		*
		 *******************************/

#define sendv(rec, sel, ac, av) vm_send((rec), (sel), NULL, (ac), (av))
#define getv(rec, sel, ac, av) vm_get((rec), (sel), NULL, (ac), (av))
#define FixSendFunctionClass(cl, m) if ( !(cl)->send_function ) \
				      fixSendFunctionClass((cl), (m))
#define FixGetFunctionClass(cl, m) if ( !(cl)->get_function ) \
				      fixGetFunctionClass((cl), (m))

		 /*******************************
		 *	  HOST INTERFACE	*
		 *******************************/

#include <h/interface.h>

		/********************************
		*        INLINE SUPPORT		*
		********************************/

#if O_COUNT
#define COUNT(g) {g;}

GLOBAL int hash_cmp_failed;		/* failed comparisons for lookup */
GLOBAL int hash_lookups;		/* Total lookups */
GLOBAL int hash_resizes;		/* # resizes done */
GLOBAL int hash_shifts;			/* Shifts in append */
#else
#define COUNT(g)
#endif

#define unboundedKey(name) (isInteger(name) ? (uintptr_t)(name)>>1 \
					    : (uintptr_t)(name)>>2)

#if USE_PRIMES
#define hashKey(name, buckets) (unboundedKey(name) % (buckets))
#else
#define hashKey(name, buckets) (unboundedKey(name) & ((buckets)-1))
#endif


#include "../ker/inline.c"
