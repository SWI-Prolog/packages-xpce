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
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

#include <h/kernel.h>

static char *ppsavestring(const char *s);

status
toString(Any obj, String s)
{ char tmp[25];
  char *str;
  status rval = FAIL;

  if ( instanceOfObject(obj, ClassCharArray) )
  { CharArray ca = obj;

    str_cphdr(s, &ca->data);
    s->s_text = ca->data.s_text;
    succeed;
  } else if ( isInteger(obj) )
  { sprintf(tmp, INTPTR_FORMAT, valInt(obj));
    str = ppsavestring(tmp);
    rval = SUCCEED;
  } else if ( instanceOfObject(obj, ClassReal) )
  { sprintf(tmp, "%g", valReal(obj));
    str = ppsavestring(tmp);
    rval = SUCCEED;
  } else if ( instanceOfObject(obj, ClassNumber) )
  { sprintf(tmp, INTPTR_FORMAT, ((Number)obj)->value);
    str = ppsavestring(tmp);
    rval = SUCCEED;
  }

  if ( rval )
  { str_set_ascii(s, str);
  }

  return rval;
}


char *
toCharp(Any obj)
{ string s;

  if ( toString(obj, &s) )
    return (char *)s.s_text;

  return NULL;
}


Int
toInteger(Any obj)
{ if ( isInteger(obj) )					/* int */
  { return (Int) obj;
  } else if ( instanceOfObject(obj, ClassNumber) )	/* number */
  { return toInt(((Number)obj)->value);
  } else if ( instanceOfObject(obj, ClassReal) )	/* real */
  { return toInt(rfloat(valReal(obj)));
  } else if ( instanceOfObject(obj, ClassCharArray) )	/* char_array */
  { CharArray ca = obj;
    String s = &ca->data;

    if ( isstrA(s) && s->size > 0 )
    { char *end;
      long i;

      i = strtol((char *)s->s_textA, &end, 10);
      if ( end == (char *)&s->s_textA[s->size] )
	return toInt(i);
    }
  }

  fail;
}


Real
toReal(Any obj)
{ if ( instanceOfObject(obj, ClassReal) )
    return obj;

  return getConvertReal(ClassReal, obj);
}


BoolObj
toBool(Any obj)
{ Int i;
  string s;

  if ( isBoolean(obj) )
    return obj;

  if ( (i = checkType(obj, TypeInt, NIL)) )
  { if ( i == ZERO )
      return OFF;
    else if ( i == ONE )
      return ON;
  }

  if ( toString(obj, &s) && isstrA(&s) )
  { if       ( streq_ignore_case((char *)s.s_textA, "@on") ||
	       streq_ignore_case((char *)s.s_textA, "true") ||
	       streq_ignore_case((char *)s.s_textA, "yes") ||
	       str_icase_eq(&s, &ON->name->data) )
      return ON;
    else if ( streq_ignore_case((char *)s.s_textA, "@off") ||
	      streq_ignore_case((char *)s.s_textA, "false") ||
	      streq_ignore_case((char *)s.s_textA, "no") ||
	      str_icase_eq(&s, &OFF->name->data) )
      return OFF;
  }

  fail;
}


Name
toName(Any obj)
{ string s;

  if (isName(obj))
    return obj;
  if ( toString(obj, &s) )
    return StringToName(&s);
  fail;
}


Type
toType(Any obj)
{ Name name;

  if ( instanceOfObject(obj, ClassType) )
    return obj;
  if ( (name = toName(obj)) )
    return nameToType(name);

  fail;
}


		/********************************
		*               PP		*
		********************************/

#define PPRINGSIZE 16

static char *ppring[PPRINGSIZE];
static int   ppindex = 0;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Donot use (un)alloc() to faciliate debugging of the allocation routines.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static char *
ppsavestring(const char *s)
{ char *q = pceMalloc(strlen(s)+1);

  strcpy(q, s);

  if ( ppring[ppindex] )
    pceFree(ppring[ppindex]);
  ppring[ppindex] = q;

  ppindex = (ppindex+1) % PPRINGSIZE;

  return q;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Safe version of strName(), making sure the datastructures are fairly ok.
Used for debugging purposes only. Returns the string as UTF-8.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static char *
safeStringName(Name n)
{ if ( isProperObject(n) && instanceOfObject(n, ClassName) )
    return nameToUTF8(n);
  else
  { char buf[100];

    sprintf(buf, "0x%lx", (unsigned long)n);
    return ppsavestring(buf);
  }
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
do_pp() creates a UTF-8 string  to  describe   an  object  on  behalf of
debugging. The function is careful to check the consistency before doing
most  of  its  work  to   avoid    crashes   on   partial  or  corrupted
datastructures.

The returned string either points to static data   or is part of a ring,
which implies upto PPRINGSIZE values can be requested before overwrite.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static char *
do_pp(Any obj)
{ char tmp[LINESIZE];
  char summary[256];
  char *s;

  if ( !obj )
    return ppsavestring("FAIL");

  if ( isInteger(obj) )
  { sprintf(tmp, INTPTR_FORMAT, valInt(obj));
    return ppsavestring(tmp);
  }

  if ( isProperObject(obj) )
  { if ( isName(obj))
      return safeStringName((Name) obj);

    if ( instanceOfObject(obj, ClassCharArray) &&
	 isAddress(((CharArray)obj)->data.s_text) )
    { CharArray ca = obj;

      summary[0] = '"';
      if ( ca->data.size < 25 )
      { strcpy(&summary[1], charArrayToUTF8(ca));
      } else
      { strncpy(&summary[1], charArrayToUTF8(ca), 25);
	summary[26] = '\0';
	strcat(summary, " ...");
      }

      strcat(summary, "\"");
      s = summary;
    } else if ( instanceOfObject(obj, ClassType) &&
		isName(((Type)obj)->fullname) )
    { s = nameToUTF8(((Type)obj)->fullname);
    } else if ( instanceOfObject(obj, ClassReal) )
    { sprintf(summary, "%g", valReal(obj));
      s = summary;
    } else if ( instanceOfObject(obj, ClassNumber) )
    { sprintf(summary, INTPTR_FORMAT, ((Number)obj)->value);
      s = summary;
    } else if ( instanceOfObject(obj, ClassHostData) )
    { Any pn = qadGetv(obj, NAME_printName, 0, NULL);

      if ( pn && instanceOfObject(pn, ClassCharArray) )
	return ppsavestring(charArrayToUTF8(pn));
      else
	s = nameToUTF8(classOfObject(obj)->name);
    } else
      s = nameToUTF8(classOfObject(obj)->name);

    { Name name;

      if ( (name = getNameAssoc(obj)) )
        sprintf(tmp, "@%s/%s", nameToUTF8(name), s);
      else
	sprintf(tmp, "@" INTPTR_FORMAT "/%s", valInt(PointerToInt(obj)), s);
    }

    if ( isFreedObj(obj) )
      strcat(tmp, " (freed)");
    else if ( isFreeingObj(obj) )
      strcat(tmp, " (unlinking)");

    return ppsavestring(tmp);
  }

  sprintf(tmp, "0x%lx", (unsigned long) obj);
  return ppsavestring(tmp);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The biggest mess of all.  Basically,  the   above  may  yield error when
passed wrong data. The code below catches   these  errors and prints the
value hexadecimal in case of error.

We have structure exception-handling of   Windows, Unix with traditional
signal(), Unix with sigaction (preserves more  context) and systems with
and without SIGBUS ...
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifdef __WINDOWS__

#include <excpt.h>

char *
pcePP(Any obj)
{ char *s;

#ifndef O_RUNTIME
  int old = PCEdebugging;

  PCEdebugging = FALSE;
#endif

  __try
  { s = do_pp(obj);
  } __except(EXCEPTION_EXECUTE_HANDLER)
  { char tmp[100];
    sprintf(tmp, "0x%lx", (unsigned long)obj);
    s = ppsavestring(tmp);
  }

#ifndef O_RUNTIME
  PCEdebugging = old;
#endif

  return s;
}


#else /*__WINDOWS__*/

#include <setjmp.h>
#include <signal.h>

jmp_buf pp_env;

static RETSIGTYPE
pp_sig(sig)
int sig;
{ longjmp(pp_env, 1);
}

typedef RETSIGTYPE (*handler_t)();

#ifdef HAVE_SIGACTION

typedef struct sigaction sigsave_t;

static void
set_sighandler(int sig, handler_t func, sigsave_t *old)
{ struct sigaction new;

  memset(&new, 0, sizeof(new));	/* deal with other fields */
  new.sa_handler = func;

  sigaction(sig, &new, (struct sigaction *)old);
/*Cprintf("pcePP: handler = %p, flags = 0x%x\n",
	  old->sa_handler, old->sa_flags);
*/
}

static void
restore_handler(int sig, struct sigaction *old)
{ sigaction(sig, old, NULL);
}

#else /*HAVE_SIGACTION*/

typedef handler_t sigsave_t;

static void
set_sighandler(int sig, handler_t func, handler_t *old)
{ old = signal(sig, new);
}

static void
restore_handler(int sig, handler_t *old)
{ signal(sig, *old);
}

#endif /*HAVE_SIGACTION*/

char *
pcePP(Any obj)
{ char *s;
#ifndef O_RUNTIME
  int old = PCEdebugging;
#endif
  sigsave_t oldsegv;
#ifdef SIGBUS
  sigsave_t oldbus;
#endif

  set_sighandler(SIGSEGV, pp_sig, &oldsegv);
#ifdef SIGBUS
  set_sighandler(SIGBUS, pp_sig, &oldbus);
#endif

#ifndef O_RUNTIME
  PCEdebugging = FALSE;
#endif
  if ( setjmp(pp_env) == 0 )
  { s = do_pp(obj);
  } else
  { char tmp[100];
    sprintf(tmp, "0x%lx", (unsigned long)obj);
    s = ppsavestring(tmp);
  }
#ifndef O_RUNTIME
  PCEdebugging = old;
#endif

  restore_handler(SIGSEGV, &oldsegv);
#ifdef SIGBUS
  restore_handler(SIGBUS, &oldbus);
#endif

  return s;
}

#endif /*__WINDOWS__*/

		/********************************
		*           FUNCTIONS		*
		********************************/

Any
expandFunction(Any obj)
{ while ( isFunction(obj) )
  { Function f = (Function) obj;
    Any rval = getExecuteFunction(f);

    if ( rval == FAIL )
    { DEBUG(NAME_obtain, Cprintf("Function: %s\n", pp(f)));
      fail;
    }

    obj = rval;
  }

  return obj;
}
