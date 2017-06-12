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
#include <h/trace.h>
#include "alloc.h"
#include <h/graphics.h>
#include <h/unix.h>
#include <math.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

/* (AA)	isqrt(a).  Returns the square root of a as an integer.  The
	algorithm only uses bit-shifts (multiplication by a power of 2) and
	3.5 integer multiplications on average.
 */

long
rdouble(double f)
{ if (f > 0.0)
    return (long) (f+0.4999999);

  return (long) (f-0.4999999);
}

int
isqrt(long a)
{ if ( a < 0 )
    return errorPce(NAME_sqrt, NAME_domainError, toInt(a));

#ifdef BITSHIFT_SQRT
  register long n, d, m1, m2;

  for (m2=5; ((a<<1) & (0xffff << (m2<<1))); m2++);
  m1 = m2 - 1;
  n = a >> m1;
  for (;;)
  { d = a - n*n;
    if (d >= 0)
    { if (d <= (n<<1))
	return (d <= n? n : n+1);
    } else
    { if (-d <= (n<<1))
	return (-d<=n ? n : n-1);
    }
    n += (1 + (d>>m2));
  }
#else
  return rdouble(sqrt((double)a));
#endif
}


int
distance(int x1, int y1, int x2, int y2)
{ double dx = (double)(x1-x2), dy = (double)(y1-y2);

  return rfloat(sqrt(dx*dx + dy*dy));
}


int
rfloat(double f)
{ if (f > 0.0)
    return (int) (f+0.4999999);

  return (int) (f-0.4999999);
}

		 /*******************************
		 *	STRING <-> NUMBER	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
cstrtod() is like strtod(), but not locale dependent.  It is used for
translating things such as resource values.  Although one can do

	const char *old = setlocale(LC_NUMERIC, "C");
	strtod(...)
	setlocale(LC_NUMERIC, old);

this is not acceptable to  us  as   it  is  not  thread-safe. XPCE isn't
multi-threading anyway, but it is  designed   to  operate  in a threaded
environment.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define cisdigit(c)	((c) >= '0' && (c) <= '9')
#define digitval(c)	((c)-'0')

double
cstrtod(const char *in, char **end)
{ double rval;
  int sign = 1;

  if ( (*in == '-' || *in == '+') && cisdigit(in[1]) )
  { if ( *in == '-' )
      sign = -1;
    in++;
  }

  if ( cisdigit(*in) )
  { rval = digitval(*in);
    in++;

    while(cisdigit(*in))
    { rval = rval*10.0 + digitval(*in);
      in++;
    }
  } else if ( *in == '.' )
  { rval = 0.0;
  } else
  { *end = (char *)in;
    return 0.0;
  }

  if ( *in == '.' && cisdigit(in[1]) )
  { double n = 10.0;
    in++;

    while(cisdigit(*in))
    { rval += digitval(*in)/n;
      n *= 10.0;
      in++;
    }
  }

  if ( *in == 'e' || *in == 'E' )
  { int esign;
    long exp;
    const char *eend = in;

    in++;
    if ( *in == '-' )
    { esign = -1;
      in++;
    } else if ( *in == '+' )
    { esign = 1;
      in++;
    } else
      esign = 1;

    if ( cisdigit(*in) )
    { exp = digitval(*in);
      in++;
    } else
    { *end = (char *)eend;
      return rval*sign;
    }
    while(cisdigit(*in))
    { exp = exp*10+digitval(*in);
      in++;
    }
    rval *= pow(10.0, (double)(esign*exp));
  }

  *end = (char *)in;
  return rval*sign;
}


double
cwcstod(const wchar_t *in, wchar_t **end)
{ double rval;
  int sign = 1;

  if ( (*in == '-' || *in == '+') && cisdigit(in[1]) )
  { if ( *in == '-' )
      sign = -1;
    in++;
  }

  if ( cisdigit(*in) )
  { rval = digitval(*in);
    in++;

    while(cisdigit(*in))
    { rval = rval*10.0 + digitval(*in);
      in++;
    }
  } else if ( *in == '.' )
  { rval = 0.0;
  } else
  { *end = (wchar_t *)in;
    return 0.0;
  }

  if ( *in == '.' && cisdigit(in[1]) )
  { double n = 10.0;
    in++;

    while(cisdigit(*in))
    { rval += digitval(*in)/n;
      n *= 10.0;
      in++;
    }
  }

  if ( *in == 'e' || *in == 'E' )
  { int esign;
    long exp;
    const wchar_t *eend = in;

    in++;
    if ( *in == '-' )
    { esign = -1;
      in++;
    } else if ( *in == '+' )
    { esign = 1;
      in++;
    } else
      esign = 1;

    if ( cisdigit(*in) )
    { exp = digitval(*in);
      in++;
    } else
    { *end = (wchar_t *)eend;
      return rval*sign;
    }
    while(cisdigit(*in))
    { exp = exp*10+digitval(*in);
      in++;
    }
    rval *= pow(10.0, (double)(esign*exp));
  }

  *end = (wchar_t *)in;
  return rval*sign;
}


		/********************************
		*           STRINGS             *
		*********************************/


char *
strcpyskip(char *t, char *f)
{ while( (*t = *f) )
    t++, f++;

  return t;
}


status
substr(register char *str, register char *sb)
{ register char *r, *q;

  for (; *str; str++)
  { for (r=str, q=sb; *r == *q && *r != '\0'; r++, q++)
      ;

    if (*q == '\0')
      succeed;
  }

  fail;
}


status
prefixstr(char *s1, char *s2)			/* S2 is a prefix of s1 */
{ while( *s1 == *s2 && *s2 )
    s1++, s2++;

  return *s2 == EOS;
}


status
substr_ignore_case(register char *str, register char *sb)
{ register char *r, *q;

  for (; *str; str++)
  { for (r=str, q=sb; tolower(*r) == tolower(*q) && *r != '\0'; r++, q++)
      ;

    if (*q == EOS)
      succeed;
  }

  fail;
}


status
prefixstr_ignore_case(char *s1, char *s2) /* S2 is a prefix of s1 */
{ while( tolower(*s1) == tolower(*s2) && *s2 )
   s1++, s2++;

  return *s2 == EOS;
}


status
streq_ignore_case(char *s1, char *s2)
{ while( tolower(*s1) == tolower(*s2) && *s2 )
   s1++, s2++;

  return *s1 == EOS && *s2 == EOS;
}


char *
upcasestr(char *s)
{ register char *q = s;

  for( ; *q; q++ )
    *q = toupper(*q);

  return s;
}


char *
downcasestr(char *s)
{ register char *q = s;

  for( ; *q; q++ )
    *q = tolower(*q);

  return s;
}


void
checkSummaryCharp(Name classname, Name name, char *s)
{ int l;

  for(l=0; l<70 && *s; l++, s++)
    if ( !((*s >= ' ' && *s < 127) || *s == '\t') )
      sysPce("%s (%s): Bad summary string", pp(classname), pp(name));

  if ( *s != EOS || (l != 0 && l < 5) )
    sysPce("%s (%s): Bad summary string: %s", pp(classname), pp(name), s);
}



Name
characterName(Any chr)
{ wchar_t buf[10];
  int ctrl;
  int c;

  if ( instanceOfObject(chr, ClassEvent) )
  { EventObj ev = chr;

    if ( !isInteger(ev->id) )
      return ev->id;
    else
    { c = valInt(ev->id);
      ctrl = (valInt(ev->buttons) & BUTTON_control);
    }
  } else
  { if ( !isInteger(chr) )
      return chr;

    c = valInt(chr);
    ctrl = FALSE;
  }

  if ( c >= META_OFFSET )
  { wcscpy(buf, L"\\e");
    c -= META_OFFSET;
  } else
    buf[0] = EOS;

  if ( ctrl )
    goto ctrl;

  switch(c)
  { case ESC:
      wcscat(buf, L"\\e");
      break;
    case ' ':
      wcscat(buf, L"SPC");
      break;
    case '\t':
      wcscat(buf, L"TAB");
      break;
    case '\r':
      wcscat(buf, L"RET");
      break;
    case '\n':
      wcscat(buf, L"LFD");
      break;
    case DEL:
      wcscat(buf, L"DEL");
      break;
    default:
    ctrl:
      if ( c < ' ' )
      {	size_t l;

	wcscat(buf, L"\\C-");
	buf[l=wcslen(buf)] = tolower(c + '@');
	buf[l+1] = EOS;
      } else
      { size_t l;

	if ( ctrl )
	  wcscat(buf, L"\\C-");
        buf[l=wcslen(buf)] = c;
	buf[l+1] = EOS;
      }
  }

  return WCToName(buf, wcslen(buf));
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
				WRITEF
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

status
writef_arguments(char *fm, va_list args, int *argc, Any *argv)
{ int ac = 0;

  while(*fm)
  { switch(*fm)
    { case '\\':
	if ( *++fm )
	  fm++;
	continue;
      case '%':
	if ( *++fm == '%' )
	{ fm++;
	  continue;
	}
	if ( *fm == '+' || *fm == '-' || *fm == ' ' || *fm == '#' )
	  fm++;
	if ( *fm == '*' )
	  argv[ac++] = va_arg(args, Any);
	else
	{ while( (*fm >= '0' && *fm <= '9') || *fm == '.' )
	    fm++;
	}
	if ( *fm )
	{ fm++;
	  argv[ac++] = va_arg(args, Any);
	}
	continue;
      default:
	fm++;
	continue;
    }
  }

  *argc = ac;
  succeed;
}


static void
vwritef(char *fm, va_list args)
{ Any argv[VA_PCE_MAX_ARGS];
  int argc;
  CharArray ca;

  writef_arguments(fm, args, &argc, argv);
  ca = CtoScratchCharArray(fm);
  ServiceMode(PCE_EXEC_SERVICE, formatPcev(PCE, ca, argc, argv));
  doneScratchCharArray(ca);
}


void
writef(char *fm, ...)
{ va_list args;

  va_start(args, fm);
  vwritef(fm, args);
  va_end(args);
}


#define NOT_SET INT_MAX
#define Put(c) if ( !(*out)(closure, c) ) return FALSE;
#define PutString(s) do { const char *_s=(s); \
			  while(*_s) \
			  { Put(*_s); _s++; \
			  } \
			} while(0)

static int
put_string(int (*out)(void*, wint_t), void *closure, String s)
{ int i;

  if ( isstrA(s) )
  { charA *t = s->s_textA;

    for(i=0; i<s->s_size; i++)
      Put(t[i]);
  } else
  { charW *t = s->s_textW;

    for(i=0; i<s->s_size; i++)
      Put(t[i]);
  }

  return TRUE;
}


#ifdef __WINDOWS__
/* See pl-nt.c of SWI-Prolog for details on this madness
*/

#define snprintf ms_snprintf

int
ms_snprintf(char *buffer, size_t count, const char *fmt, ...)
{ va_list ap;
  int ret;

  va_start(ap, fmt);
  ret = _vsnprintf(buffer, count-1, fmt, ap);
  va_end(ap);

  if ( ret < 0 || ret == count )
  { ret = (int)count;
    buffer[count-1] = '\0';
  }

  return ret;
}
#endif


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Comment to avoid mkproto not generating the prototype for this function
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
swritefv(int (*out)(void*, wint_t), void *closure,
	 const String fmt, int argc, const Any argv[])
{ int i;

  for( i=0; i<fmt->s_size; i++ )
  { wint_t c;

    switch((c=str_fetch(fmt, i)))
    { case '\\':
	if ( ++i == fmt->s_size )
	{ Put('\\');
	  continue;
	} else
	{ switch(c = str_fetch(fmt, i))

	  { case 'r':	Put('\r');	break;
	    case 'n':	Put('\n');	break;
	    case 'b':	Put('\b');	break;
	    case 'f':	Put('\f');	break;
	    case 't':	Put('\t');	break;
	    default:	Put(c);		break;
	  }
	  continue;
	}
      case '%':
	if ( ++i == fmt->s_size )
	{ Put('\\');
	  continue;
	}
        c = str_fetch(fmt, i);
	if ( c == '%' )			/* `%%' */
	{ Put('%');
	  continue;
	} else
	{ char fmtbuf[100];
	  char *r = fmtbuf;
	  int arg;

	  *r++ = '%';

	  if ( c == '+' || c == '-' || c == ' ' || c == '#' )
	  { *r++ = c;
	    i++;
	    c = str_fetch(fmt, i);
	  }

	  if ( c == '*' )
	  { Int i;

	    if ( argc <= 0 )
	      arg = 0;
	    else if ( (i = checkType(argv[0], TypeInt, NIL)) )
	      arg = valInt(i);
	    else
	      arg = 0;

	    argc--, argv++;
	  } else
	  { arg = NOT_SET;
	    while( (c >= '0' && c <= '9') || c == '.' )
	    { *r++ = c;
	      i++;
	      c = str_fetch(fmt, i);
	    }
	  }

	  switch(c)
	  { case 'c':
	    case 'd':
	    case 'i':
	    case 'o':
	    case 'u':
	    case 'x':
	    case 'X':
	    { int a;
	      Int i;
	      char buf[64];
	      int rc;

	      if ( argc <= 0 )
		a = 0;
	      else
	      { if ( (i = checkType(argv[0], TypeInt, NIL)) )
		  a = valInt(i);
	        else
		{ PutString(pp(argv[0]));
		  argc--, argv++;

		  continue;
		}
		argc--, argv++;
	      }

	      *r++ = c;
	      *r = EOS;

	      if ( arg == NOT_SET )
		rc = snprintf(buf, sizeof(buf), fmtbuf, a);
	      else
		rc = snprintf(buf, sizeof(buf), fmtbuf, arg, a);

	      if ( rc < sizeof(buf) )
	      { PutString(buf);
	      } else
	      { size_t size = sizeof(buf)*2;

		for(;;)
		{ char *b2 = pceMalloc(size);

		  if ( arg == NOT_SET )
		    rc = snprintf(b2, size, fmtbuf, a);
		  else
		    rc = snprintf(b2, size, fmtbuf, arg, a);

		  if ( rc < size )
		  { PutString(b2);
		    pceFree(b2);
		    break;
		  }

		  pceFree(b2);
		  size *= 2;
		}
	      }

	      continue;
	    }
	    case 'f':
	    case 'e':
	    case 'E':
	    case 'g':
	    case 'G':
	    { double a;
	      Real f;
	      char buf[64];
	      int rc;

	      if ( argc <= 0 )
		a = 0.0;
	      else
	      { if ( (f = checkType(argv[0], TypeReal, NIL)) )
		  a = valReal(f);
	        else
		{ PutString(pp(argv[0]));
		  argc--, argv++;

		  continue;
		}
		argc--, argv++;
	      }

	      *r++ = c;
	      *r = EOS;
	      if ( arg == NOT_SET )
		rc = snprintf(buf, sizeof(buf), fmtbuf, a);
	      else
		rc = snprintf(buf, sizeof(buf), fmtbuf, arg, a);

	      if ( rc < sizeof(buf) )
	      { PutString(buf);
	      } else
	      { size_t size = sizeof(buf)*2;

		for(;;)
		{ char *b2 = pceMalloc(size);

		  if ( arg == NOT_SET )
		    snprintf(b2, size, fmtbuf, a);
		  else
		    snprintf(b2, size, fmtbuf, arg, a);

		  if ( rc < size )
		  { PutString(b2);
		    pceFree(b2);
		    break;
		  }

		  pceFree(b2);
		  size *= 2;
		}
	      }

	      continue;
	    }
	  { string a;

	    case 's':
	    case 'N':
	    { if ( argc <= 0 )
	      { str_set_ascii(&a, "(nil)");
		a.s_readonly = TRUE;
	      } else if ( !toString(argv[0], &a) )
	      { Any pn;

		ServiceMode(PCE_EXEC_SERVICE,
			    pn = get(argv[0], NAME_printName, EAV));
		if ( !(pn && toString(pn, &a)) )
		  str_set_utf8(&a, pp(argv[0]));
	      }

	    outstr:
	      *r++ = 's';
	      *r = EOS;

	      if ( arg == NOT_SET )
		arg = atoi(fmtbuf+1);

	      if ( a.s_size >= abs(arg) )
	      { if ( !put_string(out, closure, &a) )
		  return FALSE;
	      } else
	      { int pad = abs(arg) - a.s_size;
		int i;

		if ( arg > 0 )
		{ for(i=0; i<pad; i++)
		    Put(' ');
		  if ( !put_string(out, closure, &a) )
		    return FALSE;
		} else
		{ if ( !put_string(out, closure, &a) )
		    return FALSE;
		  for(i=0; i<pad; i++)
		    Put(' ');
		}
	      }

	      argc--, argv++;
	      continue;
	    }
	    case 'O':			/* object via pp() */
	    { if ( argc <= 0 )
		str_set_ascii(&a, "(nil)");
	      else
		str_set_utf8(&a, pp(argv[0]));

	      goto outstr;
	    }
	  }				/* end scope of a */
	    case 'I':			/* ignore */
	    { argc--, argv++;
	      continue;
	    }
	    default:
	      Put('%');
	      Put(c);
	      continue;
	  }
	}
      default:
	Put(c);
	continue;
      }
  }

  return TRUE;
}


static int
put_void_str(void *ctx, wint_t c)
{ String s = ctx;

  s->s_size++;
  if ( c > 0xff )
    s->s_iswide = TRUE;

  return TRUE;
}


static int
put_str(void *ctx, wint_t c)
{ String s = ctx;

  str_store(s, s->s_size, c);
  s->s_size++;

  return TRUE;
}


status
str_writefv(String s, CharArray format, int argc, const Any *argv)
{ int len;

  str_inithdr(s, FALSE);
  s->s_size = 0;
  swritefv(put_void_str, s, &format->data, argc, argv);
  len = s->s_size;
  str_alloc(s);
  s->s_size = 0;		/* dubious */
  swritefv(put_str, s, &format->data, argc, argv);
  assert(s->s_size == len);

  succeed;
}


		/********************************
		*              SCANF		*
		********************************/

#define T_CHAR	    0			/* C-types returned */
#define T_INT       1
#define T_FLOAT     2
#define T_CHARP     3

#define S_UNSIGNED  8

#define L_SHORT    16
#define L_LONG     32

#ifdef __linux__			/* delete if conflict arrises */
extern int vsscanf(const char *, const char *, va_list);
#endif

#ifdef ALLOCA_BUG
#undef alloca
#define alloca(n) pceMalloc(n)
#endif /*ALLOCA_BUG*/

#if !(defined(HAVE_VSSCANF) && defined(HAVE_CAST_VA_LIST))
#define NO_VSSCANF 1
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wformat-security"
#endif

Int
scanstr(char *str, char *fmt, Any *r)
{ int types[SCAN_MAX_ARGS];
  void *ptrs [SCAN_MAX_ARGS];
  int argn = 0;
  int ar, n;
  char *s = fmt;

  while( *s )
  { int supress = FALSE;
    int length;

    switch( *s )
    { case '\\':
	s += (s[1] == EOS ? 1 : 2);
	continue;
      case '%':
	s++;
	if ( *s == '%' )		/* %% */
	{ s++;
	  continue;
	}
	if ( isdigit(*s) && s[1] == '$' ) /* %digit$ */
	  s += 2;
	if ( *s == '*' )		/* %* */
	{ supress = TRUE;
	  s++;
	}
	while( *s && isdigit(*s) )	/* field width */
	  s++;
	if ( *s == EOS )		/* check for end */
	  continue;

	if ( *s == 'l' )		/* Integer size */
	{ length = L_LONG;
	  s++;
	} else if ( *s == 'h' )
	{ length = L_SHORT;
	  s++;
	} else
	  length = 0;

	switch(*s)			/* conversion char */
	{ case 'u':
	    if ( !supress )
	    { types[argn] = length|T_INT|S_UNSIGNED;
	      ptrs[argn]  = (void *)alloca(sizeof(long));
	      argn++;
	    }
	    s++;
	    continue;
	  case 'd':
	  case 'o':
	  case 'x':
	  case 'i':
	  case 'n':
	    if ( !supress )
	    { types[argn] = length|T_INT;
	      ptrs[argn]  = (void *)alloca(sizeof(long));
	      argn++;
	    }
	    s++;
	    continue;
	  case 'e':
	  case 'f':
	  case 'g':
	    if ( !supress )
	    { types[argn] = length|T_FLOAT;
	      ptrs[argn]  = (void *)alloca(length == L_LONG ? sizeof(double)
				                            : sizeof(float));
	      argn++;
	    }
	    s++;
	    continue;
	  case '[':
	    s++;
	    if ( *s == '^' ) s++;
	    if ( *s == ']' ) s++;
	    while( *s && *s != ']' )
	      s++;
	  case 's':
	    if ( !supress )
	    { types[argn]  = T_CHARP;
	      ptrs[argn] = (void *)alloca(LINESIZE);
	      argn++;
	    }
	    s++;
	    continue;
	  case 'c':
	    if ( !supress )
	    { types[argn] = T_CHAR;
	      ptrs[argn]  = (void *)alloca(sizeof(char));
	      argn++;
	    }
	    s++;
	    continue;
	  default:
	    s++;
	    continue;
	}
      default:
	s++;
	continue;
    }
  }

  DEBUG(NAME_scan, Cprintf("argn = %d\n", argn));
#ifndef NO_VSSCANF
  ar = vsscanf(str, fmt, (va_list) ptrs);
#else
  switch(argn)
  { case 0:	ar = sscanf(str, fmt); break;
    case 1:	ar = sscanf(str, fmt, ptrs[0]); break;
    case 2:	ar = sscanf(str, fmt, ptrs[0], ptrs[1]); break;
    case 3:	ar = sscanf(str, fmt, ptrs[0], ptrs[1], ptrs[2]);
				      break;
    case 4:	ar = sscanf(str, fmt, ptrs[0], ptrs[1], ptrs[2],
			              ptrs[3]); break;
    case 5:	ar = sscanf(str, fmt, ptrs[0], ptrs[1], ptrs[2],
			              ptrs[3], ptrs[4]); break;
    case 6:	ar = sscanf(str, fmt, ptrs[0], ptrs[1], ptrs[2],
			              ptrs[3], ptrs[4], ptrs[5]);
				      break;
    case 7:	ar = sscanf(str, fmt, ptrs[0], ptrs[1], ptrs[2],
			              ptrs[3], ptrs[4], ptrs[5],
			              ptrs[6]); break;
    case 8:	ar = sscanf(str, fmt, ptrs[0], ptrs[1], ptrs[2],
			              ptrs[3], ptrs[4], ptrs[5],
			              ptrs[6], ptrs[7]); break;
    case 9:	ar = sscanf(str, fmt, ptrs[0], ptrs[1], ptrs[2],
			              ptrs[3], ptrs[4], ptrs[5],
			              ptrs[6], ptrs[7], ptrs[8]);
				      break;
    case 10:	ar = sscanf(str, fmt, ptrs[0], ptrs[1], ptrs[2],
			              ptrs[3], ptrs[4], ptrs[5],
			              ptrs[6], ptrs[7], ptrs[8],
				      ptrs[9]);
				      break;
    case 11:	ar = sscanf(str, fmt, ptrs[0], ptrs[1], ptrs[2],
			              ptrs[3], ptrs[4], ptrs[5],
			              ptrs[6], ptrs[7], ptrs[8],
				      ptrs[9], ptrs[10]);
				      break;
    case 12:	ar = sscanf(str, fmt, ptrs[0], ptrs[1], ptrs[2],
			              ptrs[3], ptrs[4], ptrs[5],
			              ptrs[6], ptrs[7], ptrs[8],
				      ptrs[9], ptrs[10], ptrs[11]);
				      break;
    case 13:	ar = sscanf(str, fmt, ptrs[0], ptrs[1], ptrs[2],
			              ptrs[3], ptrs[4], ptrs[5],
			              ptrs[6], ptrs[7], ptrs[8],
				      ptrs[9], ptrs[10], ptrs[11],
				      ptrs[12]);
				      break;
    case 14:	ar = sscanf(str, fmt, ptrs[0], ptrs[1], ptrs[2],
			              ptrs[3], ptrs[4], ptrs[5],
			              ptrs[6], ptrs[7], ptrs[8],
				      ptrs[9], ptrs[10], ptrs[11],
				      ptrs[12], ptrs[13]);
				      break;
    case 15:	ar = sscanf(str, fmt, ptrs[0], ptrs[1], ptrs[2],
			              ptrs[3], ptrs[4], ptrs[5],
			              ptrs[6], ptrs[7], ptrs[8],
				      ptrs[9], ptrs[10], ptrs[11],
				      ptrs[12], ptrs[13], ptrs[14]);
				      break;
    case 16:	ar = sscanf(str, fmt, ptrs[0], ptrs[1], ptrs[2],
			              ptrs[3], ptrs[4], ptrs[5],
			              ptrs[6], ptrs[7], ptrs[8],
				      ptrs[9], ptrs[10], ptrs[11],
				      ptrs[12], ptrs[13], ptrs[14],
				      ptrs[15]);
				      break;
    case 17:	ar = sscanf(str, fmt, ptrs[0], ptrs[1], ptrs[2],
			              ptrs[3], ptrs[4], ptrs[5],
			              ptrs[6], ptrs[7], ptrs[8],
				      ptrs[9], ptrs[10], ptrs[11],
				      ptrs[12], ptrs[13], ptrs[14],
				      ptrs[15], ptrs[16]);
				      break;
    case 18:	ar = sscanf(str, fmt, ptrs[0], ptrs[1], ptrs[2],
			              ptrs[3], ptrs[4], ptrs[5],
			              ptrs[6], ptrs[7], ptrs[8],
				      ptrs[9], ptrs[10], ptrs[11],
				      ptrs[12], ptrs[13], ptrs[14],
				      ptrs[15], ptrs[16], ptrs[17]);
				      break;
    case 19:	ar = sscanf(str, fmt, ptrs[0], ptrs[1], ptrs[2],
			              ptrs[3], ptrs[4], ptrs[5],
			              ptrs[6], ptrs[7], ptrs[8],
				      ptrs[9], ptrs[10], ptrs[11],
				      ptrs[12], ptrs[13], ptrs[14],
				      ptrs[15], ptrs[16], ptrs[17],
				      ptrs[18]);
				      break;
    case 20:	ar = sscanf(str, fmt, ptrs[0], ptrs[1], ptrs[2],
			              ptrs[3], ptrs[4], ptrs[5],
			              ptrs[6], ptrs[7], ptrs[8],
				      ptrs[9], ptrs[10], ptrs[11],
				      ptrs[12], ptrs[13], ptrs[14],
				      ptrs[15], ptrs[16], ptrs[17],
			              ptrs[18], ptrs[19]);
				      break;
    default:	errorPce(NIL, NAME_tooManyArguments);
		fail;
  }
#endif /*HAVE_VSSCANF*/

  DEBUG(NAME_scan, Cprintf("ar = %d\n", argn));

#define arg(n, tp) (*((tp *)(ptrs[n])))

  if ( ar < 0 )
    fail;

  for(n = 0; n<ar; n++)
  { Any val = NIL;

    switch(types[n])
    { case T_CHAR:
	val = toInt(arg(n, char));
	break;
      case T_INT|L_SHORT:
	val = toInt(arg(n, short));
	break;
      case T_INT:
	val = toInt(arg(n, int));
	break;
      case T_INT|L_LONG:
	val = toInt(arg(n, long));
	break;
      case T_INT|L_SHORT|S_UNSIGNED:
	val = toInt(arg(n, unsigned short));
	break;
      case T_INT|S_UNSIGNED:
	val = toInt(arg(n, unsigned int));
	break;
      case T_INT|L_LONG|S_UNSIGNED:
	val = toInt(arg(n, unsigned long));
	break;
      case T_FLOAT:
	val = CtoReal(arg(n, float));
	break;
      case T_FLOAT|L_LONG:
	val = CtoReal(arg(n, double));
	break;
      case T_CHARP:
	val = CtoString(ptrs[n]);
	break;
    }

#ifdef ALLOCA_BUG
    pceFree(ptrs[n]);
#endif

    r[n] = val;
  }

#undef arg


  return toInt(ar);
}

#ifdef NO_VSSCANF
#pragma GCC diagnostic pop
#endif

		/********************************
		*         FATAL ERRORS		*
		********************************/

#include <h/interface.h>

status
sysPce(char *fm, ...)
{ va_list args;
  static int nth = 0;

  if ( nth > 12 )
    exit(1);				/* no delay, just go away */
  if ( nth++ > 10 )			/* lets try decent way */
    hostAction(HOST_HALT);

  va_start(args, fm);
  catchErrorSignalsPce(PCE, OFF);

  Cprintf("[PCE system error: ");
  Cvprintf(fm, args);
  va_end(args);

#ifndef O_RUNTIME
  Cprintf("\n\tStack:\n");
  pceBackTrace(NULL, 20);
  Cprintf("]\n");

  catchErrorSignalsPce(PCE, ON);
  Cprintf("Requesting host to dump stack ...\n");
  hostAction(HOST_BACKTRACE, 10);
  hostAction(HOST_RECOVER_FROM_FATAL_ERROR);

#ifdef HAVE_GETPID
  Cprintf("[pid = %d]\n", (int) getpid());
#endif
  if (confirmTerminal("Continue", "n"))
    return PCE_FAIL;
  if (confirmTerminal("Save core image", "n"))
    abort();

  hostAction(HOST_HALT);
  exit(1);
#else /*O_RUNTIME*/
  hostAction(HOST_RECOVER_FROM_FATAL_ERROR);
  hostAction(HOST_HALT);
  exit(1);
#endif /*O_RUNTIME*/

  return PCE_FAIL;
}


		 /*******************************
		 *	  TIME SUPPORT		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
These functions get the time since XPCE was started in milliseconds.  Used
for timing of GUI actions (see scrollbar.c for example).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifdef HAVE_GETTIMEOFDAY
#include <sys/time.h>
static struct timeval epoch;

void
initMClock()
{ gettimeofday(&epoch, NULL);
}


unsigned long
mclock()
{ struct timeval now;

  gettimeofday(&now, NULL);
  return (now.tv_sec - epoch.tv_sec) * 1000 +
	 (now.tv_usec - epoch.tv_usec)/1000;
}

#else
#if HAVE_FTIME
#include <sys/timeb.h>

static struct _timeb epoch;

void
initMClock()
{ _ftime(&epoch);
}


unsigned long
mclock()
{ struct _timeb now;

  _ftime(&now);
  return (now.time - epoch.time) * 1000 +
	 (now.millitm - epoch.millitm);
}

#endif /*HAVE_FTIME*/
#endif


		/********************************
		*            SLEEP		*
		********************************/

#if !defined(MSLEEP_DONE) && defined(HAVE_NANOSLEEP)
#define MSLEEP_DONE 1
#include <errno.h>

void
msleep(int time)
{ struct timespec req;
  int rc;

  if ( time < 0 )
    return;

  DEBUG(NAME_flash,
	Cprintf("nanosleep() %d milliseconds ...\n", time));

  req.tv_sec = time/1000;
  req.tv_nsec = (time%1000)*1000000;

  do
  { rc = nanosleep(&req, &req);
  } while ( rc == -1 && errno == EINTR );

  DEBUG(NAME_flash, Cprintf("ok\n"));
}

#endif

#if !defined(MSLEEP_DONE) && defined(HAVE_SELECT)
#define MSLEEP_DONE 1

#ifdef HAVE_SYS_SELECT_H
#include <sys/select.h>
#endif

void
msleep(int time)
{ struct timeval timeout;
  timeout.tv_sec = time/1000;
  timeout.tv_usec = (time%1000)*1000;

  DEBUG(NAME_flash,
	Cprintf("waiting %d milliseconds ...\n", time));

#ifdef SELLIST
  { SELLIST(1,1) readbits = {0, 0};
    SELLIST(1,1) writebits = {0, 0};
    SELLIST(1,1) exceptbits = {0, 0};

    select(0, &readbits, &writebits, &exceptbits, &timeout);
  }
#else
  select(0, NULL, NULL, NULL, &timeout);
#endif

  DEBUG(NAME_flash, Cprintf("ok\n"));
}

#endif

#ifndef MSLEEP_DONE

extern void ws_msleep(int time);

void
msleep(int time)
{ ws_msleep(time);
}

#endif

		 /*******************************
		 *	 ASSERT() SUPPORT	*
		 *******************************/

int
pceAssert(int expr, char *text, char* file, int line)
{ if ( !expr )
    sysPce("%s:%d: Assertion failed: %s", file, line, text);

  return 0;
}

		 /*******************************
		 *	    AT_PCE_EXIT		*
		 *******************************/

typedef struct atexit_entry *AtexitEntry;

struct atexit_entry
{ atexit_function	function;
  AtexitEntry		next;
};

static AtexitEntry atexit_head;
static AtexitEntry atexit_tail;
static int exit_running;

void
at_pce_exit(atexit_function f, int flags)
{ if ( !exit_running )
  { AtexitEntry h = alloc(sizeof(struct atexit_entry));

    h->function = f;
    if ( !atexit_head )
    { atexit_head = atexit_tail = h;
      h->next = NULL;
    } else
    { if ( flags & ATEXIT_FIFO )	/* first-in-first-out */
      { h->next = atexit_head;
	atexit_head = h;
      } else				/* first-in-last-out */
      { h->next = NULL;
	atexit_tail->next = h;
	atexit_tail = h;
      }
    }
  }
}


int
run_pce_exit_hooks(int rval)
{ AtexitEntry h;

#ifndef O_RUNTIME
  assign(PCE, trap_errors, OFF);
  debuggingPce(PCE, OFF);
#endif

  if ( exit_running++ )
    return -1;

  for(h = atexit_head; h; h = h->next)
    (*h->function)(rval);

  return 0;
}

