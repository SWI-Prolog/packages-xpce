/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org
    Copyright (c)  2026, SWI-Prolog Solutions b.v.
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

/* This file is shared by packages/clib (process.c) and
 * packages/xpce (src/msw/msprocess.c), which include it.  The two
 * copies must be identical: change both.
 *
 * The includer may define WBUF_REALLOC as the function that (re)allocates
 * wbuf.base.  The default is realloc().  The includer frees wbuf.base
 * with the matching function.
 */

#include <wchar.h>
#include <stdbool.h>
#include <stdlib.h>

#ifndef WBUF_REALLOC
#define WBUF_REALLOC realloc
#endif

/* Build a Windows command line from the arguments.  Windows programs
 * split the command line using the rules of the C runtime
 * (CommandLineToArgvW()): an argument is quoted using "..." if it is
 * empty or contains white space or a ", backslashes are literal unless
 * they precede a ", where 2n backslashes give n backslashes and 2n+1
 * give n backslashes and a literal ".  Batch files (.bat, .cmd) are run
 * by cmd.exe, which does not use these rules: there we also quote the
 * metacharacters of cmd.exe and refuse characters that cannot be passed
 * safely.
 */

typedef struct wbuf
{ wchar_t *base;
  size_t   len;
  size_t   size;
} wbuf;

static bool
wbuf_add(wbuf *b, wchar_t c)
{ if ( b->len+1 >= b->size )
  { size_t nsize = b->size ? b->size*2 : 256;
    wchar_t *n = WBUF_REALLOC(b->base, nsize*sizeof(wchar_t));

    if ( !n )
      return false;
    b->base = n;
    b->size = nsize;
  }
  b->base[b->len++] = c;
  b->base[b->len] = 0;
  return true;
}

static bool
wbuf_add_n(wbuf *b, wchar_t c, size_t n)
{ while(n-- > 0)
  { if ( !wbuf_add(b, c) )
      return false;
  }
  return true;
}

static bool
is_batch_file(const wchar_t *exe)
{ size_t len = wcslen(exe);

  return ( len > 4 &&
	   ( _wcsicmp(exe+len-4, L".bat") == 0 ||
	     _wcsicmp(exe+len-4, L".cmd") == 0 ) );
}

static bool
add_quoted_arg(wbuf *b, const wchar_t *s, size_t len, int batch)
{ int needq = (len == 0);
  size_t i;

  for(i=0; i<len; i++)
  { wchar_t c = s[i];

    if ( c == ' ' || c == '\t' || c == '\n' || c == '\v' || c == '"' )
      needq = true;
    else if ( batch && wcschr(L"&|<>^()", c) )
      needq = true;
  }

  if ( !needq )
  { for(i=0; i<len; i++)
    { if ( !wbuf_add(b, s[i]) )
	return false;
    }
    return true;
  }

  if ( !wbuf_add(b, '"') )
    return false;
  for(i=0; i<len; )
  { size_t bs = 0;

    while(i < len && s[i] == '\\')
    { bs++;
      i++;
    }
    if ( i == len )			/* backslashes before closing quote */
    { if ( !wbuf_add_n(b, '\\', bs*2) )
	return false;
      break;
    } else if ( s[i] == '"' )		/* backslashes before a quote */
    { if ( !wbuf_add_n(b, '\\', bs*2+1) ||
	   !wbuf_add(b, '"') )
	return false;
      i++;
    } else
    { if ( !wbuf_add_n(b, '\\', bs) ||
	   !wbuf_add(b, s[i]) )
	return false;
      i++;
    }
  }
  return wbuf_add(b, '"');
}

/* Characters that cmd.exe interprets also inside "...": quotes end the
   quoted string, % and ! expand variables, CR and LF end the command. */

static bool
batch_safe_arg(const wchar_t *s, size_t len)
{ size_t i;

  for(i=0; i<len; i++)
  { if ( wcschr(L"\"%!\r\n", s[i]) )
      return false;
  }
  return true;
}
