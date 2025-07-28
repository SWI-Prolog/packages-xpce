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
#include <h/str.h>

#undef min
#define min(a, b) ((a) < (b) ? (a) : (b))

#define sameEncoding(s1, s2) \
	if ( s1->iswide != s2->iswide ) \
	  return FALSE;


		 /*******************************
		 *	       ALLOC		*
		 *******************************/

inline int
str_allocsize(PceString s)
{ int len;

  len = isstrA(s) ? s->s_size : s->s_size*sizeof(charW);

  return ((len + sizeof(long))/sizeof(long))*sizeof(long);
}


inline void
str_pad(PceString s)			/* only 8-bit strings */
{ if ( isstrA(s) )
  { int from = s->s_size;
    int len  = str_allocsize(s);

    while(from < len)
      s->s_textA[from++] = '\0';
  } else
  { int from = s->s_size;
    int len  = str_allocsize(s)/sizeof(charW);

    while(from < len)
      s->s_textW[from++] = '\0';
  }
}


void
str_alloc(PceString s)
{ s->s_textA  = alloc(str_allocsize(s));
  s->s_readonly = FALSE;
  str_pad(s);
}


#define STR_RING_SIZE 16
static char *str_ring[STR_RING_SIZE] = {NULL};
int    str_ring_ptr = 0;

static void
str_ring_alloc(PceString s)
{ int size = str_allocsize(s);

  if ( !str_ring[str_ring_ptr] )
  { str_ring[str_ring_ptr] = pceMalloc(size);
  } else
  { str_ring[str_ring_ptr] = pceRealloc(str_ring[str_ring_ptr], size);
  }
  s->s_textA = (charA*)str_ring[str_ring_ptr];
  s->s_readonly = TRUE;

  if ( ++str_ring_ptr == STR_RING_SIZE )
    str_ring_ptr = 0;
}


void
str_unalloc(PceString s)
{ if ( s->s_textA && !s->s_readonly )
  { unalloc(str_allocsize(s), s->s_textA);
    s->s_textA = NULL;
  }
}


PceString
str_init(PceString s, PceString proto, charA *data)
{ str_cphdr(s, proto);
  s->s_text = data;

  return s;
}


PceString
fstr_inithdr(PceString s, int iswide, void *data, int len)
{ str_inithdr(s, iswide);
  s->s_text = data;
  s->s_size = len;

  return s;
}


status
str_set_n_ascii(PceString str, size_t len, char *text)
{ if ( len > STR_MAX_SIZE )
    return errorPce(NIL, NAME_stringTooLong, toInt(len));

  str_inithdr(str, FALSE);
  str->s_size = (int)len;
  str->s_textA = (charA *) text;

  succeed;
}


status
str_set_n_wchar(PceString str, size_t len, wchar_t *text)
{ if ( len > STR_MAX_SIZE )
    return errorPce(NIL, NAME_stringTooLong, toInt(len));

  str_inithdr(str, TRUE);
  str->s_size = (int)len;
  str->s_textW = text;

  succeed;
}


status
str_set_ascii(PceString str, char *text)
{ size_t len = strlen(text);

  return str_set_n_ascii(str, len, text);
}


status
str_set_utf8(PceString str, const char *text)
{ const char *s = text;
  const char *e = &text[strlen(s)];
  int iswide = FALSE;
  int len = 0;

  while(s<e)
  { int chr;

    s = utf8_get_char(s, &chr);
    if ( chr > 0xff )
      iswide = TRUE;
    len++;
  }

  str_inithdr(str, iswide);
  str->s_size = len;
  str_ring_alloc(str);			/* NOTE: temporary space */

  for(len=0, s=text; s<e; len++)
  { int chr;

    s = utf8_get_char(s, &chr);
    str_store(str, len, chr);
  }

  succeed;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
str_set_static(): initialise a string from a static C-string
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

status
str_set_static(PceString str, const char *text)
{ size_t len = strlen(text);

  if ( len > STR_MAX_SIZE )
    return errorPce(NIL, NAME_stringTooLong, toInt(len));

  str_inithdr(str, FALSE);
  str->s_readonly = TRUE;
  str->s_size = (int)len;
  str->s_textA = (charA *) text;

  succeed;
}


status
str_iswide(PceString s)
{ if ( s->s_iswide )
  { const charW *w = s->s_textW;
    const charW *e = &w[s->s_size];

    while(w<e)
    { if ( *w++ > 0xff )
	succeed;
    }
  }

  fail;
}



		 /*******************************
		 *	     COPY STUFF		*
		 *******************************/

void
str_ncpy(PceString dest, int at, PceString src, int from, int len)
{ if ( dest->s_iswide == src->s_iswide )	/* same size */
  { if ( isstrA(dest) )
      memcpy(&dest->s_textA[at], &src->s_textA[from], len * sizeof(charA));
    else
      cpdata(&dest->s_textW[at], &src->s_textW[from], charW, len);
  } else if ( dest->s_iswide )		/* 8bit --> wide */
  { const charA *s = &src->s_textA[from];
    const charA *e = &s[len];
    charW *d = &dest->s_textW[at];

    while(s<e)
      *d++ = *s++;
  } else				/* wide --> 8bit (may truncate) */
  { const charW *s = &src->s_textW[from];
    const charW *e = &s[len];
    charA *d = &dest->s_textA[at];

    while(s<e)
      *d++ = *s++;
  }
}


void
str_cpy(PceString dest, PceString src)
{ str_cphdr(dest, src);
  str_ncpy(dest, 0, src, 0, src->s_size);
}


charA *
str_textp(PceString s, int i)
{ return isstrA(s) ? &s->s_textA[i] : (charA *)&s->s_textW[i];
}


		 /*******************************
		 *	CASE MANIPULATION	*
		 *******************************/

void
str_upcase(PceString str, int from, int to)
{ if ( isstrA(str) )
  { charA *s = &str->s_textA[from];

    for(; from < to; from++, s++)
      *s = toupper(*s);
  } else
  { charW *s = &str->s_textW[from];

    for(; from < to; from++, s++)
      *s = towupper(*s);
  }
}


void
str_downcase(PceString str, int from, int to)
{ if ( isstrA(str) )
  { charA *s = &str->s_textA[from];

    for(; from < to; from++, s++)
      *s = tolower(*s);
  } else
  { charW *s = &str->s_textW[from];

    for(; from < to; from++, s++)
      *s = towlower(*s);
  }
}

void
str_translate(PceString str, int from, int to)
{ if ( isstrA(str) )
  { charA *s = str->s_textA;
    for(int i=0; i<str->s_size; i++, s++)
    { if ( ((*s)&0xff) == from )
	*s = to;
    }
  } else
  { charW *s = str->s_textW;
    for(int i=0; i<str->s_size; i++, s++)
    { if ( *s == from )
	*s = to;
    }
  }
}


		 /*******************************
		 *	      COMPARE		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
int str_cmp(PceString s1, PceString s2)
    returns < 0 if s1 is before s2, == 0 if equal and > 0 if s2 is
    before s2.

int str_eq(PceString s1, PceString s2)
    returns != 0 if s1 equals s2.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
str_cmp(PceString s1, PceString s2)
{ int n = min(s1->s_size, s2->s_size);

  if ( s1->s_iswide == s2->s_iswide )
  { if ( isstrA(s1) )
    { int d;

      if ( (d=strncmp((const char*)s1->s_textA, (const char*)s2->s_textA, n)) == 0 )
	return s1->s_size - s2->s_size;

      return d;
    } else
    { charW *d1 = s1->s_textW;
      charW *d2 = s2->s_textW;
      int d;

      while(n-- > 0)
	if ( (d = (*d1++ - *d2++)) )
	  return d;

      return s1->s_size - s2->s_size;
    }
  } else				/* inconsistent encoding */
  { int i;

    for(i=0; i<n; i++)
    { wint_t c1 = str_fetch(s1, i);
      wint_t c2 = str_fetch(s2, i);

      if ( c1 != c2 )
	return c1 -c2;
    }

    return s1->s_size - s2->s_size;
  }
}


int
str_icase_cmp(PceString s1, PceString s2)
{ int n = min(s1->s_size, s2->s_size);

  if ( s1->s_iswide == s2->s_iswide )
  { if ( isstrA(s1) )
    { charA *d1 = s1->s_textA;
      charA *d2 = s2->s_textA;
      int d;

      for(; n-- > 0; d1++, d2++)
	if ( (d = (tolower(*d1) - tolower(*d2))) )
	  return d;

      return s1->s_size - s2->s_size;
    } else
    { charW *d1 = s1->s_textW;
      charW *d2 = s2->s_textW;
      int d;

      for(; n-- > 0; d1++, d2++)
	if ( (d = (towlower(*d1) - towlower(*d2))) )
	  return d;

      return s1->s_size - s2->s_size;
    }
  } else
  { int i;

    for(i=0; i<n; i++)
    { wint_t c1 = towlower(str_fetch(s1, i));
      wint_t c2 = towlower(str_fetch(s2, i));

      if ( c1 != c2 )
	return c1 -c2;
    }

    return s1->s_size - s2->s_size;
  }
}


int
str_eq(PceString s1, PceString s2)
{ if ( s1->s_size == s2->s_size )
    return str_cmp(s1, s2) == 0;

  return FALSE;
}


int
str_icase_eq(PceString s1, PceString s2)
{ if ( s1->s_size == s2->s_size )
    return str_icase_cmp(s1, s2) == 0;

  return FALSE;
}


int					/* s2 is prefix of s1+offset */
str_prefix_offset(PceString s1, unsigned int offset, PceString s2)
{ if ( s2->s_size <= s1->s_size-offset )
  { int n = s2->s_size;

    if ( isstrA(s1) && isstrA(s2) )
    { charA *d1 = s1->s_textA+offset;
      charA *d2 = s2->s_textA;

      while(n-- > 0)
	if ( *d1++ != *d2++ )
	  return FALSE;

      return TRUE;
    } else
    { int i;

      for(i=0; i<n; i++)
	if ( str_fetch(s1, i+offset) != str_fetch(s2, i) )
	  return FALSE;
    }

    return TRUE;
  }

  return FALSE;
}


int
str_prefix(PceString s1, PceString s2)	/* s2 is prefix of s1 */
{ return str_prefix_offset(s1, 0, s2);
}


int
str_icase_prefix(PceString s1, PceString s2)	/* s2 is prefix of s1 */
{ if ( s2->s_size <= s1->s_size )
  { int n = s2->s_size;

    if ( isstrA(s1) && isstrA(s2) )
    { charA *d1 = s1->s_textA;
      charA *d2 = s2->s_textA;

      for(; n-- > 0; d1++, d2++)
	if ( tolower(*d1) != tolower(*d2) )
	  return FALSE;
    } else
    { int i = 0;

      for(; n-- > 0; i++)
	if ( towlower(str_fetch(s1, i)) != towlower(str_fetch(s2, i)) )
	  return FALSE;
    }

    return TRUE;
  }

  return FALSE;
}


int
str_suffix(PceString s1, PceString s2)	/* s2 is suffix of s1 */
{ if ( s2->s_size <= s1->s_size )
  { int n = s2->s_size;
    int offset = s1->s_size - s2->s_size;

    if ( isstrA(s1) && isstrA(s2) )
    { charA *d1 = &s1->s_textA[offset];
      charA *d2 = s2->s_textA;

      while(n-- > 0)
	if ( *d1++ != *d2++ )
	  return FALSE;

      return TRUE;
    } else
    { while(--n >= 0)
	if ( str_fetch(s1, n+offset) != str_fetch(s2, n) )
	  return FALSE;

      return TRUE;
    }
  }

  return FALSE;
}


int
str_icase_suffix(PceString s1, PceString s2)	/* s2 is suffix of s1 */
{ if ( s2->s_size <= s1->s_size )
  { int n = s2->s_size;
    int offset = s1->s_size - s2->s_size;

    if ( isstrA(s1) && isstrA(s2) )
    { charA *d1 = &s1->s_textA[offset];
      charA *d2 = s2->s_textA;

      for( ; n-- > 0; d1++, d2++)
      { if ( tolower(*d1) != tolower(*d2) )
	  return FALSE;
      }

      return TRUE;
    } else
    { int i = 0;

      for( ; n-- > 0; i++)
      { if ( towlower(str_fetch(s1, i)) != towlower(str_fetch(s2, i)) )
	  return FALSE;
      }
    }

    return TRUE;
  }

  return FALSE;
}


int
str_sub(PceString s1, PceString s2)		/* s2 is substring of s1 */
{ if ( s2->s_size <= s1->s_size )
  { int n = 0;
    int m = s1->s_size - s2->s_size;

    if ( s1->s_iswide == s2->s_iswide )
    { if ( isstrA(s1) )
      { for(; n <= m; n++)
	{ charA *d1 = &s1->s_textA[n];
	  charA *d2 = s2->s_textA;
	  int i = s2->s_size;

	  while( i-- > 0 )
	    if ( *d1++ != *d2++ )
	      goto next8;

	  return TRUE;
	next8:;
	}
      } else
      { for(; n <= m; n++)
	{ charW *d1 = &s1->s_textW[n];
	  charW *d2 = s2->s_textW;
	  int i = s2->s_size;

	  while( i-- > 0 )
	    if ( *d1++ != *d2++ )
	      goto next16;

	  return TRUE;
	next16:;
	}
      }
    } else
    { for(; n <= m; n++)
      { int i1 = n;
	int i2 = 0;
	int i = s2->s_size;

	for( ; i-- > 0; i1++, i2++ )
	  if ( str_fetch(s1, i1) != str_fetch(s2, i2) )
	    goto nextmixed;

	return TRUE;
      nextmixed:;
      }
    }
  }

  return FALSE;
}


int
str_icasesub(PceString s1, PceString s2)		/* s2 is substring of s1 */
{ if ( s2->s_size <= s1->s_size )
  { int n = 0;
    int m = s1->s_size - s2->s_size;

    if ( s1->s_iswide == s2->s_iswide )
    { if ( isstrA(s1) )
      { for(; n <= m; n++)
	{ charA *d1 = &s1->s_textA[n];
	  charA *d2 = s2->s_textA;
	  int i;

	  for(i=s2->s_size; i-- > 0; d1++, d2++ )
	  { if ( tolower(*d1) != tolower(*d2) )
	      goto next8;
	  }

	  return TRUE;
	next8:;
	}
      } else
      { for(; n <= m; n++)
	{ charW *d1 = &s1->s_textW[n];
	  charW *d2 = s2->s_textW;
	  int i;

	  for(i=s2->s_size; i-- > 0; d1++, d2++ )
	  { if ( towlower(*d1) != towlower(*d2) )
	      goto next16;
	  }

	  return TRUE;
	next16:;
	}
      }
    } else
    { for(; n <= m; n++)
      { int i1 = n;
	int i2 = 0;
	int i = s2->s_size;

	for( ; i-- > 0; i1++, i2++ )
	  if ( towlower(str_fetch(s1, i1)) != towlower(str_fetch(s2, i2)) )
	    goto nextmixed;

	return TRUE;
      nextmixed:;
      }
    }
  }

  return FALSE;
}


int
str_next_index(PceString s, int from, wint_t chr)
{ int i, n = s->s_size;

  if ( isstrA(s) )
  { charA *d = &s->s_textA[from];

    for(i=from; i<n; i++, d++)
      if ( *d == chr )
	return i;
  } else
  { charW *d = &s->s_textW[from];

    for(i=from; i<n; i++, d++)
      if ( (wint_t)*d == chr )
	return i;
  }

  return -1;
}


int
str_next_rindex(PceString s, int from, wint_t chr)
{ int i;

  if ( isstrA(s) )
  { charA *d = &s->s_textA[from];

    for(i=from; i >= 0; i--, d--)
      if ( *d == chr )
	return i;
  } else
  { charW *d = &s->s_textW[from];

    for(i=from; i >= 0; i--, d--)
      if ( (wint_t)*d == chr )
	return i;
  }

  return -1;
}


int
str_index(PceString s, wint_t chr)
{ return str_next_index(s, 0, chr);
}


int
str_rindex(PceString s, wint_t chr)
{ return str_next_rindex(s, s->s_size, chr);
}

/* count chr in [from,to) */

int
str_count_chr(PceString s, int from, int to, wint_t chr)
{ int i, count = 0;

  if ( isstrA(s) )
  { charA *d = &s->s_textA[from];

    for(i=from; i<to; i++, d++)
      if ( *d == chr )
	count++;
  } else
  { charW *d = &s->s_textW[from];

    for(i=from; i<to; i++, d++)
      if ( (wint_t)*d == chr )
	count++;
  }

  return count;
}


int
str_lineno(PceString s, int at)
{ return str_count_chr(s, 0, at, '\n') + 1;
}


wint_t
str_fetch(PceString s, int idx)
{ return s->s_iswide ? str_fetchW(s, idx)
		   : str_fetchA(s, idx) & 0xff;
}


int
str_store(PceString s, int idx, unsigned int chr)
{ return s->s_iswide ? str_storeW(s, idx, chr)
		   : str_storeA(s, idx, chr);
}

		 /*******************************
		 *	       UTIL		*
		 *******************************/

static void
str_from_char(PceString s, char c)
{ unsigned char *text = alloc(sizeof(char)*2);
  text[0] = c;
  text[1] = '\0';

  str_inithdr(s, FALSE);
  s->s_textA  = text;
  s->s_size     = 1;
}


static void
str_from_char16(PceString s, int c)
{ charW *text = alloc(sizeof(charW)*2);
  text[0] = c;
  text[1] = '\0';

  str_inithdr(s, TRUE);
  s->s_textW = text;
  s->s_size     = 1;
}


PceString
str_nl(PceString proto)
{ static string nl8;
  static string nl16;

  if ( !proto || !proto->s_iswide )
  { if ( !nl8.s_size )
      str_from_char(&nl8, '\n');

    return &nl8;
  } else
  { if ( !nl16.s_size )
      str_from_char16(&nl16, '\n');

    return &nl16;
  }
}


PceString
str_spc(PceString proto)
{ static string spc8;
  static string spc16;

  if ( !proto || !proto->s_iswide )
  { if ( !spc8.s_size )
      str_from_char(&spc8, ' ');

    return &spc8;
  } else
  { if ( !spc16.s_size )
      str_from_char16(&spc16, ' ');

    return &spc16;
  }
}


PceString
str_tab(PceString proto)
{ static string tab8;
  static string tab16;

  if ( !proto || !proto->s_iswide )
  { if ( !tab8.s_size )
      str_from_char(&tab8, '\t');

    return &tab8;
  } else
  { if ( !tab16.s_size )
      str_from_char16(&tab16, '\t');

    return &tab16;
  }
}


void
str_strip(PceString s)
{ int size = s->s_size;

  if ( isstrA(s) )
  { charA *f = s->s_textA;
    charA *t = s->s_textA;
    charA *e = &s->s_textA[size];

    while( f < e && iswspace(*f) )	/* ISO-Latin-1 */
      f++;

    do
    { while( f < e && !iswspace(*f) )
	*t++ = *f++;
      while( f < e && iswspace(*f) )
	f++;
      if ( f < e )
	*t++ = ' ';
    } while( f < e );
    s->s_size = t - s->s_textA;
  } else
  { charW *f = s->s_textW;
    charW *t = s->s_textW;
    charW *e = &s->s_textW[size];

    while( f < e && iswspace(*f) )
      f++;

    do
    { while( f < e && !iswspace(*f) )
	*t++ = *f++;
      while( f < e && iswspace(*f) )
	f++;
      if ( f < e )
	*t++ = ' ';
    } while( f < e );
    s->s_size = t - s->s_textW;
  }
}


int
str_common_length(PceString s1, PceString s2)
{ int i = 0;
  int size = min(s1->s_size, s2->s_size);

  if ( s1->s_iswide == s2->s_iswide )
  { if ( isstrA(s1) )
    { charA *t1 = s1->s_textA;
      charA *t2 = s2->s_textA;

      while( i < size && *t1++ == *t2++ )
	i++;
    } else
    { charW *t1 = s1->s_textW;
      charW *t2 = s2->s_textW;

      while( i < size && *t1++ == *t2++ )
	i++;
    }
  }

  return i;
}


int
str_icase_common_length(PceString s1, PceString s2)
{ int i = 0;
  int size = min(s1->s_size, s2->s_size);

  if ( s1->s_iswide == s2->s_iswide )
  { if ( isstrA(s1) )
    { charA *t1 = s1->s_textA;
      charA *t2 = s2->s_textA;

      while( i < size && tolower(*t1) == tolower(*t2) )
	i++, t1++, t2++;
    } else
    { charW *t1 = s1->s_textW;
      charW *t2 = s2->s_textW;

      while( i < size && towlower(*t1) == towlower(*t2) )
	i++, t1++, t2++;
    }
  }

  return i;
}


		 /*******************************
		 *	 TEMPORARY STRINGS	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Temporary strings are  designed  to   get  character  data  code-by-code
without knowing the size in advance or wether or not the data fits in an
ISO Latin-1 string or not.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

tmp_string *
str_tmp_init(tmp_string *tmp)
{ str_inithdr(&tmp->s, FALSE);
  tmp->s.s_textA = tmp->buffer;
  tmp->allocated = sizeof(tmp->buffer);

  return tmp;
}


wint_t
str_tmp_put(tmp_string *tmp, wint_t c)
{ PceString s = &tmp->s;

  if ( c > 0xff && !s->s_iswide )
  { if ( s->s_textA == tmp->buffer &&
	 s->s_size*sizeof(charW) < sizeof(tmp->buffer) )
    { charA b2[sizeof(tmp->buffer)];
      const charA *f = b2;
      const charA *e = &f[s->s_size];
      charW *t = s->s_textW;

      memcpy(b2, tmp->buffer, s->s_size);
      while(f<e)
	*t++ = *f++;
      tmp->allocated /= sizeof(charW);
    } else
    { charW *new = pceMalloc(tmp->allocated * sizeof(charW));
      const charA *f = tmp->buffer;
      const charA *e = &f[s->s_size];
      charW *t = new;

      while(f<e)
	*t++ = *f++;

      if ( s->s_textA != tmp->buffer )
	pceFree(s->s_textA);
      s->s_textW = new;
    }
    s->s_iswide = TRUE;
  }
  if ( s->s_size >= tmp->allocated )
  { if ( s->s_textA == tmp->buffer )
    { long len = tmp->allocated*2;

      if ( isstrA(s) )
      { s->s_textA = pceMalloc(len);

	memcpy(s->s_textA, tmp->buffer, sizeof(tmp->buffer));
      } else
      { s->s_textW = pceMalloc(len*sizeof(charW));

	memcpy(s->s_textA, tmp->buffer, sizeof(tmp->buffer));
      }

      tmp->allocated = len;
    } else
    { tmp->allocated *= 2;

      if ( isstrA(s) )
	s->s_textA = pceRealloc(s->s_textA, tmp->allocated);
      else
	s->s_textW = pceRealloc(s->s_textW, tmp->allocated*sizeof(charW));
    }
  }

  if ( !s->s_iswide )
    s->s_textA[s->s_size++] = c;
  else
    s->s_textW[s->s_size++] = c;

  return c;
}


void
str_tmp_done(tmp_string *tmp)
{ if ( tmp->s.s_textA != tmp->buffer )
    pceFree(tmp->s.s_textA);
}
