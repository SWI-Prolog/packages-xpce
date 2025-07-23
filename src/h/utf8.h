/*  Part of SWI-Prolog

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2005-2011, University of Amsterdam
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

#ifndef UTF8_H_INCLUDED
#define UTF8_H_INCLUDED

#define UTF8_MALFORMED_REPLACEMENT 0xfffd

#define ISUTF8_MB(c) ((unsigned)(c) >= 0xc0 && (unsigned)(c) <= 0xfd)

#define ISUTF8_CB(c)  (((c)&0xc0) == 0x80) /* Is continuation byte */
#define ISUTF8_FB2(c) (((c)&0xe0) == 0xc0)
#define ISUTF8_FB3(c) (((c)&0xf0) == 0xe0)
#define ISUTF8_FB4(c) (((c)&0xf8) == 0xf0)
#define ISUTF8_FB5(c) (((c)&0xfc) == 0xf8)
#define ISUTF8_FB6(c) (((c)&0xfe) == 0xfc)

#define UTF8_FBN(c) (!(c&0x80)     ? 0 : \
		     ISUTF8_FB2(c) ? 1 : \
		     ISUTF8_FB3(c) ? 2 : \
		     ISUTF8_FB4(c) ? 3 : \
		     ISUTF8_FB5(c) ? 4 : \
		     ISUTF8_FB6(c) ? 5 : -1)
#define UTF8_FBV(c,n) ( n == 0 ? c : (c & ((0x01<<(6-n))-1)) )

#define F_UTF8_GET_CHAR pce_utf8_get_char
#define F_UTF8_PUT_CHAR pce_utf8_put_char
#define F_UTF8_STRLEN   pce_utf8_strlen
#define F_UTF8_ENCLENW	pce_utf8_enclenW
#define F_UTF8_ENCLENA	pce_utf8_enclenA

#define utf8_get_char(in, chr) \
	(*(in) & 0x80 ? F_UTF8_GET_CHAR(in, chr) \
		      : (*(chr) = *(in), (char *)(in)+1))
#define utf8_put_char(out, chr) \
	(chr < 0x80 ? out[0]=chr, out+1 : F_UTF8_PUT_CHAR(out, chr))
#define pce_utf8_strlen(s, len) F_UTF8_STRLEN(s, len)
#define pce_utf8_enclenW(s, len) F_UTF8_ENCLENW(s, len)
#define pce_utf8_enclenA(s, len) F_UTF8_ENCLENA(s, len)

extern char *F_UTF8_GET_CHAR(const char *in, int *chr);
extern char *F_UTF8_PUT_CHAR(char *out, int chr);

extern size_t F_UTF8_STRLEN(const char *s, size_t len);
extern size_t F_UTF8_ENCLENW(const wchar_t *s, size_t len);
extern size_t F_UTF8_ENCLENA(const char *s, size_t len);




		 /*******************************
		 *	      UTF-16		*
		 *******************************/

#include <stddef.h>			/* get wchar_t */

/* See https://en.wikipedia.org/wiki/UTF-16#Examples */

#define IS_UTF16_LEAD(c)      ((c) >= 0xD800 && (c) <= 0xDBFF)
#define IS_UTF16_TRAIL(c)     ((c) >= 0xDC00 && (c) <= 0xDFFF)
#define IS_UTF16_SURROGATE(c) ((c) >= 0xD800 && (c) <= 0xDFFF)
#define VALID_CODE_POINT(c)   ((c) >= 0 && (c) <= UNICODE_MAX && !IS_UTF16_SURROGATE(c))

static inline int
utf16_decode(int lead, int trail)
{ int l = (lead-0xD800) << 10;
  int t = (trail-0xDC00);

  return l+t+0x10000;
}

static inline void
utf16_encode(int c, int *lp, int *tp)
{ c -= 0x10000;
  *lp = (c>>10)+0xD800;
  *tp = (c&0X3FF)+0xDC00;
}

static inline wchar_t*
utf16_put_char(wchar_t *out, int chr)
{ if ( chr <= 0xffff )
  { *out++ = chr;
  } else
  { int l, t;

    utf16_encode(chr, &l, &t);
    *out++ = l;
    *out++ = t;
  }

  return out;
}

static inline wchar_t*
put_wchar(wchar_t *out, int chr)
{
#if SIZEOF_WCHAR_T == 2
  return utf16_put_char(out, chr);
#else
  *out++ = chr;
  return out;
#endif
}

static inline const wchar_t*
get_wchar(const wchar_t *in, int *chr)
{
#if SIZEOF_WCHAR_T == 2
  int c = *in++;
  if ( IS_UTF16_LEAD(c) && IS_UTF16_TRAIL(in[0]) )
  { *chr = utf16_decode(c, in[0]);
    in++;
  } else
  { *chr = c;
  }
  return in;
#else
  *chr = *in++;
  return in;
#endif
}

#endif /*UTF8_H_INCLUDED*/
