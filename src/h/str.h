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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

#ifndef _STR_H_INCLUDED
#define _STR_H_INCLUDED

#include <wchar.h>
#include <wctype.h>
#include <ctype.h>

#undef charA				/* from pce-include.h */

typedef unsigned char charA;		/* 8-bit character */
typedef wchar_t       charW;		/* wide character */

#define STR_SIZE_BITS 30
#define STR_MAX_SIZE ((1L<<STR_SIZE_BITS)-1)

typedef struct _string
{ union
  { struct
    { unsigned	size : STR_SIZE_BITS;	/* size indication (512 MB) */
      unsigned	iswide : 1;		/* char- or wide characters */
      unsigned	readonly : 1;		/* storage is externally managed */
    } f;
    unsigned mask;
  } hdr;
  union
  { charA *	textA;
    charW *	textW;
  } text_union;
} string;

#define s_text		text_union.textA
#define s_textA		text_union.textA
#define s_textW		text_union.textW
#define s_size		hdr.f.size
#define s_iswide	hdr.f.iswide
#define s_readonly	hdr.f.readonly

#define isstrA(s) ((s)->s_iswide == 0)	/* 8-bit string */
#define isstrW(s) ((s)->s_iswide == 1)	/* 16-bit string */

#define str_len(s) ((s)->s_size)	/* length of the string */
#define str_wsize(s) ((((s)->s_iswide \
	? (s)->size * sizeof(charW) \
	: (s)->size) + sizeof(wint_t) - 1) / sizeof(wint_t))
#define str_fetchA(s, i)	(s->s_textA[(i)])
#define str_fetchW(s, i)	(s->s_textW[(i)])
#define str_storeA(s, i, c)	(s->s_textA[(i)] = (charA)(c))
#define str_storeW(s, i, c)	(s->s_textW[(i)] = (charW)(c))

#define str_cphdr(t, f) do { (t)->hdr.mask = (f)->hdr.mask; \
			   } while(0)
#define str_inithdr(s, w) do { (s)->hdr.mask = 0; \
			       (s)->s_iswide = w; \
			     } while(0)

#define str_datasize(s) (isstrA(s) ? (s)->s_size : (s)->s_size * sizeof(charW))

#ifndef FALSE
#define FALSE 0
#define TRUE 1
#endif


		 /*******************************
		 *	    TMP STRINGS		*
		 *******************************/

typedef struct tmp_string
{ string s;				/* string itself */
  long   allocated;			/* allocated size */
  charA  buffer[1024];			/* initial buffer */
} tmp_string;


		 /*******************************
		 *	      ENCODING		*
		 *******************************/

typedef struct
{ charA newline;
  charA *tolower;
  charA *toupper;
} str_encodingA, *StrEncodingA;


typedef struct
{ charW newline;
  int min_byte1;
  int max_byte1;
  charW **tolower;
  charW **toupper;
} str_encodingW, *StrEncodingW;

#endif /*_STR_H_INCLUDED*/
