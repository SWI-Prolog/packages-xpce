/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  1985-2017, University of Amsterdam
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

#ifndef GLOBAL
#define GLOBAL extern
#endif

NewClass(syntax_table)
  Name		name;			/* Name of this table */
  Int		size;			/* Size of the table (256) */
  Regex		sentence_end;		/* End-Of-Sentence */
  Regex		paragraph_end;		/* End-Of-Pargraph */
  Name		qq_start;		/* Quasi quotation start */
  Name		qq_end;			/* Quasi quotation end */
  BoolObj	prolog;			/* Is Prolog syntax */
  unsigned short *table;		/* Type-flags */
  unsigned char  *context;		/* Context info */
End;

#define makeCFlag(n)	(1 << (n-1))

#define LC	makeCFlag(1)		/* Lower case letter */
#define UC	makeCFlag(2)		/* Upper case letter */
#define DI	makeCFlag(3)		/* Digit */
#define WS	makeCFlag(4)		/* Word separator (in symbol) */
#define SY	makeCFlag(5)		/* Other symbol-characters */
#define OB	makeCFlag(6)		/* Open Brace (context: close) */
#define CB	makeCFlag(7)		/* Close Brace (context: open) */
#define EL	makeCFlag(8)		/* Ends Line */
#define BL	makeCFlag(9)		/* Blank */
#define QT	makeCFlag(10)		/* String quote (context: escape) */
#define PU	makeCFlag(11)		/* Punctuation */
#define EB	makeCFlag(12)		/* End Buffer/string */
#define CS	makeCFlag(13)		/* Comment-start (context: 2nd char) */
#define CE	makeCFlag(14)		/* Comment-end (context: 2nd char) */
#define CT	makeCFlag(15)		/* Control character */
#define XD	makeCFlag(16)		/* Xdigit */

#define AN	(LC|UC|DI|WS|SY)	/* General symbol-character */

		/********************************
		*       CHARACTER TYPES		*
		********************************/

#define streq(s, t)	((s) && (t) && (strcmp((s), (t)) == 0))

#define UNICODE_MAX		(0x10FFFF)
#define META_OFFSET		(1L<<16)

#define EOS	0			/* end of string */
#define ESC	27			/* char escape */
#define TAB	9			/* tab character */
#define DEL	127			/* delete character */
#define Control(x) (x & 037)
#define Meta(x)    (x + META_OFFSET)

GLOBAL SyntaxTable DefaultSyntaxTable;	/* Systems default table */
extern unsigned short char_flags[];	/* Initial flags table */
extern unsigned short syntax_spec_code[]; /* Char --> syntax (for \sC regex) */
extern unsigned char  char_context[];	/* Initial context table */

#define Is8char(c)		(((c) & ~0xff) == 0)
#define HasSyntax(c, f)		(Is8char(c) && \
				 (char_flags[(unsigned int)(c)] & (f)))
#define HasSyntax8(c, f)	((char_flags[(unsigned int)(c)] & (f)))

#ifndef iscsym
#define iscsym(c)		(iswalnum(c) || c == '_')
#endif
#define isopenbrace(c)		HasSyntax((c), OB)
#define isclosebrace(c)		HasSyntax((c), CB)
#define isendsline(c)		HasSyntax((c), EL)
#define isquote(c)		HasSyntax((c), QT)
#define issymbol(c)		HasSyntax((c), SY)
#define iswordsep(c)		HasSyntax((c), WS)

#define ischtype(c, tp)		HasSyntax((c), tp)

#define ismatching(c1, c2)      (Is8char(c1) && \
				 (char_context[(unsigned int)(c1)] == (c2))
#define isstringescape(q, e)	(Is8char(q) && \
				 char_context[((unsigned int))(q)] == (e))

#if !defined(_GNU_SOURCE) && !defined(isblank)
#define isblank(c)		((c) == ' ' || (c) == '\t')
#endif

		/********************************
		*         TABLE VERSIONS	*
		********************************/

#define THasSyntax(t, c, f)	(Is8char(c) && \
				 ((t)->table[(unsigned int)(c)] & (f)))

#define tislower(t, c)		THasSyntax(t, c, LC)
#define tisupper(t, c)		THasSyntax(t, c, UC)
#define tisdigit(t, c)		THasSyntax(t, c, DI)
#define tisopenbrace(t, c)	THasSyntax(t, c, OB)
#define tisclosebrace(t, c)	THasSyntax(t, c, CB)
#define tisendsline(t, c)	THasSyntax(t, c, EL)
#define tisblank(t, c)		THasSyntax(t, c, BL)
#define tislayout(t, c)		THasSyntax(t, c, BL|EL)
#define tisquote(t, c)		THasSyntax(t, c, QT)
#define tissymbol(t, c)		THasSyntax(t, c, SY)
#define tiswordsep(t, c)	THasSyntax(t, c, WS)
#define tisprint(t, c)		!THasSyntax(t, c, CT)

#define tisalnum(t, c)		THasSyntax(t, c, AN)
#define tisletter(t, c)		THasSyntax(t, c, LC|UC)
#define tischtype(t, c, tp)	THasSyntax(t, c, (tp))

#define tismatching(t, c1, c2)  (Is8char(c1) && (t)->context[c1] == (c2))
#define tisstringescape(t,q,e)	(Is8char(q) && (t)->context[q] == (e))

#define tiscommentstart(t, c)	(THasSyntax(t, c, CS) && \
				 !(t)->context[(unsigned int)(c)])
#define tiscommentend(t, c)	(THasSyntax(t, c, CE) && \
				 !(t)->context[(unsigned int)(c)])
#define tiscommentstart1(t, c)	(THasSyntax(t, c, CS) && \
				 ((t)->context[(unsigned int)(c)] & 1))
#define tiscommentend1(t, c)	(THasSyntax(t, c, CE) && \
				 ((t)->context[(unsigned int)(c)] & 4))
#define tiscommentstart2(t, c)	(THasSyntax(t, c, CS) && \
				 ((t)->context[(unsigned int)(c)] & 2))
#define tiscommentend2(t, c)	(THasSyntax(t, c, CE) && \
				 ((t)->context[(unsigned int)(c)] & 8))


		/********************************
		*     HOST-LANGUAGE SYMBOLS	*
		********************************/

GLOBAL struct
{ int	uppercase;			/* keywords mapped to uppercase */
  char	word_separator;			/* current word separator */
} syntax;
