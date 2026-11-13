/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org/packages/xpce/
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

#ifndef XPCE_KER_GOODIES_H_INCLUDED
#define XPCE_KER_GOODIES_H_INCLUDED

/* Functions defined in ker/goodies.c */

COMMON(long)	rdouble(double f);
COMMON(int)	isqrt(long a);
COMMON(int)	distance(int x1, int y1, int x2, int y2);
COMMON(int)	rfloat(double f);
COMMON(double)	cstrtod(const char *in, char **end);
COMMON(double)	cwcstod(const wchar_t *in, wchar_t **end);
COMMON(double)	valNumShortest(Num n);
COMMON(char *)	formatNum(Num n, char *buf, size_t size);
COMMON(char *)	strcpyskip(char *t, char *f);
COMMON(status)	substr(register char *str, register char *sb);
COMMON(status)	prefixstr(char *s1, char *s2);
COMMON(status)	substr_ignore_case(register char *str, register char *sb);
COMMON(status)	prefixstr_ignore_case(char *s1, char *s2);
COMMON(status)	streq_ignore_case(char *s1, char *s2);
COMMON(char *)	upcasestr(char *s);
COMMON(char *)	downcasestr(char *s);
COMMON(void)	checkSummaryCharp(Name classname, Name name, char *s);
COMMON(Name)	characterName(Any chr);
COMMON(status)	writef_arguments(char *fm, va_list args, int *argc, Any *argv);
COMMON(void)	writef(char *fm, ...);
COMMON(status)	str_writefv(PceString s, CharArray format, int argc, const Any *argv);
COMMON(status)	str_writefv_markup(PceString s, CharArray format, int argc, const Any *argv);
COMMON(Int)	scanstr(char *str, char *fmt, Any *r);
COMMON(status)	sysPce(char *fm, ...);
COMMON(int64_t) mclock(void);
COMMON(int)	pceAssert(int expr, char *text, char *file, int line);
COMMON(void)	at_pce_exit(atexit_function f, int flags);
COMMON(int)	run_pce_exit_hooks(int rval);

#endif /*XPCE_KER_GOODIES_H_INCLUDED*/
