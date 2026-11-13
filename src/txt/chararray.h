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

#ifndef XPCE_TXT_CHARARRAY_H_INCLUDED
#define XPCE_TXT_CHARARRAY_H_INCLUDED

/* Functions defined in txt/chararray.c */

COMMON(status)	initialiseCharArray(CharArray n, CharArray value);
COMMON(Name)	getValueCharArray(CharArray n);
COMMON(status)	equalCharArray(CharArray n1, CharArray n2, BoolObj ign_case);
COMMON(status)	prefixCharArray(CharArray n1, CharArray n2, BoolObj ign_case);
COMMON(status)	suffixCharArray(CharArray n, CharArray s, BoolObj ign_case);
COMMON(status)	isWideCharArray(Any s);
COMMON(CharArray) getCopyCharArray(CharArray n);
COMMON(CharArray) getCapitaliseCharArray(CharArray n);
COMMON(CharArray) getLabelNameCharArray(CharArray n);
COMMON(CharArray) getDowncaseCharArray(CharArray n);
COMMON(CharArray) getAppendCharArray(CharArray n1, CharArray n2);
COMMON(CharArray) getDeleteSuffixCharArray(CharArray n, CharArray s);
COMMON(CharArray) getEnsureSuffixCharArray(CharArray n, CharArray s);
COMMON(CharArray) getSubCharArray(CharArray n, Int start, Int end);
COMMON(Int)	getSizeCharArray(Any n);
COMMON(void)	initCharArrays(void);
COMMON(CharArray) CtoScratchCharArray(const char *s);
COMMON(CharArray) StringToScratchCharArray(const PceString s);
COMMON(void)	doneScratchCharArray(CharArray n);
COMMON(CharArray) CtoCharArray(char *s);
COMMON(status)	makeClassCharArray(Class class);

#endif /*XPCE_TXT_CHARARRAY_H_INCLUDED*/
