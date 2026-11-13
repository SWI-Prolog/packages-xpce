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

#ifndef XPCE_TXT_STRING_H_INCLUDED
#define XPCE_TXT_STRING_H_INCLUDED

/* Functions defined in txt/string.c */

COMMON(StringObj) StringToString(PceString s);
COMMON(StringObj) StringToTempString(PceString s);
COMMON(StringObj) CtoString(const char *s);
COMMON(StringObj) staticCtoString(const char *s);
COMMON(StringObj) CtoTempString(char *s);
COMMON(status)	initialiseStringv(StringObj str, CharArray fmt, int argc, Any *argv);
COMMON(status)	valueString(StringObj s1, CharArray s2);
COMMON(status)	insertCharacterString(StringObj str, Int chr, Int where, Int times);
COMMON(status)	upcaseString(StringObj s);
COMMON(status)	deleteString(StringObj str, Int start, Int length);
COMMON(status)	insertString(StringObj s1, Int n, CharArray s2);
COMMON(status)	str_insert_string(StringObj str, Int where, PceString s);
COMMON(StringObj) getSubString(StringObj n, Int start, Int end);
COMMON(status)	makeClassString(Class class);

#endif /*XPCE_TXT_STRING_H_INCLUDED*/
