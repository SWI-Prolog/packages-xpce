/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org
    Copyright (c)  2005-2025, University of Amsterdam
			      SWI-Prolog Solutions b.v.
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

#ifndef XPCE_TXT_I18N_H_INCLUDED
#define XPCE_TXT_I18N_H_INCLUDED

/* Functions defined in txt/i18n.c */

COMMON(wchar_t *) charArrayToWC(CharArray ca, size_t *len);
COMMON(charW *) charArrayToCharW(CharArray ca, size_t *len);
COMMON(char *)	stringToUTF8(PceString s, size_t *len);
COMMON(char *)	charArrayToUTF8(CharArray ca);
COMMON(char *)	charArrayToMB(CharArray ca);
COMMON(char *)	nameToMB(Name nm);
COMMON(char *)	nameToUTF8(Name nm);
COMMON(wchar_t *) nameToWC(Name nm, size_t *len);
COMMON(Name)	UTF8ToName(const char *utf8);
COMMON(Name)	MBToName(const char *mb);
COMMON(Name)	WCToName(const wchar_t *wc, size_t len);
COMMON(StringObj) WCToString(const wchar_t *wc, size_t len);
COMMON(StringObj) UTF8ToString(const char *utf8);
COMMON(Name)	FNToName(const char *name);
COMMON(char *)	charArrayToFN(CharArray ca);
COMMON(char *)	stringToFN(PceString s);
COMMON(char *)	nameToFN(Name nm);

#endif /*XPCE_TXT_I18N_H_INCLUDED*/
