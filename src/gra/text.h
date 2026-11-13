/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org/packages/xpce/
    Copyright (c)  1985-2025, University of Amsterdam
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

#ifndef XPCE_GRA_TEXT_H_INCLUDED
#define XPCE_GRA_TEXT_H_INCLUDED

/* Functions defined in gra/text.c */

COMMON(void)	str_format(PceString out, const PceString in, const int width, const FontObj font);
COMMON(void)	str_one_line(PceString to, PceString from);
COMMON(status)	repaintText(TextObj t, int x, int y, int w, int h);
COMMON(Int)	get_pointed_text(TextObj t, int x, int y, int round);
COMMON(status)	transparentText(TextObj t, BoolObj val);
COMMON(status)	fontText(TextObj t, FontObj font);
COMMON(status)	borderText(TextObj t, Int border);
COMMON(status)	stringText(TextObj t, CharArray s);
COMMON(status)	showCaretText(TextObj t, Any val);
COMMON(status)	pasteText(TextObj t, Name buffer);
COMMON(status)	lengthText(TextObj t, Int l);
COMMON(status)	marginText(TextObj t, Int width, Name wrap);
COMMON(status)	makeClassText(Class class);

#endif /*XPCE_GRA_TEXT_H_INCLUDED*/
