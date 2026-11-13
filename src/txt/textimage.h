/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org/packages/xpce/
    Copyright (c)  1985-2026, University of Amsterdam
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

#ifndef XPCE_TXT_TEXTIMAGE_H_INCLUDED
#define XPCE_TXT_TEXTIMAGE_H_INCLUDED

/* Functions defined in txt/textimage.c */

COMMON(status)	InsertTextImage(TextImage ti, Int where, Int amount);
COMMON(status)	ChangedRegionTextImage(TextImage ti, Int from, Int to);
COMMON(status)	ChangedEntireTextImage(TextImage ti);
COMMON(status)	get_character_box_textimage(TextImage ti, int index, int *x, int *y, int *w, int *h, int *b);
COMMON(Int)	getLinesTextImage(TextImage ti);
COMMON(Int)	getIndexTextImage(TextImage ti, EventObj ev);
COMMON(status)	computeTextImage(TextImage ti);
COMMON(status)	startTextImage(TextImage ti, Int start, Int skip);
COMMON(status)	centerTextImage(TextImage ti, Int position, Int screen_line);
COMMON(Int)	getStartTextImage(TextImage ti, Int line);
COMMON(status)	backgroundTextImage(TextImage ti, Any bg);
COMMON(status)	tabDistanceTextImage(TextImage ti, Int tab);
COMMON(status)	tabStopsTextImage(TextImage ti, Vector v);
COMMON(Int)	getViewTextImage(TextImage ti);
COMMON(status)	bubbleScrollBarTextImage(TextImage ti, ScrollBar sb);
COMMON(Int)	getScrollStartTextImage(TextImage ti, Name dir, Name unit, Int amount);
COMMON(Int)	getUpDownColumnTextImage(TextImage ti, Int here);
COMMON(Int)	getUpDownCursorTextImage(TextImage ti, Int here, Int updown, Int column);
COMMON(Int)	getBeginningOfLineCursorTextImage(TextImage ti, Int here);
COMMON(Int)	getEndOfLineCursorTextImage(TextImage ti, Int here);
COMMON(status)	ensureVisibleTextImage(TextImage ti, Int caret);
COMMON(status)	makeClassTextImage(Class class);

#endif /*XPCE_TXT_TEXTIMAGE_H_INCLUDED*/
