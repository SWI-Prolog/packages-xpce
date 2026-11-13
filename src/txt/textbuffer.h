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

#ifndef XPCE_TXT_TEXTBUFFER_H_INCLUDED
#define XPCE_TXT_TEXTBUFFER_H_INCLUDED

/* Functions defined in txt/textbuffer.c */

COMMON(status)	changedTextBuffer(TextBuffer tb);
COMMON(status)	ChangedRegionTextBuffer(TextBuffer tb, Int start, Int end);
COMMON(status)	ChangedFragmentListTextBuffer(TextBuffer tb);
COMMON(status)	clearTextBuffer(TextBuffer tb);
COMMON(status)	insertFileTextBuffer(TextBuffer tb, Int where, SourceSink file, Int times);
COMMON(status)	insertTextBuffer(TextBuffer tb, Int where, CharArray ca, Int times);
COMMON(status)	CAppendTextBuffer(TextBuffer tb, const char *text);
COMMON(status)	appendTextBuffer(TextBuffer tb, CharArray ca, Int times);
COMMON(status)	deleteTextBuffer(TextBuffer tb, Int where, Int times);
COMMON(status)	saveTextBuffer(TextBuffer tb, SourceSink file, Int from, Int len);
COMMON(status)	CmodifiedTextBuffer(TextBuffer tb, BoolObj val);
COMMON(status)	characterTextBuffer(TextBuffer tb, Int where, Int c);
COMMON(status)	transposeTextBuffer(TextBuffer tb, Int f1, Int t1, Int f2, Int t2);
COMMON(status)	downcaseTextBuffer(TextBuffer tb, Int from, Int len);
COMMON(status)	upcaseTextBuffer(TextBuffer tb, Int from, Int len);
COMMON(status)	capitaliseTextBuffer(TextBuffer tb, Int from, Int len);
COMMON(Int)	getScanTextBuffer(TextBuffer tb, Int from, Name unit, Int amount, Name az);
COMMON(StringObj) getContentsTextBuffer(TextBuffer tb, Int from, Int length);
COMMON(status)	parsep_line_textbuffer(TextBuffer tb, intptr_t here);
COMMON(intptr_t) scan_textbuffer(TextBuffer tb, intptr_t from, Name unit, intptr_t amount, int az);
COMMON(Int)	getMatchingBracketTextBuffer(TextBuffer tb, Int idx, Int bracket);
COMMON(Int)	getSkipBlanksTextBuffer(TextBuffer tb, Int where, Name direction, BoolObj skipnl);
COMMON(Int)	getLineNumberTextBuffer(TextBuffer tb, Int i);
COMMON(ssize_t) find_textbuffer(TextBuffer tb, intptr_t here, PceString str, intptr_t times, char az, int ec, int wm);
COMMON(int)	match_textbuffer(TextBuffer, intptr_t, PceString, int, int);
COMMON(intptr_t) fill_line_textbuffer(TextBuffer tb, intptr_t here, intptr_t to, int sc, int rm, int justify);
COMMON(status)	sortTextBuffer(TextBuffer tb, Int from, Int to);
COMMON(intptr_t) count_lines_textbuffer(TextBuffer tb, intptr_t f, intptr_t t);
COMMON(int)	start_of_line_n_textbuffer(TextBuffer tb, int lineno);
COMMON(int)	fetch_textbuffer(TextBuffer tb, intptr_t where);
COMMON(status)	change_textbuffer(TextBuffer tb, intptr_t where, PceString s);
COMMON(status)	str_sub_text_buffer(TextBuffer tb, PceString s, intptr_t start, intptr_t len);
COMMON(status)	insert_textbuffer(TextBuffer tb, intptr_t where, intptr_t times, PceString s);
COMMON(status)	delete_textbuffer(TextBuffer tb, intptr_t where, intptr_t length);
COMMON(status)	makeClassTextBuffer(Class class);

#endif /*XPCE_TXT_TEXTBUFFER_H_INCLUDED*/
