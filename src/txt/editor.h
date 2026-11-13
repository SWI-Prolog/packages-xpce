/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org/packages/xpce/
    Copyright (c)  1985-2025, University of Amsterdam
                              VU University Amsterdam
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

#ifndef XPCE_TXT_EDITOR_H_INCLUDED
#define XPCE_TXT_EDITOR_H_INCLUDED

/* Functions defined in txt/editor.c */

COMMON(status)	normaliseEditor(Editor e, Int start, Int end);
COMMON(Any)	ReceiverOfEditor(Editor e);
COMMON(status)	forwardModifiedEditor(Editor e, BoolObj val);
COMMON(status)	scrollToEditor(Editor e, Int pos, Int screenline);
COMMON(status)  scrollVerticalEditor(Editor e, Name dir, Name unit, Int amount);
COMMON(status)	selectionEditor(Editor e, Int from, Int to, Name status);
COMMON(Point)	getSelectionEditor(Editor e);
COMMON(StringObj) getSelectedEditor(Editor e);
COMMON(status)	formatEditor(Editor e, CharArray fmt, int argc, Any *argv);
COMMON(status)	clearEditor(Editor e);
COMMON(status)	backgroundEditor(Editor e, Any bg);
COMMON(status)	colourEditor(Editor e, Any c);
COMMON(status)	makeClassEditor(Class class);

#endif /*XPCE_TXT_EDITOR_H_INCLUDED*/
