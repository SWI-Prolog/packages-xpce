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

#ifndef XPCE_TXT_UNDO_H_INCLUDED
#define XPCE_TXT_UNDO_H_INCLUDED

/* Functions defined in txt/undo.c */

COMMON(void)	destroyUndoBuffer(UndoBuffer ub);
COMMON(Int)	getUndoTextBuffer(TextBuffer tb);
COMMON(status)	undoTextBuffer(TextBuffer tb);
COMMON(status)	undoBufferSizeTextBuffer(TextBuffer tb, Int size);
COMMON(status)	markUndoTextBuffer(TextBuffer tb);
COMMON(status)	resetUndoTextBuffer(TextBuffer tb);
COMMON(status)	checkpointUndoTextBuffer(TextBuffer tb);
COMMON(void)	register_insert_textbuffer(TextBuffer tb, size_t where, size_t len);
COMMON(void)	register_delete_textbuffer(TextBuffer tb, size_t where, size_t len);
COMMON(void)	register_change_textbuffer(TextBuffer tb, size_t where, size_t len);
COMMON(status)	makeClassTextChange(Class class);

#endif /*XPCE_TXT_UNDO_H_INCLUDED*/
