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

#ifndef XPCE_MEN_DIALOGITEM_H_INCLUDED
#define XPCE_MEN_DIALOGITEM_H_INCLUDED

/* Functions defined in men/dialogitem.c */

COMMON(status)	createDialogItem(Any obj, Name name);
COMMON(status)	unlinkDialogItem(DialogItem di);
COMMON(void)	dia_text_label_size(CharArray label, FontObj font, int *w, int *h);
COMMON(void)	dia_draw_text_label(CharArray label, int acc, FontObj font, int x, int y, int w, int h, Name hadjust, Name vadjust, int flags);
COMMON(status)	RedrawLabelDialogItem(Any obj, int acc, int x, int y, int w, int h, Name hadjust, Name vadjust, int flags);
COMMON(status)	dia_label_size(Any obj, int *w, int *h, int *isimage);
COMMON(status)	labelDialogItem(DialogItem di, Any label);
COMMON(status)	labelFontDialogItem(DialogItem di, FontObj font);
COMMON(void)	r_pill(int x, int y, int w, int h, Any fill);
COMMON(void)	r_pill_outline(int x, int y, int w, int h);
COMMON(void)	r_chevron(double x, double y, double w, double pen);
COMMON(int)	hasInputFocusDialogItem(Any obj);
COMMON(status)	eventDialogItem(Any obj, EventObj ev);
COMMON(status)	changedDialogItem(Any obj);
COMMON(Point)	getReferenceDialogItem(Any obj);
COMMON(status)	modifiedDialogItem(Any di, BoolObj modified);
COMMON(status)	assignAccelerators(Chain objects, Name prefix, Name label_method);
COMMON(Any)	normaliseAccelerator(Any acc);
COMMON(void)	reassignAcceleratorsOf(Graphical gr);
COMMON(Any)	currentAccelerator(Any obj);
COMMON(void)	autoAccelerator(Any obj, Any acc);
COMMON(void)	markAccelerator(Any acc, unsigned char *taken);
COMMON(bool)	strongAccelerator(Any obj);
COMMON(status)	assignAcceleratorsSoft(Chain objects, Name prefix,
				       Name label_method, unsigned char *taken,
				       const unsigned char *soft);
COMMON(status)	makeClassDialogItem(Class class);

#endif /*XPCE_MEN_DIALOGITEM_H_INCLUDED*/
