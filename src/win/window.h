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

#ifndef XPCE_WIN_WINDOW_H_INCLUDED
#define XPCE_WIN_WINDOW_H_INCLUDED

#include <h/graphics.h>

/* Functions defined in win/window.c */

COMMON(status)	initialiseWindow(PceWindow sw, Name label, Size size, DisplayObj display);
COMMON(status)	createdWindow(PceWindow sw);
COMMON(Any)	tileManagerWindow(PceWindow sw);
COMMON(status)	grabPointerWindow(PceWindow sw, BoolObj val);
COMMON(status)	unlinkWindow(PceWindow sw);
COMMON(Chain)	addFixedGraphicalsWindow(PceWindow sw, Chain ch);
COMMON(status)	updatePositionWindow(PceWindow sw);
COMMON(status)	resizeWindow(PceWindow sw);
COMMON(void)	offset_window(PceWindow sw, int *x, int *y);
COMMON(void)	compute_window(PceWindow sw, int *x, int *y, int *w, int *h);
COMMON(status)	frame_offset_window(Any obj, FrameObj *fr, int *X, int *Y);
COMMON(void)	offset_windows(PceWindow w1, Any w2, int *X, int *Y);
COMMON(int)	is_service_window(Any from);
COMMON(status)	postEventWindow(PceWindow sw, EventObj ev);
COMMON(status)	typedWindow(PceWindow sw, EventId id, BoolObj delegate);
COMMON(status)	keyboardFocusWindow(PceWindow sw, Graphical gr);
COMMON(status)	focusWindow(PceWindow sw, Graphical gr, Recogniser recogniser, CursorObj cursor, Name button);
COMMON(status)	computeWindow(PceWindow sw);
COMMON(void)	changed_window(PceWindow sw, int x, int y, int w, int h, int clear);
COMMON(status)	redrawWindow(PceWindow sw, Area a);
COMMON(status)	pceRedrawWindow(PceWindow sw);
COMMON(status)	pointerWindow(PceWindow sw, Point pos);
COMMON(status)	focusCursorWindow(PceWindow sw, CursorObj cursor);
COMMON(status)	updateCursorWindow(PceWindow sw);
COMMON(status)	geometryWindow(PceWindow sw, Int X, Int Y, Int W, Int H);
COMMON(status)	requestGeometryWindow(PceWindow sw, Int X, Int Y, Int W, Int H);
COMMON(status)	get_display_position_window(PceWindow sw, int *X, int *Y);
COMMON(PceWindow) getUserWindow(PceWindow sw);
COMMON(status)	frameWindow(PceWindow sw, FrameObj frame);
COMMON(TileObj)	getTileWindow(PceWindow sw);
COMMON(FrameObj) getFrameWindow(PceWindow sw, BoolObj create);
COMMON(status)	flashWindow(PceWindow sw, Area a, Int time);
COMMON(status)	makeClassWindow(Class class);

#endif /*XPCE_WIN_WINDOW_H_INCLUDED*/
