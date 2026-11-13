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

#ifndef XPCE_EVT_EVENT_H_INCLUDED
#define XPCE_EVT_EVENT_H_INCLUDED

/* Functions defined in evt/event.c */

COMMON(void)	considerLocStillEvent(void);
COMMON(void)	freePointerStateDevice(InputDevice d);
COMMON(InputDevice) lastPointerDevice(void);
COMMON(double)	wheelStepsEvent(EventObj ev, Any rec, double unit);
COMMON(void)	setFrameEvent(EventObj ev, FrameObj fr);
COMMON(PceWindow) WindowOfLastEvent(void);
COMMON(void)	unlinkedWindowEvent(Any sw);
COMMON(unsigned long) LastEventTime(void);
COMMON(void)	setLastEventTime(unsigned long time);
COMMON(status)	isAEvent(EventObj e, Any id);
COMMON(status)	eventName(Name name);
COMMON(status)	allButtonsUpEvent(EventObj e);
COMMON(status)	isUpEvent(EventObj e);
COMMON(status)	isDownEvent(EventObj e);
COMMON(status)	isPopupEvent(EventObj e);
COMMON(Name)	getButtonEvent(EventObj e);
COMMON(status)	isDragEvent(EventObj ev);
COMMON(status)	hasModifierEvent(EventObj ev, Modifier m);
COMMON(Name)	getMulticlickEvent(EventObj e);
COMMON(Int)	getClickTimeEvent(EventObj e);
COMMON(Int)	getClickDisplacementEvent(EventObj e);
COMMON(status)	windowEvent(EventObj ev, PceWindow sw);
COMMON(status)	get_xy_event(EventObj ev, Any obj, BoolObj area, Int *rx, Int *ry);
COMMON(Point)	getPositionEvent(EventObj ev, Any obj);
COMMON(Point)	getAreaPositionEvent(EventObj ev, Graphical gr);
COMMON(Int)	getXEvent(EventObj ev, Any obj);
COMMON(Int)	getYEvent(EventObj ev, Any obj);
COMMON(status)	insideEvent(EventObj ev, Graphical gr);
COMMON(Int)	getDistanceEvent(EventObj ev1, EventObj ev2);
COMMON(Any)	getIdEvent(EventObj ev);
COMMON(Any)	getReceiverEvent(EventObj ev);
COMMON(status)	postNamedEvent(EventObj ev, Graphical obj, Recogniser rec, Name method);
COMMON(status)	postEvent(EventObj ev, Graphical obj, Recogniser rec);
COMMON(Any)	getMasterEvent(EventObj ev);
COMMON(DisplayObj) getDisplayEvent(EventObj ev);
COMMON(status)	mapWheelMouseEventAs(EventObj ev, Any rec, bool horizontal);
COMMON(status)	mapWheelMouseEvent(EventObj ev, Any rec);
COMMON(status)	makeClassEvent(Class class);

#endif /*XPCE_EVT_EVENT_H_INCLUDED*/
