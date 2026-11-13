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

#ifndef XPCE_GRA_GRAPHICAL_H_INCLUDED
#define XPCE_GRA_GRAPHICAL_H_INCLUDED

#include <h/graphics.h>

/* Functions defined in gra/graphical.c */

struct d_buffer;

COMMON(status)	svgBufferGraphical(Graphical gr, struct d_buffer *buf);
COMMON(status)	initialiseGraphical(Any obj, Int x, Int y, Int w, Int h);
COMMON(status)	unlinkGraphical(Graphical gr);
COMMON(status)	copyGraphical(Any obj1, Any obj2);
COMMON(status)	DeviceGraphical(Any obj, Device dev);
COMMON(status)	deviceGraphical(Any obj, Device dev);
COMMON(status)	reparentGraphical(Graphical gr);
COMMON(status)	DisplayedGraphical(Any obj, BoolObj val);
COMMON(status)	displayedGraphical(Any obj, BoolObj val);
COMMON(BoolObj)	getIsDisplayedGraphical(Graphical gr, Device dev);
COMMON(status)	initialiseDeviceGraphical(Any obj, int *x, int *y, int *w, int *h);
COMMON(status)	offsetDeviceGraphical(Any obj, int *x, int *y);
COMMON(Area)	getAbsoluteAreaGraphical(Graphical gr, Device device);
COMMON(Graphical) getRootGraphical(Graphical gr);
COMMON(PceWindow) getWindowGraphical(Graphical gr);
COMMON(bool)	hasInputFocusGraphical(Graphical gr);
COMMON(FrameObj) getFrameGraphical(Graphical gr);
COMMON(DisplayObj) getDisplayGraphical(Graphical gr);
COMMON(Application) getApplicationGraphical(Graphical gr);
COMMON(Device)	getCommonDeviceGraphical(Graphical gr1, Graphical gr2);
COMMON(int)	get_extension_margin_graphical(Graphical gr);
COMMON(status)	changedAreaGraphical(Any obj, Int x, Int y, Int w, Int h);
COMMON(status)	changedImageGraphical(Any obj, Int x, Int y, Int w, Int h);
COMMON(status)	changedEntireImageGraphical(Any obj);
COMMON(status)	redrawGraphical(Graphical gr, Area a);
COMMON(status)	requestComputeGraphical(Any obj, Any val);
COMMON(status)	ComputeGraphical(Any obj);
COMMON(status)	RedrawArea(Any obj, Area area);
COMMON(status)	paintSelectedGraphical(Graphical gr);
COMMON(status)	RedrawAreaGraphical(Any obj, Area area);
COMMON(status)	exposeGraphical(Any obj1, Any obj2);
COMMON(status)	setGraphical(Any obj, Int x, Int y, Int w, Int h);
COMMON(status)	doSetGraphical(Any obj, Int x, Int y, Int w, Int h);
COMMON(status)	requestGeometryGraphical(Any gr, Int x, Int y, Int w, Int h);
COMMON(status)	geometryGraphical(Any obj, Int x, Int y, Int w, Int h);
COMMON(status)	xGraphical(Graphical gr, Int x);
COMMON(status)	yGraphical(Graphical gr, Int y);
COMMON(status)	heightGraphical(Graphical gr, Int h);
COMMON(status)	positionGraphical(Graphical gr, Point pos);
COMMON(status)	centerGraphical(Graphical gr, Point pos);
COMMON(status)	relativeMoveGraphical(Graphical gr, Point pos);
COMMON(status)	init_resize_graphical(Any obj, Num xfactor, Num yfactor, Point origin, float *xf, float *yf, int *ox, int *oy);
COMMON(Area)	getAreaGraphical(Graphical gr);
COMMON(Int)	getXGraphical(Graphical gr);
COMMON(Int)	getYGraphical(Graphical gr);
COMMON(Int)	getWidthGraphical(Graphical gr);
COMMON(Int)	getHeightGraphical(Graphical gr);
COMMON(Int)	getLeftSideGraphical(Graphical gr);
COMMON(Int)	getRightSideGraphical(Graphical gr);
COMMON(Int)	getBottomSideGraphical(Graphical gr);
COMMON(Point)	getPositionGraphical(Graphical gr);
COMMON(status)	get_absolute_xy_graphical(Graphical gr, Device *dev, Int *X, Int *Y);
COMMON(Int)	getAbsoluteXGraphical(Any gr, Device dev);
COMMON(Int)	getAbsoluteYGraphical(Any gr, Device dev);
COMMON(Point)	getFramePositionGraphical(Graphical gr);
COMMON(Size)	getSizeGraphical(Graphical gr);
COMMON(status)	appendDialogItemNetworkDevice(Device dev, Graphical gr1);
COMMON(status)	aboveGraphical(Graphical gr1, Graphical gr2);
COMMON(status)	belowGraphical(Graphical gr1, Graphical gr2);
COMMON(status)	rightGraphical(Graphical gr1, Graphical gr2);
COMMON(status)	leftGraphical(Graphical gr1, Graphical gr2);
COMMON(status)	referenceGraphical(Graphical gr, Point ref);
COMMON(status)	penGraphical(Graphical gr, Int pen);
COMMON(status)	shadowGraphical(Graphical gr, Any s);
COMMON(status)	fillGraphical(Graphical gr, Any pattern);
COMMON(status)	fillOffsetGraphical(Graphical gr, Point pattern);
COMMON(status)	colourGraphical(Graphical gr, Any c);
COMMON(Any)	getDisplayColourGraphical(Graphical gr);
COMMON(Handle)	getHandleGraphical(Graphical gr, Name name);
COMMON(Point)	getHandlePositionGraphical(Graphical gr, Name name, Device dev);
COMMON(Chain)	getHandlesGraphical(Graphical gr, Point pos, Name kind, Int distance);
COMMON(status)	activeGraphical(Graphical gr, BoolObj val);
COMMON(status)	focusCursorGraphical(Graphical gr, CursorObj cursor);
COMMON(status)	focusGraphical(Graphical gr, Recogniser recogniser, CursorObj cursor, Name button);
COMMON(status)	updateConnectionsGraphical(Graphical gr, Int level);
COMMON(status)	connectGraphical(Graphical gr, Graphical gr2, Link link, Name from, Name to);
COMMON(status)	attachConnectionGraphical(Graphical gr, Connection c);
COMMON(status)	detachConnectionGraphical(Graphical gr, Connection c);
COMMON(status)	connectedGraphical(Graphical gr, Graphical gr2, Link link, Name from, Name to);
COMMON(status)	disconnectGraphical(Graphical gr, Graphical gr2, Link link, Name from, Name to);
COMMON(status)	eventGraphical(Any obj, EventObj ev);
COMMON(BoolObj)	getKeyboardFocusGraphical(Graphical gr);
COMMON(status)	generateEventGraphical(Graphical gr, Name name);
COMMON(status)	inEventAreaGraphical(Graphical gr, Int xc, Int yc);
COMMON(Chain)	getAllRecognisersGraphical(Any obj, BoolObj create);
COMMON(status)	assignGraphical(Any obj, Name slot, Any value);
COMMON(status)	flashGraphical(Graphical gr, Area a, Int time);
COMMON(status)	alertGraphical(Graphical gr);
COMMON(Node)	getNodeGraphical(Graphical gr);
COMMON(status)	pointerGraphical(Graphical gr, Point pos);
COMMON(Any)	getMasterGraphical(Graphical gr);
COMMON(status)	nameGraphical(Graphical gr, Name name);
COMMON(Any)	getContainedInGraphical(Graphical gr);
COMMON(status)	initialiseNewSlotGraphical(Graphical gr, Variable new);
COMMON(status)	clipGraphical(Graphical gr, Area a);
COMMON(status)	unclipGraphical(Graphical gr);
COMMON(status)	makeClassGraphical(Class class);

#endif /*XPCE_GRA_GRAPHICAL_H_INCLUDED*/
