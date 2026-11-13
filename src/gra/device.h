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

#ifndef XPCE_GRA_DEVICE_H_INCLUDED
#define XPCE_GRA_DEVICE_H_INCLUDED

#include <h/graphics.h>

/* Functions defined in gra/device.c */

COMMON(status)	initialiseDevice(Device dev);
COMMON(status)	unlinkDevice(Device dev);
COMMON(CursorObj) getDisplayedCursorDevice(Device dev);
COMMON(Chain)	getPointedObjectsDevice(Device dev, Any pos, Chain ch);
COMMON(status)	inspectDevice(Device dev, EventObj ev);
COMMON(status)	eventDevice(Any obj, EventObj ev);
COMMON(Button)	getDefaultButtonDevice(Device d);
COMMON(status)	advanceDevice(Device dev, Graphical gr, BoolObj propagate, Name direction);
COMMON(status)	requestComputeDevice(Device dev, Any val);
COMMON(status)	computeGraphicalsDevice(Device dev);
COMMON(status)	computeLayoutDevice(Device dev);
COMMON(status)	computeDevice(Any obj);
COMMON(status)	updateBoundingBoxDevice(Device dev, Int *od);
COMMON(status)	computeBoundingBoxDevice(Device dev);
COMMON(status)	EnterRedrawAreaDevice(Device dev, Area a, DeviceDrawContext ctx);
COMMON(void)	ExitRedrawAreaDevice(Device dev, Area a, DeviceDrawContext ctx);
COMMON(status)	RedrawAreaDevice(Device dev, Area a);
COMMON(status)	flashDevice(Device dev, Area a, Int time);
COMMON(status)	clearDevice(Device dev, Name how);
COMMON(status)	displayDevice(Any Dev, Any Gr, Point pos);
COMMON(status)	appendDevice(Device dev, Graphical gr);
COMMON(status)	subGraphical(Graphical gr, Graphical sub);
COMMON(status)	eraseDevice(Device dev, Graphical gr);
COMMON(status)	displayedGraphicalDevice(Device dev, Graphical gr, BoolObj val);
COMMON(status)	exposeDevice(Device dev, Graphical gr, Graphical gr2);
COMMON(status)	hideDevice(Device dev, Graphical gr, Graphical gr2);
COMMON(status)	swapGraphicalsDevice(Device dev, Graphical gr, Graphical gr2);
COMMON(status)	layoutDialogDevice(Device d, Size gap, Size bb, Size border);
COMMON(status)	appendDialogItemDevice(Device d, Graphical item, Name where);
COMMON(Graphical) getMemberDevice(Device dev, Name name);
COMMON(status)	updateConnectionsDevice(Device dev, Int level);
COMMON(status)	geometryDevice(Device dev, Int x, Int y, Int w, Int h);
COMMON(status)	makeClassDevice(Class class);

#endif /*XPCE_GRA_DEVICE_H_INCLUDED*/
