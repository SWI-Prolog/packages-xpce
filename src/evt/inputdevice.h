/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org
    Copyright (c)  2026, SWI-Prolog Solutions b.v.
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

#ifndef XPCE_EVT_INPUTDEVICE_H_INCLUDED
#define XPCE_EVT_INPUTDEVICE_H_INCLUDED

/* Functions defined in evt/inputdevice.c */

COMMON(InputDevice) findInputDevice(Name kind, intptr_t id, const char *name);
COMMON(Chain)	getInputDevicesDisplay(DisplayObj d);
COMMON(status)	makeClassInputDevice(Class class);
COMMON(status)	makeClassEventDetail(Class class);
COMMON(status)	makeClassDropData(Class class);
COMMON(status)	makeClassWheelData(Class class);
COMMON(status)	makeClassKeyData(Class class);
COMMON(status)	makeClassPenState(Class class);
COMMON(status)	makeClassTouchPoint(Class class);
COMMON(status)	makeClassPinchData(Class class);
COMMON(status)	makeClassGamepad(Class class);
COMMON(status)	makeClassKeyboard(Class class);
COMMON(status)	makeClassGamepadInput(Class class);
COMMON(void)	removeInputDevice(InputDevice d);
COMMON(Chain)	getGamepadsDisplay(DisplayObj d);

#endif /*XPCE_EVT_INPUTDEVICE_H_INCLUDED*/
