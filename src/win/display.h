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

#ifndef XPCE_WIN_DISPLAY_H_INCLUDED
#define XPCE_WIN_DISPLAY_H_INCLUDED

/* Functions defined in win/display.c */

COMMON(status)	dispatchDisplay(DisplayObj d);
COMMON(status)	bellDisplay(DisplayObj d, Int vol);
COMMON(Size)	getSizeDisplay(DisplayObj d);
COMMON(Int)	getWidthDisplay(DisplayObj d);
COMMON(Int)	getHeightDisplay(DisplayObj d);
COMMON(status)	confirmDisplay(DisplayObj d, Any client, CharArray title,
			       CharArray fmt, int argc, Any *argv);
COMMON(status)	informDisplay(DisplayObj d, Any client, CharArray title,
			      CharArray fmt, int argc, Any *argv);
COMMON(status)	busyCursorDisplay(DisplayObj d, CursorObj c, BoolObj block_events);
COMMON(status)	inspectDisplay(DisplayObj d, Graphical gr, EventObj ev);
COMMON(status)	makeClassDisplay(Class class);
COMMON(status)	hasVisibleFramesDisplay(DisplayObj d, BoolObj keep_alive);

#endif /*XPCE_WIN_DISPLAY_H_INCLUDED*/
