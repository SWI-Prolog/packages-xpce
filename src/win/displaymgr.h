/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org/packages/xpce/
    Copyright (c)  1995-2026, University of Amsterdam
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

#ifndef XPCE_WIN_DISPLAYMGR_H_INCLUDED
#define XPCE_WIN_DISPLAYMGR_H_INCLUDED

/* Functions defined in win/displaymgr.c */

COMMON(status)	appendDisplayManager(DisplayManager dm, DisplayObj d);
COMMON(DisplayObj) getMemberDisplayManager(DisplayManager dm, Any name);
COMMON(DisplayObj) CurrentDisplay(Any obj);
COMMON(DisplayObj) getPrimaryDisplayManager(DisplayManager dm);
COMMON(status)	deleteDisplayManager(DisplayManager dm, DisplayObj d);
COMMON(status)	forwardFocusDisplayManager(Any focus);
COMMON(status)	RedrawDisplayManager(DisplayManager dm);
COMMON(status)	dispatchDisplayManager(DisplayManager dm, IOSTREAM *fd, Num timeout);
COMMON(DisplayManager) TheDisplayManager(void);
COMMON(status)	makeClassDisplayManager(Class class);

#endif /*XPCE_WIN_DISPLAYMGR_H_INCLUDED*/
