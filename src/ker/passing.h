/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org/packages/xpce/
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

#ifndef XPCE_KER_PASSING_H_INCLUDED
#define XPCE_KER_PASSING_H_INCLUDED

/* Functions defined in ker/passing.c */

COMMON(status)	vm_send(Any receiver, Name selector, Class class, int argc, const Any argv []);
COMMON(Any)	vm_get(Any receiver, Name selector, Class class, int argc, const Any argv []);
COMMON(status)	sendSendMethod(SendMethod sm, Any receiver, int argc, const Any argv []);
COMMON(Any)	getGetGetMethod(GetMethod gm, Any receiver, int argc, const Any argv []);
COMMON(status)	sendv(Any receiver, Name selector, int argc, Any *argv);
COMMON(status)	qadSendv(Any r, Name selector, int ac, Any *av);
COMMON(Any)	getv(Any receiver, Name selector, int argc, Any *argv);
COMMON(Any)	qadGetv(Any r, Name selector, int ac, Any *av);
COMMON(status)	send(Any receiver, Name selector, ...);
COMMON(Any)	get(Any receiver, Name selector, ...);
COMMON(Any)	resolveSendMethodObject(Any obj, Class class, Name sel, Any *receiver);
COMMON(Any)	resolveGetMethodObject(Any obj, Class class, Name sel, Any *receiver);
COMMON(void)	setRedrawing(bool val);
COMMON(bool)	getRedrawing(void);

#endif /*XPCE_KER_PASSING_H_INCLUDED*/
