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

#ifndef XPCE_KER_SELF_H_INCLUDED
#define XPCE_KER_SELF_H_INCLUDED

/* Functions defined in ker/self.c */

COMMON(status)	formatPcev(Pce pce, CharArray fmt, int argc, Any *argv);
COMMON(Name)	getOsErrorPce(Pce pce);
COMMON(status)	catchErrorPce(Pce pce, Any ids);
COMMON(status)	catchPopPce(Pce pce);
COMMON(status)	catchedErrorPce(Pce pce, Name id);
COMMON(Name)	getEnvironmentVariablePce(Pce pce, Name name);
COMMON(status)	debuggingPce(Pce pce, BoolObj val);
COMMON(status)	exceptionPcev(Pce pce, Name name, int argc, Any *argv);
COMMON(status)	exceptionPce(Pce pce, Name kind, ...);
COMMON(int)	getFileDesCount(void);
COMMON(Name)	getHostnamePce(Pce pce);
COMMON(status)	catchErrorSignalsPce(Pce pce, BoolObj val);
COMMON(status)	resetPce(Pce pce);
COMMON(Any)	getObjectFromReferencePce(Pce pce, Any ref);
COMMON(status)	makeClassPce(Class class);
COMMON(export status) pceInitialise(int handles, const char *home, const char *appdata,
				    int argc, char **argv);

#endif /*XPCE_KER_SELF_H_INCLUDED*/
