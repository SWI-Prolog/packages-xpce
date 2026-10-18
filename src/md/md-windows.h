/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org/packages/xpce/
    Copyright (c)  2007-2026, University of Amsterdam
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

/* Windows additions to the config.h that CMake generates (see md.h).
   CMake cannot know these, or finds a function that MinGW provides but
   that does not do what xpce expects of it on Windows.
*/

#define HAVE_UXNT_H 1			/* file names through uxnt.h */
#define HAVE_ACCESS 1			/* access() function */
#define O_CPLUSPLUS 1
#define export __declspec(dllexport)
#ifndef STDC_HEADERS
#define STDC_HEADERS 1
#endif

#undef HAVE_SELECT			/* only works on sockets */
#undef HAVE_SOCKET			/* no Unix domain sockets */
#undef HAVE_SHUTDOWN			/* winsock has no SHUT_RD/SHUT_WR */
#undef HAVE_MKSTEMP			/* uses /tmp; tempnam() knows better */
#undef HAVE_GETHOSTNAME			/* needs WSAStartup() */
#undef HAVE_POPEN

#undef PCE_OS
#ifdef WIN64
#define PCE_OS "win64"
#else
#define PCE_OS "win32"
#endif
