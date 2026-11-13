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

#ifndef XPCE_UNX_FILE_H_INCLUDED
#define XPCE_UNX_FILE_H_INCLUDED

/* Functions defined in unx/file.c */

COMMON(Name)	expandFileName(Name in);
COMMON(Name)	getOsNameFile(FileObj f);
COMMON(status)	closeFile(FileObj f);
COMMON(status)	existsFile(FileObj f, BoolObj mustbefile);
COMMON(status)	sameOsPath(const char *s1, const char *s2);
COMMON(Name)	getAbsolutePathFile(FileObj f);
COMMON(status)	isAbsoluteFile(FileObj f);
COMMON(status)	doBOMFile(FileObj f);
COMMON(status)	openFile(FileObj f, Name mode, Name filter, CharArray extension);
COMMON(status)	removeFile(FileObj f);
COMMON(Name)	getBaseNameFile(FileObj f);
COMMON(status)	reportErrorFile(FileObj f);
COMMON(status)	checkErrorFile(FileObj f);
COMMON(status)	storeCharFile(FileObj f, int c);
COMMON(void)	putstdw(uint32_t w, IOSTREAM *fd);
COMMON(status)	storeWordFile(FileObj f, Any w);
COMMON(status)	storeDoubleFile(FileObj file, double f);
COMMON(status)	storeStringFile(FileObj f, PceString s);
COMMON(status)	storeNameFile(FileObj f, Name n);
COMMON(status)	storeIntFile(FileObj f, Int i);
COMMON(status)	findFile(FileObj f, CharArray path, Name mode);
COMMON(status)	makeClassFile(Class class);

#endif /*XPCE_UNX_FILE_H_INCLUDED*/
