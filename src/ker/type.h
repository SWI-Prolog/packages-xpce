/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org/packages/xpce/
    Copyright (c)  1985-2002, University of Amsterdam
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

#ifndef XPCE_KER_TYPE_H_INCLUDED
#define XPCE_KER_TYPE_H_INCLUDED

/* Functions defined in ker/type.c */

COMMON(status)	initialiseType(Type t, Name name, Name kind, Any context, Chain supers);
COMMON(Type)	getLookupType(Class class, Name name);
COMMON(Type)	createType(Name name, Name kind, Any context);
COMMON(Name)	getNameType(Type t);
COMMON(void)	superType(Type t, Type t2);
COMMON(status)	vectorType(Type t, BoolObj val);
COMMON(status)	isClassType(Type t);
COMMON(status)	specialisedType(Type t1, Type t2);
COMMON(status)	equalType(Type t1, Type t2);
COMMON(status)	includesType(Type t1, Type t2);
COMMON(Chain)	getValueSetType(Type t, Any ctx);
COMMON(Any)	getTranslateType(Type t, Any val, Any ctx);
COMMON(status)	validateType(Type t, const Any val, const Any ctx);
COMMON(status)	makeClassType(Class class);
COMMON(Type)	nameToType(Name name);
COMMON(void)	resetTypes(void);
COMMON(void)	initTypes(void);
COMMON(Type)	defineType(char *name, char *def);

#endif /*XPCE_KER_TYPE_H_INCLUDED*/
