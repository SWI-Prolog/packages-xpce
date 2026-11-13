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

#ifndef XPCE_ADT_VECTOR_H_INCLUDED
#define XPCE_ADT_VECTOR_H_INCLUDED

/* Functions defined in adt/vector.c */

COMMON(status)	initialiseVectorv(Vector v, int argc, Any *argv);
COMMON(Vector)	createVectorv(int argc, Any *argv);
COMMON(status)	unlinkVector(Vector v);
COMMON(Int)	getLowIndexVector(Vector v);
COMMON(Int)	getHighIndexVector(Vector v);
COMMON(status)	rangeVector(Vector v, Int low, Int high);
COMMON(status)	clearVector(Vector v);
COMMON(Any)	getTailVector(Vector v);
COMMON(Vector)	getCopyVector(Vector v);
COMMON(status)	fillVector(Vector v, Any obj, Int from, Int to);
COMMON(status)	shiftVector(Vector v, Int places);
COMMON(Any)	getElementVector(Vector v, Int e);
COMMON(status)	elementVector(Vector v, Int e, Any obj);
COMMON(status)	appendVector(Vector v, int argc, Any obj []);
COMMON(Any)	getArgVector(Vector v, Int arg);
COMMON(Int)	getArityVector(Vector v);
COMMON(status)	makeClassVector(Class class);

#endif /*XPCE_ADT_VECTOR_H_INCLUDED*/
