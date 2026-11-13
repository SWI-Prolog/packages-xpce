/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org/packages/xpce/
    Copyright (c)  1985-2011, University of Amsterdam
                              VU University Amsterdam
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

#ifndef XPCE_ARI_EXPRESSION_H_INCLUDED
#define XPCE_ARI_EXPRESSION_H_INCLUDED

#include <h/arith.h>

/* Functions defined in ari/expression.c */

COMMON(void)	promoteToRealNumericValue(NumericValue n);
COMMON(status)	ar_add(NumericValue n1, NumericValue n2, NumericValue r);
COMMON(int)	ar_minus(NumericValue n1, NumericValue n2, NumericValue r);
COMMON(int)	ar_divide(NumericValue n1, NumericValue n2, NumericValue r);
COMMON(int)	ar_times(NumericValue n1, NumericValue n2, NumericValue r);
COMMON(status)	evaluateExpression(Any e, NumericValue v);
COMMON(Any)	ar_result(NumericValue n);
COMMON(Int)	ar_int_result(Any e, NumericValue n);
COMMON(Int)	getVarInBinaryExpression(BinaryExpression e, Var var);
COMMON(status)	makeClassBinaryExpression(Class class);
COMMON(status)	makeClassDivide(Class class);
COMMON(status)	makeClassIntDivide(Class class);
COMMON(status)	makeClassUnaryExpression(Class class);
COMMON(status)	makeClassRound(Class class);
COMMON(status)	makeClassFloor(Class class);
COMMON(status)	makeClassTimes(Class class);
COMMON(status)	makeClassPlus(Class class);
COMMON(status)	makeClassMinus(Class class);
COMMON(Int)	getValueExpression(Expression e, ...);

#endif /*XPCE_ARI_EXPRESSION_H_INCLUDED*/
