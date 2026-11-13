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

#ifndef XPCE_ADT_CHAIN_H_INCLUDED
#define XPCE_ADT_CHAIN_H_INCLUDED

/* Functions defined in adt/chain.c */

COMMON(status)	initialiseChainv(Chain ch, int argc, Any *argv);
COMMON(status)	clearChain(Chain ch);
COMMON(Int)	getSizeChain(Chain ch);
COMMON(status)	prependChain(Chain ch, Any obj);
COMMON(status)	appendChain(Chain ch, Any obj);
COMMON(status)	addChain(Chain ch, Any obj);
COMMON(status)	insertChain(Chain ch, Any obj);
COMMON(status)	insertAfterChain(Chain ch, Any obj, Any obj2);
COMMON(status)	insertBeforeChain(Chain ch, Any obj, Any obj2);
COMMON(status)	swapChain(Chain ch, Any obj1, Any obj2);
COMMON(status)	deleteHeadChain(Chain ch);
COMMON(status)	deleteChain(Chain ch, Any obj);
COMMON(status)	deleteCellChain(Chain ch, Cell cell);
COMMON(status)	memberChain(Chain ch, Any obj);
COMMON(Any)	getNextChain(Chain ch, Any val);
COMMON(Any)	getPreviousChain(Chain ch, Any val);
COMMON(status)	forAllChain(Chain ch, Code code, BoolObj safe);
COMMON(status)	forSomeChain(Chain ch, Code code, BoolObj safe);
COMMON(Any)	getFindChain(Chain ch, Code code);
COMMON(Chain)	getFindAllChain(Chain ch, Code code);
COMMON(status)	mergeChain(Chain ch, Chain ch2);
COMMON(status)	replaceChain(Chain ch, Any obj1, Any obj2);
COMMON(status)	emptyChain(Chain ch);
COMMON(Chain)	getCopyChain(Chain ch);
COMMON(int)	qsortCompareObjects(const void *o1, const void *o2);
COMMON(status)	sortChain(Chain ch, Code msg, BoolObj unique);
COMMON(status)	sortNamesChain(Chain ch, BoolObj unique);
COMMON(Tuple)	getCompleteNameChain(Chain ch, CharArray prefix, Function map, BoolObj ignore_case);
COMMON(Chain)	getIntersectionChain(Chain ch, Chain ch2);
COMMON(Any)	getHeadChain(Chain ch);
COMMON(Any)	getDeleteHeadChain(Chain ch);
COMMON(Any)	getTailChain(Chain ch);
COMMON(status)	moveBeforeChain(Chain ch, Any obj1, Any obj2);
COMMON(status)	moveAfterChain(Chain ch, Any obj1, Any obj2);
COMMON(status)	beforeChain(Chain ch, Any obj1, Any obj2);
COMMON(Any)	getNth1Chain(Chain ch, Int index);
COMMON(Any)	getNth0Chain(Chain ch, Int index);
COMMON(status)	cellValueChain(Chain ch, Int c, Any obj);
COMMON(Cell)	getNth0CellChain(Chain ch, Int index);
COMMON(Int)	getIndexChain(Chain ch, Any obj);
COMMON(Int)	getArityChain(Chain ch);
COMMON(Any)	getArgChain(Chain ch, Int arg);
COMMON(status)	makeClassChain(Class class);

#endif /*XPCE_ADT_CHAIN_H_INCLUDED*/
