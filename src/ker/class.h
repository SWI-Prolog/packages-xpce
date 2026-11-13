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

#ifndef XPCE_KER_CLASS_H_INCLUDED
#define XPCE_KER_CLASS_H_INCLUDED

/* Functions defined in ker/class.c */

COMMON(Class)	typeClass(Name name);
COMMON(Class)	nameToTypeClass(Name name);
COMMON(Class)	defineClass(Name name, Name super, StringObj summary, SendFunc makefunction);
COMMON(status)	defineClasses(struct class_definition *classes);
COMMON(status)	realiseClass(Class class);
COMMON(void)	bindNewMethodsClass(Class class);
COMMON(status)	realiseBootClass(Class class);
COMMON(Class)	bootClass(Name name, Name super_name, int size, int slots, SendFunc newF, int argc, ...);
COMMON(void)	lookupBootClass(Class class, Func f, int argc, ...);
COMMON(Class)	getConvertClass(Class class_class, Any obj);
COMMON(void)	fixSendFunctionClass(Class class, Name selector);
COMMON(void)	appendManSummary(TextBuffer tb, Any obj, CharArray summary);
COMMON(void)	fixGetFunctionClass(Class class, Name selector);
COMMON(status)	setChangedFunctionClass(Class class, SendFunc func);
COMMON(status)	setInEventAreaFunctionClass(Class class, SendFunc func);
COMMON(status)	isPceSlot(Class class, int n);
COMMON(status)	allPceSlotsClass(Class class);
COMMON(void)	termClass(Class class, char *name, int argc, ...);
COMMON(status)	sendMethodv(Class class, Name name, Name group, int argc, va_list args);
COMMON(status)	sendMethod(Class class, Name name, Name group, int argc, ...);
COMMON(status)	storeMethod(Class class, Name name, SendFunc function);
COMMON(status)	getMethodv(Class class, Name name, Name group, const char *rtype, int argc, va_list args);
COMMON(status)	getMethod(Class class, Name name, Name group, const char *rtype, int argc, ...);
COMMON(status)	cloneStyleClass(Class class, Name style);
COMMON(status)	cloneStyleVariableClass(Class class, Name slot, Name style);
COMMON(status)	saveStyleVariableClass(Class class, Name slot, Name style);
COMMON(status)	saveStyleClass(Class class, Name style);
COMMON(status)	setCloneFunctionClass(Class class, SendFunc function);
COMMON(status)	setRedrawFunctionClass(Class class, SendFunc function);
COMMON(status)	setLoadStoreFunctionClass(Class class, SendFunc load, SendFunc store);
COMMON(status)	solidClass(Class class, BoolObj val);
COMMON(status)	sourceClass(Class class, SendFunc f, char *file);
COMMON(void)	localClass(Class class, Name name, Name group, char *type, Name access, char *doc);
COMMON(status)	declareClass(Class class, const classdecl *decls);
COMMON(status)	delegateClass(Class class, Name name);
COMMON(status)	prependDelegateClass(Class class, Name name);
COMMON(Variable) getInstanceVariableClass(Class class, Any which);
COMMON(void)	resetMessageResolve(void);
COMMON(Any)	getResolveSendMethodClass(Class class, Name name);
COMMON(Any)	getResolveGetMethodClass(Class class, Name name);
COMMON(Int)	getNoCreatedClass(Class class, BoolObj subtoo);
COMMON(Int)	getNoFreedClass(Class class, BoolObj subtoo);
COMMON(status)	createdClass(Class class, Any instance, Name how);
COMMON(status)	freedClass(Class class, Any instance);
COMMON(status)	isAClass(Class class, Class super);
COMMON(int)	numberTreeClass(Class class, int n);
COMMON(status)	featureClass(Class class, Name name, Any value);
COMMON(Any)	getFeatureClass(Class class, Name name);
COMMON(Chain)	getSendMethodsClass(Class class);
COMMON(status)	makeClassClass(Class class);

#endif /*XPCE_KER_CLASS_H_INCLUDED*/
