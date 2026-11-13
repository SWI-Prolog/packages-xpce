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

#ifndef XPCE_KER_OBJECT_H_INCLUDED
#define XPCE_KER_OBJECT_H_INCLUDED

/* Functions defined in ker/object.c */

COMMON(void)	unreferencedObject(Any obj);
COMMON(void)	addRefObject(Any from, Any to);
COMMON(void)	delRefObject(Any from, Any to);
COMMON(void)	assignField(Instance instance, Any *field, Any value);
COMMON(void)	unallocInstanceProtoClass(Class class);
COMMON(Any)	allocObject(Class class, int funcs);
COMMON(status)	initialiseObject(Instance obj);
COMMON(Any)	createObjectv(Name assoc, Class class, int argc, const Any argv []);
COMMON(Any)	newObjectv(Class class, int argc, const Any argv []);
COMMON(status)	considerPreserveObject(Any obj);
COMMON(Any)	answerObjectv(Class class, int argc, const Any *argv);
COMMON(Any)	newObject(Class class, ...);
COMMON(Any)	tempObject(Class class, ...);
COMMON(Any)	globalObject(Name assoc, Class class, ...);
COMMON(Any)	answerObject(Class class, ...);
COMMON(status)	freeObject(Any obj);
COMMON(status)	createdObject(Any obj, Name how);
COMMON(status)	succeedObject(Any obj, ...);
COMMON(status)	failObject(Any obj, ...);
COMMON(Any)	getFailObject(Any obj);
COMMON(status)	virtualObject(Any obj);
COMMON(status)	virtualObject1(Any obj, Any a1);
COMMON(status)	virtualObject2(Any obj, Any a1, Any a2);
COMMON(Any)	getVirtualObject(Any obj);
COMMON(Any)	getVirtualObject1(Any obj, Any a1);
COMMON(Any)	getVirtualObject2(Any obj, Any a1, Any a2);
COMMON(Int)	getReferencesObject(Any obj);
COMMON(Name)	getFlagsObject(Any obj);
COMMON(status)	protectObject(Any obj);
COMMON(status)	doneObject(Any obj);
COMMON(status)	lockObject(Any obj, BoolObj val);
COMMON(status)	inspectObject(Any obj, BoolObj val);
COMMON(BoolObj)	getInspectObject(Any obj);
COMMON(Name)	getClassNameObject(Any obj);
COMMON(Class)	getClassObject(Any obj);
COMMON(Any)	getSelfObject(Any obj);
COMMON(status)	setSlotInstance(Any obj, Variable var, Any value);
COMMON(status)	nameReferenceObject(Any obj, Name name);
COMMON(status)	constraintObject(Any obj, Constraint c);
COMMON(status)	deleteConstraintObject(Any obj, Constraint c);
COMMON(status)	attachHyperObject(Any obj, Hyper h, Any to);
COMMON(status)	attributeObject(Any obj, Any name, Any value);
COMMON(status)	deleteAttributeObject(Any obj, Any att);
COMMON(Any)	getAttributeObject(Any obj, Name name);
COMMON(status)	updateConstraintsObject(Any obj);
COMMON(Tuple)	getSendMethodObject(Any obj, Name selector);
COMMON(Tuple)	getGetMethodObject(Any obj, Name selector);
COMMON(status)	hasSendMethodObject(Any obj, Name selector);
COMMON(status)	hasGetMethodObject(Any obj, Name selector);
COMMON(Any)	getCreateContextObject(Any obj, Code cond);
COMMON(Chain)	getAllConstraintsObject(Any obj, BoolObj create);
COMMON(Chain)	getAllHypersObject(Any obj, BoolObj create);
COMMON(Chain)	getAllAttributesObject(Any obj, BoolObj create);
COMMON(Chain)	getAllSendMethodsObject(Any obj, BoolObj create);
COMMON(Chain)	getAllGetMethodsObject(Any obj, BoolObj create);
COMMON(status)  sendMethodObject(Any obj, SendMethod m);
COMMON(status)  getMethodObject(Any obj, GetMethod m);
COMMON(Any)	getCloneObject(Any obj);
COMMON(status)	clonePceSlots(Any org, Any Clone);
COMMON(Any)	getClone2Object(Any obj);
COMMON(Int)	getArityObject(Any obj);
COMMON(Name)	getFunctorObject(Any obj);
COMMON(Any)	getArgObject(Any obj, Int arg);
COMMON(Any)	getSlotObject(Any obj, Any which);
COMMON(status)	slotObject(Any obj, Any which, Any value);
COMMON(status)	equalObject(Any o1, Any o2);
COMMON(status)	sameReferenceObject(Any o1, Any o2);
COMMON(status)	sendSuperObject(Any obj, Name selector, int argc, const Any argv []);
COMMON(Any)	getFindHyperObject(Any obj, Name hname, Code cond);
COMMON(Any)	getHyperedObject(Any obj, Name hname, Code cond);
COMMON(status)	freeHypersObject(Any obj, Name hname, Code cond);
COMMON(status)	changedObject(Any obj, ...);
COMMON(status)	changedFieldObject(Any obj, Any *field);
COMMON(Any)	getClassVariableValueObject(Any obj, Name name);
COMMON(status)	obtainClassVariablesObject(Any obj);
COMMON(status)	convertLoadedObjectObject(Any obj, Int oldversion, Int currentversion);
COMMON(Any)	getConvertObject(Any ctx, Any x);
COMMON(status)	CheckObject(Any obj, BoolObj recursive, HashTable checked, BoolObj silent);
COMMON(status)	errorObjectv(Any obj, Error e, int argc, Any *argv);
COMMON(Name)	getManIdObject(Any obj);
COMMON(status)	makeClassObject(Class class);
COMMON(status)	printReportObject(Any obj, Name kind,
				  CharArray fmt, int argc, Any *argv);

#endif /*XPCE_KER_OBJECT_H_INCLUDED*/
