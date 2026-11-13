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

#ifndef XPCE_ADT_AREA_H_INCLUDED
#define XPCE_ADT_AREA_H_INCLUDED

/* Functions defined in adt/area.c */

COMMON(status)	intersectionArea(Area a, Area b);
COMMON(status)	unionNormalisedArea(Area a, Area b);
COMMON(status)	sizeArea(Area a, Size s);
COMMON(Size)	getSizeArea(Area a);
COMMON(status)	pointInArea(Area a, Point p);
COMMON(status)	overlapArea(Area a, Area b);
COMMON(status)	relativeMoveArea(Area a, Point p);
COMMON(status)	relativeMoveBackArea(Area a, Point p);
COMMON(Point)	getPositionArea(Area a);
COMMON(status)	copyArea(Area a, Area b);
COMMON(Area)	getCopyArea(Area a);
COMMON(status)	insideArea(Area a, Area b);
COMMON(Int)	getDistanceXArea(Area a, Area b);
COMMON(Int)	getDistanceYArea(Area a, Area b);
COMMON(Int)	getDistanceArea(Area a, Area b);
COMMON(status)	clearArea(Area a);
COMMON(status)	setArea(Area a, Int x, Int y, Int w, Int h);
COMMON(status)	increaseArea(Area a, Int i);
COMMON(status)	orientationArea(Area a, Name orientation);
COMMON(Name)	getOrientationArea(Area a);
COMMON(status)	makeClassArea(Class class);

#endif /*XPCE_ADT_AREA_H_INCLUDED*/
