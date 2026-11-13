/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org/packages/xpce/
    Copyright (c)  2026, SWI-Prolog Solutions b.v.
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

#ifndef XPCE_GRA_COORDS_H_INCLUDED
#define XPCE_GRA_COORDS_H_INCLUDED

/* Functions defined in gra/coords.c */

COMMON(bool)	deviceLocalToWindowCoord(Device dev, double lx, double ly,
					 double *wx, double *wy);
COMMON(bool)	windowToDeviceLocalCoord(Device dev, double wx, double wy,
					 double *lx, double *ly);
COMMON(bool)	graphicalToWindowCoord(Graphical gr, double lx, double ly,
				       double *wx, double *wy);
COMMON(bool)	windowToGraphicalCoord(Graphical gr, double wx, double wy,
				       double *lx, double *ly);
COMMON(bool)	graphicalToWindowArea(Graphical gr, Area in, double bbox[4]);
COMMON(bool)	graphicalToDeviceCoord(Graphical gr, Device *target_io,
				       double lx, double ly,
				       double *ox, double *oy);
COMMON(bool)	graphicalToDeviceAreaAABB(Graphical gr, Device *target_io,
					  int lx, int ly, int lw, int lh,
					  int *ox, int *oy,
					  int *ow, int *oh);
COMMON(bool)	deviceLocalAreaToWindowAABB(Device dev,
					    int lx, int ly, int lw, int lh,
					    int *wx, int *wy,
					    int *ww, int *wh);
COMMON(bool)	hasTransformInDeviceChain(Device dev);
COMMON(bool)	deviceChainHasTransform(Graphical gr);

#endif /*XPCE_GRA_COORDS_H_INCLUDED*/
