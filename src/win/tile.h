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

#ifndef XPCE_WIN_TILE_H_INCLUDED
#define XPCE_WIN_TILE_H_INCLUDED

#include <h/graphics.h>

/* Functions defined in win/tile.c */

typedef void *(*for_tile_func)(Any ctx, TileObj t, Int x, Int y, Int w, Int h);
COMMON(status)	unrelateTile(TileObj t);
COMMON(TileObj)	getRootTile(TileObj t);
COMMON(Any)	managerTile(TileObj t);
COMMON(void)	setManagerTile(TileObj t, Any manager);
COMMON(status)	distribute_stretches(stretch *s, int n, int w);
COMMON(void)	sum_stretches(stretch *sp, int len, stretch *r);
COMMON(void)	join_stretches(stretch *stretches, int len, stretch *r);
COMMON(status)	setTile(TileObj t, Int x, Int y, Int w, Int h);
COMMON(status)	enforceTile(TileObj t, BoolObj val);
COMMON(bool)	canResizeTile(TileObj t);
COMMON(BoolObj)	getCanResizeTile(TileObj t);
COMMON(status)	rebalanceTile(TileObj t);
COMMON(TileObj)	getSubTileToResizeTile(TileObj t, Point pos);
COMMON(status)	makeClassTile(Class class);
COMMON(void *)	forResizeAreaTile(TileObj t, for_tile_func func, Any ctx);

#endif /*XPCE_WIN_TILE_H_INCLUDED*/
