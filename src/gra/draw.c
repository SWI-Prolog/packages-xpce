/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  1996-2011, University of Amsterdam
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

#include <h/kernel.h>
#include <h/graphics.h>

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This file will contain window-system independent  3-d routines, based on
the real low-level routines in  xdraw.c   and  msdraw.c.  Various things
should be abstracted from these modules and  moved into this module. For
now, it contains only the new primitives.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
r_3d_rectangular_polygon(int n, IPoint pts, Elevation e, int flags)
	Draws a 3-d polygon.  The current implementation only deals
	with horizontal and vertical lines.  Might be abstracted
	further.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define LIGHT  1
#define DARK  -1

typedef struct
{ signed char dx, dy;
  signed char dlight;
} edge;

static const edge edges[3][3] =
{ { { 0,  0,LIGHT}, { 0, 0, LIGHT}, { 0, 0, LIGHT} },
  { { 0,  0,DARK},  { 0, 0, DARK},  { 0, 0, LIGHT} },
  { { 0,  0,DARK},  {-1, 0, DARK},  {-1, 0, DARK} }
};


void
r_3d_rectangular_polygon(int n, IPoint pts, Elevation e, int flags)
{ int h = valInt(e->height);
  int up = !(flags & DRAW_3D_DOWN);

  if ( h < 0 )
  { up = !up;
    h = -h;
  }

  if ( h )
  { ISegment dark  = (ISegment)alloca(sizeof(isegment) * n * h);
    ISegment light = (ISegment)alloca(sizeof(isegment) * n * h);
    ISegment last  = NULL;
    int ndark = 0, nlight = 0;
    int m;
    IPoint p1, p2;

    for(m=0; m<h; m++)
    { int i;

      for(p1 = pts, p2 = p1+1, i=0; i<n; i++, p1++, p2++)
      { isegment s;
	int dx, dy;
	const edge *e;

	if ( i == n-1 )
	  p2 = pts;			/* closing line */

	s.x1 = p1->x;
	s.y1 = p1->y;
	s.x2 = p2->x;
	s.y2 = p2->y;

	dx = s.x2 - s.x1;
	dy = s.y2 - s.y1;

	dx = (dx < 0 ? 0 : dx > 0 ? 2 : 1);	/* 0: < 0, 1: ==0, 2: > 0 */
	dy = (dy < 0 ? 0 : dy > 0 ? 2 : 1);
	e = &edges[dy][dx];

	DEBUG(NAME_path,
	      Cprintf("edge %d (%d,%d->%d,%d): dx=%d, dy=%d, dlight=%d\n",
		      i, p1->x, p1->y, p2->x, p2->y,
		      dx, dy, e->dlight));

	if ( i < n-1 || (flags & DRAW_3D_CLOSED) )
	{ if ( (up && e->dlight == LIGHT) ||
	       (!up && e->dlight == DARK) )
	    last = &light[nlight++];
	  else
	    last = &dark[ndark++];

	  *last = s;
	}
      }
    }

    r_3d_segments(nlight, light, e, TRUE);
    r_3d_segments(ndark,  dark,  e, FALSE);
  }
}
