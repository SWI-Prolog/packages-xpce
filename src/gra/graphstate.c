/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  1995-2011, University of Amsterdam
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

typedef struct graphics_state *GraphicsState;

struct graphics_state
{ int		level;
  int		thickness;
  Name		texture;
  Any		foreground;
  Any   	background;
  GraphicsState savedstate;		/* previous state */
};

static GraphicsState statelist;
long   statechange;

void
g_save()
{ GraphicsState gs = alloc(sizeof(struct graphics_state));

  gs->level	 = (statelist ? statelist->level+1 : 1);
#ifdef __WINDOWS__
  gs->thickness  = context.thickness;
  gs->texture    = context.texture;
  gs->foreground = context.colour;
  gs->background = context.background;
#else
  gs->thickness  = context.gcs->pen;
  gs->texture    = context.gcs->dash;
  gs->foreground = context.gcs->colour;
  gs->background = context.gcs->background;
#endif

  gs->savedstate = statelist;
  statelist = gs;
}


void
g_restore()
{ GraphicsState gs = statelist;

  if ( !gs )
  { errorPce(NAME_gRestore, NAME_nestMisMatch);
    return;
  }

  r_thickness(gs->thickness);
  r_dash(gs->texture);
  r_colour(gs->foreground);
  r_background(gs->background);

  statelist = gs->savedstate;
  unalloc(sizeof(struct graphics_state), gs);
}


int
g_level()
{ return statelist ? 0 : statelist->level;
}
