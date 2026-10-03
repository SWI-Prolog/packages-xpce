/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  2001-2011, University of Amsterdam
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
Convert between HSV  (Hue-Saturnation-Value)   and  RGB (Red-Green-Blue)
colour models. XPCE uses the RGB  model internally, but provides methods
to class colour to  deal  with  the  HSV   model  as  it  is  much  more
comfortable in computing human aspects  of   colour  preception  such as
distance, shading, etc.

	RBG	Set of intensities in range 0.0-1.0
	HSV	Also ranged 0.0-1.0
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
RGBToHSV(float r, float g, float b, float *H, float *S, float *V)
{ float cmax, cmin;
  float h, s, v;

  cmax = r;
  cmin = r;
  if ( g > cmax )
  { cmax = g;
  } else if ( g < cmin )
  { cmin = g;
  }
  if ( b > cmax )
  { cmax = b;
  } else if ( b < cmin )
  { cmin = b;
  }
  v = cmax;

  if ( v > 0.0 )
  { s = (cmax - cmin) / cmax;
  } else
  { s = 0.0;
  }

  if ( s > 0 )
  { if ( r == cmax )
    { h = (g - b) / (cmax - cmin) / 6.0f;
    } else if ( g == cmax )
    { h = (2.0f + (b - r) / (cmax - cmin)) / 6.0f;
    } else
    { h = (4.0f + (r - g) / (cmax - cmin)) / 6.0f;
    }
    if ( h < 0.0 )
    { h = h + (float)1.0;
    }
  } else
  { h = 0.0;
  }

  *H = h;
  *S = s;
  *V = v;
}


void
HSVToRGB(float hue, float sat, float V, float *R, float *G, float *B)
{ float h6 = (hue < 0.0f || hue >= 1.0f ? 0.0f : hue) * 6.0f;
  int sextant = (int)h6;
  float f = h6 - (float)sextant;
  float p = V * (1.0f - sat);
  float q = V * (1.0f - sat * f);
  float t = V * (1.0f - sat * (1.0f - f));

  switch(sextant)
  { case 0:  *R = V; *G = t; *B = p; break; /* red/green */
    case 1:  *R = q; *G = V; *B = p; break; /* green/red */
    case 2:  *R = p; *G = V; *B = t; break; /* green/blue */
    case 3:  *R = p; *G = q; *B = V; break; /* blue/green */
    case 4:  *R = t; *G = p; *B = V; break; /* blue/red */
    default: *R = V; *G = p; *B = q; break; /* red/blue */
  }
}
