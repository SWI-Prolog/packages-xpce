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
    { h = (float)0.17 * (g - b) / (cmax - cmin);
    } else if ( g == cmax )
    { h = (float)0.33 + (float)0.17 * (b - r) / (cmax - cmin);
    } else
    { h = (float)0.67 + (float)0.17 * (r - g) / (cmax - cmin);
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
{ float r, g, b;


  if (hue > 0.17 && hue <= 0.33)	/* green/red */
  { g = 1.0;
    r = ((float)0.33 - hue) / (float)0.16;
    b = 0.0;
  } else if (hue > 0.33 && hue <= 0.5)	/* green/blue */
  { g = 1.0;
    b = (hue - (float)0.33) / (float)0.17;
    r = 0.0;
  } else if (hue > 0.5 && hue <= 0.67)	/* blue/green */
  { b = 1.0;
    g = ((float)0.67 - hue) / (float)0.17;
    r = 0.0;
  } else if (hue > 0.67 && hue <= 0.83)	/* blue/red */
  { b = 1.0;
    r = (hue - (float)0.67) / (float)0.16;
    g = 0.0;
  } else if (hue > 0.83 && hue <= 1.0)	/* red/blue */
  { r = 1.0;
    b = ((float)1.0 - hue) / (float)0.17;
    g = 0.0;
  } else				/* red/green */
  { r = 1.0;
    g = hue / (float)0.17;
    b = 0.0;
  }

  r = (sat * r + ((float)1.0 - sat));
  g = (sat * g + ((float)1.0 - sat));
  b = (sat * b + ((float)1.0 - sat));

  r = r * V;
  g = g * V;
  b = b * V;

  *R = r;
  *G = g;
  *B = b;
}
