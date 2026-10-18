/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org
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

#include <h/kernel.h>
#include <h/graphics.h>

/* An animation holds the frames of an animated image file (PNG, GIF,
 * WebP, AVIF or a Windows ANI cursor) as images, with the time each is
 * shown.  It does not play itself: `bitmap ->animation` and
 * `bitmap ->play` show it.  An image file that is not animated loads
 * as an animation of one frame.
 */

static status
initialiseAnimation(Animation a, SourceSink file)
{ assign(a, file,	file);
  assign(a, frames,	newObject(ClassVector, EAV));
  assign(a, delays,	newObject(ClassVector, EAV));
  assign(a, loop_count, ZERO);
  assign(a, size,	newObject(ClassSize, EAV));

  if ( instanceOfObject(file, ClassFile) )
    TRY(send(file, NAME_find,
	     getClassVariableValueClass(ClassImage, NAME_path),
	     NAME_read, EAV));

  return ws_load_animation(a);
}


static Image
getFrameAnimation(Animation a, Int n)
{ answer(getElementVector(a->frames, n));
}


static Int
getCountAnimation(Animation a)
{ answer(a->frames->size);
}


/* Instance Variables */

static vardecl var_animation[] =
{ IV(NAME_file, "source_sink", IV_GET,
     NAME_file, "Where the frames were loaded from"),
  IV(NAME_frames, "vector", IV_GET,
     NAME_frames, "The frames, as images"),
  IV(NAME_delays, "vector", IV_GET,
     NAME_frames, "Milliseconds to show each frame"),
  IV(NAME_loopCount, "int", IV_GET,
     NAME_frames, "Number of times to play; 0 is forever"),
  IV(NAME_size, "size", IV_GET,
     NAME_dimension, "Size of the frames")
};

/* Send Methods */

static senddecl send_animation[] =
{ SM(NAME_initialise, 1, "file=source_sink", initialiseAnimation,
     DEFAULT, "Load all frames of an animated image file")
};

/* Get Methods */

static getdecl get_animation[] =
{ GM(NAME_frame, 1, "image", "index=int", getFrameAnimation,
     NAME_frames, "The image of frame index (1-based)"),
  GM(NAME_count, 0, "int", NULL, getCountAnimation,
     NAME_frames, "Number of frames")
};

/* Class Declaration */

static Name animation_termnames[] = { NAME_file };

ClassDecl(animation_decls,
          var_animation, send_animation, get_animation, NULL,
          1, animation_termnames);


status
makeClassAnimation(Class class)
{ declareClass(class, &animation_decls);

  succeed;
}
