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
 *
 * An animation can also be made by appending images, e.g., by
 * recording a window, and saved as an animated image file.
 */

static status	stopAnimation(Animation a);

static status
initialiseAnimation(Animation a, SourceSink file)
{ assign(a, file,	isDefault(file) ? NIL : file);
  assign(a, frames,	newObject(ClassVector, EAV));
  assign(a, delays,	newObject(ClassVector, EAV));
  assign(a, loop_count, ZERO);
  assign(a, size,	newObject(ClassSize, EAV));
  assign(a, title,	NIL);
  assign(a, author,	NIL);
  assign(a, description, NIL);
  assign(a, copyright,	NIL);
  assign(a, recording,	NIL);
  assign(a, timer,	NIL);

  if ( isNil(a->file) )
    succeed;

  if ( instanceOfObject(file, ClassFile) )
    TRY(send(file, NAME_find,
	     getClassVariableValueClass(ClassImage, NAME_path),
	     NAME_read, EAV));

  return ws_load_animation(a);
}


static status
unlinkAnimation(Animation a)
{ stopAnimation(a);
  if ( notNil(a->timer) )
    assign(a, timer, NIL);

  succeed;
}


/* Append a copy of `image`: the source may be reused, as the image of
 * a camera.  The first frame sets <-size; others are scaled to it.  The
 * copy keeps the hot spot, which ANI (cursor) files save.
 */

static status
appendAnimation(Animation a, Image image, Int delay)
{ Image copy;

  if ( a->frames->size == ZERO )
    copySize(a->size, image->size);

  if ( equalSize(image->size, a->size) )
  { TRY(copy = newObject(ClassImage, NIL, a->size->w, a->size->h,
			 NAME_pixmap, EAV));
    TRY(send(copy, NAME_copy, image, EAV));
  } else
  { TRY(copy = get(image, NAME_scale, a->size, EAV));
  }
  if ( notNil(image->hot_spot) )	/* e.g., for cursors */
    assign(copy, hot_spot, newObject(ClassPoint, image->hot_spot->x,
				     image->hot_spot->y, EAV));

  Any d = isDefault(delay) ? toInt(100) : delay;
  appendVector(a->frames, 1, (Any *)&copy);
  appendVector(a->delays, 1, &d);

  succeed;
}


static status
clearAnimation(Animation a)
{ clearVector(a->frames);
  clearVector(a->delays);
  assign(a->size, w, ZERO);
  assign(a->size, h, ZERO);

  succeed;
}


/* Record `from` (a frame or window) by appending its <-image every
 * `interval` seconds until ->stop.
 */

static status
recordAnimation(Animation a, Any from, Real interval)
{ double secs = isDefault(interval) ? 0.1 : valReal(interval);

  stopAnimation(a);
  assign(a, recording, from);
  if ( isNil(a->timer) )
    assign(a, timer,
	   newObject(ClassTimer, toNum(secs),
		     newObject(ClassMessage, a, NAME_recordFrame, EAV),
		     EAV));
  else
    intervalTimer(a->timer, toNum(secs));

  TRY(send(a, NAME_recordFrame, EAV));
  return startTimer(a->timer, NAME_repeat, DEFAULT);
}


static status
recordFrameAnimation(Animation a)
{ Image image;

  if ( isNil(a->recording) || isFreedObj(a->recording) ||
       !(image = get(a->recording, NAME_image, EAV)) )
    return stopAnimation(a);

  int ms = (int)(valNum(a->timer->interval)*1000.0 + 0.5);
  status rc = appendAnimation(a, image, toInt(ms));
  doneObject(image);

  return rc;
}


static status
stopAnimation(Animation a)
{ if ( notNil(a->timer) )
    stopTimer(a->timer);
  assign(a, recording, NIL);

  succeed;
}


static status
saveAnimation(Animation a, FileObj file, Name format, Int quality)
{ if ( a->frames->size == ZERO )
    return errorPce(a, NAME_noData);

  return ws_save_animation(a, file, format,
			   isDefault(quality) ? -1 : valInt(quality));
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
{ IV(NAME_file, "source_sink*", IV_GET,
     NAME_file, "Where the frames were loaded from"),
  IV(NAME_frames, "vector", IV_GET,
     NAME_frames, "The frames, as images"),
  IV(NAME_delays, "vector", IV_GET,
     NAME_frames, "Milliseconds to show each frame"),
  IV(NAME_loopCount, "int", IV_BOTH,
     NAME_frames, "Number of times to play; 0 is forever"),
  IV(NAME_size, "size", IV_GET,
     NAME_dimension, "Size of the frames"),
  IV(NAME_title, "char_array*", IV_BOTH,
     NAME_metadata, "Title stored in the file"),
  IV(NAME_author, "char_array*", IV_BOTH,
     NAME_metadata, "Author stored in the file"),
  IV(NAME_description, "char_array*", IV_BOTH,
     NAME_metadata, "Description stored in the file"),
  IV(NAME_copyright, "char_array*", IV_BOTH,
     NAME_metadata, "Copyright notice stored in the file"),
  IV(NAME_recording, "frame|window*", IV_GET,
     NAME_record, "Frame or window being recorded"),
  IV(NAME_timer, "timer*", IV_NONE,
     NAME_record, "Records frames")
};

static char *T_append[] =
	{ "image=image", "delay=[int]" };
static char *T_record[] =
	{ "from=frame|window", "interval=[real]" };
static char *T_save[] =
	{ "in=file", "format=[{png,gif,webp,avif,ani}]", "quality=[0..100]" };

/* Send Methods */

static senddecl send_animation[] =
{ SM(NAME_initialise, 1, "file=[source_sink]", initialiseAnimation,
     DEFAULT, "Load all frames of an animated image file"),
  SM(NAME_unlink, 0, NULL, unlinkAnimation,
     DEFAULT, "Stop recording"),
  SM(NAME_append, 2, T_append, appendAnimation,
     NAME_frames, "Append a copy of image, shown for delay ms"),
  SM(NAME_clear, 0, NULL, clearAnimation,
     NAME_frames, "Remove all frames"),
  SM(NAME_record, 2, T_record, recordAnimation,
     NAME_record, "Append the image of a frame or window every interval"),
  SM(NAME_recordFrame, 0, NULL, recordFrameAnimation,
     NAME_internal, "Append the image of <-recording"),
  SM(NAME_stop, 0, NULL, stopAnimation,
     NAME_record, "Stop recording"),
  SM(NAME_save, 3, T_save, saveAnimation,
     NAME_file, "Save as animated PNG, GIF, WebP, AVIF or ANI")
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
