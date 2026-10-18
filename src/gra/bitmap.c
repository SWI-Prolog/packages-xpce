/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi.psy.uva.nl/packages/xpce/
    Copyright (c)  1985-2026, University of Amsterdam
			      SWI-Prolog Solutions b.v.
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
#include <h/unix.h>

static status	imageBitmap(BitmapObj bm, Image image);
static status	animationBitmap(BitmapObj bm, Animation a);
static status	stopBitmap(BitmapObj bm);
static BoolObj	getPlayingBitmap(BitmapObj bm);

static status
initialiseBitmap(BitmapObj b, Any source)
{ Image image;
  Animation a = NIL;

  if ( isDefault(source) )
  { TRY(image = newObject(ClassImage, NIL, EAV));
  } else if ( instanceOfObject(source, ClassAnimation) )
  { a = source;
    TRY(image = getElementVector(a->frames, ONE));
  } else
  { image = source;
  }

  initialiseGraphical(b, ZERO, ZERO, image->size->w, image->size->h);

  assign(b, pen, ZERO);
  assign(b, image, image);
  assign(b, animation, NIL);
  assign(b, frame, ZERO);
  assign(b, timer, NIL);
  assign(b, loops, ZERO);
  if ( image->access == NAME_both && isNil(image->bitmap) )
    assign(image, bitmap, b);
  updateSolidBitmap(b);

  if ( notNil(a) )
    animationBitmap(b, a);

  succeed;
}


static status
unlinkBitmap(BitmapObj bm)
{ if ( notNil(bm->timer) )
  { stopTimer(bm->timer);
    assign(bm, timer, NIL);
  }
  if ( notNil(bm->image) && bm->image->bitmap == bm )
    assign(bm->image, bitmap, NIL);

  return unlinkGraphical((Graphical) bm);
}


/* Bitmaps saved before the animation slots existed */

static status
initialiseNewSlotBitmap(BitmapObj bm, Variable new)
{ if ( new->name == NAME_frame || new->name == NAME_loops )
    setSlotInstance(bm, new, ZERO);

  return initialiseNewSlotGraphical((Graphical)bm, new);
}


static BitmapObj
getConvertBitmap(Class class, Name name)
{ Image im;

  if ( (im = getConvertImage(ClassImage, (Any) name)) != FAIL )
    answer(answerObject(ClassBitmap, im, EAV));

  fail;
}


static status
RedrawAreaBitmap(BitmapObj b, Area a)
{ int x, y, w, h;

  initialiseDeviceGraphical(b, &x, &y, &w, &h);
  if ( notNil(b->image) )
    r_image(b->image, 0, 0, x, y, w, h);

  if ( b->pen != ZERO )
  { r_thickness(valInt(b->pen));
    r_dash(b->texture);
    r_box(x, y, w, h, 0, NIL);
  }

  return RedrawAreaGraphical(b, a);
}


static status
geometryBitmap(BitmapObj b, Int x, Int y, Int w, Int h)
{ return geometryGraphical(b, x, y, DEFAULT, DEFAULT);
}


static BitmapObj
getCopyBitmap(BitmapObj bm)
{ BitmapObj copy = answerObject(ClassBitmap, EAV);

  copyGraphical(copy, bm);
  imageBitmap(copy, bm->image);
  if ( notNil(bm->animation) )
  { assign(copy, animation, bm->animation);
    assign(copy, frame, bm->frame);
  }

  answer(copy);
}


		 /*******************************
		 *	     ANIMATION		*
		 *******************************/

/* An animation is played by showing its frames as <-image, one after
 * the other, using a timer that fires once for each frame.
 */

static status
frameBitmap(BitmapObj bm, Int n)
{ Animation a = bm->animation;
  Image image;

  if ( isNil(a) ||
       !(image = getElementVector(a->frames, n)) )
    fail;

  assign(bm, frame, n);
  return imageBitmap(bm, image);
}


static status
animationBitmap(BitmapObj bm, Animation a)
{ stopBitmap(bm);
  assign(bm, animation, a);
  assign(bm, loops, ZERO);
  if ( isNil(a) )
  { assign(bm, frame, ZERO);
    succeed;
  }

  return frameBitmap(bm, ONE);
}


/* Schedule showing the frame after the current.  Like web browsers, we
 * show frames that ask for 10 milliseconds or less for 100
 * milliseconds: GIF files often use 0 for "as fast as possible".
 */

static status
scheduleFrameBitmap(BitmapObj bm)
{ Int d = getElementVector(bm->animation->delays, bm->frame);
  int ms = d ? valInt(d) : 0;

  if ( ms <= 10 )
    ms = 100;
  if ( isNil(bm->timer) )
    assign(bm, timer,
	   newObject(ClassTimer, toNum(ms/1000.0),
		     newObject(ClassMessage, bm, NAME_nextFrame, EAV),
		     EAV));
  else
    intervalTimer(bm->timer, toNum(ms/1000.0));

  return startTimer(bm->timer, NAME_once, DEFAULT);
}


static status
playBitmap(BitmapObj bm)
{ Animation a = bm->animation;

  if ( isNil(a) || valInt(a->frames->size) < 2 )
    fail;
  if ( getPlayingBitmap(bm) == ON )
    succeed;

  assign(bm, loops, ZERO);
  return scheduleFrameBitmap(bm);
}


/* Stop playing.  We keep the timer: this may run from its message */

static status
stopBitmap(BitmapObj bm)
{ if ( notNil(bm->timer) )
    stopTimer(bm->timer);

  succeed;
}


static status
nextFrameBitmap(BitmapObj bm)
{ Animation a = bm->animation;

  if ( isNil(a) )
    return stopBitmap(bm);

  int n = valInt(bm->frame) + 1;
  if ( n > valInt(a->frames->size) )
  { assign(bm, loops, toInt(valInt(bm->loops)+1));
    if ( a->loop_count != ZERO &&
	 valInt(bm->loops) >= valInt(a->loop_count) )
      return stopBitmap(bm);		/* stay at the last frame */
    n = 1;
  }

  TRY(frameBitmap(bm, toInt(n)));
  return scheduleFrameBitmap(bm);
}


static BoolObj
getPlayingBitmap(BitmapObj bm)
{ answer(notNil(bm->timer) && bm->timer->status != NAME_idle ? ON : OFF);
}


static status
imageBitmap(BitmapObj bm, Image image)
{ if ( bm->image != image )
  { if ( hasAlphaImage(bm->image) )
      clearFlag(bm, F_SOLID);

    CHANGING_GRAPHICAL(bm,
      addRefObj(bm);			/* avoid drop-out */
      if ( notNil(bm->image) && bm->image->bitmap == bm )
	assign(bm->image, bitmap, NIL);
      assign(bm, image, image);
      sizeArea(bm->area, image->size);
      if ( image->access == NAME_both && isNil(image->bitmap) )
	assign(image, bitmap, bm);
      delRefObj(bm);
      changedEntireImageGraphical(bm));

    updateSolidBitmap(bm);
  }

  succeed;
}


status
updateSolidBitmap(BitmapObj bm)
{ if ( ws_has_alpha_image(bm->image) )
    clearFlag(bm, F_SOLID);
  else
    setFlag(bm, F_SOLID);

  succeed;
}


static status
redrawBitmap(BitmapObj bm, Area a)
{ CHANGING_GRAPHICAL(bm, sizeArea(bm->area, bm->image->size));

  return redrawGraphical((Graphical) bm, DEFAULT);
}


static status
loadBitmap(BitmapObj bm, FileObj file, CharArray path)
{ Image image;

  if ( isDefault(path) )
    TRY( path = getClassVariableValueClass(ClassImage, NAME_path));

  TRY(findFile(file, path, NAME_read));

  TRY(image = newObject(ClassImage, file->name, EAV));

  return imageBitmap(bm, image);
}


		/********************************
		*     BACKWARD COMPATIBILITY    *
		********************************/


static status
storeBitmap(BitmapObj bm, FileObj file)
{ return storeSlotsObject(bm, file);
}


static status
loadFdBitmap(BitmapObj bm, IOSTREAM *fd, ClassDef def)
{ TRY(loadSlotsObject(bm, fd, def));

  updateSolidBitmap(bm);

  succeed;
}


static Chain
getContainsBitmap(BitmapObj bm)
{ answer(answerObject(ClassChain, bm->image, EAV));
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_load[] =
        { "file", "path=[char_array]" };
static char *T_geometry[] =
        { "x=[int]", "y=[int]", "width=[int]", "height=[int]" };

/* Instance Variables */

static vardecl var_bitmap[] =
{ SV(NAME_image, "image", IV_GET|IV_STORE, imageBitmap,
     NAME_appearance, "The pixel collection managed"),
  SV(NAME_animation, "animation*", IV_GET|IV_STORE, animationBitmap,
     NAME_animation, "Animation whose frames are shown"),
  SV(NAME_frame, "int", IV_GET|IV_STORE, frameBitmap,
     NAME_animation, "Index of the frame of <-animation shown"),
  IV(NAME_timer, "timer*", IV_NONE,
     NAME_animation, "Shows the next frame while playing"),
  IV(NAME_loops, "int", IV_GET,
     NAME_animation, "Times the animation was played since ->play")
};

/* Send Methods */

static senddecl send_bitmap[] =
{ SM(NAME_geometry, 4, T_geometry, geometryBitmap,
     DEFAULT, "Bitmaps can only be moved"),
  SM(NAME_initialise, 1, "image=[image|animation]", initialiseBitmap,
     DEFAULT, "Create from image or animation"),
  SM(NAME_initialiseNewSlot, 1, "new=variable", initialiseNewSlotBitmap,
     NAME_file, "Initialise <-frame and <-loops"),
  SM(NAME_unlink, 0, NULL, unlinkBitmap,
     DEFAULT, "Unlink from <-image"),
  SM(NAME_play, 0, NULL, playBitmap,
     NAME_animation, "Play <-animation from the current frame"),
  SM(NAME_stop, 0, NULL, stopBitmap,
     NAME_animation, "Stop playing; the current frame stays"),
  SM(NAME_nextFrame, 0, NULL, nextFrameBitmap,
     NAME_internal, "Show the next frame while playing"),
  SM(NAME_redraw, 1, "[area]", redrawBitmap,
     NAME_change, "Update size and repaint indicated area"),
  SM(NAME_load, 2, T_load, loadBitmap,
     NAME_file, "Load file (in path) into bitmap")
};

/* Get Methods */

static getdecl get_bitmap[] =
{ GM(NAME_contains, 0, "chain", NULL, getContainsBitmap,
     DEFAULT, "New chain with <-image"),
  GM(NAME_convert, 1, "bitmap", "name", getConvertBitmap,
     DEFAULT, "Convert image-names"),
  GM(NAME_copy, 0, "bitmap", NULL, getCopyBitmap,
     NAME_copy, "Make a copy that shares the image"),
  GM(NAME_playing, 0, "bool", NULL, getPlayingBitmap,
     NAME_animation, "@on while the animation plays")
};

/* Resources */

#define rc_bitmap NULL
/*
static classvardecl rc_bitmap[] =
{
};
*/

/* Class Declaration */

static Name bitmap_termnames[] = { NAME_image };

ClassDecl(bitmap_decls,
          var_bitmap, send_bitmap, get_bitmap, rc_bitmap,
          1, bitmap_termnames);



status
makeClassBitmap(Class class)
{ declareClass(class, &bitmap_decls);

  solidClass(class, ON);
  setRedrawFunctionClass(class, RedrawAreaBitmap);
  setLoadStoreFunctionClass(class, loadFdBitmap, storeBitmap);
  cloneStyleVariableClass(class, NAME_image, NAME_reference);
  cloneStyleVariableClass(class, NAME_animation, NAME_reference);
  cloneStyleVariableClass(class, NAME_timer, NAME_nil);
  saveStyleVariableClass(class, NAME_timer, NAME_nil);
  delegateClass(class, NAME_image);

  succeed;
}
