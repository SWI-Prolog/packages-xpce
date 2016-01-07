/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
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

#include <h/kernel.h>
#include <h/graphics.h>
#include <h/unix.h>

static status	imageBitmap(BitmapObj bm, Image image);
static status	transparentBitmap(BitmapObj bm, BoolObj transparent);

static status
initialiseBitmap(BitmapObj b, Image image, BoolObj transparent)
{ if ( isDefault(image) )
    TRY(image = newObject(ClassImage, NIL, EAV));
  if ( isDefault(transparent) )
    transparent = OFF;

  initialiseGraphical(b, ZERO, ZERO, image->size->w, image->size->h);

  assign(b, pen, ZERO);
  assign(b, transparent, transparent);
  assign(b, image, image);
  if ( image->access == NAME_both && isNil(image->bitmap) )
    assign(image, bitmap, b);
  updateSolidBitmap(b);

  succeed;
}


static status
unlinkBitmap(BitmapObj bm)
{ if ( notNil(bm->image) && bm->image->bitmap == bm )
    assign(bm->image, bitmap, NIL);

  return unlinkGraphical((Graphical) bm);
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
    r_image(b->image, 0, 0, x, y, w, h, b->transparent);

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

  answer(copy);
}


static status
imageBitmap(BitmapObj bm, Image image)
{ if ( bm->image != image )
  { if ( (notNil(bm->image) && notNil(bm->image->mask)) ||
	 notNil(image->mask) )
      clearFlag(bm, F_SOLID);

    CHANGING_GRAPHICAL(bm,
      addRefObj(bm);			/* avoid drop-out */
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
{ if ( notNil(bm->image->mask) || bm->transparent == ON )
    clearFlag(bm, F_SOLID);
  else
    setFlag(bm, F_SOLID);

  succeed;
}


static status
transparentBitmap(BitmapObj bm, BoolObj transparent)
{ CHANGING_GRAPHICAL(bm,
		     assign(bm, transparent, transparent);
		     if ( transparent == OFF )
		       setFlag(bm, F_SOLID);
		     else
		       clearFlag(bm, F_SOLID);
		     changedEntireImageGraphical(bm));

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

  if ( restoreVersion < 7 )
  { if ( restoreVersion == 1 )
    { Image image = newObject(ClassImage, EAV);

      ws_load_old_image(image, fd);
      assign(bm, image, image);
    } else if ( restoreVersion <= 5 )
    { assign(bm, image, newObject(ClassImage, EAV));
      assign(bm, pen, ZERO);
      assign(bm, request_compute, NIL);

      switch( Sgetc(fd) )
      { case 'O':				/* no image */
	  setSize(bm->image->size, ZERO, ZERO);
	  break;
	case 'X':
	  loadXImage(bm->image, fd);
      }
    }

    if ( isNil(bm->texture) )
      assign(bm, texture, NAME_none);
    if ( isNil(bm->colour) )
      assign(bm, colour, DEFAULT);
    if ( isNil(bm->inverted) )
      assign(bm, inverted, OFF);
    if ( isNil(bm->transparent) )
      assign(bm, transparent, OFF);
  }

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
static char *T_initialise[] =
        { "image=[image]", "transparent=[bool]" };
static char *T_geometry[] =
        { "x=[int]", "y=[int]", "width=[int]", "height=[int]" };

/* Instance Variables */

static vardecl var_bitmap[] =
{ SV(NAME_image, "image", IV_GET|IV_STORE, imageBitmap,
     NAME_appearance, "The pixel collection managed"),
  SV(NAME_transparent, "bool", IV_GET|IV_STORE, transparentBitmap,
     NAME_appearance, "When @on, 0-pixels are not painted")
};

/* Send Methods */

static senddecl send_bitmap[] =
{ SM(NAME_geometry, 4, T_geometry, geometryBitmap,
     DEFAULT, "Bitmaps can only be moved"),
  SM(NAME_initialise, 2, T_initialise, initialiseBitmap,
     DEFAULT, "Create from image"),
  SM(NAME_unlink, 0, NULL, unlinkBitmap,
     DEFAULT, "Unlink from <-image"),
  SM(NAME_redraw, 1, "[area]", redrawBitmap,
     NAME_change, "Update size and repaint indicated area"),
  SM(NAME_load, 2, T_load, loadBitmap,
     NAME_file, "Load file (in path) into bitmap"),
  SM(NAME_DrawPostScript, 1, "{head,body}", drawPostScriptBitmap,
     NAME_postscript, "Create PostScript")
};

/* Get Methods */

static getdecl get_bitmap[] =
{ GM(NAME_contains, 0, "chain", NULL, getContainsBitmap,
     DEFAULT, "New chain with <-image"),
  GM(NAME_convert, 1, "bitmap", "name", getConvertBitmap,
     DEFAULT, "Convert image-names"),
  GM(NAME_copy, 0, "bitmap", NULL, getCopyBitmap,
     NAME_copy, "Make a copy that shares the image")
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
          1, bitmap_termnames,
          "$Rev$");



status
makeClassBitmap(Class class)
{ declareClass(class, &bitmap_decls);

  solidClass(class, ON);
  setRedrawFunctionClass(class, RedrawAreaBitmap);
  setLoadStoreFunctionClass(class, loadFdBitmap, storeBitmap);
  cloneStyleVariableClass(class, NAME_image, NAME_reference);
  delegateClass(class, NAME_image);

  succeed;
}

