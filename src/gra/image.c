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

#include <math.h>
#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

static status drawInImage(Image image, Graphical gr, Point pos);

		/********************************
		*         CREATE/DESTROY	*
		********************************/

status
initialiseImage(Image image, SourceSink data, Int w, Int h, Name kind)
{ Name name = FAIL;

  if ( isDefault(data) )
    data = (SourceSink) NIL;
  obtainClassVariablesObject(image);

  if ( notNil(data) && hasGetMethodObject(data, NAME_name) )
    name = get(data, NAME_name, EAV);
  if ( !name )
    name = NIL;

  assign(image, name,       name);
  assign(image, background, DEFAULT);
  assign(image, foreground, DEFAULT);
  ws_init_image(image);

  if ( isNil(data) || notDefault(w) || notDefault(h) || notDefault(kind) )
  { if ( isDefault(w) )    w = toInt(16);
    if ( isDefault(h) )    h = toInt(16);
    if ( isDefault(kind) ) kind = NAME_bitmap;

    assign(image, kind,   kind);
    assign(image, file,   NIL);
    assign(image, depth,  kind == NAME_bitmap ? ONE : (Int) DEFAULT);
    assign(image, size,	  newObject(ClassSize, w, h, EAV));
    assign(image, access, NAME_both);
  } else
  { assign(image, kind,	  NAME_bitmap);
    assign(image, file,	  data);
    assign(image, depth,  ONE);
    assign(image, size,	  newObject(ClassSize, EAV));
    TRY(loadImage(image, DEFAULT, DEFAULT));
    assign(image, access, NAME_read);
  }

  if ( notNil(name) )
  { protectObject(image);
    appendHashTable(ImageTable, name, image);
  }

  succeed;
}


static Image
getLookupImage(Class class, Any from)
{ if ( !isName(from) )
    from = qadGetv(from, NAME_name, 0, NULL);

  answer(getMemberHashTable(ImageTable, from));
}


static status
unlinkImage(Image image)
{ XcloseImage(image, DEFAULT);
  ws_destroy_image(image);

  if ( notNil(image->bitmap) && image->bitmap->image == image )
  { BitmapObj bm = image->bitmap;

    assign(image, bitmap, NIL);
    freeObject(bm);
  }

  if ( notNil(image->name) )
    deleteHashTable(ImageTable, image->name);

  succeed;
}


static BitmapObj
getContainedInImage(Image image)
{ if ( notNil(image->bitmap) )
    return image->bitmap;

  fail;
}

		/********************************
		*           CONVERSION		*
		********************************/

Image
getConvertImage(Class class, Any obj)
{ Image image;
  Name name;

  if ( (image = getConvertObject(class, obj)) )
  { if ( instanceOfObject(image, ClassImage) )
      answer(image);

    obj = image;
  }

  if ( instanceOfObject(obj, ClassBitmap) )
    answer(((BitmapObj)obj)->image);

  if ( instanceOfObject(obj, ClassRC) )
  { RC rc = obj;

    if ( (image = getMemberHashTable(ImageTable, rc->name)) )
      answer(image);

    answer(answerObject(ClassImage, obj, EAV));
  }

  if ( (name = checkType(obj, TypeName, class)) )
  { if ( (image = getMemberHashTable(ImageTable, name)) )
      answer(image);
    else
      answer(answerObject(ClassImage, name, EAV));
  }

  if ( instanceOfObject(obj, ClassGraphical) )
  { Graphical gr = obj;
    Image img;

    ComputeGraphical(gr);
    if ( (img = newObject(ClassImage, NIL, gr->area->w, gr->area->h, EAV)) )
    { drawInImage(img, gr, answerObject(ClassPoint, EAV));
      answer(img);
    }
  }

  fail;
}


		/********************************
		*            CHANGES		*
		********************************/

	/* TBD: function! */
#define CHANGING_IMAGE(img, code) \
  { BitmapObj _b = (img)->bitmap; \
 \
    code; \
 \
    if ( notNil(_b) ) \
    { Size _s = (img)->size; \
      Area _a = _b->area; \
 \
      if ( _s->w != _a->w || _s->h != _a->h ) \
      { Int _w = _a->w, _h = _a->h; \
 \
	assign(_a, w, _s->w); \
	assign(_a, h, _s->h); \
	changedAreaGraphical(_b, _a->x, _a->y, _w, _h); \
      } \
    } \
  }


		/********************************
		*           LOAD/STORE		*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Store/load images to/form file. Format:

<image>	::= <pce-slots>
	    'X' <image> | 'O'
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static status
storeImage(Image image, FileObj file)
{ TRY( storeSlotsObject(image, file) );

  if ( isNil(image->file) )
    return ws_store_image(image, file);
  else
  { Sputc('O', file->fd);
    succeed;
  }
}


static status
loadFdImage(Image image, IOSTREAM *fd, ClassDef def)
{ FileObj file;

  TRY( loadSlotsObject(image, fd, def) );
  ws_init_image(image);

					/* convert old path-representation */
  if ( instanceOfObject((file=(FileObj)image->file), ClassFile) &&
       isAbsoluteFile(file) &&
       getBaseNameFile(file) == image->name )
  { assign(file, path, file->name);
    assign(file, name, image->name);
  }

  switch( Sgetc(fd) )
  { case 'O':				/* no image */
      break;
    case 'X':
      return loadXImage(image, fd);
    case 'P':
      return loadPNMImage(image, fd);
  }

  succeed;
}


		/********************************
		*          XOPEN/XCLOSE		*
		********************************/


status
XopenImage(Image image, DisplayObj d)
{ if ( image->bits )			/* built-in.  See stdImage() */
  { switch(image->bits->type)
    { case XBM_DATA:
	ws_create_image_from_x11_data(image,
				      image->bits->bits.xbm,
				      valInt(image->size->w),
				      valInt(image->size->h));
	break;
      case XPM_DATA:
	ws_create_image_from_xpm_data(image,
				      image->bits->bits.xpm,
				      d);
	break;
      default:
	assert(0);
    }
			/* Windows already does the registration */
    if ( getExistingXrefObject(image, d) )
      succeed;
  }

  return ws_open_image(image, d, valReal(image->scale));
}


status
XcloseImage(Image image, DisplayObj d)
{ ws_close_image(image, d);

  succeed;
}

		 /*******************************
		 *	    COLOURMAP		*
		 *******************************/

static ColourMap
getColourMapImage(Image image)
{ if ( image->kind != NAME_bitmap )
    return ws_colour_map_for_image(image);

  fail;
}


		/********************************
		*         FILE OPERATIONS	*
		********************************/

status
loadImage(Image image, SourceSink file, CharArray path)
{ status rval;

  if ( notDefault(file) )
    assign(image, file, file);

  if ( isNil(image->file) )
    fail;

  if ( instanceOfObject(image->file, ClassFile) )
  { if ( isDefault(path) )
      TRY(path = getClassVariableValueObject(image, NAME_path));

    TRY(send(image->file, NAME_find, path, NAME_read, EAV));
  }

  CHANGING_IMAGE(image,
		 (rval = ws_load_image_file(image)));

  return rval;
}


static status
saveImage(Image image, SourceSink file, Name fmt)
{ if ( isDefault(file) )
    file = image->file;
  if ( isDefault(fmt) )
    fmt = NAME_xbm;

  if ( isNil(file) )
    return errorPce(image, NAME_noFile);

  return ws_save_image_file(image, file, fmt);
}



		/********************************
		*        EDIT OPERATIONS	*
		********************************/

static status
verifyAccessImage(Image image, Name sel)
{ if ( image->access != NAME_both )
    return errorPce(image, NAME_readOnly);

  if ( isNil(image->display) )
    assign(image, display, CurrentDisplay(image));

  openDisplay(image->display);

  succeed;
}


static status
changedImageImage(Image image, Int x, Int y, Int w, Int h)
{ if ( notNil(image->bitmap))
    return changedImageGraphical(image->bitmap, x, y, w, h);

  succeed;
}


static status
changedEntireImageImage(Image image)
{ if ( notNil(image->bitmap))
    return changedImageGraphical(image->bitmap, ZERO, ZERO,
				 image->size->w, image->size->h);

  succeed;
}


static status
inImage(Image image, Int x, Int y)
{ if ( valInt(x) >= 0 && valInt(y) >= 0 &&
       valInt(x) < valInt(image->size->w) &&
       valInt(y) < valInt(image->size->h) )
    succeed;

  fail;
}


static status
clearImage(Image image)
{ TRY( verifyAccessImage(image, NAME_clear) );

  CHANGING_IMAGE(image,
		 if ( image->size->w != ZERO && image->size->h != ZERO &&
		      notNil(image->display) &&
		      getExistingXrefObject(image, image->display) != NULL )
		 { int w = valInt(image->size->w);
		   int h = valInt(image->size->h);

		   d_image(image, 0, 0, w, h);
		   d_modify();
		   r_clear(0, 0, w, h);
		   d_done();
		   changedEntireImageImage(image);
		 });

  succeed;
}


static status
scaleImage(Image image, Real factor)
{ assign(image, scale, factor);
  if ( notNil(image->mask) )
    assign(image->mask, scale, factor);
  /* TBD: Verify the image is not yet opened */
  succeed;
}

static status
resizeImage(Image image, Int w, Int h)
{ TRY( verifyAccessImage(image, NAME_resize) );

  CHANGING_IMAGE(image,
		 ws_resize_image(image, w, h));

  succeed;
}


static status
copyImage(Image image, Image i2)
{ Int w = i2->size->w;
  Int h = i2->size->h;

  TRY(verifyAccessImage(image, NAME_copy));

  CHANGING_IMAGE(image,
    TRY(resizeImage(image, w, h));

    if ( d_image(image, 0, 0, valInt(w), valInt(h)) )
    { d_modify();
      r_image(i2, 0, 0, 0, 0, valInt(w), valInt(h), OFF);
      d_done();
      changedEntireImageImage(image);
    });

  succeed;
}


static status
drawInImage(Image image, Graphical gr, Point pos)
{ Int oldx, oldy;
  Device dev;
  int x, y, w, h;
  int iw = valInt(image->size->w);
  int ih = valInt(image->size->h);
  int m;
  Area a;
  BoolObj olddisplayed = gr->displayed;

  TRY(verifyAccessImage(image, NAME_drawIn));

  if ( notDefault(pos) )
  { oldx = gr->area->x;
    oldy = gr->area->y;
    dev = gr->device;
    gr->device = NIL;
    setGraphical(gr, pos->x, pos->y, DEFAULT, DEFAULT);
  } else
  { oldx = oldy = DEFAULT;
    dev = NIL;				/* keep compiler happy */
  }

  displayedGraphical(gr, ON);
  ComputeGraphical(gr);
  x = valInt(gr->area->x);
  y = valInt(gr->area->y);
  w = valInt(gr->area->w);
  h = valInt(gr->area->h);
  NormaliseArea(x,y,w,h);

  if ( x < 0 )				/* clip and normalise the area */
  { w += x;
    x = 0;
  } else if ( x > iw )
    goto out;

  if ( y < 0 )
  { h += y;
    y = 0;
  } else if ( y > ih )
    goto out;

  if ( w < 0 || h < 0 )
    goto out;

  if ( x+w > iw )
    w = iw - x;
  if ( y+h > ih )
    h = ih - y;

  if ( (m=get_extension_margin_graphical(gr)) )
  { x -= m; y -= m; w += m*2; h += 2*m;
  }

  a = answerObject(ClassArea, toInt(x), toInt(y), toInt(w), toInt(h), EAV);
  CHANGING_IMAGE(image,
    d_image(image, x, y, w, h);
    d_modify();
    RedrawArea(gr, a);			/* Was gr->area */
    d_done();
    if ( notNil(image->bitmap) )
      changedImageGraphical(image->bitmap,
			    toInt(x), toInt(y), toInt(w), toInt(h)););
  doneObject(a);

out:
  displayedGraphical(gr, olddisplayed);
  if ( notDefault(oldx) )
  { setGraphical(gr, oldx, oldy, DEFAULT, DEFAULT);
    gr->device = dev;
  }

  succeed;
}

		/********************************
		*            FILLING		*
		********************************/

status
fillImage(Image image, Any pattern, Area area)
{ int x, y, w, h;

  TRY(verifyAccessImage(image, NAME_fill));

  if ( isDefault(area) )
  { x = y = 0;
    w = valInt(image->size->w);
    h = valInt(image->size->h);
  } else
  { x = valInt(area->x);
    y = valInt(area->y);
    w = valInt(area->w);
    h = valInt(area->h);

    NormaliseArea(x, y, w, h);
    if ( x < 0 ) w += x, x = 0;
    if ( y < 0 ) h += y, y = 0;
    if ( x+w > valInt(image->size->w) ) w = valInt(image->size->w) - x;
    if ( y+h > valInt(image->size->h) ) h = valInt(image->size->h) - y;
  }

  if ( w > 0 && h > 0 )
  { CHANGING_IMAGE(image,
	  d_image(image, 0, 0, valInt(image->size->w), valInt(image->size->h));
	  d_modify();
	  r_fill(x, y, w, h, pattern);
	  d_done();
	  changedEntireImageImage(image));
  }

  succeed;
}


		/********************************
		*           PIXELS		*
		********************************/


static status
pixelImage(Image image, Int X, Int Y, Any val)
{ int x = valInt(X);
  int y = valInt(Y);

  TRY( verifyAccessImage(image, NAME_pixel) );

  if ( inImage(image, X, Y) )
  { if ( (image->kind == NAME_bitmap && !instanceOfObject(val, ClassBool)) ||
	 (image->kind == NAME_pixmap && !instanceOfObject(val, ClassColour)) )
      return errorPce(image, NAME_pixelMismatch, val);

    CHANGING_IMAGE(image,
	  d_image(image, 0, 0, valInt(image->size->w), valInt(image->size->h));
	  d_modify();
	  r_pixel(x, y, val);
	  d_done();
	  changedImageImage(image, X, Y, ONE, ONE));

    succeed;
  }

  fail;
}


static status
setPixelImage(Image image, Int x, Int y)
{ if ( image->kind == NAME_bitmap )
    return pixelImage(image, x, y, ON);
  else
    return pixelImage(image, x, y, image->foreground);
}


static status
clearPixelImage(Image image, Int x, Int y)
{ if ( image->kind == NAME_bitmap )
    return pixelImage(image, x, y, OFF);
  else
    return pixelImage(image, x, y, image->background);
}


static status
invertPixelImage(Image image, Int x, Int y)
{ TRY(verifyAccessImage(image, NAME_invertPixel));

  if ( inImage(image, x, y) )
  { CHANGING_IMAGE(image,
	d_image(image, 0, 0, valInt(image->size->w), valInt(image->size->h));
	d_modify();
	r_complement_pixel(valInt(x), valInt(y));
	d_done();
	changedImageImage(image, x, y, ONE, ONE));
  }
  succeed;
}


static status
invertImage(Image image)
{ TRY(verifyAccessImage(image, NAME_invert));

  CHANGING_IMAGE(image,
	d_image(image, 0, 0, valInt(image->size->w), valInt(image->size->h));
	d_modify();
	r_complement(0, 0, valInt(image->size->w), valInt(image->size->h));
	d_done();
	changedEntireImageImage(image));

  succeed;
}


static Any
getPixelImage(Image image, Int x, Int y)
{ if ( inImage(image, x, y) )
  { Any result;
    d_image(image, 0, 0, valInt(image->size->w), valInt(image->size->h));

    if ( image->kind == NAME_bitmap )
      result = (r_get_mono_pixel(valInt(x), valInt(y)) ? ON : OFF);
    else
    { unsigned long pixel;

      pixel = r_get_pixel(valInt(x), valInt(y));
      if ( pixel == NoPixel )
	result = FAIL;
      else
	result = ws_pixel_to_colour(image->display, pixel);
    }
    d_done();

    answer(result);
  }

  fail;
}


static status
maskImage(Image image, Image mask)
{ assign(image, mask, mask);

  if ( notNil(image->bitmap) )
    updateSolidBitmap(image->bitmap);

  ws_prepare_image_mask(image);

  return changedEntireImageImage(image);
}


		/********************************
		*      LOGICAL OPERATIONS	*
		********************************/

static status
opImage(Image image, Image i2, Name op, Point pos)
{ int x, y;

  TRY(verifyAccessImage(image, op));
  if ( notDefault(pos) )
  { x = valInt(pos->x);
    y = valInt(pos->y);
  } else
    x = y = 0;

  CHANGING_IMAGE(image,
	d_image(image, x, y, valInt(image->size->w), valInt(image->size->h));
	d_modify();
	r_op_image(i2, 0, 0, x, y, valInt(i2->size->w), valInt(i2->size->h),
		   op);
	d_done();
	changedEntireImageImage(image));

  succeed;
}


static status
orImage(Image image, Image i2, Point pos)
{ return opImage(image, i2, NAME_or, pos);
}


static status
andImage(Image image, Image i2, Point pos)
{ return opImage(image, i2, NAME_and, pos);
}


static status
xorImage(Image image, Image i2, Point pos)
{ return opImage(image, i2, NAME_xor, pos);
}


		/********************************
		*        GET SUB IMAGES		*
		********************************/

static Image
getClipImage(Image image, Area area)
{ int x, y, w, h;
  Image i2;

  if ( isDefault(area) )
  { x = y = 0;
    w = valInt(image->size->w);
    h = valInt(image->size->h);
  } else
  { x = valInt(area->x); y = valInt(area->y);
    w = valInt(area->w); h = valInt(area->h);
  }

  i2 = answerObject(ClassImage, NIL, toInt(w), toInt(h), image->kind, EAV);

  if ( notNil(image->hot_spot) )
  { int hx = valInt(image->hot_spot->x) - x;
    int hy = valInt(image->hot_spot->y) - y;

    if ( hx >= 0 && hx <= w && hy >= 0 && hy <= h )
      assign(i2, hot_spot, newObject(ClassPoint, toInt(hx), toInt(hy), EAV));
  }
  if ( notNil(image->mask) )
    assign(i2, mask, getClipImage(image->mask, area));

  CHANGING_IMAGE(i2,
    d_image(i2, 0, 0, w, h);
    d_modify();
    r_image(image, x, y, 0, 0, w, h, OFF);
    d_done();
    changedEntireImageImage(i2););

  answer(i2);
}


Image
getMonochromeImage(Image image)
{ if ( image->kind == NAME_bitmap )
    answer(image);

  answer(ws_monochrome_image(image));
}


static Image
getGrayscaleImage(Image image)
{ answer(ws_grayscale_image(image));
}


static Image
getScaleImage(Image image, Size size)
{ Image i2;

  if ( equalSize(size, image->size) )	/* just make a copy */
    return getClipImage(image, DEFAULT);
  if ( size->w == ZERO || size->h == ZERO )
    return answerObject(ClassImage, NIL, size->w, size->h, image->kind, EAV);

  i2 = ws_scale_image(image, valInt(size->w), valInt(size->h));

  if ( notNil(image->mask) )
  { Image m = getScaleImage(image->mask, size);

    if ( m )
      assign(i2, mask, m);
  }

  if ( notNil(image->hot_spot) )
  { int hx = (valInt(image->hot_spot->x) * valInt(size->w)) /
						    valInt(image->size->w);
    int hy = (valInt(image->hot_spot->y) * valInt(size->h)) /
						    valInt(image->size->h);
    assign(i2, hot_spot, newObject(ClassPoint, toInt(hx), toInt(hy), EAV));
  }

  answer(i2);
}


static Image
getRotateImage(Image image, Real degrees)
{ float a = (float)valReal(degrees);
  Image rimg;

  a -= (float)(((int)a / 360)*360);
  if ( a < 0.0 )				/* normalise 0<=a<360 */
    a += 360.0;
  else if ( a == 0.0 )				/* just copy */
    answer(getClipImage(image, DEFAULT));

  rimg = ws_rotate_image(image, a);

  if ( rimg )
  { if ( notNil(image->hot_spot) )
    { int hx = valInt(image->hot_spot->x);
      int hy = valInt(image->hot_spot->y);
      int nhx, nhy;
      double rads = (a * M_PI) / 180.0;

      nhx = rfloat((double)hx * cos(rads) + (double)hy * sin(rads));
      nhy = rfloat((double)hy * cos(rads) - (double)hx * sin(rads));

      if ( a <= 90.0 )
      { nhy += rfloat(sin(rads) * (double)valInt(image->size->w));
      } else if ( a <= 180.0 )
      { nhx -= rfloat(cos(rads) * (double)valInt(image->size->w));
	nhy += valInt(rimg->size->h);
      } else if ( a <= 270.0 )
      { nhx += valInt(rimg->size->w);
	nhy -= rfloat(cos(rads) * (double)valInt(image->size->h));
      } else
      { nhx -= rfloat(sin(rads) * (double)valInt(image->size->h));
      }

      assign(rimg, hot_spot, newObject(ClassPoint, toInt(nhx), toInt(nhy), EAV));
    }

    if ( notNil(image->mask) )
      assign(rimg, mask, getRotateImage(image->mask, degrees));
  }

  answer(rimg);
}


		/********************************
		*           POSTSCRIPT		*
		********************************/

static Area
getBoundingBoxImage(Image image)
{ answer(answerObject(ClassArea,
		      ZERO, ZERO, image->size->w, image->size->h, EAV));
}


static Int
getPostscriptDepthImage(Image image)
{ if ( image->kind == NAME_bitmap )
    return ONE;
  if ( valInt(image->depth) < 3 )	/* 1, 2 */
    return image->depth;
  if ( valInt(image->depth) < 8 )	/* 3-7 */
    return toInt(4);

  return toInt(8);
}


static Int
getPostscriptFormatImage(Image image)
{ if ( image->kind == NAME_bitmap )
    return NAME_monochrome;
  else
    return NAME_colour;			/* may also return greyscale */
}




		/********************************
		*       PREDEFINED IMAGES	*
		********************************/

#if __WINDOWS__ || HAVE_LIBXPM || SDL_GRAPHICS
#define XPM_PCEIMAGE 1			/* use an XPM image */
#endif

#include "bitmaps/mark_handle_bm"
#include "bitmaps/ms_mark.bm"
#include "bitmaps/ms_nomark.bm"
#include "bitmaps/ms_left_arrow.bm"
#include "bitmaps/ol_pulldown.bm"
#include "bitmaps/ol_pullright.bm"
#include "bitmaps/ol_cycle.bm"
#include "bitmaps/intarrows.bm"

static Image
stdImage(Name name, Image *global, unsigned char *bits, int w, int h)
{ Image image = globalObject(name, ClassImage, name, toInt(w), toInt(h), EAV);

  assign(image, access, NAME_read);
  image->bits = alloc(sizeof(*image->bits));
  image->bits->type = XBM_DATA;
  image->bits->bits.xbm = bits;
  if ( global )
    *global = image;

  return image;
}

static inline int
rescale(Image image, int px)
{ return (int)((double)px*valReal(image->scale)+0.5);
}

#ifdef XPM_PCEIMAGE
static void
stdXPMImage(Name name, Image *global, char **bits)
{ int w, h, colours;

  if ( sscanf(bits[0], "%d %d %d", &w, &h, &colours) == 3 )
  { Image image = globalObject(name, ClassImage, name, toInt(w), toInt(h), EAV);

    if ( colours == 2 )
    { assign(image, depth, ONE);
      assign(image, kind, NAME_bitmap);
    } else
    { assign(image, kind, NAME_pixmap);
    }

    assign(image, access, NAME_read);
    setSize(image->size,
	    toInt(rescale(image, w)),
	    toInt(rescale(image, h)));
    image->bits = alloc(sizeof(*image->bits));
    image->bits->type = XPM_DATA;
    image->bits->bits.xpm = bits;

    if ( global )
      *global = image;
  } else
    Cprintf("Failed to initialise image %s\n", pp(name));
}


#include "bitmaps/swipl48.xpm"
#include "bitmaps/hadjusttile.xpm"
#include "bitmaps/vadjusttile.xpm"
#include "bitmaps/up.xpm"
#include "bitmaps/down.xpm"
#include "bitmaps/left.xpm"
#include "bitmaps/right.xpm"
#include "bitmaps/exclamation.xpm"
#include "bitmaps/cnode.xpm"
#include "bitmaps/enode.xpm"
#include "bitmaps/mark.xpm"
#include "bitmaps/nomark.xpm"
#endif

#include "bitmaps/white_bm"
#include "bitmaps/grey50_bm"
#include "bitmaps/black_bm"

static void
greyImage(Name name, int grey, Image *global,
	  unsigned char *bits, int w, int h)
{ Image image;

  image = stdImage(name, global, bits, w, h);

  attributeObject(image, NAME_postscriptGrey, toInt(grey));
}


static void
standardImages(void)
{ greyImage(NAME_whiteImage,  0,  &WHITE_IMAGE,
	    white_bm_bits, white_bm_width, white_bm_height);
  greyImage(NAME_grey50Image, 50, &GREY50_IMAGE,
	    grey50_bm_bits, grey50_bm_width, grey50_bm_height);
  greyImage(NAME_blackImage, 100, &BLACK_IMAGE,
	    black_bm_bits, black_bm_width, black_bm_height);

  stdImage(NAME_msMarkImage, &MS_MARK_IMAGE,
	   ms_mark_bits, ms_mark_width, ms_mark_height);
  stdImage(NAME_msNomarkImage, &MS_NOMARK_IMAGE,
	   ms_nomark_bits, ms_nomark_width, ms_nomark_height);
  stdImage(NAME_msLeftArrowImage, NULL,
	   ms_left_arrow_bits, ms_left_arrow_width, ms_left_arrow_height);
  stdImage(NAME_markHandleImage, &MARK_HANDLE_IMAGE,
	   mark_handle_bm_bits, mark_handle_bm_width, mark_handle_bm_height);
  stdImage(NAME_olPullrightImage, NULL,
	   ol_pullright_bits, ol_pullright_width, ol_pullright_height);
  stdImage(NAME_olPulldownImage, NULL,
	   ol_pulldown_bits, ol_pulldown_width, ol_pulldown_height);
  stdImage(NAME_olCycleImage, NULL,
	   ol_cycle_bits, ol_cycle_width, ol_cycle_height);
  stdImage(NAME_intItemImage, &INT_ITEM_IMAGE,
	   intarrows_bits, intarrows_width, intarrows_height);
#ifdef XPM_PCEIMAGE
  stdXPMImage(NAME_pceImage,	       NULL,		    swipl48_xpm);
  stdXPMImage(NAME_hadjustTileImage,   NULL,		    hadjusttile_xpm);
  stdXPMImage(NAME_vadjustTileImage,   NULL,		    vadjusttile_xpm);
  stdXPMImage(NAME_scrollUpImage,      &SCROLL_UP_IMAGE,    up_xpm);
  stdXPMImage(NAME_scrollDownImage,    &SCROLL_DOWN_IMAGE,  down_xpm);
  stdXPMImage(NAME_scrollLeftImage,    &SCROLL_LEFT_IMAGE,  left_xpm);
  stdXPMImage(NAME_scrollRightImage,   &SCROLL_RIGHT_IMAGE, right_xpm);
  stdXPMImage(NAME_exclamationImage,   &EXCLAMATION_IMAGE,  exclamation_xpm);
  stdXPMImage(NAME_treeExpandedImage,  NULL,		    enode_xpm);
  stdXPMImage(NAME_treeCollapsedImage, NULL,		    cnode_xpm);
  stdXPMImage(NAME_markImage,          &MARK_IMAGE,	    mark_xpm);
  stdXPMImage(NAME_nomarkImage,        &NOMARK_IMAGE,	    nomark_xpm);
#endif

  stdImage(NAME_nullImage, &NULL_IMAGE,
	   NULL, 0, 0);

  ws_system_images();			/* make sure system images exist */
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_load[] =
	{ "from=[source_sink]", "path=[char_array]" };
static char *T_drawIn[] =
	{ "graphical", "at=[point]" };
static char *T_fill[] =
	{ "image", "[area]" };
static char *T_initialise[] =
	{ "name=[source_sink]*", "width=[int]", "height=[int]",
	  "kind=[{bitmap,pixmap}]" };
static char *T_image_atADpointD[] =
	{ "image", "at=[point]" };
static char *T_save[] =
	{ "in=[source_sink]", "format=[{xbm,xpm,pnm,pbm,pgm,ppm,jpeg,gif}]" };
static char *T_postscript[] =
	{ "landscape=[bool]", "maximum_area=[area]" };
static char *T_resize[] =
	{ "width=int", "height=int" };
static char *T_xAint_yAint[] =
	{ "x=int", "y=int" };
static char *T_pixel[] =
	{ "x=int", "y=int", "value=colour|bool" };
#ifdef O_XLI
static char *T_loadXli[] =
	{ "file=file", "bright=[0..]" };
#endif

/* Instance Variables */

static vardecl var_image[] =
{ IV(NAME_name, "name*", IV_GET,
     NAME_name, "Name of the image"),
  IV(NAME_kind, "{bitmap,pixmap}", IV_GET,
     NAME_colour, "`bitmap' (0 and 1's) or `pixmap' (coloured)"),
  IV(NAME_file, "source_sink*", IV_GET,
     NAME_file, "Source (file,resource) from which to load"),
  IV(NAME_access, "{read,both}", IV_GET,
     NAME_permission, "One of {read, both}"),
  IV(NAME_background, "[colour|pixmap]", IV_BOTH,
     NAME_colour, "Colour of background (pixmap)"),
  IV(NAME_foreground, "[colour|pixmap]", IV_BOTH,
     NAME_colour, "Colour of foreground (pixmap)"),
  IV(NAME_depth, "[int]", IV_GET,
     NAME_colour, "Number of bits/pixel"),
  IV(NAME_size, "size", IV_GET,
     NAME_dimension, "Size of the image in pixels"),
  SV(NAME_scale, "real", IV_GET|IV_STORE, scaleImage,
     NAME_dimension, "Set scale factor for image"),
  IV(NAME_display, "display*", IV_GET,
     NAME_organisation, "X-Display this image belongs to"),
  IV(NAME_bitmap, "bitmap*", IV_GET,
     NAME_organisation, "Access both and displayed on this bitmap"),
  IV(NAME_hotSpot, "point*", IV_BOTH,
     NAME_dimension, "Hot-spot position"),
  SV(NAME_mask, "image*", IV_GET|IV_STORE, maskImage,
     NAME_area, "Image for masked painting"),
  IV(NAME_wsRef, "alien:WsRef", IV_NONE,
     NAME_storage, "Window System Reference"),
  IV(NAME_bits, "alien:char*", IV_NONE,
     NAME_storage, "Data for built-in images")
};

/* Send Methods */

#ifdef O_XLI
extern status loadXliImage(Image image, FileObj file, Int bright);
#endif

static senddecl send_image[] =
{ SM(NAME_initialise, 4, T_initialise, initialiseImage,
     DEFAULT, "Create from name, [width, height, kind]"),
  SM(NAME_unlink, 0, NULL, unlinkImage,
     DEFAULT, "Destroy private memory and window-system resources"),
  SM(NAME_copy, 1, "from=image", copyImage,
     NAME_copy, "Copy contents of argument in image"),
  SM(NAME_drawIn, 2, T_drawIn, drawInImage,
     NAME_copy, "Paint graphical in image [at point]"),
  SM(NAME_resize, 2, T_resize, resizeImage,
     NAME_dimension, "Resize image to width, height"),
  SM(NAME_and, 2, T_image_atADpointD, andImage,
     NAME_edit, "Bitwise and with argument"),
  SM(NAME_clear, 0, NULL, clearImage,
     NAME_edit, "Clear all pixels to 0 or <-background"),
  SM(NAME_fill, 2, T_fill, fillImage,
     NAME_edit, "Fill rectangular area of image with pattern"),
  SM(NAME_invert, 0, NULL, invertImage,
     NAME_edit, "Invert all pixels in image"),
  SM(NAME_or, 2, T_image_atADpointD, orImage,
     NAME_edit, "Bitwise or with argument"),
  SM(NAME_xor, 2, T_image_atADpointD, xorImage,
     NAME_edit, "Bitwise xor with argument"),
  SM(NAME_load, 2, T_load, loadImage,
     NAME_file, "Load image from file (searching in path)"),
#ifdef O_XLI
  SM(NAME_loadXli, 2, T_loadXli, loadXliImage,
     NAME_file, "Load image using xli library"),
#endif
  SM(NAME_save, 2, T_save, saveImage,
     NAME_file, "Save image to file in specified format"),
  SM(NAME_clearPixel, 2, T_xAint_yAint, clearPixelImage,
     NAME_pixel, "Clear pixel at x-y (to 0 or background)"),
  SM(NAME_invertPixel, 2, T_xAint_yAint, invertPixelImage,
     NAME_pixel, "Invert pixel at x-y"),
  SM(NAME_pixel, 3, T_pixel, pixelImage,
     NAME_pixel, "Set pixel at x-y to bool or colour"),
  SM(NAME_setPixel, 2, T_xAint_yAint, setPixelImage,
     NAME_pixel, "Set pixel at x-y (to 1 or foreground)"),
  SM(NAME_Postscript, 1, "{head,body}", drawPostScriptImage,
     NAME_postscript, "Create PostScript"),
  SM(NAME_DrawPostScript, 1, "{head,body}", drawPostScriptImage,
     NAME_postscript, "Create PostScript"),
  SM(NAME_Xclose, 1, "display", XcloseImage,
     NAME_x, "Destroy associated window-system resources"),
  SM(NAME_Xopen, 1, "display", XopenImage,
     NAME_x, "Open X-image")
};

/* Get Methods */

static getdecl get_image[] =
{ GM(NAME_containedIn, 0, "bitmap", NULL, getContainedInImage,
     DEFAULT, "Equivalent to <-bitmap if ot @nil"),
  GM(NAME_convert, 1, "image", "bitmap|name|resource|graphical", getConvertImage,
     DEFAULT, "Convert bitmap or (file-)name"),
  GM(NAME_clip, 1, "image", "[area]", getClipImage,
     NAME_copy, "Get a subimage"),
  GM(NAME_monochrome, 0, "image", NULL, getMonochromeImage,
     NAME_copy, "Get monochrome version of pixmap image"),
  GM(NAME_grayscale, 0, "image", NULL, getGrayscaleImage,
     NAME_copy, "Get grayscale version of image"),
  GM(NAME_scale, 1, "image", "size", getScaleImage,
     NAME_copy, "Get copy with different dimensions"),
  GM(NAME_rotate, 1, "image", "degrees=real", getRotateImage,
     NAME_copy, "Get anti-clockwise rotated copy"),
  GM(NAME_lookup, 1, "image", "name|resource", getLookupImage,
     NAME_oms, "Lookup in @images table"),
  GM(NAME_pixel, 2, "value=bool|colour", T_xAint_yAint, getPixelImage,
     NAME_pixel, "Get 0-1 (image) or colour for x-y"),
  GM(NAME_colourMap, 0, "colour_map", NULL, getColourMapImage,
     NAME_colour, "New colour_map for best display of image"),
  GM(NAME_boundingBox, 0, "area", NULL, getBoundingBoxImage,
     NAME_postscript, "BoundingBox for PostScript generation"),
  GM(NAME_postscript, 2, "string", T_postscript, getPostscriptObject,
     NAME_postscript, "New string holding PostScript description"),
  GM(NAME_postscriptDepth, 0, "int", NULL, getPostscriptDepthImage,
     NAME_postscript, "Depth for PostScript image to be generated"),
  GM(NAME_postscriptFormat, 0, "{monochrome,greyscale,colour}",
     NULL, getPostscriptFormatImage,
     NAME_postscript, "Format of generated PostScript")
};

/* Resources */

static classvardecl rc_image[] =
{ RC(NAME_path, "string",
     "\".:bitmaps:~/lib/bitmaps:$PCEHOME/bitmaps:" /* concat */
     "/usr/include/X11/bitmaps\"",
     "Search path for loading images"),
  RC(NAME_scale, "real", UXWIN("when(@display?dpi?width > 150, 2, 1)", "1.0"),
     "Scale factor for the image")
};

/* Class Declaration */

static Name image_termnames[] = { NAME_name };

ClassDecl(image_decls,
	  var_image, send_image, get_image, rc_image,
	  1, image_termnames,
	  "$Rev$");


status
makeClassImage(Class class)
{ declareClass(class, &image_decls);

  saveStyleClass(class, NAME_external);
  setLoadStoreFunctionClass(class, loadFdImage, storeImage);
  cloneStyleClass(class, NAME_none);	/* just copy reference */

  ImageTable = globalObject(NAME_images, ClassHashTable, toInt(32), EAV);
  standardImages();

  succeed;
}
