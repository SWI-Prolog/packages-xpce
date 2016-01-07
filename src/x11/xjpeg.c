/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  2002-2011, University of Amsterdam
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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Someday (well, mostly this is still true   in XPCE 6.0.6), we translated
all image formats into the internal structure   of  an XPM image to make
XPM handle color-spaces and other complicated   stuff.  This works nice,
but has two drawbacks. XPM is a colormapped format, thus we loose detail
if the source is not colormapped or has a richer colormap (>256 entries)
and there is a lot of unnecessary computation going on.

This is the first module (PPM and  GIF   to  go)  that bypasses this for
`static' visuals. For static visuals  we   can  create  a ZPixmap format
XImage structure that has mask and shift   for  the color components, so
all we need to do is to create   the  XImage, get the pixels one-by-one,
map the brightness values and call XPutPixel().

For some popular formats we also   bypass  XPutPixel(), storing our bits
directly into the XImage structure and saving another 30% (Linux on dual
AMD system).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#include <h/kernel.h>
#include <h/graphics.h>
#include "include.h"
#include <memory.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#include <setjmp.h>

#undef GLOBAL				/* avoid conflicts */
#undef HAVE_STDLIB_H
#include <jpeglib.h>
#include <jerror.h>

#define RESCALE(v, o, n)	((v) * (n) / (o))

extern void	jpeg_iostream_src(j_decompress_ptr cinfo, IOSTREAM* infile);

struct my_jpeg_error_mgr
{ struct jpeg_error_mgr	jerr;
  jmp_buf 		jmp_context;
};


static void
my_exit(j_common_ptr cl)
{ struct jpeg_decompress_struct *cinfo = (struct jpeg_decompress_struct *)cl;
  struct my_jpeg_error_mgr *jerr = (struct my_jpeg_error_mgr *)cinfo->err;

  longjmp(jerr->jmp_context, 1);
}


static XImage *
freshXImage(Display *dsp, int depth, int width, int height)
{ XImage *i;
  int bitmap_pad;

  switch(depth)
  { case 16:
      bitmap_pad = 16;
      break;
    case 24:
    case 32:
      bitmap_pad = 32;
      break;
    default:
      assert(0);
      return NULL;
  }

  /* First pass data and bytes_per_line as 0.  XCreateImage() fills these
     with proper values
  */

  if ( !(i = XCreateImage(dsp,
			  DefaultVisual(dsp, DefaultScreen(dsp)),
			  depth, ZPixmap, 0,
			  NULL,
			  width, height,
			  bitmap_pad, 0)) )
    return NULL;

  if ( !(i->data = malloc(i->bytes_per_line * height)) )
  { XDestroyImage(i);

    return NULL;
  }

  return i;
}

		 /*******************************
		 *	      RESCALING		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Re-scaling and packaging implies a   multiplication,  division and shift
for each color component. As there are   only 256 input values, it seems
worthwhile to use static arrays  and  limit   the  job  to  or'ing three
entries in an array. The difference varies   per processor. On an AMD it
saves about 10% on the whole process.  This   will  get a lot more if we
improve XPutPixel()
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static XImage *current_img;		/* Img we are initialised for */
static int r_b, g_b, b_b;		/* map parameters */

static unsigned long r_map[256];	/* the maps themselves */
static unsigned long g_map[256];
static unsigned long b_map[256];


static void
init_map(unsigned long *map, int bright, int shift)
{ int i;

  for(i=0; i<256; i++)
    map[i] = (RESCALE(i, 255, bright) << shift);
}


static void
init_maps(XImage *img)
{ if ( img != current_img )
  { int r_shift  = shift_for_mask(img->red_mask);
    int g_shift  = shift_for_mask(img->green_mask);
    int b_shift  = shift_for_mask(img->blue_mask);
    int r_bright = img->red_mask   >> r_shift;
    int g_bright = img->green_mask >> g_shift;
    int b_bright = img->blue_mask  >> b_shift;

    if ( !(r_bright==r_b && g_bright==g_b && b_bright == b_b) )
    { init_map(r_map, r_bright, r_shift);
      init_map(g_map, g_bright, g_shift);
      init_map(b_map, b_bright, b_shift);

      r_b = r_bright;
      g_b = g_bright;
      b_b = b_bright;
    }

    current_img = img;
  }
}

#define MKPIXEL(r,g,b) (r_map[r] | g_map[g] | b_map[b])


		 /*******************************
		 *     SCANLINE CONVERSION	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Write a scanline. Convert colour brightness   using  the stuff above and
use specialised versions of the XPutPixel()  to speed-up a little (about
30%). The code for  avoiding  XPutPixel()   is  written  after examining
create.c from the XPM library source (version 3.4k).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
writeRGBScanLine(JSAMPLE *line, int width, int y, XImage *img)
{ int x;
  JSAMPLE *i;

  init_maps(img);

  if ( img->bits_per_pixel > 16 )
  { char *data = img->data + y * img->bytes_per_line;

    for(x=0, i=line; x<width; x++)
    { unsigned long pixel;
      int r = *i++;
      int g = *i++;
      int b = *i++;

      pixel = MKPIXEL(r, g, b);

      if ( img->byte_order == MSBFirst )
      { *data++ = pixel >> 24;
        *data++ = pixel >> 16;
        *data++ = pixel >> 8;
	*data++ = pixel;
      } else
      { *data++ = pixel;
	*data++ = pixel >> 8;
	*data++ = pixel >> 16;
	*data++ = pixel >> 24;
      }
    }
  } else if ( img->bits_per_pixel == 16 )
  { char *data = img->data + y * img->bytes_per_line;

    for(x=0, i=line; x<width; x++)
    { unsigned long pixel;
      int r = *i++;
      int g = *i++;
      int b = *i++;

      pixel = MKPIXEL(r, g, b);

      if ( img->byte_order == MSBFirst )
      { *data++ = pixel >> 8;
	*data++ = pixel;
      } else
      { *data++ = pixel;
	*data++ = pixel >> 8;
      }
    }
  } else
  { for(x=0, i=line; x<width; x++)
    { unsigned long pixel;
      int r = *i++;
      int g = *i++;
      int b = *i++;

      pixel = MKPIXEL(r, g, b);

      XPutPixel(img, x, y, pixel);
    }
  }
}


static void
writeGrayScanLine(JSAMPLE *line, int width, int y, XImage *img)
{ int x;
  JSAMPLE *i;

  init_maps(img);

  for(x=0, i=line; x<width; x++)
  { unsigned long pixel;
    int g = *i++;

    pixel = MKPIXEL(g, g, g);

    XPutPixel(img, x, y, pixel);
  }
}



		 /*******************************
		 *	     READ JPEG		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Try to load a JPEG file directly into memory.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

img_status
staticColourReadJPEGFile(Image image, IOSTREAM *fd, XImage **return_image)
{ struct jpeg_decompress_struct cinfo;
  struct my_jpeg_error_mgr jerr;
  long here = Stell(fd);
  XImage *img = NULL;
  JSAMPLE *line = NULL;
  int y;
  DisplayObj d = image->display;
  DisplayWsXref r;
  img_status rval = IMG_OK;

					/* check enviroment */
  if ( isNil(d) )
    d = CurrentDisplay(image);
  r = d->ws_ref;

  if ( r->depth < 16 )			/* better test? */
    return IMG_NO_STATIC_COLOUR;

					/* setup JPEG error handling */
  cinfo.err = jpeg_std_error((struct jpeg_error_mgr *)&jerr);
  if ( setjmp(jerr.jmp_context) )
  { DEBUG(NAME_image,
	  { char buf[1024];

	    (*jerr.jerr.format_message)((j_common_ptr)&cinfo, buf);
	    Cprintf("JPEG: %s\n", buf);
	  });

    switch(jerr.jerr.msg_code)
    { case JERR_OUT_OF_MEMORY:
	/*rval = IMG_NOMEM;	   Sometimes this is reported uncorrectly*/
	rval = IMG_UNRECOGNISED;
	break;
      case JERR_NO_SOI:
	rval = IMG_UNRECOGNISED;
	break;
      default:
        rval = IMG_INVALID;
    }

    jpeg_destroy_decompress(&cinfo);

    Sseek(fd, here, SEEK_SET);

    return rval;
  }
  jerr.jerr.error_exit = my_exit;

					/* open the image */
  jpeg_create_decompress(&cinfo);
  jpeg_iostream_src(&cinfo, fd);
  jpeg_save_markers(&cinfo, JPEG_COM, 0xffff);
  jpeg_read_header(&cinfo, TRUE);
  jpeg_start_decompress(&cinfo);

  if ( !(line = pceMalloc(cinfo.output_components *
			  cinfo.output_width *
			  sizeof(JSAMPLE))) )
  { rval = IMG_NOMEM;
    goto out;
  }
  if ( !(img = freshXImage(r->display_xref,
			   r->depth,
			   cinfo.output_width,
			   cinfo.output_height)) )
  { rval = IMG_NOMEM;
    goto out;
  }

  for( y=0; cinfo.output_scanline < cinfo.output_height; y++ )
  { jpeg_read_scanlines(&cinfo, &line, 1);

    switch(cinfo.output_components)
    { case 1:
	writeGrayScanLine(line, cinfo.output_width, y, img);
	break;
      case 3:
	writeRGBScanLine(line, cinfo.output_width, y, img);
        break;
      default:
	Cprintf("JPEG: Unsupported: %d output components\n",
		cinfo.output_components);
        rval = IMG_INVALID;
	goto out;
    }
  }

  if ( cinfo.marker_list )
  { jpeg_saved_marker_ptr m;
    Chain ch;

    attributeObject(image, NAME_comment, (ch=newObject(ClassChain, EAV)));

    for(m = cinfo.marker_list; m; m = m->next )
    { if ( m->marker == JPEG_COM )
      { string s;

	if ( str_set_n_ascii(&s, m->data_length, (char*)m->data) )
	  appendChain(ch, StringToString(&s));
      }
    }
  }

  jpeg_finish_decompress(&cinfo);
out:
  if ( line )
    pceFree(line);
  jpeg_destroy_decompress(&cinfo);

  if ( img )
    *return_image = img;

  return rval;
}


