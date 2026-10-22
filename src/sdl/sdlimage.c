/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org
    Copyright (c)  2025-2026, SWI-Prolog Solutions b.v.
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
#include "sdlimage.h"
#include "sdlcolour.h"
#include "sdlstream.h"
#include <SDL3_image/SDL_image.h>
#include <math.h>

static status sdl_surface_to_image(Image image, SDL_Surface *surf0);

cairo_surface_t *
pceImage2CairoSurface(Image image)
{ if ( !image->ws_ref )
  { if ( !XopenImage(image, CurrentDisplay(NIL)) )
      return NULL;
  }
  return image->ws_ref;
}

/**
 * Free resources associated with an Image object.
 *
 * @param image Pointer to the Image object.
 */
void
ws_destroy_image(Image image)
{ cairo_surface_t *s = image->ws_ref;
  if ( s )
  { image->ws_ref = NULL;
    cairo_surface_destroy(s);
  }
}

/**
 * Add image to a file for creating a saved object.  Note this is _not_
 * for saving images to a file.  ws_save_image_file() does that.
 *
 * @param image Pointer to the Image object.
 * @param file File object representing the target file.  This file
 * is open for writing through `file->fd`.
 */
status
ws_store_image(Image image, FileObj file)
{ SDL_Surface *surf = pceImage2SDL_Surface(image);
  status rc = FAIL;

  if ( surf )
  { SDL_IOStream *io = IOSTREAM_to_SDL_IOStream(file->fd);

    if ( io )
    { Sputc('P', file->fd);
      rc = IMG_SavePNG_IO(surf, io, true);
      if ( !rc )
	errorPce(image, NAME_SDL_Error, CtoString(SDL_GetError()));
    } else
    { Sputc('O', file->fd);
    }

    SDL_DestroySurface(surf);
  } else
  { Sputc('O', file->fd);
  }

  return rc;
}

/**
 * Load an image in PNM format.
 *
 * @param image Pointer to the Image object.
 * @param fd IOSTREAM to read from.
 * @return SUCCEED if loading succeeds; otherwise, FAIL.
 */
status
loadPNMImage(Image image, IOSTREAM *fd)
{ assert(image->ws_ref == NULL);

  SDL_IOStream *io = IOSTREAM_to_SDL_IOStream(fd);
  if ( !io )
    fail;

  SDL_Surface *surf0 = IMG_Load_IO(io, true);  /* closes io */
  if ( !surf0 )
  { Cprintf("loadPNMImage(%s): %s\n", pp(image), SDL_GetError());
    fail;
  }

  return sdl_surface_to_image(image, surf0);
}

static void
premultiply_alpha(SDL_Surface *surface, bool *pisbitmap)
{ if ( !SDL_LockSurface(surface) )
    return;

  Uint8 *pixels = surface->pixels;
  int pitch = surface->pitch;
  int width = surface->w;
  int height = surface->h;
  const SDL_PixelFormatDetails *format_details =
    SDL_GetPixelFormatDetails(surface->format);
  bool isbitmap = true;

  for (int y = 0; y < height; y++)
  { Uint32 *row = (Uint32 *)(pixels + y * pitch);
    for (int x = 0; x < width; x++)
    { Uint8 r, g, b, a;
      SDL_GetRGBA(row[x], format_details, NULL, &r, &g, &b, &a);
      if ( isbitmap )
      { if ( !( ((r|g|b) == 0   && a == 255) ||
		((r&g&b) == 255 && a == 0) ) )
	  isbitmap = false;
      }

      r = (r * a) / 255;
      g = (g * a) / 255;
      b = (b * a) / 255;
      row[x] = SDL_MapRGBA(format_details, NULL, r, g, b, a);
    }
  }

  *pisbitmap = isbitmap;
  SDL_UnlockSurface(surface);
}


/**
 * Copy a Cairo surface such that the data is always owned
 * by Cairo.
 */

static cairo_surface_t *
my_cairo_copy_surface(cairo_surface_t *src)
{ int width  = cairo_image_surface_get_width(src);
  int height = cairo_image_surface_get_height(src);
  cairo_surface_t *dst = cairo_image_surface_create(
    CAIRO_FORMAT_ARGB32, width, height);
  if ( dst )
  { cairo_t *cr = cairo_create(dst);
    cairo_set_source_surface(cr, src, 0, 0);
    cairo_paint(cr);
    cairo_destroy(cr);
  }

  return dst;
}

static status
my_cairo_check_surface(Any ctx, cairo_surface_t *s)
{ if ( cairo_surface_status(s) != CAIRO_STATUS_SUCCESS )
  { Cprintf("%s: cairo surface error: %s\n",
	    pp(ctx), cairo_status_to_string(cairo_surface_status(s)));
    fail;
  }
  succeed;
}

/**
 * Convert an SDL_Surface to a cairo-backed Image, consuming surf0.
 *
 * Converts to ARGB8888, premultiplies alpha, wraps in a cairo surface,
 * copies to a cairo-owned buffer, then sets image->kind, size, ws_ref.
 */
static status
sdl_surface_to_image(Image image, SDL_Surface *surf0)
{ SDL_PropertiesID props = SDL_GetSurfaceProperties(surf0);

  if ( props && SDL_HasProperty(props, SDL_PROP_SURFACE_HOTSPOT_X_NUMBER) )
  { Int x = toInt(SDL_GetNumberProperty(props,
					SDL_PROP_SURFACE_HOTSPOT_X_NUMBER, 0));
    Int y = toInt(SDL_GetNumberProperty(props,
					SDL_PROP_SURFACE_HOTSPOT_Y_NUMBER, 0));

    assign(image, hot_spot, newObject(ClassPoint, x, y, EAV));
  }

  SDL_Surface *surf1 = SDL_ConvertSurface(surf0, SDL_PIXELFORMAT_ARGB8888);
  SDL_DestroySurface(surf0);
  if ( !surf1 )
  { Cprintf("Failed to convert %s: %s\n", pp(image), SDL_GetError());
    fail;
  }

  bool isbitmap = false;
  premultiply_alpha(surf1, &isbitmap);
  cairo_surface_t *surf = cairo_image_surface_create_for_data(
    (unsigned char *)surf1->pixels,
    CAIRO_FORMAT_ARGB32,
    surf1->w,
    surf1->h,
    surf1->pitch);
  if ( !my_cairo_check_surface(image, surf) )
  { SDL_DestroySurface(surf1);
    fail;
  }
  cairo_surface_t *final = my_cairo_copy_surface(surf);
  cairo_surface_destroy(surf);
  SDL_DestroySurface(surf1);
  if ( !my_cairo_check_surface(image, final) )
    fail;

  if ( isbitmap )
    DEBUG(NAME_bitmap, Cprintf("%s: bitmap\n", pp(image)));
  assign(image, kind, isbitmap ? NAME_bitmap : NAME_pixmap);
  assign(image->size, w, toInt(cairo_image_surface_get_width(final)));
  assign(image->size, h, toInt(cairo_image_surface_get_height(final)));
  image->ws_ref = final;
  succeed;
}

/**
 * Encode `image` as PNG into a malloc()ed buffer.
 */
bool
ws_image_to_png(Image image, void **data, size_t *size)
{ SDL_Surface *surf = pceImage2SDL_Surface(image);
  bool rc = false;

  if ( !surf )
    return false;

  SDL_IOStream *io = SDL_IOFromDynamicMem();
  if ( io && IMG_SavePNG_IO(surf, io, false) )
  { Sint64 n = SDL_GetIOSize(io);
    void *mem = SDL_GetPointerProperty(SDL_GetIOProperties(io),
				       SDL_PROP_IOSTREAM_DYNAMIC_MEMORY_POINTER,
				       NULL);
    if ( n > 0 && mem && (*data = malloc((size_t)n)) )
    { memcpy(*data, mem, (size_t)n);
      *size = (size_t)n;
      rc = true;
    }
  }
  if ( io )
    SDL_CloseIO(io);
  SDL_DestroySurface(surf);

  return rc;
}

/**
 * Load `image` from encoded data in any format SDL_image supports.
 */
status
ws_image_from_data(Image image, const void *data, size_t size)
{ SDL_IOStream *io = SDL_IOFromConstMem(data, size);
  SDL_Surface *surf = io ? IMG_Load_IO(io, true) : NULL;

  if ( !surf )
    fail;

  ws_destroy_image(image);
  return sdl_surface_to_image(image, surf);	/* consumes surf */
}

		 /*******************************
		 *	    ANIMATIONS		*
		 *******************************/

/* Add `surf` (consumed) as a frame of `a` that is shown for `ms`
 * milliseconds.
 */

static status
add_animation_frame(Animation a, SDL_Surface *surf, Uint64 ms)
{ Image img = newObject(ClassImage, NIL, ONE, ONE, NAME_pixmap, EAV);

  if ( !img )
  { SDL_DestroySurface(surf);
    fail;
  }
  ws_destroy_image(img);
  TRY(sdl_surface_to_image(img, surf));

  Any delay = toInt(ms);
  appendVector(a->frames, 1, (Any *)&img);
  appendVector(a->delays, 1, &delay);

  succeed;
}

#if SDL_IMAGE_VERSION_ATLEAST(3, 4, 0)
/* The metadata of animations that we copy from and to SDL properties */

static const struct metadata_prop
{ const char *prop;			/* SDL_image property */
  size_t      offset;			/* Slot of struct animation */
} metadata_props[] =
{ { IMG_PROP_METADATA_TITLE_STRING,	  offsetof(struct animation, title) },
  { IMG_PROP_METADATA_AUTHOR_STRING,	  offsetof(struct animation, author) },
  { IMG_PROP_METADATA_DESCRIPTION_STRING, offsetof(struct animation, description) },
  { IMG_PROP_METADATA_COPYRIGHT_STRING,	  offsetof(struct animation, copyright) },
  { NULL, 0 }
};

#define METADATA_SLOT(a, mp) ((Any *)((char *)(a) + (mp)->offset))

static void
get_animation_metadata(Animation a, SDL_PropertiesID props)
{ assign(a, loop_count,
	 toInt(SDL_GetNumberProperty(props,
				     IMG_PROP_METADATA_LOOP_COUNT_NUMBER, 0)));

  for(const struct metadata_prop *mp = metadata_props; mp->prop; mp++)
  { const char *s = SDL_GetStringProperty(props, mp->prop, NULL);

    if ( s && *s )
      assignField((Instance)a, METADATA_SLOT(a, mp), UTF8ToString(s));
  }
}

static void
set_animation_metadata(Animation a, SDL_PropertiesID props)
{ SDL_SetNumberProperty(props, IMG_PROP_METADATA_LOOP_COUNT_NUMBER,
			valInt(a->loop_count));

  for(const struct metadata_prop *mp = metadata_props; mp->prop; mp++)
  { CharArray s = *METADATA_SLOT(a, mp);

    if ( notNil(s) )
      SDL_SetStringProperty(props, mp->prop, charArrayToUTF8(s));
  }
}

/* The type of animation in `io`, as IMG_CreateAnimationDecoder_IO()
 * wants it, judged from the data rather than a file name.  NULL if it
 * is not a format that can be animated.
 */

static const char *
animation_type(SDL_IOStream *io)
{ if ( IMG_isPNG(io) )  return "png";
  if ( IMG_isGIF(io) )  return "gif";
  if ( IMG_isWEBP(io) ) return "webp";
  if ( IMG_isAVIF(io) ) return "avif";
  if ( IMG_isANI(io) )  return "ani";

  return NULL;
}

/* Add the frames of the animated image in `io` to `a`.  An image that
 * is not animated becomes a single frame.
 */

static status
load_animation_frames(Animation a, SDL_IOStream *io)
{ IMG_AnimationDecoder *dec = NULL;
  SDL_Surface *single = NULL;
  status rc = FAIL;

  { const char *type = animation_type(io);

    if ( !(type && (dec = IMG_CreateAnimationDecoder_IO(io, false, type))) &&
	 SDL_SeekIO(io, 0, SDL_IO_SEEK_SET) == 0 )
      single = IMG_Load_IO(io, false);
  }

  if ( dec )
  { SDL_PropertiesID props = IMG_GetAnimationDecoderProperties(dec);
    SDL_Surface *frame;
    Uint64 ms;

    if ( props )
      get_animation_metadata(a, props);
    rc = SUCCEED;
    while( rc && IMG_GetAnimationDecoderFrame(dec, &frame, &ms) )
      rc = add_animation_frame(a, frame, ms);
    if ( IMG_GetAnimationDecoderStatus(dec) == IMG_DECODER_STATUS_FAILED )
      rc = FAIL;
    IMG_CloseAnimationDecoder(dec);
  } else if ( single )
  { rc = add_animation_frame(a, single, 0);
  }

  return rc;
}
#else /* SDL_image < 3.4: no metadata and no encoder */

static status
load_animation_frames(Animation a, SDL_IOStream *io)
{ IMG_Animation *anim = IMG_LoadAnimation_IO(io, false);
  status rc = FAIL;

  if ( anim )
  { rc = SUCCEED;
    for(int i=0; rc && i < anim->count; i++)
    { rc = add_animation_frame(a, anim->frames[i], anim->delays[i]);
      anim->frames[i] = NULL;		/* add_animation_frame() owns it */
    }
    IMG_FreeAnimation(anim);
  }

  return rc;
}
#endif

/**
 * Load all frames of the animated image file `a->file`.  A file that
 * is not animated becomes a single frame.
 */
status
ws_load_animation(Animation a)
{ IOSTREAM *fd = NULL;
  SDL_IOStream *io = NULL;
  status rc = FAIL;

  if ( instanceOfObject(a->file, ClassFile) )
  { const char *fname =
      charArrayToFN((CharArray)getOsNameFile((FileObj)a->file));

    io = SDL_IOFromFile(fname, "rb");
  } else if ( (fd = Sopen_object(a->file, "rbr")) )
  { io = IOSTREAM_to_SDL_IOStream(fd);
  }

  if ( io )
  { rc = load_animation_frames(a, io);
    SDL_CloseIO(io);
  }
  if ( fd )
    Sclose(fd);

  if ( rc && valInt(a->frames->size) > 0 )
  { Image first = getElementVector(a->frames, ONE);

    copySize(a->size, first->size);
    succeed;
  }

  return errorPce(a, NAME_cannotLoad, a->file, CtoName(SDL_GetError()));
}

/**
 * Save the frames of `a` as an animated image file.  If `format` is
 * default, the extension of the file selects it.  `quality` is -1 for
 * the default of SDL_image.
 */
#if SDL_IMAGE_VERSION_ATLEAST(3, 4, 0)
status
ws_save_animation(Animation a, FileObj file, Name format, int quality)
{ const char *fname = nameToFN(file->name);
  SDL_PropertiesID props = SDL_CreateProperties();
  IMG_AnimationEncoder *enc;
  bool rc = true;

  SDL_SetStringProperty(props, IMG_PROP_ANIMATION_ENCODER_CREATE_FILENAME_STRING,
			fname);
  if ( notDefault(format) )
    SDL_SetStringProperty(props, IMG_PROP_ANIMATION_ENCODER_CREATE_TYPE_STRING,
			  format == NAME_png ? "apng" : strName(format));
  if ( quality >= 0 )
    SDL_SetNumberProperty(props, IMG_PROP_ANIMATION_ENCODER_CREATE_QUALITY_NUMBER,
			  quality);
  set_animation_metadata(a, props);	/* encoders copy them from props */
  enc = IMG_CreateAnimationEncoderWithProperties(props);
  SDL_DestroyProperties(props);
  if ( !enc )
    return errorPce(a, NAME_cannotSave, file, CtoName(SDL_GetError()));

  for(int i=1; rc && i <= valInt(a->frames->size); i++)
  { Image img = getElementVector(a->frames, toInt(i));
    Int delay = getElementVector(a->delays, toInt(i));
    SDL_Surface *surf = pceImage2SDL_Surface(img);

    rc = ( surf &&
	   IMG_AddAnimationEncoderFrame(enc, surf,
					isInteger(delay) ? valInt(delay) : 0) );
    if ( surf )
      SDL_DestroySurface(surf);
  }
  if ( !IMG_CloseAnimationEncoder(enc) )
    rc = false;

  if ( !rc )
    return errorPce(a, NAME_cannotSave, file, CtoName(SDL_GetError()));

  succeed;
}

/**
 * Chain with the names of the formats animation->save can write,
 * found by encoding a 1x1 frame in memory.
 */
Chain
ws_animation_save_formats(void)
{ static const char *formats[] = { "png", "gif", "webp", "avif", "ani", NULL };
  Chain ch = answerObject(ClassChain, EAV);
  SDL_Surface *surf = SDL_CreateSurface(1, 1, SDL_PIXELFORMAT_ARGB8888);

  if ( surf )
  { for(const char **f = formats; *f; f++)
    { SDL_IOStream *io = SDL_IOFromDynamicMem();
      const char *type = streq(*f, "png") ? "apng" : *f;
      IMG_AnimationEncoder *enc;

      if ( io && (enc = IMG_CreateAnimationEncoder_IO(io, true, type)) )
      { bool ok = IMG_AddAnimationEncoderFrame(enc, surf, 0);

	if ( IMG_CloseAnimationEncoder(enc) && ok )
	  appendChain(ch, CtoName(*f));
      } else if ( io )
      { SDL_CloseIO(io);
      }
    }
    SDL_DestroySurface(surf);
  }

  return ch;
}
#else
status
ws_save_animation(Animation a, FileObj file, Name format, int quality)
{ return errorPce(a, NAME_cannotSave, file,
		  CtoName("saving animations requires SDL_image 3.4"));
}

Chain
ws_animation_save_formats(void)
{ return answerObject(ClassChain, EAV);
}
#endif

/* The standard Huffman tables of JPEG (ITU T.81, Annex K.3) as a DHT
 * segment.  The MJPG of many cameras leaves them out and expects the
 * decoder to use these.  libjpeg does, but the decoder SDL_image uses
 * without it (e.g., on Windows) does not.
 */

static const Uint8 std_dht[] =
{ 0xFF, 0xC4, 0x01, 0xA2,
					/* DC luminance */
  0x00, 0x00, 0x01, 0x05, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x00, 0x00,
  0x00, 0x00, 0x00, 0x00, 0x00,
  0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0A, 0x0B,
					/* DC chrominance */
  0x01, 0x00, 0x03, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01,
  0x00, 0x00, 0x00, 0x00, 0x00,
  0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0A, 0x0B,
					/* AC luminance */
  0x10, 0x00, 0x02, 0x01, 0x03, 0x03, 0x02, 0x04, 0x03, 0x05, 0x05, 0x04,
  0x04, 0x00, 0x00, 0x01, 0x7D,
  0x01, 0x02, 0x03, 0x00, 0x04, 0x11, 0x05, 0x12, 0x21, 0x31, 0x41, 0x06,
  0x13, 0x51, 0x61, 0x07, 0x22, 0x71, 0x14, 0x32, 0x81, 0x91, 0xA1, 0x08,
  0x23, 0x42, 0xB1, 0xC1, 0x15, 0x52, 0xD1, 0xF0, 0x24, 0x33, 0x62, 0x72,
  0x82, 0x09, 0x0A, 0x16, 0x17, 0x18, 0x19, 0x1A, 0x25, 0x26, 0x27, 0x28,
  0x29, 0x2A, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39, 0x3A, 0x43, 0x44, 0x45,
  0x46, 0x47, 0x48, 0x49, 0x4A, 0x53, 0x54, 0x55, 0x56, 0x57, 0x58, 0x59,
  0x5A, 0x63, 0x64, 0x65, 0x66, 0x67, 0x68, 0x69, 0x6A, 0x73, 0x74, 0x75,
  0x76, 0x77, 0x78, 0x79, 0x7A, 0x83, 0x84, 0x85, 0x86, 0x87, 0x88, 0x89,
  0x8A, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97, 0x98, 0x99, 0x9A, 0xA2, 0xA3,
  0xA4, 0xA5, 0xA6, 0xA7, 0xA8, 0xA9, 0xAA, 0xB2, 0xB3, 0xB4, 0xB5, 0xB6,
  0xB7, 0xB8, 0xB9, 0xBA, 0xC2, 0xC3, 0xC4, 0xC5, 0xC6, 0xC7, 0xC8, 0xC9,
  0xCA, 0xD2, 0xD3, 0xD4, 0xD5, 0xD6, 0xD7, 0xD8, 0xD9, 0xDA, 0xE1, 0xE2,
  0xE3, 0xE4, 0xE5, 0xE6, 0xE7, 0xE8, 0xE9, 0xEA, 0xF1, 0xF2, 0xF3, 0xF4,
  0xF5, 0xF6, 0xF7, 0xF8, 0xF9, 0xFA,
					/* AC chrominance */
  0x11, 0x00, 0x02, 0x01, 0x02, 0x04, 0x04, 0x03, 0x04, 0x07, 0x05, 0x04,
  0x04, 0x00, 0x01, 0x02, 0x77,
  0x00, 0x01, 0x02, 0x03, 0x11, 0x04, 0x05, 0x21, 0x31, 0x06, 0x12, 0x41,
  0x51, 0x07, 0x61, 0x71, 0x13, 0x22, 0x32, 0x81, 0x08, 0x14, 0x42, 0x91,
  0xA1, 0xB1, 0xC1, 0x09, 0x23, 0x33, 0x52, 0xF0, 0x15, 0x62, 0x72, 0xD1,
  0x0A, 0x16, 0x24, 0x34, 0xE1, 0x25, 0xF1, 0x17, 0x18, 0x19, 0x1A, 0x26,
  0x27, 0x28, 0x29, 0x2A, 0x35, 0x36, 0x37, 0x38, 0x39, 0x3A, 0x43, 0x44,
  0x45, 0x46, 0x47, 0x48, 0x49, 0x4A, 0x53, 0x54, 0x55, 0x56, 0x57, 0x58,
  0x59, 0x5A, 0x63, 0x64, 0x65, 0x66, 0x67, 0x68, 0x69, 0x6A, 0x73, 0x74,
  0x75, 0x76, 0x77, 0x78, 0x79, 0x7A, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87,
  0x88, 0x89, 0x8A, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97, 0x98, 0x99, 0x9A,
  0xA2, 0xA3, 0xA4, 0xA5, 0xA6, 0xA7, 0xA8, 0xA9, 0xAA, 0xB2, 0xB3, 0xB4,
  0xB5, 0xB6, 0xB7, 0xB8, 0xB9, 0xBA, 0xC2, 0xC3, 0xC4, 0xC5, 0xC6, 0xC7,
  0xC8, 0xC9, 0xCA, 0xD2, 0xD3, 0xD4, 0xD5, 0xD6, 0xD7, 0xD8, 0xD9, 0xDA,
  0xE2, 0xE3, 0xE4, 0xE5, 0xE6, 0xE7, 0xE8, 0xE9, 0xEA, 0xF2, 0xF3, 0xF4,
  0xF5, 0xF6, 0xF7, 0xF8, 0xF9, 0xFA
};

/* True if the JPEG data `d` of `len` bytes defines Huffman tables
 * before its first scan.
 */

static bool
jpeg_has_dht(const Uint8 *d, size_t len)
{ size_t i = 2;				/* after SOI */

  while( i+4 <= len && d[i] == 0xFF )
  { Uint8 marker = d[i+1];

    if ( marker == 0xC4 )
      return true;
    if ( marker == 0xDA )
      return false;
    i += 2 + ((size_t)d[i+2]<<8 | d[i+3]);
  }

  return false;
}

/* Decode the JPEG of an MJPG camera frame, adding the standard Huffman
 * tables if it has none.
 */

SDL_Surface *
ws_decode_mjpg(const Uint8 *d, size_t len)
{ Uint8 *buf = NULL;

  if ( len > 2 && !jpeg_has_dht(d, len) )
  { if ( !(buf = SDL_malloc(len + sizeof(std_dht))) )
      return NULL;
    memcpy(buf, d, 2);				/* SOI */
    memcpy(buf+2, std_dht, sizeof(std_dht));
    memcpy(buf+2+sizeof(std_dht), d+2, len-2);
    d = buf;
    len += sizeof(std_dht);
  }

  SDL_IOStream *io = SDL_IOFromConstMem(d, len);
  SDL_Surface *surf = io ? IMG_Load_IO(io, true) : NULL;

  SDL_free(buf);
  return surf;
}

/**
 * Replace the pixels of `image` by those of `surf`, which may have any
 * format, e.g., the YUV format of a camera.  The cairo surface of the
 * image is reused if it has the right size.  An MJPG surface, as many
 * cameras deliver, holds a JPEG image that SDL cannot convert: its
 * `pixels` points at the JPEG data and its `pitch` is the length of
 * the data.  We decode it using SDL_image.
 */
status
ws_set_image_pixels(Image image, SDL_Surface *surf)
{ SDL_Surface *decoded = NULL;

  if ( surf->format == SDL_PIXELFORMAT_MJPG )
  { if ( !(decoded = ws_decode_mjpg(surf->pixels, (size_t)surf->pitch)) )
    { Cprintf("Failed to decode MJPG frame for %s: %s\n",
	      pp(image), SDL_GetError());
      fail;
    }
    surf = decoded;
  }

  SDL_Surface *argb = SDL_ConvertSurface(surf, SDL_PIXELFORMAT_ARGB8888);

  if ( decoded )
    SDL_DestroySurface(decoded);
  if ( !argb )
  { Cprintf("Failed to convert frame for %s: %s\n",
	    pp(image), SDL_GetError());
    fail;
  }

  cairo_surface_t *cs = image->ws_ref;
  if ( !cs ||
       cairo_image_surface_get_width(cs)  != argb->w ||
       cairo_image_surface_get_height(cs) != argb->h )
  { ws_destroy_image(image);
    cs = cairo_image_surface_create(CAIRO_FORMAT_ARGB32, argb->w, argb->h);
    if ( !my_cairo_check_surface(image, cs) )
    { SDL_DestroySurface(argb);
      fail;
    }
    image->ws_ref = cs;
  }

  cairo_surface_flush(cs);
  unsigned char *dst = cairo_image_surface_get_data(cs);
  int dst_stride = cairo_image_surface_get_stride(cs);
  for(int y=0; y<argb->h; y++)
    memcpy(dst+y*dst_stride, (Uint8*)argb->pixels+y*argb->pitch,
	   (size_t)argb->w*4);
  cairo_surface_mark_dirty(cs);

  int w = argb->w, h = argb->h;
  SDL_DestroySurface(argb);
  assign(image, kind, NAME_pixmap);

  return pixelsChangedImage(image, w, h);
}

/**
 * Load an image from its associated file path.
 *
 * @param image Pointer to the Image object.
 * @return SUCCEED if loading succeeds; otherwise, FAIL.
 */
status
ws_load_image_file(Image image)
{ IOSTREAM *fd;

  assert(image->ws_ref == NULL);

  int req_w = valInt(image->size->w);
  int req_h = valInt(image->size->h);

  if ( instanceOfObject(image->file, ClassFile) )
  { FileObj f = (FileObj)image->file;
    char *fname = charArrayToFN((CharArray)getOsNameFile(f));
    /* Detect SVG by extension and use the size-aware loader so that
     * aspect ratio is preserved when only one dimension is specified.
     * SDL auto-computes the missing dimension when it receives 0.
     */
    size_t len = strlen(fname);
    bool is_svg = ( len > 4 &&
		    strcasecmp(fname + len - 4, ".svg") == 0 );
    if ( is_svg )
    { SDL_IOStream *io = SDL_IOFromFile(fname, "rb");
      if ( !io )
      { Cprintf("Failed to open %s: %s\n", fname, SDL_GetError());
	fail;
      }
      SDL_Surface *surf0 = IMG_LoadSizedSVG_IO(io, req_w, req_h);
      SDL_CloseIO(io);
      if ( !surf0 )
      { Cprintf("Failed to load SVG %s: %s\n", fname, SDL_GetError());
	fail;
      }
      return sdl_surface_to_image(image, surf0);
    }
    SDL_Surface *surf0 = IMG_Load(fname);
    if ( !surf0 )
    { Cprintf("Failed to load %s: %s\n", fname, SDL_GetError());
      fail;
    }
    return sdl_surface_to_image(image, surf0);
  } else if ( (fd = Sopen_object(image->file, "rbr")) )
  { SDL_IOStream *io = IOSTREAM_to_SDL_IOStream(fd);
    status rc;
    if ( io && IMG_isSVG(io) )
    { SDL_Surface *surf0 = IMG_LoadSizedSVG_IO(io, req_w, req_h);
      SDL_CloseIO(io);
      Sclose(fd);
      if ( !surf0 )
      { Cprintf("Failed to load SVG %s: %s\n", pp(image), SDL_GetError());
	fail;
      }
      return sdl_surface_to_image(image, surf0);
    }
    if ( io )
      SDL_CloseIO(io);
    Sseek(fd, 0, SIO_SEEK_SET);
    rc = loadPNMImage(image, fd);
    Sclose(fd);
    return rc;
  } else
  { fail;
  }
}

/**
 * Create an image from XPM (X PixMap) data.
 *
 * @param image Pointer to the Image object.
 * @param xpm Array of XPM strings.
 * @param d Display on which to create the image.
 * @return SUCCEED if creation succeeds; otherwise, FAIL.
 */

static uint32_t *
parse_xpm_1_data(int width, int height, int ncolors, char **xpm)
{ uint32_t palette[256] = {0};

  for (int i = 0; i < ncolors; i++)
  { char symbol;
    char color[32];
    sscanf(xpm[1 + i], "%c c %s", &symbol, color);

    uint32_t rgb = 0xFF000000;  // default alpha

    if (strcmp(color, "None") == 0)
    { rgb = 0x00000000;
    } else if (color[0] == '#')
    { unsigned int r, g, b;
      sscanf(color + 1, "%02x%02x%02x", &r, &g, &b);
      rgb |= (r << 16) | (g << 8) | b;
    } else
    { Int Rgb = getNamedRGB(CtoName(color));
      if ( Rgb )
	rgb |= valInt(Rgb);
      else
	Cprintf("XPM: Unknown colour name: %s\n", color);
    }
    palette[(unsigned char)symbol] = rgb;
  }

  // Allocate pixel buffer
  uint32_t *pixels = calloc(width * height, sizeof(uint32_t));

  for (int y = 0; y < height; y++)
  { const char *row = xpm[1 + ncolors + y];
    for (int x = 0; x < width; x++) {
      pixels[y * width + x] = palette[(unsigned char)row[x]];
    }
  }

  return pixels;
}

static uint32_t *
parse_xpm_2_data(int width, int height, int ncolors, char **xpm)
{ uint32_t *palette[256] = {0};

  for (int i = 0; i < ncolors; i++)
  { unsigned char c1 = (unsigned char)xpm[1 + i][0];
    unsigned char c2 = (unsigned char)xpm[1 + i][1];
    char color[32];
    sscanf(xpm[1 + i]+2, " c %s", color);

    uint32_t rgb = 0xFF000000;  // default alpha

    if (strcmp(color, "None") == 0)
    { rgb = 0x00000000;
    } else if (color[0] == '#')
    { unsigned int r, g, b;
      sscanf(color + 1, "%02x%02x%02x", &r, &g, &b);
      rgb |= (r << 16) | (g << 8) | b;
    } else
    { Int Rgb = getNamedRGB(CtoName(color));
      if ( Rgb )
	rgb |= valInt(Rgb);
      else
	Cprintf("XPM: Unknown colour name: %s\n", color);
    }
    if ( !palette[c2] )
      palette[c2] = calloc(256, sizeof(uint32_t));

    palette[c2][c1] = rgb;
  }

  uint32_t *pixels = calloc(width * height, sizeof(uint32_t));
  for (int y = 0; y < height; y++)
  { const char *row = xpm[1 + ncolors + y];
    for (int x = 0; x < width; x++)
    { unsigned char c1 = (unsigned char)row[x*2];
      unsigned char c2 = (unsigned char)row[x*2+1];

      pixels[y * width + x] = palette[c2][c1];
    }
  }

  for(int i=0; i<256; i++)
  { if ( palette[i] )
      free(palette[i]);
  }

  return pixels;
}


status
ws_create_image_from_xpm_data(Image image, char **xpm, DisplayObj d)
{ int width, height, ncolors, cpp;
  sscanf(xpm[0], "%d %d %d %d", &width, &height, &ncolors, &cpp);

  assert(image->ws_ref == NULL);

  DEBUG(NAME_xpm,
	Cprintf("Parsing XPM image of %dx%d, %d colors, cpp=%d\n",
		width, height, ncolors, cpp));

  uint32_t *pixels = NULL;
  if ( cpp == 1 )
    pixels = parse_xpm_1_data(width, height, ncolors, xpm);
  else if ( cpp == 2 )
    pixels =parse_xpm_2_data(width, height, ncolors, xpm);
  else
    Cprintf("XPM: No support for %d cpp images\n", cpp);

  if ( pixels )
  { int stride = width * 4;
    cairo_surface_t *surf = cairo_image_surface_create_for_data(
      (unsigned char *)pixels, CAIRO_FORMAT_ARGB32, width, height, stride);

    image->ws_ref = surf;
    succeed;
  } else
    fail;
}

/**
 * Convert an XPCE image to an SDL surface.
 */

SDL_Surface *
pceImage2SDL_Surface(Image image)
{ cairo_surface_t *cairo_surf = pceImage2CairoSurface(image);

  if ( cairo_surf )
  { int width = cairo_image_surface_get_width(cairo_surf);
    int height = cairo_image_surface_get_height(cairo_surf);
    int stride = cairo_image_surface_get_stride(cairo_surf);
    unsigned char *data = cairo_image_surface_get_data(cairo_surf);

    SDL_Surface *sdl_surf = SDL_CreateSurface(width, height,
					      SDL_PIXELFORMAT_ARGB8888);
    if ( !sdl_surf )
    { Cprintf("Failed to create SDL surface %dx%x: %s\n",
	      width, height, SDL_GetError());
      return NULL;
    }

    SDL_LockSurface(sdl_surf);
    for (int y = 0; y < height; ++y)
    { uint32_t *dst_row = (uint32_t *)((uint8_t *)sdl_surf->pixels +
				       y * sdl_surf->pitch);
      uint32_t *src_row = (uint32_t *)(data + y * stride);
      for (int x = 0; x < width; ++x)	/* Cairo is premultiplied */
	dst_row[x] = unpremultiply_pixel(src_row[x]);
    }
    SDL_UnlockSurface(sdl_surf);

    if ( notNil(image->hot_spot) )	/* used by CUR and ANI files */
    { SDL_PropertiesID props = SDL_GetSurfaceProperties(sdl_surf);

      SDL_SetNumberProperty(props, SDL_PROP_SURFACE_HOTSPOT_X_NUMBER,
			    valInt(image->hot_spot->x));
      SDL_SetNumberProperty(props, SDL_PROP_SURFACE_HOTSPOT_Y_NUMBER,
			    valInt(image->hot_spot->y));
    }

    return sdl_surf;
  }

  return NULL;
}

/**
 * Save an image to a file in a specified format.
 *
 * @param image Pointer to the Image object.
 * @param into Target source/sink.
 * @param fmt Format name (e.g., "png").
 * @return SUCCEED if saving succeeds; otherwise, FAIL.
 */
/* The formats image->save writes, with the file name extensions that
 * select them if the format is not given.
 */

static const struct save_format
{ const char *name;			/* as in image->save */
  const char *extensions;		/* space separated */
} save_formats[] =
{ { "png",  "png apng" },
  { "jpeg", "jpg jpeg jpe jfif" },
  { "webp", "webp" },
  { "avif", "avif" },
  { "gif",  "gif" },
  { "bmp",  "bmp" },
  { "tga",  "tga" },
  { "ico",  "ico" },
  { "cur",  "cur" },
  { NULL,   NULL }
};

/* The format for saving to `file`, as selected by its extension; png
 * if the extension is unknown.
 */

static const char *
save_format_from_file(const char *file)
{ const char *ext = strrchr(file, '.');

  if ( ext && !strpbrk(ext, "/\\") )
  { size_t len = strlen(++ext);

    for(const struct save_format *f = save_formats; f->name; f++)
    { for(const char *e = f->extensions; *e; )
      { size_t elen = strcspn(e, " ");

	if ( elen == len && strncasecmp(e, ext, len) == 0 )
	  return f->name;
	e += elen;
	if ( *e )
	  e++;
      }
    }
  }

  return "png";
}

/* IMG_SaveTyped_IO() for SDL_image before 3.4, which only writes
 * PNG, JPEG and AVIF.  SDL itself writes BMP.
 */

#if !SDL_IMAGE_VERSION_ATLEAST(3, 4, 0)
static bool
IMG_SaveTyped_IO(SDL_Surface *surf, SDL_IOStream *io, bool closeio,
		 const char *format)
{ if ( strcmp(format, "png") == 0 )
    return IMG_SavePNG_IO(surf, io, closeio);
  if ( strcmp(format, "jpeg") == 0 )
    return IMG_SaveJPG_IO(surf, io, closeio, 90);
  if ( strcmp(format, "avif") == 0 )
    return IMG_SaveAVIF_IO(surf, io, closeio, 90);
  if ( strcmp(format, "bmp") == 0 )
    return SDL_SaveBMP_IO(surf, io, closeio);

  if ( closeio )
    SDL_CloseIO(io);
  return SDL_SetError("Saving %s images requires SDL_image 3.4", format);
}
#endif

/* Save `surf` to the file `fname` in `format`.  JPEG, WebP and AVIF
 * use `quality` (0..100); IMG_SaveTyped_IO() would use 90.
 */

static bool
save_surface(SDL_Surface *surf, const char *fname, const char *format,
	     int quality)
{ if ( strcmp(format, "jpeg") == 0 )
    return IMG_SaveJPG(surf, fname, quality);
#if SDL_IMAGE_VERSION_ATLEAST(3, 4, 0)
  if ( strcmp(format, "webp") == 0 )
    return IMG_SaveWEBP(surf, fname, (float)quality);
#endif
  if ( strcmp(format, "avif") == 0 )
    return IMG_SaveAVIF(surf, fname, quality);

  SDL_IOStream *io = SDL_IOFromFile(fname, "wb");

  return io && IMG_SaveTyped_IO(surf, io, true, format);
}

/**
 * Chain with the names of the formats image->save can write.  Which
 * formats SDL_image writes depends on how it was built, which we find
 * out by saving a 1x1 image in memory.
 */
Chain
ws_image_save_formats(void)
{ Chain ch = answerObject(ClassChain, EAV);
  SDL_Surface *surf = SDL_CreateSurface(1, 1, SDL_PIXELFORMAT_ARGB8888);

  if ( surf )
  { for(const struct save_format *f = save_formats; f->name; f++)
    { SDL_IOStream *io = SDL_IOFromDynamicMem();

      if ( io && IMG_SaveTyped_IO(surf, io, true, f->name) )
	appendChain(ch, CtoName(f->name));
    }
    SDL_DestroySurface(surf);
  }

  return ch;
}

status
ws_save_image_file(Image image, SourceSink into, Name fmt, int quality)
{ SDL_Surface *surf = pceImage2SDL_Surface(image);
  status rc = FAIL;

  if ( surf )
  { if ( instanceOfObject(into, ClassFile) )
    { FileObj file = (FileObj)into;
      if ( file->fd == NULL )
      { const char *fname = nameToFN(file->name);
	const char *format = ( isDefault(fmt)
			       ? save_format_from_file(fname)
			       : strName(fmt) );

	if ( !(rc = save_surface(surf, fname, format, quality)) )
	  Cprintf("Cannot save %s as %s to %s: %s\n",
		  pp(image), format, fname, SDL_GetError());
      } else
      { Cprintf("Cannot save %s to %s: file is open\n",
		pp(image), pp(file));
      }
    } else
    { Cprintf("Cannot save %s to %s: Can only save to a file\n",
	      pp(image), pp(into));
    }

    SDL_DestroySurface(surf);
    return rc;
  }

  fail;
}

/**
 * Open (realize) the image in the context of a display, possibly scaling.
 *
 * @param image Pointer to the Image object.
 * @param d Target display.
 * @param scale Scaling factor.
 * @return SUCCEED on success; otherwise, FAIL.
 */
status
ws_open_image(Image image)
{ if ( !image->ws_ref )
  { int w = valInt(image->size->w);
    int h = valInt(image->size->h);

    cairo_surface_t *surf =
      cairo_image_surface_create(CAIRO_FORMAT_ARGB32, w, h);

    image->ws_ref = surf;
    d_image(image, 0, 0, w, h);
    r_clear(0, 0, w, h);
    d_done();
  }

  succeed;
}

/**
 * Close (unrealize) the image from the display context.
 *
 * @param image Pointer to the Image object.
 * @param d Target display.
 */
void
ws_close_image(Image image)
{ if ( image->ws_ref )
  { cairo_surface_destroy(image->ws_ref);
    image->ws_ref = NULL;
  }
}

/**
 * Create a new scaled version of the image.
 *
 * @param image Pointer to the source Image.
 * @param w Desired width.
 * @param h Desired height.
 * @return A new Image object; NULL on failure.
 */
Image
ws_scale_image(Image image, int w, int h)
{ if ( !image->ws_ref && !XopenImage(image, CurrentDisplay(NIL)) )
    return NULL;

  int src_w = cairo_image_surface_get_width(image->ws_ref);
  int src_h = cairo_image_surface_get_height(image->ws_ref);

  cairo_surface_t *dst = cairo_image_surface_create(CAIRO_FORMAT_ARGB32, w, h);
  cairo_t *cr = cairo_create(dst);
  cairo_scale(cr, (double)w / src_w, (double)h / src_h);
  cairo_set_source_surface(cr, image->ws_ref, 0, 0);
  cairo_pattern_set_filter(cairo_get_source(cr), CAIRO_FILTER_BILINEAR);
  cairo_paint(cr);
  cairo_destroy(cr);

  Image scaled = answerObject(ClassImage, NIL, toInt(w), toInt(h),
			      NAME_pixmap, EAV);
  scaled->ws_ref = dst;
  answer(scaled);
}

/**
 * Replace the pixels of `image` by a copy of those of `src`, including
 * their transparency.  `image` gets the size of `src`.
 */
status
ws_copy_image(Image image, Image src)
{ if ( !src->ws_ref && !XopenImage(src, CurrentDisplay(NIL)) )
    fail;

  cairo_surface_t *from = src->ws_ref;
  int w = cairo_image_surface_get_width(from);
  int h = cairo_image_surface_get_height(from);
  cairo_surface_t *dst = cairo_image_surface_create(CAIRO_FORMAT_ARGB32, w, h);

  if ( !my_cairo_check_surface(image, dst) )
    fail;

  cairo_t *cr = cairo_create(dst);
  cairo_set_operator(cr, CAIRO_OPERATOR_SOURCE);
  cairo_set_source_surface(cr, from, 0, 0);
  cairo_paint(cr);
  cairo_destroy(cr);

  ws_destroy_image(image);
  image->ws_ref = dst;
  assign(image->size, w, toInt(w));
  assign(image->size, h, toInt(h));

  succeed;
}

/**
 * Create a rotated version of the image.
 *
 * @param image Pointer to the source Image.
 * @param angle Rotation angle in degrees.
 * @return A new Image object; NULL on failure.
 */
Image
ws_rotate_image(Image image, float angle)
{ if ( !image->ws_ref && !XopenImage(image, CurrentDisplay(NIL)) )
    return NULL;

  double rad = angle * M_PI / 180.0;
  double cosA = fabs(cos(rad));
  double sinA = fabs(sin(rad));
  int src_w = cairo_image_surface_get_width(image->ws_ref);
  int src_h = cairo_image_surface_get_height(image->ws_ref);
  int dst_w = (int)ceil(src_w * cosA + src_h * sinA);
  int dst_h = (int)ceil(src_w * sinA + src_h * cosA);

  cairo_surface_t *dst =
    cairo_image_surface_create(CAIRO_FORMAT_ARGB32, dst_w, dst_h);
  cairo_t *cr = cairo_create(dst);

  /* Transparent background */
  cairo_set_operator(cr, CAIRO_OPERATOR_CLEAR);
  cairo_paint(cr);
  cairo_set_operator(cr, CAIRO_OPERATOR_OVER);

  /* Rotate around the centre of the destination, painting the source
   * centred there.
   */
  cairo_translate(cr, dst_w / 2.0, dst_h / 2.0);
  cairo_rotate(cr, rad);
  cairo_set_source_surface(cr, image->ws_ref, -src_w / 2.0, -src_h / 2.0);
  cairo_pattern_set_filter(cairo_get_source(cr), CAIRO_FILTER_BILINEAR);
  cairo_paint(cr);
  cairo_destroy(cr);

  Image rotated = answerObject(ClassImage, NIL, toInt(dst_w), toInt(dst_h),
			       NAME_pixmap, EAV);
  rotated->ws_ref = dst;
  answer(rotated);
}

/**
 * Create a greyscale version of the image.
 *
 * @param image Pointer to the source Image.
 * @return A new monochrome Image; NULL on failure.
 */
Image
ws_grayscale_image(Image image)
{ if ( !image->ws_ref && !XopenImage(image, CurrentDisplay(NIL)) )
    fail;

  Int w = image->size->w;
  Int h = image->size->h;
  Image img_gray = answerObject(ClassImage, NIL, w, h, NAME_pixmap, EAV);
  cairo_surface_t *gray =
    cairo_image_surface_create(CAIRO_FORMAT_ARGB32, valInt(w), valInt(h));
  cairo_surface_t *orig = image->ws_ref;

  int stride = cairo_image_surface_get_stride(orig);
  unsigned char *src = cairo_image_surface_get_data(orig);
  unsigned char *dst = cairo_image_surface_get_data(gray);

  assert(valInt(w) == cairo_image_surface_get_width(orig));
  assert(valInt(h) == cairo_image_surface_get_height(orig));

  for(int y = 0; y < valInt(h); y++)
  { for(int x = 0; x < valInt(w); x++)
    { unsigned char *s = src + y * stride + x * 4;
      unsigned char *d = dst + y * stride + x * 4;

      unsigned char a = s[3];
      if (a == 0)
      { // Fully transparent, keep transparent
	d[0] = d[1] = d[2] = 0;
	d[3] = 0;
      } else
      { // Convert to grayscale using luminosity method
	uint8_t r = s[2], g = s[1], b = s[0]; // BGRA
	uint8_t G = (uint8_t)(0.3 * r + 0.59 * g + 0.11 * b);

	d[0] = d[1] = d[2] = G;  // B = G = R
	d[3] = a;
      }
    }
  }

  cairo_surface_mark_dirty(gray);
  img_gray->ws_ref = gray;

  answer(img_gray);
}

status
ws_has_alpha_image(Image image)
{ if ( !image->ws_ref && !XopenImage(image, CurrentDisplay(NIL)) )
    fail;

  cairo_surface_t *surf = image->ws_ref;
  if ( cairo_image_surface_get_format(surf) != CAIRO_FORMAT_ARGB32 )
    fail;

  int w      = cairo_image_surface_get_width(surf);
  int h      = cairo_image_surface_get_height(surf);
  int stride = cairo_image_surface_get_stride(surf);
  unsigned char *data = cairo_image_surface_get_data(surf);

  for(int y = 0; y < h; y++)
  { unsigned char *row = data + y * stride;
    for(int x = 0; x < w; x++)
    { if ( row[x*4 + 3] != 255 )
	succeed;
    }
  }

  fail;
}
