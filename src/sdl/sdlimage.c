/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org
    Copyright (c)  2025, SWI-Prolog Solutions b.v.
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
#include <SDL3_image/SDL_image.h>

/**
 * Initialize internal state for a new Image object.
 *
 * @param image Pointer to the Image object.
 */
void
ws_init_image(Image image)
{
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
 * Store an image to a file.
 *
 * @param image Pointer to the Image object.
 * @param file File object representing the target file.
 * @return SUCCEED if the image is stored successfully; otherwise, FAIL.
 */
status
ws_store_image(Image image, FileObj file)
{
    return SUCCEED;
}

/**
 * Load an image in XImage format.
 *
 * @param image Pointer to the Image object.
 * @param fd IOSTREAM to read from.
 * @return SUCCEED if loading succeeds; otherwise, FAIL.
 */
status
loadXImage(Image image, IOSTREAM *fd)
{
    return SUCCEED;
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
{
    return SUCCEED;
}

/**
 * Load an image in the legacy XPCE format.
 *
 * @param image Pointer to the Image object.
 * @param fd IOSTREAM to read from.
 * @return SUCCEED if loading succeeds; otherwise, FAIL.
 */
status
ws_load_old_image(Image image, IOSTREAM *fd)
{
    return SUCCEED;
}

static void
premultiply_alpha(SDL_Surface *surface)
{ if ( !SDL_LockSurface(surface) )
    return;

  Uint8 *pixels = surface->pixels;
  int pitch = surface->pitch;
  int width = surface->w;
  int height = surface->h;
  const SDL_PixelFormatDetails *format_details =
    SDL_GetPixelFormatDetails(surface->format);

  for (int y = 0; y < height; y++)
  { Uint32 *row = (Uint32 *)(pixels + y * pitch);
    for (int x = 0; x < width; x++)
    { Uint8 r, g, b, a;
      SDL_GetRGBA(row[x], format_details, NULL, &r, &g, &b, &a);
      r = (r * a) / 255;
      g = (g * a) / 255;
      b = (b * a) / 255;
      row[x] = SDL_MapRGBA(format_details, NULL, r, g, b, a);
    }
  }

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
 * Load an image from its associated file path.
 *
 * @param image Pointer to the Image object.
 * @return SUCCEED if loading succeeds; otherwise, FAIL.
 */
status
ws_load_image_file(Image image)
{ assert(image->ws_ref == NULL);
  SDL_Surface *surf0 = NULL;

  if ( instanceOfObject(image->file, ClassFile) )
  { FileObj f = (FileObj)image->file;
    char *fname = charArrayToFN((CharArray)f->path);
    surf0 = IMG_Load(fname);
    if ( !surf0 )
    { Cprintf("Failed to load %s: %s\n", fname, SDL_GetError());
      fail;
    }
  } else
  { IOSTREAM *fd;
    if ( !(fd = Sopen_object(image->file, "rbr")) )
      fail;
    int64_t size = Ssize(fd);
    if ( size != -1 )
    { char *data = malloc(size);
      if ( data )
      { fd->encoding = ENC_OCTET;
	Sfread(data, 1, size, fd);
	Sclose(fd);
	SDL_IOStream *io = SDL_IOFromConstMem(data, size);
	surf0 = IMG_Load_IO(io, true);
	if ( !surf0 )
	{ Cprintf("Failed to load image from %s: %s\n",
		  pp(image->file), SDL_GetError());
	  fail;
	}
      } else
      { assert(0);
      }
    } else
    { Cprintf("Cannot load images from %s yet\n", pp(image->file));
      fail;
    }
  }

  SDL_Surface *surf1 = SDL_ConvertSurface(surf0,
					  SDL_PIXELFORMAT_ARGB8888);
  SDL_DestroySurface(surf0);
  if ( !surf1 )
  { Cprintf("Failed to convert %s: %s\n", pp(image), SDL_GetError());
    fail;
  }
  premultiply_alpha(surf1);
  cairo_surface_t *surf = cairo_image_surface_create_for_data(
    (unsigned char *)surf1->pixels,
    CAIRO_FORMAT_ARGB32,
    surf1->w,
    surf1->h,
    surf1->pitch);
  if ( !my_cairo_check_surface(image, surf) )
    fail;
  cairo_surface_t *final = my_cairo_copy_surface(surf);
  cairo_surface_destroy(surf);
  if ( !my_cairo_check_surface(final, surf) )
    fail;

  assign(image, kind, NAME_pixmap);
  assign(image, depth, toInt(32));
  assign(image->size, w, toInt(surf1->w));
  assign(image->size, h, toInt(surf1->h));
  image->ws_ref = final;
  succeed;
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
 * Save an image to a file in a specified format.
 *
 * @param image Pointer to the Image object.
 * @param into Target source/sink.
 * @param fmt Format name (e.g., "png").
 * @return SUCCEED if saving succeeds; otherwise, FAIL.
 */
status
ws_save_image_file(Image image, SourceSink into, Name fmt)
{
    return SUCCEED;
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
ws_open_image(Image image, DisplayObj d, double scale)
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
ws_close_image(Image image, DisplayObj d)
{
}

/**
 * Resize the image to a new width and height.
 *
 * @param image Pointer to the Image object.
 * @param w New width.
 * @param h New height.
 * @return SUCCEED on success; otherwise, FAIL.
 */
status
ws_resize_image(Image image, Int w, Int h)
{
    return SUCCEED;
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
{
    return NULL;
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
{
    return NULL;
}

/**
 * Create a monochrome version of the image.
 *
 * @param image Pointer to the source Image.
 * @return A new monochrome Image; NULL on failure.
 */
Image
ws_monochrome_image(Image image)
{
    return NULL;
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

/**
 * Render the image to PostScript.
 *
 * @param image Pointer to the Image object.
 * @param depth Color depth for rendering.
 * @param iscolor Boolean flag indicating color output.
 */
void
ws_postscript_image(Image image, Int depth, int iscolor)
{
}

/**
 * Load an image using the XLI utility.
 *
 * @param image Pointer to the Image object.
 * @param file Pointer to the FileObj to load from.
 * @param bright Brightness adjustment.
 * @return SUCCEED on success; otherwise, FAIL.
 */
status
loadXliImage(Image image, FileObj file, Int bright)
{
    return SUCCEED;
}

/**
 * Create an image from raw X11 data.
 *
 * @param image Pointer to the Image object.
 * @param data Pointer to raw image bytes.
 * @param w Width of the image.
 * @param h Height of the image.
 */
void
ws_create_image_from_x11_data(Image image, unsigned char *data, int w, int h)
{
}

/**
 * Retrieve the colour map used for the image.
 *
 * @param image Pointer to the Image object.
 * @return The associated ColourMap.
 */
ColourMap
ws_colour_map_for_image(Image image)
{
    return NULL;
}

/**
 * Initialize system image resources (e.g., built-in icons).
 */
void
ws_system_images(void)
{
}

/**
 * Prepare the transparency mask for the image.
 *
 * @param image Pointer to the Image object.
 */
void
ws_prepare_image_mask(Image image)
{
}
