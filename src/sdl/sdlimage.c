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
#include "sdlimage.h"
#include "sdlcolour.h"

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
{
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

/**
 * Load an image from its associated file path.
 *
 * @param image Pointer to the Image object.
 * @return SUCCEED if loading succeeds; otherwise, FAIL.
 */
status
ws_load_image_file(Image image)
{
    return SUCCEED;
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
{
    return SUCCEED;
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
