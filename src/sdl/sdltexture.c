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
#include <math.h>
#include "sdltexture.h"

/* An sdl_texture keeps its pixels in an SDL_Surface in the format they
 * arrived in, e.g., the YUY2 or NV12 of a camera.  The GPU textures
 * are made from it when a frame first draws it.  A texture belongs to
 * the renderer of one frame, so each frame keeps a table of the
 * sdl_texture objects it has drawn and the SDL_Texture it uses for
 * each.  ->changed marks these stale, so the next draw uploads the
 * surface again, and asks for the frames to be drawn.  The renderer
 * converts YUV to RGB and scales, so a video frame costs no more than
 * the upload.
 */

#define PREMULTIPLIED "xpce.premultiplied"

/* ARGB8888 pixels we made from an image or drew using Cairo have their
 * colour multiplied by alpha, as Cairo does.  We mark such surfaces,
 * so copying them to and from images is a plain copy and the renderer
 * blends them correctly.
 */

static bool
is_premultiplied(SDL_Surface *s)
{ return SDL_GetBooleanProperty(SDL_GetSurfaceProperties(s),
				PREMULTIPLIED, false);
}

static void
set_premultiplied(SDL_Surface *s, bool val)
{ SDL_SetBooleanProperty(SDL_GetSurfaceProperties(s), PREMULTIPLIED, val);
}


typedef void (*frame_func)(FrameObj fr, WsFrame wfr, void *closure);

static void
for_created_frames(frame_func func, void *closure)
{ DisplayManager dm = TheDisplayManager();
  Cell c1;

  if ( !dm )
    return;

  for_cell(c1, dm->members)
  { DisplayObj d = c1->value;
    Cell c2;

    for_cell(c2, d->frames)
    { FrameObj fr = c2->value;

      if ( ws_created_frame(fr) )
	func(fr, fr->ws_ref, closure);
    }
  }
}


static frame_texture *
find_frame_texture(WsFrame wfr, Any t)
{ for(int i=0; i<wfr->ntextures; i++)
  { if ( wfr->textures[i].owner == t )
      return &wfr->textures[i];
  }

  return NULL;
}


static frame_texture *
add_frame_texture(WsFrame wfr, Any t)
{ if ( wfr->ntextures == wfr->atextures )
  { int n = wfr->atextures ? wfr->atextures*2 : 4;
    frame_texture *new = SDL_realloc(wfr->textures, n*sizeof(*new));

    if ( !new )
      return NULL;
    wfr->textures  = new;
    wfr->atextures = n;
  }

  frame_texture *ft = &wfr->textures[wfr->ntextures++];
  ft->owner    = t;
  ft->texture  = NULL;
  ft->made_for = SDL_PIXELFORMAT_UNKNOWN;
  ft->premultiplied = false;
  ft->straight = false;
  ft->stale    = true;
  ft->generation = -1;

  return ft;
}


/* Create a streaming texture of `format` for the pixels of `surf`.  A
 * YUV texture uses the colour space of the camera that made them.
 */

static SDL_Texture *
create_texture(SDL_Renderer *r, SDL_PixelFormat format, SDL_Surface *surf)
{ SDL_PropertiesID props = SDL_CreateProperties();

  SDL_SetNumberProperty(props, SDL_PROP_TEXTURE_CREATE_FORMAT_NUMBER, format);
  SDL_SetNumberProperty(props, SDL_PROP_TEXTURE_CREATE_ACCESS_NUMBER,
			SDL_TEXTUREACCESS_STREAMING);
  SDL_SetNumberProperty(props, SDL_PROP_TEXTURE_CREATE_WIDTH_NUMBER, surf->w);
  SDL_SetNumberProperty(props, SDL_PROP_TEXTURE_CREATE_HEIGHT_NUMBER, surf->h);
  if ( SDL_ISPIXELFORMAT_FOURCC(format) )
    SDL_SetNumberProperty(props, SDL_PROP_TEXTURE_CREATE_COLORSPACE_NUMBER,
			  SDL_GetSurfaceColorspace(surf));
  SDL_Texture *tex = SDL_CreateTextureWithProperties(r, props);
  SDL_DestroyProperties(props);

  return tex;
}


/* Can the renderer draw textures of `format` itself?  SDL accepts more
 * formats, converting them on the CPU for each upload.
 */

static bool
renderer_supports(SDL_Renderer *r, SDL_PixelFormat format)
{ const SDL_PixelFormat *f =
    SDL_GetPointerProperty(SDL_GetRendererProperties(r),
			   SDL_PROP_RENDERER_TEXTURE_FORMATS_POINTER, NULL);

  for(; f && *f != SDL_PIXELFORMAT_UNKNOWN; f++)
  { if ( *f == format )
      return true;
  }

  return false;
}


/* The software renderer draws triangles (SDL_RenderGeometry(),
 * SDL_RenderTextureAffine()) ignoring the premultiplied blend modes:
 * the texture replaces what is below.  We therefore give it textures
 * with straight alpha.
 */

static bool
is_software_renderer(SDL_Renderer *r)
{ const char *name = SDL_GetRendererName(r);

  return name && strcmp(name, SDL_SOFTWARE_RENDERER) == 0;
}


/* Divide the colours of the ARGB8888 surface `s` by alpha */

static void
unpremultiply_surface(SDL_Surface *s)
{ for(int y=0; y<s->h; y++)
  { Uint32 *p = (Uint32*)((Uint8*)s->pixels + (size_t)y*s->pitch);

    for(int x=0; x<s->w; x++)
    { Uint32 v = p[x];
      Uint32 a = v>>24;

      if ( a > 0 && a < 255 )
      { Uint32 r = SDL_min(255, (((v>>16)&0xff)*255 + a/2)/a);
	Uint32 g = SDL_min(255, (((v>> 8)&0xff)*255 + a/2)/a);
	Uint32 b = SDL_min(255, (( v     &0xff)*255 + a/2)/a);

	p[x] = (a<<24) | (r<<16) | (g<<8) | b;
      }
    }
  }
}


/* The texture of `t` in the renderer of `fr`, made or uploaded if
 * needed.  The texture has the format of the surface if the renderer
 * supports it.  A YUV format the renderer does not support, such as
 * the YUY2 of many cameras, is converted to NV12 that most renderers
 * convert to RGB on the GPU.  Otherwise we leave the conversion to SDL
 * and as a last resort convert to ARGB8888 ourselves.
 */

static frame_texture *
sdl_frame_texture(FrameObj fr, SdlTexture t)
{ WsFrame wfr = fr->ws_ref;
  SDL_Surface *surf = t->surface;
  frame_texture *ft;

  if ( !(ft = find_frame_texture(wfr, t)) &&	/* also if empty, so */
       !(ft = add_frame_texture(wfr, t)) )	/* ->changed redraws us */
    return NULL;
  if ( !surf || surf->w <= 0 || surf->h <= 0 )
    return NULL;

  if ( ft->texture &&
       ( ft->texture->w != surf->w || ft->texture->h != surf->h ||
	 ft->made_for != surf->format ||
	 ft->premultiplied != is_premultiplied(surf) ) )
  { SDL_DestroyTexture(ft->texture);
    ft->texture = NULL;
  }

  if ( !ft->texture )
  { SDL_PixelFormat formats[3];
    int ntry = 0;

    if ( SDL_ISPIXELFORMAT_FOURCC(surf->format) &&
	 !renderer_supports(wfr->ws_renderer, surf->format) &&
	 renderer_supports(wfr->ws_renderer, SDL_PIXELFORMAT_NV12) )
      formats[ntry++] = SDL_PIXELFORMAT_NV12;
    formats[ntry++] = surf->format;
    formats[ntry++] = SDL_PIXELFORMAT_ARGB8888;

    for(int i=0; i<ntry && !ft->texture; i++)
    { if ( !(ft->texture = create_texture(wfr->ws_renderer, formats[i], surf)) )
	DEBUG(NAME_sdl,
	      Cprintf("%s: no %s texture: %s\n",
		      pp(t), SDL_GetPixelFormatName(formats[i]), SDL_GetError()));
    }
    if ( !ft->texture )
    { Cprintf("%s: cannot create texture: %s\n", pp(t), SDL_GetError());
      return NULL;
    }
    DEBUG(NAME_sdl,
	  Cprintf("%s: %s texture for %s in %s renderer\n",
		  pp(t), SDL_GetPixelFormatName(ft->texture->format),
		  SDL_GetPixelFormatName(surf->format),
		  SDL_GetRendererName(wfr->ws_renderer)));
    ft->made_for = surf->format;
    ft->premultiplied = is_premultiplied(surf);
    ft->straight = ( ft->premultiplied &&
		     ft->texture->format == SDL_PIXELFORMAT_ARGB8888 &&
		     is_software_renderer(wfr->ws_renderer) );
    ft->stale = true;
  }

  if ( ft->stale )
  { if ( ft->straight )
    { SDL_Surface *copy = SDL_ConvertSurface(surf, SDL_PIXELFORMAT_ARGB8888);

      if ( copy )
      { unpremultiply_surface(copy);
	SDL_UpdateTexture(ft->texture, NULL, copy->pixels, copy->pitch);
	SDL_DestroySurface(copy);
      }
    } else if ( ft->texture->format == surf->format )
    { SDL_UpdateTexture(ft->texture, NULL, surf->pixels, surf->pitch);
    } else
    { SDL_Colorspace cs = SDL_ISPIXELFORMAT_FOURCC(ft->texture->format)
				? SDL_GetSurfaceColorspace(surf)
				: SDL_COLORSPACE_SRGB;
      SDL_Surface *conv = SDL_ConvertSurfaceAndColorspace(
			    surf, ft->texture->format, NULL, cs, 0);

      if ( conv )
      { SDL_UpdateTexture(ft->texture, NULL, conv->pixels, conv->pitch);
	SDL_DestroySurface(conv);
      }
    }
    ft->stale = false;
  }

  return ft;
}


/* Called before the renderer of a frame is destroyed or after it lost
 * its textures.
 */

void
sdl_frame_free_textures(FrameObj fr)
{ WsFrame wfr = fr->ws_ref;

  if ( wfr && wfr->textures )
  { for(int i=0; i<wfr->ntextures; i++)
    { if ( wfr->textures[i].texture )
	SDL_DestroyTexture(wfr->textures[i].texture);
    }
    SDL_free(wfr->textures);
    wfr->textures   = NULL;
    wfr->ntextures  = 0;
    wfr->atextures  = 0;
  }
}


static void
forget_texture(FrameObj fr, WsFrame wfr, void *closure)
{ frame_texture *ft = find_frame_texture(wfr, closure);

  if ( ft )
  { if ( ft->texture )
      SDL_DestroyTexture(ft->texture);
    *ft = wfr->textures[--wfr->ntextures];
  }
}


static void
stale_texture(FrameObj fr, WsFrame wfr, void *closure)
{ frame_texture *ft = find_frame_texture(wfr, closure);

  if ( ft )
  { ft->stale = true;
    if ( ChangedFrames )
      addChain(ChangedFrames, fr);
  }
}


		 /*******************************
		 *	   SDL_TEXTURE		*
		 *******************************/

static SDL_Surface *
empty_surface(int w, int h)
{ SDL_Surface *s = SDL_CreateSurface(w, h, SDL_PIXELFORMAT_ARGB8888);

  if ( s )
  { SDL_ClearSurface(s, 0.0f, 0.0f, 0.0f, 0.0f);
    set_premultiplied(s, true);
  }

  return s;
}


/* A copy of the pixels of `image`, premultiplied as Cairo has them */

static SDL_Surface *
image_surface(Image image)
{ cairo_surface_t *cs = pceImage2CairoSurface(image);

  if ( !cs )
    return NULL;

  cairo_surface_flush(cs);
  int w = cairo_image_surface_get_width(cs);
  int h = cairo_image_surface_get_height(cs);
  int stride = cairo_image_surface_get_stride(cs);
  const Uint8 *data = cairo_image_surface_get_data(cs);
  SDL_Surface *s = SDL_CreateSurface(w, h, SDL_PIXELFORMAT_ARGB8888);

  if ( s )
  { for(int y=0; y<h; y++)
      memcpy((Uint8*)s->pixels + (size_t)y*s->pitch,
	     data + (size_t)y*stride, (size_t)w*4);
    set_premultiplied(s, true);
  }

  return s;
}


static void
replace_surface(SdlTexture t, SDL_Surface *s)
{ if ( t->surface )
    SDL_DestroySurface(t->surface);
  t->surface = s;
}


/* Render `gr` into a new texture of the size of its bounding box,
 * including space for, e.g., a glow.  The origin of `gr`, i.e., the
 * point (0,0) of its coordinates, becomes the hot spot, so a figure
 * drawn around (0,0) rotates around that point.
 */

static status
graphical_texture(SdlTexture t, Graphical gr)
{ ComputeGraphical(gr);
  int x = valInt(gr->area->x), y = valInt(gr->area->y);
  int w = valInt(gr->area->w), h = valInt(gr->area->h);
  NormaliseArea(x, y, w, h);
  int m = get_extension_margin_graphical(gr);
  SDL_Surface *s = empty_surface(max(w+2*m, 1), max(h+2*m, 1));

  if ( !s )
    return errorPce(t, NAME_cannotCreate, CtoName(SDL_GetError()));
  t->surface = s;

  int hx = m-x, hy = m-y;
  if ( hx >= 0 && hy >= 0 && hx < s->w && hy < s->h )
    assign(t, hot_spot, newObject(ClassPoint, toInt(hx), toInt(hy), EAV));

  return ws_sdl_texture_draw_in(t, gr,
				answerObject(ClassPoint, toInt(m), toInt(m),
					     EAV));
}


status
ws_init_sdl_texture(SdlTexture t, Any from)
{ SDL_Surface *s = NULL;

  if ( isDefault(from) )
    succeed;
  if ( instanceOfObject(from, ClassImage) )
  { s = image_surface(from);
    if ( s && notNil(((Image)from)->hot_spot) )
    { Point hs = ((Image)from)->hot_spot;

      assign(t, hot_spot, newObject(ClassPoint, hs->x, hs->y, EAV));
    }
  } else if ( instanceOfObject(from, ClassGraphical) )
  { return graphical_texture(t, from);
  }
  else if ( instanceOfObject(from, ClassSize) )
  { Size sz = from;

    s = empty_surface(valInt(sz->w), valInt(sz->h));
  }

  if ( !s )
    return errorPce(t, NAME_cannotCreate, CtoName(SDL_GetError()));

  t->surface = s;
  succeed;
}


void
ws_destroy_sdl_texture(SdlTexture t)
{ for_created_frames(forget_texture, t);

  if ( t->surface )
  { SDL_DestroySurface(t->surface);
    t->surface = NULL;
  }
}


void
ws_sdl_texture_changed(SdlTexture t)
{ for_created_frames(stale_texture, t);
}


/* Copy the pixels of `surf` into `t`, keeping their format.  Used for
 * the frames of a camera, which must be copied before they are given
 * back.  MJPG is decoded as the renderer cannot handle it.
 */

status
ws_sdl_texture_set_surface(SdlTexture t, SDL_Surface *surf)
{ SDL_Surface *decoded = NULL;
  SDL_Surface *old = t->surface;

  if ( surf->format == SDL_PIXELFORMAT_MJPG )
  { if ( !(decoded = ws_decode_mjpg(surf->pixels, (size_t)surf->pitch)) )
      return errorPce(t, NAME_cannotCreate, CtoName(SDL_GetError()));
    surf = decoded;
  }

  if ( old && old->w == surf->w && old->h == surf->h &&
       old->format == surf->format &&
       SDL_ConvertPixels(surf->w, surf->h,
			 surf->format, surf->pixels, surf->pitch,
			 old->format, old->pixels, old->pitch) )
  { SDL_SetSurfaceColorspace(old, SDL_GetSurfaceColorspace(surf));
  } else
  { SDL_Surface *copy = decoded ? decoded : SDL_DuplicateSurface(surf);

    if ( !copy )
      return errorPce(t, NAME_cannotCreate, CtoName(SDL_GetError()));
    if ( copy == decoded )
      decoded = NULL;
    if ( old )
      SDL_DestroySurface(old);
    t->surface = copy;
  }

  if ( decoded )
    SDL_DestroySurface(decoded);
  set_premultiplied(t->surface, false);
  ws_sdl_texture_changed(t);

  succeed;
}


/* Make `surf` the pixels of `t`, without copying, and return the
 * surface it had, which the caller now owns.  Used to show the frames
 * of a video, which recycles the surfaces it gets back.
 */

SDL_Surface *
ws_sdl_texture_swap_surface(SdlTexture t, SDL_Surface *surf)
{ SDL_Surface *old = t->surface;

  t->surface = surf;
  set_premultiplied(surf, false);
  ws_sdl_texture_changed(t);

  return old;
}


status
ws_sdl_texture_from_image(SdlTexture t, Image image)
{ SDL_Surface *s = image_surface(image);

  if ( !s )
    return errorPce(t, NAME_cannotCreate, CtoName(SDL_GetError()));

  replace_surface(t, s);
  ws_sdl_texture_changed(t);

  succeed;
}


/* Copy the pixels into `image`, converting them to ARGB.  Our ARGB is
 * premultiplied or opaque, so ws_set_image_pixels() can copy it.
 */

status
ws_sdl_texture_to_image(SdlTexture t, Image image)
{ if ( !t->surface )
    return errorPce(t, NAME_noData);

  return ws_set_image_pixels(image, t->surface);
}


/* Make the pixels premultiplied ARGB8888, so Cairo can draw on them */

static bool
argb_surface(SdlTexture t)
{ SDL_Surface *s = t->surface;

  if ( s->format == SDL_PIXELFORMAT_ARGB8888 )
  { if ( !is_premultiplied(s) )
    { SDL_PremultiplySurfaceAlpha(s, false);
      set_premultiplied(s, true);
    }
    return true;
  }

  SDL_Surface *argb = SDL_ConvertSurface(s, SDL_PIXELFORMAT_ARGB8888);
  if ( !argb )
    return false;
  SDL_PremultiplySurfaceAlpha(argb, false);
  set_premultiplied(argb, true);
  replace_surface(t, argb);

  return true;
}


static void
paint_in_texture(Graphical gr, Area a, void *closure)
{ cairo_surface_t *cs = closure;

  d_surface(cs, valInt(a->x), valInt(a->y), valInt(a->w), valInt(a->h));
  d_modify();
  RedrawArea(gr, a);
  d_done();
}


/* Paint `gr` onto the pixels using Cairo.  Pixels that are not ARGB,
 * e.g., the YUV of a camera, are converted first.
 */

status
ws_sdl_texture_draw_in(SdlTexture t, Graphical gr, Point pos)
{ if ( !t->surface )
    return errorPce(t, NAME_noData);
  if ( !argb_surface(t) )
    return errorPce(t, NAME_cannotCreate, CtoName(SDL_GetError()));

  SDL_Surface *s = t->surface;
  cairo_surface_t *cs =
    cairo_image_surface_create_for_data(s->pixels, CAIRO_FORMAT_ARGB32,
					s->w, s->h, s->pitch);
  if ( cairo_surface_status(cs) != CAIRO_STATUS_SUCCESS )
  { cairo_surface_destroy(cs);
    return errorPce(t, NAME_cannotCreate,
		    CtoName(cairo_status_to_string(cairo_surface_status(cs))));
  }

  status rc = drawInGraphical(gr, pos, s->w, s->h, paint_in_texture, cs);
  cairo_surface_flush(cs);
  cairo_surface_destroy(cs);
  ws_sdl_texture_changed(t);

  return rc;
}


Image
ws_sdl_texture_image(SdlTexture t)
{ Image image = newObject(ClassImage, NIL, ONE, ONE, NAME_pixmap, EAV);

  if ( image && t->surface &&
       !ws_sdl_texture_to_image(t, image) )
    fail;

  return image;
}


Size
ws_sdl_texture_size(SdlTexture t)
{ int w = t->surface ? t->surface->w : 0;
  int h = t->surface ? t->surface->h : 0;

  return newObject(ClassSize, toInt(w), toInt(h), EAV);
}


/* SDL_PIXELFORMAT_YUY2 -> yuy2 */

Name
ws_sdl_texture_format(SdlTexture t)
{ if ( !t->surface )
    return NIL;

  const char *s = SDL_GetPixelFormatName(t->surface->format);
  const char *prefix = "SDL_PIXELFORMAT_";
  char buf[64];
  size_t i;

  if ( strncmp(s, prefix, strlen(prefix)) == 0 )
    s += strlen(prefix);
  for(i=0; s[i] && i < sizeof(buf)-1; i++)
    buf[i] = tolower((unsigned char)s[i]);
  buf[i] = EOS;

  return CtoName(buf);
}


		 /*******************************
		 *	      LAYERS		*
		 *******************************/

/* Where layer `l` of `sw` shows which part of its texture.  The layout
 * places the part `src` of the texture on the rectangle `dst` in the
 * coordinates of the window, according to <-scale.  Using `none`, its
 * hot spot is at the top-left of <-area.  Using `tile`, `src` is
 * repeated at its own size to fill `dst`, the first tile at `tile`.
 * If `sliced`, `centre` is the part of `src` that stretches or tiles
 * in both directions; see sdl_layer<-slices.
 *
 * `map` maps the layout onto the device pixels of the renderer: it
 * flips around the centre of `dst`, rotates by <-angle around the hot
 * spot or else the centre, applies <-transform and scrolling and
 * finally the position and pixel density of the window.
 */

/* What layers are drawn on: the layers, the background, the size and
 * the scroll offset.  See window_canvas().
 */

typedef struct
{ Chain		layers;			/* sdl_layer objects, bottom first */
  Any		background;		/* colour or not */
  float		w, h;			/* size */
  float		sx, sy;			/* scroll offset */
} layer_canvas;

static void
window_canvas(SdlWindow sw, layer_canvas *cv)
{ cv->layers     = sw->layers;
  cv->background = sw->background;
  cv->w          = valNum(sw->area->w);
  cv->h          = valNum(sw->area->h);
  cv->sx         = valNum(sw->scroll_offset->x);
  cv->sy         = valNum(sw->scroll_offset->y);
}


/* A layer shows an sdl_texture or an sdl_canvas.  The functions below
 * hide the difference.
 */

static frame_texture *canvas_frame_texture(FrameObj fr, SdlCanvas c);
static SDL_Surface   *canvas_surface(SdlCanvas c);

static bool
canvas_size(SdlCanvas c, int *w, int *h)
{ *w = (int)valInt(c->size->w);
  *h = (int)valInt(c->size->h);

  return *w > 0 && *h > 0;
}


/* The size of what layer `l` shows.  False if it shows no pixels. */

static bool
layer_source_size(SdlLayer l, int *w, int *h)
{ if ( instanceOfObject(l->texture, ClassSdlCanvas) )
    return canvas_size(l->texture, w, h);
  if ( isNil(l->texture) )
    return false;

  SdlTexture t = l->texture;
  SDL_Surface *s = t->surface;

  if ( !s || s->w <= 0 || s->h <= 0 )
    return false;
  *w = s->w;
  *h = s->h;

  return true;
}


static Point
layer_hot_spot(SdlLayer l)
{ if ( instanceOfObject(l->texture, ClassSdlTexture) )
    return ((SdlTexture)l->texture)->hot_spot;

  return NIL;
}


/* The GPU texture of what layer `l` shows in frame `fr`.  A canvas has
 * been drawn by sdl_render_canvases().
 */

static frame_texture *
layer_frame_texture(FrameObj fr, SdlLayer l)
{ if ( instanceOfObject(l->texture, ClassSdlCanvas) )
    return canvas_frame_texture(fr, l->texture);

  return sdl_frame_texture(fr, l->texture);
}


/* What layer `l` shows as a new ARGB8888 surface with premultiplied
 * alpha, for drawing with Cairo.
 */

static SDL_Surface *
layer_argb_surface(SdlLayer l)
{ if ( instanceOfObject(l->texture, ClassSdlCanvas) )
    return canvas_surface(l->texture);

  SDL_Surface *s = ((SdlTexture)l->texture)->surface;
  SDL_Surface *argb = SDL_ConvertSurface(s, SDL_PIXELFORMAT_ARGB8888);

  if ( argb && !is_premultiplied(s) )
    SDL_PremultiplySurfaceAlpha(argb, false);

  return argb;
}


typedef struct
{ double xx, xy, yx, yy, tx, ty;	/* x' = xx*x + xy*y + tx, ... */
} layer_map;

typedef struct
{ SDL_FRect	src;			/* part of the texture */
  SDL_FRect	dst;			/* where in the window */
  bool		tiled;			/* scale is `tile` */
  SDL_FPoint	tile;			/* top-left of the first tile */
  bool		sliced;			/* nine parts */
  SDL_FRect	centre;			/* centre part of `src` */
  layer_map	map;			/* window -> device pixels */
} layer_layout;

/* Part `src` of a texture on the parallelogram with corners `origin`
 * (top-left of `src`), `right` (top-right) and `down` (bottom-left),
 * in device pixels.
 */

typedef struct
{ SDL_FRect	src;
  SDL_FPoint	origin;
  SDL_FPoint	right;
  SDL_FPoint	down;
} layer_quad;


/* m := n o m, where n is given by its coefficients */

static void
map_then(layer_map *m, double xx, double xy, double yx, double yy,
	 double tx, double ty)
{ layer_map o = *m;

  m->xx = xx*o.xx + xy*o.yx;
  m->xy = xx*o.xy + xy*o.yy;
  m->yx = yx*o.xx + yy*o.yx;
  m->yy = yx*o.xy + yy*o.yy;
  m->tx = xx*o.tx + xy*o.ty + tx;
  m->ty = yx*o.tx + yy*o.ty + ty;
}


static SDL_FPoint
map_point(const layer_map *m, double x, double y)
{ return (SDL_FPoint){ (float)(m->xx*x + m->xy*y + m->tx),
		       (float)(m->yx*x + m->yy*y + m->ty) };
}


/* First tile position at or before `from` for tiles of size `size`,
 * shifted by `offset`.
 */

static float
tile_start(float from, float size, float offset)
{ float t = from + fmodf(offset, size);

  return t > from ? t - size : t;
}


/* Compute the layout of `l` on `cv`, whose top-left is at `ox`,`oy` in
 * device pixels with `scale` device pixels per pixel of `cv`.  False if
 * nothing is shown.
 */

static bool
layer_geometry(const layer_canvas *cv, SdlLayer l,
	       float ox, float oy, float scale, layer_layout *ly)
{ SDL_FRect base, *src = &ly->src, *dst = &ly->dst;
  int tw, th;

  if ( l->displayed != ON || !layer_source_size(l, &tw, &th) )
    return false;
  Point hot_spot = layer_hot_spot(l);

  if ( isNil(l->source) )
  { *src = (SDL_FRect){0, 0, (float)tw, (float)th};
  } else
  { SDL_FRect a = { valNum(l->source->x), valNum(l->source->y),
		    valNum(l->source->w), valNum(l->source->h) };
    SDL_FRect all = {0, 0, (float)tw, (float)th};

    if ( !SDL_GetRectIntersectionFloat(&a, &all, src) )
      return false;
  }

  if ( isNil(l->area) )
  { base = (SDL_FRect){0, 0, cv->w, cv->h};
  } else
  { base = (SDL_FRect){ valNum(l->area->x), valNum(l->area->y),
			valNum(l->area->w), valNum(l->area->h) };
  }
  if ( src->w <= 0 || src->h <= 0 ||
       (l->scale != NAME_none && (base.w <= 0 || base.h <= 0)) )
    return false;

  ly->tiled = (l->scale == NAME_tile);
  bool hot = !ly->tiled && notNil(hot_spot);
  float hx = hot ? valNum(hot_spot->x) - src->x : src->w/2;
  float hy = hot ? valNum(hot_spot->y) - src->y : src->h/2;

  if ( l->scale == NAME_none )
  { *dst = (SDL_FRect){base.x - (hot ? hx : 0), base.y - (hot ? hy : 0),
		       src->w, src->h};
  } else if ( l->scale == NAME_fit )
  { float s = fminf(base.w/src->w, base.h/src->h);
    float w = src->w*s, h = src->h*s;

    *dst = (SDL_FRect){base.x+(base.w-w)/2, base.y+(base.h-h)/2, w, h};
  } else if ( l->scale == NAME_fill )
  { float s = fmaxf(base.w/src->w, base.h/src->h);
    float w = base.w/s, h = base.h/s;

    hx -= (src->w-w)/2;
    hy -= (src->h-h)/2;
    src->x += (src->w-w)/2;
    src->y += (src->h-h)/2;
    src->w  = w;
    src->h  = h;
    *dst = base;
  } else if ( ly->tiled )
  { bool off = notNil(l->offset);

    ly->tile.x = tile_start(base.x, src->w, off ? valNum(l->offset->x) : 0);
    ly->tile.y = tile_start(base.y, src->h, off ? valNum(l->offset->y) : 0);
    hx = base.w/2;			/* rotate around the centre */
    hy = base.h/2;
    *dst = base;
  } else				/* stretch */
  { *dst = base;
  }

  ly->sliced = ( notNil(l->slices) &&
		 (l->scale == NAME_stretch || ly->tiled) );
  if ( ly->sliced )
  { float x0 = valNum(l->slices->x), y0 = valNum(l->slices->y);
    float x1 = x0 + valNum(l->slices->w), y1 = y0 + valNum(l->slices->h);

    x0 = SDL_clamp(x0, src->x, src->x+src->w);
    y0 = SDL_clamp(y0, src->y, src->y+src->h);
    x1 = SDL_clamp(x1, x0, src->x+src->w);
    y1 = SDL_clamp(y1, y0, src->y+src->h);
    ly->centre = (SDL_FRect){x0, y0, x1-x0, y1-y0};
  }

  layer_map *m = &ly->map;
  *m = (layer_map){1, 0, 0, 1, 0, 0};

  if ( l->flip == NAME_horizontal || l->flip == NAME_both )
    map_then(m, -1, 0, 0, 1, 2*dst->x+dst->w, 0);
  if ( l->flip == NAME_vertical || l->flip == NAME_both )
    map_then(m, 1, 0, 0, -1, 0, 2*dst->y+dst->h);

  double rad = valNum(l->angle)*M_PI/180.0;
  double c = cos(rad), sn = sin(rad);
  double px = dst->x + (ly->tiled ? hx : hx*dst->w/src->w);
  double py = dst->y + (ly->tiled ? hy : hy*dst->h/src->h);
  map_then(m, c, -sn, sn, c, px - c*px + sn*py, py - sn*px - c*py);

  Transform tr = l->transform;
  if ( notNil(tr) )
    map_then(m, valNum(tr->xx), valNum(tr->xy), valNum(tr->yx),
	     valNum(tr->yy), valNum(tr->tx), valNum(tr->ty));
  if ( notNil(l->area) || notNil(tr) )
    map_then(m, 1, 0, 0, 1, cv->sx, cv->sy);
  map_then(m, scale, 0, 0, scale, ox, oy);

  return true;
}


/* Call `func` for the quads that make up the layout: one, one for
 * each (part of a) tile, or those for the nine parts of a sliced
 * texture.
 */

typedef void (*quad_func)(const layer_quad *q, void *closure);

static void
layer_quad_part(const layer_layout *ly, const SDL_FRect *src,
		const SDL_FRect *dst, quad_func func, void *closure)
{ layer_quad q = { .src = *src };

  q.origin = map_point(&ly->map, dst->x,        dst->y);
  q.right  = map_point(&ly->map, dst->x+dst->w, dst->y);
  q.down   = map_point(&ly->map, dst->x,        dst->y+dst->h);
  (*func)(&q, closure);
}


/* Show `src` on `dst`, repeating it at its own size from `start` in the
 * directions `tx` and `ty` and stretching it in the others.
 */

static void
layer_part(const layer_layout *ly, const SDL_FRect *src, const SDL_FRect *dst,
	   bool tx, bool ty, SDL_FPoint start, quad_func func, void *closure)
{ float tw = tx ? src->w : dst->w;
  float th = ty ? src->h : dst->h;
  int nx = 1, ny = 1;

  if ( src->w <= 0 || src->h <= 0 || dst->w <= 0 || dst->h <= 0 )
    return;
  if ( tx )
    nx = (int)ceilf((dst->x+dst->w-start.x)/tw);
  else
    start.x = dst->x;
  if ( ty )
    ny = (int)ceilf((dst->y+dst->h-start.y)/th);
  else
    start.y = dst->y;

  for(int j=0; j<ny; j++)
  { for(int i=0; i<nx; i++)
    { SDL_FRect tile = { start.x+i*tw, start.y+j*th, tw, th };
      SDL_FRect part;

      if ( SDL_GetRectIntersectionFloat(&tile, dst, &part) )
      { float sx = src->w/tw, sy = src->h/th;
	SDL_FRect s = { src->x + (part.x-tile.x)*sx,
			src->y + (part.y-tile.y)*sy,
			part.w*sx, part.h*sy };

	layer_quad_part(ly, &s, &part, func, closure);
      }
    }
  }
}


/* Split `from`..`from+size` of the source in three at the centre part
 * `c0`..`c1`, and `to`..`to+room` of the window such that the outer
 * parts keep their size, shrinking them if there is not enough room.
 */

static void
slice_axis(float from, float size, float c0, float c1, float to, float room,
	   float s[4], float d[4])
{ float lo = c0-from, hi = from+size-c1;
  float f = lo+hi > room ? room/(lo+hi) : 1.0f;

  s[0] = from; s[1] = c0;      s[2] = c1;           s[3] = from+size;
  d[0] = to;   d[1] = to+lo*f; d[2] = to+room-hi*f; d[3] = to+room;
}


static void
for_layer_quads(const layer_layout *ly, quad_func func, void *closure)
{ const SDL_FRect *src = &ly->src, *dst = &ly->dst;

  if ( !ly->sliced )
  { layer_part(ly, src, dst, ly->tiled, ly->tiled, ly->tile, func, closure);
    return;
  }

  const SDL_FRect *c = &ly->centre;
  float sx[4], dx[4], sy[4], dy[4];

  slice_axis(src->x, src->w, c->x, c->x+c->w, dst->x, dst->w, sx, dx);
  slice_axis(src->y, src->h, c->y, c->y+c->h, dst->y, dst->h, sy, dy);
  for(int j=0; j<3; j++)
  { for(int i=0; i<3; i++)
    { SDL_FRect s = { sx[i], sy[j], sx[i+1]-sx[i], sy[j+1]-sy[j] };
      SDL_FRect d = { dx[i], dy[j], dx[i+1]-dx[i], dy[j+1]-dy[j] };

      layer_part(ly, &s, &d, ly->tiled && i == 1, ly->tiled && j == 1,
		 (SDL_FPoint){d.x, d.y}, func, closure);
    }
  }
}


/* Draw the texture onto the parallelogram of `q`.  If this is a
 * rectangle, possibly rotated and mirrored, we use
 * SDL_RenderTextureRotated(): the software renderer implements
 * SDL_RenderTextureAffine() using triangles, which do not support
 * premultiplied alpha.
 */

typedef struct
{ SDL_Renderer *renderer;
  SDL_Texture  *texture;
} render_target;

static void
render_quad(const layer_quad *q, void *closure)
{ render_target *rt = closure;
  SDL_Renderer *r = rt->renderer;
  SDL_Texture *tex = rt->texture;
  float ux = q->right.x - q->origin.x, uy = q->right.y - q->origin.y;
  float vx = q->down.x  - q->origin.x, vy = q->down.y  - q->origin.y;
  float w = hypotf(ux, uy), h = hypotf(vx, vy);

  if ( w > 0 && h > 0 && fabsf(ux*vx + uy*vy) <= 1e-3f*w*h )
  { float cross = ux*vy - uy*vx;	/* < 0: mirrored */
    SDL_FlipMode flip = SDL_FLIP_NONE;
    float ax = ux, ay = uy;		/* direction of the x axis */

    if ( cross < 0 )			/* mirror x */
    { flip = SDL_FLIP_HORIZONTAL;
      ax = -ux; ay = -uy;
    }
    double angle = atan2(ay, ax)*180.0/M_PI;
    float cx = q->origin.x + (ux+vx)/2;	/* centre */
    float cy = q->origin.y + (uy+vy)/2;
    SDL_FRect dst = { cx-w/2, cy-h/2, w, h };

    SDL_RenderTextureRotated(r, tex, &q->src, &dst, angle, NULL, flip);
  } else
  { SDL_RenderTextureAffine(r, tex, &q->src,
			    &q->origin, &q->right, &q->down);
  }
}


/* Tiles that are not rotated, flipped or sheared and scaled the same
 * in both directions are drawn by SDL_RenderTextureTiled(), which
 * uses a single draw with a repeating texture if the renderer can and
 * the tile is the whole texture.  `clip` is the clip rectangle of the
 * window.  False if the tiles are not upright.
 */

static bool
render_upright_tiles(SDL_Renderer *r, SDL_Texture *tex,
		     const layer_layout *ly, const SDL_Rect *clip)
{ const layer_map *m = &ly->map;
  const SDL_FRect *dst = &ly->dst;

  if ( m->xy != 0 || m->yx != 0 || m->xx <= 0 ||
       fabs(m->xx - m->yy) > 1e-6*m->xx )
    return false;

  SDL_FPoint a = map_point(m, dst->x, dst->y);
  SDL_FPoint b = map_point(m, dst->x+dst->w, dst->y+dst->h);
  SDL_FPoint t = map_point(m, ly->tile.x, ly->tile.y);
  int x0 = (int)SDL_lroundf(a.x), y0 = (int)SDL_lroundf(a.y);
  SDL_Rect area = { x0, y0,
		    (int)SDL_lroundf(b.x)-x0, (int)SDL_lroundf(b.y)-y0 };
  SDL_Rect visible;

  if ( SDL_GetRectIntersection(&area, clip, &visible) )
  { SDL_FRect tiles = { t.x, t.y, b.x-t.x, b.y-t.y };

    SDL_SetRenderClipRect(r, &visible);
    SDL_RenderTextureTiled(r, tex, &ly->src, (float)m->xx, &tiles);
    SDL_SetRenderClipRect(r, clip);
  }

  return true;
}


/* The SDL blend mode for <-blend of `l`.  Textures without alpha
 * (camera, video) must blend as well, or <-alpha is ignored.
 */

static SDL_BlendMode
layer_blend_mode(SdlLayer l, bool pm)
{ if ( l->blend == NAME_add )
    return pm ? SDL_BLENDMODE_ADD_PREMULTIPLIED : SDL_BLENDMODE_ADD;
  if ( l->blend == NAME_mod )
    return SDL_BLENDMODE_MOD;
  if ( l->blend == NAME_mul )
    return SDL_BLENDMODE_MUL;
  if ( l->blend == NAME_none )
    return SDL_BLENDMODE_NONE;

  return pm ? SDL_BLENDMODE_BLEND_PREMULTIPLIED : SDL_BLENDMODE_BLEND;
}


/* As layer_blend_mode() for drawing with Cairo */

static cairo_operator_t
layer_cairo_operator(SdlLayer l)
{ if ( l->blend == NAME_add )
    return CAIRO_OPERATOR_ADD;
  if ( l->blend == NAME_mod || l->blend == NAME_mul )
    return CAIRO_OPERATOR_MULTIPLY;
  if ( l->blend == NAME_none )
    return CAIRO_OPERATOR_SOURCE;

  return CAIRO_OPERATOR_OVER;
}


static SDL_ScaleMode
layer_scale_mode(SdlLayer l)
{ if ( l->scale_mode == NAME_nearest )
    return SDL_SCALEMODE_NEAREST;
#if SDL_VERSION_ATLEAST(3, 4, 0)
  if ( l->scale_mode == NAME_pixelart )
    return SDL_SCALEMODE_PIXELART;
#else
  if ( l->scale_mode == NAME_pixelart )
    return SDL_SCALEMODE_NEAREST;
#endif

  return SDL_SCALEMODE_LINEAR;
}


/* The map from the coordinates of a mesh to device pixels: through
 * <-transform, scrolling and the position and pixel density of the
 * window.
 */

static void
mesh_map(const layer_canvas *cv, SdlLayer l, float ox, float oy, float scale,
	 layer_map *m)
{ Transform tr = l->transform;

  *m = (layer_map){1, 0, 0, 1, 0, 0};
  if ( notNil(tr) )
    map_then(m, valNum(tr->xx), valNum(tr->xy), valNum(tr->yx),
	     valNum(tr->yy), valNum(tr->tx), valNum(tr->ty));
  map_then(m, 1, 0, 0, 1, cv->sx, cv->sy);
  map_then(m, scale, 0, 0, scale, ox, oy);
}



/* A white pixel, the texture of meshes without a texture.  See
 * render_mesh().
 */

static SdlTexture
white_texture(void)
{ static SdlTexture white = NULL;

  if ( !white )
  { SdlTexture t = newObject(ClassSdlTexture,
			     newObject(ClassSize, ONE, ONE, EAV), EAV);

    if ( t && t->surface )
    { SDL_FillSurfaceRect(t->surface, NULL,
			  SDL_MapSurfaceRGBA(t->surface, 255, 255, 255, 255));
      protectObject(t);
      white = t;
    }
  }

  return white;
}


/* Draw the mesh of layer `l`.  SDL_RenderGeometry() ignores the colour
 * and alpha mod of the texture, so <-alpha and <-tint go into the
 * colours of the vertices.  With a premultiplied texture, these must
 * be premultiplied as well.
 *
 * The software renderer draws triangles that form a rectangle as a
 * rectangle, ignoring the blend mode if the texture coordinates are
 * clamped (SDL 3.4).  We avoid this by repeating the texture there,
 * and by always using a texture: a white pixel if the layer has none.
 */

static void
render_mesh(FrameObj fr, const layer_canvas *cv, SdlLayer l, SDL_Renderer *r,
	    float ox, float oy, float scale, double opacity)
{ sdl_mesh_data *md = l->mesh->data;
  int tw = 0, th = 0;
  bool textured = layer_source_size(l, &tw, &th);
  frame_texture *ft;

  if ( l->displayed != ON || !md || md->nvertices < 3 )
    return;
  if ( textured )
  { if ( !(ft = layer_frame_texture(fr, l)) )
      return;
  } else
  { SdlTexture white = white_texture();

    if ( !white || !(ft = sdl_frame_texture(fr, white)) )
      return;
  }

  SDL_Vertex *v = SDL_malloc(md->nvertices*sizeof(*v));
  if ( !v )
    return;

  bool pm = ft->premultiplied && !ft->straight;
  bool none = (l->blend == NAME_none);
  float alpha = (float)(valNum(l->alpha)*opacity);
  float tr = 1.0f, tg = 1.0f, tb = 1.0f;
  layer_map m;

  if ( notNil(l->tint) )
  { SDL_Color c = pceColour2SDL_Color(l->tint);

    tr = c.r/255.0f; tg = c.g/255.0f; tb = c.b/255.0f;
  }
  mesh_map(cv, l, ox, oy, scale, &m);
  for(int i=0; i<md->nvertices; i++)
  { const SDL_Vertex *s = &md->vertices[i];
    float a = none ? s->color.a : s->color.a*alpha;
    float k = pm && !none ? a : 1.0f;

    v[i].position = map_point(&m, s->position.x, s->position.y);
    v[i].color = (SDL_FColor){ s->color.r*tr*k, s->color.g*tg*k,
			       s->color.b*tb*k, a };
    v[i].tex_coord = textured ? (SDL_FPoint){ s->tex_coord.x/tw,
					      s->tex_coord.y/th }
			      : (SDL_FPoint){0.5f, 0.5f};
  }

  SDL_SetTextureBlendMode(ft->texture, layer_blend_mode(l, pm));
  SDL_SetTextureScaleMode(ft->texture, layer_scale_mode(l));
#if SDL_VERSION_ATLEAST(3, 4, 0)
  bool wrap = is_software_renderer(r);
  if ( wrap )
    SDL_SetRenderTextureAddressMode(r, SDL_TEXTURE_ADDRESS_WRAP,
				    SDL_TEXTURE_ADDRESS_WRAP);
#endif
  if ( md->nindices > 0 )
    SDL_RenderGeometry(r, ft->texture, v, md->nvertices,
		       md->indices, md->nindices);
  else
    SDL_RenderGeometry(r, ft->texture, v, md->nvertices/3*3, NULL, 0);
#if SDL_VERSION_ATLEAST(3, 4, 0)
  if ( wrap )
    SDL_SetRenderTextureAddressMode(r, SDL_TEXTURE_ADDRESS_AUTO,
				    SDL_TEXTURE_ADDRESS_AUTO);
#endif
  SDL_free(v);
}


/* Draw the background and layers of `cv` in the renderer of `fr`.
 * `dstrect` is the area of `cv` in device pixels.
 */

static void
draw_layers(FrameObj fr, const layer_canvas *cv, const SDL_FRect *dstrect,
	    float scale, double opacity)
{ WsFrame wfr = fr->ws_ref;
  SDL_Renderer *r = wfr->ws_renderer;
  SDL_Rect clip = { (int)floorf(dstrect->x), (int)floorf(dstrect->y),
		    (int)ceilf(dstrect->w), (int)ceilf(dstrect->h) };

  SDL_SetRenderClipRect(r, &clip);
  if ( instanceOfObject(cv->background, ClassColour) )
  { SDL_Color c = pceColour2SDL_Color(cv->background);

    SDL_SetRenderDrawBlendMode(r, SDL_BLENDMODE_BLEND);
    SDL_SetRenderDrawColor(r, c.r, c.g, c.b, (Uint8)(c.a*opacity));
    SDL_RenderFillRect(r, dstrect);
    SDL_SetRenderDrawBlendMode(r, SDL_BLENDMODE_NONE);
  }

  Cell cell;
  for_cell(cell, cv->layers)
  { SdlLayer l = cell->value;
    frame_texture *ft;
    layer_layout ly;

    if ( notNil(l->mesh) )
    { render_mesh(fr, cv, l, r, dstrect->x, dstrect->y, scale, opacity);
      continue;
    }
    if ( !layer_geometry(cv, l, dstrect->x, dstrect->y, scale, &ly) ||
	 !(ft = layer_frame_texture(fr, l)) )
      continue;

    /* The texture is shared by all layers that show it, so we set its
     * modes for each draw.  SDL scales the alpha, but not the RGB, of
     * premultiplied textures by the alpha mod.  `none` ignores alpha.
     */
    SDL_Texture *tex = ft->texture;
    bool pm = ft->premultiplied && !ft->straight;
    float alpha = (float)(valNum(l->alpha)*opacity);
    float rgb = pm && l->blend != NAME_none ? alpha : 1.0f;
    float tr = rgb, tg = rgb, tb = rgb;

    if ( notNil(l->tint) )
    { SDL_Color c = pceColour2SDL_Color(l->tint);

      tr *= c.r/255.0f; tg *= c.g/255.0f; tb *= c.b/255.0f;
    }
    SDL_SetTextureBlendMode(tex, layer_blend_mode(l, pm));
    SDL_SetTextureScaleMode(tex, layer_scale_mode(l));
    SDL_SetTextureAlphaModFloat(tex, alpha);
    SDL_SetTextureColorModFloat(tex, tr, tg, tb);
    if ( !ly.tiled || ly.sliced || !render_upright_tiles(r, tex, &ly, &clip) )
      for_layer_quads(&ly, render_quad, &(render_target){r, tex});
  }
  SDL_SetRenderClipRect(r, NULL);
}


void
sdl_draw_layers(FrameObj fr, SdlWindow sw, const SDL_FRect *dstrect,
		float scale, double opacity)
{ layer_canvas cv;

  window_canvas(sw, &cv);
  draw_layers(fr, &cv, dstrect, scale, opacity);
}


/* Multiply the colours of the ARGB8888 surface `s` by `c`, as
 * SDL_SetTextureColorModFloat() does.
 */

static void
tint_surface(SDL_Surface *s, Colour tint)
{ SDL_Color c = pceColour2SDL_Color(tint);

  for(int y=0; y<s->h; y++)
  { Uint32 *p = (Uint32*)((Uint8*)s->pixels + (size_t)y*s->pitch);

    for(int x=0; x<s->w; x++)
    { Uint32 v = p[x];
      Uint32 r = ((v>>16)&0xff)*c.r/255;
      Uint32 g = ((v>> 8)&0xff)*c.g/255;
      Uint32 b = ( v     &0xff)*c.b/255;

      p[x] = (v&0xff000000) | (r<<16) | (g<<8) | b;
    }
  }
}


/* Paint the current source for layer `l` */

static void
cairo_paint_layer(cairo_t *cr, SdlLayer l, double opacity)
{ cairo_pattern_set_filter(cairo_get_source(cr),
			   l->scale_mode == NAME_linear
			     ? CAIRO_FILTER_BILINEAR
			     : CAIRO_FILTER_NEAREST);
  cairo_set_operator(cr, layer_cairo_operator(l));
  if ( l->blend == NAME_none )
    cairo_paint(cr);
  else
    cairo_paint_with_alpha(cr, valNum(l->alpha)*opacity);
}


typedef struct
{ cairo_t	   *cr;
  cairo_surface_t *surface;		/* the texture */
  SdlLayer	    layer;
  double	    opacity;
} cairo_quad_context;

static void
cairo_quad(const layer_quad *q, void *closure)
{ cairo_quad_context *ctx = closure;
  cairo_t *cr = ctx->cr;
  cairo_matrix_t m;			/* src pixels -> device pixels */

  cairo_matrix_init(&m,
		    (q->right.x-q->origin.x)/q->src.w,
		    (q->right.y-q->origin.y)/q->src.w,
		    (q->down.x -q->origin.x)/q->src.h,
		    (q->down.y -q->origin.y)/q->src.h,
		    q->origin.x, q->origin.y);
  cairo_save(cr);
  cairo_transform(cr, &m);
  cairo_rectangle(cr, 0, 0, q->src.w, q->src.h);
  cairo_clip(cr);
  cairo_set_source_surface(cr, ctx->surface, -q->src.x, -q->src.y);
  cairo_paint_layer(cr, ctx->layer, ctx->opacity);
  cairo_restore(cr);
}


/* The affine map that takes the texture coordinates `uv` of a
 * triangle to its corners `p`.  False if the texture coordinates are
 * on a line.
 */

static bool
triangle_matrix(const SDL_FPoint uv[3], const SDL_FPoint p[3],
		cairo_matrix_t *m)
{ double du1 = uv[1].x-uv[0].x, dv1 = uv[1].y-uv[0].y;
  double du2 = uv[2].x-uv[0].x, dv2 = uv[2].y-uv[0].y;
  double det = du1*dv2 - du2*dv1;

  if ( fabs(det) < 1e-9 )
    return false;

  double dx1 = p[1].x-p[0].x, dy1 = p[1].y-p[0].y;
  double dx2 = p[2].x-p[0].x, dy2 = p[2].y-p[0].y;
  double a = (dx1*dv2 - dx2*dv1)/det, b = (dx2*du1 - dx1*du2)/det;
  double d = (dy1*dv2 - dy2*dv1)/det, e = (dy2*du1 - dy1*du2)/det;

  cairo_matrix_init(m, a, d, b, e,
		    p[0].x - a*uv[0].x - b*uv[0].y,
		    p[0].y - d*uv[0].x - e*uv[0].y);
  return true;
}


/* Draw the mesh of `l` with Cairo, one triangle at a time.  Colours
 * are interpolated using a mesh pattern.  A textured triangle is
 * painted through the map its texture coordinates define.  If its
 * vertices are not white, the texture is multiplied by their average
 * colour, which is exact for triangles of one colour.  As the
 * renderer, we do not anti-alias the edges, which would show seams
 * between triangles.
 */

static void
cairo_mesh(cairo_t *cr, const layer_canvas *cv, SdlLayer l,
	   float wx, float wy, float scale, double opacity)
{ sdl_mesh_data *md = l->mesh->data;
  SDL_Surface *argb = NULL;
  cairo_surface_t *cs = NULL;
  int tw, th;

  if ( l->displayed != ON || !md || md->nvertices < 3 )
    return;
  if ( layer_source_size(l, &tw, &th) )
  { if ( !(argb = layer_argb_surface(l)) )
      return;
    cs = cairo_image_surface_create_for_data(argb->pixels,
					     CAIRO_FORMAT_ARGB32,
					     argb->w, argb->h, argb->pitch);
  }

  bool none = (l->blend == NAME_none);
  float alpha = (float)(valNum(l->alpha)*opacity);
  float tr = 1.0f, tg = 1.0f, tb = 1.0f;
  int nt = (md->nindices > 0 ? md->nindices : md->nvertices)/3;
  layer_map m;

  if ( notNil(l->tint) )
  { SDL_Color c = pceColour2SDL_Color(l->tint);

    tr = c.r/255.0f; tg = c.g/255.0f; tb = c.b/255.0f;
  }
  mesh_map(cv, l, wx, wy, scale, &m);

  for(int t=0; t<nt; t++)
  { SDL_FPoint p[3], uv[3];
    SDL_FColor c[3];
    SDL_FColor avg = {0, 0, 0, 0};

    for(int k=0; k<3; k++)
    { int i = md->nindices > 0 ? md->indices[t*3+k] : t*3+k;
      const SDL_Vertex *s = &md->vertices[i];

      p[k]  = map_point(&m, s->position.x, s->position.y);
      uv[k] = s->tex_coord;
      c[k]  = (SDL_FColor){ s->color.r*tr, s->color.g*tg, s->color.b*tb,
			    none ? s->color.a : s->color.a*alpha };
      avg.r += c[k].r/3; avg.g += c[k].g/3; avg.b += c[k].b/3;
      avg.a += c[k].a/3;
    }

    cairo_save(cr);
    cairo_set_antialias(cr, CAIRO_ANTIALIAS_NONE);
    cairo_move_to(cr, p[0].x, p[0].y);
    cairo_line_to(cr, p[1].x, p[1].y);
    cairo_line_to(cr, p[2].x, p[2].y);
    cairo_close_path(cr);
    cairo_clip(cr);
    cairo_set_operator(cr, layer_cairo_operator(l));

    if ( !cs )
    { cairo_pattern_t *pat = cairo_pattern_create_mesh();

      cairo_mesh_pattern_begin_patch(pat);
      cairo_mesh_pattern_move_to(pat, p[0].x, p[0].y);
      cairo_mesh_pattern_line_to(pat, p[1].x, p[1].y);
      cairo_mesh_pattern_line_to(pat, p[2].x, p[2].y);
      for(int k=0; k<3; k++)
	cairo_mesh_pattern_set_corner_color_rgba(pat, k, c[k].r, c[k].g,
						 c[k].b, c[k].a);
      cairo_mesh_pattern_end_patch(pat);
      cairo_set_source(cr, pat);
      cairo_paint(cr);
      cairo_pattern_destroy(pat);
    } else
    { cairo_matrix_t tm;

      if ( triangle_matrix(uv, p, &tm) &&
	   cairo_matrix_invert(&tm) == CAIRO_STATUS_SUCCESS )
      { cairo_pattern_t *tex = cairo_pattern_create_for_surface(cs);
	bool white = avg.r > 0.999f && avg.g > 0.999f && avg.b > 0.999f;

	cairo_pattern_set_matrix(tex, &tm);
	cairo_pattern_set_filter(tex, l->scale_mode == NAME_linear
					? CAIRO_FILTER_BILINEAR
					: CAIRO_FILTER_NEAREST);
	if ( !white )			/* texture * colour, in a group */
	{ cairo_push_group(cr);
	  cairo_set_operator(cr, CAIRO_OPERATOR_SOURCE);
	  cairo_set_source(cr, tex);
	  cairo_paint(cr);
	  cairo_set_operator(cr, CAIRO_OPERATOR_MULTIPLY);
	  cairo_set_source_rgb(cr, avg.r, avg.g, avg.b);
	  cairo_paint(cr);
	  cairo_set_operator(cr, CAIRO_OPERATOR_DEST_IN);
	  cairo_set_source(cr, tex);
	  cairo_paint(cr);
	  cairo_pop_group_to_source(cr);
	  cairo_set_operator(cr, layer_cairo_operator(l));
	} else
	{ cairo_set_source(cr, tex);
	}
	if ( none )
	  cairo_paint(cr);
	else
	  cairo_paint_with_alpha(cr, avg.a);
	cairo_pattern_destroy(tex);
      }
    }
    cairo_restore(cr);
  }

  if ( cs )
  { cairo_surface_destroy(cs);
    SDL_DestroySurface(argb);
  }
}


/* As draw_layers(), drawing onto a Cairo context for <-image of a
 * frame or window.  `cv` is at `wx`,`wy` in device pixels.
 */

static void
cairo_layers(cairo_t *cr, const layer_canvas *cv, float wx, float wy,
	     float scale, double opacity)
{ cairo_save(cr);
  cairo_rectangle(cr, wx, wy, cv->w*scale, cv->h*scale);
  cairo_clip(cr);
  if ( instanceOfObject(cv->background, ClassColour) )
  { SDL_Color c = pceColour2SDL_Color(cv->background);

    cairo_set_source_rgba(cr, c.r/255.0, c.g/255.0, c.b/255.0, c.a/255.0);
    cairo_paint_with_alpha(cr, opacity);
  }

  Cell cell;
  for_cell(cell, cv->layers)
  { SdlLayer l = cell->value;
    SDL_Surface *argb;
    layer_layout ly;

    if ( notNil(l->mesh) )
    { cairo_mesh(cr, cv, l, wx, wy, scale, opacity);
      continue;
    }

    if ( !layer_geometry(cv, l, wx, wy, scale, &ly) ||
	 !(argb = layer_argb_surface(l)) )
      continue;

    if ( notNil(l->tint) )
      tint_surface(argb, l->tint);
    cairo_surface_t *cs =
      cairo_image_surface_create_for_data(argb->pixels, CAIRO_FORMAT_ARGB32,
					  argb->w, argb->h, argb->pitch);
    cairo_surface_t *tile = NULL;
    const SDL_FRect *src = &ly.src, *dst = &ly.dst;
    cairo_matrix_t m;			/* window -> device pixels */

    cairo_matrix_init(&m, ly.map.xx, ly.map.yx, ly.map.xy, ly.map.yy,
		      ly.map.tx, ly.map.ty);
    cairo_matrix_t inv = m;
    if ( cairo_matrix_invert(&inv) != CAIRO_STATUS_SUCCESS )
    { cairo_surface_destroy(cs);	/* zero size: nothing to show */
      SDL_DestroySurface(argb);
      continue;
    }
    if ( ly.sliced )
    { cairo_quad_context ctx = { cr, cs, l, opacity };

      for_layer_quads(&ly, cairo_quad, &ctx);
      cairo_surface_destroy(cs);
      SDL_DestroySurface(argb);
      continue;
    }
    cairo_save(cr);
    cairo_transform(cr, &m);
    cairo_rectangle(cr, dst->x, dst->y, dst->w, dst->h);
    cairo_clip(cr);
    if ( ly.tiled )
    { tile = cairo_surface_create_for_rectangle(cs, src->x, src->y,
						src->w, src->h);
      cairo_translate(cr, ly.tile.x, ly.tile.y);
      cairo_set_source_surface(cr, tile, 0, 0);
      cairo_pattern_set_extend(cairo_get_source(cr), CAIRO_EXTEND_REPEAT);
    } else
    { cairo_translate(cr, dst->x, dst->y);
      cairo_scale(cr, dst->w/src->w, dst->h/src->h);
      cairo_set_source_surface(cr, cs, -src->x, -src->y);
    }
    cairo_paint_layer(cr, l, opacity);
    cairo_restore(cr);

    if ( tile )
      cairo_surface_destroy(tile);
    cairo_surface_destroy(cs);
    SDL_DestroySurface(argb);
  }
  cairo_restore(cr);
}


void
sdl_cairo_layers(cairo_t *cr, SdlWindow sw, float wx, float wy,
		 float scale, double opacity)
{ layer_canvas cv;

  window_canvas(sw, &cv);
  cairo_layers(cr, &cv, wx, wy, scale, opacity);
}


		 /*******************************
		 *	      MESHES		*
		 *******************************/

static sdl_mesh_data *
mesh_data(SdlMesh m)
{ if ( !m->data )
    m->data = SDL_calloc(1, sizeof(*m->data));

  return m->data;
}


static bool
mesh_room(sdl_mesh_data *md, int nv, int ni)
{ if ( nv > md->avertices )
  { int n = SDL_max(nv, md->avertices*2);
    SDL_Vertex *new = SDL_realloc(md->vertices, (size_t)n*sizeof(*new));

    if ( !new )
      return false;
    md->vertices = new;
    md->avertices = n;
  }
  if ( ni > md->aindices )
  { int n = SDL_max(ni, md->aindices*2);
    int *new = SDL_realloc(md->indices, (size_t)n*sizeof(*new));

    if ( !new )
      return false;
    md->indices = new;
    md->aindices = n;
  }

  return true;
}


void
ws_destroy_sdl_mesh(SdlMesh m)
{ sdl_mesh_data *md = m->data;

  if ( md )
  { m->data = NULL;
    SDL_free(md->vertices);
    SDL_free(md->indices);
    SDL_free(md);
  }
}


void
ws_sdl_mesh_clear(SdlMesh m)
{ sdl_mesh_data *md = m->data;

  if ( md )
    md->nvertices = md->nindices = 0;
}


/* Append a vertex and return its index or -1 if there is no memory */

int
ws_sdl_mesh_append(SdlMesh m, float x, float y, float u, float v, Colour c)
{ sdl_mesh_data *md = mesh_data(m);

  if ( !md || !mesh_room(md, md->nvertices+1, 0) )
    return -1;

  SDL_Vertex *vx = &md->vertices[md->nvertices];
  vx->position  = (SDL_FPoint){x, y};
  vx->tex_coord = (SDL_FPoint){u, v};
  if ( notDefault(c) )
  { SDL_Color sc = pceColour2SDL_Color(c);

    vx->color = (SDL_FColor){sc.r/255.0f, sc.g/255.0f, sc.b/255.0f,
			     sc.a/255.0f};
  } else
  { vx->color = (SDL_FColor){1, 1, 1, 1};
  }

  return md->nvertices++;
}


status
ws_sdl_mesh_triangle(SdlMesh m, int i0, int i1, int i2)
{ sdl_mesh_data *md = mesh_data(m);

  if ( !md || !mesh_room(md, 0, md->nindices+3) )
    fail;
  md->indices[md->nindices++] = i0;
  md->indices[md->nindices++] = i1;
  md->indices[md->nindices++] = i2;

  succeed;
}


/* Replace the vertices and indices.  The caller checks them */

status
ws_sdl_mesh_set(SdlMesh m, const SDL_Vertex *v, int nv, const int *idx, int ni)
{ sdl_mesh_data *md = mesh_data(m);

  if ( !md || !mesh_room(md, nv, ni) )
    fail;
  if ( nv > 0 )
    memcpy(md->vertices, v, (size_t)nv*sizeof(*v));
  if ( ni > 0 )
    memcpy(md->indices, idx, (size_t)ni*sizeof(*idx));
  md->nvertices = nv;
  md->nindices  = ni;

  succeed;
}


int
ws_sdl_mesh_count(SdlMesh m)
{ return m->data ? m->data->nvertices : 0;
}


		 /*******************************
		 *	      CANVASES		*
		 *******************************/

/* Each frame that shows an sdl_canvas keeps a target texture of the
 * size of the canvas.  sdl_render_canvases() draws the canvases that
 * changed before the frame is drawn, a canvas shown by another canvas
 * first.  A canvas is drawn again if its generation differs from the
 * one drawn, or if what it shows changed: an sdl_texture that is stale
 * in this frame or a canvas drawn in this pass.  The pixels drawn are
 * premultiplied.
 *
 * Note that the software renderer ignores premultiplied blending for
 * triangles: a translucent canvas on a sheared layer or mesh is drawn
 * incorrectly there.
 */

static frame_texture *
canvas_frame_texture(FrameObj fr, SdlCanvas c)
{ frame_texture *ft = find_frame_texture(fr->ws_ref, c);

  return ft && ft->texture ? ft : NULL;
}


/* Does a layer of `c` show something that changed in this frame?
 * `drawn` holds the canvases drawn in this pass.
 */

static bool
canvas_inputs_changed(WsFrame wfr, SdlCanvas c, SdlCanvas *drawn, int ndrawn)
{ Cell cell;

  for_cell(cell, c->layers)
  { SdlLayer l = cell->value;

    if ( instanceOfObject(l->texture, ClassSdlCanvas) )
    { for(int i=0; i<ndrawn; i++)
      { if ( drawn[i] == l->texture )
	  return true;
      }
    } else if ( notNil(l->texture) )
    { frame_texture *ft = find_frame_texture(wfr, l->texture);

      if ( !ft || ft->stale )
	return true;
    }
  }

  return false;
}


/* Draw the layers of `c` into its texture in `fr` */

static bool
render_canvas(FrameObj fr, SdlCanvas c, bool force)
{ WsFrame wfr = fr->ws_ref;
  SDL_Renderer *r = wfr->ws_renderer;
  frame_texture *ft;
  int w, h;

  if ( !canvas_size(c, &w, &h) ||
       (!(ft = find_frame_texture(wfr, c)) &&
	!(ft = add_frame_texture(wfr, c))) )
    return false;

  bool fresh = false;
  if ( ft->texture && (ft->texture->w != w || ft->texture->h != h) )
  { SDL_DestroyTexture(ft->texture);
    ft->texture = NULL;
  }
  if ( !ft->texture )
  { if ( !(ft->texture = SDL_CreateTexture(r, SDL_PIXELFORMAT_ARGB8888,
					   SDL_TEXTUREACCESS_TARGET, w, h)) )
    { Cprintf("%s: cannot create texture: %s\n", pp(c), SDL_GetError());
      return false;
    }
    ft->made_for = SDL_PIXELFORMAT_ARGB8888;
    ft->premultiplied = true;
    ft->straight = false;
    fresh = true;
  }
  if ( !fresh && !force && ft->generation == c->generation )
    return false;

  SDL_Texture *prev = SDL_GetRenderTarget(r);
  SDL_SetRenderTarget(r, ft->texture);
  if ( fresh || c->clear != OFF )
  { SDL_SetRenderDrawBlendMode(r, SDL_BLENDMODE_NONE);
    SDL_SetRenderDrawColor(r, 0, 0, 0, 0);
    SDL_RenderClear(r);
  }
  layer_canvas cv = { c->layers, c->background, (float)w, (float)h, 0, 0 };
  SDL_FRect all = { 0, 0, (float)w, (float)h };
  draw_layers(fr, &cv, &all, 1.0f, 1.0);
  SDL_SetRenderTarget(r, prev);
  ft->generation = c->generation;
  ft->stale = false;

  return true;
}


/* Collect the canvases shown by the sdl_window objects of `fr`, either
 * directly or through other canvases, such that a canvas comes after
 * the canvases it shows.  This is a depth-first search using an
 * explicit stack.  A canvas that shows itself, directly or indirectly,
 * is reported and the cycle is cut.
 */

typedef struct
{ SdlCanvas	canvas;
  int		state;			/* see below */
} canvas_node;

#define CN_PUSHED	1			/* on the stack */
#define CN_EXPANDED	2			/* its canvases are pushed */
#define CN_DONE		3			/* in the order */

typedef struct
{ canvas_node  *nodes;			/* canvases seen */
  int		nnodes, anodes;
  SdlCanvas    *stack;
  int		nstack, astack;
  SdlCanvas    *order;			/* result */
  int		norder, aorder;
} canvas_search;

static bool
grow(void **ptr, int *alloc, int need, size_t size)
{ if ( need > *alloc )
  { int n = SDL_max(need, *alloc ? *alloc*2 : 16);
    void *new = SDL_realloc(*ptr, (size_t)n*size);

    if ( !new )
      return false;
    *ptr = new;
    *alloc = n;
  }

  return true;
}


static canvas_node *
canvas_node_of(canvas_search *cs, SdlCanvas c)
{ for(int i=0; i<cs->nnodes; i++)
  { if ( cs->nodes[i].canvas == c )
      return &cs->nodes[i];
  }
  if ( !grow((void**)&cs->nodes, &cs->anodes, cs->nnodes+1,
	     sizeof(*cs->nodes)) )
    return NULL;
  canvas_node *n = &cs->nodes[cs->nnodes++];
  n->canvas = c;
  n->state  = 0;

  return n;
}


static bool
push_canvas(canvas_search *cs, SdlCanvas c)
{ canvas_node *n = canvas_node_of(cs, c);

  if ( !n )
    return false;
  if ( n->state == CN_EXPANDED )
  { Cprintf("%s: shows itself; not drawing it again\n", pp(c));
    return true;
  }
  if ( n->state == CN_DONE )
    return true;
  if ( !grow((void**)&cs->stack, &cs->astack, cs->nstack+1,
	     sizeof(*cs->stack)) )
    return false;
  n->state = CN_PUSHED;
  cs->stack[cs->nstack++] = c;

  return true;
}


static void
push_layer_canvases(canvas_search *cs, Chain layers)
{ Cell cell;

  for_cell(cell, layers)
  { SdlLayer l = cell->value;

    if ( instanceOfObject(l->texture, ClassSdlCanvas) )
      push_canvas(cs, l->texture);
  }
}


static void
order_canvases(canvas_search *cs)
{ while( cs->nstack > 0 )
  { SdlCanvas c = cs->stack[cs->nstack-1];
    canvas_node *n = canvas_node_of(cs, c);

    if ( !n )
      return;
    if ( n->state == CN_PUSHED )	/* first visit: push what it shows */
    { n->state = CN_EXPANDED;
      push_layer_canvases(cs, c->layers);
    } else				/* all it shows is done */
    { cs->nstack--;
      if ( n->state == CN_EXPANDED &&
	   grow((void**)&cs->order, &cs->aorder, cs->norder+1,
		sizeof(*cs->order)) )
      { n->state = CN_DONE;
	cs->order[cs->norder++] = c;
      }
    }
  }
}


static void
free_canvas_search(canvas_search *cs)
{ SDL_free(cs->nodes);
  SDL_free(cs->stack);
  SDL_free(cs->order);
}


/* Draw the canvases of `cs->order` that changed in `fr` */

static void
render_ordered_canvases(FrameObj fr, canvas_search *cs)
{ SdlCanvas *drawn = SDL_malloc((cs->norder ? cs->norder : 1)*sizeof(*drawn));
  int ndrawn = 0;

  if ( !drawn )
    return;
  for(int i=0; i<cs->norder; i++)
  { SdlCanvas c = cs->order[i];
    bool force = canvas_inputs_changed(fr->ws_ref, c, drawn, ndrawn);

    if ( render_canvas(fr, c, force) )
      drawn[ndrawn++] = c;
  }
  SDL_free(drawn);
}


/* Push the canvases shown by the sdl_window objects in the window tree
 * of `sw`.  Uses an explicit stack of windows.
 */

static void
push_window_canvases(canvas_search *cs, PceWindow root)
{ PceWindow *stack = NULL;
  int n = 0, a = 0;

  if ( !grow((void**)&stack, &a, 1, sizeof(*stack)) )
    return;
  stack[n++] = root;
  while( n > 0 )
  { PceWindow sw = stack[--n];

    if ( instanceOfObject(sw, ClassSdlWindow) )
      push_layer_canvases(cs, ((SdlWindow)sw)->layers);
    if ( instanceOfObject(sw, ClassWindowDecorator) &&
	 grow((void**)&stack, &a, n+1, sizeof(*stack)) )
      stack[n++] = ((WindowDecorator)sw)->window;
    if ( notNil(sw->subwindows) )
    { Cell cell;

      for_cell(cell, sw->subwindows)
      { if ( grow((void**)&stack, &a, n+1, sizeof(*stack)) )
	  stack[n++] = cell->value;
      }
    }
  }
  SDL_free(stack);
}


/* Draw the canvases shown in `fr` that changed.  Called before the
 * frame is drawn.
 */

void
sdl_render_canvases(FrameObj fr)
{ canvas_search cs = {0};
  Cell cell;

  for_cell(cell, fr->members)
    push_window_canvases(&cs, cell->value);
  if ( cs.nstack > 0 )
  { order_canvases(&cs);
    render_ordered_canvases(fr, &cs);
  }
  free_canvas_search(&cs);
}


/* Make sure `c` and the canvases it shows are drawn in `fr` */

static void
render_canvas_in(FrameObj fr, SdlCanvas c)
{ canvas_search cs = {0};

  if ( push_canvas(&cs, c) )
  { order_canvases(&cs);
    render_ordered_canvases(fr, &cs);
  }
  free_canvas_search(&cs);
}


/* A created frame to read `c` from, preferably one that has drawn it */

typedef struct
{ SdlCanvas	canvas;
  FrameObj	any;			/* some created frame */
  FrameObj	has;			/* one that has drawn the canvas */
} canvas_frame_search;

static void
find_canvas_frame(FrameObj fr, WsFrame wfr, void *closure)
{ canvas_frame_search *s = closure;

  if ( !s->any )
    s->any = fr;
  if ( !s->has && canvas_frame_texture(fr, s->canvas) )
    s->has = fr;
}


/* Read the pixels of `c` as a new ARGB8888 surface with premultiplied
 * alpha, drawing the canvas first if needed.  NULL if there is no
 * created frame, i.e., no renderer.
 */

static SDL_Surface *
canvas_surface(SdlCanvas c)
{ canvas_frame_search s = { c, NULL, NULL };

  for_created_frames(find_canvas_frame, &s);
  FrameObj fr = s.has ? s.has : s.any;
  if ( !fr )
    return NULL;

  render_canvas_in(fr, c);
  frame_texture *ft = canvas_frame_texture(fr, c);
  if ( !ft )
    return NULL;

  SDL_Renderer *r = ((WsFrame)fr->ws_ref)->ws_renderer;
  SDL_Texture *prev = SDL_GetRenderTarget(r);
  SDL_SetRenderTarget(r, ft->texture);
  SDL_Surface *read = SDL_RenderReadPixels(r, NULL);
  SDL_SetRenderTarget(r, prev);
  if ( !read )
    return NULL;

  SDL_Surface *argb = SDL_ConvertSurface(read, SDL_PIXELFORMAT_ARGB8888);
  SDL_DestroySurface(read);
  if ( argb )
    set_premultiplied(argb, true);

  return argb;
}


Image
ws_sdl_canvas_image(SdlCanvas c)
{ SDL_Surface *s = canvas_surface(c);

  if ( !s )
  { errorPce(c, NAME_noRenderer);
    return NULL;
  }

  Image image = newObject(ClassImage, NIL, ONE, ONE, NAME_pixmap, EAV);
  status rc = image && ws_set_image_pixels(image, s);
  SDL_DestroySurface(s);

  return rc ? image : NULL;
}


/* `c` changed: redraw the frames that show it */

static void
changed_canvas(FrameObj fr, WsFrame wfr, void *closure)
{ if ( find_frame_texture(wfr, closure) && ChangedFrames )
    addChain(ChangedFrames, fr);
}


void
ws_sdl_canvas_changed(SdlCanvas c)
{ for_created_frames(changed_canvas, c);
}


void
ws_destroy_sdl_canvas(SdlCanvas c)
{ for_created_frames(forget_texture, c);
}
