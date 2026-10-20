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
#include <SDL3_image/SDL_image.h>

/* Cameras.  SDL has no event for a new frame, so the open cameras are
 * polled from the media tick (see sdlaudio.c).  The camera subsystem
 * is started on first use, as this may ask the user for permission.
 */

typedef struct open_camera
{ struct open_camera *next;
  Camera	camera;			/* we hold a code reference */
  SDL_Camera   *sdl;
  VideoWriter	writer;			/* writer that writer_t0 is for */
  Uint64	writer_t0;		/* timestamp of its first frame */
  double	writer_at;		/* time in the video at writer_t0 */
} open_camera;

static open_camera *cameras = NULL;

static bool
init_cameras(void)
{ if ( !SDL_WasInit(SDL_INIT_CAMERA) &&
       !SDL_InitSubSystem(SDL_INIT_CAMERA) )
  { Cprintf("Could not initialise cameras: %s\n", SDL_GetError());
    return false;
  }

  return true;
}


Chain
ws_cameras(void)
{ Chain ch = answerObject(ClassChain, EAV);

  if ( init_cameras() )
  { int count = 0;
    SDL_CameraID *ids = SDL_GetCameras(&count);

    for(int i=0; i<count; i++)
    { const char *name = SDL_GetCameraName(ids[i]);

      if ( name )
	appendChain(ch, UTF8ToName(name));
    }
    SDL_free(ids);
  }

  return ch;
}


static void
set_status(Camera c, Name status)
{ if ( c->status == status )
    return;

  assign(c, status, status);
  if ( notNil(c->status_message) )
    forwardReceiverCode(c->status_message, c, c, status, EAV);
}


static SDL_CameraID
find_camera(Camera c)
{ int count = 0;
  SDL_CameraID *ids = SDL_GetCameras(&count);
  SDL_CameraID id = 0;

  for(int i=0; i<count && !id; i++)
  { const char *name = SDL_GetCameraName(ids[i]);

    if ( isNil(c->name) || (name && UTF8ToName(name) == c->name) )
      id = ids[i];
  }
  SDL_free(ids);

  return id;
}


static double
spec_fps(const SDL_CameraSpec *s)
{ return s->framerate_denominator ?
	   (double)s->framerate_numerator/s->framerate_denominator : 0.0;
}

/* True if `a` is a better match for a w x h video than `b`: the exact
 * size, else the smallest that is at least as large, else the largest.
 * Then the highest frame rate and an uncompressed format, which does
 * not have to be decoded.
 */

static bool
better_spec(const SDL_CameraSpec *a, const SDL_CameraSpec *b, int w, int h)
{ bool a_fits = a->width >= w && a->height >= h;
  bool b_fits = b->width >= w && b->height >= h;
  long a_area = (long)a->width*a->height;
  long b_area = (long)b->width*b->height;

  if ( a_fits != b_fits )
    return a_fits;
  if ( a_area != b_area )
    return a_fits ? a_area < b_area : a_area > b_area;
  if ( spec_fps(a) != spec_fps(b) )
    return spec_fps(a) > spec_fps(b);

  return a->format != SDL_PIXELFORMAT_MJPG && b->format == SDL_PIXELFORMAT_MJPG;
}

static bool
choose_spec(SDL_CameraID id, int w, int h, SDL_CameraSpec *spec)
{ int count = 0;
  SDL_CameraSpec **specs = SDL_GetCameraSupportedFormats(id, &count);
  const SDL_CameraSpec *best = NULL;

  for(int i=0; i<count; i++)
  { if ( !best || better_spec(specs[i], best, w, h) )
      best = specs[i];
  }
  if ( best )
    *spec = *best;
  SDL_free(specs);

  return best != NULL;
}


/* The sizes the camera supports, smallest first */

Chain
ws_camera_sizes(Camera c)
{ Chain ch = answerObject(ClassChain, EAV);
  SDL_CameraID id;

  if ( init_cameras() && (id = find_camera(c)) )
  { int count = 0;
    SDL_CameraSpec **specs = SDL_GetCameraSupportedFormats(id, &count);

    for(int i=0; i<count; i++)		/* insertion sort, no duplicates */
    { int w = specs[i]->width, h = specs[i]->height;
      Cell cell;
      Any before = NIL;
      bool dup = false;

      for_cell(cell, ch)
      { Size s = cell->value;
	long a = (long)valInt(s->w)*valInt(s->h);

	if ( valInt(s->w) == w && valInt(s->h) == h )
	{ dup = true;
	  break;
	}
	if ( a > (long)w*h )
	{ before = s;
	  break;
	}
      }
      if ( !dup )
      { Size s = newObject(ClassSize, toInt(w), toInt(h), EAV);

	if ( isNil(before) )
	  appendChain(ch, s);
	else
	  insertBeforeChain(ch, s, before);
      }
    }
    SDL_free(specs);
  }

  return ch;
}


status
ws_open_camera(Camera c, Size size)
{ if ( !init_cameras() )
    fail;

  SDL_CameraID id = find_camera(c);
  if ( !id )
    return errorPce(c, NAME_noCamera);

  SDL_CameraSpec spec;
  bool have_spec = ( notDefault(size) &&
		     choose_spec(id, valInt(size->w), valInt(size->h), &spec) );
  SDL_Camera *cam = SDL_OpenCamera(id, have_spec ? &spec : NULL);
  if ( !cam )
    return errorPce(c, NAME_cannotOpen, CtoName(SDL_GetError()));

  open_camera *oc = SDL_calloc(1, sizeof(*oc));
  oc->camera = c;
  oc->sdl    = cam;
  oc->next   = cameras;
  cameras    = oc;
  c->ws_ref  = oc;
  addCodeReference(c);
  sdl_add_media_source(sdl_camera_tick);

  switch(SDL_GetCameraPermissionState(cam))
  { case SDL_CAMERA_PERMISSION_STATE_APPROVED:
      set_status(c, NAME_open);
      break;
    case SDL_CAMERA_PERMISSION_STATE_DENIED:
      set_status(c, NAME_denied);
      break;
    default:
      set_status(c, NAME_waiting);
  }
  sdl_start_media_tick();

  succeed;
}


void
ws_close_camera(Camera c)
{ open_camera *oc = c->ws_ref;

  if ( !oc )
    return;

  for(open_camera **pp = &cameras; *pp; pp = &(*pp)->next)
  { if ( *pp == oc )
    { *pp = oc->next;
      break;
    }
  }
  SDL_CloseCamera(oc->sdl);
  SDL_free(oc);
  c->ws_ref = NULL;
  if ( !onFlag(c, F_FREED|F_FREEING) )
    set_status(c, NAME_closed);
  delCodeReference(c);
}


/* Append the frame, now in the texture or image of the camera, to
 * <-writer.  The frames get the time of the camera's timestamps, which
 * start at the next frame of the writer.
 */

static void
write_frame(open_camera *oc, Uint64 ts)
{ Camera c = oc->camera;
  VideoWriter vw = c->writer;

  if ( oc->writer != vw )
  { oc->writer	  = vw;
    oc->writer_t0 = ts;
    oc->writer_at = valInt(vw->frames)/valNum(vw->frame_rate);
  }

  double at = oc->writer_at + (double)(ts-oc->writer_t0)/1e9;
  Any frame = notNil(c->texture) ? (Any)c->texture : (Any)c->image;

  if ( !send(vw, NAME_append, frame, toNum(at), EAV) )
    assign(c, writer, NIL);		/* do not repeat the error */
}


/* Copy a frame into the texture or image of `c`, append it to the
 * writer and run <-message
 */

static void
deliver_frame(open_camera *oc, SDL_Surface *frame, Uint64 ts)
{ Camera c = oc->camera;
  status rc;

  if ( notNil(c->texture) )
    rc = ws_sdl_texture_set_surface(c->texture, frame);
  else
    rc = ws_set_image_pixels(c->image, frame);

  if ( rc && notNil(c->writer) )
    write_frame(oc, ts);
  if ( rc && notNil(c->message) )
    forwardReceiverCode(c->message, c, c, EAV);
}


bool
sdl_camera_tick(void)
{ for(open_camera *oc = cameras; oc; )
  { open_camera *next = oc->next;	/* message may close it */
    Camera c = oc->camera;

    if ( c->status == NAME_open )
    { Uint64 ts;
      SDL_Surface *frame = SDL_AcquireCameraFrame(oc->sdl, &ts);

      if ( frame )
      { addCodeReference(c);
	SDL_Camera *cam = oc->sdl;
	deliver_frame(oc, frame, ts);
	SDL_ReleaseCameraFrame(cam, frame);
	delCodeReference(c);
	freeableObj(c);
      }
    }
    oc = next;
  }

  return cameras != NULL;
}


/* Permission decisions and cameras that are unplugged */

bool
sdl_camera_event(SDL_Event *event)
{ switch(event->type)
  { case SDL_EVENT_CAMERA_DEVICE_APPROVED:
    case SDL_EVENT_CAMERA_DEVICE_DENIED:
    case SDL_EVENT_CAMERA_DEVICE_REMOVED:
      break;
    default:
      return false;
  }

  pceMTLock();
  for(open_camera *oc = cameras; oc; )
  { open_camera *next = oc->next;

    if ( SDL_GetCameraID(oc->sdl) == event->cdevice.which )
    { Camera c = oc->camera;

      switch(event->type)
      { case SDL_EVENT_CAMERA_DEVICE_APPROVED:
	  set_status(c, NAME_open);
	  break;
	case SDL_EVENT_CAMERA_DEVICE_DENIED:
	  set_status(c, NAME_denied);
	  break;
	default:			/* removed */
	  ws_close_camera(c);
      }
    }
    oc = next;
  }
  pceMTUnlock();

  return true;
}


/* Testing: deliver a frame of `w` x `h` pixels of colour in one of the
 * formats of cameras: YUY2 or MJPG, which holds a JPEG image.  Like the
 * MJPG of many webcams, the JPEG has no Huffman tables (DHT segments):
 * the decoder must use the standard tables, which SDL's own conversion
 * and some decoders of SDL_image do not do.
 */

static size_t
strip_dht(Uint8 *d, size_t len)
{ size_t i = 2, o = 2;			/* after SOI */

  while( i+4 <= len && d[i] == 0xFF )
  { Uint8 marker = d[i+1];
    size_t seglen = 2 + ((size_t)d[i+2]<<8 | d[i+3]);

    if ( marker == 0xDA )		/* start of scan: copy the rest */
      break;
    if ( marker != 0xC4 )
    { memmove(d+o, d+i, seglen);
      o += seglen;
    }
    i += seglen;
  }
  memmove(d+o, d+i, len-i);

  return o + (len-i);
}

static SDL_Surface *
mjpg_surface(SDL_Surface *rgb, void **data)
{ SDL_IOStream *io = SDL_IOFromDynamicMem();
  SDL_Surface *mjpg = NULL;

  *data = NULL;
  if ( io && IMG_SaveJPG_IO(rgb, io, false, 90) )
  { Sint64 len = SDL_GetIOSize(io);
    void *mem = SDL_GetPointerProperty(SDL_GetIOProperties(io),
				       SDL_PROP_IOSTREAM_DYNAMIC_MEMORY_POINTER,
				       NULL);

    if ( len > 0 && mem && (*data = SDL_malloc((size_t)len)) )
    { memcpy(*data, mem, (size_t)len);
      len = (Sint64)strip_dht(*data, (size_t)len);
      mjpg = SDL_CreateSurfaceFrom(rgb->w, rgb->h, SDL_PIXELFORMAT_MJPG,
				   *data, (int)len);
    }
  }
  if ( io )
    SDL_CloseIO(io);

  return mjpg;
}

status
ws_test_camera_frame(Camera c, int w, int h, Colour colour, Name format)
{ SDL_Surface *rgb = SDL_CreateSurface(w, h, SDL_PIXELFORMAT_XRGB8888);

  if ( !rgb )
    fail;
  SDL_FillSurfaceRect(rgb, NULL,
		      SDL_MapSurfaceRGB(rgb,
					(Uint8)valInt(getRedColour(colour)),
					(Uint8)valInt(getGreenColour(colour)),
					(Uint8)valInt(getBlueColour(colour))));
  void *data = NULL;
  SDL_Surface *frame = ( format == NAME_mjpg	/* BT.709, as many webcams */
			 ? mjpg_surface(rgb, &data)
			 : SDL_ConvertSurfaceAndColorspace(
			     rgb, SDL_PIXELFORMAT_YUY2, NULL,
			     SDL_COLORSPACE_BT709_LIMITED, 0) );
  SDL_DestroySurface(rgb);
  if ( !frame )
  { Cprintf("Cannot create %s frame: %s\n", pp(format), SDL_GetError());
    SDL_free(data);
    fail;
  }

  static open_camera closed_camera;	/* for a camera that is not open */
  open_camera *oc = c->ws_ref;

  if ( !oc )
  { if ( closed_camera.camera != c )
      closed_camera = (open_camera){ .camera = c };
    oc = &closed_camera;
  }
  deliver_frame(oc, frame, SDL_GetTicksNS());
  SDL_DestroySurface(frame);
  SDL_free(data);

  succeed;
}
