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

/* Cameras.  SDL has no event for a new frame, so the open cameras are
 * polled from the media tick (see sdlaudio.c).  The camera subsystem
 * is started on first use, as this may ask the user for permission.
 */

typedef struct open_camera
{ struct open_camera *next;
  Camera	camera;			/* we hold a code reference */
  SDL_Camera   *sdl;
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


status
ws_open_camera(Camera c)
{ if ( !init_cameras() )
    fail;

  SDL_CameraID id = find_camera(c);
  if ( !id )
    return errorPce(c, NAME_noCamera);

  SDL_Camera *cam = SDL_OpenCamera(id, NULL);
  if ( !cam )
    return errorPce(c, NAME_cannotOpen, CtoName(SDL_GetError()));

  open_camera *oc = SDL_calloc(1, sizeof(*oc));
  oc->camera = c;
  oc->sdl    = cam;
  oc->next   = cameras;
  cameras    = oc;
  c->ws_ref  = oc;
  addCodeReference(c);

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


/* Copy a frame into the image of `c` and run <-message */

static void
deliver_frame(Camera c, SDL_Surface *frame)
{ if ( ws_set_image_pixels(c->image, frame) &&
       notNil(c->message) )
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
	deliver_frame(c, frame);
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


/* Testing: deliver a frame of `w` x `h` pixels of colour in the YUY2
 * format of many cameras, so that it is converted as a camera frame.
 */

status
ws_test_camera_frame(Camera c, int w, int h, Colour colour)
{ SDL_Surface *rgb = SDL_CreateSurface(w, h, SDL_PIXELFORMAT_XRGB8888);

  if ( !rgb )
    fail;
  SDL_FillSurfaceRect(rgb, NULL,
		      SDL_MapSurfaceRGB(rgb,
					(Uint8)valInt(getRedColour(colour)),
					(Uint8)valInt(getGreenColour(colour)),
					(Uint8)valInt(getBlueColour(colour))));
  SDL_Surface *yuv = SDL_ConvertSurface(rgb, SDL_PIXELFORMAT_YUY2);
  SDL_DestroySurface(rgb);
  if ( !yuv )
  { Cprintf("Cannot create YUY2 frame: %s\n", SDL_GetError());
    fail;
  }

  deliver_frame(c, yuv);
  SDL_DestroySurface(yuv);

  succeed;
}
