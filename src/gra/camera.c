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

/* A camera delivers video as an image.  ->open starts the camera;
 * each new frame is copied into <-image, so a bitmap that displays
 * this image shows the video, and <-message is executed.  If
 * <-texture is an sdl_texture, frames go there instead, in the format
 * of the camera, and an sdl_layer shows them without using Cairo.  The user may
 * have to give permission to use the camera: <-status is `waiting`
 * until this is decided.
 */

static status
initialiseCamera(Camera c, Name name)
{ assign(c, name,	    isDefault(name) ? NIL : name);
  assign(c, image,	    newObject(ClassImage, NIL, ONE, ONE,
				      NAME_pixmap, EAV));
  assign(c, status,	    NAME_closed);
  assign(c, message,	    NIL);
  assign(c, status_message, NIL);
  assign(c, texture,	    NIL);
  assign(c, writer,	    NIL);
  c->ws_ref = NULL;

  succeed;
}


static status
unlinkCamera(Camera c)
{ ws_close_camera(c);

  succeed;
}


static status
openCamera(Camera c, Size size)
{ if ( c->status != NAME_closed )
    succeed;

  return ws_open_camera(c, size);
}


static Chain
getSizesCamera(Camera c)
{ answer(ws_camera_sizes(c));
}


static status
closeCamera(Camera c)
{ ws_close_camera(c);

  succeed;
}


static status
sdlTestFrameCamera(Camera c, Int w, Int h, Colour colour, Name format)
{ return ws_test_camera_frame(c, valInt(w), valInt(h), colour,
			      isDefault(format) ? NAME_yuy2 : format);
}


/* Instance Variables */

static vardecl var_camera[] =
{ IV(NAME_name, "name*", IV_GET,
     NAME_device, "Name of the camera; @nil uses the first"),
  IV(NAME_image, "image", IV_GET,
     NAME_video, "The last frame"),
  IV(NAME_status, "{closed,waiting,open,denied}", IV_GET,
     NAME_status, "Whether the camera delivers frames"),
  IV(NAME_message, "code*", IV_BOTH,
     NAME_video, "Executed with the camera for each new frame"),
  IV(NAME_statusMessage, "code*", IV_BOTH,
     NAME_status, "Executed with the camera and status if it changes"),
  IV(NAME_texture, "sdl_texture*", IV_BOTH,
     NAME_video, "If not @nil, receives the frames instead of <-image"),
  IV(NAME_writer, "video_writer*", IV_BOTH,
     NAME_video, "If not @nil, the frames are also appended to it"),
  IV(NAME_wsRef, "alien:WsRef", IV_NONE,
     NAME_internal, "SDL camera")
};

/* Send Methods */

static char *T_sdlTestFrame[] =
        { "width=int", "height=int", "colour=colour",
	  "format=[{yuy2,mjpg}]" };

static senddecl send_camera[] =
{ SM(NAME_initialise, 1, "name=[name]", initialiseCamera,
     DEFAULT, "Create for the named or first camera"),
  SM(NAME_unlink, 0, NULL, unlinkCamera,
     DEFAULT, "Close the camera"),
  SM(NAME_open, 1, "size=[size]", openCamera,
     NAME_status, "Start delivering frames of about this size"),
  SM(NAME_close, 0, NULL, closeCamera,
     NAME_status, "Stop delivering frames"),
  SM(NAME_sdlTestFrame, 4, T_sdlTestFrame, sdlTestFrameCamera,
     NAME_test, "Deliver a frame of one colour (testing)")
};

/* Get Methods */

static getdecl get_camera[] =
{ GM(NAME_sizes, 0, "chain", NULL, getSizesCamera,
     NAME_video, "Chain of sizes the camera supports, smallest first")
};

/* Class Declaration */

static Name camera_termnames[] = { NAME_name };

ClassDecl(camera_decls,
          var_camera, send_camera, get_camera, NULL,
          1, camera_termnames);

status
makeClassCamera(Class class)
{ declareClass(class, &camera_decls);

  succeed;
}
