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
 * this image shows the video, and <-message is executed.  The user may
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
  c->ws_ref = NULL;

  succeed;
}


static status
unlinkCamera(Camera c)
{ ws_close_camera(c);

  succeed;
}


static status
openCamera(Camera c)
{ if ( c->status != NAME_closed )
    succeed;

  return ws_open_camera(c);
}


static status
closeCamera(Camera c)
{ ws_close_camera(c);

  succeed;
}


static status
sdlTestFrameCamera(Camera c, Int w, Int h, Colour colour)
{ return ws_test_camera_frame(c, valInt(w), valInt(h), colour);
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
  IV(NAME_wsRef, "alien:WsRef", IV_NONE,
     NAME_internal, "SDL camera")
};

/* Send Methods */

static char *T_sdlTestFrame[] =
        { "width=int", "height=int", "colour=colour" };

static senddecl send_camera[] =
{ SM(NAME_initialise, 1, "name=[name]", initialiseCamera,
     DEFAULT, "Create for the named or first camera"),
  SM(NAME_unlink, 0, NULL, unlinkCamera,
     DEFAULT, "Close the camera"),
  SM(NAME_open, 0, NULL, openCamera,
     NAME_status, "Start delivering frames"),
  SM(NAME_close, 0, NULL, closeCamera,
     NAME_status, "Stop delivering frames"),
  SM(NAME_sdlTestFrame, 3, T_sdlTestFrame, sdlTestFrameCamera,
     NAME_test, "Deliver a frame of one colour (testing)")
};

/* Class Declaration */

static Name camera_termnames[] = { NAME_name };

ClassDecl(camera_decls,
          var_camera, send_camera, NULL, NULL,
          1, camera_termnames);

status
makeClassCamera(Class class)
{ declareClass(class, &camera_decls);

  succeed;
}
