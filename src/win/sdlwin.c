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

/* An sdl_window shows sdl_layer objects, drawn by the renderer of the
 * frame rather than by Cairo, below its graphicals.  This makes it
 * suitable for video with, e.g., controls or annotations on top.  The
 * layers are in window coordinates; the window has no scrollbars.
 * Display it on a picture if scrolling is needed.
 */

static status
initialiseSdlWindow(SdlWindow sw, Name label, Size size, DisplayObj display)
{ initialiseWindow((PceWindow) sw, label, size, display);
  assign(sw, layers, newObject(ClassChain, EAV));

  succeed;
}


static status
unlinkSdlWindow(SdlWindow sw)
{ Cell cell;

  for_cell(cell, sw->layers)
  { SdlLayer l = cell->value;

    assign(l, device, NIL);
  }
  clearChain(sw->layers);

  return unlinkWindow((PceWindow) sw);
}


static status
appendSdlWindow(SdlWindow sw, SdlLayer l)
{ return attachSdlLayer(sw, sw->layers, l);
}


static status
deleteSdlWindow(SdlWindow sw, SdlLayer l)
{ return detachSdlLayer(sw, sw->layers, l);
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Instance Variables */

static vardecl var_sdl_window[] =
{ IV(NAME_layers, "chain", IV_GET,
     NAME_organisation, "sdl_layer objects, bottom first")
};

/* Send Methods */

static char *T_initialise[] =
        { "label=[name]", "size=[size]", "display=[display]" };

static senddecl send_sdl_window[] =
{ SM(NAME_initialise, 3, T_initialise, initialiseSdlWindow,
     DEFAULT, "Create from label, size and display"),
  SM(NAME_unlink, 0, NULL, unlinkSdlWindow,
     DEFAULT, "Detach the layers"),
  SM(NAME_append, 1, "sdl_layer", appendSdlWindow,
     NAME_organisation, "Show a layer above the others"),
  SM(NAME_delete, 1, "sdl_layer", deleteSdlWindow,
     NAME_organisation, "Stop showing a layer")
};

/* Get Methods */

#define get_sdl_window NULL

/* Class Declaration */

static Name sdl_window_termnames[] = { NAME_name };

ClassDecl(sdl_window_decls,
          var_sdl_window, send_sdl_window, get_sdl_window, NULL,
          1, sdl_window_termnames);


status
makeClassSdlWindow(Class class)
{ return declareClass(class, &sdl_window_decls);
}
