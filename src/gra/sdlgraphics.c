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

/* Graphics that bypass Cairo.  An sdl_texture holds pixels that the
 * renderer draws directly, e.g., the frames of a camera in their YUV
 * format.  An sdl_layer places a texture in an sdl_window.  The
 * layers are drawn below the graphicals of the window.  See
 * sdl/sdltexture.c.
 */

		 /*******************************
		 *	    SDL_TEXTURE		*
		 *******************************/

static status
initialiseSdlTexture(SdlTexture t, Any from)
{ assign(t, hot_spot, NIL);
  t->surface = NULL;

  return ws_init_sdl_texture(t, from);
}


static status
unlinkSdlTexture(SdlTexture t)
{ ws_destroy_sdl_texture(t);

  succeed;
}


static status
imageSdlTexture(SdlTexture t, Image image)
{ return ws_sdl_texture_from_image(t, image);
}


static status
drawInSdlTexture(SdlTexture t, Graphical gr, Point pos)
{ return ws_sdl_texture_draw_in(t, gr, pos);
}


static status
changedSdlTexture(SdlTexture t)
{ ws_sdl_texture_changed(t);

  succeed;
}


static Image
getImageSdlTexture(SdlTexture t)
{ answer(ws_sdl_texture_image(t));
}


static Size
getSizeSdlTexture(SdlTexture t)
{ answer(ws_sdl_texture_size(t));
}


static Name
getFormatSdlTexture(SdlTexture t)
{ answer(ws_sdl_texture_format(t));
}


/* Instance Variables */

static status
hotSpotSdlTexture(SdlTexture t, Point hot_spot)
{ assign(t, hot_spot, hot_spot);
  ws_sdl_texture_changed(t);

  succeed;
}


static vardecl var_sdl_texture[] =
{ SV(NAME_hotSpot, "point*", IV_GET|IV_STORE, hotSpotSdlTexture,
     NAME_area, "Point placed at the position of a layer; @nil: top-left"),
  IV(NAME_surface, "alien:SDL_Surface*", IV_NONE,
     NAME_internal, "The pixels")
};

/* Send Methods */

static char *T_drawIn[] =
	{ "graphical", "at=[point]" };

static senddecl send_sdl_texture[] =
{ SM(NAME_initialise, 1, "from=[image|size|graphical]", initialiseSdlTexture,
     DEFAULT, "Create from an image, a graphical or transparent of a size"),
  SM(NAME_unlink, 0, NULL, unlinkSdlTexture,
     DEFAULT, "Free the pixels and the textures made from them"),
  SM(NAME_image, 1, "image", imageSdlTexture,
     NAME_pixels, "Replace the pixels by those of an image"),
  SM(NAME_drawIn, 2, T_drawIn, drawInSdlTexture,
     NAME_pixels, "Paint graphical on the pixels [at point]"),
  SM(NAME_changed, 0, NULL, changedSdlTexture,
     NAME_pixels, "Show the pixels again where they are displayed")
};

/* Get Methods */

static getdecl get_sdl_texture[] =
{ GM(NAME_image, 0, "image", NULL, getImageSdlTexture,
     NAME_pixels, "New image holding a copy of the pixels"),
  GM(NAME_size, 0, "size", NULL, getSizeSdlTexture,
     NAME_dimension, "Size in pixels"),
  GM(NAME_format, 0, "name*", NULL, getFormatSdlTexture,
     NAME_pixels, "Pixel format, e.g., argb8888 or yuy2")
};

/* Class Declaration */

ClassDecl(sdl_texture_decls,
          var_sdl_texture, send_sdl_texture, get_sdl_texture, NULL,
          0, NULL);

status
makeClassSdlTexture(Class class)
{ return declareClass(class, &sdl_texture_decls);
}


		 /*******************************
		 *	     SDL_LAYER		*
		 *******************************/

static status
presentSdlLayer(SdlLayer l)
{ if ( notNil(l->window) )
    ws_present_window((PceWindow)l->window);

  succeed;
}


static status
initialiseSdlLayer(SdlLayer l, SdlTexture texture, Area area, Name scale)
{ assign(l, texture,   isDefault(texture) ? NIL : texture);
  assign(l, source,    NIL);
  assign(l, area,      isDefault(area) ? NIL : area);
  assign(l, scale,     isDefault(scale) ? NAME_fit : scale);
  assign(l, alpha,     toNum(1.0));
  assign(l, angle,     toNum(0.0));
  assign(l, flip,      NAME_none);
  assign(l, transform, NIL);
  assign(l, displayed, ON);
  assign(l, window,    NIL);

  succeed;
}


static status
unlinkSdlLayer(SdlLayer l)
{ if ( notNil(l->window) )
    send(l->window, NAME_delete, l, EAV);

  succeed;
}


/* Move the layer to the top or bottom of the layers of its window, or
 * just above or below l2.  These are the same as graphical->expose
 * and graphical->hide.
 */

static status
restackSdlLayer(SdlLayer l, SdlLayer l2, bool top)
{ SdlWindow sw = l->window;

  if ( isNil(sw) || l == l2 || (notDefault(l2) && l2->window != sw) )
    fail;

  if ( isDefault(l2) )
  { addCodeReference(l);
    deleteChain(sw->layers, l);
    if ( top )
      appendChain(sw->layers, l);
    else
      prependChain(sw->layers, l);
    delCodeReference(l);
  } else if ( top )
  { moveAfterChain(sw->layers, l, l2);
  } else
  { moveBeforeChain(sw->layers, l, l2);
  }

  return presentSdlLayer(l);
}


static status
exposeSdlLayer(SdlLayer l, SdlLayer l2)
{ return restackSdlLayer(l, l2, true);
}


static status
hideSdlLayer(SdlLayer l, SdlLayer l2)
{ return restackSdlLayer(l, l2, false);
}


#define LAYER_SLOT(slot, func) \
static status \
func(SdlLayer l, Any value) \
{ assign(l, slot, value); \
  return presentSdlLayer(l); \
}

LAYER_SLOT(texture,   textureSdlLayer)
LAYER_SLOT(source,    sourceSdlLayer)
LAYER_SLOT(area,      areaSdlLayer)
LAYER_SLOT(scale,     scaleSdlLayer)
LAYER_SLOT(alpha,     alphaSdlLayer)
LAYER_SLOT(angle,     angleSdlLayer)
LAYER_SLOT(flip,      flipSdlLayer)
LAYER_SLOT(transform, transformSdlLayer)
LAYER_SLOT(displayed, displayedSdlLayer)


/* Instance Variables */

static vardecl var_sdl_layer[] =
{ SV(NAME_texture, "sdl_texture*", IV_GET|IV_STORE, textureSdlLayer,
     NAME_appearance, "What is shown"),
  SV(NAME_source, "area*", IV_GET|IV_STORE, sourceSdlLayer,
     NAME_area, "Part of the texture shown; @nil: all of it"),
  SV(NAME_area, "area*", IV_GET|IV_STORE, areaSdlLayer,
     NAME_area, "Where in the window; @nil: the whole window"),
  SV(NAME_scale, "{stretch,fit,fill,none}", IV_GET|IV_STORE, scaleSdlLayer,
     NAME_area, "How the texture is fitted into <-area"),
  SV(NAME_alpha, "0.0..1.0", IV_GET|IV_STORE, alphaSdlLayer,
     NAME_appearance, "Opacity"),
  SV(NAME_angle, "num", IV_GET|IV_STORE, angleSdlLayer,
     NAME_appearance, "Clockwise rotation in degrees around the centre"),
  SV(NAME_flip, "{none,horizontal,vertical,both}", IV_GET|IV_STORE,
     flipSdlLayer,
     NAME_appearance, "Mirror the texture"),
  SV(NAME_transform, "transform*", IV_GET|IV_STORE, transformSdlLayer,
     NAME_area, "Finally map the texture through this transform"),
  SV(NAME_displayed, "bool", IV_GET|IV_STORE, displayedSdlLayer,
     NAME_visibility, "Whether the layer is shown"),
  IV(NAME_window, "sdl_window*", IV_GET,
     NAME_organisation, "Window showing the layer")
};

/* Send Methods */

static char *T_initialiseSdlLayer[] =
        { "texture=[sdl_texture*]", "area=[area*]",
	  "scale=[{stretch,fit,fill,none}]" };

static senddecl send_sdl_layer[] =
{ SM(NAME_initialise, 3, T_initialiseSdlLayer, initialiseSdlLayer,
     DEFAULT, "Create from texture, area and scale"),
  SM(NAME_unlink, 0, NULL, unlinkSdlLayer,
     DEFAULT, "Delete from <-window"),
  SM(NAME_expose, 1, "above=[sdl_layer]", exposeSdlLayer,
     NAME_stacking, "Put on top of all layers or just above argument"),
  SM(NAME_hide, 1, "below=[sdl_layer]", hideSdlLayer,
     NAME_stacking, "Put below all layers or just below argument")
};

/* Get Methods */

#define get_sdl_layer NULL

/* Class Declaration */

static Name sdl_layer_termnames[] = { NAME_texture, NAME_area, NAME_scale };

ClassDecl(sdl_layer_decls,
          var_sdl_layer, send_sdl_layer, get_sdl_layer, NULL,
          3, sdl_layer_termnames);

status
makeClassSdlLayer(Class class)
{ return declareClass(class, &sdl_layer_decls);
}
