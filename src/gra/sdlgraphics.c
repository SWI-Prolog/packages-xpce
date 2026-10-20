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
  assign(l, blend,     NAME_blend);
  assign(l, tint,      NIL);
  assign(l, scale_mode, NAME_linear);
  assign(l, offset,    NIL);
  assign(l, slices,    NIL);
  assign(l, mesh,      NIL);
  assign(l, displayed, ON);
  assign(l, window,    NIL);

  succeed;
}


static status
unlinkSdlLayer(SdlLayer l)
{ if ( notNil(l->window) )
    send(l->window, NAME_delete, l, EAV);
  if ( notNil(l->mesh) )
    deleteChain(l->mesh->layers, l);

  succeed;
}


static status
meshSdlLayer(SdlLayer l, SdlMesh mesh)
{ if ( l->mesh != mesh )
  { if ( notNil(l->mesh) )
      deleteChain(l->mesh->layers, l);
    assign(l, mesh, mesh);
    if ( notNil(mesh) )
      appendChain(mesh->layers, l);
  }

  return presentSdlLayer(l);
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
LAYER_SLOT(blend,     blendSdlLayer)
LAYER_SLOT(tint,      tintSdlLayer)
LAYER_SLOT(scale_mode, scaleModeSdlLayer)
LAYER_SLOT(offset,    offsetSdlLayer)
LAYER_SLOT(slices,    slicesSdlLayer)
LAYER_SLOT(displayed, displayedSdlLayer)


/* Instance Variables */

static vardecl var_sdl_layer[] =
{ SV(NAME_texture, "sdl_texture*", IV_GET|IV_STORE, textureSdlLayer,
     NAME_appearance, "What is shown"),
  SV(NAME_source, "area*", IV_GET|IV_STORE, sourceSdlLayer,
     NAME_area, "Part of the texture shown; @nil: all of it"),
  SV(NAME_area, "area*", IV_GET|IV_STORE, areaSdlLayer,
     NAME_area, "Where in the window; @nil: the whole window"),
  SV(NAME_scale, "{stretch,fit,fill,none,tile}", IV_GET|IV_STORE, scaleSdlLayer,
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
  SV(NAME_blend, "{blend,add,mod,mul,none}", IV_GET|IV_STORE, blendSdlLayer,
     NAME_appearance, "How the texture is combined with what is below"),
  SV(NAME_tint, "colour*", IV_GET|IV_STORE, tintSdlLayer,
     NAME_appearance, "Multiply the colours of the texture by this"),
  SV(NAME_scaleMode, "{linear,nearest,pixelart}", IV_GET|IV_STORE,
     scaleModeSdlLayer,
     NAME_appearance, "How pixels are sampled when scaling"),
  SV(NAME_offset, "point*", IV_GET|IV_STORE, offsetSdlLayer,
     NAME_area, "Shift of the tiles for scale `tile`"),
  SV(NAME_slices, "area*", IV_GET|IV_STORE, slicesSdlLayer,
     NAME_area, "Centre part of the texture for 9-slice scaling"),
  SV(NAME_mesh, "sdl_mesh*", IV_GET|IV_STORE, meshSdlLayer,
     NAME_appearance, "Draw these triangles instead of placing <-texture"),
  SV(NAME_displayed, "bool", IV_GET|IV_STORE, displayedSdlLayer,
     NAME_visibility, "Whether the layer is shown"),
  IV(NAME_window, "sdl_window*", IV_GET,
     NAME_organisation, "Window showing the layer")
};

/* Send Methods */

static char *T_initialiseSdlLayer[] =
        { "texture=[sdl_texture*]", "area=[area*]",
	  "scale=[{stretch,fit,fill,none,tile}]" };

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


		 /*******************************
		 *	      SDL_MESH		*
		 *******************************/

/* An sdl_mesh holds triangles that an sdl_layer draws using
 * SDL_RenderGeometry().  It knows the layers that show it, so changing
 * it shows their windows again.
 */

static status
changedSdlMesh(SdlMesh m)
{ Cell cell;

  for_cell(cell, m->layers)
    presentSdlLayer(cell->value);

  succeed;
}


static status
initialiseSdlMesh(SdlMesh m)
{ assign(m, layers, newObject(ClassChain, EAV));
  m->data = NULL;

  succeed;
}


static status
unlinkSdlMesh(SdlMesh m)
{ Cell cell;

  for_cell(cell, m->layers)
  { SdlLayer l = cell->value;

    if ( l->mesh == m )
      assign(l, mesh, NIL);
  }
  ws_destroy_sdl_mesh(m);

  succeed;
}


static status
clearSdlMesh(SdlMesh m)
{ ws_sdl_mesh_clear(m);

  return changedSdlMesh(m);
}


static status
appendSdlMesh(SdlMesh m, Point pos, Point uv, Colour colour)
{ float u = 0.0f, v = 0.0f;

  if ( notDefault(uv) )
  { u = (float)valNum(uv->x);
    v = (float)valNum(uv->y);
  }
  if ( ws_sdl_mesh_append(m, (float)valNum(pos->x), (float)valNum(pos->y),
			  u, v, colour) < 0 )
    return errorPce(m, NAME_notEnoughMemory);

  return changedSdlMesh(m);
}


static status
triangleSdlMesh(SdlMesh m, Int i0, Int i1, Int i2)
{ int n = ws_sdl_mesh_count(m);
  Int idx[3] = {i0, i1, i2};

  for(int i=0; i<3; i++)
  { if ( valInt(idx[i]) < 0 || valInt(idx[i]) >= n )
      return errorPce(m, NAME_noVertex, idx[i]);
  }
  if ( !ws_sdl_mesh_triangle(m, (int)valInt(i0), (int)valInt(i1),
			     (int)valInt(i2)) )
    return errorPce(m, NAME_notEnoughMemory);

  return changedSdlMesh(m);
}


static Int
getCountSdlMesh(SdlMesh m)
{ answer(toInt(ws_sdl_mesh_count(m)));
}


/* Instance Variables */

static vardecl var_sdl_mesh[] =
{ IV(NAME_layers, "chain", IV_GET,
     NAME_organisation, "sdl_layer objects showing the mesh"),
  IV(NAME_data, "alien:SdlMeshData*", IV_NONE,
     NAME_storage, "Vertices and indices")
};

/* Send Methods */

static char *T_appendSdlMesh[] =
	{ "position=point", "texture=[point]", "colour=[colour]" };
static char *T_triangleSdlMesh[] =
	{ "int", "int", "int" };

static senddecl send_sdl_mesh[] =
{ SM(NAME_initialise, 0, NULL, initialiseSdlMesh,
     DEFAULT, "Create an empty mesh"),
  SM(NAME_unlink, 0, NULL, unlinkSdlMesh,
     DEFAULT, "Remove from the layers and free the triangles"),
  SM(NAME_clear, 0, NULL, clearSdlMesh,
     NAME_edit, "Remove all vertices and triangles"),
  SM(NAME_append, 3, T_appendSdlMesh, appendSdlMesh,
     NAME_edit, "Append a vertex"),
  SM(NAME_triangle, 3, T_triangleSdlMesh, triangleSdlMesh,
     NAME_edit, "Append a triangle from the indices of three vertices"),
  SM(NAME_changed, 0, NULL, changedSdlMesh,
     NAME_appearance, "Show the windows of the layers again")
};

/* Get Methods */

static getdecl get_sdl_mesh[] =
{ GM(NAME_count, 0, "int", NULL, getCountSdlMesh,
     NAME_edit, "Number of vertices")
};

/* Class Declaration */

ClassDecl(sdl_mesh_decls,
          var_sdl_mesh, send_sdl_mesh, get_sdl_mesh, NULL,
          0, NULL);

status
makeClassSdlMesh(Class class)
{ return declareClass(class, &sdl_mesh_decls);
}


/* See interface.h */

int
pceSdlMeshSet(PceObject mesh, const float *vertices, int nvertices,
	      const int *indices, int nindices)
{ if ( !instanceOfObject(mesh, ClassSdlMesh) )
    return PCE_MESH_NOT_A_MESH;
  if ( (nindices > 0 ? nindices : nvertices) % 3 != 0 )
    return PCE_MESH_NOT_TRIANGLES;
  for(int i=0; i<nindices; i++)
  { if ( indices[i] < 0 || indices[i] >= nvertices )
      return PCE_MESH_BAD_INDEX;
  }

  SDL_Vertex *v = pceMalloc(nvertices > 0 ? nvertices*sizeof(*v) : 1);
  for(int i=0; i<nvertices; i++)
  { const float *f = &vertices[i*8];

    v[i].position  = (SDL_FPoint){f[0], f[1]};
    v[i].color     = (SDL_FColor){f[2], f[3], f[4], f[5]};
    v[i].tex_coord = (SDL_FPoint){f[6], f[7]};
  }
  status rc = ws_sdl_mesh_set(mesh, v, nvertices, indices, nindices);
  pceFree(v);
  if ( !rc )
    return PCE_MESH_NO_MEMORY;
  changedSdlMesh(mesh);

  return PCE_MESH_OK;
}
