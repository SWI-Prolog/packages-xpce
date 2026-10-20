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

#ifndef SDLTEXTURE_H
#define SDLTEXTURE_H
#include <SDL3/SDL.h>
#include <cairo/cairo.h>

/* An sdl_texture in the renderer of a frame.  See sdl_frame_texture() */

typedef struct frame_texture
{ SdlTexture	owner;			/* No reference: removed on unlink */
  SDL_Texture  *texture;		/* NULL: not yet created */
  SDL_PixelFormat made_for;		/* format of the surface it shows */
  bool		premultiplied;		/* and whether it is premultiplied */
  bool		straight;		/* texture holds straight alpha */
  bool		stale;			/* owner changed since the upload */
} frame_texture;

/* The triangles of an sdl_mesh.  Positions are in the coordinates of
 * the window, colours have straight alpha and texture coordinates are
 * in pixels of the texture.  No indices means consecutive triples.
 */

typedef struct sdl_mesh_data
{ SDL_Vertex   *vertices;
  int		nvertices;
  int		avertices;		/* allocated */
  int	       *indices;
  int		nindices;
  int		aindices;		/* allocated */
} sdl_mesh_data;

status	ws_init_sdl_texture(SdlTexture t, Any from);
void	ws_destroy_sdl_texture(SdlTexture t);
status	ws_sdl_texture_set_surface(SdlTexture t, SDL_Surface *surf);
SDL_Surface *ws_sdl_texture_swap_surface(SdlTexture t, SDL_Surface *surf);
status	ws_sdl_texture_from_image(SdlTexture t, Image image);
status	ws_sdl_texture_to_image(SdlTexture t, Image image);
status	ws_sdl_texture_draw_in(SdlTexture t, Graphical gr, Point pos);
void	ws_sdl_texture_changed(SdlTexture t);
Image	ws_sdl_texture_image(SdlTexture t);
Size	ws_sdl_texture_size(SdlTexture t);
Name	ws_sdl_texture_format(SdlTexture t);

void	ws_destroy_sdl_mesh(SdlMesh m);
void	ws_sdl_mesh_clear(SdlMesh m);
int	ws_sdl_mesh_append(SdlMesh m, float x, float y, float u, float v,
			   Colour c);
status	ws_sdl_mesh_triangle(SdlMesh m, int i0, int i1, int i2);
status	ws_sdl_mesh_set(SdlMesh m, const SDL_Vertex *v, int nv,
			const int *idx, int ni);
int	ws_sdl_mesh_count(SdlMesh m);

void	sdl_frame_free_textures(FrameObj fr);
void	sdl_draw_layers(FrameObj fr, SdlWindow sw, const SDL_FRect *dstrect,
			float scale, double opacity);
void	sdl_cairo_layers(cairo_t *cr, SdlWindow sw, float wx, float wy,
			 float scale, double opacity);

#endif /*SDLTEXTURE_H*/
