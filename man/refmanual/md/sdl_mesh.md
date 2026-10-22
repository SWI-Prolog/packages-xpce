# class sdl_mesh {#class-sdl_mesh}

An sdl_mesh holds triangles that the SDL renderer draws without Cairo,
using SDL_RenderGeometry().  An sdl_layer shows it if it is the
sdl_layer<-mesh of the layer.  Each corner (*vertex*) of a triangle
has a position, a colour and a position in the texture of the layer.
The colours are interpolated over the triangle and multiplied by the
texture.  Without a texture, the triangle shows the colours only.

Positions are in the coordinates of the window and mapped through
sdl_layer<-transform, so a whole mesh can be moved, rotated and
scaled.  Positions in the texture are in its pixels.  The layer's
<-alpha, <-tint, <-blend and <-scale_mode apply; its <-area, <-scale,
<-source, <-angle, <-flip, <-offset and <-slices are ignored.

Meshes are good for:

  - Shapes with gradients, e.g., coloured plots.
  - Warping a texture, e.g., a camera image on a deformed grid.
  - Many particles in a single layer: one textured rectangle (two
    triangles) per particle, rebuilt on every frame.  This is much
    cheaper than an sdl_layer per particle.

Small meshes can be built using ->append and ->triangle.  For large
meshes that change on every frame, use the predicate
pce_sdl_mesh_set/3, which replaces all vertices and triangles in one
call:

	pce_sdl_mesh_set(+Mesh, +Vertices, +Indices)

Vertices is a list of `v(X,Y)`, `v(X,Y,Colour)`, `v(X,Y,U,V)` or
`v(X,Y,U,V,Colour)`, where U,V is the position in the texture (0,0 if
omitted).  Colour is `rgba(R,G,B,A)`,
where R, G, B and A are integers 0..255, or an integer `0xRRGGBBAA`,
e.g., `0xFF0000FF` is opaque red.  The default colour is opaque white.  Indices is a list of indices of Vertices, three per triangle,
counting from 0.  If Indices is `[]`, each three consecutive vertices
make a triangle.  For example, a rectangle with a red and a blue side:

	new(M, sdl_mesh),
	pce_sdl_mesh_set(M,
			 [ v(0,0,0,0,rgba(255,0,0,255)),
			   v(100,0,0,0,rgba(0,0,255,255)),
			   v(100,50,0,0,rgba(0,0,255,255)),
			   v(0,50,0,0,rgba(255,0,0,255))
			 ],
			 [0,1,2, 0,2,3]),
	new(L, sdl_layer),
	send(L, mesh, M),
	send(Window, append, L).

The edges of the triangles are not anti-aliased.  `frame <-image`
draws meshes with Cairo; a textured triangle whose corners have
different colours is drawn using their average colour.

@see class sdl_layer
@see class sdl_texture


## Instance variables {#class-sdl_mesh-instvars}

- sdl_mesh<-layers: chain
    The sdl_layer objects that show the mesh.  Changing the mesh shows
    their windows again.


## Send methods {#class-sdl_mesh-send}

- sdl_mesh->initialise
    Create an empty mesh.

- sdl_mesh->append: position=point, texture=[point], colour=[colour]
    Append a vertex at position, in the coordinates of the window.
    texture is its position in the texture, in pixels (default
    point(0,0)), and colour its colour (default opaque white).  The
    index of the new vertex is the old sdl_mesh<-count.

- sdl_mesh->triangle: int, int, int
    Append a triangle between the vertices with these indices.  Raises
    an error if there is no such vertex.  If a mesh has no triangles,
    each three consecutive vertices make a triangle.

- sdl_mesh->clear
    Remove all vertices and triangles.

- sdl_mesh->changed
    Show the windows of the layers again.  This is done by the methods
    that change the mesh.


## Get methods {#class-sdl_mesh-get}

- sdl_mesh<-count: -> int
    Number of vertices.
