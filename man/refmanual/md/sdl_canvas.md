# class sdl_canvas {#class-sdl-canvas}

An sdl_canvas holds sdl_layer objects as an sdl_window does, but the
SDL renderer draws them into a texture of the size of the canvas
rather than on the screen.  A layer shows the canvas as it shows an
sdl_texture: placed in an area, scaled, rotated, blended or as the
texture of an sdl_mesh.  Canvases may show other canvases.

This is useful to:

  - Draw many sprites that do not move relative to each other once and
    then move or rotate them as one layer.
  - Show the same layers twice, e.g., at a smaller scale as a minimap.
  - Leave trails or paint: with <-clear @off, drawing again keeps the
    old pixels and draws the layers on top.

The canvas is drawn again only after it changed: after a change to
one of its layers, to a texture or canvas these show, or after
->render.  Each frame that shows the canvas keeps its own texture,
drawn before the frame itself.  If the renderer loses its textures,
e.g., when the GPU is reset, the canvas is drawn again; pixels kept
using <-clear @off are then lost.

	new(C, sdl_canvas(size(200, 200))),
	send(C, append, sdl_layer(ShipTexture, area(100, 100, 0, 0), none)),
	send(C, append, sdl_layer(FlameTexture, area(100, 140, 0, 0), none)),
	send(Window, append, new(L, sdl_layer(C, @nil, none))),
	send(L, transform, Transform).   % moves ship and flame as one

The pixels of the canvas are in pixels of its own size.  Layers placed
with area @nil use the whole canvas.  A canvas that shows itself,
directly or through other canvases, is drawn without that layer and
a warning is printed.

@see class sdl_layer
@see class sdl_window
@see class sdl_texture


## Instance variables {#class-sdl-canvas-instvars}

- sdl_canvas<->size: size
    Size of the canvas in pixels.

- sdl_canvas<-layers: chain
    The sdl_layer objects, the first drawn at the bottom.

- sdl_canvas<->background: colour*
    Colour drawn below the layers.  With @nil (default), the canvas is
    transparent where no layer is drawn.

- sdl_canvas<->clear: bool
    If @on (default), the canvas is cleared to transparent before the
    layers are drawn.  If @off, the pixels of the previous drawing are
    kept and the layers are drawn on top of them, e.g., for trails.
    Use ->render to draw again, and a translucent layer in
    <-background colour to make old pixels fade.


## Send methods {#class-sdl-canvas-send}

- sdl_canvas->initialise: size=size, background=[colour*]
    Create a canvas of the given size.

- sdl_canvas->append: sdl_layer
    Draw the layer above the other layers.  If it is shown by an
    sdl_window or another canvas, it is deleted from there.

- sdl_canvas->delete: sdl_layer
    Stop drawing the layer.  Fails if it is not a layer of this canvas.

- sdl_canvas->render
    Draw the canvas again.  This is needed with <-clear @off to draw
    on top of the previous pixels without changing a layer.

- sdl_canvas->changed
    The same as ->render.


## Get methods {#class-sdl-canvas-get}

- sdl_canvas<-image: -> image
    New image holding the pixels, read back from the renderer.  If no
    frame shows the canvas, it is drawn using the renderer of an open
    frame.  Raises an error if no frame is open.
