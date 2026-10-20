# class sdl_window {#class-sdl_window}

An sdl_window shows sdl_layer objects below its graphicals.  The
layers are drawn by the SDL renderer of the frame rather than by
Cairo, which makes this window suitable for video: each frame of the
video is uploaded and drawn by the renderer, while the graphicals of
the window, e.g., controls or annotations, are drawn on top and need
not be redrawn.

	new(C, camera),
	send(C, texture, new(T, sdl_texture)),
	new(W, sdl_window),
	send(W, background, black),
	send(W, append, sdl_layer(T)),
	send(W, display, text('Live'), point(10, 10)),
	send(W, open),
	send(C, open).

window<-background is drawn below the layers.  The window has no
scrollbars and its layers do not scroll.  To scroll, display the
sdl_window on a picture.

@see class sdl_layer
@see class sdl_texture
@see frame<-sdl_test_image


## Instance variables {#class-sdl_window-instvars}

- sdl_window<-layers: chain
    The sdl_layer objects shown, bottom first.


## Send methods {#class-sdl_window-send}

- sdl_window->initialise: label=[name], size=[size], display=[display]
    Create as a window.

- sdl_window->append: sdl_layer
    Show the layer above the other layers.  If it is shown by another
    window or sdl_canvas, it is deleted from there.

- sdl_window->delete: sdl_layer
    Stop showing the layer.  Fails if it is not a layer of this window.
