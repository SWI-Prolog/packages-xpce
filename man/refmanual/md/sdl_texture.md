# class sdl_texture {#class-sdl_texture}

An sdl_texture holds pixels that the SDL renderer draws itself,
without Cairo.  An sdl_layer places it in an sdl_window or
sdl_canvas, or maps it onto the triangles of an sdl_mesh.  The pixels
keep the format they arrived in: camera->texture stores the frames of
a camera in, e.g., YUY2, and the renderer converts them to RGB when it
draws them, on the GPU if it can.  This makes it suitable for video.

Copying the pixels from and to an image is cheap for pixels in ARGB:
these are stored with premultiplied alpha, as Cairo stores them.

The renderer of each frame that shows the texture keeps its own copy.
After the pixels change, these copies are updated and the frames are
drawn again; the Cairo drawings of the windows are not redrawn.

	new(T, sdl_texture(image('pce.png'))),
	new(W, sdl_window),
	send(W, append, sdl_layer(T)),
	send(W, open).

@see class sdl_layer
@see class sdl_window
@see class sdl_mesh
@see camera<->texture


## Instance variables {#class-sdl_texture-instvars}

- sdl_texture<->hot_spot: point*
    Point of the texture, in its pixels, that is placed at the position
    of an sdl_layer using scale `none` and around which the layer
    rotates.  @nil places the top-left corner and rotates around the
    centre.  Copied from the image or set to the origin of the
    graphical the texture is made from.


## Send methods {#class-sdl_texture-send}

- sdl_texture->initialise: from=[image|size|graphical]
    Create with a copy of the pixels of an image or, given a size, with
    transparent pixels.  Given a graphical, the texture is as large as
    its bounding box, including room for, e.g., a glow, and holds the
    graphical drawn with Cairo.  If the origin of the graphical, the
    point (0,0) of its coordinates, is on the texture, it becomes
    <-hot_spot: a figure drawn around its origin thus becomes a sprite
    that rotates around that point.  Without an argument the texture
    has no pixels until, e.g., a camera delivers a frame.

- sdl_texture->image: image
    Replace the pixels by a copy of those of the image.  The size
    changes to the size of the image.

- sdl_texture->draw_in: graphical, at=[point]
    Paint the graphical onto the pixels using Cairo, at the given
    position or else at its own position.  As image->draw_in.  Pixels
    in another format than ARGB, such as a camera frame, are first
    converted to ARGB.  The texture is shown again where it is
    displayed.

- sdl_texture->changed
    Show the pixels again wherever the texture is displayed.  This is
    done by the methods that change the pixels.


## Get methods {#class-sdl_texture-get}

- sdl_texture<-image: -> image
    New image holding a copy of the pixels, converted to RGB.  Use this
    to save or analyse a frame of a camera.  image->copy copies the
    pixels into an existing image.

- sdl_texture<-size: -> size
    New size object holding the size in pixels.  `size(0,0)` if there
    are no pixels.

- sdl_texture<-format: -> name*
    Name of the pixel format, e.g., `argb8888` or the `yuy2` and `nv12`
    of cameras.  @nil if there are no pixels.
