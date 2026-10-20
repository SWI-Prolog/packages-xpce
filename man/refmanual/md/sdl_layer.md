# class sdl_layer {#class-sdl_layer}

An sdl_layer shows an sdl_texture in an sdl_window.  The renderer
scales, rotates and flips the texture while it draws it.  Layers are
drawn below the graphicals of the window, the first layer of
sdl_window<-layers at the bottom.

The texture is first placed in <-area according to <-scale.  It is
then flipped according to <-flip, rotated by <-angle and finally
mapped through <-transform.  With the default <-area and <-scale, the
texture fits the window, as is common for video.

A layer is a *sprite* if its scale is `none` and it has a <-transform:
the hot spot of the texture (see sdl_texture<->hot_spot) is then at the
origin of the transform, which moves and rotates it around this point,
as figure->transform does for a figure.  Draw the figure into a texture
once, after which moving the sprite is cheap:

	new(T, sdl_texture(ShipFigure)),
	send(Window, append, new(Ship, sdl_layer(T, @nil, none))),
	...
	new(Tr, transform),
	send(Tr, set, Cos, -Sin, Sin, Cos, X, Y),
	send(Ship, transform, Tr).

Setting any of the instance variables shows the window again.  An
area object passed to sdl_layer->area or sdl_layer->source is used as
is: changing it later is not noticed, so send the area again after
changing it.

@see class sdl_texture
@see class sdl_mesh
@see class transform
@see class sdl_window


## Instance variables {#class-sdl_layer-instvars}

- sdl_layer<->texture: sdl_texture*
    What is shown.  Nothing is shown if this is @nil.

- sdl_layer<->source: area*
    Part of the texture that is shown, in pixels of the texture.  @nil
    shows the whole texture.

- sdl_layer<->area: area*
    Where the texture is shown, in the coordinates of the window.  @nil
    uses the whole window.  If <-scale is `none`, only the position is
    used.

- sdl_layer<->scale: {stretch,fit,fill,none,tile}
    How the texture is fitted into <-area.  `stretch` makes it exactly
    as large as the area; `fit` (default) makes it as large as possible
    while keeping its aspect ratio and centres it; `fill` covers the
    area while keeping the aspect ratio, cutting off what falls outside;
    `none` shows it at its own size with its hot spot at the
    top-left corner of the area.  The hot spot is
    sdl_texture<-hot_spot or else the top-left of the texture.
    `tile` repeats the texture at its own size to fill the area,
    starting at its top-left corner shifted by <-offset; flipping,
    rotating and <-transform apply to the whole tiled area.  Tiles
    that are not rotated or flipped are drawn in a single call by
    renderers that can repeat a texture, provided <-source is @nil.
    Otherwise each tile is drawn separately, so tiling a large area
    with a small rotated texture is slow.

- sdl_layer<->alpha: 0.0..1.0
    Opacity: 0.0 is invisible, 1.0 (default) is opaque.

- sdl_layer<->angle: num
    Clockwise rotation in degrees around the hot spot of the texture
    or, if it has none, around its centre.

- sdl_layer<->flip: {none,horizontal,vertical,both}
    Mirror the texture.  `horizontal` shows a camera as in a mirror.

- sdl_layer<->transform: transform*
    If not @nil, the placed texture is finally mapped through this
    affine transform, in the coordinates of the window.  This allows
    for any combination of moving, rotating, scaling and shearing.
    Changing the transform object itself is not noticed: send a
    (new) transform after changing it.

- sdl_layer<->blend: {blend,add,mod,mul,none}
    How the texture is combined with what is below it, using the
    blend modes of SDL.  `blend` (default) is normal alpha blending.
    `add` adds the colours, which makes glows, light, fire and
    particles.  `mod` multiplies the colours below with those of the
    texture, ignoring its transparency.  `mul` does the same, but keeps
    what is below where the texture is transparent, e.g., for shadows.
    `none` replaces what is below by the texture, ignoring <-alpha
    and transparency; it is only useful for opaque textures.

- sdl_layer<->tint: colour*
    If not @nil, the red, green and blue of the texture are
    multiplied by those of this colour (SDL's *color mod*).  A white
    texture thus shows in the tint colour, which allows for one
    texture in many colours or to flash a sprite.  The alpha of the
    colour is ignored; use <-alpha.

- sdl_layer<->scale_mode: {linear,nearest,pixelart}
    How the pixels of a scaled or rotated texture are sampled.
    `linear` (default) interpolates, which is smooth for photos and
    video.  `nearest` uses the nearest pixel, keeping sharp pixel
    edges.  `pixelart` (SDL 3.4 or later, otherwise `nearest`) also
    keeps the pixels sharp, but avoids the uneven pixel sizes of
    `nearest` at scales that are not whole numbers.

- sdl_layer<->offset: point*
    Shift of the tiles if <-scale is `tile`, in pixels of the window.
    @nil is the same as point(0,0).  Changing the offset scrolls the
    pattern, e.g., for a scrolling background; layers that share a
    texture can scroll at different speeds for a parallax effect.
    As with <-area, send a new point rather than changing it.

- sdl_layer<->slices: area*
    If not @nil and <-scale is `stretch` or `tile`, the texture is
    scaled as nine parts (*9-slice scaling*), as is common for the
    frames of buttons and panels.  The area is the centre part, in
    pixels of the texture; it is limited to <-source.  The four
    corners keep their size, the top and bottom edges only stretch
    horizontally, the left and right edges only vertically and the
    centre in both directions.  Using `tile`, the edges and centre are
    repeated instead of stretched.  If <-area is too small for the
    corners, they are shrunk.  Other scales ignore the slices.  With
    <-scale_mode `linear`, a stretched part blends with the pixels of
    its neighbours at its border; use `nearest` if this is a problem.

- sdl_layer<->mesh: sdl_mesh*
    If not @nil, the layer draws the triangles of this mesh instead
    of placing <-texture in <-area.  The texture is then mapped onto
    the triangles as their vertices say; without a texture, the
    triangles show the colours of the vertices.  See class sdl_mesh.

- sdl_layer<->displayed: bool
    If @off, the layer is not shown.

- sdl_layer<-device: sdl_window*
    The window showing the layer, as graphical<-device is the device
    showing a graphical.  See sdl_window->append.


## Send methods {#class-sdl_layer-send}

- sdl_layer->initialise: texture=[sdl_texture*], area=[area*], scale=[{stretch,fit,fill,none,tile}]
    Create to show texture in area of the window, scaled as described
    with <-scale (default `fit`).  If area is omitted, the layer uses
    the whole window.

- sdl_layer->expose: above=[sdl_layer]
    Move the layer to the top of sdl_window<-layers, or just above
    `above`.  Fails if the layer is not shown in a window, or if `above`
    is in another window.  This is the same as graphical->expose.

- sdl_layer->hide: below=[sdl_layer]
    Move the layer to the bottom of sdl_window<-layers, or just below
    `below`.  This is the same as graphical->hide.
