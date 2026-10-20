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

- sdl_layer<->scale: {stretch,fit,fill,none}
    How the texture is fitted into <-area.  `stretch` makes it exactly
    as large as the area; `fit` (default) makes it as large as possible
    while keeping its aspect ratio and centres it; `fill` covers the
    area while keeping the aspect ratio, cutting off what falls outside
    and `none` shows it at its own size with its hot spot at the
    top-left corner of the area.  The hot spot is
    sdl_texture<-hot_spot or else the top-left of the texture.

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

- sdl_layer<->displayed: bool
    If @off, the layer is not shown.

- sdl_layer<-window: sdl_window*
    The window showing the layer.  See sdl_window->append.


## Send methods {#class-sdl_layer-send}

- sdl_layer->initialise: texture=[sdl_texture*], area=[area*], scale=[{stretch,fit,fill,none}]
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
