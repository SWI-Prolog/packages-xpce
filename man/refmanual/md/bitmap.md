# class bitmap {#class-bitmap}

A bitmap is a graphical that displays an image.  Most of the behaviour
to manipulate the pixels of a bitmaps thus can be found with class
image.

Bitmaps (images) come in two flavours.  If `image <-kind` =
bitmap, the elements of the bitmap are regarded as booleans:
boolean true (@on) is painted in the <-colour of the bitmap and
boolean false (@off) in the current background.  If `image <-kind`
is pixmap each individual pixel represents a colour.

Transparency comes from the image rather than from the bitmap: pixels
that the image's alpha channel calls transparent are not painted, and
the bitmap lets whatever is behind it show through.  Use
`image->has_alpha` to test whether an image has such pixels.  Older
versions had a `bitmap<->transparent` to say that the @off pixels of a
bitmap-kind image should not be painted, and took it as the second
argument of `->initialise`; both are gone.

A bitmap can show an animation: each frame of the animation is in turn
its <-image.  `->play` plays the animation and `->frame` shows a frame:

	new(BM, bitmap(animation('spinner.png'))),
	send(BM, play).

**Bugs**:

Stencils are not implemented.

There are no means for scaling bitmaps: `->geometry` moves a bitmap but
ignores the requested size.  Scale the image instead, with
`image<-scale`.

@see class image
@see class animation
@see bitmap-status


## Instance variables {#class-bitmap-instvars}

- bitmap<-image: image
    The image that contains the actual pixels.  If the bitmap shows an
    <-animation, this is the frame shown.

- bitmap<-animation: animation*
    The animation whose frames are shown, or @nil.

- bitmap<-frame: int
    The index of the frame of <-animation shown, where the first frame
    is 1.  0 if there is no animation.

- bitmap<-loops: int
    How many times the animation was played to its end since `->play`.


## Send methods {#class-bitmap-send}

- bitmap->geometry: x=[int], y=[int], width=[int], height=[int]
    Just moves the bitmap.  In the current implementation bitmaps cannot be
    scaled.

- bitmap->image: image
    Associate an image with a bitmap.  Sets the width and height of the
    bitmap to the width and height of the image.  If the image has <->access
    `both` and <->bitmap @nil, the <->bitmap variable of the image is filled
    with the bitmap.  Subsequent changes to the image are forwarded to the
    bitmap.

    @see image-bitmap

- bitmap->initialise: image=[image|animation]
    Create a bitmap for the given image object.  The bitmap will be given
    the same <-width and <-height as the image.  If the argument is an
    animation, the bitmap shows its first frame (see `->animation`).

    Note that images are automatically converted from names (see
    `image->initialise`).  Thus, the following displays XPCE's icon
    on a picture window:

    	?- send(new(P, picture), open),
    	   send(P, display,
    		    bitmap('16x16/pce.xpm'), point(25, 25)).

    By default, image objects are cached (i.e.  remain in memory).
    If you have many or large ones, please study `image->initialise`
    and `image->load`.

- bitmap->animation: animation*
    Show the frames of an animation.  This stops playing the previous
    animation and shows the first frame.  @nil leaves the bitmap
    showing the current frame as an ordinary image.

- bitmap->frame: int
    Show the frame of <-animation at this index, where the first frame
    is 1.  Fails if there is no such frame.  If the animation is
    playing, it continues from this frame.

- bitmap->play
    Play <-animation from the current frame: show each frame for its
    `animation<-delays` and continue with the next.  After the last
    frame it starts again, until it has played `animation<-loop_count`
    times; it then stops at the last frame.  Fails if there is no
    animation or it has only one frame.

- bitmap->stop
    Stop playing.  The current frame stays.

- bitmap->load: file, path=[char_array]
    Load file in the associated image.  XPCE searches for the file
    in the path described by the last argument.  The default path
    is image.path.

    Backward compatibility only.  New code should manipulate the image
    directly.

    **Defaults**:
    The default search path is determined by the resource _Bitmap.path_.
    Its system defined value is

    	.:~/lib/bitmaps:$PCEHOME/bitmaps:/usr/include/X11/bitmaps

    **Diagnostics**: Fails silently on bad format or non-existing file.

    @see image->save

## Get methods {#class-bitmap-get}

- bitmap<-contains: -> chain
    Return a new chain object holding <-image.

- bitmap<-playing: -> bool
    @on while the animation plays.

- bitmap<-convert: name -> bitmap
    `Bitmap <-convert` converts text of the form

    - @Reference
    	Succeeds if @Reference is an existing named reference to
    	an image object.

    - File basename
    	Succeeds if `image <-convert` succeeds and returns a new
    	bitmap from this image.

    @see bitmap<-clip

