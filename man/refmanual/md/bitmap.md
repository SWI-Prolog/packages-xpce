# class bitmap {#class-bitmap}

A bitmap is a graphical that displays an image.  Most of the behaviour
to manipulate the pixels of a bitmaps thus can be found with class
image.

Bitmaps (images) come in two flavours.  If `image <-kind` =
bitmap, the elements of the bitmap are regarded as booleans.
Boolean true (@on) is displayed in the <-colour of the bitmap
and Boolean false (@off) is displayed in the current background
or not painted if <-transparent equals @on.  If `image <-kind`
is pixmap each individual pixel represents a colour.

**Bugs**:

Stencils are not implemented.

A bitmap always paints all the pixels inside it's area.  Support for
partly transparent bitmaps should be provided.

There are no means for scaling bitmaps.

@see class image
@see bitmap-status


## Instance variables {#class-bitmap-instvars}

- bitmap<-image: image
    The image that contains the actual pixels.


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

- bitmap->initialise: image=[image]
    Create a bitmap for the given image object.  The bitmap will be given
    the same <-width and <-height as the image.

    Note that images are automatically converted from names (see
    `image->initialise`).  Thus, the following displays XPCE's icon
    on a picture window:

    	?- send(new(P, picture), open),
    	   send(P, display,
    		    bitmap('16x16/pce.xpm'), point(25, 25)).

    By default, image objects are cached (i.e.  remain in memory).
    If you have many or large ones, please study `image->initialise`
    and `image->load`.

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

    @see bitmap->save

## Get methods {#class-bitmap-get}

- bitmap<-contains: -> chain
    Return a new chain object holding <-image.

- bitmap<-convert: name -> bitmap
    `Bitmap <-convert` converts text of the form

    - @Reference
    	Succeeds if @Reference is an existing named reference to
    	an image object.

    - File basename
    	Succeeds if `image <-convert` succeeds and returns a new
    	bitmap from this image.

    @see bitmap<-clip

