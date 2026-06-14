# class image {#class-image}

An `image` is a two-dimensional array of pixels.  Images are used as:

- the pixel store behind a `bitmap` graphical;
- icons and decorations on menus, dialog items, the cursor, ...;
- scratch surfaces for off-screen drawing.

Pixels are 32-bit RGBA values.  An image can also have `<-kind`
`bitmap`, in which case it is interpreted as a black/white mask:
white pixels paint with the current foreground, black pixels with
the background.  `pixmap` (full RGBA) is the default.

The most common case is the read-only image loaded from a file —
typically used for icons.  Such images are pooled in `@images` and
shared across callers; see `->initialise` and `<-lookup`.  Read-write
images backing a single `bitmap` are constructed with explicit
dimensions; see `<->access`.

The image loader is **SDL_image**: PNG, JPEG, BMP, GIF, SVG and the
other formats that SDL_image supports are all accepted.  SVG is
sized using the image's `<-size`.  Saving currently only supports
PNG output.

@see class bitmap
@see image->initialise


## Class variables {#class-image-classvars}

- image.path: string = ".:bitmaps:~/.local/share/swi-prolog/bitmaps:$PCEHOME/bitmaps"
    Search path used by `->load` (and indirectly by `<-convert`) to
    locate image files.  See also `pce_image_directory/1`.


## Instance variables {#class-image-instvars}

- image<-name: name*
    Lookup key for the image.  Used to populate `@images`; @nil for
    anonymous (read-write) images.

    @see image->initialise

- image<-kind: {bitmap,pixmap}
    Interpretation of the pixel values:

    - `pixmap` (default) — each pixel is a 32-bit RGBA colour.
    - `bitmap` — each pixel is either black or white.  When the
      image is painted, white pixels are mapped to the current
      foreground and black pixels to the background.  Used for
      monochrome icons.

- image<-access: {read,both}
    `read` for reusable images (the file-loaded case): the pixels
    are immutable, the image can be shared by many callers.
    `both` for read-write scratch images.  A `both` image may be
    associated with at most one `bitmap` graphical; edits propagate
    to that bitmap.

    @see image<-bitmap

- image<-file: source_sink*
    File (or other `source_sink`) from which the image was loaded.
    @nil for anonymous images.

- image<-bitmap: bitmap*
    Bitmap graphical backed by this image.  Only meaningful when
    `<-access` is `both`; at most one bitmap per image.

    @see bitmap->image
    @see image<-access

- image<-size: size
    Pixel dimensions of the image.  When loading from a file the
    size is derived from the file's contents; the default for a
    fresh anonymous image is 16x16 pixels.  For SVG loads the size
    field can be set before `->load` to drive the rendered size
    (see `->load`).

- image<->hot_spot: point*
    Hot-spot position used when this image is rendered as a cursor
    (typically the click point of a mouse cursor).  May be set
    manually or loaded from an XPM file's hot-spot record.


## Send methods {#class-image-send}

- image->initialise: name=[source_sink]*, width=[int], height=[int], kind=[{bitmap,pixmap}]
    Two idiomatic ways to construct an image:

    - **Reusable file-backed image**: `new(I, image(File))`.  The
      constructor first looks `File` up in `@images`; on a hit it
      returns the existing instance.  Otherwise it builds a new
      image and `->load`s its pixels from the file.  Such images
      get `<-access` `read`.

    - **Anonymous read-write image**:
      `new(I, image(@nil, Width, Height))`.  These images are not
      pooled and have `<-access` `both`.

    The `kind` argument selects between `pixmap` (default) and
    `bitmap`.

    @see image->load
    @see image<-lookup

- image->unlink
    Release the pixel store and the cairo surface backing the
    image.  Called automatically on destruction.

- image->load: from=[source_sink], path=[char_array]
    Load pixels from a file or other `source_sink`.  When the
    source is a `file`, it is first located via `file ->find` on
    `path` (defaults to `image.path`).

    The decoder is **SDL_image**, so PNG, JPEG, BMP, GIF, PCX and
    the other formats supported by SDL_image all load.  SVG is
    recognised by file extension or by sniffing the stream and is
    rendered through SDL's size-aware SVG loader:

    - both `<-width` and `<-height` non-zero → SVG is rendered at
      exactly that size;
    - both zero → SVG viewport size is used;
    - one of the two zero → the other dimension scales to preserve
      the SVG's aspect ratio.

    Streams that are not SVG fall back to a built-in PNM reader
    when SDL_image cannot interpret them.

    @see pce_image_directory/1

- image->save: in=[source_sink], format=[name]
    Write the image to a file.  Only `png` is currently supported;
    omitting `format` saves as PNG.  Other format values are
    rejected with a console message.

- image->resize: width=int, height=int
    Resize the image to the indicated dimensions, preserving the
    overlapping pixels.  Newly-exposed area is cleared to the
    background.  See also `<-scale`.

- image->copy: from=image
    Replace the receiver's pixels with an exact copy of the
    argument.  Use `->draw_in` to *blit* an image into a sub-area
    instead.

- image->draw_in: graphical, at=[point]
    Render the given `graphical` (or `image`) onto the receiver at
    the given offset (default `point(0,0)`).

- image->clear
    Clear the entire image to the background (transparent for
    `pixmap`, 0 for `bitmap`).

- image->fill: colour, [area]
    Fill the indicated area (default whole image) with the given
    colour.

- image->pixel: x=int, y=int, value=colour|bool
    Set the pixel at `(x, y)` to the given colour (for `pixmap`)
    or boolean (for `bitmap`).

- image->has_alpha
    Succeed when the image is not fully opaque (at least one pixel
    has `alpha < 255`).


## Get methods {#class-image-get}

- image<-contained_in: -> bitmap
    Equivalent to `<-bitmap` when that is non-@nil.

- image<-pixel: x=int, y=int -> value=bool|colour
    Read the pixel at `(x, y)`.  Returns a colour for `pixmap`
    images and a boolean for `bitmap` images.

- image<-clip: [area] -> image
    New read/write image with the pixels of the indicated sub-area
    (default the whole image).

- image<-scale: size -> image
    New image scaled to the given dimensions.  Scaling is done
    once at copy time, so passing a target *size* avoids the
    rounding problems of a multiplicative factor.  To magnify by a
    factor:

    	magnify(Im, Factor, Magnified) :-
    		get(Im, size, size(W, H)),
    		NW is round(W * Factor),
    		NH is round(H * Factor),
    		get(Im, scale, size(NW, NH), Magnified).

    See also `->resize` and `<-rotate`.

- image<-rotate: degrees=num -> image
    New image rotated anti-clockwise by the indicated number of
    degrees.  The result is again a horizontal/vertical rectangle,
    so for non-multiples of 90 degrees it is larger than the
    original; the new area is cleared to the background (for
    `pixmap`) or to white (for `bitmap`).  The `<-hot_spot` is
    updated automatically.

    For rotated text and other rotated graphicals, prefer a
    `figure` with a `transform` instead of pre-rendering into an
    image: the figure rotates its children on the fly via cairo,
    so the text stays glyph-accurate at every angle and the
    receiver remains interactive (hit-tested, repainted on demand,
    ...).  Example:

    	draw_vertical(Dev, X, Y, String, Font) :-
    		new(F, figure),
    		send(F, display, text(String, left, Font)),
    		send(F, rotate, 90),
    		send(Dev, display, F, point(X, Y)).

- image<-grayscale: -> image
    New image with the pixels converted to grayscale.

- image<-lookup: name|resource -> image
    Return an existing image from `@images` matching the argument,
    or fail.  Mirrors the lookup performed by `->initialise`.

- image<-convert: bitmap|name|resource|graphical -> image
    Type-checker hook accepting:

    - a `bitmap` object — yields its `<-image`;
    - a name registered in `@images` — yields the pooled image;
    - a file name found on `image.path` — loads and pools the
      image;
    - an `rc` resource term — yields the resource's image;
    - a `graphical` — produces a fresh image by rendering the
      graphical into it via `->draw_in`.
