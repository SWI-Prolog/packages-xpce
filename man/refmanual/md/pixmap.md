# class pixmap {#class-pixmap}

A pixmap object is a coloured image: a rectangular area of
<-width x <-height `pixels`, each of which may have its own
colour.

Coloured images are actually supported by class image using
`image <-kind: pixmap`.   This class is introduced to provide
type specification for `coloured` images as needed for some
graphical operations as well as automatic conversion of
monochrome `bitmaps` into colour pixmaps.

This class is often used implicitly: `window ->background:
@grey25_image' will automatically convert the @grey25_image
into a pixmap using <-convert.

Explicit usage is normally done to create a coloured version of
some monochrome bitmap:

	?- new(I, pixmap(image('pce.bm'), red, green)).

Will create a version of the library image pce.bm using a red
<-foreground and a green <background.  See also ->initialise.

Pixmaps are in general reusable objects like normal images.
Therefore, the <-convert method ensure reuse of a pixmap
create from some monochrome image.


## Send methods {#class-pixmap-send}

- pixmap->initialise: source=[image|file]*, width=[int], height=[int]
    The initialisation of a pixmap can take three forms:

    - pixmap(@nil, foreground, background, width, height)
    	Creates a read-write pixmap of specified dimensions with
    	specified colours.

    - pixmap(image, [foreground], [background])
    	Creates a pixmap with specified colours of the same
    	dimensions as the image and copies the image to the
    	pixmap, where each _1_ maps to a foreground and each
    	_0_ to a background pixel.

    	After doing so, it will establish a hyper object with
    	names `pixmap` and `image` that relates the original
    	image and the new pixmap.  See also <-lookup and
    	<-source.

    - pixmap(file, [foreground], [background])
    	Load the pixmap from a file.  If the file is an image
    	file, the colour will be used to map the 1's and 0's.


## Get methods {#class-pixmap-get}

- pixmap<-convert: name|image|graphical|file -> pixmap
    Converts the below types to a full-colour image object.

    - name
    	Do general transformation of @some-name, verifying
    	that the result is a pixmap.

    - image
    	Converts an monochrome image into a coloured pixmap.
    	Basically simply calls new(Pm, pixmap(image)), first
    	trying <-lookup to reuse an older transformation and
    	then ->initialise to create a new pixmap object.

    - graphical
    	Create a pixmap object of the required size and then
    	use ->draw_in to paint the graphical in the pixmap.

    - file
    	Create a pixmap object and load the file-data into
    	it.

- pixmap<-lookup: source=image, width=[int], height=[int] -> pixmap
    Checks whether it can find a pixmap object from the image using
    <-hypered: pixmap with the same foreground and background.

    This method ensures reuse of converted monochrome images.
    see also <-source.

- pixmap<-source: -> image|file*
    Determine source for term representation: if the <-file is not
    @nil, this value is returned.  If <-hypered: image returns an
    image object, this is returned.  Otherwise @nil is returned.

    Used for converting a pixmap to its term representation.

