# class shadow {#class-shadow}

A shadow object describes a drop shadow, as the CSS property
`box-shadow` does.  It is the shape of a graphical, moved by
<-x_offset and <-y_offset, grown by <-spread, blurred over <-blur
pixels and painted in <-colour, which normally has an alpha so the
background shows through.  The shadow is painted outside the area of
the graphical, which keeps its size, and not under the graphical
itself, so it also shows correctly for a graphical without fill.

The classes box, ellipse, circle and figure have a <->shadow:

	send(Box, shadow, new(_, shadow))
	send(Box, shadow, new(_, shadow(0, 6, 16, colour(@default, 0, 0, 0, 110))))

For compatibility, ->shadow also accepts an integer N, which is a
soft shadow N pixels to the bottom right, blurred over 2N pixels in
the default colour, and 0, which removes the shadow.

A shadow with a large <-blur is drawn by blurring an image of the
shape and thus takes more time to paint than the shape itself.


## Instance variables {#class-shadow-instvars}

- shadow<-x_offset: int
    Horizontal distance from the shape.  Positive is to the right.

- shadow<-y_offset: int
    Vertical distance from the shape.  Positive is down.

- shadow<-blur: 0..
    Width of the blurred edge.  As in CSS, the edge is blurred with a
    Gaussian blur with a standard deviation of half this value.  0
    gives a sharp edge.

- shadow<-colour: colour
    Colour of the shadow.  Use a colour with an alpha, e.g.,
    `colour(@default, 0, 0, 0, 80)`.

- shadow<-spread: int
    How much larger (or, if negative, smaller) the shadow is than the
    shape.


## Send methods {#class-shadow-send}

- shadow->initialise: x_offset=[int], y_offset=[int], blur=[0..], colour=[colour], spread=[int]
    Create a shadow.  Arguments that are not given come from the class
    variables of the same name: 0, 3, 8, `colour(@default, 0, 0, 0, 80)`
    and 0, a soft shadow below the shape.


## Get methods {#class-shadow-get}

- shadow<-convert: int -> shadow
    An integer N is a soft shadow N pixels to the bottom right:
    `shadow(N, N, 2*N)`.
