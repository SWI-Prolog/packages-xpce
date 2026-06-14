# class hbox {#class-hbox}

Class hbox denotes a `horizontal box` in the document rendering
system, the basics of which are described with class parbox.

An hbox itself has no content and can only be used for layout.
For this purpose they normally have a rubber object attached
to them.

There are to subclasses of the hbox class.  Class tbox
represents text and class grbox represents an embedded
graphical.

Attrinbutes:

- <-rubber
	Defines stretchability as well as possibility to break
	lines here.

- <-ascent
	Height above the `baseline`.

- <-descent
	Depth below the `baseline`

- <-width
	Horizontal width.


## Instance variables {#class-hbox-instvars}

- hbox<-ascent: 0..
    Height above baseline.  For tbox objects this is the `font<-ascent`.
    For grbox objects this is determines by the `grbox<-baseline`
    attribute.

    Hbox objects with non-zero ascent are commonly used as first
    box in a paragraph to create vertical white space.

- hbox<-rubber: rubber*
    Stretch/shrinkability.  See class rubber for details.  The
    default <-rubber for an hbox object is @nil, implying the box'
    size is fixed and it can not break a line.


## Send methods {#class-hbox-send}

- hbox->initialise: width=[int], ascent=[int], descent=[int], rubber=[rubber]*
    Create a hbox object from its attribute values.  Plain hbox
    objects are only used for layout.  See class tbox and class
    grbox for defining the content of parbox objects.

