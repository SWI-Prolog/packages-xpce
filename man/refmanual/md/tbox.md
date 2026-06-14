# class tbox {#class-tbox}

Represent text in a parbox object.  Normally a tbox object is
used to represent each word displayed in a parbox object..
Spaces are represented by hbox objects that can be stretched
and allow for line-breaks.

Please note that tbox objects are reusable: they can occurr
in multiple parboxes and at multiple locations at the same time.

See class parbox for an overview of XPCE's primitives for
rendering documents.


## Send methods {#class-tbox-send}

- tbox->initialise: text=char_array, style=[style]
    Creates a tbox object from the given text and style.  The style object
    is used to specify the font, colour and underlining of the text.


## Get methods {#class-tbox-get}

- tbox<-space: -> hbox
    Yield hbox object for a space compatible with the tbox object's
    <-style.   This space has <-width equal to `font <-advance` of
    the font used by this tbox and <-rubber @space_rubber.

