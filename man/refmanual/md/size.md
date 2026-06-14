# class size {#class-size}

A size object represents the <-width and <-height of a (rectangular)
area.  Sizes are often used to communicate about sizes with graphical
objects.  Most graphical objects interpret sizes in pixel units.  Some
textual graphical object interpret them in character units.

Both the <-width and the <-height of a size may be negative.  See class
area for details on using negative width/height.

@see class point
@see class area


## Send methods {#class-size-send}

- size->equal: size
    Succeeds if <-width and <-height of both size objects are equal.

- size->offset: width=int, height=int
    Add width to <-width and height to <-height.

