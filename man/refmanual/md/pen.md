# class pen {#class-pen}

A `pen` holds the stroke parameters used by graphicals when drawing
lines and borders: line thickness, dash pattern (texture) and stroke
colour.  Pens are normally manipulated indirectly through the
`graphical->pen`, `graphical->texture` and `graphical->colour`
slots rather than created as explicit objects, but they may be
constructed and passed where a pen-valued argument is expected.

@see class graphical
@see class colour
@see graphical->pen


## Instance variables {#class-pen-instvars}

- pen<->thickness: 0..
    Line thickness in pixels.  0 selects the thinnest device line.

- pen<->texture: texture_name
    Dash pattern.  One of `none`, `dotted`, `dashed`, `dashdot`,
    `dashdotted` or `longdash`.

- pen<->colour: [colour]
    Stroke colour, or @default to use the graphical's foreground.


## Send methods {#class-pen-send}

- pen->initialise: thickness=[0..], texture=[texture_name], colour=[colour|pixmap]
    Create a pen from a thickness, texture and stroke colour.  Omitted
    arguments default to 1, `none` and @default respectively.


## Get methods {#class-pen-get}

- pen<-convert: 0.. -> pen
    Convert an integer to a pen of the indicated thickness with default
    texture and colour.  Used by the type-checker to coerce integer
    pen-valued arguments.
