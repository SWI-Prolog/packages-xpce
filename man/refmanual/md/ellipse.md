# class ellipse {#class-ellipse}

Ellipse shape.  It is possible to define a ->texture for the line and a
->fill for the interior.  See also class circle.

For consistency with boxes, etc., the ->position of an ellipse is the
top-left corner.  Use ->center to define the center.

@see class circle


## Instance variables {#class-ellipse-instvars}

- ellipse<-fill: colour|{foreground,background}*
    When not @nil, the interior is filled with the specified bitmap.

    **Defaults**: @nil

    @see bitmap-status

- ellipse<-shadow: int
    Amount (pixels) of shadow painted below and right of the
    ellipse.  The default is 0 (zero, no shadow).

