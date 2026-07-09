# class ellipse {#class-ellipse}

Ellipse shape.  It is possible to define a ->texture for the line and a
->fill for the interior.  See also class circle.

For consistency with boxes, etc., the ->position of an ellipse is the
top-left corner.  Use ->center to define the center.

@see class circle


## Instance variables {#class-ellipse-instvars}

- ellipse<-fill: colour|{foreground,background}*
    When not @nil, the interior is filled with the specified colour.

    **Defaults**: @nil

- ellipse<-shadow: int
    Amount (pixels) of shadow painted below and right of the
    ellipse.  The default is 0 (zero, no shadow).


## Send methods {#class-ellipse-send}

- ellipse->inside: x=int, y=int
    Succeed if the point (X, Y) — in the ellipse's <-device coordinate
    system — lies inside the ellipse inscribed in the bounding box
    `<-area`.  Uses the standard ellipse equation

        ((X - cx) / rx)² + ((Y - cy) / ry)² ≤ 1

    where (cx, cy) is the centre and (rx, ry) the semi-axes.  Bbox
    corners just miss the shape.

    This is the same test used by `->in_event_area` to accept a click:
    that method additionally inflates the ellipse by
    `event_tolerance` pixels on each side so hits within tolerance of
    the outline still count.

