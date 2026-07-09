# class circle {#class-circle}

A circle just an ellipse with equal <-width and <-height.  It's
border-line may have a <-texture and <-pen. The circle's
interior may be filled with a colour using ->fill.

@see class ellipse


## Instance variables {#class-circle-instvars}

- circle<-fill: colour|{foreground,background}*
    When a colour is defined, the interior of the circle is filled with
    this colour.  The circle is transparent if <-fill equals @nil.

    **Defaults**: @nil (transparent).


## Send methods {#class-circle-send}

- circle->diameter: int
    *Inherits description from*: circle<-diameter

- circle->geometry: x=[int], y=[int], width=[int], height=[int]
    Handle general resizing of the circle.  If either <-width or <-height is
    specified this value is used for both parameters.  If both are specified
    the minimum of the two is used for both.

- circle->inside: x=int, y=int
    Succeed if the point (X, Y) — in the circle's <-device coordinate
    system — lies inside the circle: (X-cx)² + (Y-cy)² ≤ r².  Bbox
    corners just miss.

    Same test drives `->in_event_area`; that method additionally
    inflates the radius by `event_tolerance` pixels so hits within
    tolerance of the outline still count.

    @see ellipse->inside

- circle->radius: int
    *Inherits description from*: circle<-radius

- circle->rotate: int
    The only PCE class with fully implemented rotation!


## Get methods {#class-circle-get}

- circle<-diameter: -> int
    Sets <-width and <-height to this value.  See also ->radius.

- circle<-radius: -> int
    Half of the <-width.  ->radius sets <-width and <-height to twice the
    argument.

