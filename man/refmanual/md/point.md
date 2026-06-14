# class point {#class-point}

A `point` describes a discrete position in an infinite two-dimensional
plain.  Points are used to describe positions when communicating with
graphical objects.

Note that the coordinate system used by PCE deals both with negative and
positive coordinates.

@see class size
@see class area


## Instance variables {#class-point-instvars}

- point<->x: int
    Coordinate of the point object.  Most graphical objects interpret the
    value in pixel units.

- point<->y: int
    *Inherits description from*: point-x


## Send methods {#class-point-send}

- point->copy: from=point
    Copy x- and y values from the argument.  See also <-copy,
    <-clone and ->set.

- point->equal: to=point
    Succeeds if <-x and <-y of both points are equal.  See also
    `object ->equal` and `object ->same_reference`.

- point->initialise: x=[int], y=[int]
    Create a point object from its <-x and <-y value.  When a value is
    omitted, 0 is used.

- point->offset: dx=int, dy=int
    Add dx to <-x and dy to <-y.  See also ->plus.


## Get methods {#class-point-get}

- point<-convert: event|char_array -> point
    Convert event to its position relative to its receiver using
    `event <-position` or a text of the form _<number>, <number>_.
    The latter is discouraged (backward compatibility).

    @see event<-position

- point<-copy: -> point
    New point with same <-x and <-y.  The copy is created using the
    equivalent of the expression below.

    	?(P?class, instance, ?(P, slot, x), ?(P, slot, y))

    See also ->copy.

- point<-difference: to=point -> point
    New point with <-x and <-y obtained by subtracting the receiver's
    coordinates from the argument (vector subtraction).  See also
    <-distance and ->minus.

- point<-distance: to=point -> int
    Rounded integer reflecting the (pixel) distance between both points.

- point<-print_name: -> string
    Printed representation as <x>,<y>

