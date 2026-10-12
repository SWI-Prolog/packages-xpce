# class arc_point {#class-arc-point}

An arc_point is a point of a path that is reached by an elliptical arc
from the previous point.  The arc is specified as in the SVG `A`
command: the radii and rotation of the ellipse and two flags that
select one of the four arcs through the two points.  If the radii are
too small to connect the points, they are scaled up just enough.

	new(P, path),
	send_list(P, append, [point(0,50), arc_point(100,50, 50)]).

See also class path and class curve_point.

## Instance variables {#class-arc-point-instvars}

- arc_point<-rx: num
    Horizontal radius of the ellipse.

- arc_point<-ry: num
    Vertical radius of the ellipse.

    **Defaults**: <-rx, making the arc circular.

- arc_point<-rotation: num
    Rotation of the x-axis of the ellipse in degrees.

    **Defaults**: 0

- arc_point<-large: bool
    If @on, use the arc that spans more than 180 degrees.

    **Defaults**: @off

- arc_point<-sweep: bool
    If @on, draw in the direction of positive angles, which is
    clockwise on the screen.  If @off, draw anti-clockwise.

    **Defaults**: @on

## Send methods {#class-arc-point-send}

- arc_point->initialise: x=int, y=int, rx=num, ry=[num], rotation=[num], large=[bool], sweep=[bool]
    Create the end point of an arc.  Only the end point and radius are
    required.
