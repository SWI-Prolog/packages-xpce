# class curve_point {#class-curve_point}

A curve_point is a point of a path that is reached by a Bezier curve
from the previous point.  With one control point (<-control2 is @nil)
the curve is quadratic, with two it is cubic.  The control points are
in the same coordinate system as the points of the path and are moved
and scaled with them.

	new(P, path),
	send_list(P, append, [ point(0,50),
	                       curve_point(100,50, point(30,0), point(70,100))
	                     ]).

See also class path, class arc_point and class bezier_curve.

## Instance variables {#class-curve_point-instvars}

- curve_point<-control1: point
    The first (or only) control point.

- curve_point<-control2: point*
    The second control point of a cubic curve or @nil for a quadratic
    curve.

## Send methods {#class-curve_point-send}

- curve_point->initialise: x=int, y=int, control1=point, control2=[point]*
    Create the end point of a quadratic (one control point) or cubic
    curve.
