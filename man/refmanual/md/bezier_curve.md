# class bezier_curve {#class-bezier_curve}

Class bezier_curve implements Bezier curves.  A bezier
smooth curve is a curve from <-start to <-end using either one
or two control-points.  They are used to create a smooth
connection from a certain point leaving at a defined direction
to another point arriving in some direction.

XPCE's class bezier_curve is a subclass of class joint,
inheriting the possibility to have arrow objects at either end
of the curve.

To get a feeling for the behaviour of this class, please try
PceDraw from the demo programs (File/Demo Programs).

When ->selected, the system will show the control-points
connected by dotted lines.

A good description of the details on Bezier curves can be found
at http://graphics.cs.ucdavis.edu/CAGDNotes/ See also class path.


## Instance variables {#class-bezier_curve-instvars}

- bezier_curve<-control1: point
    The attributes <-control1 and <-control2 define the Bezier
    curve's `control-points`.  If <-control2 = @nil, the curve has
    only a single control-point and is known as a `quadratic`
    Bezier curve.  If the second control point is specified, it
    is known as a `cubic` Bezier curve.

    The curve departs from <-start in the direction of <-control1
    and ends in <-end from the direction of <-control2 (or
    <-control1 if <-control2 is @nil.  Unlike class path, the
    curve does not go through its control-points.

- bezier_curve<-control2: point*
    *Inherits description from*: bezier_curve-control1


## Send methods {#class-bezier_curve-send}

- bezier_curve->control1: point
    1st Control-point of the Bezier curve.

- bezier_curve->control2: point*
    *Inherits description from*: bezier_curve-control1, bezier_curve->control1

- bezier_curve->geometry: x=[int], y=[int], width=[int], height=[int]
    The `graphical->geometry` method is redefined by class bezier to
    move the bezier curve.

- bezier_curve->initialise: start=point, end=point, control1=point, control2=[point]*
    Create a bezier_curve object starting at `start` going to `end`
    using either one (quadratic) or two (cubic) control-points.

- bezier_curve->selected: bool
    Refines `graphical->selected` to deal with the <-area change
    resulting from displaying the control-points.

    The control-points and dotted connection-lines are visible if
    the `window<-selection_feedback` is set to `handles`.

- bezier_curve->set_point: point=point, x=[int], y=[int]
    Move (member) point to (X, Y).  Depending on the point,
    this modifies <-start, <-control1, <-control2 or <-end
    and ->compute's the curve afterwards,

    It is normally used with the result if <-point to allow
    interactive editing of bezier curves.  See also the
    PceDraw demo application.


## Get methods {#class-bezier_curve-get}

- bezier_curve<-point: near=point|event, max_distance=[int] -> point
    Find closest point.  This method is intended for editing and
    returns the closest of <-start, <-control1, <-control2 or <-end
    that is within `max_distance`.

    This method is designed after `path<-point`.  See also
    ->set_point.

