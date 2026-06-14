# class path {#class-path}

A path is a line going through a set of points.  There are two `kinds`
of paths:

	| poly   | Straight line segments connect the points  |
	| smooth | A smooth (curved) line connects the points |

The line of a path has a ->texture and a ->pen (thickness).  The first
and last point of the path may be connected (->closed) and the
interior of the path may be filled with an image object using
->fill.  Class path is a subclass of class joint and
therefore may have ->arrows attached.

Paths can be used for numerous purposes:

- Creating polygons
	Paths are commonly subclassed to create triangles or
	other polygons not provided by XPCE.

- Drawing diagrams (chart, graph, curve)
	->kind: smooth may be used for simple smoothing of the
	diagram, while ->mark may be used to mark the
	`control-points`.

- Defining irregular regions.


## Instance variables {#class-path-instvars}

- path<-closed: bool
    If @on, the first and last points are connected.

    **Defaults**: @off

- path<-fill: colour|{foreground,background}*
    Pattern to fill the interior of the path.  To determine the interior,
    the _Even-Odd-Rule_ is used.

    **Defaults**: @nil (not filled)

    @see bitmap-status

- path<-interpolation: chain*
    When kind is `smooth`, this is a chain of interpolated points. The
    interpolation is recomputed each time the path has been modified
    and needs to be painted.  See also ->request_compute.

    @see path-kind

- path<-intervals: 0..100
    The number of interpolation-points between each two successive
    specified points.

    **Defaults**: Resource defined (10).

- path<-kind: {poly,smooth}
    Determines whether the line is smooth or a collection of
    line-segments (poly).   For smooth path objects, the number
    of interpolated points is determined by <->intervals and the
    chain object holding all interpolated points is available in
    <-interpolation.

    **Defaults**: poly (line segments).

    @see path-interpolation

- path<-mark: image*
    When not @nil, each point of the path is marked using this image. Note
    that interpolation-points are not marked.

    **Defaults**: @nil

- path<-offset: point
    Offset to origin.  This offset is added to the location of the
    <-points of the path if the path is painted.  See also
    ->reference and ->relative_move.

- path<-points: chain
    Chain of points the line-segments goes through (control-points).
    The send-method validates and possibly converts all members of
    the argument chain to be instances of class point and raises the
    error `unexpected_type` on failure.  On successs, it replaces
    the <-points chain of the path with the argument.  The argument
    thus becomes a part of the path object.  See also ->initialise.

- path<-radius: int
    Rounding radius for the corners of `poly` lines.  Not yet implemented.

    **Defaults**: 0 (non-rounded corners).


## Send methods {#class-path-send}

- path->append: point
    Append a point at the end of the path. The point is specified in the
    <-device coordinate system. ->insert may be used to insert points in the
    middle or prepend points.

    @see path->insert

- path->compute
    Computes <-area from the <-points and <-interpolation from <-points if
    <-kind = smooth.

- path->delete: point
    Delete a point from the path. To delete points interactively, see also
    <-point.

- path->geometry: x=[int], y=[int], width=[int], height=[int]
    Moves and resizes the path to fit the requested bounding-box. Paths may
    also be resized using ->resize.

- path->initialise: kind=[{poly,smooth}], radius_or_interval=[int], points=[chain]
    The first argument specifies the <-kind. When <-kind equals
    smooth, the second argument specifies the <-intervals.  If the
    `points` arguments is supplied, the method ->points is invoked
    using this argument to specify the initial set of
    control-points.

- path->insert: point, point*
    Insert a new point after the second argument. If the second argument is
    @nil, the point is added as the first point of the chain. For
    interactive editing, <-segment may be used to find the point to insert
    after.

    @see path->append

- path->relative_move: diff=point, how=[{offset,points}]
    Move the graphical relative to its current position using the X- and
    Y-values of point.  How defines how the path is moved: by
    adusting its <-offset (default) or by adjusting each of the
    <-points.

- path->set_point: point=point, x=[int], y=[int]
    Move a member point.  The arguments are the X- and Y-coordinates in the
    <-device's coordinate system.


## Get methods {#class-path-get}

- path<-distance: point|event|graphical -> int
    If the argument is a point object or event object (in which case
    the `event<-position` is used), compute the distance of this
    point to the path-line.  If the path is interpolated, the
    distance to the interpolation is computed.

    If the argument is a graphical, use `graphical<-distance`.

    See also `line<-distance` and ->in_event_area.

- path<-end: -> point
    Position (in <-device coordinate system) of the last point of
    the path.   See also <-start.

    @see path<-start

- path<-point: near=point|event, max_distance=[int] -> point
    Return (member) point object that is closest to the given position and
    within the specified maximum distance (default 10 pixels).  When
    an event is passed, this is translated into the position of the
    event.

    See also <-segment.

    **Defaults**: The default maximum distance (2nd argument) is 10.

    **Diagnostics**: Fails silently if no such point exist.

- path<-segment: near=point|event, accept=[0..] -> point
    Return member point that is the start of a line-segment nearest to the
    indicated position or event <-position. May be used to interactively
    ->insert new points:

    	send(Path, click_gesture(left, '', single,
    			         message(@receiver, insert,
    				         ?(@event, position, @receiver?device),
    				         ?(@receiver, segment, @event)))).

    See also <-point.

- path<-start: -> point
    First point of the path (equivalent to Path?points?head).  See
    also <-end.

    @see path<-end

