# class line {#class-line}

Description

Straight line segment connecting two points.  A line may have a
->pen (thickness) and a ->texture (dash-pattern).  A line may have
->arrows on both sides, see class joint and class arrow for attaching
arrows to a line.

**Bugs**:

There are no provisions to display a label in a line.  In the
application-area of PCE, this would be a useful extension.

@see class connection


## Instance variables {#class-line-instvars}

- line<-first_arrow: graphical*
    Arrow displayed on the start-point.

    @see line->first_arrow
    @see line->arrows

- line<-second_arrow: graphical*
    Arrow displayed on the end-point.

    @see line->second_arrow
    @see line->arrows


## Send methods {#class-line-send}

- line->copy: line
    Copy the attributes from class graphical and the arrows.  This method is
    used by class connection to instantiate itself from the related link.

- line->end: point
    *Inherits description from*: line<-end

    @see line->points

- line->end_x: int
    *Inherits description from*: line<-end_x

    @see line->points

- line->end_y: int
    *Inherits description from*: line<-end_x

    @see line->points

- line->first_arrow: graphical*
    @see line-first_arrow
    @see class arrow

- line->initialise: start_x=[int], start_y=[int], end_x=[int], end_y=[int], arrows=[{none,first,second,both}]
    Create a line from X1,Y1 to X2,Y2 with specified ->arrows.  A default
    line has ->pen: 1 and ->texture: none.

- line->orientation: {north_west,south_west,north_east,south_east}
    Redefined from graphical: no-op.

- line->normalise
    Succeed without side-effect.  Normalising a line could change the
    direction as a line is defines to go from

    	<-x, <-y towards <-x + <-width, <-y + <-height

    @see line->orientation

- line->points: start_x=[int], start_y=[int], end_x=[int], end_y=[int]
    Set ->start from the first X-Y couple and ->end from the second.  Any
    default value leaves the corresponding line parameter unchanged.  See
    also `graphical ->set`.

    @see line->start_x
    @see line->start_y
    @see line->end_y
    @see line->end_x
    @see line->start
    @see line->end

- line->second_arrow: graphical*
    @see line-second_arrow
    @see class arrow

- line->start: point
    *Inherits description from*: line<-end

    @see line->points

- line->start_x: int
    *Inherits description from*: line<-start_x

    @see line->points

- line->start_y: int
    *Inherits description from*: line<-start_x

    @see line->points


## Get methods {#class-line-get}

- line<-angle: origin=[point] -> degrees=real
    Return the angle of the line with the positive X-axe in degrees
    counter clockwise.

- line<-distance: to=graphical|point|event, segment=[bool] -> int
    If the argument is a point object, this method computes the
    distance to the the infinitely extended line (if segment is
    @default or @off) or the line segment if segment is @on.

    If the argument is an event object, act as above using the
    position of the event in the coordinate space of the <-device.

    If the argument is a graphical objects, it invokes `area <-distance: area`
    using the areas of both graphicals.

    See also ->in_event_area.

- line<-start: -> point

- line<-end: -> point
    Unlike normal graphicals, lines are in general not specified using their
    <-width and <-height but using <-start and <-end.

    @see line<-start

- line<-intersection: with=line -> point
    Return a new point object describing the intersection point of both
    infinitely extended lines.  Fails if the lines are parallel.

- line<-length: -> int
    Distance between the points <-start and <-end.  See also `point <-distance`.
