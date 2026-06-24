# class arc {#class-arc}

An arc object is a graphical object that represents part of an ellipse.
An arc is a subclass of class joint and thus may have arrows at either
end.  Arcs may be used to construct piecharts as well as to display a
bounded connection line.

The basic representation of an arc consists of

	| ->position    | Position of the center          |
	| ->size        | Width and height of the ellipse |
	| ->start_angle | Degrees of starting angle       |
	| ->size_angle  | Degrees of angle (> 0 and < 0)  |
	| ->close       | Describes how the arc is closed |

This basic specification is satisfactory for the specification of
pie-charts.  The method ->points is useful for creating bounded lines
from one point to another.  The method ->connect_angle is useful
to relate two lines with an arc (like in and/or trees).


## Instance variables {#class-arc-instvars}

- arc<-close: {none,pie_slice,chord}
    Determines how the arc is closed and filled if ->fill does not
    equal @nil.

    - none
    	Only the arc is visible

    - chord
    	The arc and a straight line from <-start to <-end are displayed

    - pie_slice
    	The arc and two start lines from <-start and <-end to <-position
    	are displayed.

- arc<-fill: colour|{foreground,background}*
    Colour used to fill the arc.  When @nil, the arc is not filled.  The
    behaviour of this method is dependent of the value of ->close:

    - none or pie_slice
    	Creates a pie_slice.  If ->close is none no line is painted
    	around the straight edges of the slice.

    - chord
    	Fills the area between a straight line connecting <-start and
    	<-end and the arc.

- arc<-position: point
    The ->position describes the center of the ellipse the arc is part of.

- arc<-size: size
    The ->size describes the width and height of the ellipse the arc is part
    of.  Note that (currently) the axes of the ellipse are always horizontal
    and vertical.

- arc<-size_angle: num
    Describes the angle in degrees of the ellipse segment.  Positive values
    are counter clockwise.

- arc<-start_angle: num
    Describes the starting angle of the arc in degrees.  0 degrees denotes 3
    o'clock.  Positive values are anti-clockwise.

    The position of the starting point may be requested with <-start.


## Send methods {#class-arc-send}

- arc->compute
    Compute the <-area from the principal representation.  The <-area is
    defined to be the bounding box of the arc.

- arc->connect_angle: line, line
    Connect two lines with an arc.  The ->position of the arc will be
    located are the `line <-intersection` of the two lines.  The
    <-start_angle will make the arc start at the first line.  The
    <-size_angle will connect it to the second line.

    This method may be used to paint `and/or trees`.

- arc->end_angle: num
    @see arc->start_angle

- arc->geometry: x=[int], y=[int], width=[int], height=[int]
    Only the X and Y parameters are used to move the arc.

- arc->initialise: radius=[int], start=[num], size=[num]
    Create an arc from its radius (setting ->size), ->start_angle and
    ->size_angle.  Both angles are in degrees and measured
    counter-clockwise.

    The default radius is determined by arc.radius. The default
    ->start_angle is 0 and the default ->size_angle is 90.

- arc->points: start_x=int, start_y=int, end_x=int, end_y=int, curvature=int
    Create a bounded connection between two points.  The first 4 integers
    describe x1,y1 and x2, y2.  The last value specifies the number of
    pixels the line is bounded.

- arc->radius: int
    Specifies both ->width and ->height.

- arc->start_angle: num
    @see arc->end_angle


## Get methods {#class-arc-get}

- arc<-end: -> point
    End-point of the arc.

- arc<-radius: -> int
    Equivalent to <-width

    **Bugs**: Not properly defined if <-width is not equal to <-height

- arc<-start: -> point
    Start position of the arc.

