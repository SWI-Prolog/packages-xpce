# class area {#class-area}

An area is a rectangular set of pixels.  areas are used by graphical
objects and windows to represent their position and size.

An area has an origin, described by the <->x and <->y attributes and a
size described by the <->width and <->height attributes.  All parameters
are in pixel-units.  An area with either <->width or <->height equal to
0 (zero) contains no pixels.

The position of the origin with respect to the rectangle is determined
by the sign of the width and height attributes.  See area ->orientation
and area ->normalise.

Class area supports class graphical to represent the area of the
graphical in the coordinate system of its device.  Most of the methods
defined on class area also exist on class graphical.  For area's, these
methods are side-effect free, whereas for graphicals they change the
geometry of the graphical.  See `graphical ->set` and `graphical
->geometry'.

@see class region
@see area->orientation
@see area->normalise
@see class point
@see class size
@see class graphical


## Instance variables {#class-area-instvars}

- area<->height: int
    Height of area in pixels.  Zero height implies the area contains no
    pixels.  With a positive value, the origin is at the top, with a
    negative value it is at the bottom.

    @see area-width

- area<->width: int
    Width of area in pixels.  Zero width implies the area contains no
    pixels.  With a positive value, the origin is at the left, with a
    negative value it is at the right.

    @see area-height

- area<->x: int
    X-coordinate of the area.  The value may both be positive and negative,
    indicating the origin of the area to be at the right, resp. left of the
    origin of the coordinate system.

    @see area-y

- area<->y: int
    Y-coordinate of the area.  The value may both be positive and negative,
    indicating the origin of the area to be at below/above of the
    origin of the coordinate system.

    @see area-x


## Send methods {#class-area-send}

- area->center: point
    Move the <-x and <-y of the area such that the <-center of the area
    matches the argument point.

    @see area->set

- area->clear
    Equivalent to ->set: 0, 0, 0, 0.  Note that an area with <-width or
    <-height equal contain no pixels and ->union with such an area
    is a no-op.

    @see area->union

- area->corner: point
    Manipulate <-width and <-height such that <-corner matches the argument
    point.  Its `graphical ->corner` counterpart is sometimes used to
    implement resizing.

    @see area<-corner

- area->decrease: int
    Makes the area from all sides smaller by the specified amount.  For
    normalised areas (see ->normalise), this implies:

	<-width := <-width - 2*<arg>
	<-x     := <-x + <arg>

    and similar for <-y and <-height.  Deals properly with non-normalised
    areas.

    @see graphical->resize
    @see area->increase

- area->equal: area
    Succeed if the normalised representation of both areas is the same.  See
    ->normalise.

- area->in: point
    @see event->inside

- area->increase: int
    Move all sides outward.  This is the opposite operation of ->decrease.

    @see area->decrease

- area->initialise: x=[int], y=[int], width=[int], height=[int]
    Creates an area with origin at (X,Y) and given width and height.

- area->inside: area
    Succeeds if the argument area is inside this area.  See also ->in,
    `graphical ->in_event_area` and ->overlap.

    @see device<-inside

- area->intersection: area
    Area becomes the intersection with the argument area.  The intersection
    is the largest (rectangular) area that is ->inside both area's.  To get
    the intersection without modifying either of the areas, use
    <-intersection.  Fails without modifying the area if the areas do not
    ->overlap.

    @see area->overlap
    @see area<-intersection

- area->normalise
    Places the origin at the top-left corner.  Equivalent to ->orientation:
    north_west.

    @see area<-normalised
    @see area->orientation
    @see class area

- area->orientation: {north_west,south_west,north_east,south_east}
    An area object is described by its <-x, <-y, <-width and <-height.  The
    point (<-x, <-y) is called the origin or <-position.  Either or both
    <-width and <-height (together representing <-size) may be negative.
    This method manipulates one or both of the pairs <-x/<-width or
    <-y/<-height such that the origin becomes the indicated corner.

    The call ->orientation: north_west is equivalent to ->normalise.

	| ->orientation | ->width | ->height |
	| north_west    | >= 0    | >= 0     |
	| south_west    | >= 0    | < 0      |
	| north_east    | < 0     | >= 0     |
	| south_east    | < 0     | < 0      |

    @see class area

- area->overlap: area
    Succeeds if both areas describe common pixels.  If this message
    succeeds, <-distance returns 0 and ->intersection computes the common
    pixels.

    @see area->intersection
    @see area<-distance

- area->position: point
    Copy <-x and <-y from the corresponding values of the point object.

- area->relative_move: point
    Executes <-x := <-x + `point <-x` and <-y := <-y + `point <-y`.

- area->set: x=[int], y=[int], width=[int], height=[int]
    Sets <-x, <-y, <-width and <-height from the 4 integer arguments.  Any
    argument that is @default will not cause the corresponding parameter to
    be changed.

- area->size: size
    Sets ->width and ->height from the corresponding size parameters.

- area->union: area
    Enlarge the area such that it represents the smallest area containing
    both area objects.  <-union computes the same without changing either of
    the area objects.

    @see area<-union
    @see area->clear


## Get methods {#class-area-get}

- area<-left_side: -> int

- area<-right_side: -> int

- area<-top_side: -> int

- area<-bottom_side: -> int
    Return the pixel position of the specified edge.  Deals properly with
    areas in any <-orientation.

    @see area<-top_side
    @see area<-right_side
    @see area<-left_side

- area<-corner: -> point
    New point at <-x + <-width, <-y + <-height

    @see graphical->corner
    @see area->corner

- area<-distance: area -> int
    Yields the length of the shortest line that can connect a point on the
    outline of the first area with a point on the outline of the second
    area.  Yields 0 if the graphicals overlap.

    @see area<-distance_y
    @see area<-distance_x
    @see area->overlap
    @see graphical<-distance

- area<-distance_x: area -> int
    Yields an integer representing the distance between the areas in
    X-direction.  Yields 0 if both areas overlap in X-direction.

    @see area<-distance
    @see area<-distance_y
    @see graphical<-distance_x

- area<-distance_y: area -> int
    Yields an integer representing the distance between the areas in
    Y-direction.  Yields 0 if both areas overlap in Y-direction.

    @see area<-distance
    @see area<-distance_x

- area<-intersection: area -> area
    New area object that represents the common part of the receiver
    and argument area.  The <-orientation of the new area is the
    same as that of the receiver.

    See also ->intersection and  <-union.

    **Diagnostics**: Fails if the areas do not ->overlap.

    @see area->intersection

- area<-less_sides: area -> int
    Like ->same_sides, but returns those `sides` for which the value in the
    receiver is closer to the origin (i.e. `less`) then for the argument
    area.

    @see area<-same_sides
    @see area<-near_sides

- area<-near_sides: area, int -> int
    Same as <-same_sides, but accepts sides to be the same when they are
    within the specified distance.  With distance 0, exactly the same as
    <-same_sides.

    @see area<-same_sides
    @see area<-less_sides

- area<-normalised: -> area
    Return a new area object that describes the same collection of pixels and
    has <-orientation = north_west.

    @see area->normalise

- area<-orientation: -> {north_west,south_west,north_east,south_east}
    Current orientation.  See ->orientation for details.

    *Inherits description from*: area->orientation

- area<-position: -> point
    New point object from <-x and <-y.

- area<-same_sides: area -> int
    Returns a bitmask for sides that have the same value on both arguments.
    Implemented in the darkness of time for handling automatic layout.  The
    values of the mask are:

	| Receiver | Argument | Value (octal) |
	| top      | top      | 01            |
	| top      | center   | 02            |
	| top      | bottom   | 04            |
	| center   | top      | 010           |
	| bottom   | top      | 020           |
	| bottom   | center   | 040           |
	| bottom   | bottom   | 0100          |
	| left     | left     | 0200          |
	| left     | middle   | 0400          |
	| left     | right    | 01000         |
	| middle   | left     | 02000         |
	| middle   | middle   | 04000         |
	| middle   | right    | 010000        |
	| right    | left     | 020000        |
	| right    | middle   | 040000        |
	| right    | right    | 0100000       |

    The PCE/Prolog library file `area.pl` defines these bitmasks.

- area<-size: -> size
    New size object from the <-width and <-height of the area.  Note that
    either or both may be negative. See ->orientation.

- area<-union: area -> area

