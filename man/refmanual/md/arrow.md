# class arrow {#class-arrow}

Class arrow defines an arrow-head.  Normally, arrows are
connected to subclasses of class joint (class line, class
arc, class path and class bezier_curve), but arrows may also be
used as stand-alone graphical objects.  An arrow is determined
by the following parameters:

- ->length
	Distance between tip and baseline of the triangle

- ->wing
	Width of the baseline

Its orientation and position is determined by:

- ->tip
	Where the arrow-head is

- ->reference
	The arrow points to this imaginary position.

See also `joint ->arrows` and `joint <-default_arrow`.

@see class joint
@see line->second_arrow
@see line->first_arrow
@see line->arrows


## Instance variables {#class-arrow-instvars}

- arrow<-fill: colour|{foreground,background}*
    If not equal to @nil, the triangle is filled with this pattern.

    **Defaults**: @nil

    @see bitmap-status

- arrow-right: point
    Right-end of base.

- arrow-left: point
    Computed value to speed up redraw if the parameters of the arrow has not
    changed.

- arrow<-length: num
    Distance in pixels from the <-tip to the baseline of the triangle.  See
    also <-wing.

- arrow<-reference: point
    Imaginary point where the arrow points to.  Only the direction from
    <-tip is considered; not the distance.  <-reference and <-tip may not be
    at the same location.

- arrow<-style: {open,closed}
    If `open`, the baseline of the triangle is not painted.

- arrow<-tip: point
    Location of the arrow head.  Together with <-reference this determines
    the direction into which the arrow points.

- arrow<-wing: num
    Length of the baseline.


## Send methods {#class-arrow-send}

- arrow->compute
    Compute -left, -right and <-area from the <-tip, <-reference, <-length
    and <-wing to speed up redraw.

- arrow->geometry: x=[int], y=[int], width=[int], height=[int]
    Ignores the width and height arguments.  Compares the x and y
    with the current <-area and moves the a arrow by calling
    ->points such the <-area will match the specified x and y.

- arrow->initialise: length=[num], wing=[num], style=[{open,closed}], fill=[colour]*
    Initialise an arrow object.  `length` is the length from the
    arrow-head to the back of the wing.  `wing` is the distance
    between the two wing-tips.  A default arrow reads the following
    values from associated class-variables:

    	| <-length | Tip to wing length |
    	| <-wing   | Wing-tip distance  |
    	| <-fill   | How it is filled   |
    	| <-style  | {open,closed}      |

- arrow->reference_x: num
    *Inherits description from*: arrow<-reference_x

- arrow->reference_y: num
    *Inherits description from*: arrow<-reference_x


## Get methods {#class-arrow-get}

- arrow<-reference_y: -> num

- arrow<-reference_x: -> num
    Directly operate on X- or Y- component of the <->reference.
