# class joint {#class-joint}

Class joint is a super-class for all `line-like` graphical
classes that may have arrows at either or both ends of the line.
Currently it has the following subclasses:

- class line
	Straight line-segment

- class arc
	Part of an ellipse (or circle).

- class path
	Line through a number of control-points.  May be smooth
	(interpolated) or a series of straight line-segments.

- class bezier_curve
	Smooth curve leaving <-start in the direction of
	a control-point to <-end arriving from the direction
	of a contol-point.

Class joint defines all methods that deal with attaching and
deleting arrows at the start or end of the line.  Despite the
name, the ->arrows related to a joint need not be instances
of class arrow.  Instead, they may be instances of any subclass
of class graphical, provided the class inplements the ->points
method compliant to `arrow->points`.  This allows the definition
of new `arrow` like objects and attaching them to the
tips of line-like objects

See ->arrows, ->first_arrow and ->second_arrow.  See also class
arrow.

@see class arrow


## Instance variables {#class-joint-instvars}

- joint<-first_arrow: graphical*
    Arrow object at the start-point of the joint.  If @nil, no arrow is
    displayed.  Default (simple) arrows are normally attached using
    ->arrows.

    Any graphical implementing ->points compliant to `arrow->points`
    can be used as an arrow head.  If the joint is changed, it's
    ->compute method will send ->points to the arrow object and
    recompute the bounding box of the joint.  Similar, ->redraw will
    draw invoke the ->redraw method of the arrow(s).

- joint<-second_arrow: graphical*
    Arrow object at the end-point of the joint.  If @nil, no arrow is
    displayed.


## Send methods {#class-joint-send}

- joint->arrows: arrows={none,first,second,both}
    *Inherits description from*: joint<-arrows

- joint->initialise: x=[int], y=[int], width=[int], height=[int], arrows=[{none,first,second,both}]
    For an application programmer it is not useful to make instances of
    class joint directly.  See `line ->initialise`, `arc ->initialise` and
    `path ->initialise`.


## Get methods {#class-joint-get}

- joint<-arrows: -> arrows={none,first,second,both}
    The <->arrows method provides a simple interface to ->first_arrow and
    ->second_arrow.  If it attaches arrows the new arrow is created
    using <-default_arrow.

- joint<-default_arrow: -> graphical
    Create a default arrow (new(arrow)), see `arrow->initialise`.
    This method is called by ->arrows.  Intended to be redefined for
    making lines with different arrow styles.

