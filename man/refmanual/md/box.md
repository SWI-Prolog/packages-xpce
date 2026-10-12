# class box {#class-box}

Class `box` represents a simple rectangle.  It is the simples graphical
object of PCE.

Boxes may have rounded corners (see ->radius); their line may have some
->texture (dash-pattern) and their inside may be filled (see ->fill).


## Instance variables {#class-box-instvars}

- box<-fill: colour|gradient|image|{foreground,background}*
    If a colour object is specified, the box is filled using a solid
    colour.  A gradient fills it with a colour transition.  An image is
    repeated to fill the box, starting at the origin of the window or,
    if <-fill_offset is not @nil, at that offset from the box.  If the
    initial value @nil is used the box is transparent.  The reserved
    names foreground and background use the current foreground and
    background colours from the context.

    **Defaults**: @nil (transparent)

- box<-fill_offset: point*
    If not @nil, the image used as <-fill starts at this offset from
    the top-left corner of the box, so that the pattern moves with the
    box.  If @nil, the pattern is aligned to the window.

    **Defaults**: @nil

- box<-radius: int
    The radius of the circle-fragments used to round the corners.  When
    larger than half the shortest side, this value is used instead of
    <->radius.  A value of 0 implies square corners.

    **Defaults**: 0 (not rounded)

- box<->shadow: 0..|shadow*
    Drop shadow painted outside the box.  See class shadow.  An
    integer N is a soft shadow N pixels to the bottom right; 0 is no
    shadow.  The default is @nil (no shadow).


## Send methods {#class-box-send}

- box->initialise: width=[int], height=[int]
    Create a box from W and H.  The dimensions may be zero or negative.

