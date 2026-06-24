# class box {#class-box}

Class `box` represents a simple rectangle.  It is the simples graphical
object of PCE.

Boxes may have rounded corners (see ->radius); their line may have some
->texture (dash-pattern) and their inside may be filled (see ->fill).


## Instance variables {#class-box-instvars}

- box<-fill: colour|{foreground,background}*
    If a colour object is specified, the box is filled using a solid
    colour.  If the initial value @nil is used the box is transparent.
    The reserved names foreground and background use the current
    foreground and background colours from the context.

    **Defaults**: @nil (transparent)

- box<-radius: int
    The radius of the circle-fragments used to round the corners.  When
    larger than half the shortest side, this value is used instead of
    <->radius.  A value of 0 implies square corners.

    **Defaults**: 0 (not rounded)

- box<-shadow: int
    Amount (pixels) of shadow painted below and right of the
    box.  The default is 0 (zero, no shadow).


## Send methods {#class-box-send}

- box->initialise: width=[int], height=[int]
    Create a box from W and H.  The dimensions may be zero or negative.

- box->shadow: int
    Amount of shadow added to this box in pixels.  In the current
    implementation, shadow is always black and painted below and
    right-of the object.

