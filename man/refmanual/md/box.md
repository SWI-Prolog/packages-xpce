# class box {#class-box}

Class `box` represents a simple rectangle.  It is the simples graphical
object of PCE.

Boxes may have rounded corners (see ->radius); their line may have some
->texture (dash-pattern) and their inside may be file (see ->fill).


## Instance variables {#class-box-instvars}

- box<-fill_offset: point*
    If a box object is filled with a <-fill, this pattern is
    repeated over the entire area of the box.  By default,
    <-fill_offset is @nil and the origin of the pattern is the
    origin of the window.   By setting <-fill_offset to a
    point, the pattern's origin starts at the specified location
    relative to the X,Y of the box.

    A typical usage for this method is to make neat-looking
    bar of a barchart using a gradient fill-pattern and
    setting <-fill_offset to point(0,0).

- box<-fill: colour|{foreground,background}*
    If an image object is used as <-fill, this pattern
    is repeated over the interior of the box.  If a colour object
    is specified, the box is filled using a solid colour and finally
    if the initial value @nil is used the box is transparent.  The
    reserved names foreground and background use the current
    foreground and background colours from the context.

    See also <-fill_offset.

    **Defaults**: @nil (transparent)

    @see bitmap-status

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

