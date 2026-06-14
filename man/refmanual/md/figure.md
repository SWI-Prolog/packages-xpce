# class figure {#class-figure}

A figure defines -like a device- a collection of graphicals.  In
addition of class device, a figure offers:

	| ->clip_area      | Clip graphicals to this area |
	| ->pen, ->texture | Draw a box around the figure |
	| ->background     | Fill the background          |
	| ->rotate         | Transform the image          |
	| ->scale          | Transform the image          |
	| ->shear          | Transform the image          |

@see class device
@see class transform
@see class elevation
@see class spatial


## Instance variables {#class-figure-instvars}

- figure<->background: colour|{foreground,background}*
    Colour used as background.  Commonly used to prevent the figure
    from being transparent (->background: background).

- figure<->status: name
    The <-status of a figure is a name indicating which graphicals of the
    figure should be visible: if <-status equals `all_active`, all
    <-graphicals are visible, otherwise only graphicals with the same name
    as <-status are made visible.

    This mechanism may be used to define graphical objects that should be
    switched between different states.   Consider the example where we want
    to have a diagram which can be displayed `opened` and as an `icon`.  The
    code below outlines a possible approach:

	create_closable_diagram(F) :-
		new(F, figure),
		send(F, status, opened),
		send(F, display, new(BM, bitmap('my_icon'))),
		send(BM, name, closed),
		send(BM, recogniser,
			 click_gesture(left, '', double,
				       message(F, status, opened),
		send(F, display, new(F2, figure)),
		send(F2, name, opened),
		...
		<create the real diagram on F2>

- figure<->border: num
    Extra empty pixels added around the children's bounding box.  Used
    together with `<-pen` to draw a visible frame at a distance from
    the contents.

- figure<->radius: num
    Corner radius for the outline drawn by `<-pen`/`<-elevation`.  0
    yields a square outline.

- figure<->elevation: elevation*
    `elevation` object describing a 3-D appearance (raised, sunken,
    shadow, ...) of the figure's box.  @nil disables the effect.  The
    legacy `<->shadow` shortcut creates a shadow elevation.

- figure<->transform: transform*
    Optional 2-D affine `transform` applied to the figure's contents.
    @nil means no transform.  The transform is applied to the children
    while drawing, hit-testing and computing the bounding box, so a
    rotated/scaled figure is still selectable by the mouse and reports
    a tight bounding box in its parent's coordinates.  The convenience
    methods `->translate`, `->scale`, `->rotate` and `->shear` compose
    operations into this transform without the caller having to manage
    the transform object explicitly.

- figure<-local_area: area
    Bounding box of the children in the figure's local (pre-transform,
    pre-offset) coordinate system.  Refreshed by `->compute`.  Damage
    handling and child event dispatch use this to keep paint and hit
    tests correct under rotation/scale.


## Send methods {#class-figure-send}

- figure->initialise
    Create an empty figure.  In addition to `device ->initialise`, it
    assigns <-background to @nil, <-pen to 0 and <-status to all_active.

- figure->compute
    Recompute the figure: lay out the children, refresh `<-local_area`,
    update the bounding box and propagate `<-transform` into the
    parent-coordinate area.  Triggered automatically after slot
    changes; rarely called directly.

- figure->display: graphical, [point]
    Similar to `device ->display`, but if <-status not is all_active and the
    graphical has not the same name, the graphical is added to the figure
    with <-displayed: @off.

- figure->next_status
    Advance `<-status` to the name of the next graphical in
    `<-graphicals` (wrapping at the end).  No-op when `<-status` is
    `all_active`.  Convenient for cycling through alternative
    representations.

- figure->clip_area: area*
    Restrict the children's drawing and event-area to the indicated
    rectangle in the figure's coordinate system.  @nil disables
    clipping (children draw and receive events on their full extent).

- figure->shadow: 0..
    Backward-compatible shortcut that attaches a shadow elevation object of
    the given height.  0 removes the elevation.  Equivalent to
    creating an explicit `elevation(kind := shadow, height := N)` and
    assigning it via `->elevation`.

- figure->translate: dx=num, dy=num
    Post-compose translate(dx, dy) into `<-transform`.  Creates the
    transform on demand (starting from identity).

- figure->scale: sx=num, sy=[num]
    Post-compose scale(sx, sy) into `<-transform`.  When `sy` is
    omitted the same factor is applied to both axes.

- figure->rotate: degrees=num
    Post-compose rotate(degrees) into `<-transform`.  Composition
    follows cairo's convention: the operation is applied to the
    input first, then the existing transform on top.

- figure->shear: kx=num, ky=num
    Post-compose shear(kx, ky) into `<-transform`.

- figure->convert_old_slot: slot=name, value=any
    Backward-compatibility hook used when loading older saved
    figures: translates the obsolete `shadow` slot into an
    `elevation`.


## Get methods {#class-figure-get}

- figure<-clip_area: -> area
    Return the clipping area set with `->clip_area`, or @nil if no
    clipping is active.

- figure<-shadow: -> 0..
    Backward-compatible shortcut returning `<-elevation?height` when
    the elevation kind is `shadow`, 0 otherwise.
