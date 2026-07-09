# class gradient {#class-gradient}

A `gradient` describes a linear or radial colour transition and can be
used as the `fill_pattern` of any graphical that accepts a `colour`
fill.  The renderer draws the shape's outline as usual and fills it by
sampling the gradient across the shape's area.

Gradients live in the coordinate system of the graphical they fill —
the same one the shape's own drawing commands use.  Passing a
gradient to a scrolled or transformed device therefore just works;
the pattern scrolls and scales with the shape.

Two kinds are supported:

- `linear` — the colour interpolates along the axis from `<-p0` to
  `<-p1`.  Points at `<-p0` receive the stop at fraction 0.0; points
  at `<-p1` the stop at 1.0; intermediate points a weighted mix of the
  bracketing stops.  Perpendicular to the axis the colour is constant.

- `radial` — the colour interpolates from a start circle
  (`<-p0`, `<-r0`) to an end circle (`<-p1`, `<-r1`).  The two
  circles may be concentric (the common case) or offset (creates an
  eccentric radial highlight).

A `gradient` needs at least one stop to render anything visible; two
stops give a two-colour transition; more stops give piecewise
interpolation.

Example: a top-to-bottom yellow-to-red fill on a 100×60 box.

	new(P, picture), send(P, open),
	new(Chain, chain),
	send(Chain, append, new(_, tuple(0.0, colour(yellow)))),
	send(Chain, append, new(_, tuple(1.0, colour(red)))),
	new(G, gradient(linear, point(0,0), point(0,60),
	                @default, @default, Chain)),
	send(P, display, new(B, box(100, 60)), point(20, 20)),
	send(B, pen, 0),
	send(B, fill, G).

Because a gradient is a data object rather than a graphical, it may
be attached to as many shapes as desired.  Two shapes sharing a
gradient will each sample the pattern in their own coordinate space.

@see graphical<->fill_pattern
@see colour
@see box->fill


## Instance variables {#class-gradient-instvars}

- gradient<-kind: {linear,radial}
    Whether the gradient interpolates along a straight axis (`linear`)
    or between two circles (`radial`).  Fixed at construction — build
    a fresh gradient to change kind.

- gradient<-p0: point
    Start point of the axis (linear) or centre of the start-circle
    (radial).

- gradient<-p1: point
    End point of the axis (linear) or centre of the end-circle
    (radial).  For an axis-aligned linear gradient set `<-p0` and
    `<-p1` to the two corners of the shape; the perpendicular
    component is unused.

- gradient<-r0: num*
    Radius of the start-circle for a radial gradient.  `@nil` (the
    default for a linear gradient) is treated as zero at draw time.

- gradient<-r1: num*
    Radius of the end-circle for a radial gradient.  `@nil` (the
    default for a linear gradient) is treated as zero at draw time.

- gradient<-stops: chain
    Chain of `tuple(fraction, colour)` — the colour stops.  Fractions
    are numbers in 0..1 (values outside are silently clamped by the
    renderer); the chain need not be sorted but the visual result
    depends on stop order for repeated fractions.  Empty chains
    render as fully transparent.


## Send methods {#class-gradient-send}

- gradient->initialise: kind={linear,radial}, p0=point, p1=point, r0=[num]*, r1=[num]*, stops=[chain]
    Create the gradient.  `r0` and `r1` are only meaningful when
    `kind` is `radial`; pass `@default` for a linear gradient and
    they are stored as `@nil`.  If `stops` is `@default` an empty
    chain is created and stops must be added later with `->add_stop`
    or `->stops`.

    See the class introduction for a runnable example.

- gradient->add_stop: fraction=num, colour=colour
    Append `tuple(fraction, colour)` to `<-stops`.  Convenience
    wrapper equivalent to

	send(G?stops, append, new(_, tuple(F, C)))

    for the common incremental-build pattern.

- gradient->stops: chain
    Replace the colour-stops chain wholesale.  The elements should be
    `tuple(fraction, colour)` objects; the renderer skips anything
    else silently.
