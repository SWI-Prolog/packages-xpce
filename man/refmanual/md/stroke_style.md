# class stroke_style {#class-stroke_style}

A stroke_style describes how the lines of a graphical are drawn,
beyond their thickness (`graphical<-pen`) and colour.  It is used as
the `graphical<-texture`, in place of one of the texture names such
as `dashed`.  One stroke_style may be shared by many graphicals.

	new(S, stroke_style(round, round, vector(8, 4))),
	send(Path, pen, 3),
	send(Path, texture, S).

Using a gradient as <-paint, a line can change colour along its
length:

	new(G, gradient(linear, point(0,0), point(200,0))),
	send(G, add_stop, 0, colour(red)),
	send(G, add_stop, 1, colour(blue)),
	send(Line, texture, stroke_style(paint := G)).

Round and square caps and miter joins make a line cover more than the
area spanned by its points.  Class line and class path take this into
account when computing their area.

## Instance variables {#class-stroke_style-instvars}

- stroke_style<-cap: {butt,round,square}
    Shape of the ends of a line.  `butt` ends at the end point, `round`
    and `square` extend half the pen beyond it.

    **Defaults**: butt

- stroke_style<-join: {miter,round,bevel}
    Shape of the corners of a line.

    **Defaults**: miter

- stroke_style<-miter_limit: num
    Miter joins whose tip is more than this times the pen away from the
    corner are drawn as bevel joins, to avoid long spikes at sharp
    corners.

    **Defaults**: 10

- stroke_style<-dash: texture_name|vector*
    Either the name of a dash pattern as accepted by
    `graphical<-texture` or a vector of numbers holding the lengths of
    alternating dashes and gaps.  @nil draws a solid line.

    **Defaults**: @nil

- stroke_style<-dash_offset: num
    Distance into the dash pattern at the start of the line.

    **Defaults**: 0

- stroke_style<-paint: colour|gradient*
    Paint used for the line.  If @nil, the line is drawn using
    `graphical<-colour`.  The paint is ignored for an inactive
    graphical, which is drawn in its inactive colour.

    **Defaults**: @nil

## Send methods {#class-stroke_style-send}

- stroke_style->initialise: cap=[{butt,round,square}], join=[{miter,round,bevel}], dash=[texture_name|vector*], paint=[colour|gradient*]
    Create a stroke style.
