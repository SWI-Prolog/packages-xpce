# class canvas {#class-canvas}

A canvas is a graphical to draw on using operations modelled after the
[Cairo](https://www.cairographics.org/manual/) drawing API.  A path is
built using ->move_to, ->line_to, ->curve_to, ->arc, ->rectangle, etc.
and then drawn using ->stroke or ->fill with the current ->source,
->line_width, ->stroke_style and transformation:

	new(C, canvas(200, 100)),
	send(C, source, colour(red)),
	send(C, line_width, 3),
	send(C, move_to, 10, 90),
	send(C, curve_to, 50, 0, 150, 0, 190, 90),
	send(C, stroke),
	send(C, text, "A <b>canvas</b>", 10, 10),
	send(Picture, display, C, point(20, 20)).

Coordinates are in pixels relative to the top-left corner of the
canvas.  Angles are in degrees; positive angles turn clockwise, as the
y-axis points down.  Drawing outside the canvas is clipped.

What is drawn is recorded, so the canvas redraws itself without
application code.  Because the recording holds vectors rather than
pixels, a canvas remains sharp when scaled using a figure's
`figure<-transform` and when written using ->pdf or ->svg.  Unlike a
device, a canvas holds no graphicals: what is drawn cannot be moved or
selected afterwards.  Use ->clear to start over.  Graphicals may be
drawn on a canvas using ->draw_in.

The current path, source, transformation, etc.  are the _drawing
state_ of the canvas.  It is kept between the calls and may be saved
and restored using ->save_state and ->restore_state.  An operation
that is not valid, e.g., scaling by 0, raises an error.  If Cairo
detects the problem, e.g., when scaling by infinity, the drawing state
is also reset.  The drawing remains.

## Send methods {#class-canvas-send}

- canvas->initialise: width=int, height=int
    Create an empty canvas of the given size.

- canvas->geometry: x=[int], y=[int], width=[int], height=[int]
    Move and/or resize the canvas.  The drawing is kept.  Resizing
    resets the drawing state.

- canvas->clear
    Erase the drawing and reset the drawing state.

- canvas->draw_in: graphical, at=[point]
    Draw a graphical on the canvas, using the current transformation.
    As `image->draw_in`.

- canvas->move_to: x=num, y=num
    Start a new sub-path at (X,Y).

- canvas->line_to: x=num, y=num
    Add a line from the current point to (X,Y).

- canvas->rel_move_to: dx=num, dy=num
    Start a new sub-path relative to the current point.

- canvas->rel_line_to: dx=num, dy=num
    Add a line relative to the current point.

- canvas->curve_to: x1=num, y1=num, x2=num, y2=num, x3=num, y3=num
    Add a cubic Bezier curve to (X3,Y3) with control points (X1,Y1) and
    (X2,Y2).

- canvas->arc: xc=num, yc=num, radius=num, angle1=num, angle2=num
    Add a circular arc around (XC,YC) from Angle1 to Angle2, in the
    direction of increasing angles.  If there is a current point, a
    line connects it to the start of the arc.

- canvas->arc_negative: xc=num, yc=num, radius=num, angle1=num, angle2=num
    As ->arc, in the direction of decreasing angles.

- canvas->rectangle: x=num, y=num, width=num, height=num
    Add a closed rectangle as a sub-path.

- canvas->close_path
    Add a line to the start of the current sub-path and close it.

- canvas->new_path
    Discard the current path.

- canvas->new_sub_path
    Start a new sub-path without a current point.

- canvas->path: path
    Add the points of a path object, with its curve, arc and move
    points, to the current path.  See class path.

- canvas->stroke: preserve=[bool]
    Draw the line of the current path.  Unless Preserve is @on, the
    path is discarded.

- canvas->fill: preserve=[bool]
    Fill the current path, using ->fill_rule.  Unless Preserve is @on,
    the path is discarded.

- canvas->paint: alpha=[num]
    Paint the source everywhere within the clip, optionally with the
    given opacity (0..1).

- canvas->text: markup=char_array, x=num, y=num, font=[font], width=[num]*, alignment=[{left,center,right}]
    Draw text in Pango markup with its top-left corner at (X,Y) using
    the source for text without a colour in the markup.  See class
    markup_text.

- canvas->image: image=image, x=num, y=num
    Draw an image with its top-left corner at (X,Y).

- canvas->clip: preserve=[bool]
    Restrict further drawing to the current path, intersected with the
    current clip.  ->restore_state or ->reset_clip undo this.

- canvas->reset_clip
    Remove the clip.

- canvas->source: colour|gradient|image
    The paint used by ->stroke, ->fill, ->paint and ->text.  An image
    is repeated.

    **Defaults**: black

- canvas->line_width: num
    The width of lines drawn by ->stroke.

    **Defaults**: 2

- canvas->stroke_style: texture_name|stroke_style
    Dashes, caps and joins of lines drawn by ->stroke.  The paint of a
    stroke_style is ignored: use ->source.

- canvas->fill_rule: {winding,even_odd}
    What ->fill and ->clip consider inside.  See `path<-fill_rule`.

    **Defaults**: winding

- canvas->save_state
    Save the drawing state.

- canvas->restore_state
    Restore the drawing state of the matching ->save_state.  Raises an
    error if there is no such call.

- canvas->translate: dx=num, dy=num
    Move the origin.

- canvas->scale: sx=num, sy=[num]
    Scale the coordinates.  SY defaults to SX.

- canvas->rotate: degrees=num
    Rotate the coordinates clockwise.

- canvas->transform: transform
    Apply a transform object to the coordinates.

- canvas->identity
    Reset the transformation.
