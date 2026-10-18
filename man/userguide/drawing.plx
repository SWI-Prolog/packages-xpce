\chapter{Paths, paint and text}		\label{sec:drawing}

\chapref{graphics} introduced the primitive graphicals such as
\class{box}, \class{circle} and \class{text}.  \product{} draws using
the Cairo graphics library and Pango for text.  This chapter describes
the parts of these libraries that \product{} makes available:
\class{path} objects made of curves and arcs, paint made of colours
with transparency, gradients and images, line styles, text formatted
using markup, clipping, blending and a \class{canvas} to draw on using
Cairo's drawing model.

The examples of this chapter are in the file
\file{examples/drawing.pl} of the User Guide.  Each example displays
its graphicals on a device, normally a picture:

\begin{code}
?- [examples/drawing].
?- new(P, picture), send(P, open), paths_example(P).
\end{code}

The demo browser (\exam{?- manpce.}, menu \textsf{File/Demo programs})
holds galleries that show the features of this chapter with their
source: the \textsf{Path gallery}, \textsf{Paint gallery} and
\textsf{Text gallery}, as well as the \textsf{Canvas} demo.


\section{Paths}					\label{sec:paths}

\index{path}\index{curve}\index{arc}\index{SVG,path}%
A \class{path} is a line through a chain of points.  The class of each
point determines how the line gets there from the previous point.  A
plain \class{point} is reached by a straight line.  The subclasses of
\class{point} in \tabref{pathpoints} describe the other segments, using
the vocabulary of Cairo and SVG.  As they are points, everything that
works on the points of a path, such as `path ->append', moving and
resizing the path, keeps working.  The control points of curves and the
ellipses of arcs move and scale with the path.

\begin{table}
\begin{center}
\begin{tabularlp}{\class{curve_point}}
\hline
\class{point}		& A straight line to the point. \\
\class{curve_point}	& A quadratic Bezier curve (one control point)
			  or a cubic Bezier curve (two control points)
			  to the point. \\
\class{arc_point}	& An elliptical arc to the point.  As in SVG,
			  the arc is defined by the radii and rotation of
			  the ellipse and two flags that select one of the
			  four arcs through the two points. \\
\class{move_point}	& No line: start a new sub-path at the point. \\
\hline
\end{tabularlp}
\end{center}
\caption{Points that define the segments of a path}
\label{tab:pathpoints}
\end{table}

The example below creates the paths of \figref{drawpaths}: a curve with
an arrow, an arch, a heart and a ring.  The arrows of a path follow the
direction of the curve at its ends.

\begin{code}
paths_example(P) :-
    new(Curve, path),
    send_list(Curve, append,
              [ point(0, 80),
                curve_point(90, 80, point(20, 0), point(70, 160))
              ]),
    send(Curve, arrows, second),
    send(P, display, Curve, point(10, 20)),

    new(Arch, path),
    send_list(Arch, append,
              [ point(0, 100), point(0, 40),
                arc_point(80, 40, 40),
                point(80, 100)
              ]),
    send(Arch, closed, @on),
    send(Arch, fill, colour(light_steel_blue)),
    send(P, display, Arch, point(120, 20)),
    ...
\end{code}

\postscriptfig{drawpaths}{Paths with curves, arcs and a hole}

\index{svg_data}%
Paths are often available in the notation of SVG, for example from an
icon.  `path ->svg_data' sets the points of a path from the \exam{d}
attribute of an SVG \exam{<path>} element and `path <-svg_data' returns
them in this notation.  All SVG path commands are supported.  The heart
of \figref{drawpaths} is created using

\begin{code}
    new(Heart, path),
    send(Heart, svg_data,
         "M 10,30 A 20,20 0,0,1 50,30 A 20,20 0,0,1 90,30 \c
          Q 90,60 50,90 Q 10,60 10,30 z"),
    send(Heart, fill, colour(firebrick)),
\end{code}

\index{fill_rule}\index{sub-path}%
A \class{move_point} starts a sub-path.  A path whose sub-paths overlap
can be filled in two ways, selected using `path ->fill_rule'.  Using
\const{winding} (the default), a point is inside if the path goes
around it more often in one direction than in the other.  Using
\const{even_odd}, a point is inside if a ray from it crosses the path
an odd number of times, so a sub-path inside another one is a hole.
The ring of \figref{drawpaths} is made of two circles, each consisting
of two arcs:

\begin{code}
    new(Ring, path),
    send(Ring, svg_data,
         "M 0 50 A 50 50 0 1 1 100 50 A 50 50 0 1 1 0 50 Z \c
          M 25 50 A 25 25 0 1 1 75 50 A 25 25 0 1 1 25 50 Z"),
    send(Ring, fill_rule, even_odd),
    send(Ring, fill, colour(orange)),
\end{code}

`path ->closed' closes all sub-paths, unless a \class{move_point}
specifies otherwise.  `path ->radius' rounds the corners between
straight segments.  A path of kind \const{smooth} draws an interpolated
curve through its points and ignores the segment classes.  The area of
a path, `path ->inside' and selecting a path with the mouse are exact
for curves and arcs, and take the shape of the corners and ends of the
line into account.


\section{Paint}					\label{sec:paint}

\index{gradient}\index{alpha}\index{transparency}%
The `->fill' of a graphical such as a \class{box}, \class{ellipse} or
\class{path} and the background of a \class{figure} may be a
\class{colour}, a \class{gradient} or an \class{image}.

\paragraph{Colours} may be partially transparent.  The alpha value
ranges from 0 (transparent) to 255 (opaque) and is given as the fifth
argument of a colour created from its components or using the notation
\verb$'#RRGGBBAA'$:

\begin{code}
    send(Circle, fill, colour(@default, 255, 0, 0, 128)),
    send(Circle, fill, colour('#ff000080')),
\end{code}

\paragraph{Gradients} are linear or radial colour transitions.  A
linear gradient runs from point \arg{p0} to \arg{p1}.  A radial
gradient runs from a circle around \arg{p0} with radius \arg{r0} to a
circle around \arg{p1} with radius \arg{r1}.  The colours are added as
stops at a fraction between 0 and 1.  `gradient <-extend' determines
what is painted beyond the first and last stop: nothing
(\const{none}), the colour of the nearest stop (\const{pad}, the
default), the gradient repeated (\const{repeat}) or repeated and
mirrored (\const{reflect}).

The points and radii of a gradient are interpreted according to
`gradient <-units'.  If \const{user} (the default), they are
coordinates of the device on which the shape is displayed.  The
gradient does not move with the shape: moving a shape shows another
part of the gradient.  If \const{bounding_box}, they are fractions of
the box around the shape: (0,0) is its top-left and (1,1) its
bottom-right corner.  Such a gradient moves and scales with the shape
and can be shared by shapes of any size.  The example below fills three
boxes of different sizes with the same gradient and uses a radial
gradient with a small start circle that is not centred, so the circle
looks like a ball.  The result is in \figref{drawpaint}.

\begin{code}
paint_example(P) :-
    new(Sky, gradient(linear, point(0,0), point(0,1),
                      units := bounding_box)),
    send(Sky, add_stop, 0, colour(midnight_blue)),
    send(Sky, add_stop, 1, colour(sky_blue)),
    forall(member(X-W-H, [10-60-120, 80-90-60, 180-40-90]),
           ( new(B, box(W, H)),
             send(B, fill, Sky),
             send(P, display, B, point(X, 10))
           )),

    new(Glow, gradient(radial, point(0.35,0.35), point(0.5,0.5),
                       0, 0.5, units := bounding_box)),
    send(Glow, add_stop, 0, colour(white)),
    send(Glow, add_stop, 1, colour(dark_orange)),
    send(P, display, new(C, circle(90)), point(240, 10)),
    send(C, fill, Glow),

    checkers(Tile),
    send(P, display, new(T, box(90, 90)), point(350, 10)),
    send(T, fill, Tile),
    send(T, fill_offset, point(0, 0)).
\end{code}

\postscriptfig{drawpaint}{A shared gradient, a radial gradient and an
image fill}

\paragraph{Images} used as fill are repeated.  The pattern starts at
the origin of the window, so it does not move with the shape.
`box ->fill_offset' makes the pattern start at the given offset from
the box, so it moves along.  The image \arg{Tile} above is a 16x16
pixmap with a checkers pattern.  See \secref{images} for creating
images.

\paragraph{Opacity} applies to a graphical as a whole.  See
\secref{opacity}.


\section{Line styles}				\label{sec:linestyles}

\index{stroke_style}\index{dash}\index{line,cap}\index{line,join}%
The line of a graphical is drawn using its `graphical <-pen' (the
width) and `graphical <-colour'.  `graphical <-texture' is the name of
a dash pattern such as \const{dotted} or \const{dashed}, or a
\class{stroke_style} that also determines the shape of the ends
(\const{butt}, \const{round} or \const{square}) and corners
(\const{miter}, \const{round} or \const{bevel}) of the line.  A
\class{stroke_style} accepts a \class{vector} with the lengths of
dashes and gaps and a \arg{paint} that replaces the colour of the line,
for example a gradient.  The example below creates \figref{drawlines}.

\begin{code}
lines_example(P) :-
    forall(nth0(I, [miter, round, bevel], Join),
           ( new(Z, path),
             send_list(Z, append,
                       [point(0,40), point(30,0), point(60,40)]),
             send(Z, pen, 10),
             send(Z, texture, stroke_style(butt, Join)),
             X is 20+I*80,
             send(P, display, Z, point(X, 20))
           )),
    forall(nth0(I, [butt, round, square], Cap),
           ( new(L, line(0, 0, 80, 0)),
             send(L, pen, 10),
             send(L, texture, stroke_style(Cap)),
             Y is 90+I*22,
             send(P, display, L, point(20, Y))
           )),
    new(Rainbow, gradient(linear, point(0,0), point(1,0),
                          units := bounding_box)),
    forall(nth0(I, [red, orange, gold, green, blue, purple], C),
           ( F is I/5,
             send(Rainbow, add_stop, F, colour(C))
           )),
    new(S, path),
    send(S, svg_data,
         "M 0 60 C 40 -20 80 140 120 60 S 180 -20 200 60"),
    send(S, pen, 6),
    send(S, texture, stroke_style(round, round, vector(18, 10),
                                  Rainbow)),
    send(P, display, S, point(230, 30)).
\end{code}

\postscriptfig{drawlines}{Joins, caps and a dashed line painted with a
gradient}


\section{Text with markup}			\label{sec:markup}

\index{markup}\index{Pango}\index{text,rich}%
Class \class{markup_text} displays text formatted using Pango markup.
This markup can mix fonts, sizes, styles and colours in one text.  The
most common tags are \verb$<b>$, \verb$<i>$, \verb$<u>$, \verb$<s>$,
\verb$<tt>$, \verb$<big>$, \verb$<small>$, \verb$<sub>$ and
\verb$<sup>$.  The \verb$<span>$ tag accepts attributes such as
\exam{font}, \exam{size}, \exam{foreground} and \exam{background}.  The
characters \verb$<$, \verb$>$ and \verb$&$ are written as
\verb$&lt;$, \verb$&gt;$ and \verb$&amp;$.  Text without a colour in
the markup is drawn using `graphical <-colour'.

Pango also does the layout.  If a `markup_text <-width' is given, the
lines are wrapped at this width.  The lines are aligned according to
`markup_text <-alignment' and may be justified.
`markup_text <-ellipsize' shortens lines that do not fit by replacing
part of the text by `\ldots'.  Unlike class \class{text}, a
\class{markup_text} cannot be edited.  A \class{figure} with a
transformation rotates or scales the text.  The example below creates
\figref{drawmarkup}.

\begin{code}
markup_example(P) :-
    send(P, display,
         markup_text("Text with <b>bold</b>, <i>italic</i>, \c
                      <span foreground='red'>red</span> and \c
                      <span size='x-large'>large</span> parts"),
         point(10, 10)),
    new(Wrapped,
        markup_text("This paragraph is wrapped at a width \c
                     of 200 pixels and <i>justified</i>, \c
                     so both edges line up.", 200)),
    send(Wrapped, justify, @on),
    send(P, display, Wrapped, point(10, 45)),
    new(Ellipsized,
        markup_text("A line that is too long to fit is \c
                     ellipsized", 160)),
    send(Ellipsized, ellipsize, middle),
    send(P, display, Ellipsized, point(10, 150)),
    new(F, figure),
    send(F, display,
         markup_text("<span size='large'>Rotated \c
                      <b>text</b></span>")),
    send(F, rotate, -25),
    send(P, display, F, point(260, 50)).
\end{code}

\postscriptfig{drawmarkup}{Text formatted using Pango markup}


\section{Transforming, clipping and blending}	\label{sec:compositing}

\index{transform}\index{clip}\index{blend}%
A \class{figure} can do more with its graphicals than display them:

\begin{itemize}
    \item `figure ->transform' rotates, scales and shears the
    graphicals.  `figure ->translate', `figure ->rotate',
    `figure ->scale' and `figure ->shear' modify the transformation.
    Events and the area of the figure follow the transformation.

    \item `figure ->clip_path' clips the graphicals to a path, using its
    fill rule.  The path is in the coordinate system of the graphicals,
    so it is transformed with them.  Clipping only affects drawing:
    graphicals that are clipped away still receive events.

    \item `figure ->blend' determines how the figure is composited onto
    what is below it.  The blend modes are those of Cairo and the CSS
    property \exam{mix-blend-mode}: \const{normal}, \const{multiply},
    \const{screen}, \const{overlay}, \const{darken}, \const{lighten},
    \const{color_dodge}, \const{color_burn}, \const{hard_light},
    \const{soft_light}, \const{difference}, \const{exclusion},
    \const{hue}, \const{saturation}, \const{color},
    \const{luminosity} and \const{add}.

    \item `graphical ->opacity' fades the figure as a whole.  See
    \secref{opacity}.
\end{itemize}

The example below clips stripes to the shape of a heart and mixes three
circles using \const{multiply}, which combines colours as transparent
inks do.  The result is in \figref{drawcompose}.

\begin{code}
compose_example(P) :-
    new(F, figure),
    forall(between(0, 9, I),
           ( Colour = colour(@default, 255, I*25, 0),
             new(Stripe, box(110, 10)),
             send(Stripe, pen, 0),
             send(Stripe, fill, Colour),
             Y is I*10,
             send(F, display, Stripe, point(0, Y))
           )),
    new(Heart, path),
    send(Heart, svg_data,
         "M 10,30 A 20,20 0,0,1 50,30 A 20,20 0,0,1 90,30 \c
          Q 90,60 50,90 Q 10,60 10,30 z"),
    send(F, clip_path, Heart),
    send(P, display, F, point(10, 20)),

    forall(member(X-Y-Colour,
                  [ 0-0-cyan, 50-0-magenta, 25-40-yellow ]),
           ( new(Fig, figure),
             send(Fig, display, new(C, circle(80))),
             send(C, pen, 0),
             send(C, fill, colour(Colour)),
             send(Fig, blend, multiply),
             PX is 170+X, PY is 10+Y,
             send(P, display, Fig, point(PX, PY))
           )).
\end{code}

\postscriptfig{drawcompose}{A figure clipped to a heart and circles
blended using multiply}


\section{The canvas}				\label{sec:canvas}

\index{canvas}\index{Cairo}%
Graphical objects are the natural way to build a drawing that the user
manipulates.  Some drawings are computed instead, such as the plot of a
function, and are not manipulated afterwards.  A \class{canvas} is a
graphical to draw such drawings on using Cairo's drawing model:

\begin{enumerate}
    \item Build a path using `canvas ->move_to', `canvas ->line_to',
    `canvas ->curve_to', `canvas ->arc', `canvas ->rectangle', etc.
    `canvas ->path' adds the points of a \class{path} object.

    \item Draw it using `canvas ->stroke' or fill it using
    `canvas ->fill'.  These use the drawing state: the paint set using
    `canvas ->source' (a colour, gradient or image),
    `canvas ->line_width', `canvas ->stroke_style',
    `canvas ->fill_rule' and the transformation set using
    `canvas ->translate', `canvas ->scale' and `canvas ->rotate'.
\end{enumerate}

Further, `canvas ->text' draws markup, `canvas ->image' draws an image,
`canvas ->clip' restricts drawing to the path and `canvas ->paint'
paints the source everywhere.  `canvas ->save_state' and
`canvas ->restore_state' save and restore the drawing state (Cairo's
\exam{save} and \exam{restore}).  `canvas ->draw_in' draws a graphical
object on the canvas.

What is drawn is recorded, so the canvas redraws itself without help
of the application.  The recording consists of vectors rather than
pixels, so a canvas remains sharp when its figure is scaled and when it
is saved as SVG or PDF.  `canvas ->clear' erases the drawing.  The
example below draws a rose curve, shown in \figref{drawcanvas}.  The
\textsf{Canvas} demo draws other curves under control of sliders.

\begin{code}
canvas_example(P) :-
    new(C, canvas(200, 160)),
    send(C, translate, 100, 80),
    rose(C, 7, 70),
    send(C, line_width, 2),
    send(C, source, colour(dark_magenta)),
    send(C, stroke),
    send(C, identity),
    send(C, source, colour(black)),
    send(C, text, "<b>r = cos(7t)</b>", 5, 140),
    send(P, display, C, point(10, 10)).

%   Add the rose curve r = R*cos(K*t) to the path of canvas C.

rose(C, K, R) :-
    forall(between(0, 360, I),
           ( T is I*pi/180,
             Rad is R*cos(K*T),
             X is Rad*cos(T), Y is Rad*sin(T),
             (   I =:= 0
             ->  send(C, move_to, X, Y)
             ;   send(C, line_to, X, Y)
             )
           )),
    send(C, close_path).
\end{code}

\postscriptfig{drawcanvas}{A rose curve drawn on a canvas}

The alternative to a canvas is a graphical that redefines
->_redraw_area (see \secref{ugraphics}).  Such a graphical draws
itself each time it must be repainted, using `graphical ->draw_path'
to draw a \class{path} object and `graphical ->draw_markup' to draw
markup, besides the methods of \tabref{paintmethods}.  This is the
better choice if the drawing depends on data that changes, so it must
be drawn anew anyway.


\section{Saving drawings}			\label{sec:savedrawing}

\index{SVG}\index{PDF}%
`graphical ->svg' and `graphical ->pdf' save a graphical, including a
\class{device} or \class{figure} with everything on it, as an SVG image
or a PDF document.  `window ->svg' and `window ->pdf' save the full
contents of a window.  As \product{} draws using Cairo for the screen as
well as for these files, they look the same as the screen and hold
vectors, so they can be scaled without loss.  See also
\secref{printing}.  `display ->copy' puts a graphical on the clipboard
as SVG and PNG, so it can be pasted into other applications (see
\secref{clipboard}).
