/*  Examples of the chapter "Paths, paint and text" of the XPCE
    User Guide.  Each example displays its graphicals on a device,
    normally a picture:

        ?- new(P, picture), send(P, open), paths_example(P).
*/

:- module(drawing_examples,
          [ paths_example/1,
            paint_example/1,
            lines_example/1,
            markup_example/1,
            compose_example/1,
            canvas_example/1
          ]).
:- use_module(library(pce)).

		 /*******************************
		 *            PATHS		*
		 *******************************/

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

    new(Heart, path),
    send(Heart, svg_data,
         "M 10,30 A 20,20 0,0,1 50,30 A 20,20 0,0,1 90,30 \c
          Q 90,60 50,90 Q 10,60 10,30 z"),
    send(Heart, fill, colour(firebrick)),
    send(P, display, Heart, point(220, 25)),

    new(Ring, path),
    send(Ring, svg_data,
         "M 0 50 A 50 50 0 1 1 100 50 A 50 50 0 1 1 0 50 Z \c
          M 25 50 A 25 25 0 1 1 75 50 A 25 25 0 1 1 25 50 Z"),
    send(Ring, fill_rule, even_odd),
    send(Ring, fill, colour(orange)),
    send(P, display, Ring, point(330, 20)).

		 /*******************************
		 *            PAINT		*
		 *******************************/

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

%   An image of 16x16 pixels with two grey and two white squares.

checkers(Tile) :-
    new(Tile, image(@nil, 16, 16, pixmap)),
    send(Tile, fill, colour(white)),
    forall(( member(X0-Y0, [0-0, 8-8]),
             between(0, 7, DX), between(0, 7, DY)
           ),
           ( X is X0+DX, Y is Y0+DY,
             send(Tile, pixel, X, Y, colour(grey80))
           )).

		 /*******************************
		 *            LINES		*
		 *******************************/

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

		 /*******************************
		 *           MARKUP		*
		 *******************************/

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

		 /*******************************
		 *          COMPOSING		*
		 *******************************/

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

		 /*******************************
		 *            CANVAS		*
		 *******************************/

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
