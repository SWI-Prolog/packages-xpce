/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org
    Copyright (c)  2026, SWI-Prolog Solutions b.v.
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(test_canvas, [test_canvas/0]).

/** <module> Tests for class canvas and graphical->draw_path

Run with:

    swipl -g test_canvas -t halt packages/xpce/tests/test_canvas.pl
*/

:- set_prolog_flag('SDL_VIDEODRIVER', dummy).

:- use_module(library(pce)).
:- use_module(library(plunit)).

test_canvas :-
    run_tests([ canvas
              ]).

red(0xffff0000).
white(0xffffffff).

%!  pixels(+Graphical, +Points, -Pixels) is det.
%
%   Draw Graphical into a 40x40 image and get the RGBA values at Points.

pixels(Gr, Points, Pixels) :-
    new(I, image(@nil, 40, 40)),
    send(I, draw_in, Gr),
    findall(RGBA, ( member(X-Y, Points),
                    get(I, pixel, X, Y, C),
                    get(C, rgba, RGBA)
                  ), Pixels),
    free(I).

red_canvas(C) :-
    new(C, canvas(40, 40)),
    send(C, source, colour(red)).

%   A graphical that draws a path filled red from '_redraw_area'.

:- pce_begin_class(tc_path_drawer, graphical,
                   "Draw a path using ->draw_path").

initialise(G) :->
    send_super(G, initialise, 0, 0, 40, 40).

'_redraw_area'(G, _A:area) :->
    new(P, path),
    send(P, svg_data, "M 0 0 H 20 V 20 H 0 Z"),
    send(G, graphics_state, 0),
    send(G, draw_path, P, colour(red)).

:- pce_end_class(tc_path_drawer).

:- begin_tests(canvas).

test(fill_rectangle, Pixels == [R, W]) :-
    red(R), white(W),
    red_canvas(C),
    send(C, rectangle, 0, 0, 20, 20),
    send(C, fill),
    pixels(C, [10-10, 30-30], Pixels).
test(stroke, Pixels == [R, W]) :-
    red(R), white(W),
    red_canvas(C),
    send(C, line_width, 4),
    send(C, move_to, 0, 10),
    send(C, line_to, 40, 10),
    send(C, stroke),
    pixels(C, [20-10, 20-20], Pixels).
test(fill_preserve_keeps_path, Pixel == B) :-
    B = 0xff0000ff,
    red_canvas(C),
    send(C, rectangle, 10, 10, 20, 20),
    send(C, fill, @on),
    send(C, source, colour(blue)),
    send(C, line_width, 4),
    send(C, stroke),
    pixels(C, [10-20], [Pixel]).
test(fill_consumes_path, Pixel == R) :-
    red(R),
    red_canvas(C),
    send(C, rectangle, 10, 10, 20, 20),
    send(C, fill),
    send(C, source, colour(blue)),
    send(C, line_width, 4),
    send(C, stroke),
    pixels(C, [10-20], [Pixel]).
test(translate, Pixels == [W, R]) :-
    red(R), white(W),
    red_canvas(C),
    send(C, translate, 20, 20),
    send(C, rectangle, 0, 0, 10, 10),
    send(C, fill),
    pixels(C, [5-5, 25-25], Pixels).
test(save_and_restore_state, Pixels == [R, R]) :-
    red(R),
    red_canvas(C),
    send(C, save_state),
    send(C, translate, 20, 20),
    send(C, rectangle, 0, 0, 10, 10),
    send(C, fill),
    send(C, restore_state),
    send(C, rectangle, 0, 0, 10, 10),
    send(C, fill),
    pixels(C, [5-5, 25-25], Pixels).
test(unbalanced_restore, throws(_)) :-
    red_canvas(C),
    send(C, restore_state).
test(scale_by_zero, throws(_)) :-
    red_canvas(C),
    send(C, scale, 0).
test(usable_after_cairo_error, Pixel == R) :-  % Cairo errors are permanent
    red(R),
    red_canvas(C),
    Inf is inf,
    catch(send(C, scale, Inf), _, true),
    send(C, source, colour(red)),
    send(C, rectangle, 0, 0, 10, 10),
    send(C, fill),
    pixels(C, [5-5], [Pixel]).
test(rotate, Pixels == [R, W]) :-          % rotate 90: x becomes y
    red(R), white(W),
    red_canvas(C),
    send(C, translate, 40, 0),
    send(C, rotate, 90),
    send(C, rectangle, 0, 0, 40, 10),
    send(C, fill),
    pixels(C, [35-20, 5-20], Pixels).
test(clip, Pixels == [R, W]) :-
    red(R), white(W),
    red_canvas(C),
    send(C, rectangle, 0, 0, 20, 40),
    send(C, clip),
    send(C, paint),
    pixels(C, [10-10, 30-10], Pixels).
test(even_odd, Pixels == [R, W]) :-
    red(R), white(W),
    red_canvas(C),
    send(C, fill_rule, even_odd),
    send(C, rectangle, 0, 0, 40, 40),
    send(C, rectangle, 10, 10, 20, 20),
    send(C, fill),
    pixels(C, [5-5, 20-20], Pixels).
test(gradient_source, Pixel == R) :-
    red(R),
    new(G, gradient(linear, point(0,0), point(40,0))),
    send(G, add_stop, 0, colour(red)),
    send(G, add_stop, 1, colour(red)),
    new(C, canvas(40, 40)),
    send(C, source, G),
    send(C, paint),
    pixels(C, [20-20], [Pixel]).
test(path_object, Pixels == [R, W]) :-
    red(R), white(W),
    red_canvas(C),
    new(P, path),
    send(P, svg_data, "M 0 0 H 20 V 20 H 0 Z"),
    send(C, path, P),
    send(C, fill),
    pixels(C, [10-10, 30-30], Pixels).
test(text, Pixel == R) :-
    red(R),
    new(C, canvas(40, 40)),
    send(C, text, "<span background='#ff0000'>      </span>", 0, 0),
    pixels(C, [5-5], [Pixel]).
test(image, Pixel == R) :-
    red(R),
    new(Img, image(@nil, 10, 10, pixmap)),
    send(Img, fill, colour(red)),
    new(C, canvas(40, 40)),
    send(C, image, Img, 20, 20),
    pixels(C, [25-25], [Pixel]).
test(draw_in_transformed, Pixel == R) :-
    red(R),
    new(C, canvas(40, 40)),
    send(C, translate, 20, 20),
    new(B, box(10, 10)),
    send(B, pen, 0),
    send(B, fill, colour(red)),
    send(C, draw_in, B, point(0, 0)),
    pixels(C, [25-25], [Pixel]).
test(draw_in, Pixels == [W, R]) :-
    red(R), white(W),
    new(C, canvas(40, 40)),
    new(B, box(10, 10)),
    send(B, pen, 0),
    send(B, fill, colour(red)),
    send(C, draw_in, B, point(20, 20)),
    pixels(C, [5-5, 25-25], Pixels).
test(clear, Pixel == W) :-
    white(W),
    red_canvas(C),
    send(C, paint),
    send(C, clear),
    pixels(C, [5-5], [Pixel]).
test(resize_keeps_drawing, [Pixel == R, Width == 60]) :-
    red(R),
    red_canvas(C),
    send(C, rectangle, 0, 0, 10, 10),
    send(C, fill),
    send(C, size, size(60, 60)),
    get(C, width, Width),
    pixels(C, [5-5], [Pixel]).
test(scaled_in_figure, Pixels == [R, W]) :-
    red(R), white(W),
    red_canvas(C),
    send(C, rectangle, 0, 0, 10, 10),
    send(C, fill),
    new(F, figure),
    send(F, display, C),
    send(F, scale, 2),
    pixels(F, [15-15, 25-25], Pixels).
test(svg, [ condition(send(@pce, has_feature, svg)),
            true(sub_string(Data, _, _, _, "<svg"))
          ]) :-
    red_canvas(C),
    send(C, paint),
    tmp_file_stream(text, File, Out), close(Out),
    send(C, svg, File),
    read_file_to_string(File, Data, []),
    delete_file(File).
test(draw_path, Pixels == [R, W]) :-
    red(R), white(W),
    new(G, tc_path_drawer),
    pixels(G, [10-10, 30-30], Pixels).

:- end_tests(canvas).
