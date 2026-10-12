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

:- module(test_cairo_render, [test_cairo_render/0]).

/** <module> Tests for what the Cairo drawing primitives put in pixels

Each test draws a graphical into an image and reads pixels back, so it
needs no window and runs under SDL's dummy video driver.

Run with:

    swipl -g test_cairo_render -t halt \
          packages/xpce/tests/test_cairo_render.pl
*/

:- set_prolog_flag('SDL_VIDEODRIVER', dummy).

:- use_module(library(pce)).
:- use_module(library(plunit)).

test_cairo_render :-
    run_tests([ cairo_render,
                stroke_style,
                gradient_units,
                clip,
                blend,
                export
              ]).

%!  pixel_after_drawing(+Graphical, +Size, +X, +Y, -RGBA) is det.
%
%   Draw Graphical into a fresh image of Size x Size pixels and
%   return the RGBA value of the pixel at X,Y.

pixel_after_drawing(Gr, Size, X, Y, RGBA) :-
    new(I, image(@nil, Size, Size)),
    send(I, draw_in, Gr),
    get(I, pixel, X, Y, C),
    get(C, rgba, RGBA),
    free(I).

%!  uniform_gradient(+Colour, -Gradient) is det.
%
%   Gradient is a linear gradient that has Colour all along.

uniform_gradient(Colour, G) :-
    new(G, gradient(linear, point(0,0), point(20,0))),
    send(G, add_stop, 0, Colour),
    send(G, add_stop, 1, Colour).

%   A graphical that fills its area using ->draw_fill.

:- pce_begin_class(tcr_filler, graphical,
                   "Fill the area using ->draw_fill").

variable(fill, any, both, "Fill passed to ->draw_fill").

initialise(G, Fill:any) :->
    send_super(G, initialise, 0, 0, 20, 20),
    send(G, slot, fill, Fill).

'_redraw_area'(G, _A:area) :->
    get(G, area, area(X, Y, W, H)),
    get(G, fill, Fill),
    send(G, draw_fill, X, Y, W, H, Fill).

:- pce_end_class(tcr_filler).

%   A graphical that draws a filled square with a thick black outline
%   using ->draw_poly.

:- pce_begin_class(tcr_poly, graphical,
                   "Draw a filled polygon using ->draw_poly").

initialise(G) :->
    send_super(G, initialise, 0, 0, 20, 20).

'_redraw_area'(G, _A:area) :->
    send(G, graphics_state, 4, none, black),
    send(G, draw_poly,
         chain(point(2,2), point(18,2), point(18,18), point(2,18)),
         @on, colour(red)).

:- pce_end_class(tcr_poly).

%   A 4x4 image whose top-left 2x2 pixels are red and the others white.

tile(T) :-
    new(T, image(@nil, 4, 4, pixmap)),
    send(T, fill, colour(white)),
    forall(member(X-Y, [0-0, 1-0, 0-1, 1-1]),
           send(T, pixel, X, Y, colour(red))).

%   Pixel at X,10 of a box 40 wide filled with a gradient that goes
%   from red to blue between x=0 and x=10.

extended_gradient_pixel(Extend, X, RGBA) :-
    new(G, gradient(linear, point(0,0), point(10,0),
                    @default, @default, @default, Extend)),
    send(G, add_stop, 0, colour(red)),
    send(G, add_stop, 1, colour(blue)),
    new(B, box(40, 20)),
    send(B, pen, 0),
    send(B, fill, G),
    pixel_after_drawing(B, 40, X, 10, RGBA).

rgb(RGBA, R, G, B) :-
    R is (RGBA >> 16) /\ 0xff,
    G is (RGBA >> 8) /\ 0xff,
    B is RGBA /\ 0xff.

%   A graphical that clips to its top-left 10x10 pixels and draws a
%   rounded box with an elevation over its whole area.

:- pce_begin_class(tcr_clipped_3d, graphical,
                   "Draw a 3D box in a clipped area").

initialise(G) :->
    send_super(G, initialise, 0, 0, 40, 40).

'_redraw_area'(G, _A:area) :->
    send(G, clip, area(0, 0, 10, 10)),
    send(G, draw_box, 0, 0, 40, 40, 8, elevation(@nil, 3, red)),
    send(G, unclip).

:- pce_end_class(tcr_clipped_3d).

filled_box(Fill, B) :-
    new(B, box(20,20)),
    send(B, pen, 0),
    send(B, fill, Fill).

:- begin_tests(cairo_render).

test(gradient_stops_are_exact, RGBA == 0xffff0000) :-
    uniform_gradient(colour(red), G),
    filled_box(G, B),
    pixel_after_drawing(B, 20, 10, 10, RGBA).

test(draw_fill_colour, RGBA == 0xffff0000) :-
    new(G, tcr_filler(colour(red))),
    pixel_after_drawing(G, 20, 10, 10, RGBA).
test(draw_fill_gradient, RGBA == 0xffff0000) :-
    uniform_gradient(colour(red), Gradient),
    new(G, tcr_filler(Gradient)),
    pixel_after_drawing(G, 20, 10, 10, RGBA).
test(figure_gradient_background, RGBA == 0xffff0000) :-
    uniform_gradient(colour(red), Gradient),
    new(F, figure),
    send(F, background, Gradient),
    send(F, display, new(B, box(20, 20))),
    send(B, pen, 0),
    pixel_after_drawing(F, 20, 10, 10, RGBA).

test(image_fill_tiles, Pixels == [R, R, W, R, R, W]) :-
    R = 0xffff0000, W = 0xffffffff,
    tile(T),
    filled_box(T, B),
    findall(P, ( member(X-Y, [0-0, 1-1, 2-2, 4-0, 5-5, 6-6]),
                 pixel_after_drawing(B, 20, X, Y, P)
               ), Pixels).
test(image_fill_offset, Pixels == [W, R, R, W]) :-
    R = 0xffff0000, W = 0xffffffff,
    tile(T),
    filled_box(T, B),
    send(B, fill_offset, point(2,2)),
    findall(P, ( member(X-Y, [0-0, 2-2, 3-3, 4-4]),
                 pixel_after_drawing(B, 20, X, Y, P)
               ), Pixels).
test(image_fill_offset_is_relative_to_the_box, Pixels == [R, W, R]) :-
    R = 0xffff0000, W = 0xffffffff,
    tile(T),
    filled_box(T, B),
    send(B, set, 5, 5),
    send(B, fill_offset, point(0,0)),       % tiles start at (5,5)
    findall(P, ( member(X-Y, [6-6, 8-8, 9-9]),
                 pixel_after_drawing(B, 30, X, Y, P)
               ), Pixels).
test(colour_name_is_not_an_image, Class == colour) :-
    new(B, box(20, 20)),
    send(B, fill, red),
    get(B?fill, class_name, Class).
test(gradient_pad, RGBA == 0xff0000ff) :-
    extended_gradient_pixel(@default, 35, RGBA).
test(gradient_extend_none, RGBA == 0xffffffff) :-
    extended_gradient_pixel(none, 35, RGBA).
test(gradient_repeat, true(R > B)) :-        % 31 is 1 into a repeat
    extended_gradient_pixel(repeat, 31, RGBA),
    rgb(RGBA, R, _, B).
test(gradient_reflect, true(R > B)) :-       % 39 is 1 from the next start
    extended_gradient_pixel(reflect, 39, RGBA),
    rgb(RGBA, R, _, B).
test(rounded_3d_box_keeps_the_clip, RGBA == 0xffffffff) :-
    new(G, tcr_clipped_3d),
    pixel_after_drawing(G, 40, 30, 30, RGBA).
test(draw_poly_strokes_over_fill, Inner-Outline == 0xffff0000-0xff000000) :-
    new(G, tcr_poly),
    pixel_after_drawing(G, 20, 10, 10, Inner),
    pixel_after_drawing(G, 20, 3, 10, Outline).

:- end_tests(cairo_render).

%   A horizontal line from (10,10) to (30,10) with a thick pen.

thick_line(Texture, L) :-
    new(L, line(10, 10, 30, 10)),
    send(L, pen, 10),
    send(L, texture, Texture).

%   A graphical that draws a line with a round cap between saving and
%   restoring the graphics state, and one with the restored state.

:- pce_begin_class(tcr_saved_style, graphical,
                   "Draw using a saved stroke_style").

initialise(G) :->
    send_super(G, initialise, 0, 0, 40, 40).

'_redraw_area'(G, _A:area) :->
    send(G, graphics_state, 10, stroke_style(round), black),
    send(G, save_graphics_state),
    send(G, graphics_state, 10, none, black),
    send(G, restore_graphics_state),
    send(G, draw_line, 10, 20, 30, 20).

:- pce_end_class(tcr_saved_style).

:- begin_tests(stroke_style).

test(butt_cap_ends_at_the_point, RGBA == 0xffffffff) :-
    thick_line(none, L),
    pixel_after_drawing(L, 40, 33, 10, RGBA).
test(round_cap_extends_beyond_the_point, true(RGBA \== 0xffffffff)) :-
    thick_line(stroke_style(round), L),
    pixel_after_drawing(L, 40, 33, 10, RGBA).
test(cap_extends_line_area, true(X =< 5)) :-
    thick_line(stroke_style(square), L),
    get(L?area, x, X).
test(dash_vector_gap, [true(Dash \== 0xffffffff), Gap == 0xffffffff]) :-
    new(L, line(0, 10, 40, 10)),
    send(L, pen, 4),
    send(L, texture, stroke_style(@default, @default, vector(10, 10))),
    pixel_after_drawing(L, 40, 5, 10, Dash),
    pixel_after_drawing(L, 40, 15, 10, Gap).
test(dash_offset, Pixel == 0xffffffff) :-
    new(L, line(0, 10, 40, 10)),
    send(L, pen, 4),
    new(S, stroke_style(@default, @default, vector(10, 10))),
    send(S, dash_offset, 10),
    send(L, texture, S),
    pixel_after_drawing(L, 40, 5, 10, Pixel).
test(paint_overrides_colour, RGBA == 0xffff0000) :-
    uniform_gradient(colour(red), G),
    thick_line(stroke_style(@default, @default, @default, G), L),
    pixel_after_drawing(L, 40, 20, 10, RGBA).
test(no_paint_uses_colour, RGBA == 0xff0000ff) :-
    thick_line(stroke_style(round), L),
    send(L, colour, blue),
    pixel_after_drawing(L, 40, 20, 10, RGBA).
test(path_area_includes_miter, true(Y < -6)) :-  % hull+pen is -6
    new(P, path),
    send(P, append, point(0,30)),
    send(P, append, point(40,0)),
    send(P, append, point(80,30)),
    send(P, pen, 12),
    get(P?area, y, Y).
test(path_area_with_bevel, true(Y >= -6)) :-
    new(P, path),
    send(P, append, point(0,30)),
    send(P, append, point(40,0)),
    send(P, append, point(80,30)),
    send(P, pen, 12),
    send(P, texture, stroke_style(@default, bevel)),
    get(P?area, y, Y).
test(saved_and_restored, true(RGBA \== 0xffffffff)) :-
    new(G, tcr_saved_style),
    pixel_after_drawing(G, 40, 33, 20, RGBA).
test(dash_without_dashes, throws(_)) :-
    new(_, stroke_style(@default, @default, vector(0, 0))).

:- end_tests(stroke_style).

%   A red-to-blue gradient from top to bottom, relative to the shape.

top_down_gradient(G) :-
    new(G, gradient(linear, point(0,0), point(0,1),
                    @default, @default, @default, @default, bounding_box)),
    send(G, add_stop, 0, colour(red)),
    send(G, add_stop, 1, colour(blue)).

reddish(RGBA)  :- rgb(RGBA, R, _, B), R > B.
blueish(RGBA)  :- rgb(RGBA, R, _, B), B > R.

:- begin_tests(gradient_units).

test(user_is_default, U == user) :-
    new(G, gradient(linear, point(0,0), point(0,1))),
    get(G, units, U).
test(shared_by_two_boxes, true(( reddish(T1), blueish(B1),
                                 reddish(T2), blueish(B2) ))) :-
    top_down_gradient(G),
    new(D, device),
    send(D, display, new(Box1, box(20, 20)), point(0, 0)),
    send(D, display, new(Box2, box(20, 20)), point(20, 20)),
    forall(member(B, [Box1, Box2]), (send(B, pen, 0), send(B, fill, G))),
    pixel_after_drawing(D, 40, 10, 2, T1),
    pixel_after_drawing(D, 40, 10, 17, B1),
    pixel_after_drawing(D, 40, 30, 22, T2),
    pixel_after_drawing(D, 40, 30, 37, B2).
test(follows_moved_shape, true(reddish(Top))) :-
    top_down_gradient(G),
    new(B, box(20, 20)),
    send(B, pen, 0),
    send(B, fill, G),
    send(B, set, 10, 20),
    pixel_after_drawing(B, 40, 20, 22, Top).
test(radial, true(( reddish(Centre), blueish(Corner) ))) :-
    new(G, gradient(radial, point(0.5,0.5), point(0.5,0.5), 0, 0.5,
                    @default, @default, bounding_box)),
    send(G, add_stop, 0, colour(red)),
    send(G, add_stop, 1, colour(blue)),
    filled_box(G, B),
    pixel_after_drawing(B, 20, 10, 10, Centre),
    pixel_after_drawing(B, 20, 1, 1, Corner).
test(figure_background, true(( reddish(Top), blueish(Bottom) ))) :-
    top_down_gradient(G),
    new(F, figure),
    send(F, background, G),
    send(F, display, new(B, box(20, 20)), point(10, 10)),
    send(B, pen, 0),
    send(B, fill, @nil),
    new(D, device),
    send(D, display, F),
    pixel_after_drawing(D, 40, 15, 11, Top),
    pixel_after_drawing(D, 40, 15, 28, Bottom).

:- end_tests(gradient_units).

%   A graphical that draws a red fill over its 40x40 area after
%   executing a list of clip and unclip steps.

:- pce_begin_class(tcr_clipper, graphical,
                   "Fill after clipping").

variable(steps, prolog, both, "Steps before the fill").

initialise(G, Steps:prolog) :->
    send_super(G, initialise, 0, 0, 40, 40),
    send(G, steps, Steps).

'_redraw_area'(G, _A:area) :->
    get(G, steps, Steps),
    forall(member(Step, Steps), step(G, Step)),
    send(G, draw_fill, 0, 0, 40, 40, colour(red)).

step(G, clip(X,Y,W,H)) :-
    send(G, clip, area(X,Y,W,H)).
step(G, unclip) :-
    send(G, unclip).

:- pce_end_class(tcr_clipper).

clipped_pixels(Steps, Points, Pixels) :-
    new(G, tcr_clipper(Steps)),
    findall(P, ( member(X-Y, Points),
                 pixel_after_drawing(G, 40, X, Y, P)
               ), Pixels).

:- begin_tests(clip).

test(nested_clips_intersect, Pixels == [R, W, W]) :-
    R = 0xffff0000, W = 0xffffffff,
    clipped_pixels([clip(0,0,20,40), clip(0,0,40,20)],
                   [10-10, 30-10, 10-30], Pixels).
test(unclip_restores_the_outer_clip, Pixels == [R, W]) :-
    R = 0xffff0000, W = 0xffffffff,
    clipped_pixels([clip(0,0,20,40), clip(0,0,40,20), unclip],
                   [10-30, 30-30], Pixels).
test(forgotten_unclip_ends_with_graphical, RGBA == 0xffff0000) :-
    new(D, device),
    send(D, display, tcr_clipper([clip(0,0,5,5)])),
    send(D, display, new(B, box(20, 20)), point(20, 20)),
    send(B, fill, colour(red)),
    pixel_after_drawing(D, 40, 30, 30, RGBA).

test(figure_clip_path, [Corner, Middle] == [W, R]) :-
    R = 0xffff0000, W = 0xffffffff,
    red_figure(F),
    new(P, path),
    send(P, svg_data, "M 0 20 A 20 20 0 1 1 40 20 A 20 20 0 1 1 0 20 Z"),
    send(F, clip_path, P),
    pixel_after_drawing(F, 40, 2, 2, Corner),
    pixel_after_drawing(F, 40, 20, 20, Middle).
test(figure_clip_path_is_transformed, [In, Out] == [R, W]) :-
    R = 0xffff0000, W = 0xffffffff,
    red_figure(F),
    new(P, path),
    send(P, svg_data, "M 0 0 H 10 V 40 H 0 Z"),
    send(F, clip_path, P),
    send(F, translate, 20, 0),          % clip is now x in 20..30
    new(D, device),
    send(D, display, F),
    pixel_after_drawing(D, 60, 25, 10, In),
    pixel_after_drawing(D, 60, 35, 10, Out).
test(figure_clip_path_even_odd, Pixel == W) :-
    W = 0xffffffff,
    red_figure(F),
    new(P, path),
    send(P, svg_data, "M 0 0 H 40 V 40 H 0 Z M 10 10 H 30 V 30 H 10 Z"),
    send(P, fill_rule, even_odd),
    send(F, clip_path, P),
    pixel_after_drawing(F, 40, 20, 20, Pixel).

:- end_tests(clip).

%   A figure holding a red box of 40x40 pixels.

red_figure(F) :-
    new(F, figure),
    send(F, display, new(B, box(40, 40))),
    send(B, pen, 0),
    send(B, fill, colour(red)).

%   Pixel of a cyan figure blended with Blend over a yellow box.

blended_pixel(Blend, RGBA) :-
    new(D, device),
    send(D, display, new(Y, box(20, 20))),
    send(Y, pen, 0),
    send(Y, fill, colour(@default, 255, 255, 0)),
    new(F, figure),
    send(F, display, new(C, box(20, 20))),
    send(C, pen, 0),
    send(C, fill, colour(@default, 0, 255, 255)),
    send(F, blend, Blend),
    send(D, display, F),
    pixel_after_drawing(D, 20, 10, 10, RGBA).

:- begin_tests(blend).

test(normal, RGBA == 0xff00ffff) :-
    blended_pixel(normal, RGBA).
test(multiply, RGBA == 0xff00ff00) :-
    blended_pixel(multiply, RGBA).
test(difference, RGBA == 0xffff00ff) :-
    blended_pixel(difference, RGBA).

:- end_tests(blend).

:- begin_tests(export).

test(svg, [true(sub_string(Head, _, _, _, "<svg")), W == 40]) :-
    tmp_file_stream(text, File, Out), close(Out),
    filled_box(colour(red), B),
    send(B, size, size(40, 20)),
    send(B, svg, File),
    read_file_to_string(File, Data, []),
    delete_file(File),
    sub_string(Data, 0, 200, _, Head),
    once(sub_string(Data, Pos, _, _, "width=\"")),
    Start is Pos+7,
    sub_string(Data, Start, 2, _, WS),
    number_string(W, WS).
test(svg_cannot_write, fail) :-
    filled_box(colour(red), B),
    catch(send(B, svg, '/nonexistent/dir/out.svg'), _, fail).

:- end_tests(export).
