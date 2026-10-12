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

:- module(test_path_segments, [test_path_segments/0]).

/** <module> Tests for curve, arc and move points in class path

Run with:

    swipl -g test_path_segments -t halt \
          packages/xpce/tests/test_path_segments.pl
*/

:- set_prolog_flag('SDL_VIDEODRIVER', dummy).

:- use_module(library(pce)).
:- use_module(library(plunit)).

test_path_segments :-
    run_tests([ path_segments
              ]).

new_path(Points, P) :-
    new(P, path),
    forall(member(Pt, Points), send(P, append, Pt)).

area_term(G, area(X,Y,W,H)) :-
    get(G, area, A),
    get(A, x, X), get(A, y, Y), get(A, width, W), get(A, height, H).

xy(Pt, X-Y) :-
    get(Pt, x, X),
    get(Pt, y, Y).

%!  pixel(+Graphical, +Size, +X, +Y, -RGBA) is det.

pixel(Gr, Size, X, Y, RGBA) :-
    new(I, image(@nil, Size, Size)),
    send(I, draw_in, Gr),
    get(I, pixel, X, Y, C),
    get(C, rgba, RGBA),
    free(I).

red(0xffff0000).
white(0xffffffff).

filled(P) :-
    send(P, closed, @on),
    send(P, fill, colour(red)),
    send(P, pen, 0).

%   Two squares, the second inside the first and drawn the same way
%   round, so the middle is inside by the winding rule but not by the
%   even-odd rule.

nested_squares(P) :-
    new_path([ point(0,0), point(40,0), point(40,40), point(0,40),
               move_point(10,10), point(30,10), point(30,30), point(10,30)
             ], P),
    filled(P).

square(P) :-
    new_path([point(0,0), point(40,0), point(40,40), point(0,40)], P),
    filled(P).

%   A quadratic curve from (0,10) to (40,10) whose top is at (20,0).

hump(P) :-
    new_path([point(0,10), curve_point(40,10, point(20,-10))], P).

:- begin_tests(path_segments).

test(curve_area_covers_the_curve, true(Y =< 0)) :-
    hump(P),
    area_term(P, area(_,Y,_,_)).
test(arc_area_covers_the_arc, true(Y =< 0)) :-
    new_path([point(0,20), arc_point(40,20, 20)], P),
    area_term(P, area(_,Y,_,_)).
test(arc_sweeps_the_other_way, true(Y >= 20)) :-
    new_path([point(0,20), arc_point(40,20, 20, @default, @default,
                                     @off, @off)], P),
    area_term(P, area(_,Y,_,_)).
test(winding_rule_fills_the_hole, RGBA == Red) :-
    red(Red),
    nested_squares(P),
    pixel(P, 40, 20, 20, RGBA).
test(even_odd_rule_leaves_the_hole, RGBA == White) :-
    white(White),
    nested_squares(P),
    send(P, fill_rule, even_odd),
    pixel(P, 40, 20, 20, RGBA).
test(square_corner, RGBA == Red) :-
    red(Red),
    square(P),
    pixel(P, 40, 1, 1, RGBA).
test(radius_rounds_the_corners, [Corners, Middle] == [[W,W,W,W], Red]) :-
    red(Red), white(W),
    square(P),
    send(P, radius, 10),
    findall(C, ( member(X-Y, [1-1, 38-1, 38-38, 1-38]),
                 pixel(P, 40, X, Y, C)
               ), Corners),
    pixel(P, 40, 20, 20, Middle).
test(inside_curve, true) :-
    hump(P),
    send(P, inside, 20, 8).
test(not_inside_curve, fail) :-
    hump(P),
    send(P, inside, 20, 12).
test(inside_even_odd_hole, fail) :-
    nested_squares(P),
    send(P, fill_rule, even_odd),
    send(P, inside, 20, 20).
test(distance_to_curve, D == 22) :-          % nearest point is (30,20)
    new_path([point(0,0), curve_point(40,0, point(40,40))], P),
    get(P, distance, point(40,40), D).
test(arrow_follows_tangent, [Tip == 40-0, RX == 40, true(RY > 0)]) :-
    new_path([point(0,0), curve_point(40,0, point(40,40))], P),
    send(P, arrows, second),
    send(P, compute),
    get(P, second_arrow, A),
    get(A, tip, T), xy(T, Tip),
    get(A, reference, R), xy(R, RX0-RY),
    RX is round(RX0).
test(resize_moves_control_points, [End == 80-10, Control == 40-(-10)]) :-
    hump(P),
    send(P, resize, 2, 1, point(0,0)),
    get(P?points, tail, CP),
    xy(CP, End),
    get(CP, control1, C), xy(C, Control).
test(relative_move_moves_control_points, Control == 25-(-5)) :-
    hump(P),
    send(P, relative_move, point(5,5), points),
    get(P?points, tail, CP),
    get(CP, control1, C), xy(C, Control).
test(resize_scales_arc, [RX-RY == 40-10]) :-
    new_path([point(0,20), arc_point(40,20, 20)], P),
    send(P, resize, 2, 0.5, point(0,0)),
    get(P?points, tail, AP),
    get(AP, rx, RX0), get(AP, ry, RY0),
    RX is round(RX0), RY is round(RY0).
test(mirror_reverses_sweep, Sweep == @off) :-
    new_path([point(0,20), arc_point(40,20, 20)], P),
    send(P, resize, -1, 1, point(20,0)),
    get(P?points, tail, AP),
    get(AP, sweep, Sweep).
test(closed_polygon_inside, true) :-
    square(P),
    send(P, inside, 20, 20).

test(svg_data_absolute, S == "M 10 10 L 20 20 L 30 20 L 30 15 Z") :-
    svg_round_trip("M10 10 L 20 20 h10 v-5 z", S).
test(svg_data_reflect_cubic,
     S == "M 0 0 C 10 0 20 10 20 20 C 20 30 30 40 40 40") :-
    svg_round_trip("M0,0C10,0 20,10 20,20S30,40 40,40", S).
test(svg_data_reflect_quadratic,
     S == "M 0 0 Q 10 -10 20 0 Q 30 10 40 0") :-
    svg_round_trip("m0 0q10-10 20 0t20 0", S).
test(svg_data_arcs_and_subpaths,
     S == "M 10 10 A 25 25 -30 0 1 60 -15 A 10 10 0 1 0 50 50 Z \
M 15 15 L 16 16") :-
    svg_round_trip("M10,10 a25,25 -30 0,1 50,-25 A10 10 0 1050 50Z \
m 5 5 l1 1", S).
test(svg_data_compact_numbers, S == "M 10 0.5 L 0.5 -1") :-
    svg_round_trip("M1e1.5.5-1", S).
test(svg_data_must_start_with_moveto, fail) :-
    new(P, path),
    send(P, svg_data, "L 10 10").
test(svg_data_syntax_error, fail) :-
    new(P, path),
    send(P, svg_data, "M 10").
test(svg_data_closed_subpaths, Closed == [@on, @nil]) :-
    new(P, path),
    send(P, svg_data, "M0 0 L10 0 L10 10 Z M20 20 L30 20 L30 30 Z"),
    get(P, closed, C1),
    get(P?points, find, message(@arg1, instance_of, move_point), MP),
    get(MP, closed, C2),
    Closed = [C1, C2].

:- end_tests(path_segments).

svg_round_trip(In, Out) :-
    new(P, path),
    send(P, svg_data, In),
    get(P, svg_data, S),
    get(S, value, Atom),
    atom_string(Atom, Out).
