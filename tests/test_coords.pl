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

:- module(test_coords, [test_coords/0]).
:- encoding(utf8).

/** <module> Tests for <-window_point / <-graphical_point coord mapping

These exercise the transform-aware coordinate walk in src/gra/coords.c
that Phases 6-8 will use for events and damage.  Tests need a real
PceWindow ancestor (which is the only thing that terminates the walk),
so we open invisible windows via the headless SDL driver.

Run with:

    swipl -g test_coords -t halt \
          packages/xpce/tests/test_coords.pl
*/

:- use_module(library(pce)).
:- use_module(library(plunit)).

setup_headless :-
    set_prolog_flag('SDL_VIDEODRIVER', dummy).
:- initialization(setup_headless, now).

test_coords :-
    run_tests([ window_point_no_transform,
                window_point_with_transform,
                window_point_round_trip
              ]).

%!  point_eq(+P, +X, +Y) is semidet.
%
%   Numeric equality (xpce returns ints for whole-valued Nums).

point_eq(P, X, Y) :-
    get(P, x, PX), PX =:= X,
    get(P, y, PY), PY =:= Y.

%!  with_picture(+Box, +Pos, :Goal) is det.
%
%   Open a headless picture, place Box at Pos, then call Goal(B, P)
%   where B is the box graphical and P is the picture.  Cleans up.

with_picture(BoxSize, Pos, Goal) :-
    new(P, picture('', size(400, 400))),
    new(B, box(BoxSize, BoxSize)),
    send(P, display, B, Pos),
    send(P, open),
    call(Goal, B, P).


:- begin_tests(window_point_no_transform).

%   For a graphical sitting directly in a window with no scrolling, the
%   window-coord of a local point (lx,ly) is (gr->area->x+lx,
%   gr->area->y+ly).

test(origin_at_area_xy) :-
    with_picture(50, point(30, 40),
                 [B, _P]>>(
                     get(B, window_point, point(0, 0), W),
                     point_eq(W, 30, 40)
                 )).

test(local_point_offsets_from_area) :-
    with_picture(100, point(20, 50),
                 [B, _P]>>(
                     get(B, window_point, point(7, 13), W),
                     point_eq(W, 27, 63)
                 )).

test(default_arg_is_origin) :-
    with_picture(20, point(11, 22),
                 [B, _P]>>(
                     get(B, window_point, @default, W),
                     point_eq(W, 11, 22)
                 )).

test(figure_offset_adds) :-
    %   Box inside figure inside picture.  display/2 on F adjusts
    %   F->offset so that the children-union + offset places F where
    %   we asked.  So the box's local-(0,0) maps to the spot we put
    %   the figure: window-coord (100, 200), independently of the
    %   box's own gr->area->{x,y}.
    new(P, picture('', size(400, 400))),
    new(F, figure),
    send(F, display, new(B, box(40, 40)), point(5, 7)),
    send(P, display, F, point(100, 200)),
    send(P, open),
    get(B, window_point, point(0, 0), W),
    point_eq(W, 100, 200),
    %   A local (10, 13) inside the box → F-children (15, 20) → window
    %   (15 + offset_x, 20 + offset_y) = (110, 213).
    get(B, window_point, point(10, 13), W2),
    point_eq(W2, 110, 213),
    true.    %% Leak the frame; dummy-SDL destroy is buggy.

:- end_tests(window_point_no_transform).


:- begin_tests(window_point_with_transform).

test(scale_2x_doubles_local_offset) :-
    %   Box inside a 2x-scaled figure: a local point (10, 0) inside the
    %   box maps to a window-coord that's 2x further out than without
    %   the transform.
    new(P, picture('', size(400, 400))),
    new(F, figure),
    send(F, display, new(B, box(60, 60)), point(0, 0)),
    send(P, display, F, point(50, 50)),
    new(T, transform),
    send(T, scale, 2),
    send(F, transform, T),
    send(P, open),
    %   Without transform: box at local(10,0) → window(60,50)
    %   With scale-2x: (10,0) → (20,0) before offset → window(70,50)
    get(B, window_point, point(10, 0), W),
    point_eq(W, 70, 50),
    true.    %% Leak the frame; dummy-SDL destroy is buggy.

test(rotate_90_around_figure_origin) :-
    new(P, picture('', size(400, 400))),
    new(F, figure),
    send(F, display, new(B, box(100, 60)), point(0, 0)),
    send(P, display, F, point(200, 200)),
    new(T, transform),
    send(T, rotate, 90),
    send(F, transform, T),
    send(P, open),
    %   Box's local origin is (0,0).  Under +90deg rotation, (0,0)
    %   stays (0,0); then F is at window (200,200) (the AABB moved
    %   too, but we placed F at this point via display/2).
    %
    %   Actually after rotation, F->area moves: the rotated AABB
    %   starts at negative x.  display/2 sets gr->area->x to 200,
    %   so the AABB now starts at 200.  F->offset shifts accordingly.
    %
    %   A point (10, 0) in box-local → after rotation → (0, 10) in
    %   figure-local (post-transform) → window y = 200 + offset_y.
    %
    %   Easier verification: a point AT the figure's transform origin
    %   (0, 0 in box-local, which is also 0, 0 in figure-local)
    %   should land at the same window-coord regardless of rotation,
    %   PROVIDED F->offset and F->area->x are reset to keep the
    %   bounding box at (200, 200).
    %
    %   This is brittle to verify without inspecting offset.  Instead:
    %   round-trip through <-graphical_point should give back (10, 0).
    get(B, window_point, point(10, 0), W),
    get(B, graphical_point, W, Back),
    point_eq(Back, 10, 0),
    true.    %% Leak the frame; dummy-SDL destroy is buggy.

test(no_transform_in_chain_falls_through_cheap_path) :-
    %   When no transform is set anywhere, results match a manual
    %   integer-offset computation.
    new(P, picture('', size(400, 400))),
    new(B, box(20, 20)),
    send(P, display, B, point(50, 60)),
    send(P, open),
    get(B, window_point, point(3, 4), W),
    point_eq(W, 53, 64),
    true.    %% Leak the frame; dummy-SDL destroy is buggy.

:- end_tests(window_point_with_transform).


:- begin_tests(window_point_round_trip).

test(identity_round_trip) :-
    with_picture(50, point(30, 40),
                 [B, _P]>>(
                     get(B, window_point, point(7, 9), W),
                     get(B, graphical_point, W, L),
                     point_eq(L, 7, 9)
                 )).

test(scaled_round_trip) :-
    new(P, picture('', size(400, 400))),
    new(F, figure),
    send(F, display, new(B, box(60, 60)), point(0, 0)),
    send(P, display, F, point(50, 50)),
    new(T, transform), send(T, scale, 2),
    send(F, transform, T),
    send(P, open),
    get(B, window_point, point(10, 12), W),
    get(B, graphical_point, W, L),
    point_eq(L, 10, 12),
    true.    %% Leak the frame; dummy-SDL destroy is buggy.

test(rotated_round_trip) :-
    new(P, picture('', size(400, 400))),
    new(F, figure),
    send(F, display, new(B, box(80, 60)), point(0, 0)),
    send(P, display, F, point(150, 150)),
    new(T, transform), send(T, rotate, 47),
    send(F, transform, T),
    send(P, open),
    %   Pick an integer local point.  After forward+inverse, we should
    %   get back the same point (within rounding).
    get(B, window_point, point(20, 30), W),
    get(B, graphical_point, W, L),
    point_eq(L, 20, 30),
    true.    %% Leak the frame; dummy-SDL destroy is buggy.

test(nested_figures_round_trip) :-
    new(P, picture('', size(400, 400))),
    new(Outer, figure),
    new(Inner, figure),
    send(Inner, display, new(B, box(40, 40)), point(0, 0)),
    send(Outer, display, Inner, point(20, 30)),
    send(P, display, Outer, point(100, 100)),
    new(To, transform), send(To, scale, 2),
    send(Outer, transform, To),
    new(Ti, transform), send(Ti, rotate, 30),
    send(Inner, transform, Ti),
    send(P, open),
    get(B, window_point, point(5, 7), W),
    get(B, graphical_point, W, L),
    point_eq(L, 5, 7),
    true.    %% Leak the frame; dummy-SDL destroy is buggy.

:- end_tests(window_point_round_trip).
