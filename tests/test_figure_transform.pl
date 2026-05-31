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

:- module(test_figure_transform, [test_figure_transform/0]).
:- encoding(utf8).

/** <module> Tests for figure->transform and its bounding box semantics

Run with:

    swipl -g test_figure_transform -t halt \
          packages/xpce/tests/test_figure_transform.pl
*/

:- use_module(library(pce)).
:- use_module(library(plunit)).

setup_headless :-
    set_prolog_flag('SDL_VIDEODRIVER', dummy).
:- initialization(setup_headless, now).

test_figure_transform :-
    run_tests([ figure_transform_slot,
                figure_transform_bbox,
                figure_transform_recompute
              ]).

%!  area_eq(+Area, +X, +Y, +W, +H) is semidet.
%
%   Succeed if Area has the given x/y/width/height.  Uses numeric
%   comparison because xpce returns integers as Prolog ints.

area_eq(A, X, Y, W, H) :-
    get(A, x, AX), AX =:= X,
    get(A, y, AY), AY =:= Y,
    get(A, width,  AW), AW =:= W,
    get(A, height, AH), AH =:= H.

%!  figure_with_box(-F, +X, +Y, +W, +H) is det.
%
%   Make a figure containing a single box(W,H) at point(X,Y).

figure_with_box(F, X, Y, W, H) :-
    new(F, figure),
    send(F, display, new(_, box(W, H)), point(X, Y)).


:- begin_tests(figure_transform_slot).

test(default_transform_is_nil) :-
    new(F, figure),
    get(F, transform, X), X == @nil,
    free(F).

test(set_and_get) :-
    new(F, figure),
    new(T, transform),
    send(T, scale, 2),
    send(F, transform, T),
    get(F, transform, T2), T2 == T,
    free(F).

test(set_nil_clears) :-
    new(F, figure),
    new(T, transform),
    send(F, transform, T),
    send(F, transform, @nil),
    get(F, transform, @nil),
    free(F).

test(default_local_area_exists) :-
    %% A fresh figure has a local_area object even before compute.
    new(F, figure),
    get(F, local_area, A),
    send(A, instance_of, area),
    free(F).

:- end_tests(figure_transform_slot).


:- begin_tests(figure_transform_bbox).

test(no_transform_local_area_matches_area) :-
    figure_with_box(F, 10, 10, 100, 60),
    send(F, compute),
    get(F, area, A), get(F, local_area, L),
    get(A, x, X),  get(L, x, X),
    get(A, y, Y),  get(L, y, Y),
    get(A, width, W),  get(L, width,  W),
    get(A, height, H), get(L, height, H),
    free(F).

test(identity_transform_does_not_change_area) :-
    %% Identity transform takes the fast path: area must match the
    %% no-transform case exactly.
    figure_with_box(F, 10, 20, 100, 60),
    send(F, transform, new(transform)),
    send(F, compute),
    area_eq(F?area, 10, 20, 100, 60),
    free(F).

test(rotate_90_swaps_dimensions) :-
    %% Corners (10,20),(110,20),(110,80),(10,80) under +90° rotation
    %% around the origin: (-20,10),(-20,110),(-80,110),(-80,10).
    %% AABB: x∈[-80,-20], y∈[10,110] → (-80, 10, 60, 100).
    figure_with_box(F, 10, 20, 100, 60),
    new(T, transform), send(T, rotate, 90),
    send(F, transform, T),
    send(F, compute),
    area_eq(F?area, -80, 10, 60, 100),
    %% local_area remains the un-transformed children union.
    area_eq(F?local_area, 10, 20, 100, 60),
    free(F).

test(rotate_180_negates_position) :-
    figure_with_box(F, 10, 20, 100, 60),
    new(T, transform), send(T, rotate, 180),
    send(F, transform, T),
    send(F, compute),
    %% Rotation by 180° around origin negates all coords.
    area_eq(F?area, -110, -80, 100, 60),
    free(F).

test(rotate_270_swaps_with_negation) :-
    figure_with_box(F, 10, 20, 100, 60),
    new(T, transform), send(T, rotate, 270),
    send(F, transform, T),
    send(F, compute),
    %% At 270° (= -90°): (10,20)→(20,-10),(110,20)→(20,-110),
    %% (110,80)→(80,-110),(10,80)→(80,-10) → (20,-110,60,100).
    area_eq(F?area, 20, -110, 60, 100),
    free(F).

test(scale_2x_doubles_area) :-
    figure_with_box(F, 20, 40, 50, 30),
    new(T, transform), send(T, scale, 2),
    send(F, transform, T),
    send(F, compute),
    area_eq(F?area, 40, 80, 100, 60),
    free(F).

test(scale_non_uniform) :-
    figure_with_box(F, 10, 10, 50, 30),
    new(T, transform), send(T, scale, 2, 3),
    send(F, transform, T),
    send(F, compute),
    area_eq(F?area, 20, 30, 100, 90),
    free(F).

test(translate_only) :-
    figure_with_box(F, 10, 20, 100, 60),
    new(T, transform), send(T, translate, 50, 70),
    send(F, transform, T),
    send(F, compute),
    %% Pure translation shifts the area by (50,70).
    area_eq(F?area, 60, 90, 100, 60),
    free(F).

test(empty_figure_has_zero_area) :-
    %% A figure with no children, with or without a transform, has zero
    %% width/height after compute.
    new(F, figure),
    new(T, transform), send(T, rotate, 30),
    send(F, transform, T),
    send(F, compute),
    get(F?area, width,  0),
    get(F?area, height, 0),
    free(F).

test(multiple_children_union_then_transform) :-
    %% Two boxes: (0,0,40,20) and (60,30,20,40); union = (0,0,80,70).
    %% Rotated 90°: AABB swaps to (?,?,70,80).  Compute corners:
    %% (0,0)→(0,0),(80,0)→(0,80),(80,70)→(-70,80),(0,70)→(-70,0).
    %% AABB: x∈[-70,0], y∈[0,80] → (-70, 0, 70, 80).
    new(F, figure),
    send(F, display, new(_, box(40, 20)), point(0, 0)),
    send(F, display, new(_, box(20, 40)), point(60, 30)),
    new(T, transform), send(T, rotate, 90),
    send(F, transform, T),
    send(F, compute),
    area_eq(F?area, -70, 0, 70, 80),
    area_eq(F?local_area, 0, 0, 80, 70),
    free(F).

test(border_widens_after_transform) :-
    %% Border is added to f->area after transform, so it pads the
    %% rotated AABB rather than the un-rotated children union.
    figure_with_box(F, 10, 20, 100, 60),
    send(F, border, 5),
    new(T, transform), send(T, rotate, 90),
    send(F, transform, T),
    send(F, compute),
    %% Rotated AABB (-80, 10, 60, 100) widened by 5 on each side →
    %% (-85, 5, 70, 110).
    area_eq(F?area, -85, 5, 70, 110),
    free(F).

:- end_tests(figure_transform_bbox).


:- begin_tests(figure_transform_recompute).

test(setting_transform_triggers_recompute) :-
    figure_with_box(F, 10, 20, 100, 60),
    send(F, compute),
    area_eq(F?area, 10, 20, 100, 60),
    new(T, transform), send(T, rotate, 90),
    send(F, transform, T),
    send(F, compute),
    area_eq(F?area, -80, 10, 60, 100),
    free(F).

test(clearing_transform_restores_area) :-
    figure_with_box(F, 10, 20, 100, 60),
    new(T, transform), send(T, rotate, 90),
    send(F, transform, T),
    send(F, compute),
    send(F, transform, @nil),
    send(F, compute),
    area_eq(F?area, 10, 20, 100, 60),
    free(F).

test(replacing_transform_changes_area) :-
    figure_with_box(F, 10, 20, 100, 60),
    new(T1, transform), send(T1, rotate, 90),
    send(F, transform, T1),
    send(F, compute),
    new(T2, transform), send(T2, scale, 2),
    send(F, transform, T2),
    send(F, compute),
    %% (10,20,100,60) * 2 = (20,40,200,120).
    area_eq(F?area, 20, 40, 200, 120),
    free(F).

test(local_area_independent_of_transform_changes) :-
    %% local_area reflects only the un-transformed children union, so
    %% it must NOT change when the transform changes.
    figure_with_box(F, 10, 20, 100, 60),
    new(T, transform), send(T, rotate, 90),
    send(F, transform, T),
    send(F, compute),
    area_eq(F?local_area, 10, 20, 100, 60),
    new(T2, transform), send(T2, scale, 3),
    send(F, transform, T2),
    send(F, compute),
    area_eq(F?local_area, 10, 20, 100, 60),
    free(F).

:- end_tests(figure_transform_recompute).
