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

:- module(test_event_transform, [test_event_transform/0]).
:- encoding(utf8).

/** <module> Events on transformed figures

Verifies that `<-position` returns the correct local coordinates when
the event passes through a figure carrying a non-identity transform.
We use a property-based check: if (Lx,Ly) is some local point in a
graphical, then mapping forward via <-window_point and creating an
event at the resulting window-coord must map back to (Lx,Ly) via
<-position.

Run with:

    swipl -g test_event_transform -t halt \
          packages/xpce/tests/test_event_transform.pl
*/

:- use_module(library(pce)).
:- use_module(library(plunit)).

setup_headless :-
    set_prolog_flag('SDL_VIDEODRIVER', dummy).
:- initialization(setup_headless, now).

test_event_transform :-
    run_tests([ event_transform_position,
                event_transform_round_trip
              ]).

%!  point_eq(+P, +X, +Y) is semidet.

point_eq(P, X, Y) :-
    get(P, x, PX), PX =:= X,
    get(P, y, PY), PY =:= Y.

%!  near_point(+P, +X, +Y) is semidet.
%
%   Tolerant version: allows off-by-one rounding from float<->int.

near_point(P, X, Y) :-
    get(P, x, PX), abs(PX - X) =< 1,
    get(P, y, PY), abs(PY - Y) =< 1.


%!  make_event(+Window, +X, +Y, -Event) is det.
%
%   Build a synthetic event at window-coord (X, Y).

make_event(W, X, Y, E) :-
    new(E, event(loc_move, W, X, Y)).


:- begin_tests(event_transform_position).

%   Without a transform: an event at the box's window position should
%   produce box-local (0, 0).
test(no_transform_origin) :-
    new(P, picture('', size(400, 400))),
    new(B, box(50, 50)),
    send(P, display, B, point(40, 60)),
    send(P, open),
    make_event(P, 40, 60, E),
    get(E, position, B, Loc),
    point_eq(Loc, 0, 0).

%   With a 2x scaled figure: an event at (window position of box origin)
%   still maps to box-local (0, 0); an event offset by 20 px in window
%   maps to box-local (10, 10) because of the 2x scale.
test(scale_2x_halves_window_offset) :-
    new(P, picture('', size(400, 400))),
    new(F, figure),
    new(B, box(80, 80)),
    send(F, display, B, point(0, 0)),
    send(P, display, F, point(50, 50)),
    new(T, transform), send(T, scale, 2),
    send(F, transform, T),
    send(P, open),
    %   With offset (50, 50), event at window (50, 50) is box-local 0,0.
    make_event(P, 50, 50, E0),
    get(E0, position, B, L0),
    point_eq(L0, 0, 0),
    %   Window (70, 70): F-children = (20, 20) post-translate; inverse
    %   of scale-2 gives (10, 10) in pre-transform F-children-coord,
    %   which is box-local (10, 10).
    make_event(P, 70, 70, E1),
    get(E1, position, B, L1),
    point_eq(L1, 10, 10).

test(translate_only_falls_through_integer_path) :-
    new(P, picture('', size(400, 400))),
    new(F, figure),
    new(B, box(40, 40)),
    send(F, display, B, point(0, 0)),
    send(P, display, F, point(100, 100)),
    new(T, transform), send(T, translate, 30, 40),
    send(F, transform, T),
    send(P, open),
    %   F displayed at (100,100) plus T's translate(30,40) on contents.
    %   Box-local (0,0) lives at window-coord (100+30, 100+40) = (130, 140).
    make_event(P, 130, 140, E),
    get(E, position, B, L),
    point_eq(L, 0, 0).

:- end_tests(event_transform_position).


:- begin_tests(event_transform_round_trip).

%   Property: for any local point in B, mapping forward via
%   <-window_point and creating an event there yields back the same
%   local point via <-position.

round_trip(P, B, Lx, Ly) :-
    get(B, window_point, point(Lx, Ly), Wpt),
    get(Wpt, x, WX), get(Wpt, y, WY),
    make_event(P, WX, WY, E),
    get(E, position, B, Back),
    near_point(Back, Lx, Ly).

test(rotate_47_round_trip) :-
    new(P, picture('', size(400, 400))),
    new(F, figure),
    new(B, box(80, 60)),
    send(F, display, B, point(10, 20)),
    send(P, display, F, point(150, 150)),
    new(T, transform), send(T, rotate, 47),
    send(F, transform, T),
    send(P, open),
    forall(member(Lx-Ly, [0-0, 5-7, 30-25, -10-15]),
           round_trip(P, B, Lx, Ly)).

test(scale_non_uniform_round_trip) :-
    new(P, picture('', size(400, 400))),
    new(F, figure),
    new(B, box(60, 40)),
    send(F, display, B, point(0, 0)),
    send(P, display, F, point(80, 90)),
    new(T, transform), send(T, scale, 3, 2),
    send(F, transform, T),
    send(P, open),
    forall(member(Lx-Ly, [0-0, 4-6, 15-10]),
           round_trip(P, B, Lx, Ly)).

test(nested_transforms_round_trip) :-
    new(P, picture('', size(400, 400))),
    new(Outer, figure),
    new(Inner, figure),
    new(B, box(40, 40)),
    send(Inner, display, B, point(0, 0)),
    send(Outer, display, Inner, point(20, 30)),
    send(P, display, Outer, point(120, 130)),
    new(To, transform), send(To, scale, 2),
    send(Outer, transform, To),
    new(Ti, transform), send(Ti, rotate, 30),
    send(Inner, transform, Ti),
    send(P, open),
    forall(member(Lx-Ly, [0-0, 5-7, 10-15]),
           round_trip(P, B, Lx, Ly)).

%   When no transform is in the chain, the result must be byte-exact
%   (not just within tolerance) — confirms we hit the integer path.
test(no_transform_is_exact_integer) :-
    new(P, picture('', size(400, 400))),
    new(B, box(40, 40)),
    send(P, display, B, point(75, 85)),
    send(P, open),
    forall(member(Lx-Ly, [0-0, 3-5, 17-9, -2-4]),
           ( get(B, window_point, point(Lx, Ly), Wpt),
             get(Wpt, x, WX), get(Wpt, y, WY),
             make_event(P, WX, WY, E),
             get(E, position, B, Back),
             point_eq(Back, Lx, Ly) )).

:- end_tests(event_transform_round_trip).
