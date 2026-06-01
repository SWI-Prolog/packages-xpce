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

:- module(test_transform_misc, [test_transform_misc/0]).
:- encoding(utf8).

/** <module> Phase 8: getAbsoluteArea, handles, connections under transform

Verifies that callers of offsetDeviceGraphical (absolute area, handle
positions, connection compute) honor figure->transform along the
device chain.

Run with:

    swipl -g test_transform_misc -t halt \
          packages/xpce/tests/test_transform_misc.pl
*/

:- use_module(library(pce)).
:- use_module(library(plunit)).

setup_headless :-
    set_prolog_flag('SDL_VIDEODRIVER', dummy).
:- initialization(setup_headless, now).

test_transform_misc :-
    run_tests([ absolute_area,
                handle_positions,
                handle_under_transform
              ]).


:- begin_tests(absolute_area).

%   Without a transform, getAbsoluteArea(B) inside figure F at (50, 60)
%   has the same width/height as the child but is shifted by F's
%   offset.

test(no_transform_just_shifts) :-
    new(W, picture('', size(400, 400))),
    new(F, figure),
    new(B, box(50, 30)),
    send(F, display, B, point(0, 0)),
    send(W, display, F, point(60, 90)),
    send(W, compute),
    get(B, absolute_area, W, A),
    get(A, x, 60), get(A, y, 90),
    get(A, width, 50), get(A, height, 30).

test(rotate_90_swaps_width_height) :-
    new(W, picture('', size(400, 400))),
    new(F, figure),
    new(B, box(100, 60)),
    send(F, display, B, point(0, 0)),
    send(W, display, F, point(200, 200)),
    new(T, transform), send(T, rotate, 90),
    send(F, transform, T),
    send(W, compute),
    get(B, absolute_area, W, A),
    %   Box-local (0,0,100,60) rotated 90 → AABB (-60,0,60,100).
    %   F displayed at (200,200) → F->offset adjusted such that AABB
    %   sits at (200,200,60,100).
    get(A, width, 60), get(A, height, 100).

test(scale_2x_doubles_area) :-
    new(W, picture('', size(400, 400))),
    new(F, figure),
    new(B, box(50, 30)),
    send(F, display, B, point(0, 0)),
    send(W, display, F, point(100, 100)),
    new(T, transform), send(T, scale, 2),
    send(F, transform, T),
    send(W, compute),
    get(B, absolute_area, W, A),
    get(A, width, 100), get(A, height, 60).

:- end_tests(absolute_area).


:- begin_tests(handle_positions).

%   handle attached at center of box, rotated 90° figure.  The handle's
%   position in window-coord must be the rotated center, not the
%   un-rotated center.

test(handle_center_under_rotate) :-
    new(W, picture('', size(500, 500))),
    new(F, figure),
    new(B, box(100, 60)),
    %   Attach a handle at (50, 30) — the center of the box — named 'c'.
    send(B, handle, handle(50, 30, link, c)),
    send(F, display, B, point(0, 0)),
    send(W, display, F, point(200, 200)),
    new(T, transform), send(T, rotate, 90),
    send(F, transform, T),
    send(W, compute),
    %   Get handle position in window-coord.
    get(B, handle, c, H),
    get(H, position, B, W, P),
    %   Without rotation, handle would be at (200+50, 200+30) = (250, 230).
    %   With +90° rotation: box's local center (50, 30) maps under M to
    %   (-30, 50), then + F->offset (which shifted to keep AABB at 200,200)
    %   gives the actual handle window position.
    %
    %   We don't compute the exact expected value here (it depends on
    %   F->offset adjustment).  Instead, verify it matches what
    %   <-window_point reports for the same local point.
    get(B, window_point, point(50, 30), Wp),
    get(Wp, x, WX), get(Wp, y, WY),
    get(P, x, PX), get(P, y, PY),
    abs(WX - PX) =< 1, abs(WY - PY) =< 1.

test(handle_position_unchanged_without_transform) :-
    new(W, picture('', size(500, 500))),
    new(B, box(80, 40)),
    send(B, handle, handle(40, 20, link, c)),
    send(W, display, B, point(100, 100)),
    send(W, compute),
    get(B, handle, c, H),
    get(H, position, B, W, P),
    get(P, x, PX), PX =:= 140,
    get(P, y, PY), PY =:= 120.

:- end_tests(handle_positions).


:- begin_tests(handle_under_transform).

%   When a figure's transform changes, the handle positions of its
%   children (as queried via <-position) must change to reflect the
%   new transformation.  This is the property connections rely on.

test(handle_position_changes_with_rotation) :-
    new(W, picture('', size(500, 500))),
    new(F, figure),
    new(B1, box(40, 40)),
    send(B1, handle, handle(40, 20, link, east)),
    send(F, display, B1, point(0, 0)),
    send(W, display, F, point(100, 100)),
    send(W, compute),
    get(B1, handle, east, H),
    get(H, position, B1, W, P0),
    get(P0, x, X0), get(P0, y, Y0),

    new(T, transform), send(T, rotate, 90),
    send(F, transform, T),
    send(W, compute),
    get(H, position, B1, W, P1),
    get(P1, x, X1), get(P1, y, Y1),

    %   Handle position must have moved as a result of the rotation.
    once(( X0 =\= X1 ; Y0 =\= Y1 )).

test(handle_position_via_window_point_matches) :-
    %   Handle's <-position equals what <-window_point gives for the
    %   handle's local coordinate.
    new(W, picture('', size(500, 500))),
    new(F, figure),
    new(B, box(80, 60)),
    send(B, handle, handle(40, 30, link, mid)),
    send(F, display, B, point(0, 0)),
    send(W, display, F, point(150, 150)),
    new(T, transform), send(T, rotate, 25),
    send(F, transform, T),
    send(W, compute),
    get(B, handle, mid, H),
    get(H, position, B, W, P),
    get(B, window_point, point(40, 30), Wp),
    get(P, x, PX), get(P, y, PY),
    get(Wp, x, WX), get(Wp, y, WY),
    abs(PX - WX) =< 1, abs(PY - WY) =< 1.

:- end_tests(handle_under_transform).
