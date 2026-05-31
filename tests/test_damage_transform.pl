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

:- module(test_damage_transform, [test_damage_transform/0]).
:- encoding(utf8).

/** <module> Damage propagation through transformed figures

When a child of a transformed figure is changed, the damage rectangle
sent to the enclosing window must be the AABB of the change projected
through the figure's transform — not the un-rotated parent-coord rect.

These tests use a window introspection getter <-changes_area to read
the union of pending damage rects after a controlled mutation.

Run with:

    swipl -g test_damage_transform -t halt \
          packages/xpce/tests/test_damage_transform.pl
*/

:- use_module(library(pce)).
:- use_module(library(plunit)).

setup_headless :-
    set_prolog_flag('SDL_VIDEODRIVER', dummy).
:- initialization(setup_headless, now).

test_damage_transform :-
    run_tests([ damage_no_transform,
                damage_with_transform
              ]).


%!  contains(+Outer, +InnerX, +InnerY, +InnerW, +InnerH) is semidet.
%
%   Succeed if the area Outer fully contains the rect (InnerX, InnerY,
%   InnerW, InnerH).

contains(Outer, IX, IY, IW, IH) :-
    get(Outer, x, OX), get(Outer, y, OY),
    get(Outer, width, OW), get(Outer, height, OH),
    OX =< IX,
    OY =< IY,
    OX + OW >= IX + IW,
    OY + OH >= IY + IH.

%!  flush_damage(+Window) is det.
%
%   Discard pending damage by reading and ignoring it.  We do this by
%   forcing a repaint, but in headless mode we just need to clear the
%   list — the simplest way is to compute the bounding box (which
%   triggers any pending recompute) and call <-changes_area to drain.

setup_window(W, Pos, BoxSize, Transform, B) :-
    new(W, picture('', size(500, 500))),
    new(F, figure),
    new(B, box(BoxSize, BoxSize)),
    send(F, display, B, point(0, 0)),
    send(W, display, F, Pos),
    ( Transform == none -> true ; send(F, transform, Transform) ),
    send(W, open),
    %  Headless SDL leaves <-displayed off; force it on so the
    %  change-propagation walk in changed[Area|Image]Graphical doesn't
    %  short-circuit at the window.
    send(W, displayed, @on),
    send(W, compute).


:- begin_tests(damage_no_transform).

%   Sanity: without any transform, the damage rect for a change inside
%   a figure equals the box's window-coord area.

test(box_change_invalidates_box_area) :-
    setup_window(W, point(100, 100), 50, none, B),
    %   Capture state before mutation; tests below compare the AFTER
    %   damage union against expectation.  We mutate B by setting its
    %   fill, which schedules a repaint of B's image.
    send(B, fill, colour(red)),
    get(W, changes_area, A),
    %   Box at F-local (0,0,50,50), F displayed at (100,100) → box
    %   window-coord = (100,100,50,50).  The damage rect should cover
    %   at least that area (motif-style hacks may pad it).
    contains(A, 100, 100, 50, 50).

:- end_tests(damage_no_transform).


:- begin_tests(damage_with_transform).

%   With rotate 90 around F-local origin, the box at (0,0,50,50) rotates
%   to AABB (-50,0,50,50) in F-local space.  F displayed at (100,100)
%   → window AABB (50,100,50,50).  The damage rect must cover this
%   rotated screen region, NOT the original (100,100,50,50).

test(rotate_90_invalidates_rotated_rect) :-
    new(T, transform), send(T, rotate, 90),
    setup_window(W, point(100, 100), 50, T, B),
    send(B, fill, colour(red)),
    get(W, changes_area, A),
    contains(A, 50, 100, 50, 50).

test(scale_2x_invalidates_doubled_rect) :-
    new(T, transform), send(T, scale, 2),
    setup_window(W, point(100, 100), 30, T, B),
    send(B, fill, colour(red)),
    get(W, changes_area, A),
    %   Box at (0,0,30,30) scaled 2x → (0,0,60,60).  F displayed at
    %   (100,100) → window AABB (100,100,60,60).
    contains(A, 100, 100, 60, 60).

test(rotate_45_invalidates_diagonal_aabb) :-
    new(T, transform), send(T, rotate, 45),
    setup_window(W, point(200, 200), 100, T, B),
    send(B, fill, colour(red)),
    get(W, changes_area, A),
    %   45-deg rotation of (0,0,100,100): corners (0,0),(100,0),
    %   (100,100),(0,100).  After rotation by 45 the AABB has width
    %   and height ≈ 141 (100*sqrt(2)).  F->offset shifts; the AABB
    %   ends up around (~129, 200, ~142, ~142) — verify the damage
    %   rect is at least roughly that diagonal extent (more than the
    %   axis-aligned 100x100 box).
    get(A, width, AW), AW >= 140,
    get(A, height, AH), AH >= 140.

test(child_change_inside_rotated_figure_round_trips_via_window_point) :-
    %   Property: after mutating a child, the damage rect must contain
    %   the window-coord image of every corner of the child's local
    %   area (taken through <-window_point).
    new(T, transform), send(T, rotate, 33),
    setup_window(W, point(180, 180), 60, T, B),
    send(B, fill, colour(red)),
    get(W, changes_area, A),
    forall(member(Lx-Ly,
                  [0-0, 60-0, 60-60, 0-60]),
           ( get(B, window_point, point(Lx, Ly), P),
             get(P, x, PX), get(P, y, PY),
             contains(A, PX, PY, 1, 1) )).

:- end_tests(damage_with_transform).
