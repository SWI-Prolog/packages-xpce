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

:- module(test_subpixel_events, [test_subpixel_events/0]).

/** <module> Fractional event positions

Events store the position reported by the window system, including the
fraction.  By default the position getters return whole pixels (floor),
as xpce always did.  A window with `subpixel_events` @on gets the
fractional position.

Run with:

    swipl -g test_subpixel_events -t halt \
          packages/xpce/tests/test_subpixel_events.pl
*/

:- use_module(library(pce)).
:- use_module(library(plunit)).

setup_headless :-
    set_prolog_flag('SDL_VIDEODRIVER', dummy).
:- initialization(setup_headless, now).

test_subpixel_events :-
    run_tests([ subpixel_events ]).

%!  picture_with_box(+Subpixel, -Picture, -Box) is det.
%
%   Open a picture with `subpixel_events` set to Subpixel, holding a
%   box at (40,60).

picture_with_box(Subpixel, P, B) :-
    new(P, picture('', size(400, 400))),
    send(P, subpixel_events, Subpixel),
    new(B, box(50, 50)),
    send(P, display, B, point(40, 60)),
    send(P, open).

position(E, Gr, X, Y) :-
    get(E, position, Gr, point(X, Y)).

:- begin_tests(subpixel_events).

test(default_off, Value == @off) :-
    new(P, picture),
    get(P, subpixel_events, Value),
    send(P, free).
test(stored_fraction, [X-Y == 40.25-60.5]) :-
    new(P, picture),
    new(E, event(loc_move, P, 40.25, 60.5)),
    get(E, slot, x, X),
    get(E, slot, y, Y),
    send(P, free).
test(rounded_window, [WX-WY == 40-60, BX-BY == 0-0]) :-
    picture_with_box(@off, P, B),
    new(E, event(loc_move, P, 40.75, 60.5)),
    get(E, x, P, WX),
    get(E, y, P, WY),
    position(E, B, BX, BY).
test(rounded_negative, [WX-WY == -1-(-1), BX-BY == -41-(-61)]) :-
    picture_with_box(@off, P, B),
    new(E, event(loc_move, P, -0.5, -0.25)),
    get(E, x, P, WX),
    get(E, y, P, WY),
    position(E, B, BX, BY).
test(subpixel_window, [WX-WY == 40.75-60.5, BX-BY == 0.75-0.5]) :-
    picture_with_box(@on, P, B),
    new(E, event(loc_move, P, 40.75, 60.5)),
    get(E, x, P, WX),
    get(E, y, P, WY),
    position(E, B, BX, BY).
test(subpixel_transform, [BX-BY == 0.5-1.25]) :-
    picture_with_box(@on, P, _),
    new(F, figure),
    new(B, box(80, 80)),
    send(F, display, B, point(0, 0)),
    send(P, display, F, point(100, 100)),
    new(T, transform), send(T, scale, 2),
    send(F, transform, T),
    new(E, event(loc_move, P, 101, 102.5)),
    position(E, B, BX, BY).

:- end_tests(subpixel_events).
