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


:- module(test_frame_position, [test_frame_position/0]).

/** <module> Test frame <-position of a top-level frame

A frame that is not placed explicitly is centred on its display when
it is created.  Its <-position must say so, also if the window system
sends no event that the window moved.

Run with:

    swipl -g test_frame_position -t halt packages/xpce/tests/test_frame_position.pl
*/

:- use_module(library(pce)).
:- use_module(library(plunit)).

test_frame_position :-
    run_tests([ frame_position
              ]).

%   opened_frame(-Frame)
%
%   An open frame holding a dialog with a button, after processing the
%   events of opening it, which mark its window as displayed.

opened_frame(F) :-
    new(F, frame(position_test)),
    send(F, append, new(D, dialog)),
    send(D, append, button(ok)),
    send(F, open),
    (   between(1, 50, _),
        ignore(send(@display, dispatch)),
        get(D, displayed, @on)
    ->  true
    ;   true
    ).

:- begin_tests(frame_position).

test(centred, [true(Pos == Centred)]) :-
    opened_frame(F),
    get(F, position, point(X, Y)),
    Pos = X-Y,
    get(F, area, area(_, _, W, H)),
    get(F?display, area, area(_, _, DW, DH)),
    CX is (DW-W)//2,
    CY is (DH-H)//2,
    Centred = CX-CY,
    send(F, destroy).

:- end_tests(frame_position).
