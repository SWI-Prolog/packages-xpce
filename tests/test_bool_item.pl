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

:- module(test_bool_item, [test_bool_item/0]).

/** <module> Test class bool_item

Run with:

    swipl -g test_bool_item -t halt packages/xpce/tests/test_bool_item.pl
*/

:- use_module(library(pce)).
:- use_module(library(plunit)).

test_bool_item :-
    run_tests([bool_item]).

%   item(-Item, +Default, -Log)
%
%   Item in an open dialog.  Log is a chain to which the message
%   appends the values it is called with.

item(Item, Default, Log) :-
    new(Log, chain),
    new(D, dialog),
    send(D, append,
         new(Item, bool_item(test, Default, message(Log, append, @arg1)))),
    send(D, open).

:- begin_tests(bool_item).

test(default_off, [S-P == @off-0]) :-
    new(B, bool_item(x)),
    get(B, selection, S),
    get(B, knob_position, P).
test(default_on, [S-P == @on-100]) :-
    new(B, bool_item(x, @on)),
    get(B, selection, S),
    get(B, knob_position, P).
test(toggle_executes, [S-Values == @on-[@on]]) :-
    item(B, @off, Log),
    send(B, toggle),
    get(B, selection, S),
    chain_list(Log, Values).
test(selection_does_not_execute, [S-Values == @on-[]]) :-
    item(B, @off, Log),
    send(B, selection, @on),
    get(B, selection, S),
    chain_list(Log, Values).
test(modified, [M0-M1-M2 == @off - @on - @off]) :-
    item(B, @off, _),
    get(B, modified, M0),
    send(B, displayed_value, @on),
    get(B, modified, M1),
    send(B, modified, @off),
    get(B, modified, M2).
test(restore, [S == @on]) :-
    item(B, @on, _),
    send(B, selection, @off),
    send(B, restore),
    get(B, selection, S).
test(animates_to_target, [P == 100]) :-
    item(B, @off, _),
    send(B, toggle),
    animate(B, 10),
    get(B, knob_position, P).
test(size_includes_track, [true(W >= 40), true(H >= 22)]) :-
    new(B, bool_item(x)),
    send(B, show_label, @off),
    send(B, compute),
    get(B?area, width, W),
    get(B?area, height, H).

test(destroy_after_animation) :-       % used to free the timer twice
    new(D, dialog),
    send(D, append, new(B, bool_item(x, @off))),
    send(D, open),
    send(B, slot, timer,                % as made by an animation that
         timer(0.016, message(B, animate))), % completed (headless we are
    send(D, destroy).                   % not animated)

:- end_tests(bool_item).

%   Run the animation without depending on the timer.

animate(_, 0) :- !.
animate(B, N) :-
    send(B, animate),
    N2 is N-1,
    animate(B, N2).
