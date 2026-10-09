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

:- module(test_accelerators, [test_accelerators/0]).

/** <module> Test accelerators of dialog items in groups and tabs

Run with:

    swipl -g test_accelerators -t halt packages/xpce/tests/test_accelerators.pl
*/

:- use_module(library(pce)).
:- use_module(library(plunit)).

test_accelerators :-
    run_tests([ accelerators
              ]).

%   tab_dialog(-Dialog, -Parts, -Log)
%
%   A dialog with the button `again` around a tab stack whose tabs
%   `one` and `two` each hold a button `apply`.  The buttons append
%   their name to Log.  Parts is a dict with the stack, tabs and
%   buttons.

tab_dialog(D, _{stack:TS, one:T1, two:T2,
                again:Again, apply1:A1, apply2:A2}, Log) :-
    new(Log, chain),
    new(D, dialog),
    send(D, append, new(Again, button(again, message(Log, append, again)))),
    send(D, append,
         new(TS, tab_stack(new(T1, tab(one)), new(T2, tab(two))))),
    send(T1, append, new(A1, button(apply, message(Log, append, apply1)))),
    send(T2, append, new(A2, button(apply, message(Log, append, apply2)))),
    send(D, layout).

%   acc(+Item, -Acc) is semidet.
%
%   Acc is the accelerator key assigned to Item.  Fails if it has none
%   (@nil or @default).

acc(Item, Acc) :-
    get(Item, accelerator, Acc),
    atom(Acc).

:- begin_tests(accelerators).

test(tab_items_get_one, true) :-
    tab_dialog(D, P, _),
    acc(P.apply1, _),
    acc(P.apply2, _),
    send(D, destroy).
test(tabs_share_accelerators, A1 == A2) :-
    tab_dialog(D, P, _),
    acc(P.apply1, A1),
    acc(P.apply2, A2),
    send(D, destroy).
test(around_tabs_is_distinct, true(Around \== InTab)) :-
    tab_dialog(D, P, _),
    acc(P.again, Around),
    acc(P.apply1, InTab),
    send(D, destroy).
test(key_goes_to_tab_on_top, Calls == [apply1]) :-
    tab_dialog(D, P, Log),
    acc(P.apply1, Acc),
    send(P.stack, key, Acc),
    chain_list(Log, Calls),
    send(D, destroy).
test(key_follows_on_top, Calls == [apply2]) :-
    tab_dialog(D, P, Log),
    send(P.stack, on_top, P.two),
    acc(P.apply2, Acc),
    send(P.stack, key, Acc),
    chain_list(Log, Calls),
    send(D, destroy).
test(group_items, Calls == [inside]) :-
    new(Log, chain),
    new(D, dialog),
    send(D, append, new(G, dialog_group(box))),
    send(G, append, new(B, button(inside, message(Log, append, inside)))),
    send(D, layout),
    acc(B, Acc),
    send(G, key, Acc),
    chain_list(Log, Calls),
    send(D, destroy).

:- end_tests(accelerators).
