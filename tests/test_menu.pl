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

:- module(test_menu, [test_menu/0]).

/** <module> Test class menu

Run with:

    swipl -g test_menu -t halt packages/xpce/tests/test_menu.pl
*/

:- use_module(library(pce)).
:- use_module(library(plunit)).

test_menu :-
    run_tests([menu_solo]).

%   toggle_menu(-Dialog, -Menu, -Log)
%
%   An open dialog with a toggle menu holding a, b, c and d, of which
%   a, b and c are selected.  Log collects the values the message of the
%   menu is called with.

toggle_menu(D, M, Log) :-
    new(D, dialog),
    new(Log, chain),
    send(D, append, new(M, menu(m, toggle, message(Log, append, @arg1)))),
    send_list(M, append, [a,b,c,d]),
    send(M, selection, chain(a,b,c)),
    send(D, open),
    (   nb_current(test_menu_time, _)
    ->  pause                           % no double-click across tests
    ;   nb_setval(test_menu_time, 100000)
    ).

selection(M, List) :-
    get(M, selection, Chain),
    chain_list(Chain, List).

%   click(+Dialog, +Menu, +Item, +Modifiers)
%
%   Click on Item.  The events have their own time, so that two clicks
%   are a double click unless pause/0 separates them.

click(D, M, Item, Modifiers) :-
    item_position(D, M, Item, X, Y),
    Down is 0x10 \/ Modifiers,
    post(D, ms_left_down, X, Y, Down),
    post(D, ms_left_up, X, Y, Modifiers).

alt_click(D, M, Item) :-
    click(D, M, Item, 0x4).             % BUTTON_meta

pause :-
    nb_getval(test_menu_time, T0),
    T is T0+3000,
    nb_setval(test_menu_time, T).

post(D, Id, X, Y, Buttons) :-
    nb_getval(test_menu_time, T0),
    T is T0+20,
    nb_setval(test_menu_time, T),
    new(Ev, event(Id, D, X, Y, Buttons, T)),
    send(@event, assign, Ev, global),
    ignore(send(D, post_event, Ev)),
    send(@event, assign, @nil, global).

item_position(D, M, Item, X, Y) :-
    get(M, area, area(MX, MY, MW, MH)),
    between(0, MH, DY),
    Y is MY+DY,
    between(0, MW, DX),
    X is MX+DX,
    new(Ev, event(loc_move, D, X, Y)),
    get(M, item_from_event, Ev, MI),
    get(MI, value, Item),
    !.

:- begin_tests(menu_solo).

test(alt_click_selects_only_the_item, [Sel-Calls == [b]-1]) :-
    toggle_menu(D, M, Log),
    alt_click(D, M, b),
    selection(M, Sel),
    get(Log, size, Calls),
    send(D, destroy).
test(alt_click_again_restores, Sel == [a,b,c]) :-
    toggle_menu(D, M, _),
    alt_click(D, M, b),
    pause,
    alt_click(D, M, b),
    selection(M, Sel),
    send(D, destroy).
test(double_click_selects_only_the_item, Sel == [b]) :-
    toggle_menu(D, M, _),
    click(D, M, b, 0),
    click(D, M, b, 0),
    selection(M, Sel),
    send(D, destroy).
test(double_click_again_restores, Sel == [a,b,c]) :-
    toggle_menu(D, M, _),
    click(D, M, b, 0),
    click(D, M, b, 0),
    pause,
    click(D, M, b, 0),
    click(D, M, b, 0),
    selection(M, Sel),
    send(D, destroy).
test(single_click_toggles, Sel == [a,c]) :-
    toggle_menu(D, M, _),
    click(D, M, b, 0),
    selection(M, Sel),
    send(D, destroy).

:- end_tests(menu_solo).
