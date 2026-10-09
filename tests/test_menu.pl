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
    run_tests([menu_kind, menu_solo, menu_keyboard]).

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

:- begin_tests(menu_kind).

test(toggle, [K-MS == toggle-(@on)]) :-
    new(M, menu(m, toggle)),
    get(M, kind, K),
    get(M, multiple_selection, MS).
test(marked_multiple_is_toggle, K == toggle) :-
    new(M, menu(m, marked)),
    send(M, multiple_selection, @on),
    get(M, kind, K).
test(kind_resets_multiple, [K-MS == marked-(@off)]) :-
    new(M, menu(m, toggle)),
    send(M, kind, marked),
    get(M, kind, K),
    get(M, multiple_selection, MS).
test(choice_multiple, [K-MS == choice-(@on)]) :-
    new(M, menu(m, choice)),
    send(M, multiple_selection, @on),
    get(M, kind, K),
    get(M, multiple_selection, MS).
test(default_kind, K == marked) :-
    new(M, menu(m)),
    get(M, kind, K).
test(feedback_to_cycle, K == cycle) :-
    new(M, menu(m, choice)),
    send(M, feedback, show_selection_only),
    get(M, kind, K).
test(feedback_keeps_multiple, K == toggle) :-
    new(M, menu(m, choice)),
    send(M, multiple_selection, @on),
    send(M, feedback, image),
    get(M, kind, K).
test(cycle_has_single_selection, fail) :-
    new(M, menu(m, cycle)),
    send(M, multiple_selection, @on).

:- end_tests(menu_kind).

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

%   focused_menu(+Kind, -Dialog, -Menu, -Log)
%
%   An open dialog with a menu of Kind holding a, b and c, of which b is
%   selected, that has the keyboard focus.

focused_menu(Kind, D, M, Log) :-
    new(D, dialog),
    new(Log, chain),
    send(D, append, new(M, menu(m, Kind, message(Log, append, @arg1)))),
    send_list(M, append, [a,b,c]),
    send(M, selection, b),
    send(D, open),
    send(D, keyboard_focus, M),
    send(D, input_focus, @on).

key(D, Key) :-
    post(D, Key, 0, 0, 0).

:- begin_tests(menu_keyboard).

test(wants_focus, true) :-
    new(M, menu(m, marked)),
    send(M, append, a),
    send(M, '_wants_keyboard_focus').
test(inactive_no_focus, fail) :-
    new(M, menu(m, marked)),
    send(M, append, a),
    send(M, active, @off),
    send(M, '_wants_keyboard_focus').
test(right_moves_selection, [Sel-Log == c-[c]]) :-
    focused_menu(marked, D, M, L),
    key(D, cursor_right),
    get(M, selection, Sel),
    chain_list(L, Log),
    send(D, destroy).
test(up_moves_selection, Sel == a) :-
    focused_menu(choice, D, M, _),
    key(D, cursor_up),
    get(M, selection, Sel),
    send(D, destroy).
test(right_at_end_keeps_selection, Sel == c) :-
    focused_menu(marked, D, M, _),
    key(D, cursor_right),
    key(D, cursor_right),
    get(M, selection, Sel),
    send(D, destroy).
test(skips_inactive, Sel == c) :-
    focused_menu(marked, D, M, _),
    send(M, selection, a),
    send(M, off, b),
    key(D, cursor_right),
    get(M, selection, Sel),
    send(D, destroy).
test(toggle_right_moves_focus, [Sel-FI == [b]-c]) :-
    focused_menu(toggle, D, M, _),
    key(D, cursor_right),
    selection(M, Sel),
    get(M, focus_item, MI),
    get(MI, value, FI),
    send(D, destroy).
test(toggle_space_toggles, [Sel-Log == [b,c]-[c]]) :-
    focused_menu(toggle, D, M, L),
    key(D, cursor_right),
    key(D, 32),
    selection(M, Sel),
    chain_list(L, Log),
    send(D, destroy).

:- end_tests(menu_keyboard).
