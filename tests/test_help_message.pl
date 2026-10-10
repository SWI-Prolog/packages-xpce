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

:- module(test_help_message, [test_help_message/0]).

/** <module> Test the tooltips of library(help_message)

Run with:

    swipl -g test_help_message -t halt packages/xpce/tests/test_help_message.pl
*/

:- use_module(library(pce)).
:- use_module(library(plunit)).
:- use_module(library(help_message)).

test_help_message :-
    run_tests([ help_message_owner,      % first: see combo_box_tip_over_dialog
                help_message,
                combo_box_keys
              ]).

%   browser(-ListBrowser)
%
%   A browser with three items, of which the first and the last have
%   a tooltip.

browser(LB) :-
    new(B, browser),
    send(B, append, new(A, dict_item(a))),
    send(B, append, dict_item(b)),
    send(B, append, new(C, dict_item(c))),
    send(A, help_message, tag, 'First item'),
    send(C, help_message, tag, 'Last item'),
    send(B, open),
    get(B, list_browser, LB).

%   tip(+ListBrowser, +Line, -Tip)
%
%   Tip is the tooltip for the pointer over the 0-based Line.

tip(LB, Line, Tip) :-
    get(LB?font, height, H),
    Y is round((Line+0.5)*H),
    new(Ev, event(loc_still, LB, 10, Y)),
    get(LB, help_message, tag, Ev, String),
    get(String, value, Tip).

%   Not all video drivers can open a combo box (e.g., `dummy` cannot).

combo_boxes :-
    new(D, dialog),
    send(D, append, new(TI, text_item(value, a))),
    send(TI, value_set, chain(a, b)),
    send(D, open),
    (   catch(send(TI, show_combo_box, @on), _, fail)
    ->  Ok = true
    ;   Ok = false
    ),
    send(D, destroy),
    Ok == true.

:- begin_tests(help_message).

test(item, Tip == 'First item') :-
    browser(LB),
    tip(LB, 0, Tip).
test(other_item, Tip == 'Last item') :-
    browser(LB),
    tip(LB, 2, Tip).
test(item_without_tip, fail) :-
    browser(LB),
    tip(LB, 1, _).
test(browser_tip, Tip == 'The browser') :-
    browser(LB),
    send(LB, help_message, tag, 'The browser'),
    tip(LB, 1, Tip).
test(dict_item, Tip == 'An item') :-
    new(DI, dict_item(x)),
    send(DI, help_message, tag, 'An item'),
    get(DI, help_message, tag, String),
    get(String, value, Tip).

test(print_name, Name == 'An item') :-
    new(DI, dict_item(x, 'An item')),
    get(DI, print_name, Name0),
    atom_string(Name, Name0).

%   The values of a text_item may be dict_items with a tooltip, which
%   is shown in the combo box.  Not all video drivers can open a combo
%   box (e.g., `dummy` cannot).

test(combo_box_tip, [ condition(combo_boxes),
                      Tip-Selection == 'Second value'-b
                    ]) :-
    new(D, dialog),
    send(D, append, new(TI, text_item(value, a))),
    new(B, dict_item(b)),
    send(B, help_message, tag, 'Second value'),
    send(TI, value_set, chain(dict_item(a), B)),
    send(D, open),
    send(TI, show_combo_box, @on),
    get(@completer, list_browser, LB),
    tip(LB, 1, Tip),
    send(TI, quit_completer),
    send(TI, displayed_value, b),
    get(TI, selection, Selection),
    send(D, destroy).

%   While the combo box is shown, the window of the text_item grabs the
%   pointer.  Events over the combo box are for this window, but their
%   position is relative to the frame of the combo box.  The tooltip
%   must be that of the value below the pointer, not of the item of the
%   dialog at the same position.

test(combo_box_tip_over_dialog, [ condition(combo_boxes),
                                  Tip == 'First value'
                                ]) :-
    new(D, dialog),
    send(D, append, new(TI, text_item(value, a))),
    send(D, append, new(B, button(below))),
    send(B, help_message, tag, 'Below'),
    new(A, dict_item(a)),
    send(A, help_message, tag, 'First value'),
    send(TI, value_set, chain(A, dict_item(b))),
    send(D, open),
    send(TI, show_combo_box, @on),
    get(@completer, list_browser, LB),
    get(LB?font, height, H),
    Y is round(0.5*H),
    new(Ev, event(loc_still, D, 10, Y)),
    send(Ev, slot, frame, @completer?frame),
    send(@event, assign, Ev, global),
    ignore(send(D, post_event, Ev)),
    send(@event, assign, @nil, global),
    get(@help_message_window, message, String),
    get(String, value, Tip),
    send(TI, quit_completer),
    send(D, destroy).

:- end_tests(help_message).

:- begin_tests(help_message_owner).

%   A balloon is transient for the frame it shows over.  Closing that
%   frame while the balloon shows (e.g., from the keyboard) used to
%   destroy the reused balloon window, leaving its handler to report
%   every later mouse event to the freed window.  Now the balloon is
%   hidden and kept, the next mouse event removes the handler and a new
%   balloon works.

test(owner_frame_closed, [ condition(combo_boxes),
                           Alive-Shown1-Handlers-Shown2 ==
                           true-false-0-true ]) :-
    new(D1, dialog),
    send(D1, append, new(B1, button(one))),
    send(D1, open),
    new(Ev, event(loc_still, D1, 10, 10)),
    send(@help_message_window, feedback, string('A tip'), Ev, B1),
    send(D1?frame, destroy),
    truth(object(@help_message_window), Alive),
    shown(Shown1),
    new(D2, dialog),
    send(D2, append, new(B2, button(two))),
    send(D2, open),
    new(Move, event(loc_move, D2, 10, 10)),
    ignore(send(D2, post_event, Move)), % try_hide sees the owner is gone
    count_tip_handlers(Handlers),
    new(Ev2, event(loc_still, D2, 10, 10)),
    send(@help_message_window, feedback, string('Another tip'), Ev2, B2),
    shown(Shown2),
    send(@help_message_window, hide),
    send(D2?frame, destroy).

%   As other tooltips, a key hides the balloon, but is not consumed.

test(key_hides, [ condition(combo_boxes),
                  Shown-Handlers-Typed == false-0-x ]) :-
    new(D, dialog),
    send(D, append, new(TI, text_item(name, ''))),
    send(D, open),
    send(D, keyboard_focus, TI),
    new(Ev, event(loc_still, D, 10, 10)),
    send(@help_message_window, feedback, string('A tip'), Ev, TI),
    new(Key, event(0'x, D, 10, 10)),
    ignore(send(D, post_event, Key)),
    shown(Shown),
    count_tip_handlers(Handlers),
    get(TI, selection, Typed),
    send(D?frame, destroy).

:- end_tests(help_message_owner).

%   count_tip_handlers(-N)
%
%   N is the number of balloon handlers in the display's
%   <-inspect_handlers.

count_tip_handlers(N) :-
    get(@display, inspect_handlers, Chain),
    chain_list(Chain, Hs),
    aggregate_all(count,
                  ( member(H, Hs),
                    get(H, message, M),
                    get(M, receiver, @help_message_window)
                  ), N).


%   The keyboard works on an open combo box: up and down move through the
%   values, RET selects, ESC closes and typing starts a new search.

combo(D, TI) :-
    new(D, dialog),
    send(D, append, new(TI, text_item(value, alpha))),
    send(TI, value_set, chain(alpha, beta, gamma, delta)),
    send(D, open),
    send(D, keyboard_focus, TI),
    send(TI, show_combo_box, @on).

key(D, Id) :-
    new(Ev, event(Id, D)),
    send(@event, assign, Ev, global),
    ignore(send(D, post_event, Ev)),
    send(@event, assign, @nil, global).

combo_shown(Shown) :-
    (   get(@completer, attribute, client, Client),
        Client \== @nil
    ->  Shown = true
    ;   Shown = false
    ).

value(TI, Value) :-
    get(TI?displayed_value, value, Value).

:- begin_tests(combo_box_keys,
               [condition(combo_boxes)]).

test(down_and_return, [Value-Shown == gamma-false]) :-
    combo(D, TI),
    key(D, cursor_down),
    key(D, cursor_down),
    key(D, cursor_up),
    key(D, cursor_down),
    key(D, 'RET'),
    value(TI, Value),
    combo_shown(Shown),
    send(D, destroy).
test(escape, [Value-Shown == alpha-false]) :-
    combo(D, TI),
    key(D, cursor_down),
    key(D, 'ESC'),
    value(TI, Value),
    combo_shown(Shown),
    send(D, destroy).
test(search, Value == delta) :-
    combo(D, TI),
    key(D, 0'd),
    key(D, 'RET'),
    value(TI, Value),
    send(D, destroy).

%   The combo box of a cycle menu.  The keyboard focus of the dialog is
%   on another item: an open combo box takes the keyboard.

menu_combo(D, M) :-
    new(D, dialog),
    send(D, append, new(T, text_item(name, ''))),
    send(D, append, new(M, menu(choice, cycle))),
    send_list(M, append, [alpha, beta, gamma, delta]),
    send(D, open),
    send(D, keyboard_focus, T),
    send(M, execute).

post(D, Id, Frame, X, Y) :-
    new(Ev, event(Id, D, X, Y)),
    send(Ev, slot, frame, Frame),
    send(@event, assign, Ev, global),
    ignore(send(D, post_event, Ev)),
    send(@event, assign, @nil, global).

test(menu_down_and_return, [Selection-Shown == gamma-false]) :-
    menu_combo(D, M),
    key(D, cursor_down),
    key(D, cursor_down),
    key(D, 'RET'),
    get(M, selection, Selection),
    combo_shown(Shown),
    send(D, destroy).
test(menu_escape, [Selection-Shown == alpha-false]) :-
    menu_combo(D, M),
    key(D, cursor_down),
    key(D, 'ESC'),
    get(M, selection, Selection),
    combo_shown(Shown),
    send(D, destroy).
test(menu_search, Selection == delta) :-
    menu_combo(D, M),
    key(D, 0'd),
    key(D, 'RET'),
    get(M, selection, Selection),
    send(D, destroy).
test(menu_click_outside, Shown == false) :-
    menu_combo(D, _M),
    get(D, frame, F),
    get(D?area, width, W),
    X is W-5,
    post(D, ms_left_down, F, X, 5),
    post(D, ms_left_up, F, X, 5),
    combo_shown(Shown),
    send(D, destroy).

:- end_tests(combo_box_keys).

shown(Shown) :-
    truth(get(@help_message_window?frame, status, window), Shown).

truth(Goal, true) :- call(Goal), !.
truth(_, false).
