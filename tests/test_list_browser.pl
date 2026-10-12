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


:- module(test_list_browser, [test_list_browser/0]).

/** <module> Test class list_browser

Run with:

    swipl -g test_list_browser -t halt packages/xpce/tests/test_list_browser.pl
*/

:- use_module(library(pce)).
:- use_module(library(plunit)).

test_list_browser :-
    run_tests([ list_browser_scroll,
                list_browser_selection_changed,
                list_browser_cursor
              ]).

%   list(+Items, -ListBrowser)
%
%   An open browser of 5 lines holding the numbers 1..Items.

list(Items, LB) :-
    new(B, browser(@default, size(20, 5))),
    forall(between(1, Items, I), send(B, append, I)),
    send(B, open),
    get(B, list_browser, LB).

done(LB) :-
    get(LB, window, B),
    send(B, destroy).

%   scrolled(+LB, +How, -Start)
%
%   Start is the first item shown after scrolling from the top as How
%   says.

scrolled(LB, How, Start) :-
    send(LB, scroll_to, 0),
    How =.. [Sel|Args],
    Msg =.. [send, LB, Sel|Args],
    call(Msg),
    get(LB, start, Start).

:- begin_tests(list_browser_scroll).

%   Scrolling stops when the last item is on the bottom line.

test(scrolling_stops_with_the_last_item_at_the_bottom, Starts == [45,45,45,45,45]) :-
    list(50, LB),
    findall(S, ( member(How, [ scroll_vertical(forwards, page, 10000),
                               scroll_vertical(forwards, line, 1000),
                               scroll_vertical(goto, file, 1000),
                               scroll_to(@default),
                               scroll_to(49)
                             ]),
                 scrolled(LB, How, S)
               ), Starts),
    done(LB).
test(a_short_list_does_not_scroll, Start == 0) :-
    list(3, LB),
    scrolled(LB, scroll_vertical(forwards, page, 1000), Start),
    done(LB).
test(scrolling_back_is_free, Start == 10) :-
    list(50, LB),
    send(LB, scroll_to, 10),
    get(LB, start, Start),
    done(LB).

:- end_tests(list_browser_scroll).


		 /*******************************
		 *      SELECTION CHANGED	*
		 *******************************/

%   ->selection_changed is sent once after each operation that changed
%   the selection.  We log the selection at that moment.

:- dynamic changed/1.

:- pce_begin_class(logging_list_browser, list_browser).

selection_changed(LB) :->
    selection_keys(LB, Keys),
    assertz(changed(Keys)).

:- pce_end_class(logging_list_browser).

:- pce_begin_class(logging_browser, browser).

selection_changed(B) :->
    selection_keys(B?list_browser, Keys),
    assertz(changed(browser(Keys))).

:- pce_end_class(logging_browser).

selection_keys(LB, Keys) :-
    (   get(LB, selection, Sel)
    ->  (   send(Sel, instance_of, chain)
        ->  chain_list(Sel, Items),
            maplist([DI,K]>>get(DI, key, K), Items, Keys)
        ;   get(Sel, key, Key),
            Keys = [Key]
        )
    ;   Keys = []
    ).

%   logged(+LB, :Goal, -Calls)
%
%   Calls is the list of selections logged while running Goal on LB.

logged(LB, Goal, Calls) :-
    retractall(changed(_)),
    call(Goal, LB),
    findall(C, retract(changed(C)), Calls).

lb(LB) :-
    new(LB, logging_list_browser),
    forall(between(1, 5, I), send(LB, append, I)).

item(LB, Key, DI) :-
    get(LB, member, Key, DI).

select(Key, LB) :-
    item(LB, Key, DI),
    send(LB, selection, DI).
change(How, Key, LB) :-
    item(LB, Key, DI),
    send(LB, change_selection, How, DI).

:- begin_tests(list_browser_selection_changed).

test(set_selection, Calls == [[2]]) :-
    lb(LB),
    logged(LB, select(2), Calls).
test(replace_selection, Calls == [[3]]) :-
    lb(LB),
    select(2, LB),
    logged(LB, change(set, 3), Calls).
test(same_selection, Calls == []) :-
    lb(LB),
    select(2, LB),
    logged(LB, change(set, 2), Calls).
test(clear_selection, Calls == [[]]) :-
    lb(LB),
    select(2, LB),
    logged(LB, [L]>>send(L, change_selection, clear), Calls).
test(clear_empty_selection, Calls == []) :-
    lb(LB),
    logged(LB, [L]>>send(L, change_selection, clear), Calls).
test(extend_selection, Calls == [[2,3,4]]) :-
    lb(LB),
    send(LB, multiple_selection, @on),
    change(set, 2, LB),
    logged(LB, change(extend, 4), Calls).
test(next_line, Calls == [[2],[3]]) :-
    lb(LB),
    logged(LB, [L]>>(send(L, next_line), send(L, next_line)), Calls).
test(delete_selected, Calls == [[]]) :-
    lb(LB),
    select(2, LB),
    logged(LB, [L]>>send(L, delete, 2), Calls).
test(clear_browser, Calls == [[]]) :-
    lb(LB),
    select(2, LB),
    logged(LB, [L]>>send(L, clear), Calls).
test(browser, Calls == [browser([2])]) :-
    new(B, logging_browser),
    forall(between(1, 5, I), send(B, append, I)),
    get(B, list_browser, LB),
    logged(LB, select(2), Calls).

:- end_tests(list_browser_selection_changed).


		 /*******************************
		 *	      CURSOR		*
		 *******************************/

%   pointer_cursor(+Window, +Graphical, -Name)
%
%   Name is the cursor displayed with the pointer over Graphical.

pointer_cursor(W, Gr, Name) :-
    get(Gr, area, A),
    get(A, x, X0), get(A, y, Y0),
    X is X0+20, Y is Y0+20,
    ignore(send(W, post_event, event(loc_move, W, X, Y, 0, 1000))),
    get(W, displayed_cursor, C),
    get(C, name, Name).

:- begin_tests(list_browser_cursor).

%   ->cursor is shown over the list, not only while we have the focus.

test(list_browser, Name == text) :-
    new(D, dialog),
    send(D, append, new(LB, list_browser)),
    forall(between(1, 5, I), send(LB, append, I)),
    send(D, open),
    send(LB, cursor, text),
    pointer_cursor(D, LB, Name),
    send(D, destroy).
test(browser, Name == text) :-
    new(B, browser),
    forall(between(1, 5, I), send(B, append, I)),
    send(B, open),
    send(B, cursor, text),
    get(B, list_browser, LB),
    pointer_cursor(B, LB, Name),
    send(B, destroy).

:- end_tests(list_browser_cursor).
