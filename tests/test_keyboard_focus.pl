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
:- module(test_keyboard_focus, [test_keyboard_focus/0]).

/** <module> Test moving the keyboard focus through a dialog

Tab and Shift-Tab move the keyboard focus through the items of a
dialog, entering and leaving dialog_group and tab items.  SDL names
the Tab key `TAB`, which is what these tests post.

Run with:

    swipl -g test_keyboard_focus -t halt packages/xpce/tests/test_keyboard_focus.pl
*/

:- use_module(library(pce)).
:- use_module(library(password_item)).
:- use_module(library(plunit)).

test_keyboard_focus :-
    run_tests([ keyboard_focus
              ]).

post(D, Id, Buttons) :-
    new(Ev, event(Id, D, 0, 0, Buttons, 1000)),
    ignore(send(D, post_event, Ev)).

tab(D)           :- post(D, 'TAB', 0).
shift_tab(D)     :- post(D, 'TAB', 0x2).
control_tab(D)   :- post(D, 'TAB', 0x1).

focus(D, Name) :-
    get(D, slot, keyboard_focus, F),
    (   F == @nil
    ->  Name = @nil
    ;   get(F, name, Name)
    ).

%   walk(+Dialog, +Step, +Count, -Names)
%
%   Press a key Count times, collecting the name of the focussed item
%   after each press.

walk(_, _, 0, []) :- !.
walk(D, Step, N, [Name|T]) :-
    call(Step, D),
    focus(D, Name),
    N1 is N-1,
    walk(D, Step, N1, T).

buttons(D) :-
    new(D, dialog),
    send(D, append, button(a)),
    send(D, append, new(B, button(b)), right),
    send(B, active, @off),
    send(D, append, button(c), right),
    send(D, append, button(d), right),
    send(D, open),
    send(D, keyboard_focus, D?a_member).

:- begin_tests(keyboard_focus).

test(buttons_forwards, Names == [c, d, a]) :-
    buttons(D),
    walk(D, tab, 3, Names),
    send(D, destroy).
test(buttons_backwards, Names == [d, c, a]) :-
    buttons(D),
    walk(D, shift_tab, 3, Names),
    send(D, destroy).
test(text_items, Names-Back == [p, t2, t1]-[t2, p, t1]) :-
    new(D, dialog),
    send(D, append, text_item(t1)),
    send(D, append, password_item(p)),
    send(D, append, text_item(t2)),
    send(D, open),
    send(D, advance),
    walk(D, tab, 3, Names),
    walk(D, shift_tab, 3, Back),
    send(D, destroy).
test(dialog_group, Names-Back == [g1, g2, after, before]-[after, g2, g1, before]) :-
    new(D, dialog),
    send(D, append, button(before)),
    send(D, append, new(G, dialog_group(group))),
    send(G, append, button(g1)),
    send(G, append, button(g2)),
    send(D, append, button(after)),
    send(D, open),
    send(D, keyboard_focus, D?before_member),
    walk(D, tab, 4, Names),
    send(D, keyboard_focus, D?before_member),
    walk(D, shift_tab, 4, Back),
    send(D, destroy).
test(tab_stack, Names-Back == [b, ok, a]-[ok, b, a]) :-
    new(D, dialog),
    send(D, append, tab_stack(new(T1, tab(one)), new(T2, tab(two)))),
    send(T1, append, button(a)),
    send(T1, append, button(b)),
    send(T2, append, button(hidden)),
    send(D, append, button(ok)),
    send(D, open),
    send(D, keyboard_focus, T1?a_member),
    walk(D, tab, 3, Names),
    walk(D, shift_tab, 3, Back),
    send(D, destroy).
test(editor, [Text, Names] == ["\tx", [after, editor]]) :-
    new(D, dialog),
    send(D, append, new(E, editor(@default, 20, 3))),
    send(D, append, button(after)),
    send(D, open),
    send(E, contents, x),
    send(E, caret, 0),
    send(D, keyboard_focus, E),
    tab(D),                             % inserts a tab
    get(E?text_buffer, contents, S),
    get(S, value, Text0),
    atom_string(Text0, Text),
    control_tab(D),                     % leaves the editor
    focus(D, N1),
    shift_tab(D),
    focus(D, N2),
    Names = [N1, N2],
    send(D, destroy).

test(editor_outside_dialog, fail) :-   % e.g., PceEmacs
    new(V, view),
    send(V, open),
    get(V, editor, E),
    call_cleanup(( send(E, focus_next)
                 ; send(E, focus_previous)
                 ),
                 send(V, destroy)).

%   tabs(-Dialog, -TabStack)
%
%   Four tabs, of which `two` is inactive.

tabs(D, TS) :-
    new(D, dialog),
    send(D, append,
         new(TS, tab_stack(new(T1, tab(one)), new(T2, tab(two)),
                           new(T3, tab(three)), new(T4, tab(four))))),
    send(T1, append, button(a)),
    send(T2, append, button(b)),
    send(T2, active, @off),
    send(T3, append, button(c)),
    send(T4, append, button(d)),
    send(D, open),
    send(D, keyboard_focus, T1?a_member).

on_top(TS, Name) :-
    get(TS?on_top, name, Name).

%   cycle(+Dialog, +TabStack, +Id, +Buttons, +Count, -TabsAndFoci)

cycle(_, _, _, _, 0, []) :- !.
cycle(D, TS, Id, Buttons, N, [Tab/Focus|T]) :-
    post(D, Id, Buttons),
    on_top(TS, Tab),
    focus(D, Focus),
    N1 is N-1,
    cycle(D, TS, Id, Buttons, N1, T).

test(control_tab, L == [three/c, four/d, one/a]) :-
    tabs(D, TS),
    cycle(D, TS, 'TAB', 0x1, 3, L),
    send(D, destroy).
test(control_shift_tab, L == [four/d, three/c, one/a]) :-
    tabs(D, TS),
    cycle(D, TS, 'TAB', 0x3, 3, L),
    send(D, destroy).
test(control_page_down, L == [three/c, four/d, one/a]) :-
    tabs(D, TS),
    cycle(D, TS, page_down, 0x1, 3, L),
    send(D, destroy).
test(control_page_up, L == [four/d, three/c, one/a]) :-
    tabs(D, TS),
    cycle(D, TS, page_up, 0x1, 3, L),
    send(D, destroy).

:- end_tests(keyboard_focus).
