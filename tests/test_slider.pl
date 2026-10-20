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

:- module(test_slider, [test_slider/0]).

/** <module> Test class slider

Run with:

    swipl -g test_slider -t halt packages/xpce/tests/test_slider.pl
*/

:- use_module(library(pce)).
:- use_module(library(plunit)).
:- use_module(library(apply)).

test_slider :-
    run_tests([slider, slider_keyboard]).

:- begin_tests(slider).

%   Numbers are tagged doubles.  A slider between bounds that are not
%   whole numbers edits reals rather than integers.

test(real_selection, V =:= 2.75) :-
    new(S, slider(scale, 0.5, 3.0, 1)),
    send(S, selection, 2.75),
    get(S, selection, V).
test(int_selection, V == 42) :-
    new(S, slider(count, 0, 100, 42)),
    get(S, selection, V).
test(real_default, V =:= 1.25) :-
    new(S, slider(scale, 0.5, 3.0, 1.25)),
    get(S, selection, V).

%   A slider without a label and value is as high as its bar and its
%   bar is next to the middle of the text of the items in its row.
%   The fonts are large such that the label and values are higher than
%   the bar.

big_fonts(S) :-
    new(F, font(sans, normal, 36)),
    send(S, label_font, F),
    send(S, value_font, F).

test(bare_height, [Bare-Labelled == smaller-labelled]) :-
    new(S1, slider(position, 0, 100, 50)),
    big_fonts(S1),
    send(S1, show_label, @off),
    send(S1, show_value, @off),
    new(S2, slider(position, 0, 100, 50)),
    big_fonts(S2),
    get(S1, height, H1),
    get(S2, height, H2),
    (H1 < H2 -> Bare = smaller ; Bare = H1),
    Labelled = labelled.
test(label_height, [H1 == H2]) :-
    new(S1, slider(position, 0, 100, 50)),
    big_fonts(S1),
    send(S1, show_value, @off),
    new(S2, slider(position, 0, 100, 50)),
    big_fonts(S2),
    get(S1, height, H1),
    get(S2, height, H2).
test(bare_aligned, [Aligned == true]) :-
    new(D, dialog),
    send(D, append, new(B, button(play))),
    send(D, append, new(S, slider(position, 0, 100, 50)), right),
    send(S, show_label, @off),
    send(S, show_value, @off),
    send(D, layout),
    get(B?area, center, point(_, BY)),
    get(S?area, center, point(_, SY)),
    (abs(BY-SY) =< 2 -> Aligned = true ; Aligned = BY-SY),
    free(D).

:- end_tests(slider).

%   slider_dialog(-Dialog, -Slider, -Log, +Low, +High, +Value)
%
%   An open dialog with a focussed slider whose message logs its value.

slider_dialog(D, S, Log, Low, High, Value) :-
    new(Log, chain),
    new(D, dialog),
    send(D, append,
         new(S, slider(value, Low, High, Value, message(Log, append, @arg1)))),
    send(D, append, button(after)),
    send(D, open),
    send(D, keyboard_focus, S).

keys(D, S, Keys, Values) :-
    foldl(key(D, S), Keys, Values, []).

key(D, S, Id, [V|T], T) :-
    new(Ev, event(Id, D, 0, 0, 0, 1000)),
    send(D, post_event, Ev),
    get(S, selection, V).

:- begin_tests(slider_keyboard).

test(int_keys, Values-Calls == [26,25,26,25,35,25,100,0]-Values) :-
    slider_dialog(D, S, Log, 0, 100, 25),
    keys(D, S, [cursor_right, cursor_left, cursor_up, cursor_down,
                page_up, page_down, end, cursor_home], Values),
    chain_list(Log, Calls),
    send(D, destroy).
test(clamped, Values-Calls == [100]-[]) :-
    slider_dialog(D, S, Log, 0, 100, 100),
    keys(D, S, [cursor_right], Values),
    chain_list(Log, Calls),
    send(D, destroy).
test(real_keys, Values == [0.53, 0.23, 1.5]) :-
    slider_dialog(D, S, _, -1.5, 1.5, 0.5),
    keys(D, S, [cursor_right, page_down, end], Values),
    send(D, destroy).
test(explicit_steps, Values == [27,25,45,25]) :-
    slider_dialog(D, S, _, 0, 100, 25),
    send(S, step, 2),
    send(S, page_step, 20),
    keys(D, S, [cursor_right, cursor_left, page_up, page_down], Values),
    send(D, destroy).
test(tab_leaves, Focus == after) :-
    slider_dialog(D, _, _, 0, 100, 25),
    new(Ev, event('TAB', D, 0, 0, 0, 1000)),
    send(D, post_event, Ev),
    get(D?keyboard_focus, name, Focus),
    send(D, destroy).
test(takes_focus, true) :-
    new(S, slider(value, 0, 100, 25)),
    send(S, '_wants_keyboard_focus').

:- end_tests(slider_keyboard).
