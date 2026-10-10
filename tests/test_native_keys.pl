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
:- module(test_native_keys, [test_native_keys/0]).

/** <module> Test the native key bindings of dialog text entry

The tables for text entry in dialogs follow `key_binding.dialog_style`
(`cua` or `apple`), over the Emacs bindings.  PceEmacs follows
`key_binding.style` and is not affected.

Run with:

    swipl -g test_native_keys -t halt packages/xpce/tests/test_native_keys.pl
*/

:- use_module(library(pce)).
:- use_module(library(plunit)).

test_native_keys :-
    run_tests([ native_keys
              ]).

% Modifier bits of event <-buttons
mod(control, 0x1).
mod(shift,   0x2).
mod(meta,    0x4).
mod(gui,     0x8).

buttons(Mods, Buttons) :-
    foldl([M,B0,B]>>(mod(M, V), B is B0 \/ V), Mods, 0, Buttons).

key(D, Id, Mods) :-
    buttons(Mods, Buttons),
    new(Ev, event(Id, D, 0, 0, Buttons, 1000)),
    ignore(send(D, post_event, Ev)).

%   with_style(+Style, :Goal)
%
%   Run Goal with the dialog key-binding style set to Style.

with_style(Style, Goal) :-
    get(@pce, dialog_key_binding_style, Old),
    setup_call_cleanup(
        send(@pce, dialog_key_binding_style, Style),
        Goal,
        send(@pce, dialog_key_binding_style, Old)).

text_item_dialog(D, TI, Text) :-
    new(D, dialog),
    send(D, append, new(TI, text_item(field, Text))),
    send(D, open),
    send(D, keyboard_focus, TI).

state(TI, Caret-Sel) :-
    get(TI, value_text, T),
    get(T, caret, Caret),
    (   get(T, selection, point(F, To))
    ->  Sel = F-To
    ;   Sel = none
    ).

:- begin_tests(native_keys).

test(shift_extends, States == [3-(0-3), 11-(0-11), 10-none]) :-
    text_item_dialog(D, TI, 'hello world'),
    key(D, cursor_home, []),
    key(D, cursor_right, [shift]),
    key(D, cursor_right, [shift]),
    key(D, cursor_right, [shift]),
    state(TI, S1),
    key(D, end, [shift]),
    state(TI, S2),
    key(D, cursor_left, []),
    state(TI, S3),
    States = [S1, S2, S3],
    send(D, destroy).
test(shift_shrinks, State == 6-(6-11)) :-
    text_item_dialog(D, TI, 'hello world'),
    key(D, cursor_home, []),
    key(D, end, [shift]),
    key(D, cursor_home, [shift]),       % caret back to the anchor
    key(D, end, []),
    forall(between(1, 5, _), key(D, cursor_left, [shift])),
    state(TI, State),
    send(D, destroy).
test(cua_select_all, State == 11-(0-11)) :-
    with_style(cua,
               ( text_item_dialog(D, TI, 'hello world'),
                 key(D, cursor_home, []),
                 key(D, 1, [control]),  % ^A
                 state(TI, State),
                 send(D, destroy) )).
test(cua_word, States == [6-none, 5-(5-11)]) :-
    with_style(cua,
               ( text_item_dialog(D, TI, 'hello world'),
                 key(D, end, []),
                 key(D, cursor_left, [control]),
                 state(TI, S1),
                 key(D, end, []),
                 key(D, cursor_left, [control, shift]),
                 key(D, cursor_left, [shift]),
                 state(TI, S2),
                 States = [S1, S2],
                 send(D, destroy) )).
test(apple_keys, States == [11-(0-11), 6-none, 0-none]) :-
    with_style(apple,
               ( text_item_dialog(D, TI, 'hello world'),
                 key(D, cursor_home, []),
                 key(D, 0'a, [gui]),
                 state(TI, S1),
                 key(D, end, []),
                 key(D, cursor_left, [meta]),
                 state(TI, S2),
                 key(D, cursor_left, [gui]),
                 state(TI, S3),
                 States = [S1, S2, S3],
                 send(D, destroy) )).
test(emacs_keeps_control_a, State == 0-none) :-
    with_style(emacs,
               ( text_item_dialog(D, TI, 'hello world'),
                 key(D, end, []),
                 key(D, 1, [control]),
                 state(TI, State),
                 send(D, destroy) )).
test(emacs_fallback, State == 0-none) :-
    with_style(cua,                     % ^E is not claimed by cua
               ( text_item_dialog(D, TI, 'hello world'),
                 key(D, cursor_home, []),
                 key(D, 5, [control]),
                 key(D, 2, [control]),  % ^B
                 key(D, 1, [control]),  % ^A: select all
                 key(D, cursor_home, []),
                 state(TI, State),
                 send(D, destroy) )).
test(editor_untouched, Fs == [beginning_of_line, beginning_of_line]) :-
    get(@pce, convert, editor, key_binding, KB),
    get(KB, function, '\\C-a', F1),
    with_style(cua, get(KB, function, '\\C-a', F2)),
    Fs = [F1, F2].

:- end_tests(native_keys).
