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


:- module(test_password_item, [test_password_item/0]).

/** <module> Test class password_item

Run with:

    swipl -g test_password_item -t halt packages/xpce/tests/test_password_item.pl
*/

:- use_module(library(pce)).
:- use_module(library(password_item)).
:- use_module(library(plunit)).

test_password_item :-
    run_tests([ password_item
              ]).

%   password_dialog(-Dialog, -Item)
%
%   An open dialog holding a password_item.

password_dialog(D, P) :-
    password_dialog(D, P, @default).

%   password_dialog(-Dialog, -Item, +Message)

password_dialog(D, P, Message) :-
    new(D, dialog),
    send(D, append, new(P, password_item(password, Message))),
    send(D, open).

post(D, Id, X, Y, Buttons) :-
    new(Ev, event(Id, D, X, Y, Buttons, 1000)),
    ignore(send(D, post_event, Ev)).

%   click(+Dialog, +Item, +Where)
%
%   Click near the left or right end of the entry field of Item.

click(D, P, Where) :-
    get(P, area, area(X0, Y0, W, H)),
    get(P, label_width, LW),
    (   Where == left
    ->  X is X0 + LW + 6
    ;   X is X0 + W - 20
    ),
    Y is Y0 + H//2,
    post(D, ms_left_down, X, Y, 0x10),
    post(D, ms_left_up, X, Y, 0).

type(D, Codes) :-
    forall(member(C, Codes), post(D, C, 0, 0, 0)).

password(P, Value) :-
    get(P, selection, S),
    (   get(S, value, Value)
    ->  true
    ;   Value = ''                     % string<-value fails if empty
    ).

%   click_clear(+Dialog, +Item)
%
%   Click the clear icon at the right of Item.

click_clear(D, P) :-
    get(P, area, area(X0, Y0, W, H)),
    X is X0 + W - 4,
    Y is Y0 + H//2,
    post(D, ms_left_down, X, Y, 0x10),
    post(D, ms_left_up, X, Y, 0).

:- begin_tests(password_item).

test(click_and_type, Value == abc) :-
    password_dialog(D, P),
    click(D, P, right),
    type(D, `abc`),
    password(P, Value),
    send(D, destroy).
test(shows_bullets, Shown == '\u25CF\u25CF\u25CF') :-
    password_dialog(D, P),
    click(D, P, right),
    type(D, `abc`),
    get(P, displayed_value, S),
    get(S, value, Shown),
    send(D, destroy).
test(click_moves_caret, Value == 'Xabc') :-
    password_dialog(D, P),
    click(D, P, right),
    type(D, `abc`),
    click(D, P, left),
    type(D, `X`),
    password(P, Value),
    send(D, destroy).

test(return_applies, Applied == [abc]) :-
    new(Log, chain),
    password_dialog(D, P, message(@prolog, log_password, Log, @arg1)),
    click(D, P, right),
    type(D, `abc`),
    post(D, 'RET', 0, 0, 0),
    post(D, 'RET', 0, 0, 0),           % unmodified: no message
    chain_list(Log, Applied),
    send(D, destroy).
test(clear_icon, Value-Shown == ''-0) :-
    password_dialog(D, P),
    click(D, P, right),
    type(D, `abc`),
    click_clear(D, P),
    password(P, Value),
    get(P, displayed_value, S),
    get(S, size, Shown),
    send(D, destroy).

test(click_on_undisplayed_text_item, true) :-
    %  The shadow of a password_item is not displayed.  A click must not
    %  crash it, although it cannot tell where the click is.
    password_dialog(D, _),
    new(T, text_item(shadow)),
    forall(member(Id-B, [ms_left_down-0x10, ms_left_drag-0x10]),
           ( new(Ev, event(Id, D, 10, 10, B, 2000)),
             ignore(send(T, event, Ev)) )),
    send(D, destroy).

test(select_all_shows, [forall(member(Style, [cua, apple])),
                        setup(dialog_style(Style, Old)),
                        cleanup(dialog_style(Old, _)),
                        Sel-Value == (0-3)-x]) :-
    password_dialog(D, P),
    click(D, P, right),
    type(D, `abc`),
    select_all_key(Style, Id, Buttons),
    post(D, Id, 0, 0, Buttons),
    shown_selection(P, Sel),
    type(D, `x`),                       % replaces the selection
    password(P, Value),
    send(D, destroy).
test(shift_left_shows, Sel == 2-3) :-
    password_dialog(D, P),
    click(D, P, right),
    type(D, `abc`),
    post(D, cursor_left, 0, 0, 0x2),    % Shift-Left
    shown_selection(P, Sel),
    send(D, destroy).
test(no_copy, [Pasted-Value == before-abc]) :-
    password_dialog(D, P),
    click(D, P, right),
    type(D, `abc`),
    post(D, cursor_home, 0, 0, 0x2),    % Shift-Home
    send(@display, copy, before),
    post(D, 3, 0, 0, 0x1),              % Control-C
    post(D, 24, 0, 0, 0x1),             % Control-X
    get(@display, paste, S),
    get(S, value, Pasted),
    password(P, Value),
    send(D, destroy).

:- end_tests(password_item).

shown_selection(P, From-To) :-
    get(P?value_text, selection, point(From, To)).

%   select_all_key(+Style, -Id, -Buttons)
%
%   The key that selects all in a text item for a dialog key binding
%   style: Control-A for `cua` (Windows, Unix), Command-A for `apple`
%   (MacOS), where Control-A goes to the start of the line.

select_all_key(cua,   1,   0x1).            % Control-A
select_all_key(apple, 0'a, 0x8).            % Command-A

dialog_style(New, Old) :-
    get(@pce, dialog_key_binding_style, Old),
    send(@pce, dialog_key_binding_style, New).

native_dialog_style :-
    get(@pce, convert, key_binding, class, Class),
    get(Class, class_variable, dialog_style, Var),
    get(Var, value, Style),
    Style \== emacs.

log_password(Log, Passwd) :-
    get(Passwd, value, Value),
    send(Log, append, Value).
