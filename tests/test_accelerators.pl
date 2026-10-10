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
:- use_module(library(apply)).
:- use_module(library(yall)).
:- use_module(library(tabbed_window)).

test_accelerators :-
    run_tests([ accelerators,
                accelerator_assignment
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

test(menu_bar_letters_reserved, true(Acc \== '\\ef')) :-
    new(D, dialog),
    send(D, append, new(MB, menu_bar)),
    send(MB, append, popup(file)),
    send(D, append,
         tab_stack(new(T, tab(numbers)))),
    send(T, append, new(F, text_item(float, ''))),
    send(D, layout),
    acc(F, Acc),
    send(D, destroy).
test(no_accelerator_for_label, Acc == @nil) :-
    new(D, dialog),
    send(D, append, new(L, label(name, 'Some text'))),
    send(D, layout),
    get(L, accelerator, Acc),
    send(D, destroy).
test(alt_letter_leaves_text_item, [condition(native_dialog_style),
                                   Calls == [find]]) :-
    new(Log, chain),
    new(D, dialog),
    send(D, append, new(TI, text_item(name, ''))),
    send(D, append, new(B, button(find, message(Log, append, find)))),
    send(D, open),
    send(D, keyboard_focus, TI),
    acc(B, '\\ef'),                   % Emacs: forward-word
    new(Ev, event(0'f, D, 0, 0, 0x4, 1000)),  % Alt-F
    send(D, post_event, Ev),
    chain_list(Log, Calls),
    send(D, destroy).

test(label_box_focusses_part, Focus == browser) :-
    new(D, dialog),
    send(D, append, new(B, button(other))),
    send(D, append, new(LB, label_box(list))),
    send(LB, append, new(Browser, list_browser)),
    send(Browser, append, item),
    send(D, open),
    send(D, keyboard_focus, B),
    acc(LB, Acc),
    send(D?graphicals, for_some, message(@arg1, key, Acc)),
    get(D?keyboard_focus, class_name, Focus0),
    atom_concat(list_, Focus, Focus0),
    send(D, destroy).

test(bool_item_toggles, Value-Focus == (@off)-switch) :-
    new(D, dialog),
    send(D, append, new(B, button(other))),
    send(D, append, new(BI, bool_item(switch, @on))),
    send(D, open),
    send(D, keyboard_focus, B),
    alt(D, BI),
    get(BI, selection, Value),
    get(D?keyboard_focus, name, Focus),
    send(D, destroy).
test(slider_focusses, Focus == level) :-
    new(D, dialog),
    send(D, append, new(B, button(other))),
    send(D, append, new(S, slider(level, 0, 10, 5))),
    send(D, open),
    send(D, keyboard_focus, B),
    alt(D, S),
    get(D?keyboard_focus, name, Focus),
    send(D, destroy).
test(frame_dialogs_distinct, true(A1 \== A2)) :-
    new(F, frame),
    send(F, append, new(D1, dialog)),
    send(D1, append, button(save)),
    send(new(D2, dialog), below, D1),
    send(D2, append, button(select)),
    send(F, open),
    get(D1, member, save, B1), acc(B1, A1),
    get(D2, member, select, B2), acc(B2, A2),
    send(F, destroy).
test(page_reached_from_other_dialog,
     Focus-Calls-Clash == check-[]-false) :-
    new(Log, chain),
    new(F, frame),
    send(F, append, new(Top, dialog)),
    send(Top, append, button(stop, message(Log, append, stop))),
    send(new(TW, tabbed_window), below, Top),
    send(TW, append, new(Page, dialog), page),
    send(Page, append, new(S, slider(scale, 0, 10, 5))),
    send(Page, append, new(_, menu(check, marked))),
    get(Page, member, check, M),
    send_list(M, append, [a,b]),
    send(F, open),
    acc(S, AS), acc(M, AM),
    get(Top, member, stop, Stop), acc(Stop, AStop),
    (   memberchk(AStop, [AS, AM])
    ->  Clash = true
    ;   Clash = false
    ),
    alt(Top, M),                        % Alt-key typed in the top dialog
    get(Page?keyboard_focus, name, Focus),
    chain_list(Log, Calls),
    send(F, destroy).

test(clash_moves_to_next_word, Accs == ['\\es', '\\ea']) :-
    new(D, dialog),
    send(D, append, new(Stop, button(stop))),
    send(D, append, new(All, button(select_all)), right),
    send(D, layout),
    maplist([I,A]>>get(I, accelerator, A), [Stop, All], Accs),
    send(D, destroy).
%   A dialog may display graphicals that have no accelerator, such as
%   a box.  Asking them for one raised no_behaviour.

test(plain_graphical, Error == @nil) :-
    new(D, dialog),
    send(D, display, box(10, 10)),
    send(D, append, button(run)),
    send(@pce, last_error, @nil),
    pce_catch_error(no_behaviour, send(D, open)),
    get(@pce, last_error, Error),
    send(D, destroy).

:- end_tests(accelerators).

:- pce_begin_class(tacc_button, button).
mnemonic_name(_B, _Name:name, M:char_array) :<-
    M = m.
:- pce_end_class(tacc_button).

%   items(+Items, -Dialog, -Accs)
%
%   Open a dialog with Items and return the accelerator of each.

items(Items, D, Accs) :-
    new(D, dialog),
    maplist([Spec,I]>>(object(Spec) -> I = Spec ; new(I, Spec)), Items, Objs),
    forall(member(I, Objs), send(D, append, I)),
    send(D, open),
    maplist([I,A]>>get(I, accelerator, A), Objs, Accs).

:- begin_tests(accelerator_assignment).

test(fixed_kept, Accs-Fixed == ['\\ee', '\\es']-(@on)) :-
    new(Select, button(select)),
    new(Show, bool_item(show, @off)),
    send(Show, accelerator, 'S'),               % normalised to \es
    items([Select, Show], D, Accs),
    get(Show, accelerator_fixed, Fixed),
    send(D, destroy).
test(nil_kept, Acc == @nil) :-
    new(B, button(run)),
    send(B, accelerator, @nil),
    items([B], D, [Acc]),
    send(D, destroy).
test(default_reverts, Acc-Fixed == '\\er'-(@off)) :-
    new(B, button(run)),
    send(B, accelerator, x),
    items([B], D, _),
    send(B, accelerator, @default),
    get(B, accelerator, Acc),
    get(B, accelerator_fixed, Fixed),
    send(D, destroy).
test(no_letter_for_ok_and_cancel, Accs == [@nil, @nil, '\\er']) :-
    new(Ok, button(ok)),
    send(Ok, default_button, @on),
    items([Ok, button(cancel), button(run)], D, Accs),
    send(D, destroy).
test(buttons_first, Accs == ['\\ea', '\\es']) :-
    items([bool_item(select_all, @off), button(stop)], D, Accs),
    send(D, destroy).
test(mnemonic, M == s) :-
    get(save, mnemonic, M).
test(no_mnemonic, fail) :-
    get(foo, mnemonic, _).
test(preferred_letter, Accs == ['\\eo', '\\es']) :-
    items([button(sort), button(save)], D, Accs),
    send(D, destroy).
test(preferred_not_in_label, Acc == '\\eb') :-
    new(B, button(save)),
    send(B, label, 'Bewaar'),
    items([B], D, [Acc]),
    send(D, destroy).
test(mnemonic_name_override, Acc == '\\em') :-
    items([tacc_button(name)], D, [Acc]),
    send(D, destroy).
test(preferred_wins_across_dialogs, Accs == ['\\ee', '\\es']) :-
    new(F, frame),
    send(F, append, new(D1, dialog)),
    send(D1, append, new(Select, button(select))),
    send(D1, layout),                   % takes S automatically
    send(new(D2, dialog), below, D1),
    send(D2, append, new(Save, button(save))),
    send(F, open),
    maplist([I,A]>>get(I, accelerator, A), [Select, Save], Accs),
    send(F, destroy).
test(fixed_wins_across_dialogs, true(Acc \== '\\es')) :-
    new(F, frame),
    send(F, append, new(D1, dialog)),
    send(D1, append, new(Show, bool_item(show, @off))),
    send(Show, accelerator, s),
    send(new(D2, dialog), below, D1),
    send(D2, append, new(Save, button(save))),
    send(F, open),
    get(Save, accelerator, Acc),
    send(F, destroy).

:- end_tests(accelerator_assignment).

%   alt(+Window, +Item)
%
%   Type the accelerator of Item, Alt-<letter>, in Window.

alt(W, Item) :-
    acc(Item, Acc),
    sub_atom(Acc, 2, 1, 0, Char),
    char_code(Char, Code),
    new(Ev, event(Code, W, 0, 0, 0x4, 1000)),
    send(W, post_event, Ev).

native_dialog_style :-
    get(@pce, convert, key_binding, class, Class),
    get(Class, class_variable, dialog_style, Var),
    get(Var, value, Style),
    Style \== emacs.
