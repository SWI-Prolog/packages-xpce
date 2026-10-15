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

:- module(test_key_binding_browser,
          [ test_key_binding_browser/0
          ]).

/** <module> Test the key binding browser

Tests library(keybinding): which bindings it finds in a key_binding
table and the tables it inherits from, how it labels keys and what the
browser shows.  The tables are made for the test and named apart from
those of the system.
*/

:- use_module(xpce_test_sandbox).
:- initialization(sandbox_app_config(_), now).

:- use_module(library(pce)).
:- initialization(remove_sandbox_at_exit, now).
:- use_module(library(plunit)).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(yall)).
:- use_module(library(pairs)).
:- use_module(library(keybinding)).
:- use_module(library(pce_class_variable_editor), []).

test_key_binding_browser :-
    run_tests([ key_label,
                key_sequences,
                key_binding_entries,
                key_binding_browser,
                widget_key_bindings,
                user_key_bindings
              ]).


                 /*******************************
                 *           FIXTURES           *
                 *******************************/

%   Two classes whose methods implement the functions of the tables.
%   kbt_delegator implements bar itself and delegates the rest to an
%   kbt_target.

:- pce_begin_class(kbt_target, object, "Implements key functions").

foo(_) :-> "Do foo"::true.
top_b(_) :-> "Do top_b"::true.

:- pce_end_class(kbt_target).

:- pce_begin_class(kbt_delegator, object, "Delegates key functions").

variable(target, kbt_target, get, "Who does the work").

delegate_to(target).

initialise(D) :->
    send(D, slot, target, new(kbt_target)).

bar(_) :-> "Do bar"::true.

:- pce_end_class(kbt_delegator).

%!  tables(-Top) is det.
%
%   Top inherits from mid, which inherits from base:
%
%     | table | binds                                          |
%     |-------|------------------------------------------------|
%     | top   | b = top_b, d = prefix                          |
%     | mid   | a = none, b = qux                              |
%     | base  | a = foo, b = bar, c = baz, default insert_self |

tables(Top) :-
    fresh_table(base, [], Base),
    send(Base, function, a, foo),
    send(Base, function, b, bar),
    send(Base, function, c, baz),
    send(Base, default_function, insert_self),
    fresh_table(mid, [Base], Mid),
    send(Mid, function, a, none),
    send(Mid, function, b, qux),
    fresh_table(top, [Mid], Top),
    send(Top, function, b, top_b),
    send(Top, function, d, prefix).

%   fresh_table(+Role, +Supers, -Table)
%
%   A new table, named apart: a name is global and a table of an
%   earlier test would still bind its keys.

fresh_table(Role, Supers, Table) :-
    gensym(Role, Unique),
    atom_concat(kbt_, Unique, Name),
    Term =.. [key_binding, Name|Supers],
    new(Table, Term).

%   entries(+Table, +Target, -Sections)
%
%   The entries of Table as Role-[Key-Status, ...], where Role is the
%   name of the table without its prefix and number and Status ignores
%   the method.

entries(Table, Target, Sections) :-
    key_binding_entries(Table, Target, Sections0),
    maplist(section_summary, Sections0, Sections).

section_summary(section(Table, Entries), Role-Pairs) :-
    table_role(Table, Role),
    maplist(entry_summary, Entries, Pairs0),
    msort(Pairs0, Pairs).

entry_summary(entry(Key, _, Status0, _), Key-Status) :-
    (   Status0 = shadowed(By)
    ->  table_role(By, ByRole),
        Status = shadowed(ByRole)
    ;   Status = Status0
    ).

table_role(Table, Role) :-
    get(Table, name, Name),
    atom_concat(kbt_, Unique, Name),
    atom_codes(Unique, Codes),
    include([C]>>code_type(C, alpha), Codes, RoleCodes),
    atom_codes(Role, RoleCodes).

entry_method_class(Table, Target, Key, ClassName) :-
    key_binding_entries(Table, Target, Sections),
    member(section(_, Entries), Sections),
    member(entry(Key, _, Status, Method), Entries),
    Status \= shadowed(_),
    !,
    get(Method?context, name, ClassName).

%   browser(+Table, +Target, -Browser)
%
%   A browser showing Table.  Created without being shown.

browser(Table, Target, B) :-
    new(B, key_binding_browser),
    send(B, target, Target, Table).

%   shown(+Browser, -Labels)
%
%   The labels of the rows of Browser, without the leading spaces.

shown(B, Labels) :-
    get(B, pane_window, keys, L),
    get(L?dict?members, map, @arg1?label?value, Chain),
    chain_list(Chain, Labels0),
    maplist([L0, L]>>normalize_space(atom(L), L0), Labels0, Labels).

%   strip_number(+Label0, -Label)
%
%   The table of a test is named apart with a number, which the header
%   shows.  Drop it.

strip_number(Label0, Label) :-
    atom_codes(Label0, Codes0),
    exclude([C]>>code_type(C, digit), Codes0, Codes),
    atom_codes(Label2, Codes),
    normalize_space(atom(Label1), Label2),
    (   atom_concat('Kbt ', Rest, Label1)
    ->  upcase_first(Rest, Label)
    ;   Label = Label1
    ).

upcase_first(Atom, Upper) :-
    sub_atom(Atom, 0, 1, _, First),
    sub_atom(Atom, 1, _, 0, Rest),
    upcase_atom(First, U),
    atom_concat(U, Rest, Upper).


                 /*******************************
                 *            LABELS            *
                 *******************************/

:- begin_tests(key_label).

label(Key, Label) :-
    get(@pce, convert, editor, key_binding, KB),
    get(KB, key_label, Key, Label).

test(modifiers_joined, Label == 'Ctrl-Shift-A') :-
    label('\\C-\\S-a', Label).
test(sequence, Label == 'Ctrl-X Ctrl-S') :-
    label('\\C-x\\C-s', Label).
test(plain_keys_after_prefix, Label == 'Ctrl-X 4 f') :-
    label('\\C-x4f', Label).
test(named_key_after_prefix, Label == 'Ctrl-X Return') :-
    label('\\C-xRET', Label).
test(escape, Label == 'Esc') :-
    label('\\e', Label).
test(meta, Label == 'Alt-F') :-
    label('\\ef', Label).
test(function_key, Label == 'F1') :-
    label('<f1>', Label).

:- end_tests(key_label).


:- begin_tests(key_sequences).

%   sequence_editor(-Editor, -Table)
%
%   An editor whose keys go through Table, where unbound keys insert
%   themselves and Control-X is a prefix of one bound sequence.

sequence_editor(E, KB) :-
    new(E, editor),
    new(KB, key_binding),
    send(KB, default_function, insert_self),
    send(KB, function, '\\C-x', prefix),
    send(KB, function, '\\C-x2', newline).

type(KB, E, Keys) :-
    forall(member(K, Keys), send(KB, typed, K, E)).

contents(E, Text) :-
    get(E?text_buffer, contents, S),
    get(S, value, Text).

test(plain_key_inserts, Text == '1') :-
    sequence_editor(E, KB),
    type(KB, E, [0'1]),
    contents(E, Text).
test(bound_sequence, Text == '\n') :-
    sequence_editor(E, KB),
    type(KB, E, [24, 0'2]),
    contents(E, Text).
test(unbound_sequence_is_undefined, Text == '') :-
    sequence_editor(E, KB),
    type(KB, E, [24, 0'1]),
    contents(E, Text).
test(prefix_is_reset, Text == '1') :-
    sequence_editor(E, KB),
    type(KB, E, [24, 0'1, 0'1]),
    contents(E, Text).

:- end_tests(key_sequences).


                 /*******************************
                 *            MODEL             *
                 *******************************/

:- begin_tests(key_binding_entries).

test(inheritance,
     Sections == [ top-[b-active, d-reserved],
                   mid-[a-hidden, b-shadowed(top)],
                   base-[ a-shadowed(mid),
                          b-shadowed(top),
                          c-active,
                          @default-active
                        ]
                 ]) :-
    tables(Top),
    entries(Top, @nil, Sections).

%   kbt_target implements foo and top_b, but not baz and insert_self.
%   Reserved and hidden keys need no method.

test(unimplemented,
     [ Top == [b-active, d-reserved],
       Mid == [a-hidden, b-shadowed(top)],
       Base == [ a-shadowed(mid),
                 b-shadowed(top),
                 c-unimplemented,
                 @default-unimplemented
               ]
     ]) :-
    tables(TopTable),
    new(Target, kbt_target),
    entries(TopTable, Target, [top-Top, mid-Mid, base-Base]).

test(target_class, Status == unimplemented) :-
    tables(Top),
    get(@pce, convert, kbt_target, class, Class),
    key_binding_entries(Top, Class, [_, _, section(_, Entries)]),
    memberchk(entry(c, baz, Status, _), Entries).

test(delegation, Classes == [kbt_delegator, kbt_target]) :-
    tables(Top),
    new(Target, kbt_delegator),
    fresh_table(own, [Top], Own),
    send(Own, function, x, bar),
    send(Own, function, y, foo),
    entry_method_class(Own, Target, x, C1),
    entry_method_class(Own, Target, y, C2),
    Classes = [C1, C2].

test(message_function, ClassName == kbt_target) :-
    fresh_table(msg, [], Table),
    send(Table, function, m, message(@receiver, foo)),
    new(Target, kbt_target),
    entry_method_class(Table, Target, m, ClassName).

%   Depth first, as key_binding<-function searches: base comes before
%   right, and is searched once.

test(searched_once, Roles == [top, left, base, right]) :-
    fresh_table(base, [], Base),
    send(Base, function, a, foo),
    fresh_table(left, [Base], Left),
    fresh_table(right, [Base], Right),
    fresh_table(top, [Left, Right], Top),
    entries(Top, @nil, Sections),
    pairs_keys(Sections, Roles).

:- end_tests(key_binding_entries).


                 /*******************************
                 *           BROWSER            *
                 *******************************/

:- begin_tests(key_binding_browser).

test(overridden_hidden,
     Labels == [ 'Top', 'b top_b',
                 'd prefix Prefix for multikey command',
                 'Mid', 'a none Unbound',
                 'Base', 'Other keys insert_self', 'c baz'
               ]) :-
    tables(Top),
    browser(Top, @nil, B),
    shown(B, Labels0),
    maplist(strip_number, Labels0, Labels).

test(overridden_shown, Extra == 3) :-  % a and b of base, b of mid
    tables(Top),
    browser(Top, @nil, B),
    shown(B, Hidden),
    get(B, pane_window, top, D),
    get(D, member, options, M),
    send(M, selected, overridden, @on),
    send(B, fill),
    shown(B, Shown),
    length(Hidden, NH),
    length(Shown, NS),
    Extra is NS - NH.

test(filter, Labels == ['Base', 'c baz']) :-
    tables(Top),
    browser(Top, @nil, B),
    send(B, filter, regex(baz)),
    shown(B, Labels0),
    maplist(strip_number, Labels0, Labels).

test(filter_ignores_case, Labels == ['Base', 'c baz']) :-
    tables(Top),
    browser(Top, @nil, B),
    send(B, filter, regex('BAZ')),
    shown(B, Labels0),
    maplist(strip_number, Labels0, Labels).

test(group, Labels == ['a … e (5 keys) same']) :-
    fresh_table(group, [], Table),
    forall(member(K, [a,b,c,d,e]), send(Table, function, K, same)),
    browser(Table, @nil, B),
    shown(B, [_Header|Labels]).

test(no_group_below_minimum, Count == 4) :-
    fresh_table(group, [], Table),
    forall(member(K, [a,b,c,d]), send(Table, function, K, same)),
    browser(Table, @nil, B),
    shown(B, [_Header|Labels]),
    length(Labels, Count).

test(select_key, Function == top_b) :-
    tables(Top),
    browser(Top, @nil, B),
    send(B, select_key, b),
    get(B, current_row, row(_, _, entry(b, Function, _, _), _, _, _)).

test(select_grouped_key, Keys == [a,b,c,d,e]) :-
    fresh_table(group, [], Table),
    forall(member(K, [a,b,c,d,e]), send(Table, function, K, same)),
    browser(Table, @nil, B),
    send(B, select_key, c),
    get(B, current_row, row(_, _, entry(keys(Keys), _, _, _), _, _, _)).

test(header_shows_table, Shown == Top) :-
    tables(Top),
    browser(Top, @nil, B),
    send(B, select, header_1),
    get(B, pane_window, detail, D),
    get(D, row, table(Shown)).

% Selecting a row must not touch the window the pane_stack shows, which
% the frame gives the focus to when the user returns to the tab.
test(selection_keeps_current_window, Class == window) :-
    tables(Top),
    browser(Top, @nil, B),
    send(B, select_key, b),
    get(B, current, W),
    (   send(W, instance_of, window)
    ->  Class = window
    ;   get(W, class_name, Class)
    ).

test(detail_also_on, Also == 'Also on y') :-
    fresh_table(also, [], Table),
    send(Table, function, x, foo),
    send(Table, function, y, foo),
    browser(Table, @nil, B),
    send(B, select_key, x),
    get(B, pane_window, detail, D),
    get(D?graphicals, find,
        and(message(@arg1, instance_of, text),
            message(@arg1?string, prefix, 'Also')),
        T),
    get(T?string, value, Also).

:- end_tests(key_binding_browser).

                 /*******************************
                 *            WIDGETS           *
                 *******************************/

%   A dialog that handles keys before its items, as the find dialog of
%   PceEmacs does.

:- pce_begin_class(kbt_dialog, dialog, "Handles keys before its items").

cancel(_) :-> "Cancel"::true.

:- pce_end_class(kbt_dialog).

:- multifile key_binding:widget_extra_key_binding/3.

key_binding:widget_extra_key_binding(D, D, Table) :-
    send(D, instance_of, kbt_dialog),
    kbt_dialog_table(Table).

kbt_dialog_table(Table) :-
    get(@key_bindings, member, kbt_dialog, Table),
    !.
kbt_dialog_table(Table) :-
    new(Table, key_binding(kbt_dialog)),
    send(Table, function, '\\C-g', cancel).

:- begin_tests(widget_key_bindings).

%   table_name(+Graphical, -Name, -TargetClass)
%
%   The table of Graphical is Name and its functions are implemented by
%   an instance of TargetClass.

table_name(Gr, Name, TargetClass) :-
    key_binding:graphical_tables(Gr, Target, Table),
    get(Table, name, Name0),
    (   Name0 == @nil                   % per-instance: its super
    ->  get(Table?defaults, head, Super),
        get(Super, name, Name)
    ;   Name = Name0
    ),
    get(Target, class_name, TargetClass).

test(text_item, [Name, Class] == [text_item, text_item]) :-
    new(TI, text_item(name)),
    table_name(TI, Name, Class).
test(text_item_view, Name == text_item_view) :-
    new(TI, text_item(name)),
    send(TI, editable, @off),
    table_name(TI, Name, _).
test(editor, [Name, Class] == [editor, editor]) :-
    new(E, editor),
    table_name(E, Name, Class).
test(view, [Name, Class] == [editor, editor]) :-
    new(V, view),
    table_name(V, Name, Class).
test(browser, [Name, Class] == [list_browser, list_browser]) :-
    new(B, browser),
    table_name(B, Name, Class).
test(recogniser, Name == kbt_recogniser) :-
    new(TI, text_item(name)),
    new(KB, key_binding(kbt_recogniser, text_item)),
    send(TI, recogniser, KB),
    table_name(TI, Name, _).
%   A button handles no keys, and neither does the dialog it is in.

test(button_has_no_keys, fail) :-
    new(D, dialog),
    send(D, append, new(B, button(ok))),
    key_binding:graphical_tables(B, _, _).

%   An item without a table of its own uses that of its container.

test(no_table_uses_container, Name == editor) :-
    new(V, view),
    get(V, editor, E),
    send(E, display, new(B, box(10, 10))),
    table_name(B, Name, _).

%   The dialog handles Control-G before the text_item, and implements
%   its function.

test(extra_table_first, [Supers, Class] == [[kbt_dialog, text_item], kbt_dialog]) :-
    new(D, kbt_dialog),
    send(D, append, new(TI, text_item(name))),
    key_binding:graphical_tables(TI, Targets, Table),
    get(Table?defaults, map, @arg1?name, Chain),
    chain_list(Chain, Supers),
    key_binding_entries(Table, Targets, Sections),
    member(section(_, Entries), Sections),
    memberchk(entry('\\C-g', cancel, active, Method), Entries),
    !,
    get(Method?context, name, Class).

test(frame_focus, Name == text_item) :-
    new(F, frame),
    send(F, append, new(D, dialog)),
    send(D, append, new(TI, text_item(name))),
    send(D, keyboard_focus, TI),
    send(F, keyboard_focus, D),
    key_binding:focus_graphical(F, Gr),
    table_name(Gr, Name, _),
    send(F, destroy).

%   The editor of PceEmacs passes its keys to its mode.

test(emacs_editor, [Name, Class] == ['emacs$prolog', emacs_prolog_mode]) :-
    use_module(library(emacs/emacs)),
    new(B, emacs_buffer(@nil, '*kbt*')),
    send(B, mode, prolog),
    new(V, emacs_view(B)),
    get(V, editor, E),
    table_name(E, Name, Class).

%   The find dialog of the editor handles Control-G and Escape before
%   its text_item.

test(find_dialog, [First, Class] == [editor_find_dialog, editor_find_dialog]) :-
    use_module(library(emacs/find)),
    new(E, editor),
    new(D, editor_find_dialog(E)),
    get(D, member, find, TI),
    key_binding:graphical_tables(TI, Targets, Table),
    get(Table?defaults, head, FirstTable),
    get(FirstTable, name, First),
    key_binding_entries(Table, Targets, Sections),
    member(section(_, Entries), Sections),
    memberchk(entry('\\C-g', cancel, active, Method), Entries),
    !,
    get(Method?context, name, Class).

%   The window of the IDE switches tabs with Control-Page-Up and
%   Control-Page-Down before the editor in it sees these keys.

test(pane_frame, [First, Function, Class] ==
                 [pane_frame, next_tab, pane_frame]) :-
    use_module(library(pane_frame)),
    new(V, view),
    new(F, pane_frame(@default, @default, V)),
    get(V, editor, E),
    key_binding:graphical_tables(E, Targets, Table),
    get(Table?defaults, head, FirstTable),
    get(FirstTable, name, First),
    key_binding_entries(Table, Targets, Sections),
    member(section(_, Entries), Sections),
    memberchk(entry('\\C-<page_down>', Function, active, Method), Entries),
    !,
    get(Method?context, name, Class),
    send(F, destroy).

:- end_tests(widget_key_bindings).


                 /*******************************
                 *        USER BINDINGS         *
                 *******************************/

:- begin_tests(user_key_bindings,
               [ setup(reset_user_bindings),
                 cleanup(reset_user_bindings)
               ]).

%   The keys the user binds in a table are kept in the class variable
%   key_binding.<table>, which is global: each test starts without
%   them.

table_variable(Name, Var) :-
    get(@pce, convert, key_binding, class, Class),
    get(Class, class_variable, Name, Var).

reset_user_bindings :-
    forall(( pce_keybinding:table_class_variable(Name),
             table_variable(Name, Var)
           ),
           send(Var, value, new(chain))),
    retractall(key_binding:saved_bindings(_, _)).

user_bindings(Triples) :-
    findall(t(Name, Key, Function),
            ( pce_keybinding:table_class_variable(Name),
              table_variable(Name, Var),
              get(Var, value, Chain),
              chain_list(Chain, Bindings),
              member(B, Bindings),
              get(B, name, Key),
              get(B, value, Function)
            ),
            Triples).

function(Table, Key, Function) :-
    (   get(Table?bindings, value, Key, Function0)
    ->  Function = Function0
    ;   Function = '-'
    ).

test(bind_and_revert, [F1-F2-U1-U2 == bar-foo-[t(Name, x, bar)]-[]]) :-
    fresh_table(user, [], T),
    get(T, name, Name),
    send(T, function, x, foo),
    send(T, user_binding, x, bar),
    function(T, x, F1),
    user_bindings(U1),
    send(T, user_binding, x, @nil),
    function(T, x, F2),
    user_bindings(U2).

test(revert_added_key, [F1-F2 == bar-'-']) :-
    fresh_table(user, [], T),
    send(T, user_binding, y, bar),
    function(T, y, F1),
    send(T, user_binding, y, @nil),
    function(T, y, F2).

test(changed_twice_reverts_original, F == foo) :-
    fresh_table(user, [], T),
    send(T, function, x, foo),
    send(T, user_binding, x, bar),
    send(T, user_binding, x, baz),
    send(T, user_binding, x, @nil),
    function(T, x, F).

test(applied_twice_reverts_original, F == foo) :-
    fresh_table(user, [], T),
    send(T, function, x, foo),
    send(T, user_binding, x, bar),
    send(T, apply_user_bindings),
    send(T, user_binding, x, @nil),
    function(T, x, F).

%   A table that is created after the Defaults file was read finds its
%   keys there.  A `$` in its name is written as `_`.

test(applied_to_new_table, [F1-F2 == foo-bar]) :-
    gensym(kbt_late, Unique),
    atom_concat(Unique, '$x', Name),
    atom_concat(Unique, '_x', VarName),
    tmp_file(defaults, File),
    setup_call_cleanup(
        setup_call_cleanup(
            open(File, write, Out),
            format(Out, "key_binding.~w:\t[ a := foo, \\~n\t  b := bar \\~n\t]~n",
                   [VarName]),
            close(Out)),
        send(@pce, load_defaults, File),
        delete_file(File)),
    new(T, key_binding(Name)),
    send(T, apply_preferences),
    function(T, a, F1),
    function(T, b, F2).

test(user_binding, F == bar) :-
    fresh_table(user, [], T),
    send(T, user_binding, x, bar),
    get(T, user_binding, x, F).

test(unbind_added_key_removes_it, [F1-F2 == bar-'-']) :-
    fresh_table(user, [], T),
    send(T, user_binding, y, bar),
    function(T, y, F1),
    key_binding:unbind_key(T, y),
    function(T, y, F2).

test(unbind_bound_key_binds_none, F == none) :-
    fresh_table(user, [], T),
    send(T, function, x, foo),
    key_binding:unbind_key(T, x),
    function(T, x, F).

%   Changing the style of the text items undoes the changes of the old
%   style and applies those of the new one.  The keys of the user stay.

test(survives_style_change, [F1-F2 == my_function-select_all]) :-
    get(@pce, convert, text_item, key_binding, T),
    pce_keybinding:current_style(dialog, Style),
    get(T, function, '\\C-a', Original),
    setup_call_cleanup(
        send(T, user_binding, '\\C-a', my_function),
        ( pce_keybinding:set_keybinding_style(dialog, emacs),
          pce_keybinding:set_keybinding_style(dialog, cua),
          function(T, '\\C-a', F1)
        ),
        ( send(T, user_binding, '\\C-a', @nil),
          pce_keybinding:set_keybinding_style(dialog, Style)
        )),
    pce_keybinding:set_keybinding_style(dialog, cua),
    function(T, '\\C-a', F2),
    pce_keybinding:set_keybinding_style(dialog, Style),
    assertion(get(T, function, '\\C-a', Original)).

%   The keys of a table are saved in the Defaults file as a list of
%   Key := Function, one per line, which xpce reads back.

test(defaults_round_trip,
     [ Entry-Back == "key_binding.kbt_round_x:\t\c
                     [ '\\ef' := forward_word, \\\n\c
                     \t  \"\\C-c'\" := none \\\n\c
                     \t]" -
                     [ '\\ef'-forward_word,
                       '\\C-c\''-none
                     ]
     ]) :-
    new(T, key_binding('kbt_round$x')),
    send(T, user_bindings, ['\\ef'-forward_word, '\\C-c\''-none]),
    get(T, user_bindings_variable, Var),
    get(Var, value, Chain),
    tmp_file(defaults, File),
    get(@pce, convert, key_binding, class, Class),
    setup_call_cleanup(
        pce_class_variable_editor:save_value(
            File, Class, 'key_binding.kbt_round_x', kbt_round_x, Chain),
        read_file_to_string(File, String, []),
        delete_file(File)),
    sub_string(String, Start, _, _, "key_binding.kbt_round_x:"),
    !,
    sub_string(String, Start, _, 0, Entry0),
    split_string(Entry0, "", "\n", [Entry]),
    send(T, user_bindings, []),
    sub_string(Entry, B, _, _, "["),
    !,
    sub_string(Entry, B, _, 0, Text),
    split_string(Text, "\n", "", Lines),
    maplist(pce_class_variable_editor:continued_line, Lines, Parts),
    atomic_list_concat(Parts, OneLine),
    get(Var, convert_string, OneLine, Read),
    send(Var, value, Read),
    get(T, user_bindings, Back).

%   Saving a table without keys of the user removes its entry.

test(saving_none_removes_entry, [Before-After == true-false]) :-
    new(T, key_binding(kbt_gone)),
    send(T, user_bindings, [a-foo]),
    get(T, user_bindings_variable, Var),
    get(@pce, convert, key_binding, class, Class),
    tmp_file(defaults, File),
    setup_call_cleanup(
        ( save(File, Class, Var),
          has_entry(File, Before),
          send(T, user_bindings, []),
          save(File, Class, Var),
          has_entry(File, After)
        ),
        true,
        delete_file(File)).

save(File, Class, Var) :-
    get(Var, value, Chain),
    pce_class_variable_editor:save_value(
        File, Class, 'key_binding.kbt_gone', kbt_gone, Chain).

has_entry(File, Has) :-
    read_file_to_string(File, String, []),
    (   sub_string(String, _, _, _, "key_binding.kbt_gone:")
    ->  Has = true
    ;   Has = false
    ).

test(reverting_last_removes_entry, Size == 0) :-
    reset_user_bindings,
    fresh_table(user, [], T),
    send(T, user_binding, x, bar),
    send(T, user_binding, x, @nil),
    get(T, user_bindings_variable, Var),
    get(Var?value, size, Size).

%   The browser applies a change at once.  Revert goes back to the keys
%   as they were before the first change; then there is nothing to
%   save.

test(revert, [F1-F2-M1-M2 == bar-foo-true-false]) :-
    fresh_table(user, [], T),
    send(T, function, x, foo),
    browser(T, @nil, B),
    send(B, user_bind, T, x, bar),
    function(T, x, F1),
    modified(M1),
    send(B, revert),
    function(T, x, F2),
    modified(M2).

modified(Modified) :-
    (   key_binding:modified_table(_)
    ->  Modified = true
    ;   Modified = false
    ).

%   Binding a key sequence

test(prefix_blocked, OK == @off) :-
    fresh_table(user, [], T),
    send(T, function, '\\C-x', foo),
    key_binding:sequence_status(T, ['\\C-x', a], OK, _).

test(prefix_free, OK == @on) :-
    fresh_table(user, [], T),
    key_binding:sequence_status(T, ['\\C-x', a], OK, _).

test(binds_prefix, [F1-F2 == prefix-bar]) :-
    fresh_table(user, [], T),
    key_binding:bind_prefixes(T, ['\\C-x', a]),
    send(T, user_binding, '\\C-xa', bar),
    function(T, '\\C-x', F1),
    function(T, '\\C-xa', F2).

%   The functions a key can invoke

test(text_item_functions, [Has-Hasnt == [yes, yes, yes]-[no, no, no, no]]) :-
    get(@pce, convert, text_item, class, Class),
    command_functions(Class, Names),
    maplist(has(Names), [forward_char, select_all, kill_line], Has),
    maplist(has(Names), [initialise, draw, geometry, label_width], Hasnt).

test(editor_plumbing, [Has-Hasnt == [yes, yes]-[no, no, no, no, no, no]]) :-
    get(@pce, convert, editor, class, Class),
    command_functions(Class, Names),
    maplist(has(Names), [forward_word, kill_line], Has),
    maplist(has(Names), [geometry, request_geometry, unlink, compute,
                         font, hover_fragment_icon], Hasnt).

%   A PceEmacs mode prompts for the arguments that can be typed.  It
%   offers the commands of its menus and those bound to a key, while
%   M-x also accepts its other methods.

test(emacs_commands, [Has-Hasnt-Typed-NotTyped ==
                      [yes, yes, yes, yes, yes, yes]-[no, no, no]-
                      [yes, yes]-[no]]) :-
    emacs_mode(M),
    command_functions(M, Names),
    maplist(has(Names),
            [ find_definition, forward_word, mode,
              git_grep,                 % only in a menu
              what_module,              % declared a command
              select_all                % declared in fundamental mode
            ], Has),
    maplist(has(Names), [setup_mode, geometry, typed], Hasnt),
    maplist(typed(M), [gdb, setup_mode], Typed),
    maplist(typed(M), [event], NotTyped).

test(m_x_completes_commands, Same == true) :-
    emacs_mode(M),
    command_functions(M, Names),
    get(M, command_names, Chain),
    chain_list(Chain, MX),
    (   MX == Names
    ->  Same = true
    ;   Same = false
    ).

emacs_mode(M) :-
    use_module(library(emacs/emacs)),
    new(B, emacs_buffer(@nil, '*kbt-commands*')),
    send(B, mode, prolog),
    new(V, emacs_view(B)),
    get(V, editor, E),
    get(E, mode, M).

typed(M, Name, YesNo) :-
    (   key_function(M, Name)
    ->  YesNo = yes
    ;   YesNo = no
    ).

has(Names, Name, YesNo) :-
    (   memberchk(Name, Names)
    ->  YesNo = yes
    ;   YesNo = no
    ).

test(forwarded_function_active, Status == active) :-
    get(@pce, convert, text_item, key_binding, Table),
    get(@pce, convert, text_item, class, Class),
    key_binding_entries(Table, Class, [section(_, Entries)|_]),
    memberchk(entry(_, forward_char, Status, _), Entries),
    !.

%   The browser

test(unbind_in_browser, [Label-Style == 'x none Unbound'-user]) :-
    fresh_table(user, [], T),
    send(T, function, x, foo),
    send(T, function, y, foo),
    browser(T, @nil, B),
    send(B, select_key, x),
    get(B, pane_window, detail, D),
    send(D, unbind),
    get(B, pane_window, keys, L),
    get(L, selection, DI),
    get(DI, style, Style),
    get(DI?label, value, Label0),
    normalize_space(atom(Label1), Label0),
    strip_number(Label1, Label).

test(changed_keys_not_grouped, Labels == ['User', 'a none Unbound',
                                          'b … f ( keys) none Unbound']) :-
    fresh_table(user, [], T),
    forall(member(K, [b,c,d,e,f]), send(T, function, K, none)),
    send(T, user_binding, a, none),
    browser(T, @nil, B),
    shown(B, Labels0),
    maplist(strip_number, Labels0, Labels).

test(revert_in_browser, F == foo) :-
    fresh_table(user, [], T),
    send(T, function, x, foo),
    browser(T, @nil, B),
    send(B, select_key, x),
    get(B, pane_window, detail, D),
    send(D, unbind),
    send(D, revert),
    function(T, x, F).

test(rebind_in_browser, [Old-Prefix-New == none-prefix-foo]) :-
    fresh_table(user, [], T),
    send(T, function, x, foo),
    browser(T, @nil, B),
    send(B, select_key, x),
    get(B, pane_window, detail, D),
    send(D, capture),
    get(D, member, key_capture_item, I),
    send(I, slot, keys, ['\\C-c', z]),
    send(D, bind_captured),
    function(T, x, Old),
    function(T, '\\C-c', Prefix),
    function(T, '\\C-cz', New).

test(function_completions, [Has == [yes, yes]]) :-
    fresh_table(user, [], T),
    send(T, function, x, foo),
    get(@pce, convert, text_item, class, Class),
    browser(T, Class, B),
    send(B, select_key, x),
    get(B, pane_window, detail, D),
    send(D, choose_function),
    get(D, member, function, I),
    get(I, value_set, Set),
    chain_list(Set, Names),
    maplist(has(Names), [forward_char, select_all], Has).

%   Selecting the header of a named table offers to add a key: type the
%   key, then the function.

test(add_in_browser, [Prefix-New == prefix-forward_char]) :-
    fresh_table(user, [], T),
    send(T, function, x, foo),
    get(@pce, convert, text_item, class, Class),
    browser(T, Class, B),
    get(B, rows, Rows),
    memberchk(header(Id, T, _), Rows),
    send(B, select, Id),
    get(B, pane_window, detail, D),
    get(D, member, add_key, _),
    send(D, add_key),
    get(D, member, key_capture_item, I),
    send(I, slot, keys, ['\\C-c', z]),
    send(D, bind_captured),
    get(D, member, function, FI),
    send(FI, selection, forward_char),
    send(D, bind_function),
    function(T, '\\C-c', Prefix),
    function(T, '\\C-cz', New).

test(rebind_refused, [fail]) :-
    fresh_table(user, [], T),
    send(T, function, x, foo),
    send(T, function, '\\C-c', bar),
    browser(T, @nil, B),
    send(B, select_key, x),
    get(B, pane_window, detail, D),
    send(D, capture),
    get(D, member, key_capture_item, I),
    send(I, slot, keys, ['\\C-c', z]),
    send(D, bind_captured).

:- end_tests(user_key_bindings).
