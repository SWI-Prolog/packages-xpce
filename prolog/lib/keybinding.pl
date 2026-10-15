/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org/packages/xpce/
    Copyright (c)  1985-2026, University of Amsterdam
                              SWI-Prolog Solutions b.v.
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

:- module(key_binding,
          [ show_key_bindings/1,        % +Object
            show_key_bindings/2,        % +Object, +Key
            select_key_binding/1,       % +Key
            key_binding_entries/3,      % +Table, +Target, -Sections
            command_functions/2,        % +Target, -Names
            key_function/2              % +Target, +Name
          ]).
:- use_module(library(pce)).
:- use_module(library(pane_frame)).
:- use_module(library(swi_ide), []).
:- use_module(library(pce_filter_item), []).
:- use_module(library(pce_pin_button), []).
:- use_module(library(help_message), []).
:- pce_autoload(man_html_card, library(pce_html_manual)).
:- use_module(library(pce_util), [chain_list/2, center_on/2]).
:- autoload(library(apply),
            [maplist/3, include/3, foldl/4, convlist/3, partition/4]).
:- autoload(library(pairs), [pairs_keys/2]).
:- autoload(library(ordsets), [ord_memberchk/2]).
:- autoload(library(assoc),
            [empty_assoc/1, get_assoc/3, put_assoc/4]).
:- autoload(library(atom), [restyle_identifier/3]).
:- autoload(library(edit), [edit/1]).
:- autoload(library(lists),
            [append/3, append/2, member/2, reverse/2, last/2, list_to_set/2]).
:- autoload(library(portray_object), [portray_object/2]).
:- autoload(library(pce_class_variable_editor),
            [save_class_variable/3, user_defaults_file/1]).
:- autoload(library(swi_preferences), [prolog_edit_preferences/1]).

/** <module> Browse key bindings

The keys of an editor, a PceEmacs mode or any other key_binding table
are shown as a tool pane.  The keys are grouped by the table that binds
them, starting with the table of the object itself and followed by the
tables it inherits from.  A key that a table before it binds as well is
_overridden_, and only shown on request.  Selecting a key shows the
manual entry of the method it invokes, and controls to bind the key to
another function, the function to another key, or to unbind the key.
These changes go to a named table and apply at once.  Save writes them
to the Defaults file as key_binding.<table> (see key_binding<-user_bindings).

The pane is reused for the next object, unless it is pinned.
*/

:- dynamic key_binding_browser/1 as volatile.

:- multifile
    widget_key_binding/3,       % +Graphical, -Target, -Table
    widget_extra_key_binding/3, % +Container, -Target, -Table
    command_function/2.         % +Target, +Name

%!  widget_key_binding(+Graphical, -Target, -Table) is semidet.
%
%   Hook to find the key_binding Table that handles the keys typed in
%   Graphical, where Target implements the functions of Table.  Defined
%   for objects that handle their keys in a way the defaults below do
%   not know about.  For example, the editor of PceEmacs passes its keys
%   to its mode.

%!  widget_extra_key_binding(+Container, -Target, -Table) is semidet.
%
%   Hook for a Container (a device or window) whose ->event handles
%   some keys typed in the graphicals it holds using Table, before these
%   see them.  Target implements the functions of Table.  Container is
%   also the frame, whose ->shortcut handles keys typed in its windows.

%!  command_function(+Target, +Name) is semidet.
%
%   Hook that makes Name, a method of Target defined in Prolog, a
%   command to offer for binding a key, although no table binds it.
%   For example, PceEmacs offers the commands of its menus.

%!  show_key_bindings(+Object) is det.
%!  show_key_bindings(+Object, +Key) is det.
%
%   Show the key bindings of Object, selecting Key if it is bound.
%   Object is one of
%
%     - A frame.  Show the keys of the graphical that has the keyboard
%       focus.
%     - A graphical.  Show the keys typed in it.  See
%       widget_key_binding/3 and widget_extra_key_binding/3.
%     - An object with a get method `bindings` that returns its
%       key_binding table, e.g., a PceEmacs mode.  The methods are those
%       of Object.
%     - A key_binding table or the name of one.  If there is a class
%       of the same name, the methods are those of this class.

show_key_bindings(Object) :-
    show_key_bindings(Object, @nil).

show_key_bindings(Object, Key) :-
    target_table(Object, Target, Table),
    reusable_browser(B),
    (   Target = targets(_, _)
    ->  send(B, targets, Target, Table)
    ;   send(B, target, Target, Table)
    ),
    send(B, expose),
    (   Key == @nil
    ->  true
    ;   send(B, select_key, Key)
    ).

target_table(Object, Target, Table) :-
    object(Object),
    send(Object, instance_of, key_binding),
    !,
    Table = Object,
    table_class(Table, Target).
target_table(Object, Target, Table) :-
    atom(Object),
    !,
    get(@pce, convert, Object, key_binding, Table),
    table_class(Table, Target).
target_table(Frame, Target, Table) :-
    send(Frame, instance_of, frame),
    !,
    (   focus_graphical(Frame, Gr)
    ->  target_table(Gr, Target, Table)
    ;   send(Frame, report, warning, 'Nothing has the keyboard focus'),
        fail
    ).
target_table(Gr, Target, Table) :-
    send(Gr, instance_of, graphical),
    !,
    (   graphical_tables(Gr, Target, Table)
    ->  true
    ;   send(Gr, report, warning, 'No key bindings for %O', Gr),
        fail
    ).
target_table(Object, Object, Table) :-
    send(Object, has_get_method, bindings),
    get(Object, bindings, Table).

%   focus_graphical(+Frame, -Graphical)
%
%   Graphical has the keyboard focus of Frame.  The focus of a frame is
%   a window, which may pass it on to a graphical it holds, which may be
%   a window passing it on in turn.  A window of the IDE starts from the
%   pane the user is working in: the menu bar takes the focus when it is
%   used to ask for the keys.

focus_graphical(Frame, Gr) :-
    (   send(Frame, has_get_method, current_pane),
        get(Frame, current_pane, W)
    ->  true
    ;   get(Frame, keyboard_focus, W)
    ),
    W \== @nil,
    focus_descend(W, Gr).

focus_descend(Gr0, Gr) :-
    send(Gr0, instance_of, window),
    get(Gr0, keyboard_focus, Gr1),
    Gr1 \== @nil,
    Gr1 \== Gr0,
    !,
    focus_descend(Gr1, Gr).
focus_descend(Gr, Gr).

%   graphical_tables(+Graphical, -Targets, -Table)
%
%   Table holds the bindings of the keys typed in Graphical.  If the
%   containers of Graphical handle some keys before it, Table is an
%   unnamed table that inherits from the tables of the containers,
%   outermost first, and the table of the graphical.  Targets is then
%   targets(Pairs, Default) -- see key_binding_entries/3.  If Graphical
%   has no table of its own, use the table of the first container that
%   has one.

graphical_tables(Gr, Targets, Table) :-
    own_table(Gr, Target, Own),
    !,
    containers(Gr, Containers),
    convlist(extra_table, Containers, Extras),
    (   Extras == []
    ->  Targets = Target,
        Table = Own
    ;   pairs_keys(Extras, ExtraTables),
        append(ExtraTables, [Own], Supers),
        Term =.. [key_binding, @default|Supers],
        new(Table, Term),
        Targets = targets([Own-Target|Extras], Target)
    ).
graphical_tables(Gr, Target, Table) :-
    get(Gr, contained_in, Container),
    send(Container, instance_of, graphical),
    graphical_tables(Container, Target, Table).

own_table(Gr, Target, Table) :-
    widget_key_binding(Gr, Target, Table),
    !.
own_table(Gr, Target, Table) :-
    default_widget_key_binding(Gr, Target, Table).

extra_table(Container, Table-Target) :-
    widget_extra_key_binding(Container, Target, Table),
    !.

%   containers(+Graphical, -Containers)
%
%   Containers are the devices and windows Graphical is in, outermost
%   first, starting with the frame.

containers(Gr, Containers) :-
    containers(Gr, [], Containers).

containers(Gr, Acc, Containers) :-
    get(Gr, contained_in, C),
    send(C, instance_of, graphical),
    !,
    containers(C, [C|Acc], Containers).
containers(Gr, Acc, [Frame|Acc]) :-
    get(Gr, contained_in, Frame),
    send(Frame, instance_of, frame),
    !.
containers(_, Containers, Containers).

%   default_widget_key_binding(+Graphical, -Target, -Table)
%
%   The tables of the classes of the kernel.  A key_binding used as a
%   recogniser comes first: the prompters of PceEmacs and the IDE use
%   one on a text_item, that inherits from its table.

default_widget_key_binding(Gr, Gr, Table) :-
    get(Gr, all_recognisers, @off, Recognisers),
    get(Recognisers, find, message(@arg1, instance_of, key_binding), Table),
    !.
default_widget_key_binding(Gr, Gr, Table) :-
    (   send(Gr, instance_of, editor)
    ;   send(Gr, instance_of, terminal_image)
    ),
    !,
    get(Gr, bindings, Table).
default_widget_key_binding(Gr, Gr, Table) :-
    send(Gr, instance_of, list_browser),
    !,
    get(Gr, key_binding, Table).
default_widget_key_binding(Gr, Target, Table) :-
    send(Gr, instance_of, browser),
    !,
    get(Gr, list_browser, LB),
    own_table(LB, Target, Table).
default_widget_key_binding(Gr, Target, Table) :-
    send(Gr, instance_of, view),
    !,
    get(Gr, editor, E),
    own_table(E, Target, Table).
default_widget_key_binding(Gr, Gr, Table) :-
    send(Gr, instance_of, text_item),
    !,
    (   get(Gr, editable, @on)
    ->  Name = text_item
    ;   Name = text_item_view
    ),
    get(@pce, convert, Name, key_binding, Table).
default_widget_key_binding(Gr, Gr, Table) :-
    send(Gr, instance_of, text),
    get(@pce, convert, text, key_binding, Table).

%   The tables of the C classes are named after the class.

table_class(Table, Class) :-
    get(Table, name, Name),
    atom(Name),
    table_class_name(Name, ClassName),
    catch(get(@pce, convert, ClassName, class, Class), _, fail),
    !.
table_class(_, @nil).

table_class_name(terminal, terminal_image) :- !.
table_class_name(Name, Name).

reusable_browser(B) :-
    key_binding_browser(B),
    get(B, pinned, @off),
    !.
reusable_browser(B) :-
    new(B, key_binding_browser).

%!  select_key_binding(+Key) is semidet.
%
%   Select Key in the key binding browser that is open, if any.  This
%   is used by ``describe key'' to show the details of the key.

select_key_binding(Key) :-
    key_binding_browser(B),
    get(B, pinned, @off),
    !,
    send(B, select_key, Key).


                 /*******************************
                 *            MODEL             *
                 *******************************/

%!  key_binding_entries(+Table, +Target, -Sections) is det.
%
%   Sections describes the bindings of the key_binding Table and the
%   tables it inherits from, in the order they are searched for a
%   key.  Each element is a term section(Table, Entries), where Entries
%   is a list
%
%       entry(Key, Function, Status, Method)
%
%   Key is a key or key sequence, or `@default` for the function that
%   handles keys that are not bound.  Function is a name or a code
%   object, Status is one of
%
%     - `active`
%       The key invokes Function.
%     - shadowed(By)
%       A table before this one binds the key.  By is this table.
%     - `hidden`
%       Function is `none`, which makes the key unbound.
%     - `reserved`
%       Function is handled by the key binding itself, e.g., `prefix`.
%     - `unimplemented`
%       Target has no method Function.
%
%   Method is the send method that implements Function on Target, or
%   `@nil`.  Target is an object or a class whose methods implement the
%   functions, or `@nil` if this is unknown.  If the functions of some
%   tables are implemented by another object, Target is a term
%   targets(Pairs, Default), where Pairs is a list Table-Target and
%   Default is the target of the other tables.

key_binding_entries(Table, Target, Sections) :-
    table_order(Table, Tables),
    empty_assoc(Seen),
    table_sections(Tables, Target, Seen, Sections).

%   table_order(+Table, -Tables)
%
%   Tables is Table and the tables it inherits from in the order
%   key_binding<-function searches them: depth first through
%   <-defaults.  A table that is reached twice is searched once.

table_order(Table, Tables) :-
    table_order([Table], [], Rev),
    reverse(Rev, Tables).

table_order([], Seen, Seen).
table_order([T|Ts], Seen0, Seen) :-
    (   memberchk(T, Seen0)
    ->  table_order(Ts, Seen0, Seen)
    ;   get(T, defaults, Chain),
        chain_list(Chain, Supers),
        table_order(Supers, [T|Seen0], Seen1),
        table_order(Ts, Seen1, Seen)
    ).

table_sections([], _, _, []).
table_sections([Table|Tables], Target, Seen0,
               [section(Table, Entries)|Sections]) :-
    table_pairs(Table, Pairs),
    table_target(Target, Table, TableTarget),
    table_entries(Pairs, Table, TableTarget, Seen0, Seen, Entries),
    table_sections(Tables, Target, Seen, Sections).

table_target(targets(Pairs, Default), Table, Target) :-
    !,
    (   memberchk(Table-Target0, Pairs)
    ->  Target = Target0
    ;   Target = Default
    ).
table_target(Target, _, Target).

%   table_pairs(+Table, -Pairs)
%
%   Pairs is a list Key-Function of the bindings of Table, followed
%   by @default-Function if Table has a default function.

table_pairs(Table, Pairs) :-
    get(Table?bindings?members, map,
        create(tuple, @arg1?name, @arg1?value), Tuples),
    chain_list(Tuples, TupleList),
    maplist(tuple_pair, TupleList, Pairs0),
    get(Table, default_function, Default),
    (   Default == @nil
    ->  Pairs = Pairs0
    ;   append(Pairs0, [@default-Default], Pairs)
    ).

tuple_pair(Tuple, Key-Function) :-
    get(Tuple, first, Key),
    get(Tuple, second, Function).

table_entries([], _, _, Seen, Seen, []).
table_entries([Key-Function|Pairs], Table, Target, Seen0, Seen,
              [entry(Key, Function, Status, Method)|Entries]) :-
    function_method(Target, Function, Method, Status0),
    (   get_assoc(Key, Seen0, By)
    ->  Status = shadowed(By),
        Seen1 = Seen0
    ;   Status = Status0,
        put_assoc(Key, Seen0, Table, Seen1)
    ),
    table_entries(Pairs, Table, Target, Seen1, Seen, Entries).

%   function_method(+Target, +Function, -Method, -Status)
%
%   Method is the send method invoked by Function and Status is the
%   status of Function if it is not overridden.

function_method(_, none, @nil, hidden) :-
    !.
function_method(Target, Function, Method, active) :-
    atom(Function),
    send_method(Target, Function, Method),
    !.
function_method(_, Function, @nil, reserved) :-
    atom(Function),
    reserved_binding(Function, _),
    !.
function_method(@nil, Function, @nil, active) :-
    atom(Function),
    !.
function_method(_, Function, @nil, unimplemented) :-
    atom(Function),
    !.
function_method(Target, Function, Method, Status) :-
    send(Function, instance_of, message),
    get(Function, receiver, Receiver),
    (   Receiver == @receiver
    ;   Receiver == Target
    ),
    get(Function, selector, Selector),
    atom(Selector),
    !,
    function_method(Target, Selector, Method, Status).
function_method(_, _, @nil, active).

%   send_method(+Target, +Selector, -Method) is semidet.
%
%   Method implements Selector on Target, an object or class.  For an
%   object this includes the methods it delegates to.

send_method(@nil, _, _) :-
    !,
    fail.
send_method(Target, Selector, Method) :-
    send(Target, instance_of, class),
    get(Target, send_method, Selector, Method),
    !.
send_method(Target, Selector, Method) :-
    \+ send(Target, instance_of, class),
    get(Target, send_method, Selector, tuple(_, Method)),
    !.
send_method(Target, Selector, Method) :-
    target_class(Target, Class),
    forwards_to(Class, To),
    get(To, send_method, Selector, Method).

target_class(Target, Class) :-
    (   send(Target, instance_of, class)
    ->  Class = Target
    ;   get(Target, class, Class)
    ).

%   forwards_to(+Class, -To) is semidet.
%
%   Instances of Class pass the messages they do not implement to an
%   instance of class To using ->catch_all.  A text_item passes the
%   editing functions to its text.

forwards_to(Class, To) :-
    send(Class, is_a, text_item),
    get(@pce, convert, text, class, To).

%!  reserved_binding(+Name, -Summary)
%
%   Functions that are handled by the key_binding object itself.

reserved_binding(keyboard_quit,         'Abort current sequence').
reserved_binding(prefix,                'Prefix for multikey command').
reserved_binding(prefix_or_copy,        'Prefix if there is no selection, \c
                                         else copy it').
reserved_binding(prefix_or_cut,         'Prefix if there is no selection, \c
                                         else cut it').
reserved_binding(quoted_insert,         'Insert the next key literally').
reserved_binding(digit_argument,        'Construct numeric argument').
reserved_binding(universal_argument,    'Start numeric argument').


                 /*******************************
                 *             ROWS             *
                 *******************************/

%   The browser shows rows, made from the sections once for each
%   object shown:
%
%       header(Id, Table, Label)
%       row(Id, Table, Entry, KeyLabel, FunctionText, Summary)

sections_rows(Sections, Rows) :-
    sections_rows(Sections, 1, Rows).

sections_rows([], _, []).
sections_rows([section(Table, Entries)|Sections], I0,
              [header(HId, Table, Label)|Rows]) :-
    table_label(Table, Label),
    row_id(header, I0, HId),
    maplist(entry_row(Table), Entries, Rows0),
    sort(4, @=<, Rows0, Sorted),        % by key label
    group_rows(Sorted, Grouped),
    I1 is I0+1,
    foldl(number_row, Grouped, I1, I),
    append(Grouped, Rest, Rows),
    sections_rows(Sections, I, Rest).

entry_row(Table, Entry, row(_, Table, Entry, KeyLabel, FText, Summary)) :-
    Entry = entry(Key, Function, Status, Method),
    key_label(Table, Key, KeyLabel),
    function_text(Function, FText),
    entry_summary(Status, Function, Method, Summary).

%   group_rows(+Rows, -Grouped)
%
%   A table that binds many keys to the same function, such as all
%   printable characters to `insert_self`, shows these as a single row.
%   The key of its entry is keys(Keys).  Keys the user changed are not
%   grouped, so they can be changed back.

min_group(5).

group_rows([], []).
group_rows([Row|Rows], [Group|Grouped]) :-
    Row = row(Id, Table, entry(Key, Function, Status, Method),
              First, FText, Summary),
    atom(Function),
    \+ user_changed(Table, Key),
    partition(same_function(Function, Status), Rows, Same, Others),
    length(Same, N0),
    N is N0+1,
    min_group(Min),
    N >= Min,
    !,
    maplist(row_key, Same, Keys),
    last(Same, row(_, _, _, Last, _, _)),
    format(atom(Label), '~w \u2026 ~w (~D keys)', [First, Last, N]),
    Group = row(Id, Table, entry(keys([Key|Keys]), Function, Status, Method),
                Label, FText, Summary),
    group_rows(Others, Grouped).
group_rows([Row|Rows], [Row|Grouped]) :-
    group_rows(Rows, Grouped).

same_function(Function, Status,
              row(_, Table, entry(Key, Function, Status, _), _, _, _)) :-
    \+ user_changed(Table, Key).

row_key(row(_, _, entry(Key, _, _, _), _, _, _), Key).

number_row(Row, I0, I) :-
    arg(1, Row, Id),
    row_id(row, I0, Id),
    I is I0+1.

row_id(Kind, I, Id) :-
    atomic_list_concat([Kind, I], '_', Id).

key_label(_, @default, 'Other keys') :-
    !.
key_label(Table, Key, Label) :-
    get(Table, key_label, Key, Label).

function_text(Function, Function) :-
    atom(Function),
    !.
function_text(Function, Text) :-
    portray_object(Function, Term),
    format(atom(Text), '~p', [Term]).

entry_summary(hidden, _, _, 'Unbound') :-
    !.
entry_summary(unimplemented, _, _, 'Not implemented') :-
    !.
entry_summary(_, _, Method, Summary) :-
    Method \== @nil,
    get(Method, summary, String),
    String \== @nil,
    !,
    get(String, value, Summary).
entry_summary(_, Function, _, Summary) :-
    atom(Function),
    reserved_binding(Function, Summary),
    !.
entry_summary(_, _, _, '').

%!  table_label(+Table, -Label) is det.
%
%   Label is how the user knows the key_binding Table.  The tables of
%   PceEmacs are named `emacs$<mode>`.

table_label(Table, Label) :-
    get(Table, name, Name),
    (   Name == @nil
    ->  Label = 'This object'
    ;   atom_concat('emacs$', Mode, Name)
    ->  restyle_identifier(style(true, false, ' '), Mode, Pretty),
        format(atom(Label), '~w mode', [Pretty])
    ;   restyle_identifier(style(true, false, ' '), Name, Label)
    ).

%   row_visible(+ShowShadowed, +Filter, +Row)
%
%   True if Row is shown.  Filter is a regex or @nil.

row_visible(ShowShadowed, Filter,
            row(_, _, entry(_, _, Status, _), Key, Function, Summary)) :-
    (   ShowShadowed == @on
    ->  true
    ;   Status \= shadowed(_)
    ),
    (   Filter == @nil
    ->  true
    ;   format(string(Text), '~w ~w ~w', [Key, Function, Summary]),
        send(Filter, search, Text)
    ).

%   visible_rows(+Rows, +ShowShadowed, +Filter, -Visible)
%
%   Visible are the Rows that are shown, with the headers of the tables
%   that have rows shown.

visible_rows([], _, _, []).
visible_rows([header(Id, T, L)|Rows], Show, Filter, Visible) :-
    !,
    rows_of_table(Rows, Mine, Rest),
    include(row_visible(Show, Filter), Mine, Shown),
    (   Shown == []
    ->  Visible = Visible1
    ;   Visible = [header(Id, T, L)|Visible0],
        append(Shown, Visible1, Visible0)
    ),
    visible_rows(Rest, Show, Filter, Visible1).

rows_of_table([], [], []).
rows_of_table([R|Rs], [], [R|Rs]) :-
    functor(R, header, _),
    !.
rows_of_table([R|Rs], [R|Mine], Rest) :-
    rows_of_table(Rs, Mine, Rest).


                 /*******************************
                 *            BROWSER           *
                 *******************************/

:- pce_begin_class(key_binding_browser, tool_pane,
                   "Browse the key bindings of an object").

class_variable(pane_side, {above,below,left,right}, below,
               "A list of keys goes below the window it is about").

variable(targets, prolog,       get, "target(Target) implementing the keys").
variable(table,   key_binding*, get, "Table that is shown").
variable(rows,    prolog,       get, "header/3 and row/6 terms").
variable(filter,  regex* := @nil, get, "Only show the rows that match").
variable(selected_id, name* := @nil, get, "Id of the selected row").

initialise(B) :->
    "Create an empty browser"::
    send_super(B, initialise, key_bindings),
    send(B, slot, rows, []),
    send(B, slot, targets, target(@nil)),
    assert(key_binding_browser(B)),
    send(B, append_window, new(D, dialog)),
    send(D, name, top),
    send(B, fill_dialog),
    new(L, browser),
    send(L, name, keys),
    send(L, select_message, message(B, select, @arg1?key)),
    send(L, style, header,
         style(font := bold, background := ui_dialog_background)),
    send(L, style, shadowed, style(colour := ui_inactive)),
    send(L, style, user, style(colour := ui_accent)),
    send(B, append_window, L, D, below),
    send(B, append_window, new(Bottom, dialog), L, below),
    send(Bottom, name, bottom),
    send(B, fill_bottom),
    send(B, append_window, new(Det, key_binding_detail(B)), L, right),
    send(B, append_window, new(Card, man_html_card), Det, below),
    send(Card, name, documentation).

fill_dialog(B) :->
    "Fill the dialog with the pin, filter and options"::
    get(B, pane_window, top, D),
    send(D, pen, 0),
    send(D, gap, size(5, 5)),
    send(D, append, new(Pin, pin_button)),
    send(D, append,
         new(FI, filter_item(filter, message(B, filter, @arg1), 'Filter')),
         right),
    center_on(Pin, FI),
    send(FI, show_label, @off),
    send(FI, help_message, tag,
         'Only show keys whose key, function or summary matches'),
    send(D, append, new(M, menu(options, toggle, message(B, fill))), right),
    send(M, show_label, @off),
    send(M, append, menu_item(overridden, @default, 'Show overridden')),
    send(M, help_message, tag,
         'Also show keys that a table above binds as well').

%       The same buttons as the class variable editor.  The changes
%       apply at once; Save writes them to the Defaults file and Revert
%       goes back to what the Defaults file says.

fill_bottom(B) :->
    "Fill the dialog with Save, Revert, Edit defaults file and Quit"::
    get(B, pane_window, bottom, D),
    send(D, pen, 0),
    send(D, gap, size(5, 5)),
    send(D, append, new(Save, button(save, message(B, save)))),
    send(Save, help_message, tag,
         'Save the changed keys in your Defaults file'),
    send(D, append, new(Revert, button(revert, message(B, revert)))),
    send(Revert, help_message, tag,
         'Restore the keys from your Defaults file'),
    send(D, append, new(Edit, button(edit_defaults_file,
                                     message(B, edit_defaults_file)))),
    send(Edit, help_message, tag,
         'Edit your Defaults file in the editor'),
    send(D, append, button(quit, message(B, quit))),
    send(D?tile, ver_stretch, 0),
    send(D?tile, ver_shrink, 0),
    send(B, update_buttons).

pane_window(B, Name:name, W:window) :<-
    "The window of mine called Name"::
    get(B?members, find, @arg1?name == Name, W).

unlink(B) :->
    retractall(key_binding_browser(B)),
    send_super(B, unlink).

pinned(B, Pinned:bool) :<-
    "@on if I am not to be reused for the next object"::
    get(B, pane_window, top, D),
    get(D, member, pin, Pin),
    get(Pin, pinned, Pinned).

pane_label(B, Label:name) :<-
    "What my tab is called"::
    (   get(B, table, Table),
        Table \== @nil
    ->  get(B, targets, target(Target)),
        target_label(Table, Target, TL),
        format(atom(Label), 'Keys: ~w', [TL])
    ;   Label = 'Key bindings'
    ).

%   target_label(+Table, +Target, -Label)
%
%   How the user knows what the keys are of: the table if it is named,
%   else the class of the object whose keys these are.

target_label(Table, Target, Label) :-
    get(Table, name, Name),
    Name == @nil,
    default_target(Target, Object),
    object(Object),
    \+ send(Object, instance_of, class),
    !,
    get(Object, class_name, ClassName),
    restyle_identifier(style(true, false, ' '), ClassName, Label).
target_label(Table, _, Label) :-
    table_label(Table, Label).

default_target(targets(_, Target), Target) :-
    !.
default_target(Target, Target).

target(B, Target:any*, Table:key_binding) :->
    "Show the bindings of Table, implemented by Target"::
    send(B, show_table, target(Target), Table).

targets(B, Targets:prolog, Table:key_binding) :->
    "Show Table, implemented by targets(Pairs, Default)"::
    send(B, show_table, target(Targets), Table).

%   The targets are kept as target(Targets): a slot of type prolog
%   holds a Prolog term, not an object such as @nil.

show_table(B, Wrapped:prolog, Table:key_binding) :->
    "Show the bindings of Table, implemented by target(Targets)"::
    Wrapped = target(Target),
    send(B, slot, targets, Wrapped),
    send(B, slot, table, Table),
    send(B, slot, selected_id, @nil),
    key_binding_entries(Table, Target, Sections),
    sections_rows(Sections, Rows),
    send(B, slot, rows, Rows),
    send(B, fill),
    send(B, show_detail),
    (   get(B, pane_frame, Frame)
    ->  ignore(send(Frame, update_tab_label))
    ;   true
    ).

filter(B, Filter:regex*) :->
    "Only show the rows that match Filter; @nil shows all"::
    (   Filter == @nil
    ->  true
    ;   send(Filter, ignore_case, @on)
    ),
    send(B, slot, filter, Filter),
    send(B, fill).

show_overridden(B, Show:bool) :<-
    "@on if overridden keys are shown"::
    get(B, pane_window, top, D),
    get(D, member, options, M),
    get(M, selected, overridden, Show).

fill(B) :->
    "Fill the list from <-rows"::
    get(B, pane_window, keys, L),
    send(L, clear),
    get(B, rows, Rows),
    get(B, show_overridden, Show),
    get(B, filter, Filter),
    visible_rows(Rows, Show, Filter, Visible),
    tab_stops(L, Visible),
    maplist(append_row(L), Visible),
    get(B, selected_id, Current),
    (   Current \== @nil,
        get(L, member, Current, DI)
    ->  send(L, selection, DI),
        send(L, normalise, DI)
    ;   true
    ).

append_row(L, header(Id, Table, Label)) :-
    send(L, append, new(DI, dict_item(Id, Label, @default, header))),
    (   get(Table, name, Name),
        Name \== @nil
    ->  send(DI, help_message, tag, Name)
    ;   true
    ).
append_row(L, row(Id, Table, entry(K, _, Status, _), Key, Function, Summary)) :-
    format(string(Text), '  ~w\t~w\t~w', [Key, Function, Summary]),
    (   user_changed(Table, K)
    ->  Style = user
    ;   Status == active
    ->  Style = @default
    ;   Style = shadowed
    ),
    send(L, append, dict_item(Id, Text, @default, Style)).

%   tab_stops(+Browser, +Rows)
%
%   Set the tab stops of Browser such that the key, function and
%   summary of Rows line up.

tab_stops(L, Rows) :-
    get(L, font, Font),
    get(Font, width, '    ', Gap),
    foldl(column_widths(Font), Rows, 0-0, KW-FW),
    Tab1 is KW + Gap,
    Tab2 is Tab1 + FW + Gap,
    send(L?text_image, tab_stops, vector(Tab1, Tab2)).

column_widths(Font, row(_, _, _, Key, Function, _), K0-F0, K-F) :-
    !,
    atom_concat('  ', Key, KeyText),
    get(Font, width, KeyText, KW),
    get(Font, width, Function, FW),
    K is max(K0, KW),
    F is max(F0, FW).
column_widths(_, _, KF, KF).

select(B, Id:name) :->
    "Show the details of the row or table header Id"::
    send(B, slot, selected_id, Id),
    send(B, show_detail).

select_key(B, Key:name) :->
    "Select the row that binds Key"::
    get(B, rows, Rows),
    (   member(row(Id, _, entry(Keys, _, Status, _), _, _, _), Rows),
        entry_has_key(Keys, Key),
        Status \= shadowed(_)
    ->  get(B, pane_window, keys, L),
        (   get(L, member, Id, DI)
        ->  send(L, selection, DI),
            send(L, normalise, DI)
        ;   true                        % filtered out
        ),
        send(B, slot, selected_id, Id),
        send(B, show_detail)
    ;   true
    ).

entry_has_key(Key, Key) :-
    !.
entry_has_key(keys(Keys), Key) :-
    memberchk(Key, Keys).

current_row(B, Row:prolog) :<-
    "The row that is selected"::
    get(B, selected_id, Id),
    Id \== @nil,
    get(B, rows, Rows),
    Row = row(Id, _, _, _, _, _),
    memberchk(Row, Rows).

show_detail(B) :->
    "Show the selected key or table in the detail window"::
    get(B, pane_window, detail, D),
    get(B, pane_window, documentation, Card),
    get(B, rows, Rows),
    (   get(B, current_row, Row)
    ->  send(D, show, Row, Rows),
        Row = row(_, _, entry(_, _, _, Method), _, _, _),
        send(Card, selection, Method)
    ;   get(B, selected_id, Id),
        memberchk(header(Id, Table, _), Rows)
    ->  send(D, show_table, Table),
        send(Card, selection, @nil)
    ;   send(D, show, @nil, []),
        send(Card, selection, @nil)
    ).

table_target(B, Table:key_binding, Target:any) :<-
    "The object or class that implements the functions of Table"::
    get(B, targets, target(Targets)),
    table_target(Targets, Table, Target).

:- pce_group(edit).

%   The user changes the bindings of the named tables shown.  The
%   changes are applied at once, and saved in the Defaults file by
%   ->save.  See key_binding->user_binding.

user_bind(B, Table:key_binding, Key:name, Function:name*) :->
    "Make the user bind Key in Table; @nil reverts"::
    remember_saved(Table),
    send(Table, user_binding, Key, Function),
    send(B, changed_key, Key).

unbind(B, Table:key_binding, Key:name) :->
    "Make Key do nothing in Table"::
    remember_saved(Table),
    unbind_key(Table, Key),
    send(B, changed_key, Key).

add_binding(B, Table:key_binding, Keys:prolog, Function:name) :->
    "Bind the key sequence Keys to Function in Table"::
    remember_saved(Table),
    bind_prefixes(Table, Keys),
    atomic_list_concat(Keys, Key),
    send(Table, user_binding, Key, Function),
    send(B, changed_key, Key).

rebind(B, Table:key_binding, Old:name, Keys:prolog, Function:name) :->
    "Bind the key sequence Keys instead of Old to Function in Table"::
    remember_saved(Table),
    bind_prefixes(Table, Keys),
    atomic_list_concat(Keys, New),
    send(Table, user_binding, New, Function),
    (   Old == New
    ->  true
    ;   unbind_key(Table, Old)
    ),
    send(B, changed_key, New).

changed_key(B, Key:name) :->
    "The user changed Key: refill and show Key"::
    send(B, refill),
    send(B, select_key, Key).

refill(B) :->
    "Show the table again, e.g., after changing keys"::
    get(B, targets, Targets),
    get(B, table, Table),
    (   Table == @nil
    ->  true
    ;   send(B, show_table, Targets, Table)
    ),
    send(B, update_buttons).

update_buttons(B) :->
    "Make Save and Revert active if there are changes to save"::
    (   modified_table(_)
    ->  Active = @on
    ;   Active = @off
    ),
    get(B, pane_window, bottom, D),
    send(D?save_member, active, Active),
    send(D?revert_member, active, Active).

save(B) :->
    "Save the changed keys in the Defaults file"::
    (   user_defaults_file(File)
    ->  findall(Table, modified_table(Table), Tables),
        (   maplist(save_table, Tables)
        ->  send(B, report, status, 'Saved to %s', File)
        ;   send(B, report, warning, 'Could not save all keys to %s', File)
        ),
        send(B, update_buttons)
    ;   send(B, report, error, 'No user Defaults file')
    ).

revert(B) :->
    "Restore the keys as saved in the Defaults file"::
    forall(retract(saved_bindings(Table, Pairs)),
           send(Table, user_bindings, Pairs)),
    send(B, refill),
    send(B, report, status, 'Reverted').

edit_defaults_file(_B) :->
    "Edit the Defaults file"::
    prolog_edit_preferences(xpce).

quit(B) :->
    "Close the key binding browser"::
    send(B, request_close).

:- pce_end_class(key_binding_browser).


                 /*******************************
                 *            DETAILS           *
                 *******************************/

:- pce_begin_class(key_binding_detail, dialog,
                   "Details of a key binding").

variable(browser, key_binding_browser, get, "Browser I show a key of").
variable(row,     prolog,              get,
         "row/6 term shown, table(Table), add(Table[, Keys]) or `none`").
variable(scope,   key_binding*,        get, "Table to change").

initialise(D, B:key_binding_browser) :->
    send_super(D, initialise),
    send(D, slot, browser, B),
    send(D, name, detail),
    send(D, gap, size(10, 5)),
    send(D, show, @nil, []).

show(D, Row:prolog, Rows:prolog) :->
    "Show the details of Row"::
    clear_dialog(D),
    (   Row == @nil
    ->  send(D, slot, row, none),
        send(D, append, label(none, 'Select a key', italic))
    ;   send(D, slot, row, Row),
        detail_items(Row, Rows, D),
        edit_items(Row, Rows, D)
    ),
    fit_dialog(D).

clear_dialog(D) :-
    get(D?graphicals, copy, Grs),
    send(Grs, for_all, message(@arg1, destroy)).

fit_dialog(D) :-
    send(D, layout),
    fit_tile(D).

%   fit_tile(+Dialog)
%
%   The tiles a dialog is in keep the room they gave it, also when it
%   needs more or less to show its items.  Make the height its items
%   need the ideal height of its tile and lay the tiles out again.
%   This gives the documentation below the room that is left.  The
%   height is computed as dialog ->fit does.

fit_tile(D) :-
    send(D, compute),
    get(D, bounding_box, BB),
    get(BB, y, Y),
    get(BB, height, BH),
    get(D, border, Border),
    (   Border == @default
    ->  get(D, gap, Margin)
    ;   Margin = Border
    ),
    get(Margin, height, MH),
    H is Y + BH + MH,
    get(D, tile, T),
    send(T, ideal_height, H),
    root_tile(T, Root),
    send(Root, compute),
    send(Root, layout).

root_tile(T, Root) :-
    get(T, super, Super),
    Super \== @nil,
    !,
    root_tile(Super, Root).
root_tile(T, T).

detail_items(row(_, Table, entry(Key, Function, Status, Method),
                 KeyLabel, FText, Summary), Rows, D) :-
    format(string(Title), '~w → ~w', [KeyLabel, FText]),
    send(D, append, new(T, label(title, Title))),
    send(T, font, bold),
    (   (   Summary == ''
        ;   Method \== @nil            % the documentation shows it
        )
    ->  true
    ;   send(D, append, new(S, text(Summary, left))),
        send(S, margin, 400, wrap)
    ),
    table_label(Table, TableLabel),
    status_text(Status, TableLabel, StatusText),
    send(D, append, text(StatusText, left)),
    other_keys(Rows, Key, Function, Others),
    (   Others == []
    ->  true
    ;   atomic_list_concat(Others, ', ', OtherText),
        format(string(Also), 'Also on ~w', [OtherText]),
        send(D, append, new(A, text(Also, left))),
        send(A, margin, 400, wrap)
    ),
    (   Method \== @nil,
        get(Method, source, Location),
        Location \== @nil
    ->  get(Location, file_name, File),
        get(Location, line_no, Line),
        send(D, append,
             new(E, button(edit_source,
                           message(D, edit_source, File, Line)))),
        send(E, label, 'Edit source'),
        send(E, alignment, left),
        send(E, help_message, tag, 'Edit the method this key invokes')
    ;   true
    ).

status_text(active, Table, Text) :-
    format(string(Text), 'Bound in ~w', [Table]).
status_text(reserved, Table, Text) :-
    format(string(Text), 'Bound in ~w; handled by the key binding itself',
           [Table]).
status_text(hidden, Table, Text) :-
    format(string(Text), 'Unbound in ~w', [Table]).
status_text(unimplemented, Table, Text) :-
    format(string(Text), 'Bound in ~w, but there is no such method',
           [Table]).
status_text(shadowed(By), Table, Text) :-
    table_label(By, ByLabel),
    format(string(Text), 'Bound in ~w, but overridden by ~w',
           [Table, ByLabel]).

%   edit_items(+Row, +Rows, +Dialog)
%
%   The controls to change the binding of a single key.  The changes go
%   to a named table, by default the one that binds the key.

edit_items(Row, Rows, D) :-
    Row = row(_, Table, entry(Key, Function, _, _), _, _, _),
    atom(Key),
    named_tables(Rows, Tables),
    Tables = [First|_],
    !,
    (   user_changed(Table, Key)
    ->  send(D, append, new(R, button(revert, message(D, revert)))),
        send(R, alignment, left),
        send(R, help_message, tag, 'Undo your change of this key'),
        send(D, append, new(C, text('You changed this key', left)), right),
        center_on(C, R)
    ;   true
    ),
    send(D, append, new(M, menu(scope, cycle, message(D, scope, @arg1)))),
    send(M, label, 'Change in'),
    send(M, alignment, left),
    forall(member(T, Tables),
           ( table_label(T, Label),
             send(M, append, menu_item(T, @default, Label))
           )),
    (   memberchk(Table, Tables)
    ->  Scope = Table
    ;   Scope = First
    ),
    send(M, selection, Scope),
    send(D, slot, scope, Scope),
    send(D, append, new(CF, button(change_function,
                                   message(D, choose_function)))),
    send(CF, label, 'Change function…'),
    send(CF, alignment, left),
    send(CF, help_message, tag, 'Bind the key to another function'),
    (   bound_function_name(Function)
    ->  send(D, append, new(CK, button(change_key, message(D, capture))),
             right),
        send(CK, label, 'Change key…'),
        send(CK, help_message, tag, 'Bind the function to another key'),
        send(D, append, new(U, button(unbind, message(D, unbind))), right),
        send(U, help_message, tag, 'Make the key do nothing')
    ;   true
    ).
edit_items(_, _, _).

%   Selecting the header of a table shows what the table is, and lets
%   the user add a key to a named table: first type the key, then
%   choose the function.

show_table(D, Table:key_binding) :->
    "Show the details of Table"::
    clear_dialog(D),
    send(D, slot, row, table(Table)),
    send(D, slot, scope, Table),
    table_label(Table, Label),
    send(D, append, new(T, label(title, Label))),
    send(T, font, bold),
    get(Table, name, Name),
    (   atom(Name)
    ->  get(Table, user_bindings_variable, Var),
        get(Var, name, VarName),
        format(string(Text),
               'Key binding table ~w.  Keys you change are saved as \c
                key_binding.~w.', [Name, VarName])
    ;   Text = "The keys of this object only.  Changes go to a table \c
                it uses."
    ),
    send(D, append, new(I, text(Text, left))),
    send(I, margin, 400, wrap),
    get(Table?defaults, map, @arg1, SuperChain),
    chain_list(SuperChain, Supers),
    (   Supers == []
    ->  true
    ;   maplist(table_label, Supers, SuperLabels),
        atomic_list_concat(SuperLabels, ', ', SuperText),
        format(string(Inherits), 'Uses ~w for the keys it does not bind.',
               [SuperText]),
        send(D, append, new(U, text(Inherits, left))),
        send(U, margin, 400, wrap)
    ),
    (   get(Table, user_bindings, Pairs)
    ->  length(Pairs, N),
        format(string(Changed), 'You changed ~D key~p', [N, N]),
        send(D, append, text(Changed, left))
    ;   true
    ),
    (   atom(Name)
    ->  send(D, append, new(A, button(add_key, message(D, add_key)))),
        send(A, label, 'Add key…'),
        send(A, alignment, left),
        send(A, help_message, tag, 'Bind a key in this table')
    ;   true
    ),
    fit_dialog(D).

add_key(D) :->
    "Ask for a key to add to the table shown"::
    get(D, row, table(Table)),
    send(D, slot, row, add(Table)),
    send(D, capture).

%   bound_function_name(+Function) is semidet.
%
%   True if Function is the name of a function the key invokes, which
%   can move to another key.

bound_function_name(Function) :-
    atom(Function),
    Function \== none.

named_tables(Rows, Tables) :-
    convlist(named_header, Rows, Tables).

named_header(header(_, Table, _), Table) :-
    get(Table, name, Name),
    atom(Name).

scope(D, Table:key_binding) :->
    "Make changes in Table"::
    send(D, slot, scope, Table).

revert(D) :->
    "Undo the user's change of the key"::
    get(D, row, row(_, Table, entry(Key, _, _, _), _, _, _)),
    get(D, browser, B),
    send(B, user_bind, Table, Key, @nil).

unbind(D) :->
    "Make the key do nothing"::
    get(D, row, row(_, _, entry(Key, _, _, _), _, _, _)),
    get(D, scope, Table),
    get(D, browser, B),
    send(B, unbind, Table, Key).

cancel(D) :->
    "Stop changing the key"::
    get(D, browser, B),
    send(B, show_detail).

%   Changing the key: a key_capture_item records the keys typed.

capture(D) :->
    "Ask for the new key of the function"::
    get(D, row, Row),
    capture_title(Row, Title),
    clear_dialog(D),
    send(D, append, new(T, label(title, Title))),
    send(T, font, bold),
    send(D, append, new(I, key_capture_item)),
    send(D, append, new(S, text('', left))),
    send(S, name, status),
    send(S, margin, 400, wrap),
    send(D, append, new(Bind, button(bind, message(D, bind_captured)))),
    send(D, append, button(clear, message(I, clear)), right),
    send(D, append, button(cancel, message(D, cancel)), right),
    send(Bind, alignment, left),        % RET is a key to capture,
    send(Bind, default_button, @off),   % so it does not press Bind
    send(D, captured),
    fit_dialog(D),
    send(D, keyboard_focus, I),
    (   get(D, frame, Frame)
    ->  ignore(send(Frame, keyboard_focus, D))
    ;   true
    ).

capture_title(row(_, _, _, _, FText, _), Title) :-
    format(string(Title), 'New key for ~w', [FText]).
capture_title(add(Table), Title) :-
    table_label(Table, Label),
    format(string(Title), 'New key in ~w', [Label]).

captured(D) :->
    "Update the status for the keys typed"::
    get(D, member, key_capture_item, I),
    get(I, keys, Keys),
    get(D, scope, Table),
    sequence_status(Table, Keys, OK, Text),
    get(D, member, status, S),
    send(S, string, Text),
    get(D, member, bind, Bind),
    send(Bind, active, OK).

bind_captured(D) :->
    "Bind the keys typed"::
    get(D, member, key_capture_item, I),
    get(I, keys, Keys),
    get(D, scope, Table),
    sequence_status(Table, Keys, OK, _),
    OK == @on,
    get(D, row, Row),
    (   Row = add(Table)
    ->  send(D, slot, row, add(Table, Keys)),
        send(D, choose_function)
    ;   Row = row(_, _, entry(Old, Function, _, _), _, _, _),
        bound_function_name(Function),
        get(D, browser, B),
        send(B, rebind, Table, Old, Keys, Function)
    ).

%   sequence_status(+Table, +Keys, -OK, -Text)
%
%   Text describes what binding the sequence Keys in Table does.  OK is
%   @off if it cannot be bound: a key before the last one invokes a
%   function rather than waiting for the next.

sequence_status(_, [], @off, 'Type the new key') :-
    !.
sequence_status(Table, Keys, @off, Text) :-
    append(PrefixKeys, [_|_], Keys),            % a key before the last
    PrefixKeys \== [],
    atomic_list_concat(PrefixKeys, Prefix),
    bound_function(Table, Prefix, F),
    F \== prefix,
    !,
    get(Table, key_label, Prefix, Label),
    function_text(F, FText),
    format(string(Text), '~w invokes ~w: it cannot start a longer key',
           [Label, FText]).
sequence_status(Table, Keys, @on, Text) :-
    atomic_list_concat(Keys, Key),
    get(Table, key_label, Key, Label),
    (   bound_function(Table, Key, prefix)
    ->  format(string(Text),
               '~w starts other keys, which no longer work if you bind it.  \c
                Type more keys to bind a longer one.', [Label])
    ;   bound_function(Table, Key, F)
    ->  function_text(F, FText),
        format(string(Text), '~w now invokes ~w', [Label, FText])
    ;   format(string(Text), '~w is not bound', [Label])
    ).

bound_function(Table, Key, Function) :-
    get(Table, function, Key, Function).

%   Changing the function: a text item that completes the functions the
%   target of the table implements and a key can invoke.

choose_function(D) :->
    "Ask for the new function of the key"::
    get(D, scope, Table),
    get(D, row, Row),
    (   Row = add(_, Keys)
    ->  atomic_list_concat(Keys, Key),
        get(Table, key_label, Key, KeyLabel)
    ;   Row = row(_, _, _, KeyLabel, _, _)
    ),
    get(D, browser, B),
    get(B, table_target, Table, Target),
    command_functions(Target, Names),
    clear_dialog(D),
    format(string(Title), 'New function for ~w', [KeyLabel]),
    send(D, append, new(T, label(title, Title))),
    send(T, font, bold),
    send(D, append, new(I, text_item(function, ''))),
    send(I, show_label, @off),
    (   Names == []
    ->  true
    ;   Set =.. [chain|Names],
        send(I, value_set, Set)
    ),
    send(D, append, new(S, text('', left))),
    send(S, name, status),
    send(S, margin, 400, wrap),
    send(D, append, new(Bind, button(bind, message(D, bind_function)))),
    send(D, append, button(cancel, message(D, cancel)), right),
    send(Bind, alignment, left),
    send(Bind, default_button, @on),
    fit_dialog(D),
    send(D, keyboard_focus, I),
    (   get(D, frame, Frame)
    ->  ignore(send(Frame, keyboard_focus, D))
    ;   true
    ).

bind_function(D) :->
    "Bind the key to the function typed"::
    get(D, member, function, I),
    get(I, selection, Text),
    atom_string(Function, Text),
    get(D, row, Row),
    get(D, scope, Table),
    get(D, browser, B),
    get(B, table_target, Table, Target),
    (   Function == ''
    ->  true
    ;   \+ key_function(Target, Function)
    ->  get(D, member, status, S),
        format(string(Msg), '~w is not a function a key can invoke',
               [Function]),
        send(S, string, Msg),
        fit_dialog(D)
    ;   Row = add(_, Keys)
    ->  send(B, add_binding, Table, Keys, Function)
    ;   Row = row(_, _, entry(Key, _, _, _), _, _, _),
        send(B, user_bind, Table, Key, Function)
    ).

%   other_keys(+Rows, +Key, +Function, -Labels)
%
%   Labels are the other keys that invoke Function.

other_keys(Rows, Key, Function, Labels) :-
    atom(Function),
    !,
    convlist(other_key(Key, Function), Rows, Labels).
other_keys(_, _, _, []).

other_key(Key, Function,
          row(_, _, entry(K, Function, active, _), Label, _, _), Label) :-
    K \== Key.

edit_source(_D, File:name, Line:int) :->
    "Edit the source of the method of the key"::
    edit(file(File, line(Line))).

:- pce_end_class(key_binding_detail).


                 /*******************************
                 *         USER BINDINGS        *
                 *******************************/

%   saved_bindings(?Table, ?Pairs)
%
%   Pairs are the Key-Function pairs of the named Table as they are in
%   the Defaults file.  Recorded when the user first changes Table, so
%   the tables to save are those whose bindings differ from it.

:- dynamic
    saved_bindings/2.

remember_saved(Table) :-
    saved_bindings(Table, _),
    !.
remember_saved(Table) :-
    table_user_bindings(Table, Pairs),
    assertz(saved_bindings(Table, Pairs)).

table_user_bindings(Table, Pairs) :-
    (   get(Table, user_bindings, Pairs)
    ->  true
    ;   Pairs = []
    ).

modified_table(Table) :-
    saved_bindings(Table, Saved),
    table_user_bindings(Table, Now),
    Saved \== Now.

%   save_table(+Table) is semidet.
%
%   Save the keys the user bound in Table to the Defaults file.

save_table(Table) :-
    get(Table, user_bindings_variable, Var),
    get(Var, name, Name),
    get(Var, value, Value),
    catch(save_class_variable(key_binding, Name, Value), E,
          ( print_message(warning, E), fail )),
    table_user_bindings(Table, Pairs),
    retractall(saved_bindings(Table, _)),
    assertz(saved_bindings(Table, Pairs)).

%   user_changed(+Table, +Key) is semidet.
%
%   True if the user changed Key in Table.

user_changed(Table, Key) :-
    atom(Key),
    get(Table, name, Name),
    atom(Name),
    get(Table, user_binding, Key, _).

%   unbind_key(+Table, +Key)
%
%   Make Key do nothing in Table.  If Key is a key the user added,
%   remove it.

unbind_key(Table, Key) :-
    (   get(Table, attribute, user_modified, Undo),
        get(Undo, value, Key, Old),
        Old == @nil
    ->  send(Table, user_binding, Key, @nil)
    ;   send(Table, user_binding, Key, none)
    ).

%   bind_prefixes(+Table, +Keys)
%
%   Make the keys before the last of the sequence Keys prefixes in
%   Table if they are not bound.

bind_prefixes(Table, Keys) :-
    forall(( append(PrefixKeys, [_|_], Keys),
             PrefixKeys \== [],
             atomic_list_concat(PrefixKeys, Prefix),
             \+ bound_function(Table, Prefix, _)
           ),
           send(Table, user_binding, Prefix, prefix)).

%!  command_functions(+Target, -Names) is det.
%
%   Names is a sorted list of the functions to offer for binding a key
%   of Target, an object or a class.  These are the send methods that
%   pass key_function/2 and are meant to be used as a command:
%
%     - A built-in method that is not in a group of methods that
%       manage the object, e.g., `oms`, `area` or `repaint`.
%     - A method defined in Prolog that is bound to a key in some
%       table, or for which the hook command_function/2 succeeds.  The
%       groups of these methods are too sloppy to rely on.

command_functions(@nil, []) :-
    !.
command_functions(Target, Names) :-
    prompts(Target, Prompts),
    bound_functions(Bound),
    target_classes(Target, Classes),
    foldl(class_commands(Target, Prompts, Bound), Classes, [], Names0),
    sort(Names0, Names).

class_commands(Target, Prompts, Bound, Class, Names0, Names) :-
    get(Class, send_methods, Chain),
    chain_list(Chain, Methods),
    convlist(command_method(Target, Prompts, Bound), Methods, ClassNames),
    append(ClassNames, Names0, Names).

command_method(Target, Prompts, Bound, Method, Name) :-
    key_method(Prompts, Method),
    get(Method, name, Name),
    (   get(Method, source, Source),
        Source == @nil                  % built-in
    ->  \+ ( get(Method, group, Group),
             plumbing_group(Group)
           )
    ;   ord_memberchk(Name, Bound)
    ->  true
    ;   command_function(Target, Name)
    ),
    !.

%   plumbing_group(?Group)
%
%   Groups of built-in methods that manage the object rather than act
%   for the user.

plumbing_group(accelerator).
plumbing_group(appearance).
plumbing_group(area).
plumbing_group(compatibility).
plumbing_group(delegate).
plumbing_group(dialog_item).
plumbing_group(format).
plumbing_group(iterate).
plumbing_group(label).
plumbing_group(layout).
plumbing_group(meta).
plumbing_group(name).
plumbing_group(oms).
plumbing_group(organisation).
plumbing_group(repaint).
plumbing_group(resize).
plumbing_group(storage).
plumbing_group(textual).
plumbing_group(type).
plumbing_group(update).
plumbing_group(virtual).
plumbing_group(visualisation).

%   bound_functions(-Names) is det.
%
%   Names is the ordered set of function names bound in the named
%   tables.

bound_functions(Names) :-
    new(Chain, chain),
    send(@key_bindings, for_all,
         message(@arg2?bindings, for_all,
                 message(Chain, append, @arg1?value))),
    chain_list(Chain, Functions),
    free(Chain),
    include(atom, Functions, Names0),
    sort(Names0, Names).

%!  key_function(+Target, +Name) is semidet.
%
%   True if Name is a send method of Target that a key can invoke: the
%   key binding fills its arguments with the character typed or the
%   numeric argument, or leaves them out.  If Target implements
%   <-interactive_argument, as a PceEmacs mode does, it prompts for
%   the others, which must be typed: not an event, graphical, etc.

key_function(@nil, _) :-
    !.
key_function(Target, Name) :-
    send_method(Target, Name, Method),
    prompts(Target, Prompts),
    key_method(Prompts, Method).

prompts(Target, Prompts) :-
    (   \+ send(Target, instance_of, class),
        send(Target, has_get_method, interactive_argument)
    ->  Prompts = true
    ;   Prompts = false
    ).

key_method(Prompts, Method) :-
    get(Method, name, Name),
    \+ sub_atom(Name, 0, _, _, '_'),
    \+ memberchk(Name, [initialise, unlink, compute, typed]), % typed: loops
    get(Method, types, Types),
    get(Types, size, N),
    forall(between(1, N, I),
           ( get(Types, element, I, Type0),
             real_type(Type0, Type),
             filled_argument(Prompts, Type)
           )).

%   real_type(+Type, -Real)
%
%   A named argument, e.g., `times=[int]`, has a type of kind `alias`.

real_type(Type0, Type) :-
    get(Type0, kind, alias),
    !,
    get(Type0, context, Type1),
    real_type(Type1, Type).
real_type(Type, Type).

no_key_functions(object).
no_key_functions(visual).
no_key_functions(graphical).

filled_argument(_, Type) :-
    optional(Type),
    !.
filled_argument(_, Type) :-
    get(Type, fullname, Name),
    memberchk(Name, [char, event_id]),
    !.
filled_argument(true, Type) :-
    promptable(Type).

%   optional(+Type) is semidet.
%
%   True if Type accepts @default, i.e., it is [Type].  Do not use
%   type->validate: for a value_set type this computes the set.

optional(Type) :-
    get(Type, supers, Supers),
    Supers \== @nil,
    get(Supers, find, @arg1?name == default, _).

%   promptable(+Type) is semidet.
%
%   True if the user can type a value for Type.

:- public promptable/1.

promptable(Type) :-
    get(Type, kind, Kind),
    promptable_kind(Kind, Type),
    !.
promptable(Type) :-
    get(Type, supers, Supers),
    Supers \== @nil,
    get(Supers, find, message(@prolog, promptable, @arg1), _).

promptable_kind(class, Type) :-
    !,
    get(Type, context, Context),
    get(@pce, convert, Context, class, Class),
    \+ ( not_typed(Name),
         send(Class, is_a, Name)
       ).
promptable_kind(Kind, _) :-
    \+ memberchk(Kind, [any, alien, atomic, unchecked]).

not_typed(visual).
not_typed(event).
not_typed(code).
not_typed(chain).
not_typed(vector).
not_typed(fragment).
not_typed(recogniser).

%   target_classes(+Target, -Classes)
%
%   Classes are the classes whose methods implement the functions on
%   Target: its class and the classes it forwards or delegates to with
%   their super classes, up to class graphical, whose methods are not
%   meant for keys.

target_classes(Target, Classes) :-
    target_class(Target, Class),
    super_classes(Class, Own),
    (   forwards_to(Class, To)
    ->  super_classes(To, Forwarded)
    ;   Forwarded = []
    ),
    (   send(Target, instance_of, class)
    ->  Delegated = []
    ;   get(Class, delegate, Vars),
        chain_list(Vars, VarList),
        convlist(delegate_classes(Target), VarList, Delegated)
    ),
    append([Own, Forwarded|Delegated], Classes0),
    list_to_set(Classes0, Classes).

delegate_classes(Target, Var, Classes) :-
    get(Var, name, Name),
    get(Target, Name, Object),
    object(Object),
    Object \= @nil,
    get(Object, class, Class),
    super_classes(Class, Classes).

super_classes(Class, Classes) :-
    (   get(Class, name, Name),
        no_key_functions(Name)
    ->  Classes = []
    ;   get(Class, super_class, Super),
        Super \== @nil
    ->  Classes = [Class|Supers],
        super_classes(Super, Supers)
    ;   Classes = [Class]
    ).


                 /*******************************
                 *          KEY CAPTURE         *
                 *******************************/

:- pce_begin_class(key_capture_item, text_item,
                   "Record the key sequence typed").

variable(keys, prolog, get, "List of the keys typed").

initialise(I) :->
    "Create, waiting for a key"::
    send_super(I, initialise, key_capture_item, ''),
    send(I, show_label, @off),
    send(I, slot, keys, []),
    send(I, show_keys).

event(I, Ev:event) :->
    "Record a key; leave other events to the text item"::
    (   send(Ev, is_a, keyboard)
    ->  get(Ev, key, Key),
        get(I, keys, Keys0),
        append(Keys0, [Key], Keys),
        send(I, slot, keys, Keys),
        send(I, show_keys),
        ignore(send(I?device, captured))
    ;   send_super(I, event, Ev)
    ).

clear(I) :->
    "Forget the keys typed"::
    send(I, slot, keys, []),
    send(I, show_keys),
    ignore(send(I?device, captured)),
    ignore(send(I?device, keyboard_focus, I)).

show_keys(I) :->
    "Show the keys typed"::
    get(I, keys, Keys),
    (   Keys == []
    ->  Text = 'Press the keys …'
    ;   atomic_list_concat(Keys, Key),
        get(@pce, convert, text_item, key_binding, Table),
        get(Table, key_label, Key, Text)
    ),
    send_super(I, selection, Text).

:- pce_end_class(key_capture_item).
