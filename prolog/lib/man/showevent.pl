/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog/packages/xpce/
    Copyright (c)  1999-2026, University of Amsterdam
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

:- module(pce_show_event, []).
:- encoding(utf8).
:- use_module(library(pce)).
:- use_module(library(pce_toc)).
:- autoload(library(pce_help_file)).
:- autoload(library(pce_util)).

:- pce_help_file(event_monitor, pce_help('event_monitor.hlp')).

program_version('0.0').

:- pce_begin_class(man_event_viewer, man_frame,
                   "Show event-details").

initialise(V, M) :->
    send_super(V, initialise, M, 'XPCE Event-viewer'),
    send(V, append, new(P, picture)),
    send(new(D, dialog), below, P),
    send(new(event_hierarchy_window), left, D),
    send(new(event_list), right, D),
    send(V, fill_dialog),
    send(V, fill_picture),
    send(V, keyboard_focus, P).

%       My menu goes on the bar of the window I am in -- see
%       `pane_frame ->update_menu_bar'.  It used to be a bar of my own, in
%       a dialog above my windows.

menu_bar_key(_V, Key:name) :<-
    "Every event viewer asks for the same menu bar"::
    Key = event_viewer.

fill_menu_bar(V, MD:tool_dialog) :->
    "Put my menu on the bar of the window I am in"::
    get(MD, popup, events, @on, Popup),
    send_list(Popup, append,
              [ menu_item(about,
                          message(V, about)),
                menu_item(help,
                          message(V, help))
              ]).

about(V) :->
    program_version(Version),
    send(@display, inform, V, @default,
         'XPCE event-monitor\nversion %s\n\nBy Jan Wielemaker',
         Version).

help(_) :->
    send(@helper, give_help, event_monitor).

%!  event_property(?Name, ?Where, ?Attributes)
%
%   The text items that show a property of the event.  Where says how
%   the item is placed relative to the one before it.

event_property(id,       next_row, [length(40)]).
event_property(x,        next_row, [length(4)]).
event_property(y,        right,    [length(4), alignment(left)]).
event_property(button,   next_row, [length(7)]).
event_property(click,    right,    [length(6), alignment(left)]).
event_property(rotation, right,    [length(4), alignment(left)]).
event_property(device,   next_row, [length(30)]).

modifier('C(ontrol)', control).
modifier('M(eta)',    meta).
modifier('S(hift)',   shift).
modifier('s (gui)',   gui).

fill_dialog(V) :->
    get(V, member, dialog, Dialog),
    send(Dialog, append, new(DI, text_item(description))),
    send(DI, length, 40),
    send(DI, editable, @off),
    (   event_property(Name, Where, Atts),
        send(Dialog, append, new(I, text_item(Name)), Where),
        send(I, length, 15),
        send(I, editable, @off),
        send_list(I, Atts),
        fail
    ;   true
    ),
    send(Dialog, append, new(I, menu(modifiers, marked))),
    forall(modifier(M, _),
           send(I, append, menu_item(M, label:=M, mnemonic := @nil))),
    send(Dialog, append, new(DB, list_browser(@default, 40, 5))),
    send(DB, name, detail),
    send(DB, label, 'Detail:'),
    send(DB, tab_stops, vector(150)).

update_dialog(V, Ev:event) :->
    get(V, member, dialog, Dialog),
    forall(( event_property(Name, _, _),
             get(Dialog, member, Name, Item)
           ),
           (   event_value(Name, Ev, Value)
           ->  send(Item, selection, Value)
           ;   send(Item, clear)
           )),
    get(Dialog, member, modifiers, Menu),
    forall(modifier(M, Name),
           (   send(Ev, has_modifier, modifier(Name := down))
           ->  send(Menu, selected, M, @on)
           ;   send(Menu, selected, M, @off)
           )),
    get(Dialog, member, description, DI),
    (   description(Ev, Description)
    ->  send(DI, selection, Description)
    ;   send(DI, selection, '<no description>')
    ),
    get(Dialog, member, detail, DB),
    send(DB, clear),
    forall(event_detail(Ev, Name, Value),
           send(DB, append, string('%s\t%s', Name, Value))).

%!  event_value(+Name, +Event, -Value) is semidet.
%
%   What the text item Name shows for Event.  Fails if the property does
%   not apply to Event.

event_value(button, Ev, Button) :-
    !,
    send(Ev, is_a, button),
    get(Ev, button, Button).
event_value(click, Ev, Click) :-
    !,
    (   send(Ev, is_down)
    ;   send(Ev, is_up)
    ),
    get(Ev, multiclick, Click).
event_value(rotation, Ev, Rotation) :-
    !,
    get(Ev, rotation, Rotation0),
    Rotation0 \== @nil,
    format_value(Rotation0, Rotation).
event_value(device, Ev, Device) :-
    !,
    get(Ev, device, D),
    get(D, kind, Kind),
    get(D, name, Name),
    format(atom(Device), '~w: ~w', [Kind, Name]).
event_value(Name, Ev, Value) :-
    get(Ev, Name, Value0),
    format_value(Value0, Value).

%!  event_detail(+Event, -Name, -Value) is nondet.
%
%   The properties of the device specific data of Event:  the pressure
%   of a pen, the finger of a touch, the file of a drop, etc.  They are
%   the instance variables of its event_detail.

event_detail(Ev, Name, Value) :-
    get(Ev, detail, Detail),
    Detail \== @nil,
    get(Detail?class, instance_variables, Vars),
    get(Vars, low_index, Low),
    get(Vars, high_index, High),
    between(Low, High, I),
    get(Vars, element, I, Var),
    get(Var, name, Name),
    get(Detail, Name, Value0),
    format_value(Value0, Value).

format_value(Value, Text) :-
    float(Value),
    !,
    format(atom(Text), '~3f', [Value]).
format_value(Value, Text) :-
    atomic(Value),
    !,
    Text = Value.
format_value(@nil, '') :-
    !.
format_value(@on, '@on') :-
    !.
format_value(@off, '@off') :-
    !.
format_value(Obj, Text) :-
    send(Obj, instance_of, char_array),
    !,
    get(Obj, value, Text).
format_value(Obj, Text) :-
    format(atom(Text), '~p', [Obj]).

show_event(V, Ev:event) :->
    "Show last event"::
    get(V, member, event_hierarchy_window, EH),
    send(V, update_dialog, Ev),
    send(EH, show, Ev).

append_event(V, Ev:event) :->
    get(V, member, event_list, EL),
    send(EL, append, Ev),
    send(V, show_event, Ev).

fill_picture(V) :->
    get(V, member, picture, P),
    send(P, display, new(B, event_landing)),
    send(P, keyboard_focus, B),
    send(P, resize_message, message(V, resize_box, B)).

resize_box(_V, B:box) :->
    get(B, device, P),
    get(P, visible, area(X, Y, W, H)),
    send(B, set, X+W/5, Y+H/5, 3*W/5, 3*H/5).

:- pce_end_class.

:- pce_begin_class(event_landing, box,
                   "The event-sensitive area").

initialise(B) :->
    send_super(B, initialise, 100, 100),
    send(B, fill, colour(dark_green)).

'_wants_keyboard_focus'(_) :->
    true.

event(B, Ev:event) :->
    ignore(send_super(B, event, Ev)),
    send(?(B, container, man_frame), append_event, Ev).

:- pce_end_class(event_landing).

:- pce_begin_class(event_hierarchy_window, toc_window,
                   "Show event-hierarchy").

initialise(EH) :->
    send_super(EH, initialise),
    get(@event_tree, root, Root),
    send(EH, root, toc_folder(Root?value, Root)),
    send(EH, expand_root).

expand_node(W, Node:event_node) :->
    (   get(Node, sons, Sons),
        Sons \== @nil
    ->  send(Node?sons, for_all,
             message(W, add_node, Node, @arg1))
    ;   true
    ).

add_node(W, Parent:event_node, Son:event_node) :->
    (   (   get(Son, sons, Sons),
            Sons \== @nil,
            \+ send(Sons, empty)
        ;   get(Son, value, Id),
            special(Id)
        )
    ->  send(W, son, Parent, toc_folder(Son?value, Son))
    ;   send(W, son, Parent, toc_file(Son?value, Son))
    ).

special(control).
special(printable).
special(meta).

show(W, Ev:event) :->
    "Show indicated event-id"::
    get(Ev, id, Id),
    (   special(Special),
        send(Ev, is_a, Special)
    ->  get(@event_tree, node, Special, SpecialEvNode),
        find(W, SpecialEvNode, SpecialNode),
        get(Ev, key, Key),
        get(string('%s (%d)', Key, Id), value, KeyId),
        (   get(W, node, KeyId, Node)
        ->  true
        ;   send(W, son, SpecialEvNode, new(Node, toc_file(KeyId, KeyId))),
            send_class(SpecialNode, node, collapsed(@off))
        )
    ;   get(@event_tree, node, Id, EvNode),
        find(W, EvNode, Node)
    ),
    get(Node?tree, window, Window),
    send(Window, normalise, Node),
    get(Node, member, bitmap, BM),
    send(BM, flash),
    send(Node?tree, selection, Node).

find(W, EvNode, Node) :-
    (   get(W, node, EvNode, Node)
    ->  true
    ;   get(EvNode, parent, ParentEvNode),
        find(W, ParentEvNode, ParentNode),
        send(W, add_node, ParentEvNode, EvNode),
        send_class(ParentNode, node, collapsed(@off)),
        get(W, node, EvNode, Node)
    ).

:- pce_end_class.

:- pce_begin_class(event_list, browser).

variable(repeat, int := 1, get, "How often the event at the end arrived").

initialise(EL) :->
    send_super(EL, initialise),
    send(EL, select_message,
         message(?(EL, container, man_frame), show_event, @arg1?object)).

%       An event that repeats -- the run of `loc_move' a mouse dragged
%       across the box makes -- counts up on the line it is already on
%       rather than filling the list with copies of itself.  That line
%       keeps the last of them, which is the one selecting it shows.

append(EL, Ev:event) :->
    "Add an event, or count it if it repeats the one at the end"::
    get(Ev, clone, Clone),
    event_label(Clone, Label),
    (   get(EL, members, Items),
        get(Items, tail, DI),
        get(DI, key, Label)
    ->  get(EL, repeat, Repeat0),
        Repeat is Repeat0+1,
        send(EL, slot, repeat, Repeat),
        send(DI, label, string('%s (%d×)', Label, Repeat)),
        send(DI, object, Clone)
    ;   send(EL, slot, repeat, 1),
        send_super(EL, append, new(DI, dict_item(Label, @default, Clone)))
    ),
    send(EL, normalise, DI),
    send(EL, selection, DI).

%!  event_label(+Event, -Label) is det.
%
%   What the line is called.  A keystroke says which key it was; anything
%   else says what kind of event it is.

event_label(Ev, Label) :-
    (   send(Ev, is_a, keyboard)
    ->  get(Ev, key, Label)
    ;   get(Ev, id, Label)
    ).

:- pce_end_class.

%!  description(+Event, -Description) is semidet.
%
%   The summary of the event_node of the type of Event.  A key that
%   types a character has no node of its own; it has the summary of the
%   kind of key it is.

description(Ev, Description) :-
    get(Ev, id, Id),
    (   atom(Id),
        get(@event_tree, node, Id, Node)
    ->  true
    ;   special(Kind),
        send(Ev, is_a, Kind)
    ->  get(@event_tree, node, Kind, Node)
    ),
    get(Node, summary, Summary),
    Summary \== @nil,
    get(Summary, value, Description).
