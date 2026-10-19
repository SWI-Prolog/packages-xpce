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

:- module(test_event_viewer, [test_event_viewer/0]).
:- encoding(utf8).

/** <module> The event viewer of the XPCE manual

The event viewer (library(man/showevent)) shows the events that arrive
at its landing box: a description of the event type, its properties and
the device specific data of `event <-detail'.

Run with:

    swipl -g test_event_viewer -t halt \
          packages/xpce/tests/test_event_viewer.pl
*/

:- set_prolog_flag('SDL_VIDEODRIVER', dummy).
:- use_module(xpce_test_sandbox).
:- initialization(sandbox_app_config(_), now).
:- use_module(library(pce)).
:- initialization(remove_sandbox_at_exit, now).
:- use_module(library(plunit)).
:- use_module(library(pce_manual), []).
:- use_module(library(man/showevent), []).

test_event_viewer :-
    run_tests([ event_viewer
              ]).

%!  event_id(-Id) is nondet.
%
%   Each event type of @event_tree.

event_id(Id) :-
    get(@event_tree, root, Root),
    event_id(Root, Id).

event_id(Node, Id) :-
    get(Node, value, Id).
event_id(Node, Id) :-
    get(Node, sons, Sons),
    Sons \== @nil,
    chain_list(Sons, List),
    member(Son, List),
    event_id(Son, Id).

%!  show(+Id, +Detail, -Viewer) is det.
%
%   Make an event viewer and let it show an event Id with Detail.

show(Id, Detail, V) :-
    new(V, man_event_viewer(@manual)),
    show_next(V, Id, Detail).

%!  show_next(+Viewer, +Id, +Detail) is det.
%
%   Let Viewer show another event.

show_next(V, Id, Detail) :-
    get(V, member, picture, P),
    get(P, member, event_landing, B),
    new(Ev, event(Id, P, 30, 40)),
    send(Ev, slot, receiver, B),
    send(Ev, slot, detail, Detail),
    send(V, append_event, Ev).

%!  shown(+Viewer, +Item, -Value) is det.

shown(V, Item, Value) :-
    get(V, member, dialog, D),
    get(D, member, Item, TI),
    get(TI, selection, Value).

detail_lines(V, Lines) :-
    get(V, member, dialog, D),
    get(D, member, detail, DB),
    get(DB?members, map, @arg1?label, Labels),
    chain_list(Labels, Strings),
    maplist([S,A]>>get(S, value, A), Strings, Lines).

:- begin_tests(event_viewer).

%       Each event type says what it means in event_node<-summary.

test(every_event_type_has_a_summary, Missing == []) :-
    findall(Id,
            ( event_id(Id),
              get(@event_tree, node, Id, Node),
              get(Node, summary, @nil)
            ),
            Missing).

%       The viewer shows the summary of the type of the event.  A key
%       that types a character shows that of the kind of key.

test(description, Descriptions == [ 'Move with left mouse-button down',
                                    'Printable key-press event',
                                    'Non-printable key-press event'
                                  ]) :-
    findall(D,
            ( member(Id, [ms_left_drag, 0'a, 1]),
              show(Id, @nil, V),
              shown(V, description, D),
              send(V, destroy)
            ),
            Descriptions).

test(button_event, [ Button-Click-Rotation == left-single-'' ]) :-
    show(ms_left_down, @nil, V),
    shown(V, button, Button),
    shown(V, click, Click),
    shown(V, rotation, Rotation),
    send(V, destroy).

test(pen_detail, [ Lines == [ 'pressure\t0.520', 'xtilt\t12', 'ytilt\t-3',
                              'hover_distance\t0', 'barrel_rotation\t0',
                              'slider\t0', 'tangential_pressure\t0',
                              'eraser\t@off', 'barrel_buttons\t0'
                            ]
                 ]) :-
    show(ms_left_down, pen_state(0.52, 12, -3), V),
    detail_lines(V, Lines),
    send(V, destroy).

%       What the previous event showed goes when it does not apply.

test(no_detail, [ Lines-Button == []-'' ]) :-
    show(ms_left_down, pen_state(0.52), V),
    show_next(V, loc_move, @nil),
    detail_lines(V, Lines),
    shown(V, button, Button),
    send(V, destroy).

:- end_tests(event_viewer).
