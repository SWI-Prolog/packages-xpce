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

:- module(test_preferences, [test_preferences/0]).

/** <module> Test library(pce_preferences)

Run with:

    swipl -g test_preferences -t halt packages/xpce/tests/test_preferences.pl
*/

:- use_module(library(pce)).
:- use_module(library(plunit)).
:- use_module(library(pce_preferences)).

test_preferences :-
    run_tests([preferences]).

:- pce_begin_class(tp_dialog, dialog).
:- pce_end_class(tp_dialog).
:- pce_begin_class(tp_sub_dialog, tp_dialog).
:- pce_end_class(tp_sub_dialog).

:- multifile pce_preferences:preferences/2.

pce_preferences:preferences(tp_dialog,
    [ tp_dialog - [background],
      button    - [label_font],
      text_item - [length, value_font],
      slider    - [width],
      via(button, frame, window) - [],
      no_such_class - [foo]
    ]).

component(Class, Button, Root, Spec) :-
    Term =.. [Class],
    new(D, Term),
    send(D, append, new(Button, button(ok))),
    send(D, append, text_item(name)),
    new(F, frame),
    send(F, append, D),
    preference_component(Button, Root, Spec).

sections(Root, Spec, Sections) :-
    preference_sections(Root, Spec, Sections0),
    maplist(section_name, Sections0, Sections).

section_name(section(Class, Names), Name-Names) :-
    get(Class, name, Name).

:- begin_tests(preferences).

test(root, [C == tp_dialog]) :-
    component(tp_dialog, _, Root, _),
    get(Root, class_name, C).
test(inherited, [C == tp_sub_dialog]) :-
    component(tp_sub_dialog, _, Root, _),
    get(Root, class_name, C).
test(no_spec, [fail]) :-
    component(dialog, _, _, _).
test(sections, [Sections == [ tp_dialog-[background],
                              button-[label_font],
                              text_item-[length, value_font],
                              slider-[width],
                              frame-[]
                            ]]) :-
    component(tp_dialog, _, Root, Spec),
    sections(Root, Spec, Sections).

:- end_tests(preferences).
