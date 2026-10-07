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

:- module(test_visual, [test_visual/0]).

/** <module> Test the consists-of tree of visuals

Run with:

    swipl -g test_visual -t halt packages/xpce/tests/test_visual.pl
*/

:- use_module(library(pce)).
:- use_module(library(plunit)).

test_visual :-
    run_tests([visual_tree]).

frame(F, B) :-
    new(D, dialog),
    send(D, append, new(B, button(ok))),
    send(D, append, text_item(name)),
    new(F, frame),
    send(F, append, D).

:- begin_tests(visual_tree).

test(contained_class, [C == button]) :-
    frame(F, _),
    get(F, contained, class(button), B),
    get(B, class_name, C).
test(contained_self, [M == F]) :-
    frame(F, _),
    get(F, contained, class(frame), M).
test(contained_code, [C == text_item]) :-
    frame(F, _),
    get(F, contained, message(@arg1, instance_of, text_item), T),
    get(T, class_name, C).
test(contained_none, [fail]) :-
    frame(F, _),
    get(F, contained, class(slider), _).
test(container, [M == F]) :-
    frame(F, B),
    get(B, container, class(frame), M).

test(contained_direct, [fail]) :-     % the button is in the dialog
    frame(F, _),
    get(F, contained, class(button), @off, _).
test(contained_direct_member, [C == dialog]) :-
    frame(F, _),
    get(F, contained, class(dialog), @off, D),
    get(D, class_name, C).

:- end_tests(visual_tree).
