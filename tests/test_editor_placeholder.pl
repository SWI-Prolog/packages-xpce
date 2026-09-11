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

:- module(test_editor_placeholder, [test_editor_placeholder/0]).
:- encoding(utf8).

/** <module> Tests for `editor ->placeholder'

An editor that holds no text writes its <-placeholder where the text
would be, faded to `editor.placeholder_opacity' so that it reads as a
prompt rather than as content.  The text image paints it -- it is what
covers the text area -- and stops as soon as there is text to show.

What it looks like cannot be tested here: the tests run against the SDL
dummy driver, which paints nothing.  These cover the contract.

Run with:

    swipl -g test_editor_placeholder -t halt \
          packages/xpce/tests/test_editor_placeholder.pl
*/

:- set_prolog_flag('SDL_VIDEODRIVER', dummy).

:- use_module(library(pce)).
:- use_module(library(plunit)).

test_editor_placeholder :-
    run_tests([ editor_placeholder
              ]).

:- begin_tests(editor_placeholder).

test(an_editor_has_none_to_start_with, P == @nil) :-
    new(E, editor),
    get(E, placeholder, P).

test(and_says_what_it_was_given, P == 'Type a query') :-
    new(E, editor),
    send(E, placeholder, 'Type a query'),
    get(E, placeholder, P).

test(a_string_is_taken_as_well, P == 'Type a query') :-
    new(E, editor),
    send(E, placeholder, string('Type a %s', query)),
    get(E, placeholder, P0),
    get(P0, value, P).

test(and_nil_takes_it_away, P == @nil) :-
    new(E, editor),
    send(E, placeholder, 'Type a query'),
    send(E, placeholder, @nil),
    get(E, placeholder, P).

%       Setting one while there is text to show is no error: it is what
%       the editor falls back on when the text goes.

test(it_can_be_set_while_there_is_text, P == 'Type a query') :-
    new(E, editor),
    send(E, append, 'something'),
    send(E, placeholder, 'Type a query'),
    get(E, placeholder, P).

%       How much of the text colour it is written in.  A user says so in
%       their Defaults file; the fading itself is `colour <-fade'.

test(the_opacity_is_a_class_variable, V =:= 0.5) :-
    get(@pce, convert, editor, class, Class),
    get(Class, class_variable, placeholder_opacity, CV),
    get(CV, value, V).

%       A view is a window around an editor and answers for it, so a
%       prompt can be put on the window the user has to hand.

test(a_view_answers_for_its_editor, P == 'Type a query') :-
    new(V, view),
    send(V, placeholder, 'Type a query'),
    get(V?editor, placeholder, P).

:- end_tests(editor_placeholder).
