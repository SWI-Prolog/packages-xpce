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


:- module(test_prolog_mode, [test_prolog_mode/0]).
:- encoding(utf8).

/** <module> Tests for the PceEmacs Prolog mode

Clause navigation and what depends on it.  A mode is not a text object:
it delegates to its editor, so a method of the mode that hands itself to
something wanting a text_buffer raises a type error, the get fails and
whatever asked quietly does the wrong thing instead.  `<-forward_clause'
did that, which made Alt-Q fill a clause as if it were a paragraph of
prose rather than indent it.

Run with:

    swipl -g test_prolog_mode -t halt \
          packages/xpce/tests/test_prolog_mode.pl
*/

:- set_prolog_flag('SDL_VIDEODRIVER', dummy).
:- set_prolog_flag(emacs_server, false).

:- use_module(library(pce)).
:- use_module(library(plunit)).
:- use_module(library(pce_emacs)).

test_prolog_mode :-
    run_tests([ clause_navigation ]).

                 /*******************************
                 *            HELPERS           *
                 *******************************/

:- dynamic buffer_count/1.

%!  mode(+Text, -Mode) is det.
%
%   A Prolog mode over a scratch buffer holding Text.  The buffers of a
%   run share @emacs_buffers, so each gets a name of its own.

mode(Text, M) :-
    start_emacs,
    (   retract(buffer_count(N0))
    ->  N is N0+1
    ;   N = 1
    ),
    assertz(buffer_count(N)),
    format(atom(Name), '*test-prolog-mode-~d*', [N]),
    new(B, emacs_buffer(@nil, Name)),
    send(B, mode, prolog),
    send(B, insert, 0, Text),
    new(V, emacs_view(B)),
    get(V, mode, M).

%!  contents(+Mode, -Text) is det.

contents(M, Text) :-
    get(M, text_buffer, TB),
    get(TB, contents, String),
    get(String, value, Text).

%!  clauses(-Text) is det.
%
%   Two clauses; the first ends at 33, which is just after its full stop.

clauses('foo(X) :-\n    bar(X),\n    baz(X).\n\nqux(1).\n').

                 /*******************************
                 *             TESTS            *
                 *******************************/

:- begin_tests(clause_navigation).

test(forward_clause_finds_the_end_of_the_first, true(EOC == 33)) :-
    clauses(Text),
    mode(Text, M),
    get(M, forward_clause, 0, EOC).

test(and_the_end_of_the_one_after_it, true(EOC == 42)) :-
    clauses(Text),
    mode(Text, M),
    get(M, forward_clause, 33, EOC).

test(it_fails_past_the_last_clause, fail) :-
    clauses(Text),
    mode(Text, M),
    get(M, forward_clause, 42, _).

%       ->fill_paragraph indents the clause the caret is in, which it can
%       only tell by asking <-forward_clause where the clause ends.

test(fill_paragraph_indents_the_clause_it_is_in, true(After == Before)) :-
    clauses(Text),
    mode(Text, M),
    contents(M, Before),
    send(M, caret, 15),
    send(M, fill_paragraph, 1),
    contents(M, After).

:- end_tests(clause_navigation).
