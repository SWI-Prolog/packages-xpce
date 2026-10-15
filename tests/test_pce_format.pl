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

:- module(test_pce_format, [test_pce_format/0]).

/** <module> Tests for formatting text in xpce

Run with:

    swipl -g test_pce_format -t halt \
          packages/xpce/tests/test_pce_format.pl
*/

:- use_module(library(pce)).
:- use_module(library(plunit)).

test_pce_format :-
    run_tests([ pce_format
              ]).

formatted(Format, Args, Text) :-
    Term =.. [string, Format|Args],
    new(S, Term),
    get(S, value, A),
    atom_string(A, Text).

:- begin_tests(pce_format).

test(directives, Text == "a 42 x") :-
    formatted("%s %d %c", [a, 42, 0'x], Text).
test(percent, Text == "100% done") :-
    formatted("100%% %s", [done], Text).
test(trailing_percent, Text == "100%") :-
    formatted("%d%", [100], Text).
test(star_width, Text == "|   42|42   |  x|") :-
    formatted("|%*d|%-*d|%*c|", [5, 42, 5, 42, 3, 0'x], Text).
test(star_string, Text == "|   ab|ab   |") :-
    formatted("|%*s|%-*s|", [5, ab, 5, ab], Text).
test(incomplete_flag, Text == "x%-") :-
    formatted("%s%-", [x], Text).
test(incomplete_width, Text == "x%5.2") :-
    formatted("%s%5.2", [x], Text).
test(unknown_conversion, Text == "x%-5z|") :-
    formatted("%s%-5z|", [x], Text).

:- end_tests(pce_format).
