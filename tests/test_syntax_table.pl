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

:- module(test_syntax_table, [test_syntax_table/0]).

/** <module> Class syntax_table

A syntax table created from another one (syntax_table(Name, Parent))
starts as a copy of it.  PceEmacs creates the table of a mode this way
from the table of its super mode, e.g., C++ from C.

Run with:

    swipl -g test_syntax_table -t halt packages/xpce/tests/test_syntax_table.pl
*/

:- use_module(library(pce)).
:- use_module(library(plunit)).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(yall)).

test_syntax_table :-
    run_tests([ syntax_table ]).

parent(T) :-
    new(T, syntax_table),
    send(T, line_comment, '//'),
    send(T, sentence_end, regex('[.]\\s')),
    send(T, paragraph_end, regex('\\s*\n\n')),
    send(T, quasi_quotation_start, '{|'),
    send(T, quasi_quotation_end, '|}'),
    send(T, add_syntax, '/', comment_start, '*'),
    send(T, add_syntax, '*', comment_end, '/').

pattern(T, Slot, Pattern) :-
    get(T, Slot, Re),
    get(Re, pattern, Pattern0),
    get(Pattern0, value, Pattern).

comments(T, Text, Comments) :-
    new(TB, text_buffer(Text)),
    send(TB, syntax, T),
    new(Ch, chain),
    send(TB, for_all_syntax,
         if(@arg3 == comment,
            message(Ch, append, ?(TB, contents, @arg1, @arg2 - @arg1)))),
    chain_list(Ch, L),
    maplist([S,A]>>get(S, value, A), L, Comments).

:- begin_tests(syntax_table).

test(line_comment, LC == '//') :-
    parent(P),
    new(T, syntax_table(@default, P)),
    get(T, line_comment, LC).
test(sentence_end, Pattern == '[.]\\s') :-
    parent(P),
    new(T, syntax_table(@default, P)),
    pattern(T, sentence_end, Pattern).
test(paragraph_end, Pattern == '\\s*\n\n') :-
    parent(P),
    new(T, syntax_table(@default, P)),
    pattern(T, paragraph_end, Pattern).
test(quasi_quotation, Q == '{|'-'|}') :-
    parent(P),
    new(T, syntax_table(@default, P)),
    get(T, quasi_quotation_start, S),
    get(T, quasi_quotation_end, E),
    Q = S-E.
test(fresh, Values == [@nil, @nil, @nil]) :-
    new(T, syntax_table),
    get(T, line_comment, LC),
    get(T, quasi_quotation_start, S),
    get(T, quasi_quotation_end, E),
    Values = [LC, S, E].
test(derived_comments, Comments == ['// line', '/* block */']) :-
    parent(P),
    new(T, syntax_table(@default, P)),
    comments(T, "// line\nx = 1; /* block */\n", Comments).

:- end_tests(syntax_table).
