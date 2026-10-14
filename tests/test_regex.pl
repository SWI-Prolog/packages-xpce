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

:- module(test_regex, [test_regex/0]).

/** <module> Tests for class regex

Run with:

    swipl -g test_regex -t halt packages/xpce/tests/test_regex.pl
*/

:- use_module(library(pce)).
:- use_module(library(plunit)).

test_regex :-
    run_tests([ regex_newline
              ]).

%   search_end(+Regex, -End)
%
%   End of the first match of Regex in a two-line text_buffer.  Regex
%   is a regex object or a term to create one.

search_end(Regex, End) :-
    object(Regex),
    !,
    new(TB, text_buffer),
    send(TB, append, 'foo bar\nbaz qux\n'),
    send(Regex, search, TB, 0),
    get(Regex, register_end, End).
search_end(Term, End) :-
    new(Regex, Term),
    search_end(Regex, End).

:- begin_tests(regex_newline).

test(dot_crosses_newline, End == 16) :-
    search_end(regex('o.*'), End).
test(default_off, NS == @off) :-
    new(R, regex('o.*')),
    get(R, newline_sensitive, NS).
test(dot_stops, End == 7) :-
    search_end(regex('o.*', @default, @default, @on), End).
test(negated_set_stops, End == 7) :-
    search_end(regex('[^x]*', @default, @default, @on), End).
test(explicit_newline, End == 9) :-
    search_end(regex('r\\nb', @default, @default, @on), End).
test(no_match_across, fail) :-
    search_end(regex('o.*z', @default, @default, @on), _).
test(basic, End == 7) :-
    search_end(regex('o.*', @default, basic, @on), End).
test(extended, End == 7) :-
    search_end(regex('o.*', @default, extended, @on), End).
test(anchors, End == 15) :-
    search_end(regex('^baz.*$', @default, @default, @on), End).
test(toggle, [End0, End1] == [16, 7]) :-
    new(R, regex('o.*')),
    search_end(R, End0),
    send(R, newline_sensitive, @on),
    search_end(R, End1).

:- end_tests(regex_newline).
