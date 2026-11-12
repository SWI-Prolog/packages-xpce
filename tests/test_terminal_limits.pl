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


:- module(test_terminal_limits, [test_terminal_limits/0]).

:- use_module(library(pce)).
:- use_module(library(plunit)).

/** <module> Test the xpce terminal against absurd escape sequences

What a terminal shows is often not under control of its user: `cat`
of a file someone else wrote is enough.  Escape sequences with huge
arguments must therefore not hang it.
*/

test_terminal_limits :-
    run_tests([ terminal_limits
              ]).

terminal(TI) :-
    new(TI, terminal_image(1000, 500)),
    new(W, window('test_terminal_limits')),
    send(W, display, TI),
    send(W, open),
    send(W, wait).

destroy_terminal(TI) :-
    get(TI, window, W),
    send(W, destroy).

row(TI, Row, Text) :-
    get(TI, row, Row, String),
    get(String, value, Atom),
    normalize_space(atom(Text), Atom).

:- begin_tests(terminal_limits).

test(insert_chars, [ setup(terminal(TI)), cleanup(destroy_terminal(TI)),
                     timeout(20),
                     Text == ''
                   ]) :-
    % inserted 2147483647 spaces, one at a time
    send(TI, insert, "abc\r\e[2147483647@"),
    row(TI, 0, Text).
test(huge_argument, [ setup(terminal(TI)), cleanup(destroy_terminal(TI)),
                      timeout(20),
                      Text == ''
                    ]) :-
    % the argument overflowed int; it now saturates
    send(TI, insert, "abc\r\e[99999999999999999999@"),
    row(TI, 0, Text).

test(caret_down, [ setup(terminal(TI)), cleanup(destroy_terminal(TI)),
                   timeout(20),
                   Text == x
                 ]) :-
    % added a line for each of 2147483647 rows down
    send(TI, insert, "\e[2147483647Bx"),
    get(TI, rows, Rows),
    LastRow is Rows - 1,
    row(TI, LastRow, Text).

:- end_tests(terminal_limits).
