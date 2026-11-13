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
arguments must therefore not hang it, and invalid UTF-8 must not
decode as something else.
*/

test_terminal_limits :-
    run_tests([ terminal_limits,
                terminal_utf8
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

		 /*******************************
		 *	      UTF-8		*
		 *******************************/

%!  shows(+Bytes, -Codes) is det.
%
%   Codes is the first row of a fresh terminal after a client wrote
%   Bytes to it.  The bytes go through the pty, which is where the
%   terminal decodes UTF-8.  A `|` ends the row: we wait for it.

shows(Bytes, Codes) :-
    new(TI, terminal_image(400, 100)),
    get(TI, pty_name, Pty),
    setup_call_cleanup(
        open(Pty, write, Out, [type(binary)]),
        ( append(Bytes, `|\r\n`, All),
          maplist(put_byte(Out), All),
          flush_output(Out),
          get_time(Now),
          Deadline is Now+10,
          shown(TI, Deadline, Codes)
        ),
        close(Out)),
    free(TI).

shown(TI, Deadline, Codes) :-
    ignore(send(@display, dispatch)),
    get(TI, row, 0, String),
    get(String, value, Atom),
    atom_codes(Atom, Codes0),
    (   append(Codes, [0'||_], Codes0)
    ->  true
    ;   get_time(Now),
        Now < Deadline
    ->  shown(TI, Deadline, Codes)
    ;   Codes = timeout
    ).

has_pty :-
    \+ current_prolog_flag(windows, true).

:- begin_tests(terminal_utf8, [condition(has_pty)]).

test(valid, Codes == [0xe9, 0x1F600]) :-
    shows([0xC3,0xA9, 0xF0,0x9F,0x98,0x80], Codes).
test(overlong_slash, Codes == [0xC0, 0xAF]) :-
    shows([0xC0,0xAF], Codes).
test(overlong_3, Codes == [0xE0, 0x80, 0xAF]) :-
    shows([0xE0,0x80,0xAF], Codes).
test(surrogate, Codes == [0xFFFD]) :-
    shows([0xED,0xA0,0x80], Codes).
test(too_large, Codes == [0xF4, 0x90, 0x80, 0x80]) :-
    shows([0xF4,0x90,0x80,0x80], Codes).
test(five_bytes, Codes == [0xF8, 0x88, 0x80, 0x80, 0x80]) :-
    shows([0xF8,0x88,0x80,0x80,0x80], Codes).
test(truncated, Codes == [0xE2, 0x82]) :-
    shows([0xE2,0x82], Codes).

:- end_tests(terminal_utf8).
