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


:- module(test_pce_open, [test_pce_open/0]).

/** <module> The pce_open/3 protocol of source_sink

pce_open/3 opens a Prolog stream on an object that implements
->write_as_file and <-read_as_file.  A source_sink whose encoding is
`octet` gets the bytes written unchanged, and ->close_as_file tells
it that the stream was closed.

Run with:

    swipl -g test_pce_open -t halt packages/xpce/tests/test_pce_open.pl
*/

:- use_module(library(pce)).
:- use_module(library(plunit)).

test_pce_open :-
    run_tests([ pce_open ]).

:- pce_begin_class(tpo_sink, source_sink,
                   "Collect the data written using pce_open/3").

variable(data,   string,       get, "Data written").
variable(closed, int := 0,     get, "Times ->close_as_file was sent").

initialise(S, Encoding:[name]) :->
    send_super(S, initialise),
    send(S, slot, data, new(string)),
    (   Encoding == @default
    ->  true
    ;   send(S, slot, encoding, Encoding)
    ).

write_as_file(S, _At:[int], Data:char_array) :->
    send(S?data, append, Data).

truncate_as_file(S) :->
    send(S, slot, data, new(string)).

close_as_file(S) :->
    get(S, closed, N0),
    N is N0+1,
    send(S, slot, closed, N).

:- pce_end_class(tpo_sink).

written(S, Codes) :-
    get(S?data, value, Atom),
    atom_codes(Atom, Codes).

:- begin_tests(pce_open).

test(octet, [Codes-Closed == All-1]) :-
    numlist(0, 255, All),
    new(S, tpo_sink(octet)),
    pce_open(S, write, Out),
    forall(member(B, All), put_byte(Out, B)),
    close(Out),
    written(S, Codes),
    get(S, closed, Closed).
test(text, [Codes-Closed == Text-1]) :-
    Text = `héllo €`,
    new(S, tpo_sink),
    pce_open(S, write, Out),
    format(Out, "~s", [Text]),
    close(Out),
    written(S, Codes),
    get(S, closed, Closed).
test(read_does_not_close, [Closed == 0]) :-
    new(S, tpo_sink),
    pce_open(S, write, Out),
    close(Out),
    send(S, slot, closed, 0),
    pce_open(string(abc), read, In),    % a char_array; not a sink
    close(In),
    get(S, closed, Closed).

:- end_tests(pce_open).
