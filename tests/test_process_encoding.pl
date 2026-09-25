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

:- module(test_process_encoding, [test_process_encoding/0]).
:- encoding(utf8).

/** <module> Tests for the encoding of process (stream) data

Class stream (and thus process and socket) encodes text written using
->append, ->format, etc. and decodes the data read according to its
`encoding` slot, which defaults to `utf8`.  These tests talk to Unix
tools (sh, cat, od and iconv) and check both the bytes on the wire and
the text that comes back.

Run with:

    swipl -g test_process_encoding -t halt \
          packages/xpce/tests/test_process_encoding.pl
*/

:- use_module(library(pce)).
:- use_module(library(plunit)).

test_process_encoding :-
    run_tests([ process_encoding ]).

:- dynamic
    received/1.

received_data(Str) :-
    get(Str, value, Text),
    assertz(received(Text)).

%!  open_process(+Command, +Encoding, -Process) is det.
%
%   Start Command using `/bin/sh -c`, collecting the records it writes
%   in received/1.

open_process(Command, Encoding, P) :-
    retractall(received(_)),
    new(P, process('/bin/sh', '-c', Command)),
    send(P, use_tty, @off),
    (   Encoding == default
    ->  true
    ;   send(P, encoding, Encoding)
    ),
    send(P, input_message, message(@prolog, received_data, @arg1)),
    send(P, open).

%!  close_process(+Process, -Records) is det.

close_process(P, Records) :-
    send(P, close),
    send(P, wait),
    findall(Text, retract(received(Text)), Records).

%!  run_process(+Command, +Encoding, +Data, -Output) is det.
%
%   Run Command, ->append Data to it and return the concatenated
%   records received as Output.

run_process(Command, Encoding, Data, Output) :-
    open_process(Command, Encoding, P),
    call_cleanup(send_data(P, Data),
                 close_process(P, List)),
    atomic_list_concat(List, Output).

send_data(_, "") :-
    !.
send_data(P, Data) :-
    send(P, append, Data).

%   Hex dump of the bytes sent to the process, as one line.

hex_command('od -An -tx1 | tr -d " \\n"; echo').

:- begin_tests(process_encoding).

test(default, Enc == utf8) :-
    new(P, process(cat)),
    get(P, encoding, Enc),
    free(P).
test(utf8_bytes, Out == 'c3a9e28880f09f9880\n') :-
    hex_command(Cmd),
    run_process(Cmd, default, "é∀😀", Out).
test(utf8_roundtrip, Out == 'héllo ∀x 😀\n') :-
    run_process(cat, default, "héllo ∀x 😀\n", Out).
test(utf8_split_sequence, Out == 'a∀b\n') :-
    run_process('printf "a\\342"; sleep 0.2; printf "\\210\\200b\\n"',
                default, "", Out).
test(format, List == ['∀=42\n']) :-
    open_process(cat, default, P),
    send(P, format, "%s=%d\n", "∀", 42),
    close_process(P, List).
test(record_separator, List == ['∀;', '😀;']) :-
    open_process(cat, default, P),
    send(P, record_separator, regex(';')),
    send(P, append, "∀;😀;"),
    close_process(P, List).
test(read_line, Line == '∀ line\n') :-
    new(P, process(cat)),
    send(P, use_tty, @off),
    send(P, open),
    send(P, append, "∀ line\n"),
    get(P, read_line, Str),
    get(Str, value, Line),
    send(P, close),
    send(P, wait).
test(latin1_bytes, Out == 'e9\n') :-
    hex_command(Cmd),
    run_process(Cmd, iso_latin_1, "é", Out).
test(latin1_roundtrip, Out == 'é\n') :-
    run_process(cat, iso_latin_1, "é\n", Out).
test(latin1_unrepresentable,
     error(pce(representation, [_, encoding]), _)) :-
    run_process(cat, iso_latin_1, "∀", _).
test(utf16be_bytes, Out == '0061d83dde00\n') :-
    hex_command(Hex),
    format(atom(Cmd), '(~w) | iconv -f ascii -t utf-16be', [Hex]),
    run_process(Cmd, unicode_be, "a😀", Out).
test(utf16be_roundtrip, Out == 'a😀∀\n') :-
    run_process(cat, unicode_be, "a😀∀\n", Out).
test(utf16le_roundtrip, Out == 'a😀∀\n') :-
    run_process(cat, unicode_le, "a😀∀\n", Out).

:- end_tests(process_encoding).
