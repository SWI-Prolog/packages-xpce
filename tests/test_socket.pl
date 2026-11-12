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


:- module(test_socket, [test_socket/0]).

/** <module> Tests for xpce class socket

The sockets of xpce are servers that act on what they are sent, e.g.,
the PceEmacs server.  These tests check who can reach them.
*/

:- use_module(library(pce)).
:- use_module(library(plunit)).
:- use_module(library(process)).
:- use_module(library(readutil)).

test_socket :-
    run_tests([ pce_socket
              ]).

:- begin_tests(pce_socket).

test(unix_socket_mode,
     [ condition(\+ current_prolog_flag(windows, true)),
       setup(tmp_file(sock, File)),
       cleanup(( free(S), catch(delete_file(File), _, true) )),
       Mode == 'srw-------'
     ]) :-
    % Without the chmod() it was 0777 less the umask
    new(S, socket(file(File))),
    send(S, listen),
    process_create(path(ls), ['-ld', File], [stdout(pipe(Out))]),
    read_line_to_string(Out, Line),
    close(Out),
    sub_atom(Line, 0, 10, _, Mode).
:- end_tests(pce_socket).
