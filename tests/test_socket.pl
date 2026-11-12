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

/** <module> Tests for xpce class socket and library(pce_server)

The sockets of xpce are servers that act on what they are sent, e.g.,
the PceEmacs server and pce_server/1.  These tests check who can reach
them.
*/

:- use_module(library(pce)).
:- use_module(library(plunit)).
:- use_module(library(pce_server)).
:- use_module(library(process)).
:- use_module(library(socket)).
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
test(pce_server_localhost,
     [ condition(current_prolog_flag(threads, true)),
       setup(( free_port(Port), pce_server(Port) )),
       cleanup(free_server(Port)),
       Reply == "yes"
     ]) :-
    server_reply(localhost:Port, Reply).

free_port(Port) :-
    tcp_socket(S),
    tcp_bind(S, localhost:Port),
    tcp_close_socket(S).

free_server(Port) :-
    (   get(@open_sockets, find,
            and(@arg1?status == listen,
                @arg1?address?second == Port),
            S)
    ->  free(S)
    ;   true
    ).

%   The server runs in the xpce thread, so we must talk to it from
%   another thread when xpce is running in this one.

server_reply(Address, Reply) :-
    thread_self(Me),
    thread_create(client(Me, Address), Id),
    wait_reply(Id),
    thread_join(Id, Status),
    Status == true,
    thread_get_message(Me, server_reply(Reply), [timeout(0)]).

client(Me, Address) :-
    setup_call_cleanup(
        tcp_connect(Address, Stream, []),
        ( format(Stream, 'true.~n', []),
          flush_output(Stream),
          read_reply(Stream, Reply)
        ),
        close(Stream)),
    thread_send_message(Me, server_reply(Reply)).

read_reply(Stream, Reply) :-
    read_line_to_string(Stream, Line),
    (   Line == end_of_file
    ->  Reply = Line
    ;   sub_string(Line, B, _, _, "yes")
    ->  sub_string(Line, B, 3, _, Reply)
    ;   read_reply(Stream, Reply)
    ).

wait_reply(Id) :-
    between(1, 500, _),
    (   catch(thread_property(Id, status(running)), _, fail)
    ->  ignore(send(@display, dispatch)),   % our own event loop
        sleep(0.01),
        fail
    ;   true
    ),
    !.
wait_reply(_).

:- end_tests(pce_socket).
