/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org/projects/xpce/
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

:- module(test_terminal_client,
          [ test_terminal_client/0
          ]).
:- use_module(library(pce)).
:- use_module(library(plunit)).
:- use_module(library(process)).
:- use_module(library(apply)).

/** <module> Test the descriptors the xpce terminal hands its client

The three descriptors a client runs on are the terminal as far as it,
and anything it starts, is concerned, and a terminal is one device that
a program both reads and writes.  Asking it a question is how a client
learns what nobody can tell it: the colours it draws on, where the caret
is.  The question goes to stdout and the answer comes back from that
same descriptor -- what termenv does for the OSC 10/11 colour queries,
and with it every Go program that has a terminal user interface, `gh'
among them.  That only works if stdout is open for reading as well,
which is what this tests, along with the shape of the answer.
*/

test_terminal_client :-
    run_tests([ terminal_client_query
              ]).

                /*******************************
                *            HARNESS           *
                *******************************/

%!  terminal(-Terminal) is det.
%!  destroy_terminal(+Terminal) is det.

terminal(TI) :-
    new(TI, terminal_image(1000, 500)),
    new(W, window('test_terminal_client')),
    send(W, display, TI),
    send(W, open),
    send(W, wait).

destroy_terminal(TI) :-
    get(TI, window, W),
    send(W, destroy).

%!  dispatch_until(:Goal, +Timeout) is semidet.
%
%   Dispatch xpce events until Goal succeeds or Timeout seconds pass.
%   The terminal only sees what the client wrote while we dispatch:
%   reading the pty is an event like any other.

:- meta_predicate dispatch_until(0, +).
dispatch_until(Goal, Timeout) :-
    get_time(Now),
    Deadline is Now+Timeout,
    dispatch_until_(Goal, Deadline).

dispatch_until_(Goal, Deadline) :-
    (   call(Goal)
    ->  true
    ;   get_time(Now),
        Now < Deadline,
        pce_principal:pce_dispatch(-1, 0.05),
        dispatch_until_(Goal, Deadline)
    ).

%!  ask_terminal(+Terminal, +Query, -Answer:string) is semidet.
%
%   Run a child on Terminal that writes Query to its stdout and reads
%   the answer back from that same descriptor.  The child is the point
%   of the exercise: it runs on the client's descriptors, the way a
%   program started from a shell in this terminal does.
%
%   It puts the line discipline in raw mode first, as anything that
%   queries a terminal must: the answer carries no newline, so in
%   canonical mode there would be nothing to read until the user hits
%   return.  Raw mode is also what makes one read enough: it returns as
%   soon as there is anything, and the terminal writes its answer in
%   one go.  Reading a fixed count instead would hang out the ten
%   seconds below on an answer that is merely too short.
%
%   Descriptor 3 is the pipe we read the answer from, which leaves
%   descriptor 2 free to swallow what dd reports about itself.

ask_terminal(TI, Query, Answer) :-
    pce_open_terminal_image(TI, In, Out, Err),
    Cmd = 'stty raw -echo <&1; exec 3>&2 2>/dev/null; \c
           printf %s "$1"; exec dd bs=256 count=1 <&1 >&3',
    setup_call_cleanup(
        process_create(path(sh), ['-c', Cmd, sh, Query],
                       [ stdout(stream(Out)),
                         stderr(pipe(Pipe)),
                         process(PID)
                       ]),
        (   dispatch_until(process_wait(PID, exit(_), [timeout(0)]), 10),
            read_string(Pipe, _, Answer)
        ),
        maplist([S]>>close(S, [force(true)]), [Pipe,In,Out,Err])).

                /*******************************
                *             TESTS            *
                *******************************/

:- begin_tests(terminal_client_query,
               [ condition(\+ current_prolog_flag(windows, true))
               ]).

%   `ESC ] 11 ; ? ST' asks for the background colour.  The answer is
%   `ESC ] 11 ; rgb:RRRR/GGGG/BBBB ST', 25 bytes: four hex digits per
%   channel, as xterm reports them.  Two would be just as valid an X
%   colour name, but termenv only accepts an answer as long as xterm's.
%
%   A failure prints the answer we did get, which is the whole of the
%   diagnosis: an empty one says the child could not read its stdout.

test(background_colour, [ setup(terminal(TI)),
                          cleanup(destroy_terminal(TI)),
                          Got == ok
                        ]) :-
    ask_terminal(TI, "\e]11;?\e\\", Answer),
    (   colour_answer(11, Answer)
    ->  Got = ok
    ;   Got = Answer
    ).

%!  colour_answer(+Param, +Answer) is semidet.

colour_answer(Param, Answer) :-
    format(string(Prefix), "\e]~w;rgb:", [Param]),
    string_concat(Prefix, Rest, Answer),
    string_concat(Hex, "\e\\", Rest),
    split_string(Hex, "/", "", [R,G,B]),
    maplist(hex4, [R,G,B]).

hex4(Hex) :-
    string_length(Hex, 4),
    string_codes(Hex, Codes),
    forall(member(C, Codes), code_type(C, xdigit(_))).

:- end_tests(terminal_client_query).
