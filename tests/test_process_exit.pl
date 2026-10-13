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
:- module(test_process_exit, [test_process_exit/0]).

/** <module> Tests for reporting that a process terminated

SIGCHLD only wakes the main thread, which reaps the children and sends
->exited or ->killed to the process from its event loop.  These tests
start processes and dispatch events until all have reported.

Termination is reported when the process terminates, also if its
output is held open by a process it started (a background `sleep` in
these tests), and after the output it wrote has been handled.

Run with:

    swipl -g test_process_exit -t halt \
          packages/xpce/tests/test_process_exit.pl
*/

:- use_module(library(pce)).
:- use_module(library(plunit)).

test_process_exit :-
    run_tests([ process_exit ]).

:- dynamic
    terminated/2,
    started/1.

free_processes :-
    forall(retract(started(P)), free(P)),
    retractall(terminated(_,_)).

on_terminated(Id, Code) :-
    assertz(terminated(Id, Code)).

%   start_process(+Id, +Command, -Process)
%
%   The process is locked, such that it is not reclaimed before it
%   terminated.  See free_processes/0.

start_process(Id, Command, P) :-
    new(P, process('/bin/sh', '-c', Command)),
    send(P, lock_object, @on),
    assertz(started(P)),
    send(P, use_tty, @off),
    send(P, terminate_message, message(@prolog, on_terminated, Id, @arg1)),
    send(P, open).

%   await_terminated(+Count)
%
%   Dispatch events until Count processes have reported or 3 seconds
%   have passed, which is less than the `sleep` holding their output.

await_terminated(Count) :-
    get_time(T0),
    Deadline is T0+3,
    repeat,
      (   aggregate_all(count, terminated(_,_), Count)
      ->  !
      ;   get_time(Now),
          Now > Deadline
      ->  !
      ;   send(@display, dispatch),
          fail
      ).

:- dynamic
    event/1.

on_event(line, Str) :-
    !,
    get(Str, value, Line),
    string_concat(Line, "", S),
    assertz(event(line(S))).
on_event(Type, Arg) :-
    E =.. [Type, Arg],
    assertz(event(E)).

%   start_line_process(+Command)
%
%   Start Command, recording each line of output and its termination
%   as event/1.

start_line_process(Command) :-
    retractall(event(_)),
    new(P, process('/bin/sh', '-c', Command)),
    send(P, lock_object, @on),
    assertz(started(P)),
    send(P, use_tty, @off),
    send(P, record_separator, '\n'),
    send(P, input_message, message(@prolog, on_event, line, @arg1)),
    send(P, terminate_message, message(@prolog, on_event, done, @arg1)),
    send(P, open).

events(Events) :-
    findall(E, event(E), Events).

await_event(E) :-
    get_time(T0),
    Deadline is T0+3,
    repeat,
      (   event(E)
      ->  !
      ;   get_time(Now),
          Now > Deadline
      ->  !
      ;   ignore(send(@display, dispatch)),
          fail
      ).

dispatch_for(Time) :-
    get_time(T0),
    Deadline is T0+Time,
    repeat,
      ignore(send(@display, dispatch)),
      get_time(Now),
      Now > Deadline,
    !.

:- begin_tests(process_exit, [cleanup(free_processes)]).

test(exited, [cleanup(free_processes), Reported == Expected]) :-
    numlist(1, 20, Ids),
    forall(member(Id, Ids),
           start_process(Id, 'sleep 5 & exit 0', _)),
    await_terminated(20),
    findall(Id-Code, terminated(Id, Code), Pairs),
    msort(Pairs, Reported),
    findall(Id-0, member(Id, Ids), Expected).
test(killed, [cleanup(free_processes), Code == term]) :-
    start_process(sleeper, 'sleep 5 & exec sleep 30', P),
    dispatch_for(0.5),                  % let sh start the background sleep
    send(P, kill, term),
    await_terminated(1),
    terminated(sleeper, Code).

test(killed_eof, [cleanup(free_processes), Code == term]) :-
    start_process(sleeper, 'exec sleep 30', P),
    dispatch_for(0.5),
    send(P, kill, term),
    await_terminated(1),
    terminated(sleeper, Code).
test(output_first, [cleanup(free_processes), Last-Lines == done(0)-50000]) :-
    start_line_process('seq 1 50000'),
    await_event(done(_)),
    events(Events),
    last(Events, Last),
    aggregate_all(count, member(line(_), Events), Lines).

test(wait, [cleanup(free_processes), Status-Code == killed-term]) :-
    start_process(self_kill, 'exec >&- 2>&-; sleep 0.3; kill -TERM $$', P),
    send(P, wait),
    get(P, status, Status),
    get(P, code, Code).

:- end_tests(process_exit).
