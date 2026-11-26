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

:- module(test_process_io, [test_process_io/0]).

/** <module> Portable tests for class process

The child is this Prolog system itself, so these tests run on every
platform, Windows included, where the output of a process is read and
its termination is seen in quite a different way than on Unix.

Run with:

    swipl -g test_process_io -t halt \
          packages/xpce/tests/test_process_io.pl
*/

:- use_module(library(pce)).
:- use_module(library(plunit)).
:- use_module(library(lists), [last/2]).

test_process_io :-
    run_tests([ process_io ]).

:- dynamic
    event/1.

on_line(Str) :-
    get(Str, value, Line),
    split_string(Line, "", "\r\n", [S]),
    assertz(event(line(S))).

on_done(P, Code) :-
    get(P, status, Status),
    assertz(event(done(Status, Code))).

%!  run(+Args, -Lines, -Status, -Code) is det.
%!  run(+Args, +Kill, -Lines, -Status, -Code) is det.
%
%   Run Prolog with Args, collecting its lines of output.  Status and
%   Code are the arguments of the termination report.  If Kill is
%   `true`, kill the process after half a second.

run(Args, Lines, Status, Code) :-
    run(Args, false, Lines, Status, Code).

run(Args, Kill, Lines, Status, Code) :-
    retractall(event(_)),
    current_prolog_flag(executable, Exe),
    Term =.. [process, Exe, '--no-pce', '-f', none, '-q'|Args],
    setup_call_cleanup(
        new(P, Term),
        run_process(P, Kill),
        free(P)),
    findall(L, event(line(L)), Lines),
    event(done(Status, Code)).

run_process(P, Kill) :-
    send(P, use_tty, @off),
    send(P, record_separator, '\n'),
    send(P, input_message, message(@prolog, on_line, @arg1)),
    send(P, terminate_message, message(@prolog, on_done, P, @arg1)),
    send(P, open),
    (   Kill == true
    ->  dispatch_for(0.5),
        send(P, kill)
    ;   true
    ),
    await_done(20).

%   Dispatch until the termination is reported or Max seconds passed.

await_done(Max) :-
    get_time(T0),
    repeat,
      (   event(done(_,_))
      ->  !
      ;   get_time(Now),
          Now-T0 > Max
      ->  !
      ;   ignore(send(@display, dispatch)),
          fail
      ).

dispatch_for(Time) :-
    get_time(T0),
    repeat,
      ignore(send(@display, dispatch)),
      get_time(Now),
      Now-T0 > Time,
    !.

%   A process that records its errors as event(error(Id, Context)).

:- pce_begin_class(test_error_process, process).

error(_P, Error:error, Context:any ...) :->
    get(Error, id, Id),
    assertz(event(error(Id, Context))).

:- pce_end_class(test_error_process).

%!  not_found(-Id, -Context) is det.
%
%   Start a command that does not exist.  Windows sees that when
%   starting it, Unix when the child cannot run it.

not_found(Id, Context) :-
    retractall(event(_)),
    setup_call_cleanup(
        new(P, test_error_process(no_such_command_for_xpce)),
        ( send(P, use_tty, @off),
          ignore(send(P, open)),
          await_event(error(_,_), 20)
        ),
        free(P)),
    event(error(Id, Context)).

await_event(E, Max) :-
    get_time(T0),
    repeat,
      (   event(E)
      ->  !
      ;   get_time(Now),
          Now-T0 > Max
      ->  !
      ;   ignore(send(@display, dispatch)),
          fail
      ).

:- begin_tests(process_io).

test(output, [Lines-Status-Code == ["hello world"]-exited-0]) :-
    run(['-g', 'writeln("hello world"),halt'], Lines, Status, Code).
test(exit_status, [Status-Code == exited-3]) :-
    run(['-g', 'halt(3)'], _, Status, Code).
test(many_lines, [Count-Last == 20000-"20000"]) :-
    run(['-g', 'forall(between(1,20000,I),writeln(I)),halt'],
        Lines, exited, 0),
    length(Lines, Count),
    last(Lines, Last).
test(output_before_exit, [Lines-Status == ["done"]-exited]) :-
    run(['-g', 'sleep(0.5),writeln(done),halt'], Lines, Status, _).
test(killed, [Status-Code == killed-term]) :-
    run(['-g', 'sleep(30)'], true, _, Status, Code).
test(arguments, [Lines == ["['x\"y','c:\\\\dir\\\\','','a\\\\\"b','a b']"]]) :-
    run(['-g', 'current_prolog_flag(argv,A),print(A),nl,halt', '--',
         'x"y', 'c:\\dir\\', '', 'a\\"b', 'a b'],
        Lines, exited, 0).

test(not_found, [Id == exec_error]) :-
    not_found(Id, _).
:- if(current_prolog_flag(windows, true)).
test(not_found_message, [Context == ['command not found']]) :-
    not_found(_, Context).
:- endif.

:- end_tests(process_io).
