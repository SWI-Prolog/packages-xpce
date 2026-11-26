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

:- module(test_tracepce, [test_tracepce/0]).

/** <module> Tests for tracing methods defined in Prolog

tracepce/1 on a method defined in Prolog prints its ports.  The method
must still succeed or fail as it does without tracing.

Run with:

    swipl -g test_tracepce -t halt packages/xpce/tests/test_tracepce.pl
*/

:- use_module(library(pce)).
:- use_module(library(plunit)).

test_tracepce :-
    run_tests([ tracepce ]).

:- pce_begin_class(tracepce_test, object).

yes(_) :-> true.
no(_) :-> fail.
answer(_, X:int) :<- X = 42.
none(_, _X:int) :<- fail.

:- pce_end_class(tracepce_test).

traced_method((tracepce_test->yes)).
traced_method((tracepce_test->no)).
traced_method('<-'(tracepce_test, answer)).
traced_method('<-'(tracepce_test, none)).

%   The ports are printed as informational messages.  Silence them.

traced(Goal) :-
    setup_call_cleanup(
        ( current_prolog_flag(verbose, Old),
          set_prolog_flag(verbose, silent),
          forall(traced_method(M), tracepce(M))
        ),
        ( new(O, tracepce_test),
          call(Goal, O)
        ),
        ( forall(traced_method(M), notracepce(M)),
          set_prolog_flag(verbose, Old)
        )).

send_yes(O) :- send(O, yes).
send_no(O)  :- send(O, no).
get_answer(X, O) :- get(O, answer, X).
get_none(O) :- get(O, none, _).

:- begin_tests(tracepce).

test(send_succeeds) :-
    traced(send_yes).
test(send_fails, fail) :-
    traced(send_no).
test(get_succeeds, X == 42) :-
    traced(get_answer(X)).
test(get_fails, fail) :-
    traced(get_none).

:- end_tests(tracepce).
