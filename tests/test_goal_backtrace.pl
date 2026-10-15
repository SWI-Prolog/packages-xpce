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

:- module(test_goal_backtrace, [test_goal_backtrace/0]).

/** <module> The goal stack of an xpce warning is written in one place

A warning lists the goals that led to it.  For a method defined in
Prolog the arguments are written by Prolog, and they must go where xpce
writes the rest of the line rather than to the current output, which in
swipl-win is not the console: the arguments were missing from the
warning and appeared on the terminal when the program halted.

Run with:

    swipl -g test_goal_backtrace -t halt \
          packages/xpce/tests/test_goal_backtrace.pl
*/

:- set_prolog_flag('SDL_VIDEODRIVER', dummy).

:- use_module(library(pce)).
:- use_module(library(plunit)).

test_goal_backtrace :-
    run_tests([ goal_backtrace ]).

:- pce_begin_class(tgb_object, object).

poke(_, X:name) :->
    send(X, tgb_no_such_method).

:- pce_end_class(tgb_object).

%   warn(-Console, -Current)
%
%   Raise a warning from a method defined in Prolog while user_output,
%   where xpce writes, and current output are different streams.

warn(Console, Current) :-
    new(O, tgb_object),
    stream_property(UserOut, alias(user_output)),
    tmp_file_stream(text, File, Out),
    setup_call_cleanup(
        set_stream(Out, alias(user_output)),
        with_output_to(string(Current),
                       ignore(send(O, poke, tgb_argument))),
        ( set_stream(UserOut, alias(user_output)),
          close(Out)
        )),
    read_file_to_string(File, Console, []),
    delete_file(File).

:- begin_tests(goal_backtrace).

test(host_arguments_on_console,
     [ true(sub_string(Console, _, _, _, "->poke(tgb_argument)")),
       Current == ""
     ]) :-
    warn(Console, Current).

:- end_tests(goal_backtrace).
