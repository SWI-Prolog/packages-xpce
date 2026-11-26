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

:- module(test_epilog_capture, [test_epilog_capture/0]).

/** <module> Test capturing the output of the main thread in Epilog

The main thread of swipl-win runs XPCE and writes its messages, e.g., of
tracepce/1, to the main Epilog console.  It can only do so after the
thread of that console registered its streams.  If that happens later,
the capture must be completed then.  Waiting for it a limited time lost
the messages if that thread started slowly, as on Windows.

The terminal is faked: only its registration matters.

Run with:

    swipl -g test_epilog_capture -t halt \
          packages/xpce/tests/test_epilog_capture.pl
*/

:- set_prolog_flag('SDL_VIDEODRIVER', dummy).

:- use_module(library(pce)).
:- use_module(library(plunit)).
:- use_module(library(epilog)).

test_epilog_capture :-
    run_tests([ epilog_capture ]).

%   with_fake_terminal(-Out, -Err, :Goal)
%
%   Run Goal in the main thread, restoring the capture state and the
%   standard streams afterwards.

:- meta_predicate with_fake_terminal(-, -, 0).

with_fake_terminal(Out, Err, Goal) :-
    stream_property(Stdout, alias(user_output)),
    stream_property(Stderr, alias(user_error)),
    setup_call_cleanup(
        ( new_memory_file(MFOut),
          new_memory_file(MFErr),
          open_memory_file(MFOut, write, Out),
          open_memory_file(MFErr, write, Err)
        ),
        Goal,
        ( epilog:uncapture_messages(fake_terminal),
          retractall(epilog:terminal_input(fake_terminal, _, _, _, _, _)),
          set_stream(Stdout, alias(user_output)),
          set_stream(Stderr, alias(user_error)),
          close(Out),
          close(Err),
          free_memory_file(MFOut),
          free_memory_file(MFErr)
        )).

%   register(+Out, +Err)
%
%   Register the streams of the fake terminal as its thread does.

register(Out, Err) :-
    with_mutex(epilog_capture,
               asserta(epilog:terminal_input(fake_terminal, @nil, user_input,
                                             Out, Err, false))),
    epilog:complete_capture(fake_terminal).

dispatch_for(Time) :-
    get_time(T0),
    repeat,
      ignore(send(@display, dispatch)),
      get_time(Now),
      Now-T0 > Time,
    !.

user_streams(Out, Err) :-
    stream_property(Out, alias(user_output)),
    stream_property(Err, alias(user_error)).

:- begin_tests(epilog_capture, [condition(thread_self(main))]).

test(ready, [UOut-UErr == Out-Err]) :-
    with_fake_terminal(Out, Err,
                       ( register(Out, Err),
                         epilog:capture_messages(fake_terminal),
                         user_streams(UOut, UErr)
                       )).
test(late, [ Pending-UOut-UErr == true-Out-Err ]) :-
    with_fake_terminal(Out, Err,
                       ( epilog:capture_messages(fake_terminal),
                         (   epilog:capture_pending(fake_terminal)
                         ->  Pending = true
                         ;   Pending = false
                         ),
                         register(Out, Err),
                         dispatch_for(0.5),
                         user_streams(UOut, UErr)
                       )).

:- end_tests(epilog_capture).
