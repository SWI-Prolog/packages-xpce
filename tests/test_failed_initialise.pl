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

:- module(test_failed_initialise, [test_failed_initialise/0]).

/** <module> Free objects whose ->initialise failed

If ->initialise fails, for example because an argument has the wrong
type, the object is freed using ->unlink.  Its slots are not filled
then, which used to crash some ->unlink methods.

Run with:

    swipl -g test_failed_initialise -t halt packages/xpce/tests/test_failed_initialise.pl
*/

:- use_module(library(pce)).
:- use_module(library(plunit)).
:- use_module(library(pane_frame)).

test_failed_initialise :-
    run_tests([failed_initialise]).

%   fails_to_create(+Term)
%
%   new/2 of Term fails without crashing.

fails_to_create(Term) :-
    \+ pce_catch_error(_, new(_, Term)).

:- begin_tests(failed_initialise).

test(frame) :-
    fails_to_create(frame(x, y, z, w)).
test(prolog_subclass_of_frame) :-
    fails_to_create(pane_frame(x)).
test(display) :-
    fails_to_create(display(@display_manager)).
test(text_buffer) :-
    fails_to_create(text_buffer(@display_manager)).
test(prolog_term) :-
    fails_to_create(prolog_term(@display_manager)).
test(class, Super == object) :-
    fails_to_create(class(test_failed_class, @display_manager)),
    new(C, class(test_failed_class)),
    get(C?super_class, name, Super).

:- end_tests(failed_initialise).
