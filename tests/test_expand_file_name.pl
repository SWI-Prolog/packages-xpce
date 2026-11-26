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

:- module(test_expand_file_name, [test_expand_file_name/0]).

/** <module> Tests for expanding ~ and $VAR in file names

A file name that cannot be expanded used to fail silently.  It now
raises the error `cannot_expand` with the reason.

Run with:

    swipl -g test_expand_file_name -t halt \
          packages/xpce/tests/test_expand_file_name.pl
*/

:- use_module(library(pce)).
:- use_module(library(plunit)).

test_expand_file_name :-
    run_tests([ expand_file_name ]).

%   expand_error(+Name, -Error) is det.
%
%   Error is the id of the error raised by creating a file named Name,
%   or @nil if there is none.

expand_error(Name, Error) :-
    send(@pce, last_error, @nil),
    (   pce_catch_error(cannot_expand, new(F, file(Name)))
    ->  free(F)
    ;   true
    ),
    get(@pce, last_error, Error).

:- begin_tests(expand_file_name).

test(unknown_variable, [Error == cannot_expand]) :-
    expand_error('$XPCE_TEST_NO_SUCH_VARIABLE/x', Error).
test(unknown_user, [Error == cannot_expand]) :-
    expand_error('~xpce_test_no_such_user/x', Error).
test(plain, [Error == @nil]) :-
    expand_error('plain_file_name', Error).

:- end_tests(expand_file_name).
