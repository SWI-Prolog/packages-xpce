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

:- module(test_type_syntax, [test_type_syntax/0]).

/** <module> Types in class definitions written as Prolog terms

The class compiler translates types written as Prolog terms into the
type names of xpce.  Run with:

    swipl -g test_type_syntax -t halt \
          packages/xpce/tests/test_type_syntax.pl
*/

:- set_prolog_flag('SDL_VIDEODRIVER', dummy).

:- use_module(library(pce)).
:- use_module(library(plunit)).

test_type_syntax :-
    run_tests([ type_syntax ]).

:- pce_begin_class(tts_volume, object, "Ranges without quotes").

class_variable(volume, 0..130, 75, "Volume in decibels").

variable(level, 0..10 := 5, both, "Level").

volume(X, Vol:[0..130], V:int) :<-
    default(Vol, class_variable(X, volume), V).
scale(_, F:0.5..2.0, F:num) :<-
    true.

:- pce_end_class(tts_volume).

:- begin_tests(type_syntax).

test(range_type, Name == '[0..130]') :-
    get(class(tts_volume), get_method, volume, M),
    get(M, types, Types),
    get(Types, element, 1, Type),
    get(Type, name, Name).
test(range_default, V == 75) :-
    new(X, tts_volume),
    get(X, volume, @default, V).
test(range_value, V == 42) :-
    new(X, tts_volume),
    get(X, volume, 42, V).
test(range_out_of_range, fail) :-
    new(X, tts_volume),
    pce_catch_error(_, get(X, volume, 200, _)).
test(float_range, V == 1.5) :-
    new(X, tts_volume),
    get(X, scale, 1.5, V).
test(variable_range, V == 7) :-
    new(X, tts_volume),
    send(X, level, 7),
    get(X, level, V).

:- end_tests(type_syntax).
