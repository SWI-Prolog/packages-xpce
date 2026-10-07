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

:- module(test_class_variable, [test_class_variable/0]).

/** <module> Test class variables that are inherited

A class variable declared in a class applies to its subclasses.  A
Defaults entry or a runtime ->class_variable_value for a subclass must
change the value for that subclass and its descendants, but not for the
class that declares the variable.

Run with:

    swipl -g test_class_variable -t halt \
          packages/xpce/tests/test_class_variable.pl
*/

:- use_module(library(pce)).
:- use_module(library(plunit)).

test_class_variable :-
    run_tests([class_variable]).

:- pce_begin_class(tcv_a, object).
class_variable(x, int, 1).
variable(x, int, both).
:- pce_end_class.
:- pce_begin_class(tcv_b, tcv_a).
:- pce_end_class.
:- pce_begin_class(tcv_c, tcv_a).
:- pce_end_class.
:- pce_begin_class(tcv_d, tcv_c).
:- pce_end_class.
:- pce_begin_class(tcv_e, tcv_a).
:- pce_end_class.

cv_value(Class, Value) :-
    get(class(Class), class_variable, x, CV),
    get(CV, value, Value).

new_x(Class, Value) :-
    new(Obj, Class),
    get(Obj, x, Value),
    free(Obj).

load_defaults(Lines) :-
    tmp_file_stream(text, File, Out),
    forall(member(L, Lines), format(Out, '~w~n', [L])),
    close(Out),
    send(@pce, load_defaults, File),
    delete_file(File).

:- begin_tests(class_variable).

test(defaults_subclass, [A-B-O == 1-2-2]) :-
    load_defaults(['tcv_b.x: 2']),
    cv_value(tcv_a, A),
    cv_value(tcv_b, B),
    new_x(tcv_b, O).
test(runtime_subclass, [A-C-O == 1-3-3]) :-
    cv_value(tcv_d, _),                 % caches tcv_a's variable in tcv_d
    send(class(tcv_c), class_variable_value, x, 3),
    cv_value(tcv_a, A),
    cv_value(tcv_c, C),
    new_x(tcv_c, O).
test(runtime_after_lookup, [Before-After-O == 1-4-4]) :-
    cv_value(tcv_e, Before),            % caches tcv_a's variable
    send(class(tcv_e), class_variable_value, x, 4),
    cv_value(tcv_e, After),
    new_x(tcv_e, O).
test(runtime_inherited_by_subclass, [D-O == 3-3]) :-
    cv_value(tcv_d, D),
    new_x(tcv_d, O).


%   A colour may be written as #RRGGBB or #RRGGBBAA in Defaults syntax,
%   without the "old fashioned default syntax" warning.

test(hex_colour, [Class-Error == colour-(@nil)]) :-
    send(@pce, last_error, @nil),
    get(class(box), class_variable, colour, CV),
    get(CV, convert_string, '#ff000080', Colour),
    get(Colour, class_name, Class),
    get(@pce, last_error, Error).

:- end_tests(class_variable).
