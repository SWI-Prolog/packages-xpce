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

:- module(test_display_info, [test_display_info/0]).

/** <module> Power and locale information of the display

The values depend on the machine, so we only check their form.

Run with:

    swipl -g test_display_info -t halt \
          packages/xpce/tests/test_display_info.pl
*/

:- use_module(library(pce)).
:- use_module(library(plunit)).

setup_headless :-
    set_prolog_flag('SDL_VIDEODRIVER', dummy).
:- initialization(setup_headless, now).

test_display_info :-
    run_tests([ display_info ]).

locale_name(Name) :-
    atom_codes(Name, Codes),
    phrase(locale, Codes).

locale --> lower, lower_or_upper, ( "_" -> upper_rest ; [] ).

lower --> [C], { code_type(C, lower) }.
lower_or_upper --> [C], { code_type(C, alpha) }, !, lower_or_upper.
lower_or_upper --> [].
upper_rest --> [C], { code_type(C, alnum) }, !, upper_rest.
upper_rest --> [].

:- begin_tests(display_info).

test(power_state, true(memberchk(S, [unknown, on_battery, no_battery,
                                     charging, charged]))) :-
    get(@display, power_state, S).
test(battery_percent, true(between(0, 100, P))) :-
    (   get(@display, battery_percent, P)
    ->  true
    ;   P = 0                           % no battery
    ).
test(locales, true(maplist(locale_name, Names))) :-
    get(@display, preferred_locales, Chain),
    chain_list(Chain, Names).

:- end_tests(display_info).
