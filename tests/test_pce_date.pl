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

:- module(test_pce_date, [test_pce_date/0]).

/** <module> Tests for xpce class date

Dates are seconds since the epoch.  On Windows, C long is 32 bits, so
these tests use intervals that do not fit 32 bits.  Windows' C library
cannot represent dates before 1970 and has no tm_gmtoff.
*/

:- use_module(library(pce)).
:- use_module(library(plunit)).
:- use_module(library(apply)).
:- use_module(library(lists)).

test_pce_date :-
    run_tests([ pce_date ]).

:- begin_tests(pce_date).

test(advance_large, Y == 2273) :-
    new(D, date(0,0,0,1,1,2000)),
    send(D, advance, 100_000, day),
    get(D, year, Y).
test(advance_overflow, error(pce(int_range, _), _)) :-
    new(D, date),
    send(D, advance, 2_000_000_000_000_000, week).
test(difference_large, S == 8_640_000_000) :-
    new(D1, date(0,0,0,1,1,2000)),
    new(D2, date(0,0,0,1,1,2000)),
    send(D2, advance, 100_000, day),
    get(D2, difference, D1, second, S).
test(convert, Y == 2026) :-
    new(D, date),
    send(D, convert, '21 Oct 2026'),
    get(D, year, Y).
test(time_zone_abbreviation, true(Len > 0)) :-
    new(D, date),
    get(D, time_zone, @on, Z),
    atom_length(Z, Len).
test(time_zone_offset, true) :-
    new(D, date),
    get(D, time_zone, Z),
    atom_codes(Z, [S,H1,H2,M1,M2]),
    memberchk(S, `+-`),
    maplist(code_type_digit, [H1,H2,M1,M2]).
test(time_zone_negative_minutes, Z == '-0030') :-
    \+ current_prolog_flag(windows, true),
    with_tz('<-0030>0:30',
            ( new(D, date),
              get(D, time_zone, Z) )).
test(rfc_string, Parts = ['Thu,','02','Jul','2026','03:04:05',_]) :-
    new(D, date(5,4,3,2,7,2026)),
    get(D, rfc_string, S),
    get(S, value, A),
    atomic_list_concat(Parts, ' ', A).
test(string, A == 'Thu Jul  2 03:04:05 2026') :-
    new(D, date(5,4,3,2,7,2026)),
    get(D, string, S),
    get(S, value, A).
test(dst, [Jan,Jul] == ['03:04:05','03:04:05']) :-
    \+ current_prolog_flag(windows, true),
    with_tz('CET-1CEST,M3.5.0,M10.5.0/3',
            ( hms(date(5,4,3,2,1,2026), Jan),
              hms(date(5,4,3,2,7,2026), Jul) )).
test(before_1970, [Y,M,Day] == [1969,12,31]) :-
    new(D, date(0,0,12,1,1,1970)),
    send(D, advance, -1, day),
    get(D, year, Y),
    get(D, month, M),
    get(D, day, Day).
%   Where the C library cannot represent a date, the getters must fail
%   rather than crash.

test(before_1970_strings, true) :-
    new(D, date(0,0,12,1,1,1970)),
    send(D, advance, -1, day),
    ignore(get(D, string, _)),
    ignore(get(D, rfc_string, _)),
    ignore(get(D, xml_string, _)),
    ignore(get(D, week_day, _)),
    ignore(get(D, day_name, _)),
    ignore(get(D, time_zone, _)).

:- end_tests(pce_date).

hms(Term, HMS) :-
    new(D, Term),
    get(D, string, S),
    get(S, value, A),
    split_string(A, " ", " ", Parts),
    nth1(4, Parts, Str),
    atom_string(HMS, Str).

code_type_digit(C) :-
    code_type(C, digit).

with_tz(TZ, Goal) :-
    (   getenv('TZ', Old)
    ->  Restore = setenv('TZ', Old)
    ;   Restore = unsetenv('TZ')
    ),
    setup_call_cleanup(setenv('TZ', TZ), Goal, Restore).
