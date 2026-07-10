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

:- module(test_number_conversion, [test_number_conversion/0]).
:- encoding(utf8).

/** <module> Tests for Prolog<->xpce numeric conversion

Exercise the dispatch in packages/xpce/swipl/interface.c that hands
Prolog numbers to xpce.  xpce nums pack a double, so:

  * Small integers fitting int64 pass through unchanged.
  * Floats round-trip through cToPceReal (a 1-bit LSB loss can happen
    because toNum() uses bit0 of the mantissa as its tag).
  * Rationals and bignums that overflow int64 are float-promoted by
    PL_get_term_value() and then packed into a num.
  * Bignums that overflow double raise evaluation_error(float_overflow)
    from promoteToFloatNumber().

Run with:

    swipl -g test_number_conversion -t halt \
          packages/xpce/tests/test_number_conversion.pl
*/

:- use_module(library(pce)).
:- use_module(library(plunit)).

test_number_conversion :-
    run_tests([ roundtrip_int,
                roundtrip_float,
                roundtrip_rational,
                roundtrip_bignum,
                real_slot,
                overflow
              ]).

%!  num_close(+A, +B) is semidet.
%
%   True when A and B agree to within the 1-bit LSB precision loss that
%   xpce's num tag can introduce.  Both must already be numbers.

num_close(A, B) :-
    D is abs(A - B),
    (   D =:= 0
    ->  true
    ;   D < abs(A) * 1.0e-15
    ).

%!  chain_roundtrip(+V, -Back) is det.
%
%   Send V through xpce as the sole element of a chain and read it
%   back.  Exercises get_object_arg (Prolog->xpce) and unifyObject
%   (xpce->Prolog).

chain_roundtrip(V, Back) :-
    new(C, chain(V)),
    get(C, head, Back),
    free(C).


:- begin_tests(roundtrip_int).

test(zero) :-
    chain_roundtrip(0, X),
    X == 0.

test(small_positive) :-
    chain_roundtrip(42, X),
    X == 42.

test(small_negative) :-
    chain_roundtrip(-7, X),
    X == -7.

test(fits_int64) :-
    V = 12345678901,
    chain_roundtrip(V, X),
    X == V.

:- end_tests(roundtrip_int).


:- begin_tests(roundtrip_float).

test(fractional_preserved) :-
    chain_roundtrip(3.3, X),
    num_close(3.3, X).

test(whole_float_becomes_int) :-
    % 3.0 packs into a num whose double is exactly 3; xpce reports it
    % back as the integer 3.
    chain_roundtrip(3.0, X),
    X =:= 3.

test(negative_fractional) :-
    chain_roundtrip(-1.25, X),
    X =:= -1.25.

test(small_magnitude) :-
    chain_roundtrip(1.0e-9, X),
    num_close(1.0e-9, X).

:- end_tests(roundtrip_float).


:- begin_tests(roundtrip_rational).

test(third) :-
    chain_roundtrip(1r3, X),
    num_close(float(1r3), X).

test(ten_thirds) :-
    chain_roundtrip(10r3, X),
    num_close(float(10r3), X).

test(five_halves) :-
    chain_roundtrip(5r2, X),
    X =:= 2.5.

test(negative) :-
    chain_roundtrip(-7r4, X),
    X =:= -1.75.

:- end_tests(roundtrip_rational).


:- begin_tests(roundtrip_bignum).

test(power_of_two_fits) :-
    V is 1 << 40,                       % fits int64
    chain_roundtrip(V, X),
    X == V.

test(power_of_two_100) :-
    V is 1 << 100,                      % overflows int64, fits double
    chain_roundtrip(V, X),
    num_close(float(V), X).

test(large_negative_bignum) :-
    V is -(1 << 120),
    chain_roundtrip(V, X),
    num_close(float(V), X).

:- end_tests(roundtrip_bignum).


:- begin_tests(real_slot).

%   class real stores a double in its value slot.  Setting the slot to
%   a rational or oversized bignum should go through the same
%   Prolog->xpce promotion path as chain(V).

test(rational_into_real) :-
    new(R, real(10r3)),
    get(R, value, V),
    num_close(float(10r3), V).

test(bignum_into_real) :-
    Big is 1 << 100,
    new(R, real(Big)),
    get(R, value, V),
    num_close(float(Big), V).

test(update_slot_with_rational) :-
    new(R, real(0.0)),
    send(R, value, 22r7),
    get(R, value, V),
    num_close(float(22r7), V).

:- end_tests(real_slot).


:- begin_tests(overflow).

%   A bignum whose magnitude exceeds double's range cannot be packed
%   into a num.  PL_get_term_value() returns 0 with
%   evaluation_error(float_overflow) raised by promoteToFloatNumber();
%   the interface propagates the exception.

test(bignum_overflows_double,
     [ throws(error(evaluation_error(float_overflow), _))
     ]) :-
    Huge is 1 << 10000,
    new(_, chain(Huge)).

test(negative_bignum_overflow,
     [ throws(error(evaluation_error(float_overflow), _))
     ]) :-
    Huge is -(1 << 10000),
    new(_, chain(Huge)).

:- end_tests(overflow).
