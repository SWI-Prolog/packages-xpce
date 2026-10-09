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


:- module(test_shadow, [test_shadow/0]).

/** <module> Test class shadow and the drop shadows of graphicals

Run with:

    swipl -g test_shadow -t halt packages/xpce/tests/test_shadow.pl
*/

:- use_module(library(pce)).
:- use_module(library(plunit)).

test_shadow :-
    run_tests([ shadow
              ]).

shadow_props(S, X-Y-B-Sp) :-
    get(S, x_offset, X),
    get(S, y_offset, Y),
    get(S, blur, B),
    get(S, spread, Sp).

:- begin_tests(shadow).

test(defaults, Props == 0-3-8-0) :-
    new(S, shadow),
    shadow_props(S, Props).
test(arguments, Props == 1-2-3-4) :-
    new(S, shadow(1, 2, 3, colour(red), 4)),
    shadow_props(S, Props).
test(translucent_by_default, true(A < 255)) :-
    new(S, shadow),
    get(S?colour, alpha, A).
test(an_int_is_a_soft_shadow, Props == 4-4-8-0) :-
    new(B, box(10, 10)),
    send(B, shadow, 4),
    get(B, shadow, S),
    shadow_props(S, Props).
test(zero_is_no_shadow, S == @nil) :-
    new(B, box(10, 10)),
    send(B, shadow, 4),
    send(B, shadow, 0),
    get(B, shadow, S).
test(the_area_keeps_its_size, Area == area(0,0,100,60)) :-
    new(B, box(100, 60)),
    send(B, shadow, new(_, shadow(5, 5, 10))),
    get(B, area, A),
    get(A, x, X), get(A, y, Y), get(A, width, W), get(A, height, H),
    Area = area(X,Y,W,H).
test(classes_with_a_shadow, true) :-
    forall(member(Obj, [box(10,10), ellipse(10,10), circle(10), figure]),
           ( new(G, Obj),
             send(G, shadow, new(_, shadow)),
             get(G, shadow, S),
             send(S, instance_of, shadow) )).

:- end_tests(shadow).
