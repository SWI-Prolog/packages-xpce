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


:- module(test_slice,
          [ test_slice/0
          ]).
:- use_module(library(pce)).
:- use_module(library(plunit)).

/** <module> Test chain<-slice and vector<-slice
*/

:- pce_begin_class(slice_test_chain, chain).
:- pce_end_class(slice_test_chain).

:- pce_begin_class(slice_test_vector, vector).
:- pce_end_class(slice_test_vector).

test_slice :-
    run_tests([ chain_slice,
                vector_slice
              ]).

chain_slice(Chain, Start, Length, List) :-
    get(Chain, slice, Start, Length, Slice),
    chain_list(Slice, List).

vector_slice(Vector, Start, Length, List) :-
    get(Vector, slice, Start, Length, Slice),
    vector_list(Slice, List).

vector_list(Vector, List) :-
    get(Vector, low_index, Low),
    get(Vector, high_index, High),
    findall(E, ( between(Low, High, I),
                 get(Vector, element, I, E)
               ), List).

:- begin_tests(chain_slice).

test(middle, List == [b,c,d]) :-
    chain_slice(chain(a,b,c,d,e), 2, 3, List).
test(rest, List == [b,c]) :-
    chain_slice(chain(a,b,c), 2, @default, List).
test(past_end, List == [c]) :-
    chain_slice(chain(a,b,c), 3, 10, List).
test(before_start, List == [a]) :-
    chain_slice(chain(a,b,c), 0, 2, List).
test(outside, List == []) :-
    chain_slice(chain(a,b,c), 5, 2, List).
test(empty, List == []) :-
    chain_slice(chain(a,b,c), 2, 0, List).
test(new_chain, Size == 3) :-
    new(C, chain(a,b,c)),
    get(C, slice, 1, @default, S),
    S \== C,
    send(S, clear),
    get(C, size, Size).
test(subclass, Class == slice_test_chain) :-
    %  The result is of the class of the receiver
    new(C, slice_test_chain(a,b)),
    get(C, slice, 1, 1, S),
    get(S, class_name, Class).

:- end_tests(chain_slice).

:- begin_tests(vector_slice).

test(middle, List == [b,c,d]) :-
    vector_slice(vector(a,b,c,d,e), 2, 3, List).
test(rest, List == [b,c]) :-
    vector_slice(vector(a,b,c), 2, @default, List).
test(past_end, List == [c]) :-
    vector_slice(vector(a,b,c), 3, 10, List).
test(before_start, List == [a]) :-
    vector_slice(vector(a,b,c), 0, 2, List).
test(outside, List == []) :-
    vector_slice(vector(a,b,c), 7, 1, List).
test(empty, List == []) :-
    vector_slice(vector(a,b,c), 2, 0, List).
test(offset, [List-Low == [z,a]-1]) :-
    %  The start is an index of the vector; the result starts at 1
    new(V, vector(a,b,c)),
    send(V, element, 0, z),
    get(V, slice, 0, 2, S),
    vector_list(S, List),
    get(S, low_index, Low).
test(subclass, Class == slice_test_vector) :-
    new(V, slice_test_vector(a,b)),
    get(V, slice, 1, 1, S),
    get(S, class_name, Class).

:- end_tests(vector_slice).
