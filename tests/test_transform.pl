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

:- module(test_transform, [test_transform/0]).
:- encoding(utf8).

/** <module> Tests for xpce class transform (2D affine matrix)

Run with:

    swipl -g test_transform -t halt \
          packages/xpce/tests/test_transform.pl
*/

:- use_module(library(pce)).
:- use_module(library(plunit)).

test_transform :-
    run_tests([ transform_construct,
                transform_ops,
                transform_compose,
                transform_invert,
                transform_apply
              ]).

%!  near(+X, +Y) is semidet.
%!  near(+X, +Y, +Eps) is semidet.
%
%   Compare numeric values with tolerance.  xpce returns a Num slot as a
%   Prolog integer when it holds an integral value, so we cannot rely on
%   =/2 between e.g. =|2|= and =|2.0|=.

near(X, Y)      :- near(X, Y, 1.0e-9).
near(X, Y, Eps) :- abs(X-Y) < Eps.

%!  coeffs(+T, -List) is det.
%
%   Read the six coefficients of T into a list [XX,XY,YX,YY,TX,TY].

coeffs(T, [XX,XY,YX,YY,TX,TY]) :-
    get(T, xx, XX), get(T, xy, XY),
    get(T, yx, YX), get(T, yy, YY),
    get(T, tx, TX), get(T, ty, TY).

%!  mk_transform(+XX, +XY, +YX, +YY, +TX, +TY, -T) is det.
%
%   Shortcut to build a transform with all six coefficients set
%   directly; equivalent to new(T, transform) + ->set/6.

mk_transform(XX, XY, YX, YY, TX, TY, T) :-
    new(T, transform),
    send(T, set, XX, XY, YX, YY, TX, TY).


:- begin_tests(transform_construct).

test(default_is_identity) :-
    new(T, transform),
    coeffs(T, [1,0,0,1,0,0]),
    free(T).

test(initialise_rotate_only) :-
    new(T, transform(rotate := 90)),
    coeffs(T, [XX,XY,YX,YY,TX,TY]),
    near(XX, 0.0), near(XY,-1.0),
    near(YX, 1.0), near(YY, 0.0),
    near(TX, 0.0), near(TY, 0.0),
    free(T).

test(initialise_uniform_scale) :-
    new(T, transform(scale := 2)),
    coeffs(T, [XX,XY,YX,YY,_,_]),
    near(XX, 2.0), near(YY, 2.0),
    near(XY, 0.0), near(YX, 0.0),
    free(T).

test(initialise_anisotropic_scale) :-
    new(T, transform(scale := tuple(2, 3))),
    coeffs(T, [XX,_,_,YY,_,_]),
    near(XX, 2.0), near(YY, 3.0),
    free(T).

test(initialise_shear_only) :-
    new(T, transform(shear := tuple(0.5, 0))),
    coeffs(T, [XX,XY,_,YY,_,_]),
    near(XX, 1.0), near(YY, 1.0),
    near(XY, 0.5),
    free(T).

test(initialise_combined_scale_rotate) :-
    %  Code order is scale then rotate, i.e. M = I*S*R = S*R.  Applied
    %  to a point p that means rotate then scale.
    new(T, transform(rotate := 90, scale := 2)),
    coeffs(T, [XX,XY,YX,YY,_,_]),
    near(XX, 0.0), near(XY,-2.0),
    near(YX, 2.0), near(YY, 0.0),
    free(T).

test(set_assigns_all) :-
    new(T, transform),
    send(T, set, 1, 2, 3, 4, 5, 6),
    coeffs(T, [1,2,3,4,5,6]),
    free(T).

test(copy_send_replaces_contents) :-
    mk_transform(1, 2, 3, 4, 5, 6, A),
    new(B, transform),
    send(B, copy, A),
    coeffs(B, [1,2,3,4,5,6]),
    free(A), free(B).

test(copy_get_is_independent) :-
    mk_transform(1, 2, 3, 4, 5, 6, A),
    get(A, copy, B),
    send(A, identity),
    coeffs(B, [1,2,3,4,5,6]),
    coeffs(A, [1,0,0,1,0,0]),
    free(A), free(B).

test(identity_resets) :-
    mk_transform(7, 8, 9, 10, 11, 12, T),
    send(T, identity),
    coeffs(T, [1,0,0,1,0,0]),
    free(T).

:- end_tests(transform_construct).


:- begin_tests(transform_ops).

test(translate_only_changes_translation) :-
    new(T, transform),
    send(T, translate, 10, 20),
    coeffs(T, [XX,XY,YX,YY,TX,TY]),
    near(XX, 1), near(XY, 0),
    near(YX, 0), near(YY, 1),
    near(TX,10), near(TY,20),
    free(T).

test(translate_accumulates) :-
    new(T, transform),
    send(T, translate,  3,  4),
    send(T, translate, 10, 20),
    get(T, tx, TX), near(TX, 13),
    get(T, ty, TY), near(TY, 24),
    free(T).

test(scale_uniform) :-
    new(T, transform),
    send(T, scale, 2.5),
    coeffs(T, [XX,_,_,YY,_,_]),
    near(XX, 2.5), near(YY, 2.5),
    free(T).

test(scale_xy) :-
    new(T, transform),
    send(T, scale, 2.0, 3.0),
    coeffs(T, [XX,_,_,YY,_,_]),
    near(XX, 2.0), near(YY, 3.0),
    free(T).

test(rotate_0_is_identity) :-
    new(T, transform),
    send(T, rotate, 0),
    coeffs(T, [XX,XY,YX,YY,_,_]),
    near(XX, 1), near(XY, 0),
    near(YX, 0), near(YY, 1),
    free(T).

test(rotate_90) :-
    new(T, transform),
    send(T, rotate, 90),
    coeffs(T, [XX,XY,YX,YY,_,_]),
    near(XX, 0), near(XY,-1),
    near(YX, 1), near(YY, 0),
    free(T).

test(rotate_360_is_identity) :-
    new(T, transform),
    send(T, rotate, 360),
    coeffs(T, [XX,XY,YX,YY,_,_]),
    near(XX, 1), near(XY, 0),
    near(YX, 0), near(YY, 1),
    free(T).

test(shear_x) :-
    new(T, transform),
    send(T, shear, 0.5, 0.0),
    get(T, apply, point(0, 10), P),
    get(P, x, X), near(X,  5),
    get(P, y, Y), near(Y, 10),
    free(T), free(P).

test(shear_y) :-
    new(T, transform),
    send(T, shear, 0.0, 0.5),
    get(T, apply, point(10, 0), P),
    get(P, x, X), near(X, 10),
    get(P, y, Y), near(Y,  5),
    free(T), free(P).

:- end_tests(transform_ops).


:- begin_tests(transform_compose).

test(compose_with_identity_is_self) :-
    mk_transform(1, 2, 3, 4, 5, 6, T),
    new(I, transform),
    send(T, compose, I),
    coeffs(T, [1,2,3,4,5,6]),
    free(T), free(I).

test(scale_then_rotate_order, [true(near(X,2)), true(near(Y,1))]) :-
    %% Cairo convention: each call composes on the input side, so the
    %% LAST call applies FIRST to a point.  With scale(2) then
    %% rotate(30), point (1,0) is rotated first to (cos30, sin30) ≈
    %% (0.866, 0.5), then scaled to ≈ (1.732, 1.0) which rounds to (2, 1).
    new(T, transform),
    send(T, scale, 2),
    send(T, rotate, 30),
    get(T, apply, point(1, 0), P),
    get(P, x, X), get(P, y, Y),
    free(T), free(P).

test(translate_then_scale_order) :-
    %% translate(10,0) then scale(2): point (3,0) is scaled first to
    %% (6,0), then translated to (16,0).
    new(T, transform),
    send(T, translate, 10, 0),
    send(T, scale, 2),
    get(T, apply, point(3, 0), P),
    get(P, x, X), near(X, 16),
    free(T), free(P).

test(compose_associative_with_apply) :-
    %% (A compose B) applied to p equals A applied to (B applied to p).
    new(A, transform),
    send(A, scale, 2),
    new(B, transform),
    send(B, translate, 5, 7),
    %% A2 := A * B
    get(A, copy, A2),
    send(A2, compose, B),
    get(A2, apply, point(1, 1), P1),
    %% Manual: first B(p) = (6,8), then A((6,8)) = (12,16)
    get(B, apply, point(1, 1), Pmid),
    get(A, apply, Pmid, P2),
    get(P1, x, X1), get(P2, x, X2), X1 == X2,
    get(P1, y, Y1), get(P2, y, Y2), Y1 == Y2,
    near(X1, 12), near(Y1, 16),
    free(A), free(B), free(A2), free(P1), free(Pmid), free(P2).

:- end_tests(transform_compose).


:- begin_tests(transform_invert).

test(invert_translate) :-
    new(T, transform),
    send(T, translate, 5, 7),
    send(T, invert),
    get(T, tx, TX), near(TX, -5),
    get(T, ty, TY), near(TY, -7),
    free(T).

test(invert_scale) :-
    new(T, transform),
    send(T, scale, 2, 4),
    send(T, invert),
    get(T, xx, XX), near(XX, 0.5),
    get(T, yy, YY), near(YY, 0.25),
    free(T).

test(inverse_get_is_independent) :-
    mk_transform(1.5, 0.5, -0.3, 2.0, 4, 5, T),
    get(T, inverse, Inv),
    %% T is unchanged
    coeffs(T, [A,B,C,D,E,F]),
    near(A, 1.5), near(B,  0.5),
    near(C,-0.3), near(D,  2.0),
    near(E, 4.0), near(F,  5.0),
    free(T), free(Inv).

test(round_trip_is_identity) :-
    mk_transform(1.5, 0.5, -0.3, 2.0, 4, 5, T),
    get(T, inverse, Inv),
    send(T, compose, Inv),
    coeffs(T, [XX,XY,YX,YY,TX,TY]),
    near(XX, 1), near(XY, 0),
    near(YX, 0), near(YY, 1),
    near(TX, 0), near(TY, 0),
    free(T), free(Inv).

test(singular_inverse_fails, [fail]) :-
    mk_transform(1, 1, 1, 1, 0, 0, T),
    get(T, inverse, _).

test(singular_invert_send_fails, [fail]) :-
    mk_transform(1, 1, 1, 1, 0, 0, T),
    send(T, invert).

test(scale_by_zero_is_singular) :-
    new(T, transform),
    send(T, scale, 0),
    get(T, determinant, D), near(D, 0),
    \+ send(T, invert),
    free(T).

test(determinant_scale_xy) :-
    mk_transform(2, 0, 0, 3, 0, 0, T),
    get(T, determinant, D), near(D, 6),
    free(T).

test(determinant_translation_only) :-
    new(T, transform),
    send(T, translate, 100, 200),
    get(T, determinant, D), near(D, 1),
    free(T).

test(determinant_rotation_only) :-
    new(T, transform),
    send(T, rotate, 47),
    get(T, determinant, D), near(D, 1),
    free(T).

:- end_tests(transform_invert).


:- begin_tests(transform_apply).

test(apply_identity_to_point) :-
    new(T, transform),
    get(T, apply, point(13, 17), P),
    get(P, x, 13), get(P, y, 17),
    free(T), free(P).

test(apply_identity_to_area) :-
    new(T, transform),
    get(T, apply, area(10, 20, 30, 40), A),
    get(A, x, 10), get(A, y, 20),
    get(A, width, 30), get(A, height, 40),
    free(T), free(A).

test(apply_translate_then_scale_to_point) :-
    new(T, transform),
    send(T, translate, 100, 200),
    send(T, scale, 2),
    get(T, apply, point(3, 4), P),
    get(P, x, 106), get(P, y, 208),
    free(T), free(P).

test(apply_rotate_90_swaps_dimensions) :-
    new(T, transform),
    send(T, rotate, 90),
    get(T, apply, area(10, 20, 30, 40), A),
    get(A, width, 40), get(A, height, 30),
    free(T), free(A).

test(apply_rotate_180_around_origin) :-
    new(T, transform),
    send(T, rotate, 180),
    get(T, apply, area(10, 20, 30, 40), A),
    get(A, x, -40), get(A, y, -60),
    get(A, width, 30), get(A, height, 40),
    free(T), free(A).

test(apply_scale_to_area) :-
    new(T, transform),
    send(T, scale, 2, 3),
    get(T, apply, area(10, 20, 5, 7), A),
    get(A, x, 20), get(A, y, 60),
    get(A, width, 10), get(A, height, 21),
    free(T), free(A).

test(apply_rejects_other_type, [fail]) :-
    new(T, transform),
    get(T, apply, string("hi"), _).

:- end_tests(transform_apply).
