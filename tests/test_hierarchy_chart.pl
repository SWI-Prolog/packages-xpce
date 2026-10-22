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

:- module(test_hierarchy_chart, [test_hierarchy_chart/0]).

/** <module> Tests for library(plot/hierarchy_chart)

Run with:

    swipl -g test_hierarchy_chart -t halt \
          packages/xpce/tests/test_hierarchy_chart.pl
*/

:- set_prolog_flag('SDL_VIDEODRIVER', dummy).

:- use_module(library(pce)).
:- use_module(library(plunit)).
:- use_module(library(plot/hierarchy_chart)).

test_hierarchy_chart :-
    run_tests([ hierarchy_node,
                treemap_layout,
                hierarchy_chart
              ]).

%   all (24) has a (10) and b (14), which has x (5) and y (9).

tree(R) :-
    hierarchy_from_term(node(all, 0, [ a-10,
                                       node(b, 0, [x-5, y-9])
                                     ]), R).

child(Node, Label, Child) :-
    get(Node?children, find, @arg1?label == Label, Child).

label(Node, Label) :-
    get(Node?label, value, Label).

:- begin_tests(hierarchy_node).

test(total, T == 24) :-
    tree(R),
    get(R, total, T).
test(total_follows_value, [T0-T1 == 24-30]) :-
    tree(R),
    get(R, total, T0),
    child(R, b, B),
    child(B, x, X),
    send(X, value, 11),
    get(R, total, T1).
test(total_follows_append, T == 25) :-
    tree(R),
    get(R, total, _),
    send(R, append, hierarchy_node(c, 1)),
    get(R, total, T).
test(path_label, L == "all / b / y") :-
    tree(R),
    child(R, b, B),
    child(B, y, Y),
    get(Y, path_label, S),
    get(S, value, A),
    atom_string(A, L).
test(depth, D == 2) :-
    tree(R),
    child(R, b, B),
    child(B, y, Y),
    get(Y, depth, D).
test(options, [Data-Colour == 42-red]) :-
    hierarchy_from_term(node(n, 1, [], [data(42), colour(red)]), N),
    get(N, data, Data),
    get(N?colour, name, Colour).
test(bad_term, error(type_error(hierarchy_node_term, foo))) :-
    hierarchy_from_term(foo, _).
test(free_children, [Alive == false]) :-
    tree(R),
    child(R, a, A),
    free(R),
    (object(A) -> Alive = true ; Alive = false).

:- end_tests(hierarchy_node).

:- begin_tests(treemap_layout).

%   squarify/3 covers the rectangle exactly and keeps the areas.

test(squarify_areas, [Sum =:= 600*400, true(Errors == [])]) :-
    Areas = [120000-a, 60000-b, 30000-c, 20000-d, 10000-e],
    hierarchy_chart:squarify(Areas, r(0, 0, 600, 400), Placed),
    findall(E, ( member(Item-r(_,_,W,H), Placed),
                 memberchk(A-Item, Areas),
                 abs(W*H-A) > 1.0e-6,
                 E = Item
               ), Errors),
    findall(W*H, member(_-r(_,_,W,H), Placed), WHs),
    sum_list(WHs, Sum0),
    Sum is round(Sum0).
test(squarify_inside, [true(Outside == [])]) :-
    Areas = [5000-a, 4000-b, 3000-c, 2000-d, 1000-e, 1000-f],
    hierarchy_chart:squarify(Areas, r(10, 20, 160, 100), Placed),
    findall(I, ( member(I-r(X,Y,W,H), Placed),
                 \+ ( X >= 10-1.0e-6, Y >= 20-1.0e-6,
                      X+W =< 170+1.0e-6, Y+H =< 120+1.0e-6 )
               ), Outside).
test(squarify_aspect, [true(Max < 3)]) :-
    findall(100-I, between(1, 16, I), Areas),
    hierarchy_chart:squarify(Areas, r(0, 0, 160, 10), Placed),
    findall(R, ( member(_-r(_,_,W,H), Placed),
                 R is max(W/H, H/W)
               ), Rs),
    max_list(Rs, Max).

:- end_tests(treemap_layout).

:- begin_tests(hierarchy_chart).

segments(C, Labels) :-
    get(C?graphicals, find_all,
        message(@arg1, instance_of, hierarchy_segment), Ch),
    chain_list(Ch, Segs),
    maplist([S,L]>>label(S?node, L), Segs, Labels0),
    msort(Labels0, Labels).

%   The node of the topmost segment at (X,Y).

node_at(C, X, Y, Label) :-
    send(C, compute),                   % not in a window: compute areas
    get(C, pointed_objects, point(X, Y), Ch),
    get(Ch, find, message(@arg1, instance_of, hierarchy_segment), S),
    label(S?node, Label).

test(rings_segments, Labels == [a, all, b, x, y]) :-
    tree(R),
    new(C, rings_chart(R, size(400, 400))),
    segments(C, Labels).

%   b (14 of 24) comes first, clockwise from 12 o'clock, then a.  The
%   rings are 400/2/3 pixels wide.

test(rings_hit, [Labels == [all, b, a, y, x]]) :-
    tree(R),
    new(C, rings_chart(R, size(404, 404))),
    RW is 200/3,
    MidA is 2*pi*(14+5)/24,
    MidY is 2*pi*(9/2)/24,              % y (9) before x (5)
    MidX is 2*pi*(9+5/2)/24,
    Points = [ 0-0, MidY-(1.5*RW), MidA-(1.5*RW), MidY-(2.5*RW),
               MidX-(2.5*RW) ],
    findall(L, ( member(A-Rad, Points),
                 X is round(202 + Rad*sin(A)),
                 Y is round(202 - Rad*cos(A)),
                 node_at(C, X, Y, L)
               ), Labels).
%   Close to the edge of b, a is drawn later and within the event
%   tolerance, but segments accept events only where they are painted.

test(rings_hit_edge, [Label == b]) :-
    tree(R),
    new(C, rings_chart(R, size(404, 404))),
    Rad is 1.5*200/3,
    A is 2*pi*14/24 - 2/Rad,            % 2 pixels before a starts
    X is round(202 + Rad*sin(A)),
    Y is round(202 - Rad*cos(A)),
    node_at(C, X, Y, Label).
test(rings_depth, Labels == [a, all, b]) :-
    tree(R),
    new(C, rings_chart(R, size(400, 400))),
    send(C, max_depth, 1),
    segments(C, Labels).
test(treemap_segments, Labels == [a, all, b, x, y]) :-
    tree(R),
    new(C, treemap_chart(R, size(400, 300))),
    segments(C, Labels).

%   The tile of a child is on top of that of its parent.

test(treemap_hit, [Label == x]) :-
    tree(R),
    new(C, treemap_chart(R, size(400, 300))),
    get(C?graphicals, find,
        and(message(@arg1, instance_of, hierarchy_segment),
            @arg1?node?label == x), S),
    get(S?area, center, point(X, Y)),
    node_at(C, X, Y, Label).
test(zoom, [Labels == [b, all, all]]) :-
    tree(R),
    new(C, treemap_chart(R)),
    child(R, b, B),
    send(C, clicked, B),
    label(C?focus, L1),
    send(C, clicked, B),                % the focus: zoom out
    label(C?focus, L2),
    send(C, clicked, R),                % the root: nothing
    label(C?focus, L3),
    Labels = [L1, L2, L3].
test(select_leaf, [Selected-Focus == [a]-all]) :-
    tree(R),
    new(C, rings_chart(R)),
    new(Ch, chain),
    send(C, select_message, message(Ch, append, @arg1?label)),
    child(R, a, A),
    send(C, clicked, A),
    chain_list(Ch, Selected),
    label(C?focus, Focus).
test(switch_view, [Focus-Class == b-treemap_chart]) :-
    tree(R),
    new(W, hierarchy_window(R, rings)),
    child(R, b, B),
    send(W, focus, B),
    send(W, view, treemap),
    get(W, chart, C),
    get(C, class_name, Class),
    label(C?focus, Focus),
    send(W, destroy).
test(describe, S == "y\n9 bytes (64.3% of b)") :-
    tree(R),
    new(C, rings_chart(R)),
    send(C, unit, bytes),
    child(R, b, B),
    child(B, y, Y),
    send(C, focus, B),
    get(C, describe, Y, Str),
    get(Str, value, A),
    atom_string(A, S).
test(format_value, Texts == ['1.5 kB', '2.50 s', '12.0 ms', '1,234,567']) :-
    maplist(format_value,
            [bytes-1500, seconds-2.5, seconds-0.012, count-1234567],
            Texts).

format_value(Unit-Value, Text) :-
    hierarchy_chart:format_value(Unit, Value, Text).

:- end_tests(hierarchy_chart).
