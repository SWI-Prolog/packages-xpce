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
:- module(test_idg_viewer, [test_idg_viewer/0]).

/** <module> Tests for library(swi/pce_idg_viewer)

Run with:

    swipl -g test_idg_viewer -t halt packages/xpce/tests/test_idg_viewer.pl
*/

:- set_prolog_flag('SDL_VIDEODRIVER', dummy).

:- use_module(library(pce)).
:- use_module(library(plunit)).
:- use_module(library(swi/pce_idg_viewer)).

test_idg_viewer :-
    run_tests([ idg_graph,
                idg_viewer,
                idg_variants
              ]).

%   The data are in this module, the rules are non-modular code in
%   `user`, defined in this file.  So there are two groups.

:- dynamic([tv_edge/2], [incremental(true)]).

tv_edge(a, b).
tv_edge(b, c).

:- table user:tv_reach/2 as incremental.

user:tv_reach(X, Y) :- test_idg_viewer:tv_edge(X, Y).
user:tv_reach(X, Y) :- test_idg_viewer:tv_edge(X, Z), user:tv_reach(Z, Y).

%   Only `user`: asserted data (no file) and a table in this file.

:- dynamic([user:tv_data/1], [incremental(true)]).
:- table user:tv_count/1 as incremental.

user:tv_count(N) :- aggregate_all(count, user:tv_data(_), N).

%   tv_ge(N, X) has a variant for each N; those with N > 3 have no
%   answers.

:- table user:tv_ge/2 as incremental.

user:tv_ge(N, X) :- user:tv_data(X), X >= N.

fill_ge :-
    fill_user,
    forall(between(1, 25, N), forall(user:tv_ge(N, _), true)).

%   For the answers: a moded table and one with an undefined answer.

:- table user:tv_max(_, max).

user:tv_max(a, 1).
user:tv_max(a, 3).
user:tv_max(b, 2).

:- table user:tv_u/1, user:tv_v/1.

user:tv_u(1).
user:tv_u(2) :- tnot(user:tv_v(2)).
user:tv_v(2) :- tnot(user:tv_u(2)).

fill_user :-
    abolish_all_tables,
    retractall(user:tv_data(_)),
    forall(between(1, 3, I), assertz(user:tv_data(I))),
    forall(user:tv_count(_), true).

fill :-
    abolish_all_tables,
    forall(user:tv_reach(_, _), true),
    forall(user:tv_reach(a, _), true).

%   Change the data in this module; the tests run in that of the unit.

add_edge(X, Y) :-
    assertz(tv_edge(X, Y)).
del_edge(X, Y) :-
    retract(tv_edge(X, Y)).

graph(Expanded, Items, Edges) :-
    thread_self(Me),
    idg_graph(Me, Expanded, graph(Items, Edges)).

%   nodes(+Items, -Ids)
%
%   Ids are the ids of the nodes, also those inside clusters.

nodes(Items, Ids) :-
    findall(Id, item_node(Items, Id), Ids0),
    msort(Ids0, Ids).

item_node(Items, Id) :-
    member(Item, Items),
    (   Item = node(Id, _, _, _, _)
    ;   Item = cluster(_, _, _, _, Children),
        item_node(Children, Id)
    ).

clusters(Items, Ids) :-
    findall(Id, item_cluster(Items, Id), Ids0),
    msort(Ids0, Ids).

item_cluster(Items, Id) :-
    member(cluster(CId, _, _, _, Children), Items),
    (   Id = CId
    ;   item_cluster(Children, Id)
    ).

this_file(File) :-
    module_property(test_idg_viewer, file(File)).

:- begin_tests(idg_graph, [condition(current_prolog_flag(threads, true))]).

%   The data are in module test_idg_viewer and the rules in `user`, so
%   there are two modules, both collapsed.

test(modules, [ Ids == [module(test_idg_viewer), module(user)],
                Edges == [module(test_idg_viewer)-module(user)]
              ]) :-
    fill,
    graph([], Items, Edges0),
    nodes(Items, Ids),
    findall(F-T, member(edge(F,T,_), Edges0), Edges).
test(expand_user, [ Clusters-Ids == [module(user)] -
                                    [file(File), module(test_idg_viewer)]
                  ]) :-
    fill,
    this_file(File),
    graph([module(user)], Items, _),
    clusters(Items, Clusters),
    nodes(Items, Ids).
test(expand_file, Ids == [module(test_idg_viewer), pred(user:tv_reach/2)]) :-
    fill,
    this_file(File),
    graph([module(user), file(File)], Items, _),
    nodes(Items, Ids).
test(collapsed_inside, Ids == [module(test_idg_viewer), module(user)]) :-
    fill,                               % file(File) is inside a collapsed
    this_file(File),                    % module
    graph([file(File)], Items, _),
    nodes(Items, Ids).
test(cluster_totals, Count-Invalid == Count1-Invalid1) :-
    fill,
    graph([module(user)], Items0, _),
    memberchk(cluster(module(user), _, Count, Invalid, _), Items0),
    graph([], Items1, _),
    memberchk(node(module(user), _, Count1, Invalid1, _), Items1).
test(kinds, Kinds == [dynamic, table]) :-
    fill,
    this_file(File),
    graph([module(user), file(File), module(test_idg_viewer)], Items, _),
    findall(K, ( item_node(Items, pred(PI)),
                 item_kind(Items, pred(PI), K)
               ), Kinds0),
    msort(Kinds0, Kinds).
test(patterns, Ids == [ pattern(user:tv_reach/2, tv_reach(+,-)),
                        pattern(user:tv_reach/2, tv_reach(-,-))
                      ]) :-
    fill,
    this_file(File),
    graph([module(user), file(File), pred(user:tv_reach/2)], Items, _),
    nodes(Items, All),
    findall(Id, (member(Id, All), Id = pattern(_, _)), Ids).
test(pattern_edges, true) :-
    fill,
    this_file(File),
    graph([module(user), file(File), module(test_idg_viewer),
           pred(user:tv_reach/2)], _, Edges),
    memberchk(edge(pred(test_idg_viewer:tv_edge/2),
                   pattern(user:tv_reach/2, tv_reach(-,-)), _), Edges).
test(single_module, [ Clusters-Ids == [module(user)] - [no_file, file(File)] ]) :-
    this_file(File),
    in_thread(fill_user, graph([], Items, _)),
    clusters(Items, Clusters),
    nodes(Items, Ids).
test(no_file, Ids == [file(File), pred(user:tv_data/1)]) :-
    this_file(File),
    in_thread(fill_user, graph([no_file], Items, _)),
    nodes(Items, Ids).
%   Every edge connects two nodes that are shown.

test(edges_visible, true) :-
    this_file(File),
    forall(member(Expanded, [ [], [no_file], [collapsed(module(user))],
                              [file(File)], [no_file, file(File)]
                            ]),
           ( in_thread(fill_user, graph(Expanded, Items, Edges)),
             nodes(Items, Ids),
             forall(member(edge(F, T, _), Edges),
                    assertion((memberchk(F, Ids), memberchk(T, Ids))))
           )).
test(collapse_single, Ids == [module(user)]) :-
    in_thread(fill_user, graph([collapsed(module(user))], Items, _)),
    nodes(Items, Ids).
test(counts, Count == Total) :-
    fill,
    graph([], Items, _),
    aggregate_all(sum(C), member(node(_, _, C, _, _), Items), Count),
    thread_self(Me),
    '$idg_thread_summary'(Me, Total, _).
test(invalid, true(Invalid > 0)) :-
    fill,
    add_edge(c, d),
    graph([], Items, _),
    del_edge(c, d),
    aggregate_all(sum(I), member(node(_, _, _, I, _), Items), Invalid).
test(other_thread, Outside == Inside) :-
    this_file(File),
    Expanded = [module(user), file(File), pred(user:tv_reach/2)],
    thread_self(Me),
    thread_create(( fill,
                    graph(Expanded, N, E),
                    msort(E, SE),
                    thread_send_message(Me, inside(N-SE)),
                    thread_get_message(done)
                  ), Id),
    thread_get_message(inside(Inside)),
    idg_graph(Id, Expanded, graph(N1, E1)),
    msort(E1, SE1),
    Outside = N1-SE1,
    thread_send_message(Id, done),
    thread_join(Id).
test(dot, true) :-
    fill,
    this_file(File),
    graph([module(user), file(File)], Items, Edges),
    idg_dot(graph(Items, Edges), Dot, Names),
    memberchk(Cluster-module(user), Names),
    sub_atom(Cluster, 0, _, _, cluster_),
    sub_string(Dot, 0, _, _, "digraph idg {"),
    forall(member(S, ["subgraph cluster_", "shape=", " -> "]),
           once(sub_string(Dot, _, _, _, S))).

%   In the focus graph, a click on another predicate focuses it.

test(focus_tooltips, [ Current-Other-Overview ==
                       "click for call patterns"-"click to focus"-
                       "click for call patterns" ]) :-
    Graph = graph([ node(pred(t:f/0), f, 1, 0, table),
                    node(pred(t:g/0), g, 1, 0, table)
                  ], []),
    idg_dot(Graph, Focus, _, [focus(t:f/0)]),
    idg_dot(Graph, Plain, _),
    click_hint(Focus, "f/0", Current),
    click_hint(Focus, "g/0", Other),
    click_hint(Plain, "g/0", Overview).

click_hint(Dot, PI, Hint) :-
    split_string(Dot, "\n", "", Lines),
    member(Line, Lines),
    sub_string(Line, _, _, _, PI),
    sub_string(Line, B, _, _, "(click "),
    !,
    B1 is B+1,
    sub_string(Line, B1, _, 0, After),
    sub_string(After, L, _, _, ")"),
    !,
    sub_string(After, 0, L, _, Hint).

%   in_thread(:Setup, :Goal)
%
%   Run Setup and Goal in a new thread, so its IDG only holds what
%   Setup creates.

in_thread(Setup, Goal) :-
    thread_self(Me),
    thread_create(( Setup, Goal,
                    thread_send_message(Me, result(Goal))
                  ), Id),
    thread_get_message(result(Goal)),
    thread_join(Id).

item_kind(Items, Id, Kind) :-
    member(Item, Items),
    (   Item = node(Id, _, _, _, Kind)
    ->  true
    ;   Item = cluster(_, _, _, _, Children),
        item_kind(Children, Id, Kind)
    ),
    !.

%   idg_focus_graph/4 shows the graph around a predicate.

test(focus, [ Ids-Edges ==
              [pred(test_idg_viewer:tv_edge/2), pred(user:tv_reach/2)] -
              [ pred(test_idg_viewer:tv_edge/2)-pred(user:tv_reach/2),
                pred(user:tv_reach/2)-pred(user:tv_reach/2)
              ]
            ]) :-
    fill,
    thread_self(Me),
    '$idg_predicate_edges'(Me, N, E),
    idg_focus_graph(idg(N, E), user:tv_reach/2, limits(2,2,6,12,30),
                    graph(Nodes, Edges0)),
    nodes(Nodes, Ids),
    findall(F-T, member(edge(F,T,_), Edges0), Edges1),
    msort(Edges1, Edges).
%   pi(+Name, +I, -PI): a predicate indicator for a synthetic graph.

pi(Name, I, t:PI/0) :-
    format(atom(PI), '~w~w', [Name, I]).

test(focus_prune, [Count-Min == 12-9]) :-
    findall(edge(P, t:f/0, I), (between(1, 20, I), pi(p, I, P)), Edges),
    idg_focus_graph(idg([], Edges), t:f/0, limits(1,1,6,12,30),
                    graph(Nodes, _)),
    findall(C, ( member(edge(P, t:f/0, C), Edges),
                 memberchk(node(pred(P), _, _, _, _), Nodes)
               ), Cs),
    length(Cs, Count),
    min_list(Cs, Min).
test(focus_all_few, Count == 7) :-
    findall(edge(P, t:f/0, I), (between(1, 6, I), pi(p, I, P)), Edges),
    idg_focus_graph(idg([], Edges), t:f/0, limits(1,1,6,12,30),
                    graph(Nodes, _)),
    length(Nodes, Count).
test(focus_depth, Ids == [a1, a2, b1, b2, f]) :-
    Chain = [a3, a2, a1, f, b1, b2, b3],
    findall(edge(t:F/0, t:T/0, 1), nextto(F, T, Chain), Edges),
    idg_focus_graph(idg([], Edges), t:f/0, limits(2,2,6,12,30),
                    graph(Nodes, _)),
    findall(N, member(node(pred(t:N/0), _, _, _, _), Nodes), Ids0),
    msort(Ids0, Ids).
test(focus_max_nodes, Count == 4) :-
    findall(edge(P, t:f/0, 1), (between(1, 3, I), pi(p, I, P)), E1),
    findall(edge(Q, P, 1), ( between(1, 3, I), pi(q, I, Q),
                             between(1, 3, J), pi(p, J, P) ), E2),
    append(E1, E2, Edges),
    idg_focus_graph(idg([], Edges), t:f/0, limits(3,1,6,12,4),
                    graph(Nodes, _)),
    aggregate_all(count, member(node(pred(_), _, _, _, _), Nodes), Count).

%   A predicate with relatives that are not shown has a "more" node:
%   of the 20 predicates affecting f, 12 are shown; the 8 others have
%   1+...+8 = 36 edges to f.

test(focus_more, [ More-Edge == node(more(up, t:f/0), '\u2026 8 more', 8, 36,
                                     more) -
                                edge(more(up, t:f/0), pred(t:f/0), 36)
                 ]) :-
    findall(edge(P, t:f/0, I), (between(1, 20, I), pi(p, I, P)), Edges),
    idg_focus_graph(idg([], Edges), t:f/0, limits(1,1,6,12,30),
                    graph(Nodes, GEdges)),
    memberchk(node(more(up, t:f/0), L, NP, NE, K), Nodes),
    More = node(more(up, t:f/0), L, NP, NE, K),
    memberchk(edge(more(up, t:f/0), To, C), GEdges),
    Edge = edge(more(up, t:f/0), To, C).
test(focus_more_depth, Mores == [more(down, t:b2/0), more(up, t:a2/0)]) :-
    Chain = [a3, a2, a1, f, b1, b2, b3],
    findall(edge(t:F/0, t:T/0, 1), nextto(F, T, Chain), Edges),
    idg_focus_graph(idg([], Edges), t:f/0, limits(2,2,6,12,30),
                    graph(Nodes, _)),
    findall(M, member(node(M, _, _, _, more), Nodes), Mores0),
    msort(Mores0, Mores).
%   The menu of a "more" node shows the predicates it stands for.  That
%   may give the added predicates "more" nodes of their own.

test(focus_show_all, [Count-Mores == 21-[]]) :-
    findall(edge(P, t:f/0, I), (between(1, 20, I), pi(p, I, P)), Edges),
    idg_focus_graph(idg([], Edges), t:f/0, limits(1,1,6,12,30),
                    graph(Nodes, _), [show_all([up-(t:f/0)])]),
    aggregate_all(count, member(node(pred(_), _, _, _, _), Nodes), Count),
    findall(M, member(node(M, _, _, _, more), Nodes), Mores).
test(focus_show_all_chain, Ids-Mores == [a1, a2, a3, a4, f]-[more(up, t:a4/0)]) :-
    Chain = [a5, a4, a3, a2, a1, f],
    findall(edge(t:F/0, t:T/0, 1), nextto(F, T, Chain), Edges),
    idg_focus_graph(idg([], Edges), t:f/0, limits(2,1,6,12,30),
                    graph(Nodes, _), [show_all([up-(t:a2/0), up-(t:a3/0)])]),
    findall(Id, member(node(pred(t:Id/0), _, _, _, _), Nodes), Ids0),
    msort(Ids0, Ids),
    findall(M, member(node(M, _, _, _, more), Nodes), Mores).
%   graphviz accepts the graph with "more" nodes.

test(more_dot, [condition(has_dot), true(Problem == @nil)]) :-
    findall(edge(P, t:f/0, I), (between(1, 20, I), pi(p, I, P)), Edges),
    idg_focus_graph(idg([], Edges), t:f/0, limits(1,1,6,12,30), G),
    idg_dot(G, Dot, _),
    new(X, xdot),
    send(X, load, Dot),
    get(X, problem, Problem),
    free(X).
test(click_more, PI == t:f/0) :-       % clicking "more" focuses its node
    pce_idg_viewer:id_pred(more(up, t:f/0), PI).
test(focus_no_more, Mores == []) :-
    fill,
    thread_self(Me),
    '$idg_predicate_edges'(Me, N, E),
    idg_focus_graph(idg(N, E), user:tv_reach/2, limits(2,2,6,12,30),
                    graph(Nodes, _)),
    findall(M, member(node(M, _, _, _, more), Nodes), Mores).
:- end_tests(idg_graph).

has_dot :-
    absolute_file_name(path(dot), _, [access(execute), file_errors(fail)]).

%   wait_for(:Get, :Test, +Timeout)
%
%   Dispatch events until Test is true after Get.

wait_for(Get, Test, Timeout) :-
    get_time(T0),
    repeat,
      ignore(send(@display, dispatch)),
      call(Get),
      (   call(Test)
      ->  !
      ;   get_time(Now),
          Now-T0 > Timeout
      ->  !, fail
      ;   sleep(0.05),
          fail
      ).

%   The viewer opens on the predicate with most IDG nodes of the thread
%   with most IDG nodes.  The browser shows all predicates; clicking a
%   node of the focus graph makes it the current one.

focus_names(V, Names) :-
    get(V?focus_window, names, Names).
overview_names(O, Names) :-
    get(O, names, Names).

%   click(+Window, +Id)
%
%   Click the node or cluster that shows Id in an IDG window.

click(W, Id) :-
    get(W, names, Names),
    memberchk(Name-Id, Names),
    get(W?xdot?graphicals, find, @arg1?name == Name, Node),
    send(W, clicked, Node).

viewer(V) :-
    fill,
    new(V, prolog_idg_viewer),
    send(V, thread, main),
    wait_for(get(V, predicate, P), P \== none, 10).

%   workers(-Threads)
%
%   The threads computing for the viewer, i.e., all but main and gc.

workers(Threads) :-
    findall(T, ( thread_property(T, status(running)),
                 \+ thread_property(T, alias(main)),
                 \+ thread_property(T, alias(gc))
               ), Threads).

browser_keys(V, Keys) :-
    get(V?browser?dict, members, Ch),
    chain_list(Ch, DIs),
    maplist([DI,K]>>get(DI, key, K), DIs, Keys).

:- begin_tests(idg_viewer, [ condition((current_prolog_flag(threads, true),
                                        has_dot))
                           ]).

test(browser, Keys == ['test_idg_viewer:tv_edge/2', 'user:tv_reach/2']) :-
    viewer(V),
    browser_keys(V, Keys0),
    msort(Keys0, Keys),
    free(V).
%   Incremental dynamic predicates have a style of their own.

test(browser_styles, Styles == [dynamic, @default]) :-
    viewer(V),
    get(V, browser, B),
    maplist([Key, Style]>>( get(B, member, Key, DI),
                            get(DI, style, Style) ),
            ['test_idg_viewer:tv_edge/2', 'user:tv_reach/2'], Styles),
    free(V).

%   The browser widens its column to fit the predicates, but once the
%   column is narrower than what it gave it, the user made it so.

test(browser_shrinks, Wanted == @nil) :-
    viewer(V),
    get(V, browser, B),
    send(B, slot, wanted, 1000),
    send(B, slot, applied, 1000),       % wider than the column is now
    send(B, apply_width),
    get(B, wanted, Wanted),
    free(V).
%   The column of the counts is as wide as the widest count.

test(browser_counts, true(Room >= CountW)) :-
    viewer(V),
    get(V, browser, B),
    send(B, fill, idg([node(user:tv_big/1, 2600000, 0)], [])),
    get(B?font, width, '2,600,000', CountW),
    get(B?text_image, width, W),
    get(B?text_image, tab_stops, Stops),
    get(Stops, element, 1, Stop),
    Room is W - 15 - Stop,
    free(V).
%   Fitting an empty graph does not divide by its size.

test(fit_empty, true) :-
    new(W, xdot_window),
    send(W?xdot, load, "digraph g {}"),
    send(W, fit),
    free(W).
test(focus_click, [Current-Selected == (test_idg_viewer:tv_edge/2)-
                                       'test_idg_viewer:tv_edge/2']) :-
    viewer(V),
    send(V, predicate, user:tv_reach/2),
    wait_for(focus_names(V, Names), memberchk(_-pred(test_idg_viewer:tv_edge/2), Names), 10),
    memberchk(Name-pred(test_idg_viewer:tv_edge/2), Names),
    get(V?focus_window?xdot?graphicals, find, @arg1?name == Name, Node),
    send(V?focus_window, clicked, Node),
    get(V, predicate, Current),
    get(V?browser?selection, key, Selected),
    free(V).
%   The current predicate has a star behind it.

test(focus_star, true) :-
    viewer(V),
    send(V, predicate, user:tv_reach/2),
    wait_for(focus_names(V, Names), memberchk(_-pred(user:tv_reach/2), Names), 10),
    memberchk(Name-pred(user:tv_reach/2), Names),
    get(V?focus_window?xdot, member, Name, Node),
    get(V?focus_window?xdot?graphicals, find,
        message(@arg1, instance_of, path), Star),
    get(Star, center, point(SX, SY)),
    get(Node, center, point(NX, NY)),
    abs(SX-NX) =< 2, abs(SY-NY) =< 2,
    free(V).
%   Focusing a predicate expands its groups in the overview and puts a
%   star behind it there.

test(overview_follows, true) :-
    viewer(V),
    get(V, overview, O),
    wait_for(overview_names(O, Names0), Names0 \== [], 10),
    send(V, predicate, user:tv_reach/2),
    wait_for(overview_names(O, Names), memberchk(_-pred(user:tv_reach/2), Names), 10),
    memberchk(Name-pred(user:tv_reach/2), Names),
    get(O?xdot, member, Name, Node),
    get(O?xdot, member, star, Star),
    get(Star, center, point(SX, SY)),
    get(Node, center, point(NX, NY)),
    abs(SX-NX) =< 2, abs(SY-NY) =< 2,
    free(V).
%   Moving the focus collapses the groups opened for the previous one,
%   unless the user expanded them.

test(overview_closes, true) :-
    viewer(V),
    get(V, overview, O),
    send(V, predicate, user:tv_reach/2),
    wait_for(overview_names(O, N1), memberchk(_-pred(user:tv_reach/2), N1), 10),
    send(V, predicate, test_idg_viewer:tv_edge/2),
    wait_for(overview_names(O, N2),
             ( memberchk(_-pred(test_idg_viewer:tv_edge/2), N2),
               \+ memberchk(_-pred(user:tv_reach/2), N2)
             ), 10),
    free(V).
test(overview_user_keeps, true) :-
    viewer(V),
    get(V, overview, O),
    send(V, predicate, test_idg_viewer:tv_edge/2),
    wait_for(overview_names(O, N1), memberchk(_-pred(test_idg_viewer:tv_edge/2), N1), 10),
    click(O, module(user)),             % the user expands it
    wait_for(overview_names(O, N2), memberchk(_-file(_), N2), 10),
    send(V, predicate, user:tv_reach/2),
    wait_for(overview_names(O, N3), memberchk(_-pred(user:tv_reach/2), N3), 10),
    send(V, predicate, test_idg_viewer:tv_edge/2),
    wait_for(overview_names(O, N4),
             ( memberchk(_-pred(test_idg_viewer:tv_edge/2), N4),
               \+ memberchk(_-pred(user:tv_reach/2), N4)
             ), 10),
    get(O, names, N5),
    assertion(memberchk(_-module(user), N5)),
    assertion(\+ memberchk(_-pred(user:tv_reach/2), N5)),
    free(V).
test(filter, Keys == ['user:tv_reach/2']) :-
    viewer(V),
    send(V?browser, filter, regex(reach)),
    browser_keys(V, Keys),
    free(V).
test(overview_expand, true) :-
    viewer(V),
    get(V, overview, O),
    send(V, predicate, test_idg_viewer:tv_edge/2),  % user is collapsed
    wait_for(overview_names(O, Names0),
             memberchk(_-pred(test_idg_viewer:tv_edge/2), Names0), 10),
    click(O, module(user)),
    wait_for(overview_names(O, Names), memberchk(_-file(_), Names), 10),
    click(O, module(user)),             % the cluster
    wait_for(overview_names(O, Names1), \+ memberchk(_-file(_), Names1), 10),
    free(V).
%   Clicking the current predicate in the focus graph shows its call
%   patterns as a cluster; clicking the cluster hides them.  Clicking
%   another predicate focuses it.

test(focus_patterns, Current == user:tv_reach/2) :-
    viewer(V),
    get(V, focus_window, F),
    send(V, predicate, user:tv_reach/2),
    wait_for(focus_names(V, Names0), memberchk(_-pred(user:tv_reach/2), Names0), 10),
    click(F, pred(user:tv_reach/2)),
    wait_for(focus_names(V, Names1), memberchk(_-pattern(_, _), Names1), 10),
    memberchk(Cluster-pred(user:tv_reach/2), Names1),
    sub_atom(Cluster, 0, _, _, cluster_),
    click(F, pred(user:tv_reach/2)),    % the cluster
    wait_for(focus_names(V, Names2), \+ memberchk(_-pattern(_, _), Names2), 10),
    get(V, predicate, Current),
    free(V).
%   The patterns are computed in a thread; meanwhile the window shows
%   the progress cursor.  The results arrive through in_pce_thread/1, so
%   they wait until we dispatch events.

test(focus_busy, Busy-Done == progress-arrow) :-
    viewer(V),
    get(V, focus_window, F),
    send(V, predicate, user:tv_reach/2),
    send(F, show_patterns, user:tv_reach/2),
    get(F?cursor, name, Busy),
    wait_for(focus_names(V, Names), memberchk(_-pattern(_, _), Names), 10),
    get(F?cursor, name, Done),
    free(V).
test(focus_stale, Patterns-Done == []-arrow) :-  % hidden before the result
    viewer(V),
    get(V, focus_window, F),
    send(V, predicate, user:tv_reach/2),
    send(F, show_patterns, user:tv_reach/2),
    send(F, hide_patterns, user:tv_reach/2),
    wait_for(workers(Workers), Workers == [], 10),
    forall(between(1, 3, _), ignore(send(@display, dispatch))),
    focus_names(V, Names),
    findall(P, (member(_-P, Names), P = pattern(_, _)), Patterns),
    get(F?cursor, name, Done),
    free(V).
%   Any predicate of the focus graph can show its call patterns, not
%   only the current one.  They stay while the predicate is shown.

test(focus_other_patterns, Current == test_idg_viewer:tv_edge/2) :-
    viewer(V),
    get(V, focus_window, F),
    send(V, predicate, test_idg_viewer:tv_edge/2),
    wait_for(focus_names(V, Names0), memberchk(_-pred(user:tv_reach/2), Names0), 10),
    memberchk(Name-pred(user:tv_reach/2), Names0),
    get(F?xdot, member, Name, Node),
    send(F, can_show_patterns, Node),
    send(F, call_patterns, Node, @on),
    wait_for(focus_names(V, Names1),
             memberchk(_-pattern(user:tv_reach/2, _), Names1), 10),
    send(V, predicate, user:tv_reach/2),  % still shown: keeps its patterns
    wait_for(focus_names(V, Names2),
             memberchk(_-pattern(user:tv_reach/2, _), Names2), 10),
    send(V, predicate, test_idg_viewer:tv_edge/2),
    get(V, predicate, Current),
    free(V).
test(focus_other, Current-Patterns == (test_idg_viewer:tv_edge/2)-[]) :-
    viewer(V),
    send(V, predicate, user:tv_reach/2),
    wait_for(focus_names(V, Names0),
             memberchk(_-pred(test_idg_viewer:tv_edge/2), Names0), 10),
    click(V?focus_window, pred(test_idg_viewer:tv_edge/2)),
    get(V, predicate, Current),
    focus_names(V, Names),
    findall(P, (member(_-P, Names), P = pattern(_, _)), Patterns),
    free(V).

:- end_tests(idg_viewer).


%   The variants pane shows a page of variants, more on request, and
%   filters on the text and on having answers.

variants(Spec, P) :-
    variants(Spec, true, P).

variants(Spec, Fill, P) :-
    fill_ge,
    call(Fill),
    new(P, idg_variants),
    send(P, variants, main, Spec),
    settled(P).

%   settled(+Pane)
%
%   Wait for the reply to the last request.

settled(P) :-
    wait_for(true, \+ send(P, pending), 10).

shown(P, Texts) :-
    get(P?browser?dict?members, map, @arg1?key?value, Ch),
    chain_list(Ch, Texts).

next_active(P, Active) :-
    get(P?members, find, @arg1?name == bottom, D),
    get(D, member, next_10, B),
    get(B, active, Active).

answers_menu(P, Kind, Selected) :-
    get(P?members, find, @arg1?name == top, D),
    get(D, member, answers, M),
    send(M, selected, Kind, Selected),
    send(P, apply_filter).

:- begin_tests(idg_variants, [condition(current_prolog_flag(threads, true))]).

test(page, [N0-N-Total-Active == 10-25-25-(@off)]) :-
    variants(pred(user:tv_ge/2), P),
    shown(P, Texts0), length(Texts0, N0),
    send(P, more, 100),
    settled(P),
    shown(P, Texts), length(Texts, N),
    get(P, total, Total),
    next_active(P, Active),
    free(P).
test(with_answers, Texts == ['tv_ge(1,_)', 'tv_ge(2,_)', 'tv_ge(3,_)']) :-
    variants(pred(user:tv_ge/2), P),
    answers_menu(P, no_answers, @off),
    settled(P),
    shown(P, Texts0),
    msort(Texts0, Texts),
    free(P).
test(no_answers, [N-Bad == 10-[]]) :-
    variants(pred(user:tv_ge/2), P),
    answers_menu(P, answers, @off),
    settled(P),
    shown(P, Texts), length(Texts, N),
    intersection(Texts, ['tv_ge(1,_)', 'tv_ge(2,_)', 'tv_ge(3,_)'], Bad),
    free(P).
test(filter, Texts == ['tv_ge(12,_)']) :-
    variants(pred(user:tv_ge/2), P),
    new(R, regex('ge\\(12')),
    send(P, filter, R),
    settled(P),
    shown(P, Texts),
    free(P).
test(filter_kept, Texts == ['tv_ge(12,_)']) :-  % for the next predicate
    variants(pred(user:tv_count/1), P),
    new(R, regex('ge\\(12')),
    send(P, filter, R),
    settled(P),
    send(P, variants, main, pred(user:tv_ge/2)),
    settled(P),
    shown(P, Texts),
    free(P).
test(pattern, Texts == ['tv_count(_)']) :-
    variants(pattern(user:tv_count/1, tv_count(-)), P),
    shown(P, Texts),
    free(P).
test(pin, [Same-Other == true-true]) :-
    pce_idg_viewer:reusable_variants_pane(P1),
    pce_idg_viewer:reusable_variants_pane(P2),
    (P1 == P2 -> Same = true ; Same = false),
    get(P1?members, find, @arg1?name == top, D),
    get(D, member, pin, Pin),
    send(Pin, pinned, @on),
    pce_idg_viewer:reusable_variants_pane(P3),
    (P3 \== P1 -> Other = true ; Other = false),
    free(P1), free(P3).

%   Selecting a variant shows its answers.

answers_of(Spec, Fill, Text, Answers) :-
    variants(Spec, Fill, P),
    get(P?browser, member, Text, DI),
    send(P?browser, selected, DI),
    wait_for(true, \+ send(P, answers_pending), 10),
    get(P?answer_browser?dict?members, map, @arg1?label?value, Ch),
    chain_list(Ch, Answers0),
    msort(Answers0, Answers),
    free(P).

answer_labels(P, Answers) :-
    wait_for(true, \+ send(P, answers_pending), 10),
    get(P?answer_browser?dict?members, map, @arg1?label?value, Ch),
    chain_list(Ch, Answers0),
    msort(Answers0, Answers).

select(P, Text) :-
    get(P?browser, member, Text, DI),
    send(P?browser, selected, DI).

test(answers_filter, [A1-A2 == ['tv_ge(1,3)']-['tv_ge(2,3)']]) :-
    variants(pred(user:tv_ge/2), P),
    select(P, 'tv_ge(1,_)'),
    answer_labels(P, _),
    new(R, regex(',3')),
    send(P, answers_filter, R),
    answer_labels(P, A1),
    select(P, 'tv_ge(2,_)'),            % the filter stays
    answer_labels(P, A2),
    free(P).
test(answers, Answers == ['tv_ge(1,1)', 'tv_ge(1,2)', 'tv_ge(1,3)']) :-
    answers_of(pred(user:tv_ge/2), true, 'tv_ge(1,_)', Answers).
test(answers_moded, Answers == ['tv_max(a,3)', 'tv_max(b,2)']) :-
    answers_of(pred(user:tv_max/2), forall(user:tv_max(_, _), true),
               'tv_max(_,_)', Answers).
test(answers_undefined, Answers == ['tv_u(1)', 'tv_u(2) (undefined)']) :-
    answers_of(pred(user:tv_u/1), forall(call_delays(user:tv_u(_), _), true),
               'tv_u(_)', Answers).

:- end_tests(idg_variants).
