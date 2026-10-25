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

:- module(pce_idg_viewer,
          [ idg_graph/3,                        % +Thread, +Expanded, -Graph
            idg_focus_graph/4,                  % +PredGraph, +Focus, +Limits,
                                                % -Graph
            idg_focus_graph/5,                  % +PredGraph, +Focus, +Limits,
                                                % -Graph, +Options
            idg_dot/3,                          % +Graph, -Dot, -Names
            idg_dot/4,                          % +Graph, -Dot, -Names, +Options
            idg_threads/1,                      % -Threads
            idg_show_variants/2                 % +Thread, +Spec
          ]).
:- use_module(library(pce)).
:- use_module(library(pane_frame)).
:- use_module(library(xdot)).
:- use_module(library(pce_filter_item), []).
:- use_module(library(tabbed_window), []).
:- use_module(library(pce_pin_button), []).
:- autoload(library(pce_util), [center_on/2]).
:- use_module(library(yall), [(>>)/2, (>>)/3]).
:- autoload(library(edit), [edit/1]).
:- autoload(library(apply),
            [maplist/3, foldl/4, include/3, exclude/3, partition/4]).
:- autoload(library(lists),
            [ member/2, append/3, append/2, sum_list/2, last/2,
              list_to_set/2, selectchk/3, nth0/3
            ]).
:- autoload(library(pairs),
            [group_pairs_by_key/2, pairs_keys_values/3, pairs_values/2]).
:- autoload(library(aggregate), [aggregate_all/3]).
:- autoload(library(option), [option/3]).
:- autoload(library(thread), [call_in_thread/3]).

/** <module> Show the incremental dependency graph

The incremental dependency graph (IDG) of a thread connects the nodes of
incremental dynamic predicates and incremental tables: an edge From->To
means that a change to From invalidates To.  A large program has
hundreds of thousands of nodes, so the graph is shown at a level of
abstraction that can be refined.  The levels nest:

  - A module holds its predicates.  The module `user` holds the
    non-modular code, so its predicates are grouped by the file that
    defines them; those without a file, typically asserted data, are
    in the group `no file`.
  - A file holds its predicates.
  - A predicate holds its call patterns, e.g., `p(+,-)` for its variants
    whose first argument is bound.

The viewer, like the profiler, has a browser of all predicates with IDG
nodes and a filter, and shows the graph around the current predicate:
what affects it on the left and what it affects on the right (see
idg_focus_graph/4).  Clicking a node makes it the current predicate;
clicking the current predicate shows its call patterns as a cluster.
The overview shows the levels above: a collapsed group is a node; an
expanded one is a graphviz cluster that shows its parts.  If all
predicates are in one module, that module is expanded initially.  The predicate level comes from
'$idg_predicate_edges'/3, which reads the IDG of any thread.  The call
patterns are computed by the thread that owns the IDG, using
call_in_thread/3 for another thread.
*/

%!  idg_graph(+Thread, +Expanded, -Graph) is det.
%
%   Graph is graph(Items, Edges) describing the IDG of Thread (a thread
%   or `shared`), where the groups in Expanded show their parts.  The
%   identifiers of the groups and nodes are module(M), file(File),
%   pred(PI) and pattern(PI, Pattern).  Expanded may hold
%   collapsed(module(M)) to collapse the module that is otherwise
%   expanded because it is the only one.  Items is a list of
%
%   no_file is the group of the predicates of `user` without a file.
%
%     - node(Id, Label, Count, Invalid, Kind)
%       A node of the graph.  Count is the number of IDG nodes it
%       stands for and Invalid how many of them are invalid.  Kind is
%       one of `module`, `file`, `dynamic` or `table`.
%     - cluster(Id, Label, Count, Invalid, Items)
%       An expanded group and its parts.  Count and Invalid are the
%       totals of the parts.
%
%   Edges is a list of edge(FromId, ToId, Count) between nodes.

idg_graph(Thread, Expanded0, graph(Items, Edges)) :-
    '$idg_predicate_edges'(Thread, PNodes, PEdges),
    findall(M, member(node(M:_, _, _), PNodes), Ms0),
    sort(Ms0, Ms),
    (   Ms = [M1],
        \+ memberchk(collapsed(module(M1)), Expanded0)
    ->  Expanded = [module(M1)|Expanded0]
    ;   Expanded = Expanded0
    ),
    findall(PI, ( member(node(PI, _, _), PNodes),
                  pattern_pred(PI, Expanded)
                ), PatPreds),
    pattern_data(Thread, PatPreds, PatNodes, PatEdges),
    maplist(module_item(PNodes, PatNodes, Expanded), Ms, Items),
    display_edges(PEdges, Expanded, PatPreds, PatEdges, Edges).

%   pred_groups(+PI, -Groups)
%
%   Groups are the groups that hold the predicate PI, outermost first.

pred_groups(PI, Groups) :-
    PI = M:_,
    (   M == user
    ->  (   pred_file(PI, File)
        ->  Groups = [module(M), file(File)]
        ;   Groups = [module(M), no_file]
        )
    ;   Groups = [module(M)]
    ).

%   pred_file(+PI, -File)
%
%   The predicates of `user` are grouped by the file that defines them.

pred_file(user:Name/Arity, File) :-
    functor(Head, Name, Arity),
    predicate_property(user:Head, file(File)).

%   pattern_pred(+PI, +Expanded)
%
%   PI shows its call patterns.

pattern_pred(PI, Expanded) :-
    memberchk(pred(PI), Expanded),
    pred_groups(PI, Groups),
    forall(member(G, Groups), memberchk(G, Expanded)).

%   visible_id(+PI, +Expanded, -Id)
%
%   Id is the node that shows the predicate PI: its outermost collapsed
%   group or the predicate itself.

visible_id(PI, Expanded, Id) :-
    pred_groups(PI, Groups),
    (   member(G, Groups),
        \+ memberchk(G, Expanded)
    ->  Id = G
    ;   Id = pred(PI)
    ).

%   module_item(+PNodes, +PatNodes, +Expanded, +Module, -Item)

module_item(PNodes, PatNodes, Expanded, M, Item) :-
    findall(PI-(C-I), ( member(node(PI, C, I), PNodes), PI = M:_ ), Preds),
    totals(Preds, Count, Invalid),
    (   memberchk(module(M), Expanded)
    ->  (   M == user
        ->  partition(has_file, Preds, InFiles, NoFile),
            findall(F-P, ( member(P, InFiles), P = PI-_, pred_file(PI, F) ),
                    FPs),
            keysort(FPs, SortedFPs),
            group_pairs_by_key(SortedFPs, ByFile),
            maplist(file_item(PatNodes, Expanded), ByFile, FileItems),
            (   NoFile == []
            ->  Children = FileItems
            ;   no_file_item(PatNodes, Expanded, NoFile, NoFileItem),
                append(FileItems, [NoFileItem], Children)
            )
        ;   maplist(pred_item(PatNodes, Expanded), Preds, Children)
        ),
        format(atom(Label), 'module ~w', [M]),
        Item = cluster(module(M), Label, Count, Invalid, Children)
    ;   Item = node(module(M), M, Count, Invalid, module)
    ).

has_file(PI-_) :-
    pred_file(PI, _).

no_file_item(PatNodes, Expanded, Preds, Item) :-
    totals(Preds, Count, Invalid),
    (   memberchk(no_file, Expanded)
    ->  maplist(pred_item(PatNodes, Expanded), Preds, Children),
        Item = cluster(no_file, 'no file', Count, Invalid, Children)
    ;   Item = node(no_file, 'no file', Count, Invalid, file)
    ).

file_item(PatNodes, Expanded, File-Preds, Item) :-
    totals(Preds, Count, Invalid),
    file_base_name(File, Label),
    (   memberchk(file(File), Expanded)
    ->  maplist(pred_item(PatNodes, Expanded), Preds, Children),
        Item = cluster(file(File), Label, Count, Invalid, Children)
    ;   Item = node(file(File), Label, Count, Invalid, file)
    ).

pred_item(PatNodes, Expanded, PI-(Count-Invalid), Item) :-
    pi_label(PI, Label),
    pi_kind(PI, Kind),
    (   memberchk(pred(PI), Expanded),
        findall(node(pattern(PI, P), PL, C, I, Kind),
                ( member(node(pattern(PI, P), C, I), PatNodes),
                  format(atom(PL), '~q', [P])
                ), Patterns),
        Patterns \== []
    ->  Item = cluster(pred(PI), Label, Count, Invalid, Patterns)
    ;   Item = node(pred(PI), Label, Count, Invalid, Kind)
    ).

totals(Pairs, Count, Invalid) :-
    foldl(add_counts, Pairs, 0-0, Count-Invalid).

add_counts(_-(C-I), C0-I0, C1-I1) :-
    C1 is C0+C,
    I1 is I0+I.

pi_label(user:PI, Label) :-
    !,
    format(atom(Label), '~q', [PI]).
pi_label(PI, Label) :-
    format(atom(Label), '~q', [PI]).

pi_kind(M:Name/Arity, Kind) :-
    functor(Head, Name, Arity),
    (   predicate_property(M:Head, dynamic)
    ->  Kind = (dynamic)
    ;   Kind = (table)
    ).

%   display_edges(+PEdges, +Expanded, +PatPreds, +PatEdges, -Edges)
%
%   The predicate edges between predicates that do not show their call
%   patterns plus the edges computed for the patterns, mapped to the
%   visible nodes.  Edges inside a collapsed group are not shown.

display_edges(PEdges, Expanded, PatPreds, PatEdges, Edges) :-
    findall((F-T)-C,
            ( member(edge(FP, TP, C), PEdges),
              \+ memberchk(FP, PatPreds),
              \+ memberchk(TP, PatPreds),
              visible_id(FP, Expanded, F),
              visible_id(TP, Expanded, T)
            ), Pairs0),
    findall((F-T)-C,
            ( member(edge(FK, TK, C), PatEdges),
              key_id(FK, Expanded, F),
              key_id(TK, Expanded, T)
            ), Pairs1),
    append(Pairs0, Pairs1, Pairs),
    keysort(Pairs, Sorted),
    group_pairs_by_key(Sorted, ByEdge),
    findall(edge(F, T, C),
            ( member((F-T)-Cs, ByEdge),
              \+ ( F == T, is_group(F) ),
              sum_list(Cs, C)
            ), Edges).

is_group(module(_)).
is_group(file(_)).
is_group(no_file).

key_id(pattern(PI, P), _, pattern(PI, P)) :- !.
key_id(pred(PI), Expanded, Id) :-
    visible_id(PI, Expanded, Id).

%   pattern_data(+Thread, +Preds, -Nodes, -Edges)
%
%   Nodes are node(pattern(PI,P), Count, Invalid) for the variants of
%   Preds and Edges are edge(From, To, Count) from and to the patterns,
%   where the other end is a pattern of Preds or pred(PI).  Only the
%   thread that owns the IDG can find the variants, so this runs there.

pattern_data(_, [], [], []) :-
    !.
pattern_data(Thread, Preds, Nodes, Edges) :-
    (   Thread == shared
    ->  pattern_data_in(shared, Preds, Nodes, Edges)
    ;   thread_self(Me),
        same_thread(Me, Thread)
    ->  pattern_data_in(local, Preds, Nodes, Edges)
    ;   call_in_thread(Thread,
                       pattern_data_in(local, Preds, Nodes, Edges),
                       [timeout(30)])
    ).

same_thread(T1, T2) :-
    thread_property(T1, id(Id)),
    thread_property(T2, id(Id)).

%   pattern_data_in(+Scope, +Preds, -Nodes, -Edges)
%
%   Compute the pattern data from the local or shared variant table.
%   The counts are kept in tries, as a predicate may have hundreds of
%   thousands of variants and millions of edges.

pattern_data_in(Scope, Preds, Nodes, Edges) :-
    (   scope_variant_table(Scope, VT)
    ->  trie_new(NodeCounts),
        trie_new(EdgeCounts),
        forall(( trie_gen(VT, M:Variant, Trie),
                 variant_head(M, Variant, Head),
                 functor(Head, Name, Arity),
                 memberchk(M:Name/Arity, Preds)
               ),
               count_variant(M:Name/Arity, Head, Trie, Preds,
                             NodeCounts, EdgeCounts)),
        findall(node(Key, C, I),
                ( trie_gen(NodeCounts, Key-count, C),
                  (   trie_lookup(NodeCounts, Key-invalid, I0)
                  ->  I = I0
                  ;   I = 0
                  )
                ), Nodes),
        findall(edge(F, T, C), trie_gen(EdgeCounts, F-T, C), Edges),
        trie_destroy(NodeCounts),
        trie_destroy(EdgeCounts)
    ;   Nodes = [],
        Edges = []
    ).

scope_variant_table(local, VT) :-
    '$tbl_local_variant_table'(VT).
scope_variant_table(shared, VT) :-
    '$tbl_global_variant_table'(VT).

%   variant_head(+Module, +Variant, -Head)
%
%   The variant table holds the variants of the wrappers; for a moded
%   table these differ from the head of the predicate.

variant_head(M, Variant, Head) :-
    (   catch(M:'$table_mode'(Head0, Variant, _), error(_,_), fail)
    ->  Head = Head0
    ;   Head = Variant                  % dynamic incremental predicate
    ).

count_variant(PI, Head, Trie, Preds, NodeCounts, EdgeCounts) :-
    call_pattern(Head, Pattern),
    Key = pattern(PI, Pattern),
    count(NodeCounts, Key-count),
    (   '$idg_falsecount'(Trie, FC),
        FC > 0
    ->  count(NodeCounts, Key-invalid)
    ;   true
    ),
    forall('$idg_edge'(Trie, affected, To),
           ( trie_key(To, Preds, ToKey)
           ->  count(EdgeCounts, Key-ToKey)
           ;   true
           )),
    forall('$idg_edge'(Trie, dependent, From),
           (   trie_key(From, Preds, FromKey),
               FromKey = pred(_)           % else counted from there
           ->  count(EdgeCounts, FromKey-Key)
           ;   true
           )).

%   trie_key(+Trie, +Preds, -Key)
%
%   Key is the pattern of the answer trie Trie if its predicate is in
%   Preds, else pred(PI).

trie_key(Trie, Preds, Key) :-
    '$tbl_table_status'(Trie, _Status, M:Variant, _),
    variant_head(M, Variant, Head),
    functor(Head, Name, Arity),
    PI = M:Name/Arity,
    (   memberchk(PI, Preds)
    ->  call_pattern(Head, Pattern),
        Key = pattern(PI, Pattern)
    ;   Key = pred(PI)
    ).

%   call_pattern(+Head, -Pattern)
%
%   Pattern has `+` for the arguments of Head that are bound and `-`
%   for those that are free.

call_pattern(Head, Pattern) :-
    Head =.. [Name|Args],
    maplist(arg_mode, Args, Modes),
    Pattern =.. [Name|Modes].

arg_mode(Arg, Mode) :-
    (   var(Arg)
    ->  Mode = (-)
    ;   Mode = (+)
    ).

count(Trie, Key) :-
    (   trie_lookup(Trie, Key, N0)
    ->  N is N0+1,
        trie_update(Trie, Key, N)
    ;   trie_insert(Trie, Key, 1)
    ).

%!  idg_threads(-Threads) is det.
%
%   Threads is a list of Thread-Nodes for the threads that have IDG
%   nodes, followed by shared-Nodes if the shared tables have any.

idg_threads(Threads) :-
    findall(Id-N,
            ( thread_property(T, status(running)),
              thread_property(T, id(Id0)),
              (   thread_property(T, alias(Alias))
              ->  Id = Alias
              ;   Id = Id0
              ),
              catch('$idg_thread_summary'(T, N, _), error(_,_), fail),
              N > 0
            ), Local),
    (   '$idg_thread_summary'(shared, NS, _),
        NS > 0
    ->  append(Local, [shared-NS], Threads)
    ;   Threads = Local
    ).


                 /*******************************
                 *             FOCUS            *
                 *******************************/

%!  idg_focus_graph(+PredGraph, +Focus, +Limits, -Graph) is det.
%!  idg_focus_graph(+PredGraph, +Focus, +Limits, -Graph, +Options) is det.
%
%   Graph is graph(Nodes, Edges) around the predicate Focus, like the
%   call graph of the profiler: the predicates that affect Focus and
%   those that Focus affects.  PredGraph is idg(Nodes, Edges) as
%   returned by '$idg_predicate_edges'/3.  Limits is
%
%       limits(UpDepth, DownDepth, PruneAbove, MaxRelatives, MaxNodes)
%
%   The direct relatives of a predicate in a direction are all shown if
%   there are at most PruneAbove of them; of more, those with at least
%   half their fair share of the edges, at most MaxRelatives.  From the
%   direct relatives the graph grows UpDepth-1 and DownDepth-1 levels,
%   the heaviest edges first, while it has fewer than MaxNodes nodes.
%   Edges holds all edges between the predicates shown.  A predicate
%   with relatives that are not shown has a node
%
%       node(more(Dir, PI), Label, Preds, Edges, more)
%
%   connected to it, where Dir is `up` for the predicates that affect
%   PI and `down` for those that PI affects, Preds is the number of
%   predicates not shown and Edges the number of IDG edges to them.
%   Options:
%
%     - show_all(+List)
%       List holds Dir-PI pairs: if PI is shown, so are all its
%       relatives in direction Dir.

idg_focus_graph(PredGraph, Focus, Limits, Graph) :-
    idg_focus_graph(PredGraph, Focus, Limits, Graph, []).

idg_focus_graph(idg(PNodes, PEdges), Focus, Limits, graph(Nodes, Edges),
                Options) :-
    option(show_all(ShowAll), Options, []),
    focus_preds(PEdges, Focus, Limits, Preds0),
    add_all_relatives(ShowAll, PEdges, Preds0, Preds),
    findall(node(pred(PI), Label, C, I, Kind),
            ( member(PI, Preds),
              (   memberchk(node(PI, C, I), PNodes)
              ->  true
              ;   C = 0, I = 0
              ),
              pi_label(PI, Label),
              pi_kind(PI, Kind)
            ), PredNodes),
    findall(edge(pred(F), pred(T), C),
            ( member(edge(F, T, C), PEdges),
              memberchk(F, Preds),
              memberchk(T, Preds)
            ), PredEdges),
    findall(More-MoreEdge,
            ( member(PI, Preds),
              member(Dir, [up, down]),
              hidden_relatives(PEdges, Dir, PI, Preds, NPreds, NEdges),
              format(atom(Label), '\u2026 ~D more', [NPreds]),
              More = node(more(Dir, PI), Label, NPreds, NEdges, more),
              more_edge(Dir, PI, NEdges, MoreEdge)
            ), Mores),
    pairs_keys_values(Mores, MoreNodes, MoreEdges),
    append(PredNodes, MoreNodes, Nodes),
    append(PredEdges, MoreEdges, Edges).

%   hidden_relatives(+PEdges, +Dir, +PI, +Shown, -NPreds, -NEdges)
%
%   PI has NPreds relatives in direction Dir that are not in Shown,
%   connected by NEdges IDG edges.  Fails if there are none.

hidden_relatives(PEdges, Dir, PI, Shown, NPreds, NEdges) :-
    findall(Other-C,
            ( dir_edge(Dir, PEdges, PI, Other, C),
              \+ memberchk(Other, Shown)
            ), Hidden),
    Hidden \== [],
    length(Hidden, NPreds),
    aggregate_all(sum(C), member(_-C, Hidden), NEdges).

%   add_all_relatives(+ShowAll, +PEdges, +Preds0, -Preds)
%
%   Add all relatives of the Dir-PI pairs in ShowAll whose PI is in
%   Preds0.  A predicate added may itself be in ShowAll, so this repeats
%   until nothing is added.

add_all_relatives(ShowAll, PEdges, Preds0, Preds) :-
    findall(Other,
            ( member(Dir-PI, ShowAll),
              memberchk(PI, Preds0),
              dir_edge(Dir, PEdges, PI, Other, _),
              \+ memberchk(Other, Preds0)
            ), New0),
    list_to_set(New0, New),
    (   New == []
    ->  Preds = Preds0
    ;   append(Preds0, New, Preds1),
        add_all_relatives(ShowAll, PEdges, Preds1, Preds)
    ).

more_edge(up,   PI, C, edge(more(up, PI), pred(PI), C)).
more_edge(down, PI, C, edge(pred(PI), more(down, PI), C)).

focus_preds(PEdges, Focus, Limits, Preds) :-
    Limits = limits(UpDepth, DownDepth, _, _, MaxNodes),
    relatives(PEdges, up, Focus, Limits, Ups),
    relatives(PEdges, down, Focus, Limits, Downs),
    pairs_values(Ups, UpPreds),
    pairs_values(Downs, DownPreds),
    append([[Focus], UpPreds, DownPreds], Seen0),
    list_to_set(Seen0, Seen),
    MoreUp is UpDepth-1,
    MoreDown is DownDepth-1,
    grow(MoreUp-UpPreds, MoreDown-DownPreds, PEdges, Limits, MaxNodes,
         Seen, Preds).

%   relatives(+PEdges, +Dir, +Pred, +Limits, -Relatives)
%
%   Relatives is a list Count-Other of the predicates that affect Pred
%   (up) or that Pred affects (down), pruned as described above,
%   heaviest first.

relatives(PEdges, Dir, Pred, limits(_, _, PruneAbove, MaxRelatives, _),
          Relatives) :-
    findall(C-Other, dir_edge(Dir, PEdges, Pred, Other, C), Pairs0),
    sort(1, @>=, Pairs0, Pairs),
    length(Pairs, N),
    (   N =< PruneAbove
    ->  Relatives = Pairs
    ;   aggregate_all(sum(C), member(C-_, Pairs), Total),
        include(fair_share(N, Total), Pairs, Fair),
        max_prefix(Fair, MaxRelatives, Relatives)
    ).

dir_edge(up, PEdges, Pred, Other, C) :-
    member(edge(Other, Pred, C), PEdges),
    Other \== Pred.
dir_edge(down, PEdges, Pred, Other, C) :-
    member(edge(Pred, Other, C), PEdges),
    Other \== Pred.

fair_share(N, Total, C-_) :-
    C*2*N >= Total.

max_prefix(List, Max, Prefix) :-
    length(List, Len),
    Len > Max,
    !,
    length(Prefix, Max),
    append(Prefix, _, List).
max_prefix(List, _, List).

%   grow(+Up, +Down, +PEdges, +Limits, +MaxNodes, +Seen, -Preds)
%
%   Add a level of relatives above the predicates of the Depth-Frontier
%   pair Up and below those of Down, heaviest first, while there is room.

grow(UpDepth-Ups, DownDepth-Downs, PEdges, Limits, Max, Seen, Preds) :-
    candidates(up, UpDepth, Ups, PEdges, Limits, Above),
    candidates(down, DownDepth, Downs, PEdges, Limits, Below),
    append(Above, Below, Candidates0),
    sort(1, @>=, Candidates0, Candidates),
    (   Candidates == []
    ->  Preds = Seen
    ;   admit(Candidates, Max, Seen, Seen1, NewUps, NewDowns),
        UpDepth1 is UpDepth-1,
        DownDepth1 is DownDepth-1,
        (   NewUps == [], NewDowns == []
        ->  Preds = Seen1
        ;   grow(UpDepth1-NewUps, DownDepth1-NewDowns, PEdges, Limits, Max,
                 Seen1, Preds)
        )
    ).

candidates(_, Depth, _, _, _, []) :-
    Depth =< 0,
    !.
candidates(Dir, _, Frontier, PEdges, Limits, Candidates) :-
    findall(C-(Dir-Other),
            ( member(P, Frontier),
              relatives(PEdges, Dir, P, Limits, Rs),
              member(C-Other, Rs)
            ), Candidates).

admit([], _, Seen, Seen, [], []).
admit([_-(Dir-P)|T], Max, Seen0, Seen, Ups, Downs) :-
    (   memberchk(P, Seen0)
    ->  admit(T, Max, Seen0, Seen, Ups, Downs)
    ;   length(Seen0, N),
        N < Max
    ->  append(Seen0, [P], Seen1),
        (   Dir == up
        ->  Ups = [P|Ups1], Downs = Downs1
        ;   Downs = [P|Downs1], Ups = Ups1
        ),
        admit(T, Max, Seen1, Seen, Ups1, Downs1)
    ;   admit(T, Max, Seen0, Seen, Ups, Downs)
    ).


                 /*******************************
                 *              DOT             *
                 *******************************/

%!  idg_dot(+Graph, -Dot, -Names) is det.
%!  idg_dot(+Graph, -Dot, -Names, +Options) is det.
%
%   Dot is a string in the graphviz dot language for Graph (see
%   idg_graph/3).  Names is a list Name-Id, where Name is the graphviz
%   name of the node or cluster for Id.  Clusters are subgraphs whose
%   name starts with `cluster`.  Dynamic predicates are boxes, tables
%   are ellipses, files are folders and modules are tabs.  Nodes with
%   invalid IDG nodes are filled and clusters with invalid IDG nodes
%   have a red frame.  The width of an edge grows with the logarithm of
%   the number of IDG edges it stands for.  Options:
%
%     - focus(+PI)
%       The graph is the focus graph of PI.  The tooltips say what a
%       click does there: show the call patterns of PI or focus on
%       another predicate.

idg_dot(Graph, Dot, Names) :-
    idg_dot(Graph, Dot, Names, []).

idg_dot(graph(Items, Edges), Dot, Names, Options) :-
    option(focus(Focus), Options, none),
    item_names(Items, 1, _, Names, []),
    with_output_to(
        string(Dot),
        ( format('digraph idg {~n'),
          format('  rankdir=LR;~n'),
          format('  node [fontname="sans-serif", fontsize=10];~n'),
          format('  edge [arrowsize=0.6];~n'),
          forall(member(Item, Items), dot_item(Item, Names, Focus, 1)),
          forall(member(Edge, Edges), dot_edge(Edge, Names)),
          format('}~n')
        )).

%   item_names(+Items, +N0, -N, -Names, ?Tail)
%
%   Number the items depth first.

item_names([], N, N, Names, Names).
item_names([Item|Items], N0, N, [Name-Id|Names0], Tail) :-
    item_id(Item, Id, Prefix),
    format(atom(Name), '~w~d', [Prefix, N0]),
    N1 is N0+1,
    (   Item = cluster(_, _, _, _, Children)
    ->  item_names(Children, N1, N2, Names0, Names1)
    ;   N2 = N1,
        Names1 = Names0
    ),
    item_names(Items, N2, N, Names1, Tail).

item_id(node(Id, _, _, _, _), Id, n).
item_id(cluster(Id, _, _, _, _), Id, cluster_).

dot_item(node(more(Dir, PI), Label, NPreds, NEdges, more), Names, _,
         Indent) :-
    !,
    memberchk(Name-more(Dir, PI), Names),
    more_tooltip(Dir, PI, NPreds, NEdges, Tooltip),
    atom_string(Label, Text),           % ~q writes a string as "..."
    format('~t~*|  ~w [label=~q, shape=box, style="dashed,rounded", \c
            color="#909090", fontcolor="#606060", tooltip=~q];~n',
           [Indent, Name, Text, Tooltip]).
dot_item(node(Id, Label, Count, Invalid, Kind), Names, Focus, Indent) :-
    memberchk(Name-Id, Names),
    kind_shape(Kind, Shape),
    counted_nodes(Count, Invalid, Counts),
    format(string(Text), '~w\n~w', [Label, Counts]),
    node_tooltip(Id, Kind, Counts, Focus, Tooltip),
    (   Invalid > 0
    ->  Style = ', style=filled, fillcolor="#f4b6b6"'
    ;   Style = ''
    ),
    format('~t~*|  ~w [label=~q, shape=~w, tooltip=~q~w];~n',
           [Indent, Name, Text, Shape, Tooltip, Style]).
dot_item(cluster(Id, Label, Count, Invalid, Children), Names, Focus,
         Indent) :-
    memberchk(Name-Id, Names),
    counted_nodes(Count, Invalid, Counts),
    format(string(Text), '~w\n~w', [Label, Counts]),
    cluster_tooltip(Id, Counts, Tooltip),
    (   Invalid > 0
    ->  Colour = "#c04040"
    ;   Colour = "#909090"
    ),
    format('~t~*|  subgraph ~w {~n', [Indent, Name]),
    format('~t~*|    label=~q; tooltip=~q; style=rounded; color=~q; \c
            fontname="sans-serif"; fontsize=10;~n',
           [Indent, Text, Tooltip, Colour]),
    Indent1 is Indent+2,
    forall(member(Item, Children), dot_item(Item, Names, Focus, Indent1)),
    format('~t~*|  }~n', [Indent]).

kind_shape(module,  tab).
kind_shape(file,    folder).
kind_shape(dynamic, box).
kind_shape(table,   ellipse).

counted_nodes(Count, Invalid, Text) :-
    (   Count == 1
    ->  What = node
    ;   What = nodes
    ),
    (   Invalid > 0
    ->  format(string(Text), '~D ~w, ~D invalid', [Count, What, Invalid])
    ;   format(string(Text), '~D ~w', [Count, What])
    ).

%   node_tooltip(+Id, +Kind, +Counts, +Focus, -Tooltip)
%
%   Focus is the predicate of the focus graph or `none` for the
%   overview.  In the focus graph, clicking another predicate focuses
%   it.

node_tooltip(module(M), _, Counts, _, Tooltip) :-
    !,
    format(string(Tooltip), 'Module ~w: ~w (click to expand)', [M, Counts]).
node_tooltip(file(File), _, Counts, _, Tooltip) :-
    !,
    format(string(Tooltip), 'File ~w: ~w (click to expand)', [File, Counts]).
node_tooltip(no_file, _, Counts, _, Tooltip) :-
    !,
    format(string(Tooltip),
           'Predicates of user without a file, e.g., asserted data: ~w \c
            (click to expand)', [Counts]).
node_tooltip(pred(PI), Kind, Counts, Focus, Tooltip) :-
    !,
    kind_name(Kind, KindName),
    (   ( Focus == none ; Focus == PI )
    ->  Click = 'click for call patterns'
    ;   Click = 'click to focus'
    ),
    format(string(Tooltip), '~q (~w): ~w (~w)',
           [PI, KindName, Counts, Click]).
node_tooltip(pattern(PI, Pattern), Kind, Counts, _, Tooltip) :-
    kind_name(Kind, KindName),
    PI = M:_,
    format(string(Tooltip), '~q (~w): ~w (click to collapse)',
           [M:Pattern, KindName, Counts]).

cluster_tooltip(module(M), Counts, Tooltip) :-
    !,
    format(string(Tooltip), 'Module ~w: ~w (click to collapse)', [M, Counts]).
cluster_tooltip(file(File), Counts, Tooltip) :-
    !,
    format(string(Tooltip), 'File ~w: ~w (click to collapse)', [File, Counts]).
cluster_tooltip(no_file, Counts, Tooltip) :-
    !,
    format(string(Tooltip),
           'Predicates of user without a file: ~w (click to collapse)',
           [Counts]).
cluster_tooltip(pred(PI), Counts, Tooltip) :-
    format(string(Tooltip), '~q: ~w (click to collapse)', [PI, Counts]).

more_tooltip(Dir, PI, NPreds, NEdges, Tooltip) :-
    (   NPreds == 1 -> P = predicate ; P = predicates ),
    (   NEdges == 1 -> E = edge ; E = edges ),
    (   Dir == up
    ->  format(string(Tooltip),
               '~D more ~w affect ~q by ~D ~w (click to focus ~q; \c
                its menu shows them all)',
               [NPreds, P, PI, NEdges, E, PI])
    ;   format(string(Tooltip),
               '~q affects ~D more ~w by ~D ~w (click to focus ~q; \c
                its menu shows them all)',
               [PI, NPreds, P, NEdges, E, PI])
    ).

kind_name(dynamic, 'incremental dynamic').
kind_name(table,   'incremental table').

dot_edge(edge(From, To, Count), Names) :-
    memberchk(F-From, Names),
    memberchk(T-To, Names),
    (   ( From = more(_, _) ; To = more(_, _) )
    ->  Style = ', style=dashed, color="#909090"'
    ;   Style = ''
    ),
    Width is min(6.0, 1.0 + log10(Count)),
    (   Count == 1
    ->  What = edge
    ;   What = edges
    ),
    format(string(Tooltip), '~D ~w', [Count, What]),
    format('  ~w -> ~w [penwidth=~2f, tooltip=~q~w];~n',
           [F, T, Width, Tooltip, Style]).


                 /*******************************
                 *             PANE             *
                 *******************************/

/* The viewer is a pane of the IDE (see library(pane_frame)), laid out
as the profiler: a browser of the predicates involved, with a filter
and the thread above it, and two views of the IDG: the graph around the
current predicate and an overview by module and file.

The IDG per predicate is read once per refresh using
'$idg_predicate_edges'/3, in a thread of its own.  The browser and the
focus graph are computed from it.  The overview and the call patterns
of the current predicate are also computed in a thread of their own.  A
result that arrives after a newer refresh started is dropped.
*/

:- pce_begin_class(prolog_idg_viewer, tool_pane,
                   "Show the incremental dependency graph").

variable(thread,     any*,     get, "Thread whose IDG is shown").
variable(graph,      prolog,   get, "idg(Nodes, Edges) per predicate").
variable(predicate,  prolog,   get, "Current predicate or `none`").
variable(generation, int := 0, get, "Number of the last refresh started").

class_variable(up_depth,      '1..', 2,  "Levels that affect the focus").
class_variable(down_depth,    '1..', 2,  "Levels the focus affects").
class_variable(prune_above,   '1..', 6,  "Relatives that are all shown").
class_variable(max_relatives, '1..', 12, "Most relatives shown").
class_variable(max_nodes,     '1..', 30, "Nodes beyond which no levels are added").

initialise(V) :->
    send_super(V, initialise, idg_viewer),
    send(V, slot, graph, idg([], [])),
    send(V, slot, predicate, none),
    send(V, append_window, new(B, idg_browser)),
    send(V, append_window, new(idg_tabs), B, right),
    send(V, append_window, new(idg_filter_dialog), B, above),
    keep_column_width(B),
    send(V, update_threads),
    send(V, refresh).

pane_label(_V, Label:name) :<-
    "What my tab is called"::
    Label = 'Dependencies'.

menu_bar_key(_V, Key:name) :<-
    "Every IDG viewer asks for the same menu bar"::
    Key = idg_viewer.

fill_menu_bar(V, MD:tool_dialog) :->
    "Put my menu on the bar of the window I am in"::
    get(MD, popup, dependencies, @on, Popup),
    send_list(Popup, append,
              [ menu_item(refresh, message(V, refresh)),
                menu_item(update_threads, message(V, update_threads))
              ]).

browser(V, B:idg_browser) :<-
    get(V, window, idg_browser, B).

focus_window(V, W:idg_focus) :<-
    get(V, window, idg_focus, W).

overview(V, W:idg_overview) :<-
    get(V, window, idg_overview, W).

thread_menu(V, Menu:menu) :<-
    "The menu to select the thread"::
    get(V, window, idg_filter_dialog, D),
    get(D, member, thread, Menu).

%   The threads with IDG nodes.  Initially we show the thread with
%   the most nodes.

update_threads(V) :->
    "Fill the thread menu"::
    get(V, thread_menu, Menu),
    send(Menu, clear),
    idg_threads(Threads),
    forall(member(T-N, Threads),
           ( format(atom(Label), '~w (~D)', [T, N]),
             send(Menu, append, menu_item(T, @default, Label))
           )),
    (   get(V, thread, Current),
        Current \== @nil,
        memberchk(Current-_, Threads)
    ->  send(Menu, selection, Current)
    ;   Threads \== []
    ->  sort(2, @>=, Threads, [Busiest-_|_]),
        send(V, slot, thread, Busiest),
        send(Menu, selection, Busiest)
    ;   send(V, slot, thread, @nil)
    ).

thread(V, Thread:any) :->
    "Show the IDG of another thread"::
    send(V, slot, thread, Thread),
    send(V, slot, predicate, none),
    send(V?overview, slot, expanded, []),
    send(V?overview, slot, auto, []),
    send(V, refresh).

refresh(V) :->
    "Read the IDG of the thread again"::
    get(V, generation, Gen0),
    Gen is Gen0+1,
    send(V, slot, generation, Gen),
    get(V, thread, Thread),
    (   Thread == @nil
    ->  send(V, loaded, Gen, idg([], []))
    ;   thread_create(read_idg(V, Gen, Thread), _, [detached(true)])
    ),
    send(V?overview, recompute).

read_idg(V, Gen, Thread) :-
    catch('$idg_predicate_edges'(Thread, Nodes, Edges), E, true),
    (   var(E)
    ->  in_pce_thread(viewer_send(V, loaded(Gen, idg(Nodes, Edges))))
    ;   in_pce_thread(print_message(error, E))
    ).

%   viewer_send(+Viewer, +Message)
%
%   Send Message to Viewer from another thread; the viewer may have been
%   closed since.

viewer_send(V, Message) :-
    (   object(V),
        send(V, instance_of, prolog_idg_viewer)
    ->  send(V, Message)
    ;   true
    ).

loaded(V, Gen:int, Graph:prolog) :->
    "The IDG per predicate of refresh Gen arrived"::
    (   get(V, generation, Gen)
    ->  send(V, slot, graph, Graph),
        send(V?browser, fill, Graph),
        Graph = idg(Nodes, _),
        length(Nodes, Count),
        send(V, report, status, '%d predicates', Count),
        get(V, predicate, Current),
        (   Current \== none,
            memberchk(node(Current, _, _), Nodes)
        ->  send(V, predicate, Current)
        ;   busiest(Nodes, Busiest)
        ->  send(V, predicate, Busiest)
        ;   send(V, slot, predicate, none),
            send(V?focus_window, show_predicate, none)
        )
    ;   true
    ).

busiest(Nodes, PI) :-
    aggregate_all(max(C, P), member(node(P, C, _), Nodes), max(_, PI)).

predicate(V, PI:prolog) :->
    "Make PI the current predicate"::
    (   get(V, predicate, PI)
    ->  true
    ;   send(V?focus_window, slot, show_all, [])
    ),
    send(V, slot, predicate, PI),
    send(V?browser, current, PI),
    send(V?focus_window, show_predicate, PI),
    send(V?overview, show_predicate, PI).

limits(V, Limits:prolog) :<-
    "Limits for idg_focus_graph/4"::
    maplist(class_value(V),
            [up_depth, down_depth, prune_above, max_relatives, max_nodes],
            [Up, Down, Prune, Max, MaxNodes]),
    Limits = limits(Up, Down, Prune, Max, MaxNodes).

class_value(V, Name, Value) :-
    get(V, class_variable_value, Name, Value).

:- pce_end_class(prolog_idg_viewer).

%   keep_column_width(+Browser)
%
%   Resizing the viewer resizes the graph: the column of the browser
%   neither stretches nor shrinks.  Dragging the gap still resizes it.

keep_column_width(B) :-
    (   get(B, tile, Tile),
        column_tile(Tile, Column, Row)
    ->  send(Column, hor_stretch, 0),
        send(Column, hor_shrink, 0),
        get(Row, members, Members),
        send(Members, for_all,
             if(@arg1 \== Column,
                and(message(@arg1, hor_stretch, 100),
                    message(@arg1, hor_shrink, 100))))
    ;   true
    ).

viewer(Gr, V) :-
    get(Gr, container, prolog_idg_viewer, V).


                 /*******************************
                 *            FILTER            *
                 *******************************/

:- pce_begin_class(idg_filter_dialog, dialog,
                   "Select the thread and filter the predicates").

class_variable(border, size, size(0,0)).

initialise(D) :->
    send_super(D, initialise),
    send(D, gap, size(5, 2)),
    send(D, pen, 0),
    send(D, hor_stretch, 100),
    send(D, hor_shrink, 100),
    send(D, append,
         new(TM, menu(thread, cycle, message(D, thread, @arg1)))),
    send(TM, show_label, @off),
    send(D, append,
         new(F, filter_item(filter, message(D, filter, @arg1),
                            "Filter predicates"))),
    send(F, show_label, @off).

resize(D) :->
    send(D, layout, D?visible?size).

thread(D, Thread:any) :->
    viewer(D, V),
    send(V, thread, Thread).

filter(D, Filter:regex*) :->
    "Only show the predicates that match Filter"::
    viewer(D, V),
    send(V?browser, filter, Filter).

:- pce_end_class(idg_filter_dialog).


                 /*******************************
                 *            BROWSER           *
                 *******************************/

/* All predicates with IDG nodes, most nodes first.  As in the profiler,
<-all_items holds them all and the filter decides which are shown.
*/

:- pce_begin_class(idg_browser, browser,
                   "Predicates involved in incremental tabling").

class_variable(size, size, size(36,20)).
class_variable(max_width, '1..100', 40,
               "Most percentage of the viewer ->fit_width takes").

variable(all_items, chain,   get, "All items, shown or not").
variable(filter,    regex*,  get, "Only show items matching this").
variable(wanted,    int*,    get, "Width wanted by ->fit_width").
variable(applied,   int*,    get, "Width ->apply_width gave my column").
variable(count_width, int*,  get, "Width of the widest count").
variable(current,   prolog,  get, "Current predicate or `none`").

initialise(B) :->
    send_super(B, initialise, predicates),
    send(B, slot, all_items, new(chain)),
    send(B, slot, current, none),
    italic_font(B, Italic),
    send(B, style, invalid, style(colour := red)),
    send(B, style, dynamic, style(font := Italic)),
    send(B, style, invalid_dynamic, style(colour := red, font := Italic)),
    send(B, select_message, message(B, selected, @arg1)).

resize(B) :->
    send_super(B, resize),
    send(B, place_counts),
    send(B, apply_width).

%   The counts go in a column at the right, as wide as the widest count
%   but at least as wide as 000,000.

place_counts(B) :->
    "Put the tab stop of the counts at the right"::
    get(B?text_image, width, W),
    count_width(B, ColW),
    send(B, tab_stops, vector(W-ColW-15)).

count_width(B, ColW) :-
    (   get(B, count_width, ColW),
        ColW \== @nil
    ->  true
    ;   get(B?font, width, '000,000', ColW)
    ).

fill(B, Graph:prolog) :->
    "Show the predicates of Graph"::
    Graph = idg(Nodes, _),
    get(B, all_items, All),
    send(All, clear),
    sort(2, @>=, Nodes, Sorted),
    forall(member(node(PI, C, I), Sorted),
           send(All, append, idg_dict_item(PI, C, I))),
    send(B, show_items),
    send(B, fit_width).

%   As the flat profile, the column holding me and the filter above me
%   is made as wide as the longest predicate and a count, but not wider
%   than max_width of the viewer.

fit_width(B) :->
    "Make my column as wide as my longest predicate"::
    get(B, font, Font),
    new(Max, number(0)),
    send(B?all_items, for_all,
         message(Max, maximum, ?(Font, width, @arg1?name))),
    get(Max, value, NameW),
    new(MaxCount, number(0)),
    send(B?all_items, for_all,
         message(MaxCount, maximum, @arg1?count)),
    get(MaxCount, value, Count),
    format(atom(Widest), '~D', [Count]),
    get(Font, width, Widest, CountW),
    get(Font, width, '000,000', MinW),
    CountWidth is max(CountW, MinW),
    send(B, slot, count_width, CountWidth),
    send(B, place_counts),
    get(Font, width, ' ', SpaceW),
    ColW is CountWidth + SpaceW,
    get(B?scroll_bar, width, SBW),
    Wanted is NameW + ColW + 15 + SBW + 10,
    send(B, slot, wanted, Wanted),
    send(B, slot, applied, @nil),
    send(B, apply_width).

%   The predicates may arrive before the window has its final size, so
%   the wanted width is applied again when I am resized.  It only
%   widens my column, so this does not loop.  Once my column is
%   narrower than the width I gave it, the user made it so: the names
%   are truncated to give the graph more room and I leave it alone.
%   When the viewer is resized, the graph takes the change and my column
%   keeps its width; see `prolog_idg_viewer ->initialise`.

apply_width(B) :->
    "Widen my column to <-wanted, at most max_width of the viewer"::
    (   get(B, wanted, Wanted),
        Wanted \== @nil,
        get(B, tile, Tile),
        column_tile(Tile, Column, Row)
    ->  get(Column?area, width, Current),
        (   get(B, applied, Applied),
            Applied \== @nil,
            Current < Applied
        ->  send(B, slot, wanted, @nil)
        ;   get(Row?area, width, RowW),
            get(B, class_variable_value, max_width, MaxPct),
            Width is min(Wanted, round(RowW*MaxPct/100)),
            (   Current < Width
            ->  send(Column, width, Width),
                send(B, slot, applied, Width)
            ;   true
            )
        )
    ;   true
    ).

%   column_tile(+Tile, -Column, -Row) is semidet.
%
%   Column is the tile that holds Tile and sits in the horizontal Row.

column_tile(Tile, Column, Row) :-
    get(Tile, super, Super),
    Super \== @nil,
    (   get(Super, orientation, horizontal)
    ->  Column = Tile,
        Row = Super
    ;   column_tile(Super, Column, Row)
    ).

filter(B, Filter:regex*) :->
    send(B, slot, filter, Filter),
    send(B, show_items).

show_items(B) :->
    "Show the items of <-all_items that match <-filter"::
    send(B?dict, clear),
    get(B, all_items, All),
    get(B, filter, Filter),
    (   Filter == @nil
    ->  send(All, for_all, message(B, append, @arg1))
    ;   send(All, for_all,
             if(message(Filter, search, @arg1?key),
                message(B, append, @arg1)))
    ),
    get(B, current, PI),
    send(B, current, PI).

current(B, PI:prolog) :->
    "Select the item of PI if it is shown"::
    send(B, slot, current, PI),
    (   PI \== none,
        format(atom(Key), '~q', [PI]),
        get(B, member, Key, DI)
    ->  send(B, selection, DI),
        send(B, normalise, DI)
    ;   send(B, selection, @nil)
    ).

selected(B, DI:idg_dict_item) :->
    "Make the selected predicate the current one"::
    get(DI, predicate, PI),
    viewer(B, V),
    send(V, predicate, PI).

:- pce_end_class(idg_browser).


:- pce_begin_class(idg_dict_item, dict_item,
                   "A predicate in the IDG browser").

variable(predicate, prolog, get, "Predicate indicator").
variable(name,      name,   get, "Name shown without the count").
variable(count,     int,    get, "Number of nodes").

initialise(DI, PI:prolog, Count:int, Invalid:int) :->
    format(atom(Key), '~q', [PI]),
    pi_label(PI, Name),
    format(atom(Label), '~w\t~D', [Name, Count]),
    send_super(DI, initialise, Key, Label),
    send(DI, slot, predicate, PI),
    send(DI, slot, name, Name),
    send(DI, slot, count, Count),
    pi_kind(PI, Kind),
    item_style(Kind, Invalid, Style),
    send(DI, style, Style),
    counted_nodes(Count, Invalid, Counts),
    kind_name(Kind, KindName),
    format(string(Tip), '~q (~w): ~w', [PI, KindName, Counts]),
    send(DI, help_message, tag, Tip).

:- pce_end_class(idg_dict_item).

%   item_style(+Kind, +Invalid, -Style)
%
%   Incremental dynamic predicates are in italics, predicates with
%   invalid nodes in red.

item_style(table,     0, @default) :- !.
item_style(table,     _, invalid).
item_style((dynamic), 0, dynamic) :- !.
item_style((dynamic), _, invalid_dynamic).

%   italic_font(+Window, -Font)
%
%   The italic variant of the font of Window.

italic_font(W, Italic) :-
    get(W, font, Font),
    get(Font, family, Family),
    get(Font, points, Points),
    new(Italic, font(Family, italic, Points)).


                 /*******************************
                 *             TABS             *
                 *******************************/

:- pce_begin_class(idg_tabs, tabbed_window,
                   "Views of the IDG").

initialise(W) :->
    send_super(W, initialise),
    send(W, append, new(idg_focus), focus),
    send(W, append, new(idg_overview), overview).

:- pce_end_class(idg_tabs).


                 /*******************************
                 *          FOCUS GRAPH         *
                 *******************************/

%   busy(+Window, +Busy)
%
%   Show the `progress` cursor, busy in the background, while Window
%   waits for a thread.  The user may go on using the viewer.

busy(W, @on) :-
    send(W, cursor, progress).
busy(W, @off) :-
    get(W, class_variable_value, cursor, Cursor),
    send(W, cursor, Cursor).

/* The graph around the current predicate: what affects it on the left
and what it affects on the right, pruned as the call graph of the
profiler (see idg_focus_graph/4).  Clicking a node makes it the current
predicate.  Clicking the current predicate shows its call patterns as a
cluster, computed by the thread that owns the IDG; clicking the cluster
hides them.  The popup shows the call patterns of any predicate.  They
stay shown as long as the predicate is in the graph.
*/

:- pce_begin_class(idg_focus, xdot_window,
                   "Graph around the current predicate").

variable(names, prolog, get, "Name-Id of the shown nodes").
variable(generation, int := 0, get, "Number of the last pattern request").
variable(patterns, prolog, get, "Predicates that show their call patterns").
variable(show_all, prolog, get, "Dir-PI: show all relatives of PI").

class_variable(natural_zoom, num, 1.5,
               "Zoom of a graph that fits at this scale").

initialise(W) :->
    send_super(W, initialise, @default, focus, size(400,300)),
    send(W, slot, names, []),
    send(W, slot, patterns, []),
    send(W, slot, show_all, []),
    get(W, xdot, X),
    send(X, node_clicked, message(W, clicked, @arg1)),
    send(X, node_popup, new(P, popup)),
    send_list(P, append,
              [ menu_item(focus,
                          message(W, focus_on, @arg1),
                          condition := message(W, can_focus, @arg1),
                          end_group := @on),
                menu_item(show_all,
                          message(W, show_all_relatives, @arg1),
                          'Show them all',
                          condition := message(W, can_show_all, @arg1)),
                menu_item(usual_selection,
                          message(W, usual_selection),
                          'Show the usual selection',
                          condition := message(W, can_usual_selection),
                          end_group := @on),
                menu_item(call_patterns,
                          message(W, call_patterns, @arg1, @on),
                          condition := message(W, can_show_patterns, @arg1)),
                menu_item(hide_call_patterns,
                          message(W, call_patterns, @arg1, @off),
                          condition := message(W, can_hide_patterns, @arg1)),
                menu_item(show_variants,
                          message(W, show_variants, @arg1),
                          condition := message(W, can_show_variants, @arg1),
                          end_group := @on),
                menu_item(edit,
                          message(W, edit, @arg1),
                          condition := message(W, can_edit, @arg1))
              ]).

%   The call patterns may take seconds for a predicate with many
%   variants.  They are computed in a thread while the window shows the
%   graph without them and the `progress` cursor.  Each call increments
%   <-generation, so a result that arrives after the user moved on is
%   dropped.

show_predicate(W, PI:prolog) :->
    "Show the graph around PI"::
    get(W, generation, Gen0),
    Gen is Gen0+1,
    send(W, slot, generation, Gen),
    (   PI == none
    ->  busy(W, @off),
        send(W, load_dot, "digraph idg {}", [], none)
    ;   viewer(W, V),
        get(V, graph, Graph),
        get(V, limits, Limits),
        get(W, show_all, ShowAll),
        idg_focus_graph(Graph, PI, Limits, FocusGraph, [show_all(ShowAll)]),
        FocusGraph = graph(Nodes, _),
        get(W, patterns, Patterns0),
        include([P]>>memberchk(node(pred(P), _, _, _, _), Nodes),
                Patterns0, Patterns),
        send(W, slot, patterns, Patterns),
        (   Patterns \== []
        ->  get(V, thread, Thread),
            busy(W, @on),
            thread_create(focus_patterns(W, Gen, Thread, Patterns,
                                         FocusGraph),
                          _, [detached(true)]),
            send(W, render, FocusGraph, PI)
        ;   busy(W, @off),
            send(W, render, FocusGraph, PI)
        )
    ).

render(W, Graph:prolog, PI:prolog) :->
    "Lay out and show Graph with PI as focus"::
    idg_dot(Graph, Dot, Names, [focus(PI)]),
    send(W, load_dot, Dot, Names, PI).

%   The current predicate stands out by a star behind it, as in the
%   call graph of the profiler.  If it shows its call patterns, it is a
%   cluster and has no star.

load_dot(W, Dot:string, Names:prolog, PI:prolog) :->
    send(W, slot, names, Names),
    get(W, xdot, X),
    send(X, load, Dot),
    (   memberchk(Name-pred(PI), Names),
        \+ sub_atom(Name, 0, _, _, cluster_)
    ->  send(X, star, Name)
    ;   true
    ),
    send(W, fit).

%   focus_patterns(+Window, +Gen, +Thread, +Preds, +FocusGraph)
%
%   Compute the call patterns of the predicates Preds and show each of
%   them as a cluster of its patterns.  Edges between patterns and
%   predicates that are not shown are left out, as are the "more" nodes
%   of Preds: an edge cannot end at a cluster.

focus_patterns(W, Gen, Thread, Preds, graph(Nodes0, Edges0)) :-
    catch(pattern_data(Thread, Preds, PatNodes, PatEdges), E, true),
    (   var(E)
    ->  findall(Id, member(node(Id, _, _, _, _), Nodes0), Shown),
        maplist(pattern_cluster(Nodes0, PatNodes), Preds, Clusters),
        findall(N, ( member(N, Nodes0),
                     N = node(Id, _, _, _, _),
                     \+ ( id_pred(Id, PI), memberchk(PI, Preds) )
                   ),
                Others),
        findall(edge(F, T, EC),
                ( member(edge(F, T, EC), Edges0),
                  \+ ( id_pred(F, PF), memberchk(PF, Preds) ),
                  \+ ( id_pred(T, PT), memberchk(PT, Preds) )
                ), Edges1),
        findall(edge(F, T, EC),
                ( member(edge(F, T, EC), PatEdges),
                  ( F = pattern(_, _) ; memberchk(F, Shown) ),
                  ( T = pattern(_, _) ; memberchk(T, Shown) )
                ), Edges2),
        append(Edges1, Edges2, Edges),
        append(Clusters, Others, Items),
        in_pce_thread(focus_send(W, patterns_ready(Gen, graph(Items, Edges))))
    ;   in_pce_thread(focus_send(W, failed(Gen, E)))
    ).

pattern_cluster(Nodes, PatNodes, PI,
                cluster(pred(PI), Label, C, I, Patterns)) :-
    memberchk(node(pred(PI), Label, C, I, Kind), Nodes),
    findall(node(pattern(PI, P), PL, PC, PIv, Kind),
            ( member(node(pattern(PI, P), PC, PIv), PatNodes),
              format(atom(PL), '~q', [P])
            ), Patterns).

focus_send(W, Message) :-
    (   object(W),
        send(W, instance_of, idg_focus)
    ->  send(W, Message)
    ;   true
    ).

patterns_ready(W, Gen:int, Graph:prolog) :->
    (   get(W, generation, Gen)
    ->  busy(W, @off),
        viewer(W, V),
        get(V, predicate, PI),
        send(W, render, Graph, PI)
    ;   true
    ).

failed(W, Gen:int, Error:prolog) :->
    "Computing for request Gen raised Error"::
    (   get(W, generation, Gen)
    ->  busy(W, @off)
    ;   true
    ),
    print_message(error, Error).

node_id(W, Node:xdot_node, Id:prolog) :<-
    get(Node, name, Name),
    get(W, names, Names),
    memberchk(Name-Id, Names).

%   As in the overview, clicking the current predicate shows its call
%   patterns as a cluster and clicking a cluster or one of its patterns
%   hides them again.  Clicking another node makes its predicate the
%   current one.

clicked(W, Node:xdot_node) :->
    "Show or hide the call patterns or focus on another predicate"::
    get(W, node_id, Node, Id),
    id_pred(Id, PI),
    viewer(W, V),
    (   shows_patterns(W, Node, PI)
    ->  send(W, call_patterns, Node, @off)
    ;   get(V, predicate, PI),
        Id = pred(PI)
    ->  send(W, call_patterns, Node, @on)
    ;   send(V, predicate, PI)
    ).

id_pred(pred(PI), PI).
id_pred(pattern(PI, _), PI).
id_pred(more(_, PI), PI).

%   shows_patterns(+Window, +Node, -PI)
%
%   Node is the cluster of the call patterns of PI or one of them.

shows_patterns(W, Node, PI) :-
    get(W, node_id, Node, Id),
    (   Id = pattern(PI, _)
    ->  true
    ;   Id = pred(PI),
        is_cluster(Node)
    ).

can_focus(W, Node:xdot_node) :->
    get(W, node_id, Node, Id),
    id_pred(Id, PI),
    viewer(W, V),
    \+ get(V, predicate, PI).

focus_on(W, Node:xdot_node) :->
    "Make the predicate of Node the current one"::
    get(W, node_id, Node, Id),
    id_pred(Id, PI),
    viewer(W, V),
    send(V, predicate, PI).

can_show_patterns(W, Node:xdot_node) :->
    get(W, node_id, Node, pred(_)),
    \+ is_cluster(Node).

can_hide_patterns(W, Node:xdot_node) :->
    shows_patterns(W, Node, _).

call_patterns(W, Node:xdot_node, Show:bool) :->
    "Show or hide the call patterns of the predicate of Node"::
    get(W, node_id, Node, Id),
    id_pred(Id, PI),
    (   Show == @on
    ->  send(W, show_patterns, PI)
    ;   send(W, hide_patterns, PI)
    ).

show_patterns(W, PI:prolog) :->
    "Show the call patterns of PI as a cluster"::
    get(W, patterns, Patterns0),
    (   memberchk(PI, Patterns0)
    ->  true
    ;   send(W, slot, patterns, [PI|Patterns0]),
        send(W, redraw_graph)
    ).

hide_patterns(W, PI:prolog) :->
    "Show PI as a node again"::
    get(W, patterns, Patterns0),
    (   selectchk(PI, Patterns0, Patterns)
    ->  send(W, slot, patterns, Patterns),
        send(W, redraw_graph)
    ;   true
    ).

%   A "... N more" node can be replaced by the predicates it stands
%   for.  The choice holds until the focus moves to another predicate.

can_show_all(W, Node:xdot_node) :->
    get(W, node_id, Node, more(_, _)).

show_all_relatives(W, Node:xdot_node) :->
    "Show the predicates a ... N more node stands for"::
    get(W, node_id, Node, more(Dir, PI)),
    get(W, show_all, ShowAll),
    send(W, slot, show_all, [Dir-PI|ShowAll]),
    send(W, redraw_graph).

can_usual_selection(W) :->
    \+ get(W, show_all, []).

usual_selection(W) :->
    "Show the predicates the limits select"::
    send(W, slot, show_all, []),
    send(W, redraw_graph).

redraw_graph(W) :->
    "Show the graph around the current predicate again"::
    viewer(W, V),
    get(V, predicate, Current),
    send(W, show_predicate, Current).

can_edit(W, Node:xdot_node) :->
    get(W, node_id, Node, Id),
    edit_target(Id, _).

edit(W, Node:xdot_node) :->
    get(W, node_id, Node, Id),
    edit_target(Id, Target),
    edit(Target).

can_show_variants(W, Node:xdot_node) :->
    get(W, node_id, Node, Id),
    variants_spec(Id, _).

show_variants(W, Node:xdot_node) :->
    "Browse the variants of the predicate or call pattern"::
    get(W, node_id, Node, Id),
    variants_spec(Id, Spec),
    viewer(W, V),
    get(V, thread, Thread),
    idg_show_variants(Thread, Spec).

:- pce_end_class(idg_focus).


                 /*******************************
                 *           OVERVIEW           *
                 *******************************/

/* The IDG by module and file (see idg_graph/3).  Clicking a group shows
its parts as a cluster; clicking a cluster or a call pattern collapses
it again.  The groups that hold the current predicate are expanded and
the predicate has a star behind it, as in the focus graph.
*/

:- pce_begin_class(idg_overview, xdot_window,
                   "Overview of the IDG by module and file").

variable(expanded,   prolog,   get, "Ids of the refined nodes").
variable(names,      prolog,   get, "Name-Id of the shown nodes").
variable(generation, int := 0, get, "Number of the last refresh started").
variable(predicate,  prolog,   get, "Current predicate or `none`").
variable(auto,       prolog,   get, "Groups expanded for the current predicate").

initialise(W) :->
    send_super(W, initialise, @default, overview, size(400,300)),
    send(W, slot, expanded, []),
    send(W, slot, predicate, none),
    send(W, slot, auto, []),
    send(W, slot, names, []),
    get(W, xdot, X),
    send(X, node_clicked, message(W, clicked, @arg1)),
    send(X, node_popup, new(P, popup)),
    send_list(P, append,
              [ menu_item(focus,
                          message(W, focus_on, @arg1),
                          condition := message(W, can_focus, @arg1),
                          end_group := @on),
                menu_item(expand,
                          message(W, expand, @arg1),
                          condition := message(W, can_expand, @arg1)),
                menu_item(collapse,
                          message(W, collapse, @arg1),
                          condition := message(W, can_collapse, @arg1)),
                menu_item(collapse_all,
                          message(W, collapse_all),
                          end_group := @on),
                menu_item(show_variants,
                          message(W, show_variants, @arg1),
                          condition := message(W, can_show_variants, @arg1),
                          end_group := @on),
                menu_item(edit,
                          message(W, edit, @arg1),
                          condition := message(W, can_edit, @arg1))
              ]).

recompute(W) :->
    "Compute the overview again"::
    get(W, generation, Gen0),
    Gen is Gen0+1,
    send(W, slot, generation, Gen),
    viewer(W, V),
    get(V, thread, Thread),
    get(W, expanded, Expanded),
    (   Thread == @nil
    ->  send(W, show_graph, Gen, "digraph idg {}", [])
    ;   busy(W, @on),
        thread_create(compute_overview(W, Gen, Thread, Expanded), _,
                      [detached(true)])
    ).

compute_overview(W, Gen, Thread, Expanded) :-
    catch(( idg_graph(Thread, Expanded, Graph),
            idg_dot(Graph, Dot, Names)
          ), E, true),
    (   var(E)
    ->  in_pce_thread(overview_send(W, show_graph(Gen, Dot, Names)))
    ;   in_pce_thread(overview_send(W, failed(Gen, E)))
    ).

overview_send(W, Message) :-
    (   object(W),
        send(W, instance_of, idg_overview)
    ->  send(W, Message)
    ;   true
    ).

show_graph(W, Gen:int, Dot:string, Names:prolog) :->
    "Show the overview computed by refresh Gen"::
    (   get(W, generation, Gen)
    ->  busy(W, @off),
        send(W, slot, names, Names),
        get(W, xdot, X),
        send(X, load, Dot),
        send(W, star_predicate),
        send(W, fit)
    ;   true
    ).

failed(W, Gen:int, Error:prolog) :->
    "Computing for refresh Gen raised Error"::
    (   get(W, generation, Gen)
    ->  busy(W, @off)
    ;   true
    ),
    print_message(error, Error).

%   show_predicate(+PI)
%
%   Make sure the groups holding PI are expanded and put the star behind
%   it.  The groups expanded for the previous predicate are collapsed
%   again, unless the user expanded or collapsed them since (see
%   expand/1 and collapse/1).  If nothing changes, only the star moves.

show_predicate(W, PI:prolog) :->
    "Expand the groups of PI and mark it"::
    send(W, slot, predicate, PI),
    (   PI == none
    ->  Groups = []
    ;   pred_groups(PI, Groups)
    ),
    get(W, expanded, Expanded0),
    get(W, auto, Auto0),
    exclude([G]>>(memberchk(G, Auto0), \+ memberchk(G, Groups)),
            Expanded0, Expanded1),
    foldl(open_group, Groups, Expanded1-[], Expanded-Opened),
    include([G]>>memberchk(G, Groups), Auto0, Kept),
    append(Kept, Opened, Auto),
    send(W, slot, auto, Auto),
    (   Expanded == Expanded0
    ->  send(W, star_predicate)
    ;   send(W, slot, expanded, Expanded),
        send(W, recompute)
    ).

%   open_group(+Group, +Expanded0-Opened0, -Expanded-Opened)
%
%   Add Group to the expanded groups.  A module that was collapsed
%   explicitly (see collapse/1) is no longer.  Opened are the groups
%   that were added.

open_group(G, E0-O0, E-O) :-
    (   memberchk(collapsed(G), E0)
    ->  exclude(==(collapsed(G)), E0, E1),
        E = [G|E1],
        O = [G|O0]
    ;   memberchk(G, E0)
    ->  E = E0,
        O = O0
    ;   E = [G|E0],
        O = [G|O0]
    ).

%   The user's own choice for a group: it is no longer one that the
%   viewer opened for the current predicate.

user_choice(W, Group) :-
    get(W, auto, Auto0),
    exclude(==(Group), Auto0, Auto),
    send(W, slot, auto, Auto).

star_predicate(W) :->
    "Put the star behind the current predicate if it is shown"::
    get(W, xdot, X),
    get(W, predicate, PI),
    get(W, names, Names),
    (   PI \== none,
        memberchk(Name-pred(PI), Names),
        \+ sub_atom(Name, 0, _, _, cluster_)
    ->  ignore(send(X, star, Name))
    ;   send(X, unstar)
    ).

collapse_all(W) :->
    "Show the overview at the top level"::
    send(W, slot, expanded, []),
    send(W, slot, auto, []),
    send(W, recompute).

node_id(W, Node:xdot_node, Id:prolog) :<-
    "The id of the graph node or cluster displayed by Node"::
    get(Node, name, Name),
    get(W, names, Names),
    memberchk(Name-Id, Names).

is_cluster(Node) :-
    get(Node, name, Name),
    sub_atom(Name, 0, _, _, cluster_).

clicked(W, Node:xdot_node) :->
    "Expand a node or collapse a cluster"::
    (   send(W, can_expand, Node)
    ->  send(W, expand, Node)
    ;   send(W, can_collapse, Node)
    ->  send(W, collapse, Node)
    ;   true
    ).

can_focus(W, Node:xdot_node) :->
    get(W, node_id, Node, Id),
    id_pred(Id, _).

focus_on(W, Node:xdot_node) :->
    "Make the predicate the current one and show the focus graph"::
    get(W, node_id, Node, Id),
    id_pred(Id, PI),
    viewer(W, V),
    send(V, predicate, PI),
    get(W, container, idg_tabs, Tabs),
    send(Tabs, on_top, focus).

can_expand(W, Node:xdot_node) :->
    \+ is_cluster(Node),
    get(W, node_id, Node, Id),
    expandable(Id).

expandable(module(_)).
expandable(file(_)).
expandable(no_file).
expandable(pred(_)).

expand(W, Node:xdot_node) :->
    "Show the parts of the node"::
    get(W, node_id, Node, Id),
    user_choice(W, Id),
    get(W, expanded, Expanded0),
    exclude(==(collapsed(Id)), Expanded0, Expanded1),
    send(W, slot, expanded, [Id|Expanded1]),
    send(W, recompute).

can_collapse(W, Node:xdot_node) :->
    get(W, node_id, Node, Id),
    collapse_target(Node, Id, _).

collapse(W, Node:xdot_node) :->
    "Show the group of the node, or the cluster, as a node"::
    get(W, node_id, Node, Id),
    collapse_target(Node, Id, Group),
    user_choice(W, Group),
    get(W, expanded, Expanded0),
    exclude(==(Group), Expanded0, Expanded1),
    (   Group = module(_)		% may be expanded as the only one
    ->  Expanded = [collapsed(Group)|Expanded1]
    ;   Expanded = Expanded1
    ),
    send(W, slot, expanded, Expanded),
    send(W, recompute).

%   collapse_target(+Node, +Id, -Group)
%
%   A cluster collapses itself; a node collapses the group it is in.

collapse_target(Node, Id, Id) :-
    is_cluster(Node),
    !.
collapse_target(_, pattern(PI, _), pred(PI)) :- !.
collapse_target(_, pred(PI), Group) :-
    !,
    pred_groups(PI, Groups),
    last(Groups, Group).
collapse_target(_, file(_), module(user)).
collapse_target(_, no_file, module(user)).

can_edit(W, Node:xdot_node) :->
    get(W, node_id, Node, Id),
    edit_target(Id, _).

edit(W, Node:xdot_node) :->
    "Edit the predicate, file or module"::
    get(W, node_id, Node, Id),
    edit_target(Id, Target),
    edit(Target).

can_show_variants(W, Node:xdot_node) :->
    get(W, node_id, Node, Id),
    variants_spec(Id, _).

show_variants(W, Node:xdot_node) :->
    "Browse the variants of the predicate or call pattern"::
    get(W, node_id, Node, Id),
    variants_spec(Id, Spec),
    viewer(W, V),
    get(V, thread, Thread),
    idg_show_variants(Thread, Spec).

:- pce_end_class(idg_overview).

variants_spec(pred(PI), pred(PI)).
variants_spec(pattern(PI, P), pattern(PI, P)).

edit_target(module(M), module(M)).
edit_target(file(File), file(File)).
edit_target(pred(PI), PI).
edit_target(pattern(PI, _), PI).


                 /*******************************
                 *           VARIANTS           *
                 *******************************/

/* The variants (tables or incremental dynamic calls) of a predicate or
call pattern, as a pane of the IDE.  As the bindings of the debugger,
the pane is reused for the next predicate unless it is pinned.

A predicate may have hundreds of thousands of variants, so they are
shown in pages.  A worker thread per pane holds them: it gets the
variant table of the thread that owns them using call_in_thread/3 (see
idg_variant_table/2), collects the variants of the predicate once and
formats and filters a page at a time.  Replies carry the generation of
the request, so those for a filter or predicate that has been replaced
are dropped.
*/

:- dynamic idg_variants_pane/1 as volatile.

%!  idg_show_variants(+Thread, +Spec) is det.
%
%   Show the variants of Spec, pred(PI) or pattern(PI, Pattern), in the
%   IDG of Thread, in the unpinned variants pane or a new one.

idg_show_variants(Thread, Spec) :-
    reusable_variants_pane(P),
    send(P, variants, Thread, Spec),
    send(P, expose).

reusable_variants_pane(P) :-
    idg_variants_pane(P),
    get(P, pinned, @off),
    !.
reusable_variants_pane(P) :-
    new(P, idg_variants).

:- pce_begin_class(idg_variants, tool_pane,
                   "Browse the variants of a predicate").

class_variable(page_size, '1..', 10, "Variants shown initially").

variable(spec,       prolog,   get, "pred(PI) or pattern(PI, Pattern)").
variable(worker,     prolog,   get, "Thread holding the variants or `none`").
variable(generation, int := 0, get, "Number of the last request").
variable(answered,   int := 0, get, "Number of the last request answered").
variable(total,      int*,     get, "Number of variants").
variable(pattern,    prolog,   get, "Text of the filter or `none`").
variable(answers_generation, int := 0, get,
         "Number of the last request for answers").
variable(answers_answered,   int := 0, get,
         "Number of the last request for answers answered").
variable(answers_total,      int*,     get, "Answers of the selected variant").
variable(answers_pattern,    prolog,   get, "Text of the answer filter or `none`").

initialise(P) :->
    send_super(P, initialise, idg_variants),
    send(P, slot, spec, none),
    send(P, slot, worker, none),
    send(P, slot, pattern, none),
    send(P, slot, answers_pattern, none),
    assertz(idg_variants_pane(P)),
    send(P, append_window, new(D, dialog)),
    send(D, name, top),
    send(D, pen, 0),
    send(D, gap, size(5, 5)),
    send(D, append, new(Pin, pin_button('Pin to show the next variants \c
                                         in a new pane'))),
    send(D, append,
         new(FI, filter_item(filter, message(P, filter, @arg1),
                             'Filter variants')),
         right),
    send(FI, show_label, @off),
    center_on(Pin, FI),
    send(D, append,
         new(M, menu(answers, toggle, message(P, apply_filter))), right),
    send(M, show_label, @off),
    send_list(M, append,
              [ menu_item(no_answers, @default, 'No answers'),
                menu_item(answers, @default, 'With answers')
              ]),
    send(M, selected, no_answers, @on),
    send(M, selected, answers, @on),
    send(M, help_message, tag,
         'Show the variants without answers, with answers or both'),
    send(D, append,
         new(AFI, filter_item(answers_filter,
                              message(P, answers_filter, @arg1),
                              'Filter answers')),
         right),
    send(AFI, show_label, @off),
    send(D?tile, ver_stretch, 0),
    send(D?tile, ver_shrink, 0),
    send(P, append_window, new(B, idg_variant_browser), D, below),
    send(P, append_window, new(AB, idg_answer_browser), B, right),
    send(P, append_window, new(Bottom, dialog), B, below),
    paging_dialog(Bottom, bottom, message(P, more, @arg1)),
    send(P, append_window, new(ABottom, dialog), AB, below),
    paging_dialog(ABottom, answers_bottom, message(P, more_answers, @arg1)).

%   paging_dialog(+Dialog, +Name, +Message)
%
%   Fill Dialog with the buttons for the next 10, 100 or 1000 items,
%   which call Message with the number, and a status label.

paging_dialog(D, Name, Message) :-
    send(D, name, Name),
    send(D, pen, 0),
    send(D, gap, size(5, 5)),
    forall(member(N, [10, 100, 1000]),
           ( format(atom(BName), 'next_~d', [N]),
             format(atom(Label), 'Next ~d', [N]),
             send(D, append,
                  button(BName, message(Message, forward, N), Label), right)
           )),
    send(D, append, new(L, label(status, '')), right),
    send(L, alignment, right),
    send(D?tile, ver_stretch, 0),
    send(D?tile, ver_shrink, 0).

unlink(P) :->
    retractall(idg_variants_pane(P)),
    send(P, stop_worker),
    send_super(P, unlink).

pane_label(P, Label:name) :<-
    "What my tab is called"::
    get(P, spec, Spec),
    (   spec_text(Spec, Text)
    ->  format(atom(Label), 'Variants: ~w', [Text])
    ;   Label = 'Variants'
    ).

spec_text(pred(PI), Text) :-
    format(atom(Text), '~q', [PI]).
spec_text(pattern(M:_, Pattern), Text) :-
    format(atom(Text), '~q', [M:Pattern]).

pinned(P, Pinned:bool) :<-
    "@on if I am not to be reused for the next variants"::
    get(P?members, find, @arg1?name == top, D),
    get(D, member, pin, Pin),
    get(Pin, pinned, Pinned).

browser(P, B:idg_variant_browser) :<-
    get(P, window, idg_variant_browser, B).

answer_browser(P, B:idg_answer_browser) :<-
    get(P, window, idg_answer_browser, B).

%   The filter of the answers is in the dialog above both browsers.  It
%   is moved to the start of the answers if they start further right
%   than where the dialog put it, i.e., right of the answer menu.  It
%   keeps the right edge free for the menu icon of the pane.

align_answers_filter(P) :->
    "Put the answer filter above the answers"::
    (   get(P?members, find, @arg1?name == top, D),
        get(D, member, answers_filter, FI),
        get(D, member, answers, M),
        get(P, answer_browser, AB),
        get(AB, area, area(ABX, _, _, _)),
        get(D, area, area(DX, _, _, _)),
        ABX > 0
    ->  get(D, gap, size(GW, _)),
        get(M, right_side, MR),
        X is max(MR + GW, ABX - DX),
        send(FI, x, X),
        get(D?visible, width, DW),
        get(FI, class_variable_value, length, Chars),
        get(FI?value_font, ex, Ex),
        Width is max(60, min(round(Chars*Ex), DW - X - 30)),
        send(FI, value_width, Width)
    ;   true
    ).

variants(P, Thread:any, Spec:prolog) :->
    "Show the variants of Spec in the IDG of Thread"::
    send(P, stop_worker),
    send(P, slot, spec, Spec),
    send(P, slot, total, @nil),
    send(P?browser, clear),
    send(P, status),
    send(P, clear_answers),
    (   get(P, pane_frame, Frame)
    ->  ignore(send(Frame, update_tab_label))
    ;   true
    ),
    get(P, class_variable_value, page_size, N),
    next_generation(P, Gen),
    get(P, filter_term, Filter),
    thread_create(variants_worker(P, Thread, Spec, Gen, Filter, N), Worker,
                  [detached(true)]),
    send(P, slot, worker, Worker),
    busy(P?browser, @on).

stop_worker(P) :->
    (   get(P, worker, Worker),
        Worker \== none
    ->  send(P, slot, worker, none),
        catch(thread_send_message(Worker, stop), error(_,_), true)
    ;   true
    ).

next_generation(P, Gen) :-
    get(P, generation, Gen0),
    Gen is Gen0+1,
    send(P, slot, generation, Gen).

filter(P, Filter:regex*) :->
    "Only show the variants that match Filter"::
    (   Filter == @nil
    ->  Pattern = none
    ;   get(Filter, pattern, Pattern0),
        atom_string(Pattern0, Pattern)
    ),
    send(P, slot, pattern, Pattern),
    send(P, apply_filter).

%   The filter the worker applies: filter(Pattern, Kinds), where
%   Pattern is the text of the regular expression or `none` and Kinds
%   holds `no_answers` and/or `answers`.

filter_term(P, filter(Pattern, Kinds):prolog) :<-
    get(P, pattern, Pattern),
    get(P?members, find, @arg1?name == top, D),
    get(D, member, answers, Menu),
    get(Menu, selection, Chain),
    chain_list(Chain, Kinds).

apply_filter(P) :->
    "Show the variants again with the filter as it is now"::
    send(P?browser, clear),
    get(P, filter_term, Filter),
    get(P, class_variable_value, page_size, N),
    send(P, request, filter(Filter, N)).

more(P, N:int) :->
    "Show the next N variants"::
    send(P, request, page(N)).

request(P, Request:prolog) :->
    (   get(P, worker, Worker),
        Worker \== none
    ->  next_generation(P, Gen),
        busy(P?browser, @on),
        catch(thread_send_message(Worker, request(Gen, Request)),
              error(_,_), true)
    ;   true
    ).

loaded(P, Gen:int, Total:int) :->
    "The worker collected Total variants"::
    (   get(P, generation, Gen)
    ->  send(P, slot, total, Total),
        send(P, status)
    ;   true
    ).

page(P, Gen:int, Lines:prolog, AtEnd:bool) :->
    "A page of variants arrived"::
    (   get(P, generation, Gen)
    ->  busy(P?browser, @off),
        send(P, slot, answered, Gen),
        get(P, browser, B),
        forall(member(Line, Lines), send(B, append_variant, Line)),
        send(P, status, AtEnd)
    ;   true
    ).

failed(P, Gen:int, Error:prolog) :->
    (   get(P, generation, Gen)
    ->  busy(P?browser, @off),
        send(P, slot, answered, Gen)
    ;   true
    ),
    print_message(error, Error).

pending(P) :->
    "Succeed if the last request is not yet answered"::
    get(P, generation, Gen),
    \+ get(P, answered, Gen).

status(P, AtEnd:[bool]) :->
    "Show how many variants are shown"::
    get(P?browser?dict?members, size, Shown),
    get(P, total, Total),
    (   Total == @nil
    ->  format(string(Text), '~D shown', [Shown])
    ;   AtEnd == @on
    ->  format(string(Text), '~D shown; ~D variants, no more matches',
               [Shown, Total])
    ;   format(string(Text), '~D shown; ~D variants', [Shown, Total])
    ),
    paging_status(P, bottom, Text, AtEnd).

paging_status(P, Name, Text, AtEnd) :-
    get(P?members, find, @arg1?name == Name, D),
    get(D, member, status, L),
    send(L, selection, Text),
    forall(member(N, [10, 100, 1000]),
           ( format(atom(BName), 'next_~d', [N]),
             get(D, member, BName, Button),
             (   AtEnd == @off
             ->  send(Button, active, @on)
             ;   send(Button, active, @off)
             )
           )).

%   Selecting a variant shows its answers.  They are enumerated by the
%   worker as the variants: in pages.  Their requests have a generation
%   of their own, so a page of variants and one of answers may be on
%   their way at the same time.

clear_answers(P) :->
    "Show no answers"::
    send(P?answer_browser, clear),
    send(P, slot, answers_total, @nil),
    paging_status(P, answers_bottom, "Select a variant", @default).

select_variant(P, Index:int) :->
    "Show the answers of the variant at Index"::
    get(P, class_variable_value, page_size, N),
    send(P?answer_browser, clear),
    send(P, slot, answers_total, @nil),
    get(P, answers_pattern, Pattern),
    send(P, answers_request, answers(Index, Pattern, N)).

answers_filter(P, Filter:regex*) :->
    "Only show the answers that match Filter"::
    (   Filter == @nil
    ->  Pattern = none
    ;   get(Filter, pattern, Pattern0),
        atom_string(Pattern0, Pattern)
    ),
    send(P, slot, answers_pattern, Pattern),
    (   get(P, answers_total, Total),
        Total \== @nil
    ->  get(P, class_variable_value, page_size, N),
        send(P?answer_browser, clear),
        send(P, answers_request, filter(Pattern, N))
    ;   true
    ).

more_answers(P, N:int) :->
    "Show the next N answers"::
    send(P, answers_request, page(N)).

answers_request(P, Request:prolog) :->
    (   get(P, worker, Worker),
        Worker \== none
    ->  get(P, answers_generation, Gen0),
        Gen is Gen0+1,
        send(P, slot, answers_generation, Gen),
        busy(P?answer_browser, @on),
        catch(thread_send_message(Worker, answers(Gen, Request)),
              error(_,_), true)
    ;   true
    ).

answers_page(P, Gen:int, Total:int, Lines:prolog, AtEnd:bool) :->
    "A page of answers arrived"::
    (   get(P, answers_generation, Gen)
    ->  busy(P?answer_browser, @off),
        send(P, slot, answers_answered, Gen),
        send(P, slot, answers_total, Total),
        get(P, answer_browser, B),
        forall(member(Line, Lines), send(B, append_answer, Line)),
        get(B?dict?members, size, Shown),
        (   get(P, answers_pattern, none)
        ->  format(string(Text), '~D of ~D answers', [Shown, Total])
        ;   AtEnd == @on
        ->  format(string(Text), '~D shown; ~D answers, no more matches',
                   [Shown, Total])
        ;   format(string(Text), '~D shown; ~D answers', [Shown, Total])
        ),
        paging_status(P, answers_bottom, Text, AtEnd)
    ;   true
    ).

answers_failed(P, Gen:int, Error:prolog) :->
    (   get(P, answers_generation, Gen)
    ->  busy(P?answer_browser, @off),
        send(P, slot, answers_answered, Gen)
    ;   true
    ),
    print_message(error, Error).

answers_pending(P) :->
    "Succeed if the last request for answers is not yet answered"::
    get(P, answers_generation, Gen),
    \+ get(P, answers_answered, Gen).

:- pce_end_class(idg_variants).


:- pce_begin_class(idg_variant_browser, browser,
                   "Variants, with their number of answers").

class_variable(size, size, size(50,15)).

initialise(B) :->
    send_super(B, initialise, variants),
    send(B, select_message, message(B, selected, @arg1)),
    send(B, style, invalid, style(colour := red)),
    italic_font(B, Italic),
    send(B, style, incomplete, style(font := Italic)).

resize(B) :->
    send_super(B, resize),
    get(B?text_image, width, W),
    get(B?font, width, '000,000,000', ColW),
    send(B, tab_stops, vector(W-ColW-15)),
    align_answers_filter(B).

selected(B, DI:dict_item) :->
    "Show the answers of the selected variant"::
    get(B, container, idg_variants, P),
    get(DI, object, Index),
    send(P, select_variant, Index).

append_variant(B, Line:prolog) :->
    "Add a variant(Index, Text, Answers, Status, Invalid)"::
    Line = variant(Index, Text, Answers, Status, Invalid),
    (   Answers == none
    ->  Label = Text
    ;   format(string(Label), '~w\t~D', [Text, Answers])
    ),
    (   Invalid == true
    ->  Style = invalid
    ;   Status \== complete, Status \== (dynamic)
    ->  Style = incomplete
    ;   Style = @default
    ),
    send(B, append, dict_item(Text, Label, Index, Style)).

:- pce_end_class(idg_variant_browser).


:- pce_begin_class(idg_answer_browser, browser,
                   "Answers of a variant").

class_variable(size, size, size(40,15)).

initialise(B) :->
    send_super(B, initialise, answers),
    send(B, style, undefined, style(colour := ui_inactive)).

resize(B) :->
    send_super(B, resize),
    align_answers_filter(B).

append_answer(B, Line:prolog) :->
    "Add an answer(Text, TruthValue)"::
    Line = answer(Text, TV),
    (   TV == true
    ->  send(B, append, dict_item(Text))
    ;   format(string(Label), '~w (undefined)', [Text]),
        send(B, append, dict_item(Text, Label, @default, undefined))
    ).

:- pce_end_class(idg_answer_browser).

align_answers_filter(B) :-
    (   get(B, container, idg_variants, P)
    ->  send(P, align_answers_filter)
    ;   true
    ).

%   variants_worker(+Pane, +Thread, +Spec, +Gen, +Filter, +N)
%
%   Collect the variants of Spec in the IDG of Thread, send the first N
%   that pass Filter (see `idg_variants <-filter_term`) to Pane and
%   serve its requests until it sends `stop`.

variants_worker(P, Thread, Spec, Gen, Filter, N) :-
    catch(spec_variants(Thread, Spec, Variants), E, true),
    (   var(E)
    ->  length(Variants, Total),
        pane_send(P, loaded(Gen, Total)),
        matcher(Filter, Matcher),
        worker_page(P, Gen, N, Matcher, Variants, Rest),
        worker_loop(P, Variants, Matcher, Rest, answers(0, [], none, []))
    ;   pane_send(P, failed(Gen, E))
    ).

%   worker_loop(+Pane, +Variants, +Matcher, +Rest, +Answers)
%
%   Rest are the variants not yet considered for a page and Answers is
%   answers(Total, All, Regex, Rest) for the answers of the selected
%   variant, where Regex is the answer filter or `none`.

worker_loop(P, Variants, Matcher, Rest, Answers) :-
    thread_get_message(Msg),
    (   Msg = request(Gen, page(N))
    ->  worker_page(P, Gen, N, Matcher, Rest, Rest1),
        worker_loop(P, Variants, Matcher, Rest1, Answers)
    ;   Msg = request(Gen, filter(Filter, N))
    ->  free_matcher(Matcher),
        matcher(Filter, Matcher1),
        worker_page(P, Gen, N, Matcher1, Variants, Rest1),
        worker_loop(P, Variants, Matcher1, Rest1, Answers)
    ;   Msg = answers(Gen, answers(Index, Pattern, N))
    ->  free_answers(Answers),
        nth0(Index, Variants, _-Variant),
        catch(variant_answers(Variant, All), E, true),
        (   var(E)
        ->  length(All, Total),
            answer_regex(Pattern, Regex),
            answers_page(P, Gen, N, answers(Total, All, Regex, All),
                         Answers1)
        ;   pane_send(P, answers_failed(Gen, E)),
            Answers1 = answers(0, [], none, [])
        ),
        worker_loop(P, Variants, Matcher, Rest, Answers1)
    ;   Msg = answers(Gen, filter(Pattern, N))
    ->  free_answers(Answers),
        Answers = answers(Total, All, _, _),
        answer_regex(Pattern, Regex),
        answers_page(P, Gen, N, answers(Total, All, Regex, All), Answers1),
        worker_loop(P, Variants, Matcher, Rest, Answers1)
    ;   Msg = answers(Gen, page(N))
    ->  answers_page(P, Gen, N, Answers, Answers1),
        worker_loop(P, Variants, Matcher, Rest, Answers1)
    ;   free_matcher(Matcher),
        free_answers(Answers)
    ).

answer_regex(none, none) :- !.
answer_regex(Pattern, R) :-
    new(R, regex(Pattern)).

free_answers(answers(_, _, none, _)) :- !.
free_answers(answers(_, _, R, _)) :-
    free(R).

answers_page(P, Gen, N, answers(Total, All, Regex, Rest0),
             answers(Total, All, Regex, Rest)) :-
    catch(take_answers(N, Regex, Rest0, Lines, Rest), E, true),
    (   var(E)
    ->  (   Rest == []
        ->  AtEnd = @on
        ;   AtEnd = @off
        ),
        pane_send(P, answers_page(Gen, Total, Lines, AtEnd))
    ;   Rest = [],
        pane_send(P, answers_failed(Gen, E))
    ).

take_answers(0, _, Answers, [], Answers) :- !.
take_answers(_, _, [], [], []) :- !.
take_answers(N, Regex, [Head-TV|Answers], Lines, Rest) :-
    variant_text(Head, Text),
    (   text_matches(m(Regex, []), Text)
    ->  Lines = [answer(Text, TV)|Lines1],
        N1 is N-1
    ;   Lines = Lines1,
        N1 = N
    ),
    take_answers(N1, Regex, Answers, Lines1, Rest).

%   variant_answers(+Variant, -Answers)
%
%   Answers is a list Head-TruthValue of the answers of Variant, where
%   TruthValue is `true` or `undefined`.  An answer trie holds
%   ret(V1, ...) for the variables of the variant; for a moded table the
%   moded argument is in the value of the trie node.  We do not use
%   get_returns/2 as it cannot find the variant of another thread.

variant_answers(v(Head, Variant, Moded, Trie), Answers) :-
    (   '$tbl_table_status'(Trie, Status),
        Status == (dynamic)
    ->  Answers = []
    ;   term_variables(Variant, Vars),
        Skeleton =.. [ret|Vars],
        findall(Head-TV,
                ( trie_gen(Trie, Skeleton),
                  answer_delay(Moded, Trie, Skeleton, Delay),
                  (   Delay == true
                  ->  TV = true
                  ;   TV = undefined
                  )
                ), Answers)
    ).

answer_delay(none, Trie, Skeleton, Delay) :-
    '$tbl_answer_dl'(Trie, Skeleton, Delay).
answer_delay(moded(Moded), Trie, Skeleton, Delay) :-
    '$tbl_answer_dl'(Trie, Skeleton, Moded, Delay).

%   matcher(+Filter, -Matcher)
%
%   Matcher is m(Regex, Kinds), where Regex is a regex object or `none`.

matcher(filter(none, Kinds), m(none, Kinds)) :- !.
matcher(filter(Pattern, Kinds), m(R, Kinds)) :-
    new(R, regex(Pattern)).

free_matcher(m(none, _)) :- !.
free_matcher(m(R, _)) :-
    free(R).

worker_page(P, Gen, N, Filter, Variants, Rest) :-
    catch(take_lines(N, Filter, Variants, Lines, Rest), E, true),
    (   var(E)
    ->  (   Rest == []
        ->  AtEnd = @on
        ;   AtEnd = @off
        ),
        pane_send(P, page(Gen, Lines, AtEnd))
    ;   Rest = [],
        pane_send(P, failed(Gen, E))
    ).

take_lines(0, _, Variants, [], Variants) :- !.
take_lines(_, _, [], [], []) :- !.
take_lines(N, Filter, [Index-v(Head, _, _, Trie)|Variants], Lines, Rest) :-
    (   answers_match(Filter, Trie),
        variant_text(Head, Text),
        text_matches(Filter, Text)
    ->  variant_line(Index, Text, Trie, Line),
        Lines = [Line|Lines1],
        N1 is N-1
    ;   Lines = Lines1,
        N1 = N
    ),
    take_lines(N1, Filter, Variants, Lines1, Rest).

%   answers_match(+Matcher, +Trie)
%
%   The variant has no answers or some as Matcher asks.  Calls to an
%   incremental dynamic predicate have no answer table and always pass.

answers_match(m(_, Kinds), Trie) :-
    (   '$tbl_table_status'(Trie, Status),
        Status == (dynamic)
    ->  true
    ;   '$trie_property'(Trie, value_count(Count)),
        (   Count =:= 0
        ->  memberchk(no_answers, Kinds)
        ;   memberchk(answers, Kinds)
        )
    ).

text_matches(m(none, _), _) :- !.
text_matches(m(R, _), Text) :-
    send(R, search, string(Text)).

variant_text(Head, Text) :-
    copy_term(Head, Copy),
    numbervars(Copy, 0, _, [singletons(true)]),
    format(string(Text), '~W', [Copy, [ quoted(true),
                                        numbervars(true),
                                        portray(true),
                                        max_depth(10)
                                      ]]).

%   variant_line(+Index, +Text, +Trie, -Line)
%
%   The trie of another thread is read here: holding its blob keeps
%   it, and reading its properties is safe.

variant_line(Index, Text, Trie,
             variant(Index, Text, Answers, Status, Invalid)) :-
    (   '$tbl_table_status'(Trie, Status0)  % /4 fails in another thread
    ->  Status = Status0
    ;   Status = unknown
    ),
    (   Status == (dynamic)
    ->  Answers = none
    ;   '$trie_property'(Trie, value_count(Answers0))
    ->  Answers = Answers0
    ;   Answers = none
    ),
    (   '$idg_falsecount'(Trie, FC),
        FC > 0
    ->  Invalid = true
    ;   Invalid = false
    ).

pane_send(P, Message) :-
    in_pce_thread(variants_send(P, Message)).

variants_send(P, Message) :-
    (   object(P),
        send(P, instance_of, idg_variants)
    ->  send(P, Message)
    ;   true
    ).

%   spec_variants(+Thread, +Spec, -Variants)
%
%   Variants is a list Index-v(Head, Variant, Moded, Trie) of the
%   variants of Spec in the IDG of Thread, in the order of the variant
%   table.  Variant is the variant of the wrapper, which shares its
%   variables with Head, and Moded is moded(Arg) for a moded table.

spec_variants(Thread, Spec, Variants) :-
    idg_variant_table(Thread, VT),
    spec_pi(Spec, M:Name/Arity),
    findall(v(Head, Variant, Moded, Trie),
            ( trie_gen(VT, M:Variant, Trie),
              variant_head(M, Variant, Head, Moded),
              functor(Head, Name, Arity),
              spec_matches(Spec, Head)
            ), Variants0),
    foldl(number_variant, Variants0, Variants, 0, _).

number_variant(V, I-V, I, I1) :-
    I1 is I+1.

variant_head(M, Variant, Head, Moded) :-
    (   catch(M:'$table_mode'(Head0, Variant, Moded0), error(_,_), fail)
    ->  Head = Head0,
        '$tbl_trienode'(Reserved),
        (   Moded0 == Reserved
        ->  Moded = none
        ;   Moded = moded(Moded0)
        )
    ;   Head = Variant,
        Moded = none
    ).

spec_pi(pred(PI), PI).
spec_pi(pattern(PI, _), PI).

spec_matches(pred(_), _).
spec_matches(pattern(_, Pattern), Head) :-
    call_pattern(Head, Pattern).

%   idg_variant_table(+Thread, -VT)
%
%   VT is the variant table of Thread or of the shared tables.  Only a
%   thread itself can get its variant table, so that of another thread
%   is fetched using call_in_thread/3 and inspected by the caller.

idg_variant_table(shared, VT) :-
    !,
    '$tbl_global_variant_table'(VT).
idg_variant_table(Thread, VT) :-
    (   thread_self(Me),
        same_thread(Me, Thread)
    ->  '$tbl_local_variant_table'(VT)
    ;   call_in_thread(Thread, '$tbl_local_variant_table'(VT),
                       [timeout(10)])
    ).
