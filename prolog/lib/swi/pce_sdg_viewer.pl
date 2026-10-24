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


:- module(pce_sdg_viewer,
          [ sdg_snapshot/2,                     % +Thread, -SDG
            sdg_graph/3,                        % +SDG, +Options, -Graph
            sdg_dot/3                           % +Graph, -Dot, -Names
          ]).
:- use_module(library(pce)).
:- use_module(library(pane_frame)).
:- use_module(library(xdot)).
:- autoload(library(apply), [maplist/3, foldl/4]).
:- autoload(library(lists), [member/2, sum_list/2, append/3, subtract/3]).
:- autoload(library(pairs), [map_list_to_pairs/3, group_pairs_by_key/2]).
:- autoload(library(option), [option/3]).
:- autoload(library(thread), [call_in_thread/3]).
:- autoload(library(edit), [edit/1]).
:- autoload(library(swi/pce_idg_viewer), [idg_show_variants/2]).

/** <module> Show the subgoal dependency graph of a thread

The SDG answers "why does this not complete?": it shows the incomplete
tables of a thread, the components (SCCs) they belong to and which
tables wait for answers of which (see '$tbl_sdg'/3).  The data is
thread-owned, so it is read by the thread itself using call_in_thread/3.

The graph has two levels.  At the level of variants the nodes are the
tables and the clusters the components, nested as the components are.
At the level of predicates the nodes are the predicates of the tables;
a predicate can be expanded into a cluster of its tables.  Components
are only shown at the level of variants, as the tables of a predicate
may be in different components.
*/

%!  sdg_snapshot(+Thread, -SDG) is det.
%
%   SDG is sdg(Components, Tables, Edges) as '$tbl_sdg'/3 of Thread.
%   Raises time_limit_exceeded if Thread does not respond within 5
%   seconds, e.g., because it is blocked in a foreign call.

sdg_snapshot(Thread, sdg(Components, Tables, Edges)) :-
    call_in_thread(Thread, '$tbl_sdg'(Components, Tables, Edges),
                   [timeout(5)]).

%!  sdg_graph(+SDG, +Options, -Graph) is det.
%
%   Graph is graph(Items, Edges) for sdg_dot/3.  Items are
%
%     - node(Id, Info), where Id is table(Trie) with Info
%       table(Variant, Answers, Flags, Leader) or pred(PI) with Info
%       pred(Tables, Answers, Flags).
%     - cluster(Id, Info, Children), where Id is scc(SCC) with Info
%       scc(Tables, Leader), or pred(PI), an expanded predicate.
%
%   Edges are edge(From, To, Sign, Count), where Count is the number of
%   suspensions of From on To and Sign is `positive` or `negative`.
%   Options:
%
%     - level(+Level)
%       `variants` (default) or `predicates`.
%     - expanded(+PIs)
%       At the level of predicates, the predicates shown as a cluster of
%       their tables.

sdg_graph(sdg(Cs, Ts, Es), Options, graph(Items, Edges)) :-
    option(level(Level), Options, variants),
    option(expanded(Expanded), Options, []),
    level_graph(Level, Cs, Ts, Es, Expanded, Items, Edges).

level_graph(variants, Cs, Ts, Es, _, Items, Edges) :-
    findall(Item,
            ( member(scc(SCC, null, _), Cs),
              component_item(SCC, Cs, Ts, Item)
            ), Items),
    maplist(table_edge, Es, Edges).
level_graph(predicates, Cs, Ts, Es, Expanded, Items, Edges) :-
    leaders(Cs, Leaders),
    map_list_to_pairs(table_pi, Ts, Pairs0),
    keysort(Pairs0, Pairs),
    group_pairs_by_key(Pairs, Groups),
    maplist(pred_item(Expanded, Leaders), Groups, Items),
    foldl(table_rep(Expanded), Ts, Reps0, []),
    sort(Reps0, Reps),
    findall(edge(F, T, S)-C,
            ( member(edge(From, To, S, C), Es),
              memberchk(From-F, Reps),
              memberchk(To-T, Reps)
            ), EdgePairs0),
    keysort(EdgePairs0, EdgePairs),
    group_pairs_by_key(EdgePairs, EdgeGroups),
    maplist(sum_edge, EdgeGroups, Edges).

table_edge(edge(From, To, Sign, Count),
           edge(table(From), table(To), Sign, Count)).

sum_edge(edge(F, T, S)-Counts, edge(F, T, S, Count)) :-
    sum_list(Counts, Count).

%   component_item(+SCC, +Components, +Tables, -Item) is semidet.
%
%   A component with one table and no children is just its table.  It
%   is then inside the cluster of its parent, if any.  Fails for a
%   component without tables and children.

component_item(SCC, Cs, Ts, Item) :-
    memberchk(scc(SCC, _, Leader), Cs),
    findall(Child,
            ( member(scc(C, SCC, _), Cs),
              component_item(C, Cs, Ts, Child)
            ), Children),
    findall(node(table(Trie), Info),
            ( member(T, Ts),
              T = table(Trie, _, SCC, _, _),
              table_info(T, Leader, Info)
            ), Nodes),
    (   Nodes = [Node], Children == []
    ->  Item = Node
    ;   Nodes == [], Children == []
    ->  fail
    ;   length(Nodes, Count),
        leader_variant(Leader, Ts, LeaderVariant),
        append(Nodes, Children, Members),
        Item = cluster(scc(SCC), scc(Count, LeaderVariant), Members)
    ).

table_info(table(Trie, Variant, _, Answers, Flags), Leader,
           table(Variant, Answers, Flags, IsLeader)) :-
    (   Trie == Leader
    ->  IsLeader = true
    ;   IsLeader = false
    ).

leader_variant(Leader, Ts, Variant) :-
    (   memberchk(table(Leader, Variant, _, _, _), Ts)
    ->  true
    ;   Variant = none
    ).

leaders(Cs, Leaders) :-
    findall(L, member(scc(_, _, L), Cs), Leaders).

table_pi(table(_, Variant, _, _, _), PI) :-
    variant_pi(Variant, PI).

variant_pi(M:Head, M:Name/Arity) :-
    !,
    functor(Head, Name, Arity).
variant_pi(Head, user:Name/Arity) :-
    functor(Head, Name, Arity).

pred_item(Expanded, Leaders, PI-Tables, Item) :-
    length(Tables, Count),
    foldl(add_answers, Tables, 0, Answers),
    foldl(add_flags, Tables, [], Flags),
    Info = pred(Count, Answers, Flags),
    (   memberchk(PI, Expanded)
    ->  maplist(table_node(Leaders), Tables, Nodes),
        Item = cluster(pred(PI), Info, Nodes)
    ;   Item = node(pred(PI), Info)
    ).

add_answers(table(_, _, _, A, _), A0, A1) :-
    A1 is A0+A.

add_flags(table(_, _, _, _, F), F0, F1) :-
    append(F, F0, F2),
    sort(F2, F1).

table_node(Leaders, T, node(table(Trie), Info)) :-
    T = table(Trie, _, _, _, _),
    (   memberchk(Trie, Leaders)
    ->  Leader = Trie
    ;   Leader = null
    ),
    table_info(T, Leader, Info).

%   table_rep(+Expanded, +Table)//
%
%   The item that shows Table: itself if its predicate is expanded,
%   else its predicate.

table_rep(Expanded, T, [Trie-Rep|Tail], Tail) :-
    T = table(Trie, _, _, _, _),
    table_pi(T, PI),
    (   memberchk(PI, Expanded)
    ->  Rep = table(Trie)
    ;   Rep = pred(PI)
    ).


                 /*******************************
                 *              DOT             *
                 *******************************/

%!  sdg_dot(+Graph, -Dot, -Names) is det.
%
%   Dot is the graphviz text of Graph and Names a list Name-Id of its
%   nodes and clusters.  Negative edges are red and dashed, the size of
%   a table grows with its answers, the leader of a component has a
%   double outline and a table with work left has a thick outline.

sdg_dot(graph(Items, Edges), Dot, Names) :-
    item_names(Items, 1, _, Names, []),
    with_output_to(
        string(Dot),
        ( format('digraph sdg {~n'),
          format('  rankdir=LR;~n'),
          format('  node [fontname="sans-serif", fontsize=10];~n'),
          format('  edge [arrowsize=0.6];~n'),
          forall(member(Item, Items), dot_item(Item, Names, 1)),
          forall(member(Edge, Edges), dot_edge(Edge, Names)),
          format('}~n')
        )).

item_names([], N, N, Names, Names).
item_names([Item|Items], N0, N, [Name-Id|Names0], Tail) :-
    N1 is N0+1,
    (   Item = cluster(Id, _, Children)
    ->  format(atom(Name), 'cluster_~d', [N0]),
        item_names(Children, N1, N2, Names0, Names1)
    ;   Item = node(Id, _),
        format(atom(Name), 'n~d', [N0]),
        N2 = N1,
        Names1 = Names0
    ),
    item_names(Items, N2, N, Names1, Tail).

dot_item(node(Id, Info), Names, Indent) :-
    memberchk(Name-Id, Names),
    node_label(Id, Info, Label),
    node_tooltip(Id, Info, Tooltip),
    node_attributes(Info, Attrs),
    format('~t~*|  ~w [label=~q, tooltip=~q~w];~n',
           [Indent, Name, Label, Tooltip, Attrs]).
dot_item(cluster(Id, Info, Children), Names, Indent) :-
    memberchk(Name-Id, Names),
    cluster_label(Id, Info, Label),
    cluster_tooltip(Id, Info, Tooltip),
    format('~t~*|  subgraph ~w {~n', [Indent, Name]),
    format('~t~*|    label=~q; tooltip=~q; style=rounded; color="#909090"; \c
            fontname="sans-serif"; fontsize=10;~n',
           [Indent, Label, Tooltip]),
    Indent1 is Indent+2,
    forall(member(Item, Children), dot_item(Item, Names, Indent1)),
    format('~t~*|  }~n', [Indent]).

node_label(table(_), table(Variant, Answers, _, _), Label) :-
    variant_label(Variant, VL),
    answers(Answers, AL),
    format(string(Label), '~w\n~w', [VL, AL]).
node_label(pred(PI), pred(Tables, Answers, _), Label) :-
    pi_label(PI, PL),
    tables(Tables, TL),
    answers(Answers, AL),
    format(string(Label), '~w\n~w, ~w', [PL, TL, AL]).

cluster_label(scc(_), scc(Tables, _), Label) :-
    tables(Tables, TL),
    format(string(Label), 'SCC: ~w', [TL]).
cluster_label(pred(PI), pred(Tables, Answers, _), Label) :-
    pi_label(PI, PL),
    tables(Tables, TL),
    answers(Answers, AL),
    format(string(Label), '~w: ~w, ~w', [PL, TL, AL]).

node_tooltip(table(_), table(Variant, Answers, Flags, Leader), Tooltip) :-
    variant_text(Variant, VT),
    answers(Answers, AL),
    (   Leader == true
    ->  Flags1 = [leader|Flags]
    ;   Flags1 = Flags
    ),
    flags_text(Flags1, FT),
    format(string(Tooltip), '~w: ~w~w', [VT, AL, FT]).
node_tooltip(pred(PI), pred(Tables, Answers, Flags), Tooltip) :-
    tables(Tables, TL),
    answers(Answers, AL),
    flags_text(Flags, FT),
    format(string(Tooltip), '~q: ~w, ~w~w (click to show the tables)',
           [PI, TL, AL, FT]).

cluster_tooltip(scc(_), scc(Tables, Leader), Tooltip) :-
    tables(Tables, TL),
    (   Leader == none
    ->  format(string(Tooltip), 'Incomplete component of ~w', [TL])
    ;   variant_text(Leader, LT),
        format(string(Tooltip),
               'Incomplete component of ~w, led by ~w', [TL, LT])
    ).
cluster_tooltip(pred(PI), pred(Tables, Answers, _), Tooltip) :-
    tables(Tables, TL),
    answers(Answers, AL),
    format(string(Tooltip), '~q: ~w, ~w (click to collapse)',
           [PI, TL, AL]).

flags_text([], "") :- !.
flags_text(Flags, Text) :-
    maplist(flag_text, Flags, Texts),
    atomic_list_concat(Texts, ', ', Text0),
    format(string(Text), '; ~w', [Text0]).

flag_text(leader,    'leads its component').
flag_text(pending,   'has work left').
flag_text(executing, 'executing').
flag_text(negative,  'waited for by tnot/1').
flag_text(delays,    'has conditional answers').

node_attributes(table(_, Answers, Flags, Leader), Attrs) :-
    Size is log10(1+Answers),
    Width is 0.75 + 0.5*Size,
    Height is 0.4 + 0.2*Size,
    format(string(Shape), ', shape=ellipse, width=~2f, height=~2f',
           [Width, Height]),
    flag_attributes(Flags, Leader, [], Shape, Attrs).
node_attributes(pred(_, _, Flags), Attrs) :-
    flag_attributes(Flags, false, [rounded], ", shape=box", Attrs).

%   flag_attributes(+Flags, +Leader, +Styles, +Shape, -Attrs)

flag_attributes(Flags, Leader, Styles0, Shape, Attrs) :-
    (   memberchk(delays, Flags)
    ->  append(Styles0, [filled], Styles)
    ;   Styles = Styles0
    ),
    findall(A, flag_attribute(Flags, Leader, Styles, A), As),
    atomic_list_concat([Shape|As], Attrs0),
    atom_string(Attrs0, Attrs).

flag_attribute(_, _, Styles, A) :-
    Styles \== [],
    atomic_list_concat(Styles, ',', Style),
    format(atom(A), ', style="~w"', [Style]).
flag_attribute(Flags, _, _, ', fillcolor="#fff2b3"') :-
    memberchk(delays, Flags).
flag_attribute(_, true, _, ', peripheries=2').
flag_attribute(Flags, _, _, ', penwidth=2') :-
    memberchk(pending, Flags).
flag_attribute(Flags, _, _, ', color="#c04040"') :-
    memberchk(negative, Flags).

dot_edge(edge(From, To, Sign, Count), Names) :-
    memberchk(F-From, Names),
    memberchk(T-To, Names),
    Width is min(6.0, 1.0 + log10(Count)),
    (   Count == 1
    ->  What = suspension
    ;   What = suspensions
    ),
    (   Sign == negative
    ->  Style = ', style=dashed, color="#c04040"',
        Kind = ' (tnot)'
    ;   Style = '',
        Kind = ''
    ),
    format(string(Tooltip), '~D ~w~w', [Count, What, Kind]),
    format('  ~w -> ~w [penwidth=~2f, tooltip=~q~w];~n',
           [F, T, Width, Tooltip, Style]).

tables(1, "1 table") :- !.
tables(N, Text) :- format(string(Text), '~D tables', [N]).

answers(1, "1 answer") :- !.
answers(N, Text) :- format(string(Text), '~D answers', [N]).

pi_label(user:PI, Label) :-
    !,
    format(string(Label), '~q', [PI]).
pi_label(PI, Label) :-
    format(string(Label), '~q', [PI]).

%   variant_text(+Variant, -Text)
%
%   Variant as a goal, without the module if it is `user`.

variant_text(Variant0, Text) :-
    (   Variant0 = user:Variant
    ->  true
    ;   Variant = Variant0
    ),
    copy_term(Variant, V),
    numbervars(V, 0, _, [singletons(true)]),
    format(string(Text), '~W',
           [V, [quoted(true), numbervars(true), portray(true),
                max_depth(10)]]).

%   variant_label(+Variant, -Label)
%
%   As variant_text/2, cut to 40 characters.

variant_label(Variant, Label) :-
    variant_text(Variant, Text),
    (   string_length(Text, Len),
        Len > 40
    ->  sub_string(Text, 0, 39, _, Prefix),
        string_concat(Prefix, "…", Label)
    ;   Label = Text
    ).


                 /*******************************
                 *             PANE             *
                 *******************************/

/* The viewer is a pane of the IDE: a dialog to select the thread and the
level above the graph.  A refresh takes a snapshot in a thread of its
own, which waits for the thread whose SDG is shown; the graph shows the
`progress` cursor meanwhile.  A snapshot that arrives after a newer
refresh started is dropped.
*/

:- pce_begin_class(prolog_sdg_viewer, tool_pane,
                   "Show the subgoal dependency graph of a thread").

variable(thread,     any*,     get, "Thread whose SDG is shown").
variable(sdg,        prolog,   get, "The last snapshot").
variable(graph_level, {auto,variants,predicates} := auto, get,
         "Level of the graph").
variable(expanded,   prolog,   get, "Predicates shown as their tables").
variable(generation, int := 0, get, "Number of the last refresh started").

class_variable(max_variants, '1..', 60,
               "Most tables shown as variants if the level is auto").

initialise(V, Thread:[prolog]) :->
    send_super(V, initialise, sdg_viewer),
    send(V, slot, sdg, sdg([], [], [])),
    send(V, slot, expanded, []),
    send(V, append_window, new(G, sdg_window)),
    send(V, append_window, new(sdg_dialog), G, above),
    default(Thread, main, TheThread),
    send(V, update_threads),
    send(V, thread, TheThread).

pane_label(_V, Label:name) :<-
    "What my tab is called"::
    Label = 'Incomplete tables'.

menu_bar_key(_V, Key:name) :<-
    "Every SDG viewer asks for the same menu bar"::
    Key = sdg_viewer.

fill_menu_bar(V, MD:tool_dialog) :->
    "Put my menu on the bar of the window I am in"::
    get(MD, popup, tables, @on, Popup),
    send_list(Popup, append,
              [ menu_item(refresh, message(V, refresh)),
                menu_item(update_threads, message(V, update_threads))
              ]).

graph_window(V, W:sdg_window) :<-
    get(V, window, sdg_window, W).

dialog(V, D:sdg_dialog) :<-
    get(V, window, sdg_dialog, D).

update_threads(V) :->
    "Fill the thread menu"::
    get(V?dialog, member, thread, Menu),
    send(Menu, clear),
    forall(sdg_thread(T),
           send(Menu, append, menu_item(T, @default, T))),
    (   get(V, thread, Current),
        Current \== @nil
    ->  ignore(send(Menu, selection, Current))
    ;   true
    ).

%   thread_name(+Thread, -Name)
%
%   Name is the alias or id of Thread, which may be a thread handle.
%   A thread that no longer exists keeps its alias or id, or the handle
%   of a joined thread its printed form: a pce slot holds no handle.

thread_name(Thread, Name) :-
    (   catch(thread_property(Thread, alias(Alias)), _, fail)
    ->  Name = Alias
    ;   catch(thread_property(Thread, id(Id)), _, fail)
    ->  Name = Id
    ;   blob(Thread, thread)
    ->  format(atom(Name), '~p', [Thread])
    ;   Name = Thread
    ).

%   sdg_thread(-Thread)
%
%   The threads that may have incomplete tables, by alias or id.

sdg_thread(Thread) :-
    thread_property(T, status(running)),
    \+ thread_property(T, alias(gc)),
    (   thread_property(T, alias(Thread))
    ->  true
    ;   thread_property(T, id(Thread))
    ).

thread(V, Thread0:prolog) :->
    "Show the SDG of another thread (alias, id or handle)"::
    thread_name(Thread0, Thread),
    send(V, slot, thread, Thread),
    send(V, slot, expanded, []),
    get(V?dialog, member, thread, Menu),
    ignore(send(Menu, selection, Thread)),
    send(V, refresh).

graph_level(V, Level:{auto,variants,predicates}) :->
    "Show the graph at another level"::
    send(V, slot, graph_level, Level),
    get(V?dialog, member, level, Menu),
    send(Menu, selection, Level),
    send(V, redraw).

refresh(V) :->
    "Take a new snapshot of the SDG"::
    get(V, generation, Gen0),
    Gen is Gen0+1,
    send(V, slot, generation, Gen),
    get(V, thread, Thread),
    send(V?dialog, status, 'Reading ...'),
    busy(V?graph_window, @on),
    thread_create(read_sdg(V, Gen, Thread), _, [detached(true)]).

read_sdg(V, Gen, Thread) :-
    catch(sdg_snapshot(Thread, SDG), E, true),
    (   var(E)
    ->  in_pce_thread(viewer_send(V, loaded(Gen, SDG)))
    ;   in_pce_thread(viewer_send(V, failed(Gen, E)))
    ).

%   viewer_send(+Viewer, +Message)
%
%   Send Message to Viewer from another thread; the viewer may have been
%   closed since.

viewer_send(V, Message) :-
    (   object(V),
        send(V, instance_of, prolog_sdg_viewer)
    ->  send(V, Message)
    ;   true
    ).

loaded(V, Gen:int, SDG:prolog) :->
    "The snapshot of refresh Gen arrived"::
    (   get(V, generation, Gen)
    ->  busy(V?graph_window, @off),
        send(V, slot, sdg, SDG),
        SDG = sdg(Cs, Ts, _),
        length(Cs, NC),
        length(Ts, NT),
        (   NT == 1
        ->  TT = "1 incomplete table"
        ;   format(string(TT), '~D incomplete tables', [NT])
        ),
        (   NC == 1
        ->  CT = "1 SCC"
        ;   format(string(CT), '~D SCCs', [NC])
        ),
        format(string(Status), '~w in ~w', [TT, CT]),
        send(V?dialog, status, Status),
        send(V, redraw)
    ;   true
    ).

failed(V, Gen:int, Error:prolog) :->
    "Refresh Gen raised Error"::
    (   get(V, generation, Gen)
    ->  busy(V?graph_window, @off),
        get(V, thread, Thread),
        failed_status(Error, Thread, Status),
        send(V?dialog, status, Status)
    ;   true
    ).

failed_status(time_limit_exceeded, Thread, Status) :-
    !,
    format(string(Status), 'Thread ~w does not respond', [Thread]).
failed_status(Error, Thread, Status) :-
    print_message(error, Error),
    format(string(Status), 'Cannot read the tables of ~w', [Thread]).

%   The level that is shown: at level auto, a large SDG is shown by
%   predicate.

shown_level(V, Level:{variants,predicates}) :<-
    "Level of the graph that is shown"::
    get(V, graph_level, Level0),
    (   Level0 == auto
    ->  get(V, sdg, sdg(_, Ts, _)),
        length(Ts, N),
        get(V, class_variable_value, max_variants, Max),
        (   N =< Max
        ->  Level = variants
        ;   Level = predicates
        )
    ;   Level = Level0
    ).

redraw(V) :->
    "Show the last snapshot"::
    get(V, sdg, SDG),
    get(V, shown_level, Level),
    get(V, expanded, Expanded),
    sdg_graph(SDG, [level(Level), expanded(Expanded)], Graph),
    sdg_dot(Graph, Dot, Names),
    send(V?graph_window, show, Dot, Names).

expand(V, PI:prolog, Expand:bool) :->
    "Show PI as its tables or as a predicate"::
    get(V, expanded, Expanded0),
    (   Expand == @on
    ->  Expanded = [PI|Expanded0]
    ;   subtract(Expanded0, [PI], Expanded)
    ),
    send(V, slot, expanded, Expanded),
    send(V, redraw).

:- pce_end_class(prolog_sdg_viewer).

viewer(Gr, V) :-
    get(Gr, container, prolog_sdg_viewer, V).

%   busy(+Window, +Busy)
%
%   Show the `progress` cursor while Window waits for a snapshot.

busy(W, @on) :-
    send(W, cursor, progress).
busy(W, @off) :-
    get(W, class_variable_value, cursor, Cursor),
    send(W, cursor, Cursor).


                 /*******************************
                 *            DIALOG            *
                 *******************************/

:- pce_begin_class(sdg_dialog, dialog,
                   "Select the thread and level of the SDG").

class_variable(border, size, size(0,0)).

initialise(D) :->
    send_super(D, initialise),
    send(D, gap, size(5, 2)),
    send(D, pen, 0),
    send(D, hor_stretch, 100),
    send(D, hor_shrink, 100),
    send(D, ver_stretch, 0),
    send(D, append,
         new(TM, menu(thread, cycle, message(D, thread, @arg1)))),
    send(TM, show_label, @off),
    send(D, append,
         new(LM, menu(level, cycle, message(D, graph_level, @arg1))),
         right),
    send(LM, show_label, @off),
    send_list(LM, append,
              [ menu_item(auto, @default, 'Auto'),
                menu_item(variants, @default, 'Variants'),
                menu_item(predicates, @default, 'Predicates')
              ]),
    send(D, append, button(refresh, message(D, refresh)), right),
    send(D, append, new(S, label(status, '')), right),
    send(S, length, 0).

resize(D) :->
    send(D, layout, D?visible?size).

thread(D, Thread:any) :->
    viewer(D, V),
    send(V, thread, Thread).

graph_level(D, Level:name) :->
    viewer(D, V),
    send(V, graph_level, Level).

refresh(D) :->
    viewer(D, V),
    send(V, refresh).

status(D, Text:char_array) :->
    "Show what the graph shows"::
    get(D, member, status, S),
    send(S, selection, Text).

:- pce_end_class(sdg_dialog).


                 /*******************************
                 *             GRAPH            *
                 *******************************/

/* At the level of predicates, clicking a predicate shows its tables as
a cluster and clicking the cluster or one of its tables shows the
predicate again.
*/

:- pce_begin_class(sdg_window, xdot_window,
                   "The graph of the SDG").

variable(names, prolog, get, "Name-Id of the shown nodes").

class_variable(natural_zoom, num, 1.5,
               "Zoom of a graph that fits at this scale").

initialise(W) :->
    send_super(W, initialise, @default, sdg, size(500,350)),
    send(W, slot, names, []),
    get(W, xdot, X),
    send(X, node_clicked, message(W, clicked, @arg1)),
    send(X, node_popup, new(P, popup)),
    send_list(P, append,
              [ menu_item(show_tables,
                          message(W, expand, @arg1, @on),
                          condition := message(W, can_expand, @arg1)),
                menu_item(show_predicate,
                          message(W, expand, @arg1, @off),
                          condition := message(W, can_collapse, @arg1),
                          end_group := @on),
                menu_item(show_variants,
                          message(W, show_variants, @arg1),
                          condition := message(W, can_edit, @arg1)),
                menu_item(edit,
                          message(W, edit, @arg1),
                          condition := message(W, can_edit, @arg1))
              ]).

show(W, Dot:string, Names:prolog) :->
    "Show a new graph"::
    send(W, slot, names, Names),
    send(W?xdot, load, Dot),
    send(W, fit).

node_id(W, Node:xdot_node, Id:prolog) :<-
    get(Node, name, Name),
    get(W, names, Names),
    memberchk(Name-Id, Names).

%   node_pi(+Window, +Node, -PI)
%
%   PI is the predicate shown by Node: a predicate or a table.

node_pi(W, Node:xdot_node, PI:prolog) :<-
    get(W, node_id, Node, Id),
    id_pi(W, Id, PI).

id_pi(_, pred(PI), PI).
id_pi(W, table(Trie), PI) :-
    viewer(W, V),
    get(V, sdg, sdg(_, Ts, _)),
    memberchk(table(Trie, Variant, _, _, _), Ts),
    variant_pi(Variant, PI).

clicked(W, Node:xdot_node) :->
    "Show the tables of a predicate or the predicate of a table"::
    (   send(W, can_expand, Node)
    ->  send(W, expand, Node, @on)
    ;   send(W, can_collapse, Node)
    ->  send(W, expand, Node, @off)
    ;   true
    ).

can_expand(W, Node:xdot_node) :->
    get(W, node_id, Node, pred(_)),
    \+ is_cluster(Node).

%   A table of an expanded predicate or the cluster of its tables.

can_collapse(W, Node:xdot_node) :->
    get(W, node_pi, Node, PI),
    viewer(W, V),
    get(V, expanded, Expanded),
    memberchk(PI, Expanded).

expand(W, Node:xdot_node, Expand:bool) :->
    get(W, node_pi, Node, PI),
    viewer(W, V),
    send(V, expand, PI, Expand).

can_edit(W, Node:xdot_node) :->
    get(W, node_pi, Node, _).

edit(W, Node:xdot_node) :->
    "Edit the predicate of the node"::
    get(W, node_pi, Node, PI),
    edit(PI).

show_variants(W, Node:xdot_node) :->
    "Browse the tables of the predicate of the node"::
    get(W, node_pi, Node, PI),
    viewer(W, V),
    get(V, thread, Thread),
    idg_show_variants(Thread, pred(PI)).

:- pce_end_class(sdg_window).

is_cluster(Node) :-
    get(Node, name, Name),
    sub_atom(Name, 0, _, _, cluster_).
