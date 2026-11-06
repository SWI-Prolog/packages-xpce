/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
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

:- module(graphviz_layout,
          [ graphviz_layout/2,          % +Graph, -Layout
            graphviz_layout/3,          % +Graph, -Layout, +Options
            graphviz_json/4             % +Engine, +Format, +Input, -JSON
          ]).
:- use_module(library(process)).
:- use_module(library(json)).
:- use_module(library(readutil)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(error)).

/** <module> Use graphviz to lay out a graph whose nodes are drawn elsewhere

Graphviz is used to decide where the nodes of a graph go and how the
edges between them are routed, while the nodes and edge labels are
drawn by the caller, e.g., as XPCE graphicals.  Graphviz is told only
the size of each node and edge label, so it does not need to know about
fonts, colours or anything else that is drawn.

Sizes and positions are in points.  A caller that draws in pixels
may treat them as pixels.  Positions use screen orientation: the
origin is the top-left corner of the graph and Y grows downwards.

For example:

```
?- graphviz_layout(graph([rankdir='LR'],
                         [ node(a, size(60,20), []),
                           node(b, size(40,20), [])
                         ],
                         [ edge(a, b, [label=size(20,10)]) ]),
                   Layout).
Layout = layout(size(...), [node(a, point(...)), ...],
                [edge(a, b, route([...], none, point(...), point(...)))]).
```

If the package `graphviz` is installed, library(graphviz) is used to
call the graphviz libraries.  Only their layout part is used: there is
no rendering and no text layout.  Otherwise the graphviz program is run.
*/

%!  graphviz_layout(+Graph, -Layout) is det.
%!  graphviz_layout(+Graph, -Layout, +Options) is det.
%
%   Lay out Graph using graphviz.  Graph is a term
%
%       graph(GraphAttributes, Nodes, Edges)
%
%   where
%
%     - GraphAttributes is a list Name=Value of graphviz graph
%       attributes, e.g., `rankdir='LR'` or `nodesep=0.25`.
%     - Nodes is a list of node(Id, size(Width,Height), Attributes) and
%       cluster(Id, Attributes, Nodes).  Id is any ground term that
%       identifies the node or cluster.  A node is a box of the given
%       size.  Attributes is a list Name=Value of extra graphviz node
%       attributes, e.g., `shape=ellipse` to make edges end at an ellipse
%       that fits the box.  A cluster is a graphviz cluster holding
%       Nodes, which may hold clusters in turn.  Its Attributes are
%       graphviz graph attributes, e.g., `margin=8`.  The label of a
%       cluster is given as `label=size(Width,Height)`.
%     - Edges is a list of edge(From, To, Attributes).  Attributes
%       is a list Name=Value of graphviz edge attributes.  The label
%       of an edge is given as `label=size(Width,Height)`: graphviz
%       leaves room for it and says where it goes.
%
%   Layout is a term
%
%       layout(size(Width,Height), NodePlaces, EdgeRoutes)
%
%   where
%
%     - NodePlaces is a list of node(Id, point(X,Y)), where X,Y is the
%       centre of the node, and cluster(Id, area(X,Y,W,H), Label), where
%       area is the frame of the cluster and Label is `none` or the
%       centre of its label.  Outer clusters come before the clusters
%       they hold.
%     - EdgeRoutes is a list of edge(From, To, Route), in the order of
%       Edges.  Route is route(Points, Start, End, Label).  Points is
%       a list of point(X,Y) holding the control points of a cubic
%       B-spline: the start point followed by 3 points per segment.
%       Start and End are `none` or the point where an arrow at that
%       end has its tip; the arrow goes from the first or last point of
%       Points to this point.  Label is `none` or the centre of the
%       label.
%
%   Options:
%
%     - engine(+Engine)
%       Graphviz layout program to use.  Default is `dot`.
%     - directed(+Bool)
%       If `false`, the graph is undirected.  Default is `true`.
%     - backend(+Backend)
%       One of `library`, using library(graphviz), `process`, running
%       the graphviz program Engine, or `auto` (default), using the
%       library if it is installed and provides Engine.
%
%   @error existence_error(source_sink, path(Engine)) if Engine is not
%   installed.
%   @error graphviz(Engine, Status, Message) if Engine fails.

graphviz_layout(Graph, Layout) :-
    graphviz_layout(Graph, Layout, []).

graphviz_layout(Graph, Layout, Options) :-
    must_be(compound, Graph),
    Graph = graph(GAttrs, Items, Edges),
    option(engine(Engine), Options, dot),
    option(directed(Directed), Options, true),
    number_items(Items, 0-0, _, NItems),
    findall(N-node(Id, Size, Attrs),
            sub_item(NItems, n(N, Id, Size, Attrs)), IdMap),
    findall(K-Id, sub_item(NItems, c(K, Id, _, _)), ClusterMap),
    option(backend(Backend0), Options, auto),
    backend(Backend0, Engine, Backend),
    phrase(dot_graph(Directed, GAttrs, NItems, IdMap, Edges), Codes),
    string_codes(Dot, Codes),
    raw_layout(Backend, Engine, Dot, Raw),
    final_layout(Raw, IdMap, ClusterMap, Edges, Layout).

%   number_items(+Items, +N0-K0, -N-K, -NItems)
%
%   Number the nodes (N) and clusters (K) in the order they appear.  In
%   NItems, a node is n(N, Id, Size, Attrs) and a cluster c(K, Id, Attrs,
%   NItems).

number_items([], NK, NK, []).
number_items([H|T], NK0, NK, [NH|NT]) :-
    number_item(H, NK0, NK1, NH),
    number_items(T, NK1, NK, NT).

number_item(node(Id, Size, Attrs), N0-K, N-K, n(N0, Id, Size, Attrs)) :-
    !,
    N is N0+1.
number_item(cluster(Id, Attrs, Items), N0-K0, NK, c(K0, Id, Attrs, NItems)) :-
    !,
    K1 is K0+1,
    number_items(Items, N0-K1, NK, NItems).
number_item(Item, _, _, _) :-
    type_error(graphviz_item, Item).

%   sub_item(+NItems, -Item)
%
%   Item is a node or cluster in NItems, outer clusters first.

sub_item(NItems, Item) :-
    member(Item0, NItems),
    (   Item = Item0
    ;   Item0 = c(_, _, _, Sub),
        sub_item(Sub, Item)
    ).

%   backend(+Requested, +Engine, -Backend)
%
%   Use library(graphviz) if it is installed and provides Engine: it
%   saves starting a process and needs no graphviz programs.  The
%   library may provide only `dot`, in which case the other engines run
%   the graphviz program.

backend(auto, Engine, Backend) :-
    !,
    (   library_engine(Engine)
    ->  Backend = library
    ;   Backend = process
    ).
backend(library, _, library) :-
    !,
    (   graphviz_library
    ->  true
    ;   existence_error(library, graphviz)
    ).
backend(process, _, process) :-
    !.
backend(Backend, _, _) :-
    domain_error(graphviz_backend, Backend).

%   graphviz_library is semidet.
%
%   True if library(graphviz) can be loaded.

:- dynamic graphviz_library_loaded/1 as volatile.

graphviz_library :-
    graphviz_library_loaded(Loaded),
    !,
    Loaded == true.
graphviz_library :-
    (   exists_source(library(graphviz)),
        catch(use_module(library(graphviz),
                         [graphviz_layout_dot/4, graphviz_engine/1]),
              E, (print_message(warning, E), fail))
    ->  Loaded = true
    ;   Loaded = false
    ),
    assertz(graphviz_library_loaded(Loaded)),
    Loaded == true.

%   library_engine(+Engine) is semidet.
%
%   True if library(graphviz) provides Engine.  The library opens the
%   plugin of Engine the first time it is asked for, so we remember the
%   answer.

:- dynamic library_engine_known/2 as volatile.

library_engine(Engine) :-
    library_engine_known(Engine, Known),
    !,
    Known == true.
library_engine(Engine) :-
    (   graphviz_library,
        graphviz_engine(Engine)
    ->  Known = true
    ;   Known = false
    ),
    assertz(library_engine_known(Engine, Known)),
    Known == true.

%   raw_layout(+Backend, +Engine, +Dot, -Raw)
%
%   Raw is the layout as library(graphviz) gives it: graphviz
%   coordinates and the names graphviz knows the nodes and edges by.

raw_layout(library, Engine, Dot, Raw) :-
    graphviz_layout_dot(Engine, Dot, Raw, Messages0),
    split_string(Messages0, "", " \t\n", [Messages]),
    (   Messages == ""
    ->  true
    ;   print_message(warning, format('~w: ~s', [Engine, Messages]))
    ).
raw_layout(process, Engine, Dot, Raw) :-
    graphviz_json(Engine, json0, text(Dot), JSON),
    json_raw(JSON, Raw).


                 /*******************************
                 *          WRITE DOT           *
                 *******************************/

dot_graph(Directed, GAttrs, NItems, IdMap, Edges) -->
    { graph_keyword(Directed, Keyword, Arrow) },
    Keyword, " {\n",
    "  graph [", dot_attrs(GAttrs), "];\n",
    "  node [shape=box fixedsize=true label=\"\"];\n",
    dot_items(NItems),
    dot_edges(Edges, 0, Arrow, IdMap),
    "}\n".

graph_keyword(true,  "digraph", " -> ").
graph_keyword(false, "graph",   " -- ").

dot_items([]) --> [].
dot_items([H|T]) -->
    dot_item(H),
    dot_items(T).

dot_item(n(N, _Id, size(W,H), Attrs)) -->
    { WI is max(0.01, W/72.0),
      HI is max(0.01, H/72.0)
    },
    "  n", number(N), " [",
    dot_attrs([width=WI, height=HI|Attrs]), "];\n".
dot_item(c(K, _Id, Attrs, NItems)) -->
    "  subgraph cluster_", number(K), " {\n",
    "  graph [", dot_attrs(Attrs), "];\n",
    dot_items(NItems),
    "  }\n".

dot_edges([], _, _, _) --> [].
dot_edges([edge(From, To, Attrs)|T], I, Arrow, IdMap) -->
    { node_number(From, IdMap, FN),
      node_number(To, IdMap, TN),
      I1 is I+1
    },
    "  n", number(FN), Arrow, "n", number(TN), " [",
    dot_attrs([id=I|Attrs]), "];\n",
    dot_edges(T, I1, Arrow, IdMap).

node_number(Id, IdMap, N) :-
    (   member(N-node(Id0, _, _), IdMap),
        Id0 == Id
    ->  true
    ;   existence_error(graphviz_node, Id)
    ).

dot_attrs([]) --> [].
dot_attrs([Name=Value|T]) -->
    atom(Name), "=", dot_value(Value),
    (   { T == [] }
    ->  []
    ;   " ",
        dot_attrs(T)
    ).

%   A label given as its size is an HTML-like label holding an empty
%   table of that size: graphviz needs no font to lay it out.

dot_value(size(W, H)) -->
    !,
    { WI is max(1, ceiling(W)),
      HI is max(1, ceiling(H))
    },
    "<<TABLE BORDER=\"0\" CELLBORDER=\"0\" CELLPADDING=\"0\" \c
     CELLSPACING=\"0\" FIXEDSIZE=\"TRUE\" WIDTH=\"", number(WI),
    "\" HEIGHT=\"", number(HI), "\"><TR><TD></TD></TR></TABLE>>".
dot_value(Value) -->
    { number(Value) },
    !,
    number(Value).
dot_value(Value) -->
    { format(codes(Codes), '~w', [Value]) },
    "\"", dot_chars(Codes), "\"".

dot_chars([]) --> [].
dot_chars([H|T]) -->
    dot_char(H),
    dot_chars(T).

dot_char(0'")  --> !, "\\\"".
dot_char(0'\\) --> !, "\\\\".
dot_char(0'\n) --> !, "\\n".
dot_char(C)    --> [C].

atom(A) -->
    { format(codes(Codes, Tail), '~w', [A]) },
    dcg_codes(Codes, Tail).

number(N) -->
    { integer(N)
    ->  format(codes(Codes, Tail), '~d', [N])
    ;   F is float(N),
        format(codes(Codes, Tail), '~4f', [F])
    },
    dcg_codes(Codes, Tail).

dcg_codes(Codes, Tail, Codes, Tail).


                 /*******************************
                 *          READ LAYOUT         *
                 *******************************/

%   json_raw(+JSON, -Raw)
%
%   Raw is what library(graphviz) would make of the graph that the
%   graphviz program wrote as JSON.

json_raw(JSON, layout(bb(X0,Y0,X1,Y1), Nodes, Edges, Clusters)) :-
    split_numbers(JSON.bb, [X0,Y0,X1,Y1]),
    Objects = JSON.get(objects, []),
    findall(cluster(Name, bb(CX0,CY0,CX1,CY1), Label),
            ( member(O, Objects),
              get_dict(bb, O, BB),
              atom_string(Name, O.name),
              split_numbers(BB, [CX0,CY0,CX1,CY1]),
              (   get_dict(lp, O, LP)
              ->  split_numbers(LP, [LX,LY]),
                  Label = point(LX,LY)
              ;   Label = none
              )
            ), Clusters),
    findall(node(Name, point(X,Y)),
            ( member(O, Objects),
              get_dict(pos, O, Pos),
              atom_string(Name, O.name),
              split_numbers(Pos, [X,Y])
            ), Nodes),
    findall(edge(Id, Route),
            ( member(JE, JSON.get(edges, [])),
              atom_string(Id, JE.get(id, "")),
              json_route(JE, Route)
            ), Edges).

%   The pos of an edge is a list of splines separated by `;`.  There is
%   more than one only if graphviz merges edges.  We use the first.

json_route(JE, route(Points, Start, End, Label)) :-
    (   get_dict(pos, JE, Pos)
    ->  split_string(Pos, ";", "", [Spline|_]),
        split_string(Spline, " ", " ", Parts0),
        exclude(==(""), Parts0, Parts),
        spline_parts(Parts, Points, none, Start, none, End)
    ;   Points = [], Start = none, End = none
    ),
    (   get_dict(lp, JE, LP)
    ->  split_numbers(LP, [LX,LY]),
        Label = point(LX,LY)
    ;   Label = none
    ).

spline_parts([], [], S, S, E, E).
spline_parts([H|T], Points, S0, S, E0, E) :-
    split_string(H, ",", "", Fields),
    (   Fields = ["e"|XY]
    ->  maplist(string_number, XY, [X,Y]),
        spline_parts(T, Points, S0, S, point(X,Y), E)
    ;   Fields = ["s"|XY]
    ->  maplist(string_number, XY, [X,Y]),
        spline_parts(T, Points, point(X,Y), S, E0, E)
    ;   maplist(string_number, Fields, [X,Y]),
        Points = [point(X,Y)|Points1],
        spline_parts(T, Points1, S0, S, E0, E)
    ).

%   final_layout(+Raw, +IdMap, +Edges, -Layout)
%
%   Translate the names of the nodes and edges back to what the caller
%   gave and turn the coordinates to screen orientation.

final_layout(layout(bb(X0,Y0,X1,Y1), RawNodes, RawEdges, RawClusters),
             IdMap, ClusterMap, Edges,
             layout(size(W,H), Places, Routes)) :-
    W is X1-X0,
    H is Y1-Y0,
    Flip = flip(X0, Y1),
    findall(cluster(Id, area(AX,AY,AW,AH), Label),
            ( member(K-Id, ClusterMap),
              format(atom(Name), 'cluster_~d', [K]),
              memberchk(cluster(Name, bb(CX0,CY0,CX1,CY1), Label0),
                        RawClusters),
              flip(Flip, CX0, CY1, point(AX,AY)),
              AW is CX1-CX0,
              AH is CY1-CY0,
              flip_point(Flip, Label0, Label)
            ), ClusterPlaces),
    findall(node(Id, Point),
            ( member(node(Name, point(X,Y)), RawNodes),
              atom_concat(n, NA, Name),
              atom_number(NA, N),
              memberchk(N-node(Id, _, _), IdMap),
              flip(Flip, X, Y, Point)
            ), NodePlaces),
    append(ClusterPlaces, NodePlaces, Places),
    edge_routes(Edges, 0, RawEdges, Flip, Routes).

edge_routes([], _, _, _, []).
edge_routes([edge(From, To, _)|T0], I, RawEdges, Flip,
            [edge(From, To, Route)|T]) :-
    atom_number(Id, I),
    (   memberchk(edge(Id, Route0), RawEdges)
    ->  flip_route(Flip, Route0, Route)
    ;   Route = route([], none, none, none)
    ),
    I1 is I+1,
    edge_routes(T0, I1, RawEdges, Flip, T).

flip_route(Flip, route(Points0, Start0, End0, Label0),
           route(Points, Start, End, Label)) :-
    maplist(flip_point(Flip), Points0, Points),
    maplist(flip_point(Flip), [Start0, End0, Label0], [Start, End, Label]).

flip_point(_, none, none) :- !.
flip_point(Flip, point(X,Y), Point) :-
    flip(Flip, X, Y, Point).

flip(flip(X0, Y1), X, Y, point(PX, PY)) :-
    PX is X - X0,
    PY is Y1 - Y.

split_numbers(String, Numbers) :-
    split_string(String, ",", " ", Parts),
    maplist(string_number, Parts, Numbers).

string_number(String, Number) :-
    number_string(Number, String).


                 /*******************************
                 *        RUN GRAPHVIZ          *
                 *******************************/

%!  graphviz_json(+Engine, +Format, +Input, -JSON) is det.
%
%   Run the graphviz program Engine on Input and read its output in
%   Format, one of `json` or `json0`, as a dict.  Input is one of
%   `text(String)`, the graph in the DOT language, or `file(Path)`, a
%   file holding it.  Warnings of Engine are printed.
%
%   @error existence_error(source_sink, path(Engine)) if Engine is not
%   installed.
%   @error graphviz(Engine, Status, Message) if Engine fails.  Message
%   is what it wrote to its standard error.

graphviz_json(Engine, Format, Input, JSON) :-
    must_be(oneof([json,json0]), Format),
    atom_concat('-T', Format, Flag),
    run_dot(Input, Engine, Flag, JSON0, E, Status, Message0),
    split_string(Message0, "", " \t\n", [Message]),
    (   Status == exit(0),
        var(E)
    ->  (   Message == ""
        ->  true
        ;   print_message(warning, format('~w: ~s', [Engine, Message]))
        ),
        JSON = JSON0
    ;   throw(error(graphviz(Engine, Status, Message), _))
    ).

%   run_dot(+Input, +Engine, +Flag, -JSON, -Error, -Status, -Message)
%
%   Run Engine on Input.  Error is unbound if we could read its JSON
%   output and Message is what it wrote to its standard error.  Without
%   threads we cannot feed dot and read its output and errors at the
%   same time, so the input and the errors go through temporary files.

run_dot(Input, Engine, Flag, JSON, E, Status, Message) :-
    current_prolog_flag(threads, true),
    !,
    dot_input(Input, Args, StdIn, Feed),
    process_create(path(Engine), [Flag|Args],
                   [ stdin(StdIn),
                     stdout(pipe(Out)),
                     stderr(pipe(Err)),
                     process(PID)
                   ]),
    (   Feed = feed(In, Data)
    ->  send_to_dot(Data, In)
    ;   true
    ),
    message_queue_create(Queue),
    thread_create(read_stderr(Err, Queue), Reader, []),
    call_cleanup(catch(json_read_dict(Out, JSON), E, true),
                 close(Out)),
    thread_join(Reader, _),
    (   thread_get_message(Queue, stderr(Message), [timeout(0)])
    ->  true
    ;   Message = ""
    ),
    message_queue_destroy(Queue),
    process_wait(PID, Status).
run_dot(Input, Engine, Flag, JSON, E, Status, Message) :-
    setup_call_cleanup(
        ( dot_file(Input, Path, Tmp),
          tmp_file_stream(text, ErrFile, ErrStream)
        ),
        ( call_cleanup(
              process_create(path(Engine), [Flag, Path],
                             [ stdin(null),
                               stdout(pipe(Out)),
                               stderr(stream(ErrStream)),
                               process(PID)
                             ]),
              close(ErrStream)),
          call_cleanup(catch(json_read_dict(Out, JSON), E, true),
                       close(Out)),
          process_wait(PID, Status),
          read_file_to_string(ErrFile, Message, [])
        ),
        ( delete_file(ErrFile),
          (   Tmp == true
          ->  delete_file(Path)
          ;   true
          )
        )).

%   read_stderr(+Err, +Queue)
%
%   Read what dot writes to its standard error while we read its
%   output.  Reading it afterwards would hang if dot fills the pipe with
%   warnings before it finishes its output.

read_stderr(Err, Queue) :-
    call_cleanup(catch(read_string(Err, _, Message), _, Message = ""),
                 close(Err)),
    thread_send_message(Queue, stderr(Message)).

dot_input(file(Path), [Path], null, none).
dot_input(text(Data), [], pipe(In), feed(In, Data)).

%   dot_file(+Input, -Path, -Tmp)
%
%   Path is a file holding Input.  Tmp is `true` if we created it.

dot_file(file(Path), Path, false).
dot_file(text(Data), Path, true) :-
    tmp_file_stream(utf8, Path, Out),
    call_cleanup(format(Out, '~s', [Data]), close(Out)).

send_to_dot(String, ToDOT) :-
    set_stream(ToDOT, encoding(utf8)),
    thread_create(send_to_dot_(String, ToDOT), _,
                  [ detached(true) ]).

send_to_dot_(String, ToDOT) :-
    call_cleanup(format(ToDOT, '~s', [String]),
                 close(ToDOT)), !.

:- multifile prolog:error_message//1.

%   An atom Status is an error of library(graphviz), which explains
%   it itself.

prolog:error_message(graphviz(Engine, Status, Message)) -->
    { \+ atom(Status) },
    [ 'graphviz program "~w" failed (~p)'-[Engine, Status] ],
    (   { Message == "" }
    ->  []
    ;   [ nl, '~s'-[Message] ]
    ).
