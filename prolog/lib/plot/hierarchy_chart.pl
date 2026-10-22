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

:- module(hierarchy_chart,
          [ hierarchy_from_term/2       % +Term, -Node
          ]).
:- use_module(library(pce)).
:- use_module(library(help_message)).
:- autoload(library(apply), [foldl/4, foldl/5, maplist/3]).
:- autoload(library(lists),
            [sum_list/2, max_list/2, min_list/2, append/3, reverse/2]).
:- autoload(library(pairs), [pairs_keys/2]).
:- autoload(library(pce_util), [default/3]).

/** <module> Charts of a hierarchical distribution

This library shows how a quantity, such as disk space, time or memory,
is distributed over a hierarchy, as GNOME's disk usage analyser
_baobab_ does.  The hierarchy is a tree of `hierarchy_node` objects.
Each node has a value of its own; the <-total of a node is its value
plus the totals of its children.  There are two views on the same
tree:

  - `rings_chart` shows the focus node as a disc, surrounded by a ring
    per level below it.  The angle of a segment is its share of the
    focus.
  - `treemap_chart` shows the focus node as a rectangle that is divided
    into a rectangle per child, nested for the levels below it.

Both colour the nodes the same way, show a balloon with the name and
value of the node below the pointer and _zoom_ into a node that is
clicked: it becomes the new focus.  Clicking the focus zooms out.
`hierarchy_window` is a picture that shows a chart filling the window,
with a popup to zoom and to switch between the views.

For example:

```
?- hierarchy_from_term(node(all, 0, [ node(a, 10, []),
                                      node(b, 0, [ x-5, y-7 ])
                                    ]), Root),
   send(new(W, hierarchy_window(Root)), open).
```
*/

%!  hierarchy_from_term(+Term, -Node) is det.
%
%   Create a hierarchy of `hierarchy_node` objects from a term.  Term
%   is one of
%
%     - node(Label, Value, Children)
%     - node(Label, Value, Children, Options)
%     - Label-Value
%       A leaf.
%
%   Value is the value of the node itself, excluding its children.
%   Options is a list holding data(Data) for the node's <-data and
%   colour(Colour) for its <-colour.

hierarchy_from_term(Term, Node) :-
    node_term(Term, Label, Value, Children, Options),
    (   memberchk(data(Data), Options)
    ->  true
    ;   Data = @nil
    ),
    new(Node, hierarchy_node(Label, Value, Data)),
    (   memberchk(colour(Colour), Options)
    ->  send(Node, colour, Colour)
    ;   true
    ),
    forall(member(ChildTerm, Children),
           ( hierarchy_from_term(ChildTerm, Child),
             send(Node, append, Child)
           )).

node_term(node(Label, Value, Children), Label, Value, Children, []) :- !.
node_term(node(Label, Value, Children, Options),
          Label, Value, Children, Options) :- !.
node_term(Label-Value, Label, Value, [], []) :- !.
node_term(Term, _, _, _, _) :-
    type_error(hierarchy_node_term, Term).


                 /*******************************
                 *             NODE             *
                 *******************************/

:- pce_begin_class(hierarchy_node, object,
                   "Node of a hierarchical distribution").

variable(label,    char_array,      get,  "Name of the node").
variable(value,    num := 0,        get,  "Value of the node itself").
variable(total,    num*,            none, "Cached <-total").
variable(children, chain*,          get,  "Nodes below me").
variable(parent,   hierarchy_node*, get,  "Node I am part of").
variable(data,     any*,            both, "Application data").
variable(colour,   colour*,         both, "Colour (@nil: from the position)").

initialise(N, Label:label=char_array, Value:value=[num], Data:data=[any]*) :->
    "Create from label, value and application data"::
    send_super(N, initialise),
    send(N, slot, label, Label),
    (   Value == @default
    ->  true
    ;   send(N, slot, value, Value)
    ),
    default(Data, @nil, TheData),
    send(N, slot, data, TheData).

unlink(N) :->
    "Destroy the nodes below me as well"::
    (   get(N, children, Children),
        Children \== @nil
    ->  send(N, slot, children, @nil),
        send(Children, for_all,
             and(message(@arg1, slot, parent, @nil),
                 message(@arg1, free)))
    ;   true
    ),
    (   get(N, parent, Parent),
        Parent \== @nil
    ->  send(Parent, delete, N)
    ;   true
    ),
    send_super(N, unlink).

append(N, Child:hierarchy_node) :->
    "Add a node below me"::
    (   get(N, children, Children),
        Children \== @nil
    ->  true
    ;   send(N, slot, children, new(Children, chain))
    ),
    send(Children, append, Child),
    send(Child, slot, parent, N),
    send(N, modified).

delete(N, Child:hierarchy_node) :->
    "Remove a node below me (it is not destroyed)"::
    get(N, children, Children),
    send(Children, delete, Child),
    send(Child, slot, parent, @nil),
    send(N, modified).

value(N, Value:num) :->
    "Set the value of the node itself"::
    send(N, slot, value, Value),
    send(N, modified).

label(N, Label:char_array) :->
    send(N, slot, label, Label).

modified(N) :->
    "Invalidate the <-total of me and my parents"::
    send(N, slot, total, @nil),
    (   get(N, parent, Parent),
        Parent \== @nil
    ->  send(Parent, modified)
    ;   true
    ).

total(N, Total:num) :<-
    "Value of me and all nodes below me"::
    (   get(N, slot, total, Total),
        Total \== @nil
    ->  true
    ;   get(N, value, Value),
        (   get(N, children, Children),
            Children \== @nil
        ->  chain_list(Children, List),
            foldl(add_total, List, Value, Total)
        ;   Total = Value
        ),
        send(N, slot, total, Total)
    ).

add_total(Node, T0, T) :-
    get(Node, total, NT),
    T is T0+NT.

has_children(N) :->
    "True if there are nodes below me"::
    get(N, children, Children),
    Children \== @nil,
    \+ send(Children, empty).

depth(N, Depth:int) :<-
    "Number of nodes above me"::
    node_depth(N, 0, Depth).

node_depth(N, D0, D) :-
    get(N, parent, Parent),
    (   Parent == @nil
    ->  D = D0
    ;   D1 is D0+1,
        node_depth(Parent, D1, D)
    ).

root(N, Root:hierarchy_node) :<-
    "Topmost node"::
    get(N, parent, Parent),
    (   Parent == @nil
    ->  Root = N
    ;   get(Parent, root, Root)
    ).

path(N, Path:chain) :<-
    "Nodes from the root to me"::
    node_path(N, [], List),
    new(Path, chain),
    forall(member(Node, List), send(Path, append, Node)).

node_path(N, L0, L) :-
    get(N, parent, Parent),
    (   Parent == @nil
    ->  L = [N|L0]
    ;   node_path(Parent, [N|L0], L)
    ).

path_label(N, Sep:[char_array], Label:string) :<-
    "Labels of <-path, separated by Sep (default \" / \")"::
    default(Sep, ' / ', TheSep),
    node_path(N, [], Nodes),
    maplist(node_label, Nodes, Labels),
    atomic_list_concat(Labels, TheSep, Atom),
    new(Label, string('%s', Atom)).

:- pce_end_class(hierarchy_node).

node_label(Node, Label) :-
    get(Node?label, value, Label).

%   sorted_children(+Node, -Pairs) is det.
%
%   Pairs is a list Total-Child of the children of Node that have a
%   positive total, largest first.

sorted_children(Node, Pairs) :-
    get(Node, children, Children),
    Children \== @nil,
    !,
    chain_list(Children, List),
    findall(T-C, ( member(C, List),
                   get(C, total, T),
                   T > 0
                 ), Pairs0),
    keysort(Pairs0, Ascending),
    reverse(Ascending, Pairs).
sorted_children(_, []).


                 /*******************************
                 *            SEGMENT           *
                 *******************************/

%   The graphical for a node.  A segment is a filled path that accepts
%   events exactly where it is painted, so neighbours never both claim
%   the pointer.

:- pce_begin_class(hierarchy_segment, path,
                   "Graphical representing a hierarchy_node").

variable(node,        hierarchy_node, get, "Node represented").
variable(base_colour, colour,         get, "Fill when not highlighted").

initialise(S, Node:hierarchy_node, Fill:colour, Border:colour) :->
    send_super(S, initialise),
    send(S, slot, node, Node),
    send(S, slot, base_colour, Fill),
    send(S, fill, Fill),
    send(S, colour, Border),
    send(S, event_area, painted).

highlight(S, On:bool) :->
    "Show I am below the pointer"::
    get(S, base_colour, Fill),
    (   On == @on
    ->  get(Fill, hilite, 0.4, Lit),
        send(S, fill, Lit)
    ;   send(S, fill, Fill)
    ).

:- pce_end_class(hierarchy_segment).


                 /*******************************
                 *        ABSTRACT CHART        *
                 *******************************/

:- pce_begin_class(hierarchy_chart, device,
                   "Chart of a hierarchical distribution").

class_variable(border_colour, colour, ui_window_background,
               "Lines between the segments").
class_variable(font,          font,   small, "Font of the labels").
class_variable(max_depth,     int,    5,     "Number of levels shown").
class_variable(centre_colour, colour, grey85, "Fill of the focus").

variable(root,           hierarchy_node*, get,  "Topmost node").
variable(focus,          hierarchy_node*, get,  "Node shown as a whole").
variable(hover,          hierarchy_segment*, get, "Segment below the pointer").
variable(extent,         size,    get,  "Size of the chart").
variable(max_depth,      int,     get,  "Number of levels shown").
variable(unit,           {bytes,seconds,count,plain} := plain, get,
         "How values are shown").
variable(select_message, code*,   both,
         "Called with a node without children that is clicked").
variable(focus_message,  code*,   both,
         "Called with the new focus").
variable(hover_message,  code*,   both,
         "Called with the node below the pointer, or @nil").

initialise(C, Root:root=[hierarchy_node]*, Extent:extent=[size]) :->
    "Create from the root and the size"::
    send_super(C, initialise),
    default(Extent, size(400, 400), Ext),
    send(C, slot, extent, Ext),
    get(C, class_variable_value, max_depth, Depth),
    send(C, slot, max_depth, Depth),
    default(Root, @nil, TheRoot),
    send(C, root, TheRoot).

root(C, Root:hierarchy_node*) :->
    "Show a new hierarchy"::
    send(C, slot, root, Root),
    send(C, focus, Root).

focus(C, Focus:hierarchy_node*) :->
    "Zoom to a node"::
    send(C, slot, focus, Focus),
    send(C, layout),
    (   get(C, focus_message, Msg),
        Msg \== @nil,
        Focus \== @nil
    ->  send(Msg, forward, Focus)
    ;   true
    ).

zoom_out(C) :->
    "Zoom out one level"::
    get(C, focus, Focus),
    Focus \== @nil,
    get(Focus, parent, Parent),
    Parent \== @nil,
    send(C, focus, Parent).

zoom_to_root(C) :->
    "Show the whole hierarchy"::
    get(C, root, Root),
    send(C, focus, Root).

extent(C, Extent:size) :->
    "Resize the chart"::
    send(C, slot, extent, Extent),
    send(C, layout).

max_depth(C, Depth:'1..') :->
    send(C, slot, max_depth, Depth),
    send(C, layout).

unit(C, Unit:{bytes,seconds,count,plain}) :->
    send(C, slot, unit, Unit),
    send(C, layout).

layout(C) :->
    "Draw the chart again"::
    send(C, hover_segment, @nil),
    send(C, clear),
    (   get(C, focus, Focus),
        Focus \== @nil,
        get(Focus, total, Total),
        Total > 0
    ->  send(C, draw, Focus)
    ;   true
    ).

draw(_C, _Focus:hierarchy_node) :->
    "Draw the focus and the levels below it (subclass)"::
    fail.

                 /*******************************
                 *            EVENTS            *
                 *******************************/

%   send_super/3 updates the <-pointed chain of the device, whose first
%   segment is the topmost one below the pointer.

event(C, Ev:event) :->
    "Track the segment below the pointer and zoom on a click"::
    ignore(send_super(C, event, Ev)),
    (   send(Ev, is_a, area_exit)
    ->  send(C, hover_segment, @nil)
    ;   send(Ev, is_a, loc_move)
    ->  (   pointed_segment(C, S)
        ->  send(C, hover_segment, S)
        ;   send(C, hover_segment, @nil)
        )
    ;   send(Ev, is_a, ms_left_up),
        pointed_segment(C, S)
    ->  send(C, clicked, S?node)
    ).

pointed_segment(C, S) :-
    get(C, pointed, Pointed),
    get(Pointed, find, message(@arg1, instance_of, hierarchy_segment), S).

hover_segment(C, S:hierarchy_segment*) :->
    "Highlight the segment below the pointer"::
    get(C, hover, Old),
    (   Old == S
    ->  true
    ;   (   Old \== @nil, object(Old)
        ->  send(Old, highlight, @off)
        ;   true
        ),
        send(C, slot, hover, S),
        (   S == @nil
        ->  Node = @nil
        ;   send(S, highlight, @on),
            get(S, node, Node)
        ),
        (   get(C, hover_message, Msg),
            Msg \== @nil
        ->  send(Msg, forward, Node)
        ;   true
        )
    ).

clicked(C, Node:hierarchy_node) :->
    "Zoom in or out, or call <-select_message"::
    (   get(C, focus, Node)
    ->  ignore(send(C, zoom_out))
    ;   send(Node, has_children)
    ->  send(C, focus, Node)
    ;   get(C, select_message, Msg),
        Msg \== @nil
    ->  send(Msg, forward, Node)
    ;   true
    ).

%   The balloon asks before the chart sees the event, so we find the
%   segment from the event rather than using <-hover.

help_message(C, What:{tag,summary}, Ev:[event], Msg:string) :<-
    "Describe the node below the pointer"::
    What == tag,
    (   Ev == @default
    ->  get(C, hover, S),
        S \== @nil
    ;   get(C, pointed_objects, Ev, Pointed),
        get(Pointed, find,
            message(@arg1, instance_of, hierarchy_segment), S)
    ),
    get(S, node, Node),
    get(C, describe, Node, Msg).

describe(C, Node:hierarchy_node, Text:string) :<-
    "Label, value and share of a node"::
    get(Node, label, Label),
    get(Node, total, Total),
    get(C, format_value, Total, Value),
    (   get(C, focus, Focus),
        Focus \== Node,
        get(Focus, total, FT),
        FT > 0
    ->  Share is 100*Total/FT,
        get(Focus, label, FL),
        format(string(S), '~w\n~w (~1f% of ~w)', [Label, Value, Share, FL])
    ;   format(string(S), '~w\n~w', [Label, Value])
    ),
    new(Text, string('%s', S)).

format_value(C, Value:num, Text:name) :<-
    "Format a value using <-unit"::
    get(C, unit, Unit),
    format_value(Unit, Value, Text).

format_value(bytes, V, Text) :-
    !,
    (   V < 1000
    ->  format(atom(Text), '~d bytes', [round(V)])
    ;   byte_unit(Unit, Size),
        V < Size*1000
    ->  format(atom(Text), '~1f ~w', [V/Size, Unit])
    ;   format(atom(Text), '~1f PB', [V/1.0e15])
    ).
format_value(seconds, V, Text) :-
    !,
    (   V < 0.001
    ->  format(atom(Text), '~1f µs', [V*1.0e6])
    ;   V < 1
    ->  format(atom(Text), '~1f ms', [V*1000])
    ;   V < 60
    ->  format(atom(Text), '~2f s', [V])
    ;   format(atom(Text), '~1f min', [V/60])
    ).
format_value(count, V, Text) :-
    !,
    format(atom(Text), '~D', [round(V)]).
format_value(_, V, Text) :-
    (   integer(V)
    ->  format(atom(Text), '~d', [V])
    ;   format(atom(Text), '~4g', [V])
    ).

byte_unit(kB, 1.0e3).
byte_unit('MB', 1.0e6).
byte_unit('GB', 1.0e9).
byte_unit('TB', 1.0e12).

                 /*******************************
                 *            DRAWING           *
                 *******************************/

%   node_colour(+Node, +Hue, +Depth, -Colour)
%
%   The colour of Node at Depth below the focus.  Hue (0..1) is the
%   middle of the share of Node in the focus, so siblings differ in hue
%   and descendants have a hue close to that of their ancestors.  They
%   are paler the deeper they are.

node_colour(Node, _, _, Colour) :-
    get(Node, colour, Colour),
    Colour \== @nil,
    !.
node_colour(_, Hue, Depth, Colour) :-
    H is round(Hue*360) mod 360,
    S is max(20, 62 - 8*(Depth-1)),
    V is min(98, 84 + 3*(Depth-1)),
    new(Colour, colour(@default, H, S, V, @default, hsv)).

text_colour(Fill, Colour) :-
    (   get(Fill, intensity, I),
        I < 128
    ->  Colour = white
    ;   Colour = black
    ).

%   display_label(+Chart, +Text, +Fill, +Center) is det.
%
%   Show Text centred at Center in a colour that is readable on Fill.

display_label(C, Text, Fill, Center) :-
    get(C, font, Font),
    send(C, display, new(T, text(Text, center, Font))),
    text_colour(Fill, TC),
    send(T, colour, TC),
    send(T, center, Center).

:- pce_end_class(hierarchy_chart).


                 /*******************************
                 *             RINGS            *
                 *******************************/

:- pce_begin_class(rings_chart, hierarchy_chart,
                   "Hierarchy as concentric rings").

class_variable(min_angle,     num,    0.5,
               "Segments smaller than this (degrees) are not shown").

draw(C, Focus:hierarchy_node) :->
    "Draw the focus as a disc and the rings around it"::
    get(C, extent, size(W, H)),
    get(C, max_depth, Max),
    tree_depth(Focus, Max, Depth0),
    Depth is max(1, Depth0),
    R is (min(W, H) - 4) / 2,
    RW is R/(Depth+1),
    CX is W/2, CY is H/2,
    get(C, border_colour, Border),
    get(C, centre_colour, Centre),
    disc_data(CX, CY, RW, Data),
    send(C, display, new(Disc, hierarchy_segment(Focus, Centre, Border))),
    send(Disc, svg_data, Data),
    centre_label(C, Focus, CX, CY, RW, Centre),
    get(C, min_angle, MinDeg),
    MinAngle is MinDeg*pi/180,
    Ring = ring(CX, CY, RW, Depth, MinAngle, Border),
    draw_children(C, Ring, Focus, 0, 2*pi, 1).

%   tree_depth(+Node, +Max, -Depth)
%
%   Depth is the number of levels below Node, up to Max.

tree_depth(_, 0, 0) :- !.
tree_depth(Node, Max, Depth) :-
    sorted_children(Node, Pairs),
    (   Pairs == []
    ->  Depth = 0
    ;   Max1 is Max-1,
        findall(D, ( member(_-Child, Pairs),
                     tree_depth(Child, Max1, D)
                   ), Ds),
        max_list(Ds, D0),
        Depth is D0+1
    ).

%   draw_children(+Chart, +Ring, +Node, +A0, +A1, +Level)
%
%   Draw the children of Node, which covers the angles A0..A1, in ring
%   Level.  Angles are clockwise from 12 o'clock.

draw_children(_, ring(_,_,_,Depth,_,_), _, _, _, Level) :-
    Level > Depth,
    !.
draw_children(C, Ring, Node, A0, A1, Level) :-
    sorted_children(Node, Pairs),
    get(Node, total, Total),
    Scale is (A1-A0)/Total,
    foldl(draw_child(C, Ring, Scale, Level), Pairs, A0, _).

draw_child(C, Ring, Scale, Level, Total-Child, A0, A1) :-
    A1 is A0 + Total*Scale,
    Ring = ring(CX, CY, RW, _Depth, MinAngle, Border),
    (   A1-A0 >= MinAngle
    ->  Hue is (A0+A1)/(4*pi),
        node_colour(Child, Hue, Level, Fill),
        Inner is Level*RW,
        Outer is (Level+1)*RW,
        ring_data(CX, CY, Inner, Outer, A0, A1, Data),
        send(C, display, new(S, hierarchy_segment(Child, Fill, Border))),
        send(S, svg_data, Data),
        send(S, fill_rule, even_odd),
        segment_label(C, Child, CX, CY, Inner, Outer, A0, A1, Fill),
        Level1 is Level+1,
        draw_children(C, Ring, Child, A0, A1, Level1)
    ;   true
    ).

%   ring_data(+CX, +CY, +Inner, +Outer, +A0, +A1, -Data)
%
%   SVG path data for the part of a ring between angles A0 and A1.  A
%   full ring is two circles; even_odd filling makes the inner one a
%   hole.

ring_data(CX, CY, Inner, Outer, A0, A1, Data) :-
    A1-A0 >= 2*pi - 1.0e-6,
    !,
    circle_data(CX, CY, Outer, D1),
    circle_data(CX, CY, Inner, D2),
    format(string(Data), '~w ~w', [D1, D2]).
ring_data(CX, CY, Inner, Outer, A0, A1, Data) :-
    (   A1-A0 > pi
    ->  Large = 1
    ;   Large = 0
    ),
    polar(CX, CY, Outer, A0, X1, Y1),
    polar(CX, CY, Outer, A1, X2, Y2),
    polar(CX, CY, Inner, A1, X3, Y3),
    polar(CX, CY, Inner, A0, X4, Y4),
    format(string(Data),
           'M ~4f ~4f A ~4f ~4f 0 ~d 1 ~4f ~4f \c
            L ~4f ~4f A ~4f ~4f 0 ~d 0 ~4f ~4f Z',
           [ X1, Y1, Outer, Outer, Large, X2, Y2,
             X3, Y3, Inner, Inner, Large, X4, Y4 ]).

disc_data(CX, CY, R, Data) :-
    circle_data(CX, CY, R, Data).

circle_data(CX, CY, R, Data) :-
    XL is CX-R, XR is CX+R,
    format(string(Data),
           'M ~4f ~4f A ~4f ~4f 0 1 1 ~4f ~4f A ~4f ~4f 0 1 1 ~4f ~4f Z',
           [XR, CY, R, R, XL, CY, R, R, XR, CY]).

polar(CX, CY, R, A, X, Y) :-
    X is CX + R*sin(A),
    Y is CY - R*cos(A).

%   segment_label(+Chart, +Node, +CX, +CY, +Inner, +Outer, +A0, +A1,
%                 +Fill)
%
%   Label a segment if the label fits in it horizontally.

segment_label(C, Node, CX, CY, Inner, Outer, A0, A1, Fill) :-
    get(C, font, Font),
    get(Font, height, FH),
    get(Node, label, Label),
    get(Font, width, Label, TW),
    Mid is (Inner+Outer)/2,
    Arc is (A1-A0)*Mid,
    Thick is Outer-Inner,
    (   FH+2 =< Thick,
        TW+4 =< Arc,
        TW+4 =< 2*Mid,                  % not wider than the ring
        label_fits_radially(TW, FH, Inner, Outer, (A0+A1)/2)
    ->  A is (A0+A1)/2,
        polar(CX, CY, Mid, A, X, Y),
        display_label(C, Label, Fill, point(round(X), round(Y)))
    ;   true
    ).

%   A horizontal label at angle A extends along the ring by its width
%   times |cos(A)| plus its height times |sin(A)|, and across it by the
%   width times |sin(A)| plus the height times |cos(A)|.  It fits if
%   that is not thicker than the ring.

label_fits_radially(TW, FH, Inner, Outer, A) :-
    Across is TW*abs(sin(A)) + FH*abs(cos(A)),
    Across =< Outer-Inner.

centre_label(C, Focus, CX, CY, R, Fill) :-
    get(C, font, Font),
    get(Font, height, FH),
    get(Focus, label, Label),
    get(Focus, total, Total),
    get(C, format_value, Total, Value),
    get(Font, width, Label, LW),
    get(Font, width, Value, VW),
    Room is 1.8*R,
    (   LW =< Room, VW =< Room, 2*FH =< Room
    ->  Y1 is round(CY - FH/2), Y2 is round(CY + FH/2),
        display_label(C, Label, Fill, point(round(CX), Y1)),
        display_label(C, Value, Fill, point(round(CX), Y2))
    ;   VW =< Room
    ->  display_label(C, Value, Fill, point(round(CX), round(CY)))
    ;   true
    ).

:- pce_end_class(rings_chart).


                 /*******************************
                 *            TREEMAP           *
                 *******************************/

:- pce_begin_class(treemap_chart, hierarchy_chart,
                   "Hierarchy as nested rectangles").

class_variable(max_depth, int, 4,  "Number of levels shown").
class_variable(padding,   int, 2,  "Space around the children of a node").
class_variable(min_size,  int, 3,  "Smaller rectangles are not shown").

draw(C, Focus:hierarchy_node) :->
    "Draw the focus as a rectangle holding its children"::
    get(C, extent, size(W, H)),
    get(C, border_colour, Border),
    get(C, padding, Pad),
    get(C, min_size, Min),
    get(C, max_depth, Max),
    get(C, font, Font),
    get(Font, height, FH),
    Ctx = tm(Border, Pad, Min, Max, FH),
    draw_tile(C, Ctx, Focus, 0, 1, r(0, 0, W, H), 0).

%   draw_tile(+Chart, +Ctx, +Node, +Hue0, +Hue1, +Rect, +Depth)
%
%   Draw Node in Rect, with the hues Hue0..Hue1 for its children.

draw_tile(C, Ctx, Node, Hue0, Hue1, r(X, Y, W, H), Depth) :-
    Ctx = tm(Border, Pad, Min, Max, FH),
    IX is round(X), IY is round(Y),
    IW is round(X+W)-IX, IH is round(Y+H)-IY,
    (   IW >= Min, IH >= Min
    ->  (   Depth == 0
        ->  get(C, class_variable_value, centre_colour, Fill)
        ;   Hue is (Hue0+Hue1)/2,
            node_colour(Node, Hue, Depth, Fill)
        ),
        rect_data(IX, IY, IW, IH, Data),
        send(C, display, new(S, hierarchy_segment(Node, Fill, Border))),
        send(S, svg_data, Data),
        HeaderH is FH+2,
        (   Depth < Max,
            sorted_children(Node, Pairs),
            Pairs \== [],
            IW > 2*Pad + Min,
            IH > HeaderH + Pad + Min
        ->  tile_label(C, Node, Depth, IX, IY, IW, Fill),
            Inner = r(IX+Pad, IY+HeaderH, IW-2*Pad, IH-HeaderH-Pad),
            draw_children_tiles(C, Ctx, Node, Pairs, Hue0, Hue1, Inner, Depth)
        ;   leaf_label(C, Node, IX, IY, IW, IH, FH, Fill)
        )
    ;   true
    ).

%   The children share the area with the value of the node itself,
%   which is left empty.

draw_children_tiles(C, Ctx, Node, Pairs, Hue0, Hue1, r(X,Y,W,H), Depth) :-
    get(Node, total, Total),
    get(Node, value, Own),
    Scale is W*H/Total,
    findall(A-child(Child), ( member(T-Child, Pairs),
                              A is T*Scale
                            ), Items0),
    (   Own > 0
    ->  OwnArea is Own*Scale,
        append(Items0, [OwnArea-own], Items)
    ;   Items = Items0
    ),
    squarify(Items, r(X,Y,W,H), Placed),
    HueScale is (Hue1-Hue0)/Total,
    Depth1 is Depth+1,
    foldl(draw_placed(C, Ctx, HueScale, Depth1), Placed, Hue0, _).

%   The hues of the children are their share of Hue0..Hue1, in the same
%   order as the rings chart uses for their angles.

draw_placed(C, Ctx, HueScale, Depth, child(Child)-Rect, H0, H1) :-
    !,
    get(Child, total, T),
    H1 is H0 + T*HueScale,
    draw_tile(C, Ctx, Child, H0, H1, Rect, Depth).
draw_placed(_, _, _, _, own-_, H, H).

%!  squarify(+Items, +Rect, -Placed) is det.
%
%   Divide Rect over Items, a list Area-Item sorted largest first whose
%   areas add up to that of Rect, using the _squarified_ treemap
%   algorithm of Bruls, Huizing and van Wijk: rows of items are laid
%   along the shorter side of what is left, and an item is added to the
%   row as long as that makes the worst aspect ratio in the row better.
%   Placed is a list Item-Rect in the order of Items.

squarify([], _, []) :- !.
squarify(Items, r(X,Y,W,H), Placed) :-
    Side is min(W, H),
    take_row(Items, Side, [], Row, Rest),
    layout_row(Row, r(X,Y,W,H), RowPlaced, Left),
    append(RowPlaced, Placed1, Placed),
    squarify(Rest, Left, Placed1).

take_row([], _, Row, Row, []).
take_row([I|Is], Side, Row0, Row, Rest) :-
    (   Row0 == []
    ->  take_row(Is, Side, [I], Row, Rest)
    ;   append(Row0, [I], Row1),
        worst(Row1, Side, W1),
        worst(Row0, Side, W0),
        W1 =< W0
    ->  take_row(Is, Side, Row1, Row, Rest)
    ;   Row = Row0,
        Rest = [I|Is]
    ).

%   worst(+Row, +Side, -Ratio)
%
%   The worst aspect ratio of the items of Row laid along Side.

worst(Row, Side, Ratio) :-
    pairs_keys(Row, Areas),
    sum_list(Areas, S),
    max_list(Areas, Max),
    min_list(Areas, Min),
    (   Min =< 0
    ->  Ratio = 1.0e300
    ;   Ratio is max(Side*Side*Max/(S*S), (S*S)/(Side*Side*Min))
    ).

layout_row(Row, r(X,Y,W,H), Placed, Left) :-
    pairs_keys(Row, Areas),
    sum_list(Areas, S),
    (   W >= H                          % a column at the left
    ->  RW is S/H,
        foldl(place_down(X, RW), Row, Placed, Y, _),
        Left = r(X+RW, Y, W-RW, H)
    ;   RH is S/W,                      % a row at the top
        foldl(place_right(Y, RH), Row, Placed, X, _),
        Left = r(X, Y+RH, W, H-RH)
    ).

place_down(X, RW, A-Item, Item-r(X,Y0,RW,IH), Y0, Y1) :-
    IH is A/RW,
    Y1 is Y0+IH.

place_right(Y, RH, A-Item, Item-r(X0,Y,IW,RH), X0, X1) :-
    IW is A/RH,
    X1 is X0+IW.

rect_data(X, Y, W, H, Data) :-
    format(string(Data), 'M ~d ~d h ~d v ~d h ~d Z', [X, Y, W, H, -W]).

tile_label(C, Node, Depth, X, Y, W, Fill) :-
    (   Depth == 0
    ->  get(Node?path_label, value, Text0)
    ;   get(Node?label, value, Text0)
    ),
    get(Node, total, Total),
    get(C, format_value, Total, Value),
    format(string(Text), '~w (~w)', [Text0, Value]),
    get(C, font, Font),
    (   get(Font, width, Text, TW),
        TW+6 =< W
    ->  show_label(C, Text, Fill, X+3, Y+1)
    ;   get(Font, width, Text0, TW0),
        TW0+6 =< W
    ->  show_label(C, Text0, Fill, X+3, Y+1)
    ;   true
    ).

leaf_label(C, Node, X, Y, W, H, FH, Fill) :-
    get(C, font, Font),
    get(Node, label, Label),
    get(Node, total, Total),
    get(C, format_value, Total, Value),
    get(Font, width, Label, LW),
    get(Font, width, Value, VW),
    (   LW+6 =< W, 2*FH+4 =< H, VW+6 =< W
    ->  show_label(C, Label, Fill, X+3, Y+2),
        show_label(C, Value, Fill, X+3, Y+2+FH)
    ;   LW+6 =< W, FH+4 =< H
    ->  show_label(C, Label, Fill, X+3, Y+2)
    ;   true
    ).

show_label(C, Text, Fill, X, Y) :-
    get(C, font, Font),
    send(C, display, new(T, text(Text, left, Font)), point(X, Y)),
    text_colour(Fill, TC),
    send(T, colour, TC).

:- pce_end_class(treemap_chart).


                 /*******************************
                 *            WINDOW            *
                 *******************************/

:- pce_begin_class(hierarchy_window, picture,
                   "Window showing a hierarchy chart").

class_variable(view, {rings,treemap}, rings, "Initial view").

variable(view,         {rings,treemap}, get,  "Current view").
variable(view_message, code*,           both, "Called with a new view").

initialise(W, Root:root=[hierarchy_node]*, View:view=[{rings,treemap}]) :->
    "Create, showing Root as View"::
    send_super(W, initialise),
    send(W, scrollbars, none),
    (   View == @default
    ->  get(W, class_variable_value, view, TheView)
    ;   TheView = View
    ),
    view_class(TheView, Class),
    send(W, slot, view, TheView),
    send(W, slot, view_message, @nil),
    send(W, display, new(C, Class), point(0,0)),
    send(W, chart_messages, C),
    default(Root, @nil, TheRoot),
    send(C, root, TheRoot),
    send(W, popup, new(P, popup)),
    send_list(P, append,
              [ menu_item(zoom_out,
                          message(W?chart, zoom_out),
                          condition := message(W, can_zoom_out)),
                menu_item(zoom_to_top,
                          message(W?chart, zoom_to_root),
                          condition := message(W, can_zoom_out),
                          end_group := @on),
                menu_item(rings,
                          message(W, view, rings),
                          condition := W?view \== rings),
                menu_item(treemap,
                          message(W, view, treemap),
                          condition := W?view \== treemap)
              ]).

view_class(rings,   rings_chart).
view_class(treemap, treemap_chart).

chart(W, C:hierarchy_chart) :<-
    "The chart I display"::
    get(W?graphicals, find, message(@arg1, instance_of, hierarchy_chart), C).

chart_messages(W, C:hierarchy_chart) :->
    "Report the node below the pointer"::
    send(C, hover_message, message(W, hover, @arg1)).

root(W, Root:hierarchy_node*) :->
    "Show a new hierarchy"::
    send(W?chart, root, Root).
root(W, Root:hierarchy_node*) :<-
    get(W?chart, root, Root).

focus(W, Focus:hierarchy_node*) :->
    send(W?chart, focus, Focus).
focus(W, Focus:hierarchy_node*) :<-
    get(W?chart, focus, Focus).

unit(W, Unit:{bytes,seconds,count,plain}) :->
    "How values are shown"::
    send(W?chart, unit, Unit).

can_zoom_out(W) :->
    get(W?chart, focus, Focus),
    Focus \== @nil,
    get(Focus, parent, Parent),
    Parent \== @nil.

view(W, View:{rings,treemap}) :->
    "Switch between the rings and the treemap"::
    (   get(W, view, View)
    ->  true
    ;   get(W, chart, Old),
        view_class(View, Class),
        new(New, Class),
        maplist(copy_slot(Old, New),
                [unit, select_message, focus_message]),
        send(W, chart_messages, New),
        get(Old, root, Root),
        get(Old, focus, Focus),
        send(New, slot, root, Root),
        free(Old),
        send(W, slot, view, View),
        send(W, display, New, point(0,0)),
        send(W, resize),
        send(New, focus, Focus),
        (   get(W, view_message, Msg),
            Msg \== @nil
        ->  send(Msg, forward, View)
        ;   true
        )
    ).

copy_slot(From, To, Slot) :-
    get(From, slot, Slot, Value),
    send(To, slot, Slot, Value).

resize(W) :->
    "Make the chart fill the window"::
    get(W, visible, area(_, _, Width, Height)),
    (   Width > 10, Height > 10
    ->  send(W?chart, extent, size(Width, Height))
    ;   true
    ).

hover(W, Node:hierarchy_node*) :->
    "Report the node below the pointer"::
    (   Node == @nil
    ->  send(W, report, status, '')
    ;   get(Node, path_label, Path),
        get(Node, total, Total),
        get(W?chart, format_value, Total, Value),
        send(W, report, status, '%s: %s', Path, Value)
    ).

:- pce_end_class(hierarchy_window).
