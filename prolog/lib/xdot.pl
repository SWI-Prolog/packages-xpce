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

:- module(xdot,
          [ xdot_view/1,        % +DotFile
            xdot_view/2         % +DotFile, +Options
          ]).
:- use_module(library(pce)).
:- use_module(library(pan_zoom)).
:- use_module(library(process)).
:- use_module(library(json)).
:- use_module(library(option)).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(json)).
:- use_module(library(pce_util)).
:- use_module(library(debug)).
:- use_module(library(help_message)).

/** <module> Render graphviz xdot output in XPCE

Two classes:

  * class `xdot` (subclass of `figure`) — renders a graphviz-layout
    graph as XPCE graphicals inside a figure with `@pan_zoom_recogniser`
    already attached.  Use standalone or embed in your own picture.
  * class `xdot_window` (subclass of `picture`) — a ready-made pan/zoom
    viewer that hosts an `xdot`.

Typical usage:

    ```
    ?- new(F, xdot('mygraph.dot')).             % embeddable figure
    ?- send(F, load, 'other.dot').              % reload

    ?- new(W, xdot_window('mygraph.dot')),      % ready-made viewer
       send(W, open).
    ?- xdot_view('mygraph.dot').                % one-shot convenience
    ```

The input is passed through ``dot -Tjson`` (or another graphviz engine).
The JSON contains the  same  shape/text   ops  as  text  ``-Txdot``, but
pre-tokenised — each op is a dict with an `op` field and typed geometry.
Cubic B-splines decompose into  chains   of  `bezier_curve`  graphicals.
graphviz maths coords are flipped against the  graph bounding box so the
picture reads normally on screen.
*/


                 /*******************************
                 *          CONVENIENCE         *
                 *******************************/

%!  xdot_view(+DotFile) is det.
%!  xdot_view(+DotFile, +Options) is det.
%
%   Convenience wrapper: open an `xdot_window` on DotFile.  Options:
%
%     - engine(+Engine)
%       graphviz layout program; default `dot`
%     - title(+Title)
%       window title; default is DotFile's base name
%     - size(+size(W,H))
%       initial window size; default 800x600

xdot_view(File) :-
    xdot_view(File, []).

xdot_view(File, Options) :-
    file_base_name(File, Base),
    option(title(Title),   Options, Base),
    option(size(Size),     Options, size(800,600)),
    option(engine(Engine), Options, dot),
    new(W, xdot_window(@default, Title, Size)),
    send(W?xdot, engine, Engine),
    send(W?xdot, load, File),
    send(W, fit),
    send(W, open).


                 /*******************************
                 *          CLASS XDOT          *
                 *******************************/

:- pce_begin_class(xdot, figure,
                   "graphviz xdot output rendered as an XPCE figure").

variable(engine, name := dot, both,
         "Graphviz layout program (`dot', `neato', `fdp', ...)").
variable(source, file*,       get,
         "Current source; @nil if nothing has been loaded").
variable(node_popup,   popup*, both,
         "Default right-click menu for xdot_node children").
variable(edge_popup,   popup*, both,
         "Default right-click menu for xdot_edge children").
variable(node_clicked, code*,  both,
         "Left-click handler for xdot_node; @arg1 = the clicked node").
variable(edge_clicked, code*,  both,
         "Left-click handler for xdot_edge; @arg1 = the clicked edge").

initialise(F, Source:[file]*) :->
    "Create an xdot figure; if Source is provided, load it"::
    send_super(F, initialise),
    (   send(Source, instance_of, file)
    ->  send(F, load, Source)
    ;   true
    ).

load(F, Source:file) :->
    "Clear F and render Source as graphviz xdot"::
    send(F, clear, destroy),
    send(F, transform, @nil),           % reset any prior pan/zoom
    get(F, engine, Engine),
    dot_to_json(Source, Engine, JSON),
    render_json(F, JSON),
    send(F, slot, source, Source).

:- pce_end_class(xdot).

%!  render_json(+Figure, +JSON) is det.
%
%   Internal — render a parsed graphviz  JSON dict into Figure. Reusable
%   if a caller already has JSON from another route.
%
%   Graph-level shapes go straight into Figure.  Each graphviz object
%   (node/cluster) becomes an `xdot_node` sub-figure named after its
%   graphviz id; each edge becomes an `xdot_edge` sub-figure named
%   after its `id` attribute or, absent that, `Tail-Head`.  This
%   grouping lets callers find and act on individual graph parts
%   without walking a flat pile of graphicals.

render_json(F, JSON) :-
    graph_bb(JSON, bb(_,_,_,Ymax)),
    render_object(JSON, F, Ymax),                       % graph-level draws
    Objects = JSON.get(objects, []),
    forall(( member(Obj, Objects),
             has_draw_attrs(Obj)
           ),
           ( new(N, xdot_node(Obj, Ymax)),
             send(F, display, N)
           )),
    gvid_index(Objects, Index),
    forall(member(Edge, JSON.get(edges, [])),
           ( edge_endpoints(Edge, Index, Tail, Head),
             new(E, xdot_edge(Edge, Tail, Head, Ymax)),
             send(F, display, E)
           )).

%!  has_draw_attrs(+Obj) is semidet.
%
%   True when Obj carries at least one of the six graphviz draw
%   attributes.  Filters out `{ rank=same; ... }` and other implicit
%   subgraphs graphviz emits with auto-generated `%N` names — they
%   are layout hints without any visual content.

has_draw_attrs(Obj) :-
    xdot_attrs(Attrs),
    member(Key, Attrs),
    get_dict(Key, Obj, _),
    !.

gvid_index(Objects, Pairs) :-
    findall(Gv-Name,
            ( member(O, Objects),
              Gv = O.'_gvid',
              atom_string(Name, O.name)
            ),
            Pairs).

edge_endpoints(Edge, Index, Tail, Head) :-
    memberchk(Edge.tail-Tail, Index),
    memberchk(Edge.head-Head, Index).


                 /*******************************
                 *       CLASS XDOT_GROUP       *
                 *******************************/

:- pce_begin_class(xdot_group, figure,
                   "Common base for xdot_node and xdot_edge").

in_event_area(G, X:int, Y:int) :->
    "Succeed only if X,Y lands on the actual shape of some member"::
    get(G, position, point(GX, GY)),
    LX is X - GX,
    LY is Y - GY,
    get(G?graphicals, find,
        message(@arg1, in_event_area, LX, LY),
        Hit),
    get(G, name, Name),
    debug(xdot, 'Event on ~p(~p) (in ~p)', [G, Name, Hit]).

event(G, Event:event) :->
    "Process an event"::
    send_super(G, event, Event).

highlight(G, C:colour*) :->
    "Recolour the group's shape members to make it stand out."::
    (   send(C, instance_of, colour)
    ->  send(G?graphicals, for_all,
             message(@prolog, highlight_apply, @arg1, C))
    ;   send(G?graphicals, for_all,
             message(@prolog, highlight_restore, @arg1))
    ).

%!  highlight_apply(+Member, +Colour) is det.
%!  highlight_restore(+Member) is det.
%
%   Save the original `colour` / `fill` of Member in attributes on
%   first apply, then overwrite them.  Restore reads and clears
%   those attributes.

highlight_apply(M, _) :-
    send(M, instance_of, bitmap),
    !.
highlight_apply(M, C) :-
    save_slot_once(M, colour, highlight_saved_colour),
    send(M, colour, C),
    (   send(M, has_get_method, fill),
        get(M, fill, Fill),
        send(Fill, instance_of, colour)
    ->  save_slot_once(M, fill, highlight_saved_fill),
        get(C, fade, 0.3, FadedC),
        send(M, fill, FadedC)
    ;   true
    ).

highlight_restore(M) :-
    restore_slot(M, colour, highlight_saved_colour),
    restore_slot(M, fill,   highlight_saved_fill).

save_slot_once(M, _, Attr) :-
    get(M, attribute, Attr, _),
    !.
save_slot_once(M, Slot, Attr) :-
    get(M, Slot, V),
    send(M, attribute, Attr, V).

restore_slot(M, Slot, Attr) :-
    get(M, attribute, Attr, V),
    !,
    send(M, Slot, V),
    send(M, delete_attribute, Attr).
restore_slot(_, _, _).

:- pce_end_class(xdot_group).

                 /*******************************
                 *        CLASS XDOT_NODE       *
                 *******************************/

:- pce_begin_class(xdot_node(name), xdot_group,
                   "Group of xpce graphicals for one graphviz node").

initialise(N, Obj:prolog, Ymax:num) :->
    "Render the node's graphicals; name self after the graphviz id"::
    send_super(N, initialise),
    atom_string(NodeName, Obj.name),
    send(N, name, NodeName),
    render_object(Obj, N, Ymax),
    set_tooltip(N, Obj),
    send(N, recogniser, @xdot_node_recogniser).

popup(N, P:'popup*') :<-
    "Own popup slot if set, else container xdot's <-node_popup"::
    (   get(N, slot, popup, P0), P0 \== @nil
    ->  P = P0
    ;   get(N, device, D),
        send(D, instance_of, xdot),
        get(D, node_popup, P)
    ->  true
    ;   P = @nil
    ).

dispatch_click(N) :->
    "Forward this node to the container xdot's <-node_clicked, if set"::
    (   get(N, device, D),
        send(D, instance_of, xdot),
        get(D, node_clicked, M), M \== @nil
    ->  send(M, forward, N)
    ;   true
    ).

:- pce_global(@xdot_node_recogniser,
              new(handler_group(
                      popup_gesture(@receiver?popup),
                      click_gesture(left, '', single,
                                    message(@receiver, dispatch_click))))).

event(G, Event:event) :->
    "Highlight connected edges on hover"::
    (   send(Event, is_a, area_enter)
    ->  debug(area, 'Enter ~p', [G]),
        send(G, highlight_connections, red)
    ;   send(Event, is_a, area_exit)
    ->  send(G, highlight_connections, @nil)
    ;   send_super(G, event, Event)
    ).

highlight_connections(G, With:colour*) :->
    "Highlight the node and its connections"::
     send(G, highlight, With),
     send(G?edges, for_all,
          message(@arg1, highlight, With)).


edges(N, Edges:chain) :<-
    "Find edges to either side"::
    get(N, name, Name),
    get(N?device?graphicals, find_all,
        and(message(@arg1, instance_of, xdot_edge),
            or(@arg1?tail == Name,
               @arg1?head == Name)),
        Edges).

:- pce_end_class(xdot_node).


                 /*******************************
                 *        CLASS XDOT_EDGE       *
                 *******************************/

:- pce_begin_class(xdot_edge(name), xdot_group,
                   "Group of xpce graphicals for one graphviz edge").

variable(tail, name, get, "Tail node name").
variable(head, name, get, "Head node name").

initialise(E, Edge:prolog, Tail:name, Head:name, Ymax:num) :->
    "Render the edge's graphicals; name self after the edge id or Tail-Head"::
    send_super(E, initialise),
    send(E, slot, tail, Tail),
    send(E, slot, head, Head),
    edge_name(Edge, Tail, Head, EName),
    send(E, name, EName),
    render_object(Edge, E, Ymax),
    set_tooltip(E, Edge),
    send(E, recogniser, @xdot_edge_recogniser).

popup(E, P:'popup*') :<-
    "Own popup slot if set, else container xdot's <-edge_popup"::
    (   get(E, slot, popup, P0), P0 \== @nil
    ->  P = P0
    ;   get(E, device, D),
        send(D, instance_of, xdot),
        get(D, edge_popup, P)
    ->  true
    ;   P = @nil
    ).

dispatch_click(E) :->
    "Forward this edge to the container xdot's <-edge_clicked, if set"::
    (   get(E, device, D),
        send(D, instance_of, xdot),
        get(D, edge_clicked, M), M \== @nil
    ->  send(M, forward, E)
    ;   true
    ).

:- pce_global(@xdot_edge_recogniser,
              new(handler_group(
                      popup_gesture(@receiver?popup),
                      click_gesture(left, '', single,
                                    message(@receiver, dispatch_click))))).

edge_name(Edge, _, _, Name) :-
    get_dict(id, Edge, IdStr),
    !,
    atom_string(Name, IdStr).
edge_name(_, Tail, Head, Name) :-
    atomic_list_concat([Tail, Head], -, Name).

%!  set_tooltip(+Graphical, +Obj) is det.
%
%   If Obj has a `tooltip` attribute (from graphviz), wire it up as
%   the graphical's `->help_message(tag, ...)` — library(help_message)
%   pops it up as a balloon after the mouse rests on the group.
%   Skip empty tooltip strings.

set_tooltip(G, Obj) :-
    get_dict(tooltip, Obj, TooltipStr),
    TooltipStr \== "",
    !,
    send(G, help_message, tag, string(TooltipStr)).
set_tooltip(_, _).

:- pce_end_class(xdot_edge).


                 /*******************************
                 *       CLASS XDOT_WINDOW      *
                 *******************************/

:- pce_begin_class(xdot_window, picture,
                   "Pan/zoom viewer for graphviz xdot output").

initialise(W, Source:[file]*, Label:[name], Size:[size]) :->
    "Create the viewer; if Source is given, load it and fit"::
    default(Label, 'xdot',          TheLabel),
    default(Size,  size(800,600),   TheSize),
    send_super(W, initialise, TheLabel, TheSize),
    new(F, xdot(Source)),
    send(F, recogniser, @pan_zoom_recogniser),
    send(W, display, F),
    (   send(Source, instance_of, file)
    ->  send(W, fit)
    ;   true
    ).

xdot(W, F:xdot) :<-
    "Get the displayed xdot"::
    get(W, member, xdot, F).

load(W, Source:file) :->
    "Load Source into the embedded xdot and refit"::
    get(W, xdot, F),
    send(F, load, Source),
    send(W, fit).

fit(W) :->
    "Reset zoom; scale down to fit the visible area with a margin; center"::
    get(W, xdot, F),
    send(F, transform, @nil),              % reset any prior zoom
    get(F, area, area(_,_,GW,GH)),
    get(W, visible, area(_,_,VW,VH)),
    Margin = 20,
    Sx is float(VW - 2*Margin) / GW,
    Sy is float(VH - 2*Margin) / GH,
    S is min(1.0, min(Sx, Sy)),            % never scale up on fit
    (   S < 1.0
    ->  new(T, transform(0.0, S)),
        send(F, transform, T)
    ;   true
    ),
    send(F, center, W?visible?center).

:- pce_end_class(xdot_window).


                 /*******************************
                 *      GRAPHVIZ INVOCATION     *
                 *******************************/

dot_to_json(Source, Engine, JSON) :-
    get(Source, name, Path),                   % xpce file object → path atom
    setup_call_cleanup(
        process_create(path(Engine),
                   ['-Tjson', Path],
                   [ stdout(pipe(Out)),
                     process(PID)
                   ]),
        json_read_dict(Out, JSON),
        ( close(Out),
          process_wait(PID, _Status)
        )).

graph_bb(JSON, bb(Xmin, Ymin, Xmax, Ymax)) :-
    get_dict(bb, JSON, BBString),
    !,
    split_string(BBString, ",", "", Parts),
    maplist(number_string, [Xmin, Ymin, Xmax, Ymax], Parts).
graph_bb(_, bb(0,0,0,0)).


                 /*******************************
                 *           RENDERING          *
                 *******************************/

%!  render_object(+Object, +Figure, +Ymax) is det.
%
%   Render every draw-attribute of an object.  The xdot spec says
%   graphics state (fill/pen/font/style) is fresh for each attribute
%   value — so each attribute gets its own fold.

render_object(Obj, Fig, Ymax) :-
    xdot_attrs(Attrs),
    forall(( member(Key, Attrs),
             get_dict(Key, Obj, Ops)
           ),
           render_ops(Ops, Fig, Ymax)).

xdot_attrs(['_draw_', '_ldraw_',
            '_hdraw_', '_tdraw_',
            '_hldraw_', '_tldraw_']).

render_ops(Ops, Fig, Ymax) :-
    initial_state(S0),
    render_ops_(Ops, Fig, Ymax, S0, _).

render_ops_([], _, _, S, S).
render_ops_([Op|T], Fig, Ymax, S0, S) :-
    render_op(Op, Fig, Ymax, S0, S1),
    render_ops_(T, Fig, Ymax, S1, S).

%!  initial_state(-State) is det.
%
%   Fresh graphics state.  `Flags` carries the current xdot text-flags
%   bitmask (bit 1 = bold, 2 = italic, 4 = underline, ...); it is set
%   by the `t` op and consumed at the next `T` op.

initial_state(gs(black, black, font(normal,roman,14), solid, 1, 0)).


                 /*******************************
                 *          INDIVIDUAL OPS      *
                 *******************************/

render_op(Op, _Fig, Ymax, S0, S) :-
    get_dict(op, Op, "C"), !,
    fill_paint(Op, Ymax, C),
    S0 = gs(_, Pen, F, St, W, Fl),
    S = gs(C, Pen, F, St, W, Fl).
render_op(Op, _Fig, _Ymax, S0, S) :-
    get_dict(op, Op, "c"), !,
    fill_colour(Op, C),
    S0 = gs(Fill, _, F, St, W, Fl),
    S = gs(Fill, C, F, St, W, Fl).
render_op(Op, _Fig, _Ymax, S0, S) :-
    get_dict(op, Op, "F"), !,
    font_from_op(Op, S0, F),
    S0 = gs(Fill, Pen, _, St, W, Fl),
    S = gs(Fill, Pen, F, St, W, Fl).
render_op(Op, _Fig, _Ymax, S0, S) :-
    get_dict(op, Op, "S"), !,
    style_from_op(Op, S0, S).
render_op(Op, _Fig, _Ymax, S0, S) :-
    get_dict(op, Op, "t"), !,
    font_flags(Op, S0, S).
render_op(Op, Fig, Ymax, S, S) :-
    get_dict(op, Op, OpName),
    shape_op(OpName, Filled),
    !,
    shape(OpName, Op, Fig, Ymax, Filled, S).
render_op(Op, Fig, Ymax, S, S) :-
    get_dict(op, Op, "T"), !,
    text_op(Op, Fig, Ymax, S).
render_op(Op, Fig, Ymax, S, S) :-
    get_dict(op, Op, "I"), !,
    image_op(Op, Fig, Ymax).
render_op(_, _, _, S, S).           % unknown op — ignore


                 /*******************************
                 *              STATE           *
                 *******************************/

fill_colour(Op, none) :-
    get_dict(color, Op, "none"), !.
fill_colour(Op, Colour) :-
    get_dict(color, Op, Str),
    hsv_colour(Str, Colour), !.
fill_colour(Op, Atom) :-
    get_dict(color, Op, Str),
    atom_string(Atom, Str).

%!  fill_paint(+Op, +Ymax, -Fill) is det.
%
%   Same as fill_colour/2 but also recognises graphviz gradient
%   fills.  A `C` op with `grad` = "linear" or "radial" carries
%   `p0`, `p1`, and a `stops` list; we construct an XPCE gradient
%   object with Y-flipped coordinates so it lives in the same
%   coord system as the shape it fills.

fill_paint(Op, Ymax, G) :-
    get_dict(grad, Op, GradKind),
    GradKind \== "none",
    !,
    gradient_object(GradKind, Op, Ymax, G).
fill_paint(Op, _, Fill) :-
    fill_colour(Op, Fill).

gradient_object("linear", Op, Ymax, G) :-
    get_dict(p0, Op, [X0, Y0]),
    get_dict(p1, Op, [X1, Y1]),
    Y0f is Ymax - Y0,
    Y1f is Ymax - Y1,
    stops_chain(Op, Stops),
    new(G, gradient(linear, point(X0, Y0f), point(X1, Y1f),
                    @default, @default, Stops)).
gradient_object("radial", Op, Ymax, G) :-
    get_dict(p0, Op, [X0, Y0, R0]),
    get_dict(p1, Op, [X1, Y1, R1]),
    Y0f is Ymax - Y0,
    Y1f is Ymax - Y1,
    stops_chain(Op, Stops),
    new(G, gradient(radial, point(X0, Y0f), point(X1, Y1f),
                    R0, R1, Stops)).

stops_chain(Op, Chain) :-
    get_dict(stops, Op, StopDicts),
    new(Chain, chain),
    forall(member(Stop, StopDicts),
           ( stop_tuple(Stop, T),
             send(Chain, append, T)
           )).

stop_tuple(Stop, T) :-
    get_dict(frac, Stop, Frac),
    get_dict(color, Stop, ColorStr),
    atom_string(ColorAtom, ColorStr),
    get(@pce, convert, ColorAtom, colour, C),
    new(T, tuple(Frac, C)).

%!  hsv_colour(+Str, -Colour) is semidet.
%
%   graphviz emits HSV colours as `"H S V"` or `"H,S,V"` with each
%   component in 0.0..1.0.  Construct an XPCE colour with model=hsv;
%   XPCE expects hue in 0..360, saturation and value in 0..255.

hsv_colour(Str, Colour) :-
    split_string(Str, ", ", "", [HS,SS,VS]),
    number_string(H, HS),
    number_string(S, SS),
    number_string(V, VS),
    H >= 0, H =< 1,
    S >= 0, S =< 1,
    V >= 0, V =< 1,
    HD is H*360, SD is S*255, VD is V*255,
    new(Colour, colour(@default, HD, SD, VD, @default, hsv)).

%!  font_from_op(+Op, +State0, -Font) is det.
%
%   graphviz font sizes are in points (72 pt/inch).  XPCE renders through
%   Pango at a fixed 96 DPI (see packages/xpce/src/sdl/sdlfont.c), so an
%   N-point font renders as roughly N*96/72 pixels.  On top of that,
%   users can set the `font.scale` class variable in their preferences
%   to enlarge/shrink all fonts.  The rest of the xdot output is in
%   points and we treat 1 pt = 1 px, so we divide by both factors here.

font_from_op(Op, gs(_,_,_,_,_,_), font(Family, Style, Size)) :-
    get_dict(face, Op, Face),
    get_dict(size, Op, Size0),
    font_scale(Scale),
    Size is max(1, Size0 * 72 / (96 * Scale)),
    font_family(Face, Family),
    Style = roman.

font_scale(Scale) :-
    (   get(class(font), class_variable, scale, X),
        get(X, value, Scale)
    ->  true
    ;   Scale = 1.0
    ).

font_family(Face, Family) :-
    string_lower(Face, Lower),
    (   sub_string(Lower, _, _, _, "mono")    -> Family = fixed
    ;   sub_string(Lower, _, _, _, "courier") -> Family = fixed
    ;   sub_string(Lower, _, _, _, "times")   -> Family = serif
    ;   sub_string(Lower, _, _, _, "serif")   -> Family = serif
    ;   Family = normal
    ).

style_from_op(Op, gs(Fill,Pen,F,_,W0,Fl), gs(Fill,Pen,F,Style,W,Fl)) :-
    get_dict(style, Op, Str),
    ( style_texture(Str, Style) -> W = W0
    ; setlinewidth(Str, W)      -> Style = solid
    ; Style = solid, W = W0
    ).

style_texture("dashed", dashed).
style_texture("dotted", dotted).
style_texture("solid",  solid).
style_texture("bold",   solid).
style_texture("invis",  invis).

setlinewidth(Str, W) :-
    string_concat("setlinewidth(", Rest, Str),
    string_concat(N, ")", Rest),
    number_string(W0, N),
    W is max(1, W0).

%!  font_flags(+Op, +S0, -S) is det.
%
%   xdot `t` op sets the text-flags bitmask (field `fontchar`) for the
%   next `T` op.  Bits: 1 bold, 2 italic, 4 underline, 8 superscript,
%   16 subscript, 32 strike-through, 64 overline.  XPCE supports
%   bold/italic/underline directly; the remainder are silently ignored.

font_flags(Op, gs(Fill,Pen,F,St,W,_), gs(Fill,Pen,F,St,W,Flags)) :-
    get_dict(fontchar, Op, Flags).


                 /*******************************
                 *              SHAPES          *
                 *******************************/

shape_op("E", filled).
shape_op("e", stroked).
shape_op("P", filled).
shape_op("p", stroked).
shape_op("L", stroked).
shape_op("B", stroked).
shape_op("b", filled).

shape(_, _, _, _, _, gs(_, _, _, invis, _, _)) => true.
shape("E", Dict, Fig, Ymax, Kind, S) => ellipse_op(Dict, Fig, Ymax, Kind, S).
shape("e", Dict, Fig, Ymax, Kind, S) => ellipse_op(Dict, Fig, Ymax, Kind, S).
shape("P", Dict, Fig, Ymax, Kind, S) => polygon_op(Dict, Fig, Ymax, closed, Kind, S).
shape("p", Dict, Fig, Ymax, Kind, S) => polygon_op(Dict, Fig, Ymax, closed, Kind, S).
shape("L", Dict, Fig, Ymax, Kind, S) => polygon_op(Dict, Fig, Ymax, open, Kind, S).
shape("B", Dict, Fig, Ymax, _, S)    => bspline_op(Dict, Fig, Ymax, stroked, S).
shape("b", Dict, Fig, Ymax, _, S)    => bspline_op(Dict, Fig, Ymax, stroked, S).

ellipse_op(Dict, Fig, Ymax, Kind, S) :-
    get_dict(rect, Dict, [Cx, Cy, Rx, Ry]),
    W is 2*Rx,
    H is 2*Ry,
    X is Cx - Rx,
    Y is Ymax - Cy - Ry,
    new(E, ellipse(W, H)),
    apply_paint(E, Kind, S),
    send(Fig, display, E, point(X, Y)).

polygon_op(Dict, Fig, Ymax, Close, Kind, S) :-
    get_dict(points, Dict, Pts),
    new(P, path),
    (   Close == closed
    ->  send(P, closed, @on)
    ;   true
    ),
    forall(member([X,Y0], Pts),
           ( Y is Ymax - Y0,
             send(P, append, point(X, Y))
           )),
    apply_paint(P, Kind, S),
    send(Fig, display, P).

%!  bspline_op(+Dict, +Fig, +Ymax, +Kind, +State) is det.
%
%   xdot B-spline: control polygon with n = 3k+1 points.
%   moveto(p0); for each triple (p1,p2,p3), cubic curve_to(p1,p2,p3).

bspline_op(Dict, Fig, Ymax, Kind, S) :-
    get_dict(points, Dict, Pts),
    maplist(flip_point(Ymax), Pts, XpcePts),
    XpcePts = [P0|Rest],
    bspline_beziers(Rest, P0, Fig, Kind, S).

flip_point(Ymax, [X,Y0], point(X,Y)) :-
    Y is Ymax - Y0.

bspline_beziers([C1,C2,E|T], Start, Fig, Kind, S) :-
    !,
    new(B, bezier_curve(Start, E, C1, C2)),
    apply_paint(B, Kind, S),
    send(Fig, display, B),
    bspline_beziers(T, E, Fig, Kind, S).
bspline_beziers([], _, _, _, _).


                 /*******************************
                 *              TEXT            *
                 *******************************/

%!  text_op(+Op, +Fig, +Ymax, +State) is det.
%
%   xdot gives the baseline point `pt`, an alignment, and dot's own
%   estimate of the rendered `width`.  We compare xpce's actual
%   `<-width` against dot's width:
%
%     * within 3 pixels — leave the text at its native font size.
%     * otherwise — swap in a proportionally smaller font, so
%       xpce's rendered width matches dot's.  A non-uniform CTM
%       transform doesn't work: pango-cairo computes glyph advances
%       at native metrics but scales the outlines through the CTM,
%       giving thin glyphs at native spacing.  A uniform font-size
%       shrink is the least-glitchy way to make the label fit dot's
%       box.

text_op(Op, Fig, Ymax, gs(_Fill, Pen, Font, _, _, Flags)) :-
    get_dict(pt, Op, [Xd, Yd]),
    get_dict(text, Op, Str),
    get_dict(align, Op, Align),
    get_dict(width, Op, DotW),
    text_font(Font, Flags, TFont0),
    new(T, text(Str, left, TFont0)),
    get(T, width, XpceW),
    fit_font(TFont0, Font, Flags, XpceW, DotW, TFont),
    (   TFont \== TFont0
    ->  send(T, font, TFont)
    ;   true
    ),
    (   Pen \== none
    ->  send(T, colour, Pen)
    ;   true
    ),
    (   Flags /\ 4 =\= 0
    ->  send(T, underline, @on)
    ;   true
    ),
    send(Fig, display, T),
    get(T, width, TW),
    text_center_at(Align, TW, TFont, Xd, Yd, Ymax, Cx, Cy),
    send(T, center, point(Cx, Cy)).

%!  fit_font(+TFont0, +BaseFont, +Flags, +XpceW, +DotW, -TFont) is det.
%
%   If xpce's rendered width differs from dot's designated width by
%   more than 3 pixels, return a font whose size has been scaled by
%   DotW/XpceW so the text renders to (approximately) DotW.  Otherwise
%   return TFont0 unchanged.

fit_font(TFont0, _, _, XpceW, DotW, TFont0) :-
    abs(XpceW - DotW) < 3, !.
fit_font(_, font(Family,_,Size0), Flags, XpceW, DotW, TFont) :-
    NewSize is Size0 * DotW / XpceW,
    text_font(font(Family, roman, NewSize), Flags, TFont).

text_center_at(Align, TW, Font, Xd, Yd, Ymax, Cx, Cy) :-
    get(Font, ascent, Ascent),
    get(Font, height, FH),
    align_x_offset(Align, TW, DX),
    Cx is Xd + DX,
    Cy is Ymax - Yd - Ascent + FH/2.

%!  text_font(+Font, +Flags, -TextFont) is det.
%
%   Return a font reflecting the bold/italic bits in Flags.  XPCE fonts
%   take `style` and `weight` separately, so bold+italic → style=italic
%   + weight=bold via the 4-arg font constructor.

text_font(font(Family, _, Size), Flags, TFont) :-
    text_style_weight(Flags, Style, Weight),
    new(TFont, font(Family, Style, Size, Weight)).

text_style_weight(Flags, italic, bold)   :- Flags /\ 3 =:= 3, !.
text_style_weight(Flags, italic, normal) :- Flags /\ 2 =\= 0, !.
text_style_weight(Flags, bold,   bold)   :- Flags /\ 1 =\= 0, !.
text_style_weight(_,     roman,  normal).

align_x_offset("l", TW, DX) :- DX is  TW/2.
align_x_offset("c", _,  0).
align_x_offset("r", TW, DX) :- DX is -TW/2.


                 /*******************************
                 *             IMAGE            *
                 *******************************/

image_op(Op, Fig, Ymax) :-
    get_dict(pt, Op, [X, Yd]),
    get_dict(size, Op, [W, H]),
    get_dict(name, Op, Name),
    Y is Ymax - Yd - H,
    atom_string(Atom, Name),
    catch(
        ( new(BM, bitmap(image(Atom))),
          send(BM, size, size(W, H)),
          send(Fig, display, BM, point(X, Y))
        ),
        _,
        true).                       % missing images: quietly skip


                 /*******************************
                 *              PAINT           *
                 *******************************/

%!  apply_paint(+Graphical, +Kind, +State) is det.
%
%   Kind is `filled` (uppercase xdot op  —   fill  *and* stroke with the
%   current pen) or `stroked` (lowercase —   stroke only). In both cases
%   the stroke uses the current pen colour,  width and texture; a `none`
%   colour is skipped.

apply_paint(G, filled, gs(Fill, Pen, _, Style, Width, _)) :-
    apply_fill(G, Fill),
    apply_stroke(G, Pen, Style, Width).
apply_paint(G, stroked, gs(_, Pen, _, Style, Width, _)) :-
    apply_stroke(G, Pen, Style, Width).

apply_fill(_, none) :- !.
apply_fill(G, Fill) :- send(G, fill, Fill).

apply_stroke(G, none, _, _) :-
    !,
    send(G, pen, 0).
apply_stroke(G, Pen, Style, Width) :-
    send(G, colour, Pen),
    send(G, pen, Width),
    texture(Style, Tex),
    (   Tex \== @default
    ->  send(G, texture, Tex)
    ;   true
    ).

texture(dashed, dashed).
texture(dotted, dotted).
texture(solid,  @default).
texture(invis,  @default).
