/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org/packages/xpce/
    Copyright (c)  2001-2026, University of Amsterdam
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

:- module(plotter, []).
:- use_module(axis, []).
:- use_module(library(pce)).
:- require([ default/3
           ]).

:- pce_begin_class(plotter, device,
                   "Diagram for graphs and barcharts").

variable(hover_values, bool := @off, get,
         "Show the values of the graphs below the pointer").

axis(P, Axe:plot_axis) :->
    "Associate an axis with the plotter"::
    get(Axe, type, Type),
    (   get(P, member, Type, Old)
    ->  free(Old)
    ;   true
    ),
    send(P, display, Axe),
    send(Axe, name, Type).

axis(P, Which:name, Axis:plot_axis) :<-
    "Get named axis"::
    get(P, member, Which, Axis).

x_axis(P, Axe:plot_axis) :<-
    "Find the X-axis (compatibility)"::
    get(P, member, x, Axe).

y_axis(P, Axe:plot_axis) :<-
    "Find the Y-axis (compatibility)"::
    get(P, member, y, Axe).

pixel_range(P, Dir:{x,y}, Range:tuple) :<-
    "Pixels covered in a direction"::
    (   Dir == x
    ->  get(P, x_axis, Axis)
    ;   get(P, y_axis, Axis)
    ),
    get(Axis, pixel_range, Range).

translate(P, X:num, Y:num, Point) :<-
    "Translate a coordinate"::
    get(P, member, x, XAxe),
    get(P, member, y, YAxe),
    get(XAxe, location, X, PX),
    get(YAxe, location, Y, PY),
    new(Point, point(PX, PY)).


translate_x(P, X:num, Xpoint:int) :<-
    "Translate an X- coordinate"::
    get(P, member, x, XAxe),
    get(XAxe, location, X, Xpoint).
translate_y(P, Y:num, Ypoint:int) :<-
    "Translate an Y- coordinate"::
    get(P, member, y, YAxe),
    get(YAxe, location, Y, Ypoint).

value_from_x(P, X:int, Value:num) :<-
    "Translate X- coordinate to value"::
    get(P, member, x, XAxe),
    get(XAxe, value_from_coordinate, X, Value).
value_from_y(P, Y:int, Value:num) :<-
    "Translate Y- coordinate to value"::
    get(P, member, y, YAxe),
    get(YAxe, value_from_coordinate, Y, Value).


translate_plot_point(P, PP:plot_point, Pt:point) :<-
    "Translate plot-point to physical point"::
    get(PP, x, X),
    get(PP, y, Y),
    get(P, translate, X, Y, Pt).

graph(P, Gr:plot_graph) :->
    "Display a graph on the plotter"::
    send(P, display, Gr).

clear(P) :->                            % TBD: how to decide what to remove?
    "Remove all graphs"::
    send(P?graphicals, for_all,
         if(or(message(@arg1, instance_of, plot_graph),
               message(@arg1, instance_of, plot_mark)),
            message(@arg1, free))).

modified_plot_axis(P, _A:[plot_axis]) :->
    "Trap changed axis parameters"::
    send(P?graphicals, for_all,
         if(message(@arg1, instance_of, plot_graph),
            message(@arg1, request_compute),
            if(message(@arg1, instance_of, plot_mark),
               message(@arg1, modified)))),
    send(P, expose_member, x),
    send(P, expose_member, y).

expose_member(P, Name:name) :->
    "Expose named member"::
    (   get(P, member, Name, Gr)
    ->  send(Gr, expose)
    ;   true
    ).

                 /*******************************
                 *            HOVER             *
                 *******************************/

%   If <-hover_values is @on, moving the pointer over the plotter shows
%   a vertical line at the pointer with a box per graph that shows the
%   value of the graph there.  <-hover_value and <-hover_label define
%   the value and its text and may be refined by a subclass.

hover_values(P, Show:bool) :->
    "Show the values of the graphs below the pointer"::
    send(P, slot, hover_values, Show),
    (   Show == @off
    ->  send(P, hide_hover)
    ;   true
    ).

event(P, Ev:event) :->
    "Show the values below the pointer"::
    (   get(P, hover_values, @on)
    ->  (   send(Ev, is_a, area_exit)
        ->  send(P, hide_hover)
        ;   send(Ev, is_a, loc_move)
        ->  get(Ev, position, P, point(X, _)),
            send(P, show_hover, X)
        ;   true
        )
    ;   true
    ),
    send_super(P, event, Ev).

show_hover(P, X:int) :->
    "Show the values of the graphs at pixel X"::
    (   get(P, pixel_range, x, tuple(Min, Max)),
        between(Min, Max, X)
    ->  (   get(P, member, hover, H)
        ->  true
        ;   send(P, display, new(H, plot_hover))
        ),
        send(H, show, X),
        send(H, expose)
    ;   send(P, hide_hover)
    ).

hide_hover(P) :->
    "Remove the values shown by ->show_hover"::
    (   get(P, member, hover, H)
    ->  free(H)
    ;   true
    ).

update_hover(P) :->
    "Show the values again after the graphs have changed"::
    (   get(P, member, hover, H)
    ->  get(H, pointer_x, X),
        send(P, show_hover, X)
    ;   true
    ).

hover_graphs(P, Graphs:chain) :<-
    "The graphs whose value ->show_hover shows"::
    get(P?graphicals, find_all,
        and(message(@arg1, instance_of, path),
            @arg1?displayed == @on),
        Graphs).

hover_value(P, Graph:path, X:int, Value:num) :<-
    "Value of Graph at pixel X"::
    (   send(Graph, instance_of, plot_graph)
    ->  path_segment(Graph, X, P1, P2),
        get(P1, slot, x, X1), get(P1, y_value, V1),
        get(P2, slot, x, X2), get(P2, y_value, V2),
        interpolate(X, X1, V1, X2, V2, Value)
    ;   get(P, hover_y, Graph, X, Y),
        get(P, value_from_y, Y, Value)
    ).

hover_y(_P, Graph:path, X:int, Y:int) :<-
    "Pixel Y of Graph at pixel X"::
    get(Graph, offset, point(OX, OY)),
    PX is X - OX,
    path_segment(Graph, PX, P1, P2),
    get(P1, slot, x, X1), get(P1, slot, y, Y1),
    get(P2, slot, x, X2), get(P2, slot, y, Y2),
    interpolate(PX, X1, Y1, X2, Y2, Y0),
    Y is round(Y0) + OY.

hover_label(P, _Graph:path, Value:num, Label:char_array) :<-
    "Text for the value of Graph"::
    get(P, y_axis, YAxis),
    get(YAxis, format, Format0),
    get(@pce, convert, Format0, name, Format),
    (   sub_atom(Format, _, _, _, '%d')
    ->  V is round(Value)
    ;   V = Value
    ),
    get(YAxis, label_for_value, V, Text),
    get(Text?string, value, Label),
    free(Text).

%   path_segment(+Path, +X, -P1, -P2)
%
%   P1 and P2 are the points of Path on either side of pixel X.  We
%   use the slots, as a plot_point's <-x is its value.

path_segment(Path, X, P1, P2) :-
    get(Path, points, Points),
    chain_list(Points, List),
    append(_, [P1,P2|_], List),
    get(P1, slot, x, X1),
    get(P2, slot, x, X2),
    X >= min(X1, X2),
    X =< max(X1, X2),
    !.

interpolate(X, X1, V1, X2, V2, V) :-
    (   X1 =:= X2
    ->  V = V2
    ;   V is V1 + (V2-V1)*(X-X1)/(X2-X1)
    ).

:- pce_end_class.

                 /*******************************
                 *          PLOT-HOVER          *
                 *******************************/

:- pce_begin_class(plot_hover, device,
                   "Vertical line with the values of the graphs").

class_variable(line_colour, colour, grey50, "Colour of the vertical line").
class_variable(font,        font,   small,  "Font of the values").

variable(pointer_x, int := 0, get, "Pixel X shown").

initialise(H) :->
    send_super(H, initialise),
    send(H, name, hover).

show(H, X:int) :->
    "Show the line and the values of the graphs at pixel X"::
    send(H, slot, pointer_x, X),
    send(H, clear),
    get(H, device, P),
    get(P, pixel_range, y, tuple(Bottom, Top)),
    send(H, display, new(L, line(X, Top, X, Bottom))),
    send(L, colour, H?line_colour),
    get(P, hover_graphs, Graphs),
    chain_list(Graphs, GraphList),
    findall(Y-box(Colour, Label),
            ( member(G, GraphList),
              get(P, hover_y, G, X, Y),
              get(P, hover_value, G, X, Value),
              get(P, hover_label, G, Value, Label),
              get(G, display_colour, Colour)
            ), Boxes0),
    keysort(Boxes0, Boxes),
    get(P, pixel_range, x, tuple(_, Right)),
    send(H, value_boxes, Boxes, X, Right, Top, Bottom).

value_boxes(H, Boxes:prolog, X:int, Right:int, Top:int, Bottom:int) :->
    "Display the boxes, from top to bottom without overlap"::
    get(H, font, Font),
    get(Font, height, FH),
    BH is FH+2,
    Gap = 6,
    findall(BW, ( member(_-box(_, Label), Boxes),
                  get(Font, width, Label, TW),
                  BW is TW+8
                ), Widths),
    max_list([0|Widths], MaxW),
    (   X+Gap+MaxW > Right
    ->  Side = left
    ;   Side = right
    ),
    pairs_keys(Boxes, Ys),
    foldl(box_top(BH), Ys, Tops0, Top, End),
    (   Tops0 = [First|_]
    ->  Shift is max(0, min(End-1-Bottom, First-Top)),
        maplist(plus(Shift), Tops, Tops0)
    ;   Tops = []
    ),
    maplist(value_box(H, X, Side, Gap, BH, Font), Boxes, Tops).

%   box_top(+Height, +Y, -Top, +MinTop, -NextTop)
%
%   Top is the top of a box of Height centred on Y if that is not above
%   MinTop, the bottom of the box above it.

box_top(BH, Y, BY, MinY, NextY) :-
    BY is max(MinY, Y - BH//2),
    NextY is BY+BH+1.

value_box(H, X, Side, Gap, BH, Font, Y-box(Colour, Label), BY) :-
    send(H, display, new(Dot, circle(5))),
    send(Dot, fill, Colour),
    send(Dot, pen, 0),
    send(Dot, center, point(X, Y)),
    get(Font, width, Label, TW),
    BW is TW+8,
    (   Side == right
    ->  BX is X+Gap
    ;   BX is X-Gap-BW
    ),
    send(H, display, new(B, box(BW, BH)), point(BX, BY)),
    send(B, fill, Colour),
    send(B, pen, 0),
    send(B, radius, 3),
    (   get(Colour, intensity, I),
        I < 128
    ->  TextColour = white
    ;   TextColour = black
    ),
    send(H, display, new(T, text(Label, left, Font)), point(BX+4, BY+1)),
    send(T, colour, TextColour).

:- pce_end_class(plot_hover).

                 /*******************************
                 *          PLOT-POINT          *
                 *******************************/

:- pce_begin_class(plot_point(x_value, y_value), point, "Plotter point").

variable(modified,      bool := @on,    both,   "X/Y value is modified").
variable(x_value,       num,            get,    "X-value").
variable(y_value,       num,            get,    "Y-value").
variable(curve,         plot_graph,     get,    "Curve I'm associated with").

initialise(P, C:plot_graph, X:x=num, Y:y=num) :->
    "Create from X and Y"::
    send_super(P, initialise),
    send(P, slot, curve, C),
    send(P, slot, x_value, X),
    send(P, slot, y_value, Y),
    send(C, request_compute).

modified(P) :->
    "Indicate point and curve of modification"::
    (   get(P, slot, modified, @on)
    ->  true
    ;   send(P, slot, modified, @on),
        get(P, curve, Curve),
        send(Curve, request_compute)
    ).

compute(P) :->
    "Update the represented point"::
    (   get(P, modified, @on)
    ->  send(P, slot, modified, @off),
        get(P, curve, Curve),
        get(P, x_value, XVal),
        get(P, y_value, YVal),
        get(Curve, device, Plotter),
        get(Plotter, translate_x, XVal, X),
        get(Plotter, translate_y, YVal, Y),
        send(Curve, set_point, P, X, Y)
    ;   true
    ).

x(P, X:num) :->
    send(P, slot, x_value, X),
    send(P, modified).
y(P, Y:num) :->
    send(P, slot, y_value, Y),
    send(P, modified).

x(P, X:num) :<-
    get(P, slot, x_value, X).
y(P, Y:num) :<-
    get(P, slot, y_value, Y).

redundant(P) :->
    "Succeed if I'm right between my neighbours"::
    get(P, curve, Curve),
    get(Curve, kind, poly),
    get(Curve, points, Points),
    get(Points, previous, P, P1),
    get(Points, next, P, P2),
    get(P1, x, X1),
    get(P1, y, Y1),
    get(P2, x, X2),
    get(P2, y, Y2),
    get(P, x, X),
    get(P, y, Y),
    catch(abs(((Y-Y1)/(X-X1)) / ((Y2-Y1)/(X2-X1)) - 1) < 0.1,
          _,
          fail).

:- pce_end_class.

                 /*******************************
                 *          PLOT-GRAPH          *
                 *******************************/

:- pce_begin_class(plot_graph, path, "A graph for the plotter").

initialise(PG,
           Kind:'type=[{poly,smooth,points_only}]',
           Mark:'mark=[image]*') :->
    "Create from visualisation and mark"::
    default(Kind, poly, K),
    default(Mark, @nil, M),
    send_super(PG, initialise),
    send(PG, kind, K),
    send(PG, mark, M).


values(PG, Values:chain) :<-
    "Same as <-points"::
    get(PG, points, Values).


kind(PG, T:{poly,smooth,points_only}) :->
    (   T == points_only
    ->  send(PG, pen, 0)
    ;   send(PG, pen, 1),
        send_super(PG, kind, T)
    ).


append(PG, X:x=num, Y:y=num) :->
    "Append a plot_point to <-values"::
    send_super(PG, append, plot_point(PG, X, Y)).


compute(PG) :->
    "Update points"::
    send(PG?points, for_all, message(@arg1, compute)),
    send_super(PG, compute).

:- pce_end_class.


                 /*******************************
                 *           PLOT-MARK          *
                 *******************************/

:- pce_begin_class(plot_mark, device,
                   "Graphical mark on a graph").

variable(modified,      bool := @on,    none,   "X/Y value is modified").
variable(x_value,       num,            get,    "X-value").
variable(y_value,       num,            get,    "Y-value").

initialise(PM, X:x=num, Y:y=num, Img:image=[graphical]) :->
    send_super(PM, initialise),
    (   Img \== @default
    ->  send(PM, display, Img),
        send(Img, center, point(0,0))
    ;   true
    ),
    send(PM, slot, x_value, X),
    send(PM, slot, y_value, Y),
    send(PM, slot, modified, @on),
    send(PM, request_compute).

compute(PM) :->
    (   get(PM, slot, modified, @on),
        get(PM, device, Plotter),
        Plotter \== @nil
    ->  send(PM, slot, modified, @off),
        get(PM, x_value, XVal),
        get(PM, y_value, YVal),
        get(Plotter, translate_x, XVal, X),
        get(Plotter, translate_y, YVal, Y),
        send(PM, position, point(X,Y))
    ;   true
    ),
    send_super(PM, compute).

modified(P) :->
    "Indicate mark curve of modification"::
    (   get(P, slot, modified, @on)
    ->  true
    ;   send(P, slot, modified, @on),
        send(P, request_compute)
    ).

x_value(PM, X:num) :->
    send(PM, slot, x_value, X),
    send(PM, modified).

y_value(PM, Y:num) :->
    send(PM, slot, y_value, Y),
    send(PM, modified).

:- pce_end_class(plot_mark).
