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



:- module(charts_demo,
          [ charts_demo/0
          ]).
:- encoding(utf8).

:- use_module(library(pce)).
:- use_module(library('plot/plotter')).
:- use_module(library('plot/axis')).
:- use_module(library('plot/barchart')).

/** <module> Gallery of the chart libraries

One scrollable window with a labelled cell per feature of the two
libraries that draw charts:

  - library(plot/plotter) with library(plot/axis) plots curves.  A
    `plotter` is a device holding one `plot_axis` per direction and a
    `plot_graph` per curve, plus `plot_mark` objects to annotate a
    value.  The axes do the scaling: the graphs are given values, not
    pixels.

  - library(plot/barchart) draws bar charts.  A `bar_chart` is a
    `plotter` holding `bar` objects, optionally collected in a
    `bar_stack` or a `bar_group`.  Bars can be edited with the mouse.

The bar charts respond to the mouse: hovering a bar shows its value in
a balloon and clicking its label selects it.  The last two cells edit
their bars as well, by dragging them and with a pair of buttons.

Run with:

    ?- charts_demo.
*/

cell_size(300, 250).                    % cell, including its caption
plot_size(215, 150).                    % axis lengths inside a cell
plot_origin(50, 42).                    % offset of the chart in a cell

%!  chart_cell(?Column, ?Row, ?Title, ?Goal)
%
%   Layout table: Goal draws the cell at Column/Row, which is called
%   as call(Goal, Picture, X, Y) with the cell's top-left corner.

chart_cell(0, 0, 'plot_graph: poly',        poly_cell).
chart_cell(1, 0, 'plot_graph: smooth',      smooth_cell).
chart_cell(2, 0, 'plot_graph: points_only', points_cell).
chart_cell(0, 1, 'plot_axis: grid lines',   grid_cell).
chart_cell(1, 1, 'plot_axis: log scale',    log_cell).
chart_cell(2, 1, 'plot_axis: own labels',   month_cell).
chart_cell(0, 2, 'bar_chart: vertical',     vbar_cell).
chart_cell(1, 2, 'bar_chart: horizontal',   hbar_cell).
chart_cell(2, 2, 'bar_chart: ->sort',       sort_cell).
chart_cell(0, 3, 'bar_stack',               stack_cell).
chart_cell(1, 3, 'bar_group',               group_cell).
chart_cell(2, 3, 'bar: drag me',            drag_cell).
chart_cell(0, 4, 'bar_button_group',        button_cell).

charts_demo :-
    cell_size(CW, CH),
    findall(C, chart_cell(C, _, _, _), Cols),
    findall(R, chart_cell(_, R, _, _), Rows),
    max_list(Cols, MaxCol),
    max_list(Rows, MaxRow),
    VW is min((MaxCol+1)*CW, 940),      % the gallery scrolls if it
    VH is min((MaxRow+1)*CH, 700),      % does not fit on the screen
    new(P, picture('Chart gallery', size(VW, VH))),
    send(P, scrollbars, both),
    send(P, restrict_scroll, @on),      % no scrolling past the cells
    forall(chart_cell(Col, Row, Title, Goal),
           ( X is Col*CW,
             Y is Row*CH,
             cell(P, X, Y, Title, Goal)
           )),
    send(P, open).

%!  cell(+Picture, +X, +Y, +Title, :Goal) is det.
%
%   Frame and caption a cell at (X,Y) and let Goal fill it.

:- meta_predicate cell(+,+,+,+,2).

cell(P, X, Y, Title, Goal) :-
    cell_size(CW, CH),
    BW is CW-2, BH is CH-2,
    send(P, display, new(B, box(BW, BH)), point(X, Y)),
    send(B, pen, 1),
    send(B, colour, colour(grey80)),
    LX is X+8, LY is Y+6,
    send(P, display, new(T, text(Title, left, font(sans, bold, 11))),
         point(LX, LY)),
    send(T, colour, colour(grey40)),
    call(Goal, P, X, Y).

%!  note(+Picture, +X0, +Y0, +Text) is det.
%
%   One line of small grey text at the bottom of a cell.

note(P, X0, Y0, Text) :-
    cell_size(_, CH),
    NX is X0+10, NY is Y0+CH-20,
    send(P, display, new(T, text(Text, left, font(sans, normal, 9))),
         point(NX, NY)),
    send(T, colour, colour(grey40)).


                 /*******************************
                 *        library(plotter)      *
                 *******************************/

%!  mk_plot(+Picture, +X0, +Y0, +XRange, +YRange, -Plotter) is det.
%
%   A plotter with an X and a Y axis in the cell at (X0,Y0).  Both
%   ranges are Low-High in the values of the data, not in pixels: the
%   axes translate, so a graph is fed values and redraws itself when an
%   axis changes.  The origin is put at the bottom-left of the plot so
%   that everything the plotter draws lands inside its cell.

mk_plot(P, X0, Y0, XLo-XHi, YLo-YHi, Plot) :-
    plot_at(P, X0, Y0, Plot),
    plot_size(PW, PH),
    send(Plot, axis, plot_axis(x, XLo, XHi, @default, PW, point(0, PH))),
    send(Plot, axis, plot_axis(y, YLo, YHi, @default, PH, point(0, PH))).

%!  plot_at(+Picture, +X0, +Y0, -Plotter) is det.
%
%   An empty plotter in the cell at (X0,Y0), for the cells that want
%   axes of their own.

plot_at(P, X0, Y0, Plot) :-
    plot_origin(OX, OY),
    PX is X0+OX, PY is Y0+OY,
    send(P, display, new(Plot, plotter), point(PX, PY)).

%!  graph(+Plotter, +Kind, +Colour, +Data, -Graph) is det.
%
%   Display a plot_graph of Kind holding the X-Y pairs in Data.  The
%   graph must be on the plotter before it is given points: a
%   plot_point asks its curve's device to translate its value.

graph(Plot, Kind, Colour, Data, G) :-
    send(Plot, graph, new(G, plot_graph(Kind))),
    send(G, colour, Colour),
    forall(member(X-Y, Data),
           send(G, append, X, Y)).

%!  legend(+Picture, +X0, +Y0, +Nth, +Colour, +Label) is det.
%
%   Nth colour sample plus its name, on the note line of the cell.

legend(P, X0, Y0, Nth, Colour, Label) :-
    cell_size(_, CH),
    LX is X0+10+Nth*92, LY is Y0+CH-20,
    send(P, display, new(L, line(LX, LY+6, LX+16, LY+6))),
    send(L, colour, Colour),
    send(L, pen, 2),
    send(P, display, new(T, text(Label, left, font(sans, normal, 9))),
         point(LX+21, LY)),
    send(T, colour, colour(grey30)).

%!  mark_image(+Colour, +Shape, -Image) is det.
%
%   A small mark for `plot_graph <-mark'.  The library takes an image
%   and centres it on every point, so any graphical will do: draw it
%   into a transparent image of the size wanted.

mark_image(Colour, Shape, I) :-
    new(I, image(@nil, 7, 7)),
    (   Shape == disc
    ->  new(Gr, circle(7))
    ;   new(Gr, box(7, 7))
    ),
    send(Gr, fill, Colour),
    send(Gr, pen, 0),
    send(I, draw_in, Gr),
    send(Gr, free).

wave(Phase, Data) :-
    findall(X-Y,
            ( between(0, 24, I),
              X is I/2.0,
              Y is 50+38*sin(X+Phase)
            ),
            Data).

samples([0-25, 2-58, 4-34, 6-79, 8-62, 10-93, 12-71]).

%!  poly_cell(+Picture, +X0, +Y0) is det.
%
%   The default `poly' graph: straight segments between the values.
%   Two graphs share the plotter and thus the axes.

poly_cell(P, X0, Y0) :-
    mk_plot(P, X0, Y0, 0-12, 0-100, Plot),
    wave(0, D1),
    wave(2.1, D2),
    graph(Plot, poly, colour(navy_blue), D1, _),
    graph(Plot, poly, colour(firebrick), D2, _),
    legend(P, X0, Y0, 0, colour(navy_blue), 'signal'),
    legend(P, X0, Y0, 1, colour(firebrick), 'reference').

%!  smooth_cell(+Picture, +X0, +Y0) is det.
%
%   `smooth' interpolates the same seven values with a Bezier path.
%   The grey `poly' graph behind it shows what it is smoothing.

smooth_cell(P, X0, Y0) :-
    mk_plot(P, X0, Y0, 0-12, 0-100, Plot),
    samples(D),
    graph(Plot, poly, colour(grey70), D, _),
    graph(Plot, smooth, colour(forest_green), D, G),
    send(G, pen, 2),
    note(P, X0, Y0, 'grey: the same values as poly').

%!  points_cell(+Picture, +X0, +Y0) is det.
%
%   `points_only' draws no line, just the <-mark at every value.  A
%   `poly' graph can carry a mark as well.

points_cell(P, X0, Y0) :-
    mk_plot(P, X0, Y0, 0-12, 0-100, Plot),
    samples(D),
    wave(0.5, W),
    graph(Plot, poly, colour(grey60), W, WG),
    mark_image(colour(grey60), square, WM),
    send(WG, mark, WM),
    graph(Plot, points_only, colour(dark_violet), D, G),
    mark_image(colour(dark_violet), disc, M),
    send(G, mark, M),
    note(P, X0, Y0, 'poly may carry a mark too').

%!  grid_cell(+Picture, +X0, +Y0) is det.
%
%   What an axis draws besides itself: `->lines' puts a dotted line
%   across the plot at every step (`all' at the small steps as well,
%   `none' at neither), `->format' says how a value is printed and
%   `->label' names the axis.

grid_cell(P, X0, Y0) :-
    mk_plot(P, X0, Y0, 0-12, 0-100, Plot),
    get(Plot, x_axis, XA),
    get(Plot, y_axis, YA),
    send(XA, lines, all),
    send(YA, lines, none),
    send(XA, format, '%d s'),
    send(YA, format, '%d%%'),
    send(XA, label, text('time', left, font(sans, normal, 9))),
    samples(D),
    graph(Plot, poly, colour(navy_blue), D, _),
    note(P, X0, Y0, 'x: lines all, y: lines none').

%!  log_cell(+Picture, +X0, +Y0) is det.
%
%   A logarithmic Y axis.  `->scale' can only be sent after creation,
%   by which time the step has been determined for a linear scale;
%   hence the explicit step, which on a log axis is the factor between
%   two ticks rather than the distance.  The low end must be > 0.

log_cell(P, X0, Y0) :-
    plot_at(P, X0, Y0, Plot),
    plot_size(PW, PH),
    send(Plot, axis, plot_axis(x, 0, 60, 20, PW, point(0, PH))),
    send(Plot, axis, new(YA, plot_axis(y, 1, 10000, 10, PH, point(0, PH)))),
    send(YA, scale, log),
    send(YA, format, '%d'),
    findall(X-Y,
            ( between(0, 12, I),
              X is I*5,
              Y is 8000*exp(-X/10.0)
            ),
            D),
    graph(Plot, poly, colour(firebrick), D, _),
    note(P, X0, Y0, 'decay: a straight line on a log axis').

                 /*******************************
                 *         OWN LABELS           *
                 *******************************/

%!  month_cell(+Picture, +X0, +Y0) is det.
%
%   An axis subclass that prints its own tick labels, and a plot_mark
%   annotating one value.  <-translate turns a value pair into a point
%   on the plotter, which is how the text beside the mark is placed.

month_cell(P, X0, Y0) :-
    plot_at(P, X0, Y0, Plot),
    plot_size(PW, PH),
    send(Plot, axis, month_axis(x, 1, 12, 3, PW, point(0, PH))),
    send(Plot, axis, plot_axis(y, 0, 100, @default, PH, point(0, PH))),
    rainfall(D),
    graph(Plot, poly, colour(navy_blue), D, _),
    findall(V-K, member(K-V, D), Pairs),
    max_member(PY-PX, Pairs),
    new(C, circle(7)),
    send(C, fill, colour(navy_blue)),
    send(C, pen, 0),
    send(Plot, display, new(_, plot_mark(PX, PY, C))),
    get(Plot, translate, PX, PY, point(MX, MY)),
    TX is MX+7, TY is MY-16,
    send(Plot, display,
         new(T, text('peak', left, font(sans, normal, 9))), point(TX, TY)),
    send(T, colour, colour(navy_blue)),
    note(P, X0, Y0, '<-label_for_value + plot_mark').

rainfall([1-62, 2-48, 3-55, 4-41, 5-58, 6-66,
          7-79, 8-92, 9-73, 10-70, 11-81, 12-75]).

:- pce_begin_class(month_axis, plot_axis,
                   "Axis labelled with month names").

label_for_value(A, Val:'int|real', Gr:graphical) :<-
    "Print the month name rather than its number"::
    get(A, tag_font, Font),
    Nth is round(Val),
    (   month(Nth, Name)
    ->  true
    ;   Name = ''
    ),
    new(Gr, text(Name, font := Font)).

:- pce_end_class(month_axis).

month( 1, 'Jan').  month( 2, 'Feb').  month( 3, 'Mar').
month( 4, 'Apr').  month( 5, 'May').  month( 6, 'Jun').
month( 7, 'Jul').  month( 8, 'Aug').  month( 9, 'Sep').
month(10, 'Oct').  month(11, 'Nov').  month(12, 'Dec').


                 /*******************************
                 *       library(barchart)      *
                 *******************************/

bar_scale(120, 20, 10).                 % scale length, bar width, gap

%!  mk_bars(+Picture, +X0, +Y0, +Orientation, +High, +NBars, -Chart) is det.
%
%   An empty bar_chart in the cell at (X0,Y0).  A bar_chart is a
%   plotter: it has one axis for the values and a base line the bars
%   stand on, so it needs room for the axis labels on one side and for
%   the bar labels on the other.

mk_bars(P, X0, Y0, Orientation, High, NBars, BC) :-
    bar_scale(SL, BW, BG),
    plot_origin(OX, OY),
    (   Orientation == vertical
    ->  PX is X0+OX
    ;   PX is X0+OX+10          % room for the labels left of the bars
    ),
    PY is Y0+OY,
    send(P, display,
         new(BC, bar_chart(Orientation, 0, High, SL, NBars, BW, BG)),
         point(PX, PY)).

%!  fruit(?Name, ?Value, ?Colour)

fruit(apple,  72, red).
fruit(pear,   48, forest_green).
fruit(plum,   93, dark_violet).
fruit(cherry, 35, firebrick).

add_bars(BC) :-
    forall(fruit(Name, Value, Colour),
           send(BC, append, bar(Name, Value, colour(Colour)))).

%!  vbar_cell(+Picture, +X0, +Y0) is det.
%
%   The plain case: one `bar' per value.  The bar names become
%   bar_label objects, rotated because the bars are vertical.

vbar_cell(P, X0, Y0) :-
    mk_bars(P, X0, Y0, vertical, 100, 4, BC),
    add_bars(BC),
    note(P, X0, Y0, 'a bar_label per bar, rotated').

%!  hbar_cell(+Picture, +X0, +Y0) is det.
%
%   The same bars laid on their side.  Only the chart's orientation
%   differs; it tells each bar and each label.

hbar_cell(P, X0, Y0) :-
    mk_bars(P, X0, Y0, horizontal, 100, 4, BC),
    add_bars(BC),
    note(P, X0, Y0, 'the same data, horizontal').

%!  sort_cell(+Picture, +X0, +Y0) is det.
%
%   ->sort reorders the bars by value and replaces them; the labels
%   follow their bar.

sort_cell(P, X0, Y0) :-
    mk_bars(P, X0, Y0, vertical, 100, 4, BC),
    add_bars(BC),
    send(BC, sort),
    note(P, X0, Y0, 'the same data, ->sort''ed').

%!  stack_cell(+Picture, +X0, +Y0) is det.
%
%   A bar_stack holds bars that accumulate: each is placed on top of
%   the previous one, so the stack shows both the parts and the total.

stack_cell(P, X0, Y0) :-
    mk_bars(P, X0, Y0, vertical, 100, 3, BC),
    forall(quarter(Q, Sales, Service, Support),
           send(BC, append,
                bar_stack(Q,
                          bar(sales,   Sales,   colour(steel_blue)),
                          bar(service, Service, colour(light_sea_green)),
                          bar(support, Support, colour(khaki))))),
    legend(P, X0, Y0, 0, colour(steel_blue),       'sales'),
    legend(P, X0, Y0, 1, colour(light_sea_green),  'service'),
    legend(P, X0, Y0, 2, colour(khaki),            'support').

%!  group_cell(+Picture, +X0, +Y0) is det.
%
%   A bar_group holds bars that are compared: they share a label and
%   are placed beside each other, narrower than a single bar.

group_cell(P, X0, Y0) :-
    mk_bars(P, X0, Y0, vertical, 100, 3, BC),
    forall(quarter(Q, Sales, Service, _),
           send(BC, append,
                bar_group(Q,
                          bar(planned, Sales,   colour(steel_blue)),
                          bar(actual,  Service, colour(orange))))),
    legend(P, X0, Y0, 0, colour(steel_blue), 'planned'),
    legend(P, X0, Y0, 1, colour(orange),     'actual').

quarter(q1, 38, 22, 15).
quarter(q2, 55, 30, 12).
quarter(q3, 47, 26, 18).

%!  drag_cell(+Picture, +X0, +Y0) is det.
%
%   A bar with a <->drag_message or a <->message can be edited: drag
%   its end and a plot_ruler shows the value being set.  drag_message
%   is forwarded while dragging, message once, on release.  Clicking a
%   bar label selects it (control-click adds to the selection).

drag_cell(P, X0, Y0) :-
    mk_bars(P, X0, Y0, vertical, 100, 3, BC),
    readout(P, X0, Y0, T),
    forall(quarter(Q, Sales, _, _),
           ( send(BC, append, new(B, bar(Q, Sales, colour(steel_blue)))),
             send(B, drag_message, message(@prolog, bar_dragged, T, Q, @arg1)),
             send(B, message, message(@prolog, bar_set, T, Q, @arg1))
           )),
    note(P, X0, Y0, 'drag a bar; click a label to select').

%!  readout(+Picture, +X0, +Y0, -Text) is det.
%
%   Empty text beside the cell's caption, for a cell that reports.

readout(P, X0, Y0, T) :-
    RX is X0+130, RY is Y0+6,
    send(P, display, new(T, text('', left, font(sans, bold, 11))),
         point(RX, RY)),
    send(T, colour, colour(firebrick)).

bar_dragged(Text, Name, Value) :-
    send(Text, string, string('%s -> %.0f', Name, Value)).

bar_set(Text, Name, Value) :-
    send(Text, string, string('%s = %.0f', Name, Value)).

%!  button_cell(+Picture, +X0, +Y0) is det.
%
%   A bar_button_group hangs a set of buttons off a bar and keeps them
%   there: it places itself at the bar's <-center_end, which is the
%   high end of the value axis rather than the end of the bar, so the
%   buttons stay put as the value changes.  The chart is horizontal
%   because for a vertical bar the group goes above the chart, where
%   this cell has no room.

button_cell(P, X0, Y0) :-
    mk_bars(P, X0, Y0, horizontal, 100, 3, BC),
    forall(quarter(Q, Sales, _, _),
           ( send(BC, append, new(B, bar(Q, Sales, colour(steel_blue)))),
             new(_, bar_button_group(B,
                                     button('-', message(@prolog, step_bar,
                                                         B, -10)),
                                     button('+', message(@prolog, step_bar,
                                                         B, 10))))
           )),
    note(P, X0, Y0, 'the buttons step their own bar').

%!  step_bar(+Bar, +Delta) is det.
%
%   Add Delta to the value of Bar, within the range of its chart.

step_bar(Bar, Delta) :-
    get(Bar, device, Chart),
    get(Chart, low, Low),
    get(Chart, high, High),
    get(Bar, value, V0),
    V is min(max(V0+Delta, Low), High),
    send(Bar, value, V).
