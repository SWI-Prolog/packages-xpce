/*  Figures for libplot.plx: "Plotting graphs and barcharts"
*/

:- module(fig_libplot, []).
:- use_module(library(pce)).
:- use_module('../screenshots', [settle/0, settle/1, last_frame/1]).
:- use_module(library(plot/axis)).
:- use_module(library(autowin)).
:- use_module(library(plot/barchart)).
:- use_module(library(plot/demo)).

:- multifile
    screenshot:figure/2.

%   \figref{axis}: the example of the section "Painting axis".

screenshot:figure(axis, P) :-
    new(P, auto_sized_picture),
    send(P, display,
         plot_axis(x, 0, 100, @default, 400, point(40, 320))),
    send(P, display,
         plot_axis(y, 0, 500, @default, 300, point(40, 320))).

%   \figref{plotsine}: plot_function/0 of the section "Plotting
%   graphs".  The code in the guide is in library(plot/demo).

screenshot:figure(plotsine, Frame) :-
    plot_function,
    last_frame(Frame).

%   \figref{plotclasses}: barchart/0 of the section "Drawing barcharts
%   using plot/barchart", also from library(plot/demo).  To get a
%   more interesting picture, we first make xpce do some work by
%   opening and destroying the sine plotter.

screenshot:figure(plotclasses, Frame) :-
    plot_function,
    last_frame(Plotter),
    settle,
    send(Plotter, destroy),
    settle,
    barchart,
    last_frame(Frame),
    get(Frame, member, picture, W),
    fit_window(W).

%   \figref{boot}: class bar_button_group of the section "Drawing
%   barcharts using plot/barchart".  The guide has no code for this
%   figure.  We show a small decision support tool, scoring three
%   alternatives using stacked bars for the contribution of each
%   criterion, and a chart with the weight of each criterion.  Each
%   bar has a bar_button_group with buttons for the bar.

screenshot:figure(boot, F) :-
    new(F, frame('Multicriteria tool')),
    send(F, append, new(P, auto_sized_picture)),
    heading(P, 'Score per alternative', 10),
    send(P, display,
         new(Scores, bar_chart(horizontal, 0, 1.0, 200, 3)),
         point(120, 40)),
    forall(alternative(Name, Values),
           add_alternative(Scores, Name, Values)),
    get(P, bounding_box, area(_, _, _, H)),
    Y is H+30,
    heading(P, 'Weight per criterion', Y),
    send(P, display,
         new(Weights, bar_chart(horizontal, 0, 1.0, 200, 3)),
         point(120, Y+30)),
    forall(criterion(Name, Weight, Colour),
           add_criterion(Weights, Name, Weight, Colour)).

heading(P, Text, Y) :-
    send(P, display, text(Text, left, bold), point(10, Y)).

add_alternative(Chart, Name, Values) :-
    findall(C-Colour, criterion(C, _, Colour), Criteria),
    maplist(criterion_bar, Criteria, Values, Bars),
    Stack =.. [bar_stack, Name|Bars],
    send(Chart, append, new(S, Stack)),
    image_button(details, '16x16/binocular.png', B),
    new(_, bar_button_group(S, B)).

criterion_bar(C-Colour, Value, bar(C, Value, Colour)).

add_criterion(Chart, Name, Weight, Colour) :-
    send(Chart, append, new(Bar, bar(Name, Weight, Colour))),
    image_button(colour, '16x16/cpalette1.png', B1),
    image_button(graph,  '16x16/graph.png', B2),
    image_button(view,   '16x16/eye.png', B3),
    new(_, bar_button_group(Bar, B1, B2, B3)).

image_button(Name, File, B) :-
    new(B, button(Name)),
    send(B, label, image(File)).

alternative('Video course', [0.15, 0.30, 0.35]).
alternative('E-learning',   [0.20, 0.30, 0.05]).
alternative('Classroom',    [0.20, 0.25, 0.20]).

criterion(cost,     0.25, colour(forest_green)).
criterion(quality,  0.30, colour(firebrick)).
criterion(teaching, 0.45, colour(dark_violet)).

%!  fit_window(+Window) is det.
%
%   Scroll and resize Window to show all its graphicals, as the user
%   would do after opening it.

fit_window(W) :-
    send(W, compute),
    get(W, bounding_box, area(X, Y, BW, BH)),
    Border = 10,
    send(W, scroll_to, point(X-Border, Y-Border)),
    send(W, size, size(BW+2*Border, BH+2*Border)).
