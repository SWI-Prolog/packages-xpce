/*  Figures for debug.plx: "Development and debugging tools"
*/

:- module(fig_debug, []).
:- use_module(library(pce)).
:- use_module('../screenshots', [settle/0, settle/1, last_frame/1]).
:- use_module(library(pce_manual), []).
:- use_module(library(swi_ide), []).
:- use_module(library(man/v_visual), []).
:- use_module(library(man/v_inspector), []).
:- use_module(library(draw/draw), []).

:- multifile
    screenshot:figure/2,
    screenshot:figure_option/2.

%   \figref{vishierarchy}, section "Visual hierarchy tool": the visual
%   hierarchy tool examining PceDraw.  We open PceDraw with a small
%   drawing, open the tool in a window of its own, expand the PceDraw
%   frame and its canvas and select the box of the drawing.

screenshot:figure(vishierarchy, Tool) :-
    new(Draw, draw),
    send(Draw, open),
    settle,
    get(Draw, canvas, Canvas),
    send(Canvas, display, new(Box, draw_box(120, 60)), point(40, 40)),
    send(Canvas, display, draw_ellipse(100, 60), point(200, 40)),
    send(Canvas, display, draw_line(40, 140, 300, 140)),
    settle,
    new(Tool, vis_frame),
    send(@prolog_ide, place_tool, Tool, frame),
    get(Tool, frame, Frame),
    send(Frame, size, size(540, 400)),
    settle,
    get(Tool, window, V),
    send(V, visualise, Draw),
    send(V, expand_id, Draw),
    send(V, visualise, Canvas),
    send(V, expand_id, Canvas),
    send(V, visualise, Box),
    get(V, node, Box, Node),            % not Box: a graphical is taken
    send(V, selection, Node),           % as the node's image
    settle.


%   \figref{inspector}, section "Inspector tool": the inspector showing
%   a chain and two of its elements.  The arrows link a slot value to
%   the card of the object.  The inspector places object cards at random
%   if no position is given, so we give the positions.

screenshot:figure(inspector, Tool) :-
    new(Tool, isp_frame(@manual)),
    send(@prolog_ide, place_tool, Tool, frame),
    get(Tool, frame, Frame),
    send(Frame, size, size(600, 380)),
    settle,
    new(Chain, chain(point(10, 20), area(0, 0, 100, 50), hello)),
    send(Chain, lock_object, @on),
    get(Chain, nth1, 1, Point),
    get(Chain, nth1, 2, Area),
    get(Tool, member, isp_inspector_window, W),
    send(W, inspect, Chain, point(10, 10)),
    send(W, inspect, Point, point(40, 150)),
    send(W, inspect, Area, point(320, 150)),
    settle.
