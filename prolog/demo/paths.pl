/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org/packages/xpce/
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


:- module(paths_demo,
          [ paths_demo/0
          ]).
:- use_module(library(pce)).
:- use_module(demo_gallery).

/** <module> Gallery of class path features

Opens a window with labelled cells, each showing one feature of class
path and the point classes that describe its segments:

  * curve_point   — quadratic and cubic Bezier curves and their control
                    points
  * arc_point     — the four arcs between two points selected by the
                    `large` and `sweep` flags
  * fill_rule     — a shape with a hole using move_point, filled using
                    the `winding` and `even_odd` rules
  * radius        — rounded corners
  * svg_data      — shapes defined using SVG path data
  * arrows        — arrows follow the direction of the curve
  * edit          — drag the anchors and control points of a curve
  * resize        — control points and arcs scale with the path
  * smooth        — the interpolated `smooth` kind, closed and filled

Run with:

    ?- paths_demo.
*/

paths_demo :-
    gallery('Path gallery', 3, 3, P),
    gallery_cell(P, 0, 0, 'curve_point',        curve_cell),
    gallery_cell(P, 1, 0, 'arc_point flags',    arc_cell),
    gallery_cell(P, 2, 0, 'move_point, fill_rule', fill_rule_cell),
    gallery_cell(P, 0, 1, 'radius',             radius_cell),
    gallery_cell(P, 1, 1, 'svg_data',           svg_cell),
    gallery_cell(P, 2, 1, 'arrows',             arrows_cell),
    gallery_cell(P, 0, 2, 'edit (drag the dots)', edit_cell),
    gallery_cell(P, 1, 2, 'resize',             resize_cell),
    gallery_cell(P, 2, 2, 'smooth',             smooth_cell),
    send(P?frame, open).

%!  new_path(-Path, +Points) is det.

new_path(P, Points) :-
    new(P, path),
    send_list(P, append, Points).

%!  curve_cell(+Picture, +X0, +Y0)
%
%   A quadratic curve (one control point) and a cubic curve (two
%   control points).  The dotted lines connect the control points to
%   the anchors.

curve_cell(P, X0, Y0) :-
    new_path(Q, [point(0,60), curve_point(90,60, point(45,-20))]),
    send(Q, pen, 2),
    send(Q, colour, colour(navy)),
    show(P, Q, X0+20, Y0+40),
    control_lines(P, X0+20, Y0+40, [0-60, 45-(-20), 90-60]),
    new_path(C, [point(0,60), curve_point(90,60, point(10,0), point(80,120))]),
    send(C, pen, 2),
    send(C, colour, colour(dark_green)),
    show(P, C, X0+130, Y0+40),
    control_lines(P, X0+130, Y0+40, [0-60, 10-0]),
    control_lines(P, X0+130, Y0+40, [80-120, 90-60]).

%!  arc_cell(+Picture, +X0, +Y0)
%
%   The four elliptical arcs from the same start to the same end point,
%   as in the SVG specification.

arc_cell(P, X0, Y0) :-
    forall(member(arc(Large, Sweep, Colour, Pen),     % small on top
                  [ arc(@on, @on, forest_green, 1),
                    arc(@on, @off, dark_orange, 1),
                    arc(@off, @on, red, 3),
                    arc(@off, @off, blue, 3)
                  ]),
           ( new_path(A, [point(0,0),
                          arc_point(50, 25, 40, 30, 0, Large, Sweep)]),
             send(A, pen, Pen),
             send(A, colour, colour(Colour)),
             show(P, A, X0+95, Y0+65)
           )),
    legend(P, X0+10, Y0+140,
           [red-'small, cw', blue-'small, ccw',
            forest_green-'large, cw', dark_orange-'large, ccw']).

%!  fill_rule_cell(+Picture, +X0, +Y0)
%
%   Two squares, the second a sub-path started by a move_point.  Using
%   the `winding` rule, the inner square is filled; using `even_odd`
%   it is a hole.

fill_rule_cell(P, X0, Y0) :-
    forall(nth0(I, [winding, even_odd], Rule),
           ( new_path(S, [ point(0,0), point(90,0), point(90,90),
                           point(0,90),
                           move_point(25,25), point(65,25), point(65,65),
                           point(25,65) ]),
             send(S, closed, @on),
             send(S, fill_rule, Rule),
             send(S, fill, colour(gold)),
             X is X0+20+I*110,
             show(P, S, X, Y0+30),
             label(P, Rule, X+10, Y0+130)
           )).

%!  radius_cell(+Picture, +X0, +Y0)
%
%   The same triangle with corner radius 0, 10 and 30.

radius_cell(P, X0, Y0) :-
    forall(nth0(I, [0, 10, 30], R),
           ( new_path(T, [point(30,0), point(65,90), point(0,90)]),
             send(T, closed, @on),
             send(T, radius, R),
             send(T, fill, colour(light_sky_blue)),
             X is X0+10+I*75,
             show(P, T, X, Y0+35),
             format(atom(L), 'radius ~w', [R]),
             label(P, L, X+5, Y0+135)
           )).

%!  svg_cell(+Picture, +X0, +Y0)
%
%   Shapes from the `d` attribute of an SVG <path> element.

svg_cell(P, X0, Y0) :-
    new(Heart, path),
    send(Heart, svg_data,
         "M 10,30 A 20,20 0,0,1 50,30 A 20,20 0,0,1 90,30 \c
          Q 90,60 50,90 Q 10,60 10,30 z"),
    send(Heart, fill, colour(firebrick)),
    show(P, Heart, X0+10, Y0+35),
    new(Gear, path),
    gear_data(Data),
    send(Gear, svg_data, Data),
    send(Gear, fill_rule, even_odd),
    send(Gear, fill, colour(slate_grey)),
    show(P, Gear, X0+120, Y0+35),
    label(P, 'path->svg_data', X0+70, Y0+150).

%   SVG path data for a gear with 8 teeth and a hole.

gear_data(Data) :-
    findall(Cmd,
            ( between(0, 15, I),
              A is I*pi/8,
              ( I mod 2 =:= 0 -> R = 45 ; R = 35 ),
              X is 50+R*cos(A), Y is 50+R*sin(A),
              ( I =:= 0 -> C = 'M' ; C = 'L' ),
              format(atom(Cmd), '~w ~2f ~2f', [C, X, Y])
            ), Cmds),
    atomic_list_concat(Cmds, ' ', Teeth),
    format(string(Data),
           "~w Z M 35 50 A 15 15 0 1 1 65 50 A 15 15 0 1 1 35 50 Z",
           [Teeth]).

%!  arrows_cell(+Picture, +X0, +Y0)
%
%   Arrows at the ends of curves and arcs point along the curve.

arrows_cell(P, X0, Y0) :-
    new_path(S, [point(0,90), curve_point(80,0, point(0,0)),
                 curve_point(180,90, point(150,0), point(120,100))]),
    send(S, arrows, both),
    send(S, pen, 2),
    show(P, S, X0+25, Y0+40),
    new_path(A, [point(0,0), arc_point(60,0, 30, @default, @default,
                                       @off, @off)]),
    send(A, arrows, second),
    send(A, colour, colour(red)),
    show(P, A, X0+85, Y0+150).

%!  edit_cell(+Picture, +X0, +Y0)
%
%   A cubic curve with draggable dots on its anchors (filled) and
%   control points (open).  Moving a dot updates the curve.

edit_cell(P, X0, Y0) :-
    new(End, curve_point(150, 70, point(40, 0), point(110, 130))),
    new_path(C, [point(0,70), End]),
    send(C, pen, 2),
    send(C, colour, colour(purple)),
    show(P, C, X0+40, Y0+30),
    get(C?points, head, Start),
    get(End, control1, C1),
    get(End, control2, C2),
    forall(member(dot(Pt, Fill),
                  [dot(Start, black), dot(End, black), dot(C1, @nil), dot(C2, @nil)]),
           ( new(Dot, path_dot(C, Pt)),
             (   Fill == @nil
             ->  true
             ;   send(Dot, fill, colour(Fill))
             ),
             send(P, display, Dot)
           )).

%!  resize_cell(+Picture, +X0, +Y0)
%
%   A path with a curve and an arc, and copies that are resized.  The
%   control points and the ellipse of the arc are scaled too.

resize_cell(P, X0, Y0) :-
    forall(member(SX-SY-DX, [1-1-0, 0.6-1.4-70, 1.4-0.6-115]),
           ( new_path(S, [point(0,40), curve_point(40,40, point(20,0)),
                          arc_point(40,80, 20)]),
             send(S, closed, @on),
             send(S, fill, colour(pale_green)),
             show(P, S, X0+15+DX, Y0+40),
             send(S, resize, SX, SY)
           )).

%!  smooth_cell(+Picture, +X0, +Y0)
%
%   A `smooth` path interpolates its points.  The marks show the
%   points.

smooth_cell(P, X0, Y0) :-
    new(S, path(smooth)),
    send_list(S, append, [point(0,40), point(60,0), point(160,30),
                          point(120,110), point(30,100)]),
    send(S, closed, @on),
    send(S, fill, colour(misty_rose)),
    show(P, S, X0+30, Y0+35),
    forall(member(X-Y, [0-40, 60-0, 160-30, 120-110, 30-100]),
           dot(P, X0+30+X, Y0+35+Y)).

		 /*******************************
		 *	      HELPERS		*
		 *******************************/

show(P, Path, X, Y) :-
    gallery_show(P, Path, X, Y).

%   Dotted lines between the points of a list, relative to X,Y.

control_lines(P, X, Y, Points) :-
    findall(point(PX,PY), ( member(DX-DY, Points),
                            PX is X+DX, PY is Y+DY ), Pts),
    new(L, path),
    send_list(L, append, Pts),
    send(L, texture, dotted),
    send(L, colour, colour(grey50)),
    send(P, display, L),
    forall(member(point(PX,PY), Pts), dot(P, PX, PY)).

dot(P, X, Y) :-
    DX is X-3, DY is Y-3,
    send(P, display, new(C, circle(6)), point(DX, DY)),
    send(C, fill, colour(grey50)),
    send(C, pen, 0).

label(P, Text, X, Y) :-
    PX is X, PY is Y,
    send(P, display, new(T, text(Text, left, font(sans, normal, 10))),
         point(PX, PY)),
    send(T, colour, colour(grey30)).

legend(P, X, Y, Items) :-
    forall(nth0(I, Items, Colour-Text),
           ( LX is X+(I mod 2)*110, LY is Y+(I // 2)*14,
             send(P, display, new(B, box(8, 8)), point(LX, LY+2)),
             send(B, fill, colour(Colour)),
             send(B, pen, 0),
             label(P, Text, LX+12, LY)
           )).

		 /*******************************
		 *	    DRAGGABLE DOTS	*
		 *******************************/

:- pce_begin_class(path_dot, circle,
                   "Draggable dot that moves a point of a path").

variable(path,  path,  get, "Path whose point we move").
variable(point, point, get, "Point (relative to the path) we move").

:- pce_global(@path_dot_recogniser, new(move_gesture(left))).

initialise(D, Path:path, Pt:point) :->
    send_super(D, initialise, 10),
    send(D, slot, path, Path),
    send(D, slot, point, Pt),
    send(D, colour, colour(purple)),
    get(Path, offset, Off),
    get(Pt, x, X), get(Pt, y, Y),
    get(Off, x, OX), get(Off, y, OY),
    CX is X+OX-5, CY is Y+OY-5,
    send(D, set, CX, CY).

event(D, Ev:event) :->
    (   send(@path_dot_recogniser, event, Ev)
    ->  true
    ;   send_super(D, event, Ev)
    ).

geometry(D, X:[int], Y:[int], W:[int], H:[int]) :->
    "Move the point of the path along with the dot"::
    send_super(D, geometry, X, Y, W, H),
    get(D, path, Path),
    get(D, point, Pt),
    get(Path, offset, Off),
    get(D?center, x, CX), get(D?center, y, CY),
    get(Off, x, OX), get(Off, y, OY),
    PX is CX-OX, PY is CY-OY,
    send(Pt, set, PX, PY),
    send(Path, request_compute).

:- pce_end_class(path_dot).
