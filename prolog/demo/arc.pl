/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org/projects/xpce/
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

:- module(arc_demo,
          [ arc_demo/0
          ]).

:- use_module(library(pce)).

/** <module> Gallery of class arc features

Opens a single window with labelled cells, each showing one use of
class arc:

  * angles      — start_angle and size_angle conventions
  * close       — none, chord, pie_slice (with fill)
  * points      — ->points: arc from two endpoints and a curvature
  * connect     — ->connect_angle: arc joining two lines
  * arrows      — first_arrow / second_arrow
  * ellipse     — non-circular arc via <-size
  * sweep > 360 — over-wound arc
  * CW vs CCW   — sign of size_angle

Run with:

    ?- arc_demo.
*/

arc_demo :-
    new(P, picture('Arc gallery', size(720, 540))),
    send(P, scrollbars, none),
    cell(P,   0,   0, 'angles',          angles_cell),
    cell(P, 240,   0, 'close modes',     close_cell),
    cell(P, 480,   0, '->points',        points_cell),
    cell(P,   0, 180, '->connect_angle', connect_cell),
    cell(P, 240, 180, 'arrows',          arrows_cell),
    cell(P, 480, 180, 'elliptic <-size', ellipse_cell),
    cell(P,   0, 360, 'sweep > 360',     sweep_cell),
    cell(P, 240, 360, 'CW vs CCW',       direction_cell),
    cell(P, 480, 360, 'pie chart',       pie_cell),
    send(P, open).

%!  cell(+Picture, +X, +Y, +Title, :Goal) is det.
%
%   Place a labelled 238x178 frame at (X,Y) and call Goal(Picture, X, Y)
%   to draw the demo into the cell.

:- meta_predicate cell(+,+,+,+,2).

cell(P, X, Y, Title, Goal) :-
    send(P, display, new(B, box(238, 178)), point(X, Y)),
    send(B, pen, 1),
    send(B, colour, colour(grey80)),
    LX is X+6, LY is Y+4,
    send(P, display, new(T, text(Title, left, font(sans, bold, 11))),
         point(LX, LY)),
    send(T, colour, colour(grey40)),
    call(Goal, P, X, Y).

%!  angles_cell(+Picture, +X0, +Y0) is det.
%
%   Three quarter-arcs at the same centre showing that start_angle is
%   measured from the +x axis and size_angle is positive in the
%   counter-clockwise direction (with the screen y axis pointing down).

angles_cell(P, X0, Y0) :-
    CX is X0+120, CY is Y0+105,
    send(P, display, new(A1, arc(40,   0, 90)), point(CX, CY)),
    send(A1, colour, colour(red)),
    send(P, display, new(A2, arc(60,  90, 90)), point(CX, CY)),
    send(A2, colour, colour(forest_green)),
    send(P, display, new(A3, arc(80, 180, 90)), point(CX, CY)),
    send(A3, colour, colour(navy_blue)),
    DX is CX-2, DY is CY-2,
    send(P, display, new(D, circle(4)), point(DX, DY)),
    send(D, fill, colour(black)).

%!  close_cell(+Picture, +X0, +Y0) is det.
%
%   The same 270-degree arc drawn with each of the three closing
%   modes; the two closing modes are filled to make the closed
%   region visible.

close_cell(P, X0, Y0) :-
    Y  is Y0+90,
    X1 is X0+40,
    X2 is X0+115,
    X3 is X0+190,
    mk_close_arc(P, X1, Y, none,      @nil),
    mk_close_arc(P, X2, Y, chord,     colour(light_blue)),
    mk_close_arc(P, X3, Y, pie_slice, colour(light_goldenrod)),
    LY is Y0+160,
    LX1 is X1-15, LX2 is X2-15, LX3 is X3-25,
    send(P, display,
         new(text(none,      left, font(sans, normal, 9))), point(LX1, LY)),
    send(P, display,
         new(text(chord,     left, font(sans, normal, 9))), point(LX2, LY)),
    send(P, display,
         new(text(pie_slice, left, font(sans, normal, 9))), point(LX3, LY)).

mk_close_arc(P, X, Y, Close, Fill) :-
    send(P, display, new(A, arc(28, 30, 270)), point(X, Y)),
    send(A, close, Close),
    (   Fill == @nil
    ->  true
    ;   send(A, fill, Fill)
    ).

%!  points_cell(+Picture, +X0, +Y0) is det.
%
%   ->points/5 builds an arc from a start point, an end point and a
%   curvature (signed perpendicular bulge in pixels).  Three arcs
%   between the same two endpoints with different curvatures.

points_cell(P, X0, Y0) :-
    SX is X0+60,  SY is Y0+40,
    EX is X0+180, EY is Y0+150,
    forall(member(D-Col, [25-red, 10-forest_green, -25-navy_blue]),
           ( send(P, display, new(A, arc)),
             send(A, points, SX, SY, EX, EY, D),
             send(A, colour, colour(Col)) )),
    endpoint_dot(P, SX, SY),
    endpoint_dot(P, EX, EY).

endpoint_dot(P, X, Y) :-
    DX is X-3, DY is Y-3,
    send(P, display, new(D, circle(6)), point(DX, DY)),
    send(D, fill, colour(black)).

%!  connect_cell(+Picture, +X0, +Y0) is det.
%
%   ->connect_angle/2 places the arc at the intersection of two lines
%   and derives its start and size angles from the line directions.

connect_cell(P, X0, Y0) :-
    AX is X0+30,  AY is Y0+150,
    BX is X0+200, BY is Y0+50,
    CX is X0+30,  CY is Y0+50,
    DX is X0+200, DY is Y0+150,
    send(P, display, new(L1, line(AX, AY, BX, BY))),
    send(P, display, new(L2, line(CX, CY, DX, DY))),
    send(L1, pen, 1),
    send(L2, pen, 1),
    send(P, display, new(A, arc(30))),
    send(A, connect_angle, L1, L2),
    send(A, colour, colour(red)),
    send(A, pen, 2).

%!  arrows_cell(+Picture, +X0, +Y0) is det.
%
%   first_arrow / second_arrow attach arrows at the arc endpoints.
%   The cell shows arrows at three curvatures and one CW sweep —
%   the wing midpoint should sit on the arc in every case.

arrows_cell(P, X0, Y0) :-
    CX is X0+60,  CY is Y0+95,
    arrowed_arc(P, CX, CY, 50,  30, 240,  navy_blue,    [first, second]),
    DX is X0+155, DY is Y0+60,
    arrowed_arc(P, DX, DY, 28,   0, 200,  forest_green, [second]),
    EX is X0+185, EY is Y0+135,
    arrowed_arc(P, EX, EY, 22, 180,-260,  red,          [first, second]).

arrowed_arc(P, CX, CY, R, Start, Size, Col, Ends) :-
    send(P, display, new(A, arc(R, Start, Size)), point(CX, CY)),
    send(A, pen, 2),
    send(A, colour, colour(Col)),
    (   memberchk(first, Ends)
    ->  send(A, first_arrow, new(arrow))
    ;   true
    ),
    (   memberchk(second, Ends)
    ->  send(A, second_arrow, new(arrow))
    ;   true
    ).

%!  ellipse_cell(+Picture, +X0, +Y0) is det.
%
%   <-size is the bounding (half-)box of the underlying ellipse.  Set
%   a non-square <-size to get an elliptical arc.

ellipse_cell(P, X0, Y0) :-
    CX is X0+120, CY is Y0+100,
    send(P, display, new(A1, arc), point(CX, CY)),
    send(A1, size, size(90, 50)),
    send(A1, size_angle, 360),
    send(A1, colour, colour(forest_green)),
    send(P, display, new(A2, arc), point(CX, CY)),
    send(A2, size, size(40, 70)),
    send(A2, start_angle, -45),
    send(A2, size_angle, 270),
    send(A2, colour, colour(red)),
    send(A2, pen, 2).

%!  sweep_cell(+Picture, +X0, +Y0) is det.
%
%   size_angle is unconstrained — values beyond 360 just over-wind.

sweep_cell(P, X0, Y0) :-
    CX is X0+120, CY is Y0+100,
    send(P, display, new(A, arc(60, 0, 540)), point(CX, CY)),
    send(A, colour, colour(purple)),
    send(A, pen, 3).

%!  direction_cell(+Picture, +X0, +Y0) is det.
%
%   Positive size_angle sweeps counter-clockwise on screen; negative
%   sweeps clockwise.  Both start at angle 0 (the +x direction).

direction_cell(P, X0, Y0) :-
    CX is X0+120, CY is Y0+100,
    send(P, display, new(Apos, arc(60, 0, 210)), point(CX, CY)),
    send(Apos, colour, colour(forest_green)),
    send(Apos, pen, 2),
    send(P, display, new(Aneg, arc(40, 0, -210)), point(CX, CY)),
    send(Aneg, colour, colour(red)),
    send(Aneg, pen, 2).

%!  pie_cell(+Picture, +X0, +Y0) is det.
%
%   A small pie chart built from pie_slice arcs.

pie_cell(P, X0, Y0) :-
    CX is X0+120, CY is Y0+100,
    Slices = [ 40-red, 25-forest_green, 20-navy_blue, 15-orange ],
    pie_slices(P, CX, CY, 60, 0, Slices).

pie_slices(_, _, _, _, _, []).
pie_slices(P, CX, CY, R, Acc, [Pct-Col|T]) :-
    Size is Pct * 3.6,
    send(P, display, new(A, arc(R, Acc, Size)), point(CX, CY)),
    send(A, close, pie_slice),
    send(A, fill, colour(Col)),
    send(A, colour, black),
    Acc1 is Acc + Size,
    pie_slices(P, CX, CY, R, Acc1, T).
