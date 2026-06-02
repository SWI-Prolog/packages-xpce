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

:- module(transform_demo,
          [ transform_demo/0
          ]).

:- use_module(library(pce)).

/** <module> Live demo of figure->transform

Opens a window with a small scene inside a transformed figure.  The
sliders rotate, scale and shear the scene around its centre, and
clicking on a child reports the local hit coordinate — showing that
events go through the inverse transform automatically.
*/

transform_demo :-
    new(_, transform_demo_frame).


:- pce_begin_class(transform_demo_frame, frame).

variable(scene,  figure, get, "Transformed figure with scene content").
variable(report, label,  get, "Status label").

initialise(F) :->
    "Build the demo frame"::
    send(F, send_super, initialise, 'figure->transform demo'),

    send(F, append, new(P, picture('Transformable scene', size(500, 420)))),
    send(P, scrollbars, none),

    new(Scene, figure),
    send(Scene, pen, 1),
    send(Scene, border, 4),
    fill_scene(Scene),
    send(P, display, Scene, point(130, 110)),
    send(F, slot, scene, Scene),

    new(D, dialog),
    send(D, below, P),
    send(D, append, new(Report, label(report, 'Click a child of the scene'))),
    send(F, slot, report, Report),

    Rebuild = message(F, rebuild),
    add_slider(D, rotation,   -180, 180,   0, Rebuild),
    add_slider(D, scale_x100,   25, 250, 100, Rebuild),
    add_slider(D, shear_x100, -100, 100,   0, Rebuild),

    send(D, append, button(reset, message(F, reset)), next_row),
    send(D, append, button(quit,  message(F, destroy))),

    send(F, open).

add_slider(D, Name, Lo, Hi, Init, Msg) :-
    send(D, append, new(S, slider(Name, Lo, Hi, Init, Msg)), next_row),
    send(S, drag, @on).

slider_value(F, Name, Value) :-
    get(F, member, dialog, D),
    get(D, member, Name, S),
    get(S, selection, Value).

set_slider(F, Name, Value) :-
    get(F, member, dialog, D),
    get(D, member, Name, S),
    send(S, selection, Value).


pivot(120, 90).                         % center of the 240x180 box


rebuild(F) :->
    "Rebuild the scene's transform from the current slider positions"::
    slider_value(F, rotation,   Deg),
    slider_value(F, scale_x100, Sc100),
    slider_value(F, shear_x100, Sx100),
    Sc is Sc100 / 100.0,
    Sx is Sx100 / 100.0,
    pivot(Cx, Cy),
    Scene = F?scene,
    %  Start from identity and pre-compose around the pivot:
    %  translate(Cx,Cy) o scale o rotate o shear o translate(-Cx,-Cy).
    send(Scene, transform, new(transform)),
    send(Scene, translate,  Cx,  Cy),
    send(Scene, scale,      Sc),
    send(Scene, rotate,     Deg),
    send(Scene, shear,      Sx, 0.0),
    send(Scene, translate, -Cx, -Cy).


reset(F) :->
    "Reset sliders to identity and rebuild"::
    set_slider(F, rotation,   0),
    set_slider(F, scale_x100, 100),
    set_slider(F, shear_x100, 0),
    send(F, rebuild).

:- pce_end_class.


%!  fill_scene(+Figure) is det.
%
%   Populate Figure with a recognisable scene: yellow rectangle with a
%   red diagonal, a green circle, and a text label.  Each shape carries
%   a click recogniser that reports its local hit position.

fill_scene(Scene) :-
    send(Scene, display, new(Box, box(240, 180)), point(0, 0)),
    send(Box, fill, colour(yellow)),
    add_click_report(Box, box),

    send(Scene, display, new(L, line(0, 0, 240, 180))),
    send(L, colour, colour(red)),
    send(L, pen, 3),
    add_click_report(L, line),

    send(Scene, display, new(C, circle(80)), point(80, 50)),
    send(C, fill, colour(spring_green)),
    add_click_report(C, circle),

    send(Scene, display,
         new(T, text('drag the sliders!', center, font(sans, bold, 14))),
         point(120, 200)),
    add_click_report(T, text).


add_click_report(Gr, Tag) :-
    send(Gr, recogniser,
         click_gesture(left, '', single,
                       message(@prolog, report_click, Gr, Tag, @receiver))).


%   report_click(+Graphical, +Tag, +Receiver)
%
%   Called from the click_gesture.  @receiver is the graphical the
%   gesture is bound to; we walk up to its frame and update the label
%   with the click position in the graphical's local coordinates.

:- public report_click/3.
report_click(Gr, Tag, _Recv) :-
    get(@event, position, Gr, Loc),
    get(Loc, x, X),
    get(Loc, y, Y),
    get(Gr, frame, F),
    format(string(Msg), 'hit ~w at local (~w, ~w)', [Tag, X, Y]),
    send(F?report, selection, Msg).
