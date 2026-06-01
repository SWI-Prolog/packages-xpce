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

    send(Scene, transform, new(transform)),

    new(D, dialog),
    send(D, below, P),

    send(D, append, new(Report, label(report, 'Click a child of the scene'))),
    send(F, slot, report, Report),

    send(D, append,
         new(SR, slider(rotation, -180, 180, 0,
                        message(F, set_rotation, @arg1))),
         next_row),
    send(SR, drag, @on),

    send(D, append,
         new(SS, slider(scale_x100, 25, 250, 100,
                        message(F, set_scale, @arg1))),
         next_row),
    send(SS, drag, @on),

    send(D, append,
         new(SH, slider(shear_x100, -100, 100, 0,
                        message(F, set_shear_x, @arg1))),
         next_row),
    send(SH, drag, @on),

    send(D, append, button(reset, message(F, reset_sliders, SR, SS, SH)),
         next_row),
    send(D, append, button(quit,  message(F, destroy))),

    send(F, open).


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


set_rotation(F, Degrees:int) :->
    "Slider callback: set absolute rotation"::
    rebuild_transform(F, Degrees, @default, @default).

set_scale(F, Percent:int) :->
    "Slider callback: set uniform scale (percent)"::
    rebuild_transform(F, @default, Percent, @default).

set_shear_x(F, Percent:int) :->
    "Slider callback: set horizontal shear (percent)"::
    rebuild_transform(F, @default, @default, Percent).

reset_sliders(F, SR:slider, SS:slider, SH:slider) :->
    "Reset sliders to identity and rebuild transform"::
    send(SR, selection, 0),
    send(SS, selection, 100),
    send(SH, selection, 0),
    rebuild_transform(F, 0, 100, 0).


%!  rebuild_transform(+Frame, +Rot, +Scale, +ShearX) is det.
%
%   Update the scene's transform.  Each argument may be @default to
%   keep that slider's current value (read off the dialog).

rebuild_transform(F, Rot0, Scale0, ShearX0) :-
    slider_int(F, rotation,   Rot0,    Rot),
    slider_int(F, scale_x100, Scale0,  Sc100),
    slider_int(F, shear_x100, ShearX0, Sx100),
    %  Compose around the scene's centre.
    pivot(Cx, Cy),
    Scale is Sc100 / 100.0,
    Shx   is Sx100 / 100.0,
    get(F?scene, transform, T),
    send(T, identity),
    send(T, translate, Cx, Cy),
    send(T, scale,     Scale),
    send(T, rotate,    Rot),
    send(T, shear,     Shx, 0.0),
    send(T, translate, -Cx, -Cy),
    %  Force a re-read by re-assigning the same transform; this also
    %  invalidates the scene's bounding box and triggers a repaint.
    send(F?scene, transform, T).

pivot(120, 90).         % center of the 240x180 box


slider_int(_F, _Name, Value, Value) :-
    integer(Value),
    !.
slider_int(F, Name, _, Value) :-
    get(F, member, dialog, D),
    get(D, member, Name, S),
    get(S, selection, Value).

:- pce_end_class.
