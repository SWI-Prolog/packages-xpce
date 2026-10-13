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


:- module(canvas_demo,
          [ canvas_demo/0
          ]).
:- use_module(library(pce)).
:- use_module(demo_gallery, [gallery_save_buttons/3]).

/** <module> Drawing curves on a canvas

Opens a window with a class canvas on which one of three curves is
drawn using canvas->move_to and canvas->line_to:

  * rose          — r = cos(a/b*t)
  * spirograph    — the path of a pen on a wheel of radius b rolling
                    inside a ring of radius a+b
  * lissajous     — x = sin(a*t+pi/2), y = sin(b*t)

The sliders change the parameters and the line width.  The curve is
drawn with a solid colour or with a gradient (canvas->source).  As
the canvas records the drawing as vectors, the saved SVG and PDF files
are vector drawings.

Run with:

    ?- canvas_demo.
*/

canvas_demo :-
    new(F, frame('Canvas')),
    send(F, append, new(P, picture(@default, size(500, 400)))),
    send(P, scrollbars, none),
    new(C, canvas(500, 400)),
    send(P, display, C),
    send(new(D, dialog), below, P),
    send(D, append,
         new(Curve, menu(curve, choice,
                         message(@prolog, redraw, D)))),
    send_list(Curve, append, [rose, spirograph, lissajous]),
    send(D, append,
         new(Paint, menu(paint, choice, message(@prolog, redraw, D))),
         right),
    send_list(Paint, append, [solid, gradient]),
    send(Paint, selection, gradient),
    slider(D, a, 1, 12, 7),
    slider(D, b, 1, 12, 3),
    slider(D, line_width, 1, 8, 2),
    gallery_save_buttons(D, C, curve),
    send(D, append, button(quit, message(F, destroy)), right),
    send(D, attribute, canvas, C),
    send(P, resize_message, message(@prolog, resized, D, @arg2)),
    send(F, open).

%!  resized(+Dialog, +Size) is det.
%
%   The picture was resized: make the canvas fill it and redraw.

resized(D, Size) :-
    get(D, canvas, C),
    send(C, size, Size),
    redraw(D).

slider(D, Name, Low, High, Value) :-
    send(D, append,
         new(S, slider(Name, Low, High, Value,
                       message(@prolog, redraw, D)))),
    send(S, drag, @on).

%!  redraw(+Dialog) is det.
%
%   Clear the canvas and draw the curve selected in Dialog.

redraw(D) :-
    get(D, canvas, C),
    get(D, member, curve, CurveMenu), get(CurveMenu, selection, Curve),
    get(D, member, paint, PaintMenu), get(PaintMenu, selection, Paint),
    get(D, member, a, SA), get(SA, selection, A),
    get(D, member, b, SB), get(SB, selection, B),
    get(D, member, line_width, SW), get(SW, selection, LW),
    get(C, width, W), get(C, height, H),
    CX is W/2, CY is H/2+5,
    Radius is min(W, H)/2-30,
    send(C, clear),
    background(C, CX, CY, W),
    send(C, save_state),
    send(C, translate, CX, CY),
    curve_path(Curve, A, B, Radius, C),
    send(C, line_width, LW),
    send(C, stroke_style, stroke_style(round, round)),
    paint(Paint, Radius, C),
    send(C, stroke),
    send(C, restore_state),
    title(Curve, A, B, Title),
    send(C, source, colour(white)),
    send(C, text, Title, 12, 10, font(sans, normal, 13)).

background(C, CX, CY, W) :-
    new(G, gradient(radial, point(CX,CY), point(CX,CY), 0, W*0.66)),
    send(G, add_stop, 0, colour(@default, 40, 44, 70)),
    send(G, add_stop, 1, colour(@default, 8, 8, 16)),
    send(C, source, G),
    send(C, paint).

paint(solid, _, C) :-
    send(C, source, colour(gold)).
paint(gradient, R, C) :-
    new(G, gradient(linear, point(-R,-R), point(R,R))),
    send(G, add_stop, 0,   colour(deep_sky_blue)),
    send(G, add_stop, 0.5, colour(violet)),
    send(G, add_stop, 1,   colour(gold)),
    send(C, source, G).

title(rose, A, B, Title) :-
    format(string(Title), "<b>rose</b>   r = cos(~d/~d · t)", [A, B]).
title(spirograph, A, B, Title) :-
    R is A+B,
    format(string(Title), "<b>spirograph</b>   ring ~d, wheel ~d", [R, B]).
title(lissajous, A, B, Title) :-
    format(string(Title),
           "<b>lissajous</b>   x = sin(~d·t + π/2), y = sin(~d·t)", [A, B]).

%!  curve_path(+Curve, +A, +B, +Radius, +Canvas) is det.
%
%   Add the curve to the path of Canvas, scaled to Radius around the
%   origin.

curve_path(Curve, A, B, Radius, C) :-
    Steps = 3000,
    Period is 2*pi*B,
    forall(between(0, Steps, I),
           ( T is Period*I/Steps,
             curve_point(Curve, A, B, T, X0, Y0),
             X is X0*Radius, Y is Y0*Radius,
             (   I =:= 0
             ->  send(C, move_to, X, Y)
             ;   send(C, line_to, X, Y)
             )
           )),
    send(C, close_path).

%!  curve_point(+Curve, +A, +B, +T, -X, -Y) is det.
%
%   Point of the curve at T, in -1..1.

curve_point(rose, A, B, T, X, Y) :-
    R is cos(A/B*T),
    X is R*cos(T),
    Y is R*sin(T).
curve_point(spirograph, A, B, T, X, Y) :-
    Ring is A+B, D is B*0.8,
    Max is Ring-B+D,
    X is ((Ring-B)*cos(T) + D*cos((Ring-B)/B*T))/Max,
    Y is ((Ring-B)*sin(T) - D*sin((Ring-B)/B*T))/Max.
curve_point(lissajous, A, B, T, X, Y) :-
    X is sin(A*T+pi/2),
    Y is sin(B*T).
