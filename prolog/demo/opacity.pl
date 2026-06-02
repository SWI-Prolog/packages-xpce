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

:- module(opacity_demo,
          [ opacity_demo/0
          ]).

:- use_module(library(pce)).

/** <module> Demonstration of graphical <-opacity

Opens a window with six cells showing per-graphical opacity in
different scenarios.  A single slider drives the opacity of every
foreground graphical in the window so the effect can be explored
interactively:

  * image     — a bitmap blended over a striped background
  * primitives — box, ellipse, arc and text, all controlled together
  * banner    — translucent black banner over a striped backdrop
  * group     — figure with overlapping coloured circles; the whole
                figure is alpha-blended.  Children stay fully opaque
                in the offscreen composite, so overlaps don't darken.
  * per-child — the same three circles, each given its own opacity.
                Compare directly with the `group' cell to see why
                opacity belongs on the container.
  * watermark — oversized DRAFT text over a yellow panel

Run with:

    ?- opacity_demo.
*/

opacity_demo :-
    new(_, opacity_demo_frame).

:- pce_begin_class(opacity_demo_frame, frame,
                   "Frame with picture, opacity slider and reset button").

variable(controlled, chain, get,
         "Foreground graphicals whose opacity tracks the slider").

initialise(F) :->
    "Build the demo frame, populate cells, attach slider"::
    send(F, send_super, initialise, 'Opacity demo'),
    send(F, slot, controlled, new(chain)),

    send(F, append, new(P, picture('Opacity', size(720, 360)))),
    send(P, scrollbars, none),

    cell(F, P,   0,   0, 'image overlay',     image_cell),
    cell(F, P, 240,   0, 'primitives',        primitives_cell),
    cell(F, P, 480,   0, 'translucent banner',banner_cell),
    cell(F, P,   0, 180, 'figure (group)',    group_cell),
    cell(F, P, 240, 180, 'children (per)',    per_child_cell),
    cell(F, P, 480, 180, 'DRAFT watermark',   watermark_cell),

    new(D, dialog),
    send(D, below, P),

    send(D, append,
         new(S, slider(opacity, 0, 100, 50,
                       message(F, set_opacity, @arg1)))),
    send(S, drag, @on),
    send(S, width, 360),
    send(D, append, button(reset,
                           message(F, reset_slider, S)),
         right),
    send(D, append, button(quit, message(F, destroy)),
         right),

    %  Apply the initial slider position so cells open at 0.5.
    send(F, set_opacity, 50),

    send(F, open).


set_opacity(F, Pct:int) :->
    "Slider callback: update opacity for every controlled graphical"::
    Op is Pct / 100.0,
    send(F?controlled, for_all, message(@arg1, opacity, Op)).


reset_slider(F, S:slider) :->
    "Snap slider back to fully opaque and apply"::
    send(S, selection, 100),
    send(F, set_opacity, 100).


control(F, Gr:graphical) :->
    "Register a graphical so its opacity tracks the slider"::
    send(F?controlled, append, Gr).

:- pce_end_class.


		 /*******************************
		 *           CELL FRAME         *
		 *******************************/

:- meta_predicate cell(+,+,+,+,+,3).

cell(F, P, X, Y, Title, Goal) :-
    send(P, display, new(B, box(238, 178)), point(X, Y)),
    send(B, pen, 1),
    send(B, colour, colour(grey80)),
    LX is X+6, LY is Y+4,
    send(P, display, new(T, text(Title, left, font(sans, bold, 11))),
         point(LX, LY)),
    send(T, colour, colour(grey40)),
    call(Goal, F, P, point(X, Y)).


		 /*******************************
		 *            CELLS             *
		 *******************************/

%!  image_cell(+Frame, +Picture, +Origin) is det.
%
%   Display a scaled copy of @pce_image (the built-in SWI-Prolog
%   logo, 48x48) over a checker-board background.  The bitmap's
%   opacity is tracked by the slider.

image_cell(F, P, point(X0, Y0)) :-
    checker_panel(P, X0+8, Y0+25, 220, 140, 20, grey85, grey60),
    get(@pce_image, scale, size(120, 120), Big),
    BX is X0+(238-120)//2,
    BY is Y0+35,
    send(P, display, new(BM, bitmap(Big)), point(BX, BY)),
    send(F, control, BM).

checker_panel(P, X0, Y0, W, H, S, C1, C2) :-
    Nx is W // S, Ny is H // S,
    Nx1 is Nx - 1, Ny1 is Ny - 1,
    forall(( between(0, Ny1, J), between(0, Nx1, I) ),
           ( ( 0 is (I+J) mod 2 -> C = C1 ; C = C2 ),
             SX is X0 + I*S, SY is Y0 + J*S,
             send(P, display, new(Sq, box(S, S)), point(SX, SY)),
             send(Sq, fill, colour(C)),
             send(Sq, pen, 0) )).


%!  primitives_cell(+Frame, +Picture, +Origin) is det.
%
%   Four primitives of the same colour sitting on a grey panel.

primitives_cell(F, P, point(X0, Y0)) :-
    BY is Y0+50,
    send(P, display, new(Bg, box(220, 110)), point(X0+8, BY)),
    send(Bg, fill, colour(grey60)),
    %  Top row: shape primitives, smaller than before to leave a clean
    %  bottom row for the text sample.
    send(P, display, new(B,  box(30, 45)),  point(X0+30,  BY+10)),
    send(B,  fill, colour(navy_blue)),
    send(F, control, B),
    send(P, display, new(E, ellipse(45, 45)), point(X0+85, BY+10)),
    send(E, fill, colour(navy_blue)),
    send(F, control, E),
    send(P, display, new(A, arc(22, 0, 270)), point(X0+175, BY+33)),
    send(A, close, pie_slice),
    send(A, fill, colour(navy_blue)),
    send(F, control, A),
    %  Bottom row: text sample on its own line.
    send(P, display, new(T, text('Aa', left, font(sans, bold, 30))),
         point(X0+95, BY+60)),
    send(T, colour, colour(navy_blue)),
    send(F, control, T).


%!  banner_cell(+Frame, +Picture, +Origin) is det.
%
%   Striped backdrop with a black caption banner overlaid on top.  The
%   banner's opacity is controlled by the slider; the (opaque) caption
%   stays put on top.

banner_cell(F, P, point(X0, Y0)) :-
    forall(between(0, 10, I),
           ( SY is Y0 + 25 + I*14,
             ( 0 is I mod 2 -> Col = grey85 ; Col = grey55 ),
             send(P, display, new(S, box(220, 14)), point(X0+8, SY)),
             send(S, fill, colour(Col)),
             send(S, pen, 0) )),
    send(P, display, new(Banner, box(220, 50)), point(X0+8, Y0+75)),
    send(Banner, fill, colour(black)),
    send(F, control, Banner),
    send(P, display, new(Caption, text('photo caption',
                                       left, font(sans, bold, 14))),
         point(X0+30, Y0+90)),
    send(Caption, colour, colour(white)).


%!  group_cell(+Frame, +Picture, +Origin) is det.
%
%   A figure with three overlapping coloured circles.  The figure's
%   opacity is controlled by the slider — children composite opaque
%   first, then the whole group is alpha-blended.

group_cell(F, P, point(X0, Y0)) :-
    send(P, display, new(Fig, figure), point(X0+15, Y0+30)),
    three_circles(Fig, 0, 0),
    send(F, control, Fig).


%!  per_child_cell(+Frame, +Picture, +Origin) is det.
%
%   The same three circles on a plain device; each circle's opacity
%   is registered with the slider individually, so overlaps show
%   per-child blending.

per_child_cell(F, P, point(X0, Y0)) :-
    new(Dev, device),
    send(P, display, Dev, point(X0+15, Y0+30)),
    three_circles(Dev, 0, 0),
    send(Dev?graphicals, for_all,
         message(F, control, @arg1)).


three_circles(Dev, X0, Y0) :-
    send(Dev, display, new(C1, circle(80)), point(X0+10, Y0+10)),
    send(C1, fill, colour(red)),
    send(Dev, display, new(C2, circle(80)), point(X0+70, Y0+10)),
    send(C2, fill, colour(forest_green)),
    send(Dev, display, new(C3, circle(80)), point(X0+40, Y0+60)),
    send(C3, fill, colour(navy_blue)).


%!  watermark_cell(+Frame, +Picture, +Origin) is det.
%
%   Oversized DRAFT text over a yellow panel — classic translucent
%   watermark look.  Text opacity is controlled by the slider.

watermark_cell(F, P, point(X0, Y0)) :-
    send(P, display, new(Panel, box(220, 110)), point(X0+8, Y0+50)),
    send(Panel, fill, colour(light_goldenrod)),
    send(P, display, new(Fig, figure)),
    send(Fig, display, new(WM, text('DRAFT', left, font(sans, bold, 32)))),
    send(WM, colour, colour(black)),
    new(T, transform), send(T, rotate, 20), send(Fig, transform, T),
    send(Fig, center, Panel?center),
    send(F, control, WM).
