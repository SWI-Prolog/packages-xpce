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


:- module(compose_demo,
          [ compose_demo/0
          ]).
:- use_module(library(pce)).
:- use_module(demo_gallery).

/** <module> Gallery of text formatting and compositing

Opens a window with labelled cells showing class markup_text and the
ways a figure can combine its graphicals with what is below it:

  * markup          — fonts, styles and colours in one text
  * wrap, justify   — Pango line wrapping at a width
  * alignment       — left, center and right
  * ellipsize       — where to put the `...' in a text that is too long
  * rotated text    — a markup_text on a rotated figure
  * clip_path       — a figure clipped to a path, also when it rotates
  * blend           — figure->blend, using the menu below the gallery
  * draw_markup     — markup and paths drawn from '_redraw_area'
  * svg export      — try the buttons (if XPCE supports SVG)!

Run with:

    ?- compose_demo.
*/

compose_demo :-
    gallery('Text and compositing gallery', 3, 3, P),
    gallery_cell(P, 0, 0, 'markup_text',      markup_cell),
    gallery_cell(P, 1, 0, 'wrap and justify', wrap_cell),
    gallery_cell(P, 2, 0, 'alignment',        alignment_cell),
    gallery_cell(P, 0, 1, 'ellipsize',        ellipsize_cell),
    gallery_cell(P, 1, 1, 'rotated text',     rotated_cell),
    gallery_cell(P, 2, 1, 'figure->clip_path', clip_cell),
    gallery_cell(P, 0, 2, 'figure->blend',    blend_cell),
    gallery_cell(P, 1, 2, 'draw_markup, draw_path', redraw_cell),
    gallery_cell(P, 2, 2, 'opacity',          opacity_cell),
    get(P?frame, member, dialog, D),
    send(D, append, new(M, menu(blend, cycle,
                                message(@prolog, set_blend, P, @arg1))),
         right),
    forall(blend_mode(Mode), send(M, append, Mode)),
    send(M, selection, multiply),
    send(P?frame, open).

blend_mode(normal).
blend_mode(multiply).
blend_mode(screen).
blend_mode(overlay).
blend_mode(darken).
blend_mode(lighten).
blend_mode(color_dodge).
blend_mode(color_burn).
blend_mode(hard_light).
blend_mode(soft_light).
blend_mode(difference).
blend_mode(exclusion).
blend_mode(hue).
blend_mode(saturation).
blend_mode(color).
blend_mode(luminosity).
blend_mode(add).

%!  markup_cell(+Picture, +X0, +Y0)

markup_cell(P, X0, Y0) :-
    forall(nth0(I, [ "<b>bold</b>, <i>italic</i> and <u>underlined</u>",
                     "<span foreground='firebrick'>colour</span> and \c
                      <span background='gold'>background</span>",
                     "<span size='x-large'>large</span> and \c
                      <span size='small'>small</span>",
                     "<tt>typewriter</tt>, <s>strike</s>, \c
                      H<sub>2</sub>O and x<sup>2</sup>",
                     "<span font='serif italic 14'>A serif font</span>"
                   ], Markup),
           ( Y is Y0+26+I*30,
             send(P, display, markup_text(Markup), point(X0+10, Y))
           )).

%!  wrap_cell(+Picture, +X0, +Y0)

wrap_cell(P, X0, Y0) :-
    new(T, markup_text("Pango wraps this paragraph at a width of \c
                        <b>210 pixels</b>.  With <i>justify</i>, the \c
                        spaces are stretched so that both edges \c
                        line up.", 210)),
    send(T, justify, @on),
    send(P, display, T, point(X0+12, Y0+28)).

%!  alignment_cell(+Picture, +X0, +Y0)

alignment_cell(P, X0, Y0) :-
    forall(nth0(I, [left, center, right], Align),
           ( format(string(M), "aligned <b>~w</b>", [Align]),
             new(T, markup_text(M, 200)),
             Y is Y0+35+I*44,
             send(P, display, new(B, box(200, 22)), point(X0+18, Y)),
             send(B, colour, colour(grey85)),
             send(T, alignment, Align),
             send(P, display, T, point(X0+18, Y+2))
           )).

%!  ellipsize_cell(+Picture, +X0, +Y0)

ellipsize_cell(P, X0, Y0) :-
    forall(nth0(I, [start, middle, end], Where),
           ( new(T, markup_text("A sentence that does not fit in the \c
                                 width of this cell", 150)),
             send(T, ellipsize, Where),
             Y is Y0+40+I*40,
             label(P, Where, X0+10, Y+2),
             send(P, display, T, point(X0+70, Y))
           )).

%!  rotated_cell(+Picture, +X0, +Y0)

rotated_cell(P, X0, Y0) :-
    forall(between(0, 4, I),
           ( Angle is -90+I*22.5,
             new(F, figure),
             Hue is I*60,
             hue_colour(Hue, Colour),
             format(string(M),
                    "<span foreground='~w'>rotated <b>~1f°</b></span>",
                    [Colour, Angle]),
             send(F, display, markup_text(M), point(30, -10)),
             rotate_at(P, F, X0+20, Y0+168, Angle)
           )).

hue_colour(Hue, Hex) :-
    new(C, colour(@default, Hue, 80, 70, @default, hsv)),
    get(C, red, R), get(C, green, G), get(C, blue, B),
    format(atom(Hex), '#~|~`0t~16r~2+~|~`0t~16r~2+~|~`0t~16r~2+',
           [R, G, B]).

%!  rotate_at(+Picture, +Figure, +X, +Y, +Angle) is det.
%!  rotate_at(+Picture, +Figure, +X, +Y, +Angle, +Shift) is det.
%
%   Display Figure with the origin of its contents, shifted by Shift,
%   at X,Y and rotated by Angle degrees around it.  The translation
%   comes first, so the rotation is around the translated origin.

rotate_at(P, F, X, Y, Angle) :-
    rotate_at(P, F, X, Y, Angle, point(0,0)).
rotate_at(P, F, X, Y, Angle, point(SX, SY)) :-
    PX is X, PY is Y,
    send(P, display, F),
    send(F, translate, PX, PY),
    send(F, rotate, Angle),
    send(F, translate, SX, SY).

%!  clip_cell(+Picture, +X0, +Y0)
%
%   Two figures holding coloured stripes, clipped to a star.  The second
%   figure is rotated; its clip path rotates with it.

clip_cell(P, X0, Y0) :-
    star(Star),
    forall(member(Angle-X, [0-15, 30-125]),
           ( new(F, figure),
             stripes(F),
             send(F, clip_path, Star),
             rotate_at(P, F, X0+X+50, Y0+95, Angle, point(-50, -50))
           )).

stripes(F) :-
    forall(between(0, 9, I),
           ( Hue is I*36,
             new(C, colour(@default, Hue, 70, 95, @default, hsv)),
             new(B, box(100, 10)),
             send(B, pen, 0),
             send(B, fill, C),
             Y is I*10,
             send(F, display, B, point(0, Y))
           )).

star(Star) :-
    findall(Cmd,
            ( between(0, 9, I),
              A is -pi/2 + I*pi/5,
              ( I mod 2 =:= 0 -> R = 50 ; R = 20 ),
              X is 50+R*cos(A), Y is 50+R*sin(A),
              ( I =:= 0 -> C = 'M' ; C = 'L' ),
              format(atom(Cmd), '~w ~2f ~2f', [C, X, Y])
            ), Cmds),
    atomic_list_concat(Cmds, ' ', Data0),
    atom_concat(Data0, ' Z', Data),
    new(Star, path),
    send(Star, svg_data, Data).

%!  blend_cell(+Picture, +X0, +Y0)
%
%   Three circles, each on a figure with a blend mode, over a gradient.
%   The blend menu below the gallery changes the mode.

blend_cell(P, X0, Y0) :-
    new(G, gradient(linear, point(0,0), point(1,0), units := bounding_box)),
    send(G, add_stop, 0, colour(white)),
    send(G, add_stop, 1, colour(grey30)),
    send(P, display, new(Back, box(220, 140)), point(X0+9, Y0+28)),
    send(Back, pen, 0),
    send(Back, fill, G),
    forall(member(X-Y-Colour, [ 45-35-cyan, 105-35-magenta, 75-85-yellow ]),
           ( new(F, figure),
             send(F, display, new(C, circle(80))),
             send(C, pen, 0),
             send(C, fill, colour(Colour)),
             send(F, blend, multiply),
             send(F, name, blend_figure),
             send(P, display, F, point(X0+X-20, Y0+Y-8))
           )).

set_blend(P, Mode) :-
    send(P?graphicals, for_all,
         if(@arg1?name == blend_figure,
            message(@arg1, blend, Mode))).

%!  redraw_cell(+Picture, +X0, +Y0)
%
%   A graphical that draws itself in '_redraw_area' using ->draw_path
%   and ->draw_markup.

redraw_cell(P, X0, Y0) :-
    send(P, display, new(B, badge), point(X0+20, Y0+30)),
    send(B, size, size(200, 130)).

:- pce_begin_class(badge, graphical,
                   "Graphical that draws a speech balloon").

'_redraw_area'(B, _A:area) :->
    get(B, area, area(X, Y, W, H)),
    balloon(X, Y, W, H, Balloon),
    send(B, graphics_state, 2, none, steel_blue),
    send(B, draw_path, Balloon, colour(alice_blue)),
    TW is W-28,
    send(B, draw_markup,
         "Drawn by <b>'_redraw_area'</b>\nusing <tt>->draw_path</tt>\n\c
          and <tt>->draw_markup</tt>",
         font(sans, normal, 11), X+14, Y+16, TW, center).

%   A speech balloon with rounded corners and a tail at the bottom-left
%   that fills the area X,Y,W,H.

balloon(X, Y, W, H, Balloon) :-
    BH is H-25,
    maplist(eval,
            [ X+12, Y, X+W-12, X+W, Y+12, Y+BH-12, X+W-12, Y+BH,
              X+60, X+35, Y+H, X+40, Y+BH, X+12, X, Y+BH-12, Y+12,
              X+12, Y ],
            Values),
    format(string(Data),
           "M ~w ~w H ~w A 12 12 0 0 1 ~w ~w V ~w A 12 12 0 0 1 ~w ~w \c
            H ~w L ~w ~w L ~w ~w H ~w A 12 12 0 0 1 ~w ~w V ~w \c
            A 12 12 0 0 1 ~w ~w Z",
           Values),
    new(Balloon, path),
    send(Balloon, svg_data, Data).

eval(Expr, Value) :-
    Value is Expr.

:- pce_end_class(badge).

%!  opacity_cell(+Picture, +X0, +Y0)
%
%   graphical<-opacity on a figure fades the figure as a whole, so the
%   overlap of its circles does not show.  Colours with alpha fade each
%   circle, so the overlap shows.

opacity_cell(P, X0, Y0) :-
    new(F, figure),
    forall(member(X, [0, 40]),
           ( send(F, display, new(C, circle(70)), point(X, 0)),
             send(C, pen, 0),
             send(C, fill, colour(steel_blue))
           )),
    send(F, opacity, 0.5),
    send(P, display, F, point(X0+10, Y0+40)),
    forall(member(X, [0, 40]),
           ( send(P, display, new(C, circle(70)), point(X0+X+120, Y0+40)),
             send(C, pen, 0),
             send(C, fill, colour(@default, 70, 130, 180, 128))
           )),
    label(P, 'figure opacity', X0+25, Y0+120),
    label(P, 'colour alpha', X0+145, Y0+120).

label(P, Text, X, Y) :-
    PX is X, PY is Y,
    send(P, display, new(T, text(Text, left, font(sans, normal, 10))),
         point(PX, PY)),
    send(T, colour, colour(grey30)).
