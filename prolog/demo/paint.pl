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


:- module(paint_demo,
          [ paint_demo/0
          ]).
:- use_module(library(pce)).
:- use_module(demo_gallery).

/** <module> Gallery of fills and line styles

Opens a window with labelled cells, each showing a way to paint the
inside or the line of a graphical:

  * linear gradient — several stops along an axis
  * radial gradient — a highlight that makes a circle look like a ball
  * extend          — what a gradient paints beyond its stops
  * units           — a gradient in window coordinates and one relative
                      to the shape (bounding_box), each shared by three
                      boxes
  * image fill      — a repeated image, aligned with the window or with
                      the shape using box->fill_offset
  * alpha           — translucent colours
  * caps and joins  — stroke_style cap and join
  * dashes          — named and custom dash patterns
  * gradient lines  — stroke_style paint

Run with:

    ?- paint_demo.
*/

paint_demo :-
    gallery('Paint gallery', 3, 3, P),
    gallery_cell(P, 0, 0, 'linear gradient', linear_cell),
    gallery_cell(P, 1, 0, 'radial gradient', radial_cell),
    gallery_cell(P, 2, 0, 'gradient<-extend', extend_cell),
    gallery_cell(P, 0, 1, 'gradient<-units', units_cell),
    gallery_cell(P, 1, 1, 'image fill',      image_cell),
    gallery_cell(P, 2, 1, 'alpha',           alpha_cell),
    gallery_cell(P, 0, 2, 'caps and joins',  caps_cell),
    gallery_cell(P, 1, 2, 'dashes',          dashes_cell),
    gallery_cell(P, 2, 2, 'gradient lines',  gradient_lines_cell),
    send(P?frame, open).

%!  gradient(+Kind, +P0, +P1, +Stops, -Gradient, +Options) is det.
%
%   Create a gradient in `bounding_box` units with Stops, a list of
%   Fraction-ColourName.

bb_gradient(Kind, P0, P1, R0, R1, Stops, G) :-
    new(G, gradient(Kind, P0, P1, R0, R1, units := bounding_box)),
    add_stops(G, Stops).

add_stops(G, Stops) :-
    forall(member(F-C, Stops), send(G, add_stop, F, colour(C))).

%!  linear_cell(+Picture, +X0, +Y0)

linear_cell(P, X0, Y0) :-
    bb_gradient(linear, point(0,0), point(1,0), @default, @default,
                [ 0-red, 0.2-orange, 0.4-gold, 0.6-green, 0.8-blue,
                  1-purple ], G),
    send(P, display, new(B, box(200, 40)), point(X0+18, Y0+40)),
    send(B, fill, G),
    bb_gradient(linear, point(0,0), point(1,1), @default, @default,
                [0-white, 1-steel_blue], D),
    send(P, display, new(R, box(200, 60)), point(X0+18, Y0+100)),
    send(R, radius, 12),
    send(R, fill, D).

%!  radial_cell(+Picture, +X0, +Y0)
%
%   The start circle is small and off-centre, so the highlight looks
%   like a reflection on a ball.

radial_cell(P, X0, Y0) :-
    forall(member(X-D-Colour, [20-110-dark_orange, 140-70-forest_green]),
           ( bb_gradient(radial, point(0.35,0.35), point(0.5,0.5), 0, 0.5,
                         [0-white, 1-Colour], G),
             send(P, display, new(C, circle(D)), point(X0+X, Y0+35)),
             send(C, pen, 0),
             send(C, fill, G)
           )).

%!  extend_cell(+Picture, +X0, +Y0)
%
%   A gradient from red to blue over the middle fifth of each strip,
%   with the four values of gradient<-extend.

extend_cell(P, X0, Y0) :-
    forall(nth0(I, [none, pad, repeat, reflect], Extend),
           ( new(G, gradient(linear, point(0.4,0), point(0.6,0),
                             extend := Extend, units := bounding_box)),
             add_stops(G, [0-red, 1-blue]),
             Y is Y0+30+I*36,
             send(P, display, new(B, box(150, 26)), point(X0+75, Y)),
             send(B, fill, G),
             label(P, Extend, X0+10, Y+6)
           )).

%!  units_cell(+Picture, +X0, +Y0)
%
%   Top: one gradient in `user` units (window coordinates), shared by
%   three boxes; each box shows the part of the gradient where it is.
%   Bottom: the same in `bounding_box` units; each box shows the whole
%   gradient.

units_cell(P, X0, Y0) :-
    new(U, gradient(linear, point(X0,0), point(X0+230,0))),
    add_stops(U, [0-yellow, 1-red]),
    bb_gradient(linear, point(0,0), point(1,0), @default, @default,
                [0-yellow, 1-red], B),
    forall(member(Gradient-Y-Text, [U-30-user, B-100-bounding_box]),
           ( forall(member(X-W, [10-50, 70-70, 150-70]),
                    ( send(P, display, new(R, box(W, 40)),
                           point(X0+X, Y0+Y)),
                      send(R, fill, Gradient)
                    )),
             label(P, Text, X0+10, Y0+Y+43)
           )).

%!  image_cell(+Picture, +X0, +Y0)
%
%   An image used as fill is repeated.  By default the pattern is
%   aligned with the window, so a shape that moves shows another part
%   of it.  box->fill_offset aligns it with the box instead.

image_cell(P, X0, Y0) :-
    tile(Tile),
    send(P, display, new(B, box(100, 100)), point(X0+10, Y0+35)),
    send(B, radius, 15),
    send(B, fill, Tile),
    send(B, fill_offset, point(0,0)),
    new(Heart, path),
    send(Heart, svg_data,
         "M 10,30 A 20,20 0,0,1 50,30 A 20,20 0,0,1 90,30 \c
          Q 90,60 50,90 Q 10,60 10,30 z"),
    send(Heart, fill, Tile),
    send(P, display, Heart, point(X0+125, Y0+40)),
    label(P, 'fill_offset', X0+30, Y0+145),
    label(P, 'window aligned', X0+135, Y0+145).

%   A 16x16 image with a small flower pattern.

tile(Tile) :-
    new(Tile, image(@nil, 16, 16, pixmap)),
    send(Tile, fill, colour(lavender)),
    forall(( member(DX-DY, [0-0, 8-8]),
             member(X-Y-C, [3-1-red, 2-2-red, 4-2-red, 3-3-red,
                            3-2-gold])
           ),
           ( PX is X+DX, PY is Y+DY,
             send(Tile, pixel, PX, PY, colour(C))
           )).

%!  alpha_cell(+Picture, +X0, +Y0)
%
%   Three circles in colours with an alpha of 128 (half transparent).

alpha_cell(P, X0, Y0) :-
    forall(member(X-Y-R-G-B, [50-30-255-0-0, 100-30-0-160-0,
                              75-75-0-0-255]),
           ( send(P, display, new(C, circle(80)), point(X0+X, Y0+Y)),
             send(C, pen, 0),
             send(C, fill, colour(@default, R, G, B, 128))
           )).

%!  caps_cell(+Picture, +X0, +Y0)
%
%   Thick zig-zags with each join style and lines with each cap style.
%   The thin white line shows the points.

caps_cell(P, X0, Y0) :-
    forall(nth0(I, [miter, round, bevel], Join),
           ( zigzag(Z),
             send(Z, pen, 12),
             send(Z, texture, stroke_style(butt, Join)),
             X is X0+25+I*72,
             gallery_show(P, Z, X, Y0+40),
             zigzag(Thin),
             send(Thin, colour, colour(white)),
             gallery_show(P, Thin, X, Y0+40),
             label(P, Join, X+8, Y0+88)
           )),
    forall(nth0(I, [butt, round, square], Cap),
           ( Y is Y0+118+I*20,
             new(L, line(X0+90, Y, X0+190, Y)),
             send(L, pen, 12),
             send(L, texture, stroke_style(Cap)),
             send(P, display, L),
             new(T, line(X0+90, Y, X0+190, Y)),
             send(T, colour, colour(white)),
             send(P, display, T),
             label(P, Cap, X0+20, Y-6)
           )).

zigzag(Z) :-
    new(Z, path),
    send_list(Z, append, [point(0,40), point(25,0), point(50,40)]).

%!  dashes_cell(+Picture, +X0, +Y0)

dashes_cell(P, X0, Y0) :-
    forall(nth0(I, [ dotted, dashed, dashdot,
                     stroke_style(round, @default, vector(0, 8)),
                     stroke_style(butt, @default, vector(16, 4, 4, 4)),
                     stroke_style(round, @default, vector(10, 6))
                   ], Texture),
           ( Y is Y0+35+I*22,
             new(L, line(X0+20, Y, X0+215, Y)),
             (   atom(Texture)
             ->  send(L, pen, 2)
             ;   send(L, pen, 4)
             ),
             send(L, texture, Texture),
             send(P, display, L)
           )).

%!  gradient_lines_cell(+Picture, +X0, +Y0)
%
%   A spiral whose line is painted with a gradient using
%   stroke_style<-paint.

gradient_lines_cell(P, X0, Y0) :-
    new(S, path),
    forall(between(0, 120, I),
           ( A is I*pi/15,
             R is 2+I*0.55,
             X is R*cos(A), Y is R*sin(A),
             send(S, append, point(X, Y))
           )),
    bb_gradient(radial, point(0.5,0.5), point(0.5,0.5), 0, 0.6,
                [0-gold, 0.5-dark_orange, 1-dark_magenta], G),
    send(S, pen, 5),
    send(S, texture, stroke_style(round, round, @default, G)),
    send(P, display, S, point(X0+50, Y0+28)).

label(P, Text, X, Y) :-
    PX is X, PY is Y,
    send(P, display, new(T, text(Text, left, font(sans, normal, 10))),
         point(PX, PY)),
    send(T, colour, colour(grey30)).
