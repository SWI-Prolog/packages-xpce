/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
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

:- module(demo_terminal,
          [ demo_terminal/0
          ]).
:- use_module(library(ansi_term)).

/** <module> Visual demo of the xpce terminal SGR support

Run ?- demo_terminal. from the xpce/Epilog terminal to see every
rendered attribute: the four styles (bold, underline, inverse, link),
the 16 ANSI colors, the xterm-256 color cube and grayscale ramp, and
24-bit truecolor gradients.

All output goes through library(ansi_term) so the same goal also
exercises any other terminal you point it at.
*/

demo_terminal :-
    section("Attributes"),
    attributes,
    section("ANSI 8 colors"),
    ansi_8,
    section("Bright ANSI (hfg/hbg)"),
    ansi_16_bright,
    section("xterm-256 color cube (6x6x6)"),
    cube_256,
    section("xterm-256 grayscale ramp"),
    gray_256,
    section("24-bit (truecolor) channel ramps"),
    truecolor_channels,
    section("24-bit (truecolor) HSV rainbow"),
    truecolor_rainbow,
    section("Combinations"),
    combinations,
    section("Hyperlink"),
    hyperlink_demo.

section(Title) :-
    nl,
    ansi_format([bold,fg(default),bg(default)], "── ~s ──~n", [Title]).

%!  attributes is det.
%
%   Render the styles the xpce terminal supports.  Italic, faint and
%   blink are accepted by library(ansi_term) but currently not rendered
%   by the xpce terminal — they are listed here too so visitors can
%   confirm what's wired up on their host.

attributes :-
    ansi_format([bold],        "bold ",         []),
    ansi_format([underline],   "underline ",    []),
    ansi_format([negative],    "inverse ",      []),
    ansi_format([crossed_out], "strikethrough ",[]),
    ansi_format([bold,underline,negative,crossed_out],
                "all four ", []),
    ansi_format([italic],      "italic ",       []),
    ansi_format([faint],       "faint ",        []),
    ansi_format([blink(slow)], "blink",         []),
    nl.

%!  ansi_8 is det.
%!  ansi_16_bright is det.
%
%   Show fg + bg for the 8 base ANSI colors and the 8 bright variants.

ansi_8 :-
    forall(ansi_name(C),
           ansi_format([fg(C)], "~w ", [C])),
    nl,
    forall(ansi_name(C),
           ansi_format([fg(white),bg(C)], " ~w ", [C])),
    nl.

ansi_16_bright :-
    forall(ansi_name(C),
           ansi_format([hfg(C)], "~w ", [C])),
    nl,
    forall(ansi_name(C),
           ansi_format([fg(white),hbg(C)], " ~w ", [C])),
    nl.

ansi_name(black).
ansi_name(red).
ansi_name(green).
ansi_name(yellow).
ansi_name(blue).
ansi_name(magenta).
ansi_name(cyan).
ansi_name(white).

%!  cube_256 is det.
%
%   Print the 6x6x6 xterm cube as 6 rows of 6x6 cells (one row per red
%   level; columns step green, blocks of 6 step blue).

cube_256 :-
    forall(between(0, 5, R),
           ( forall(between(0, 5, G),
                    forall(between(0, 5, B),
                           ( N is 16 + 36*R + 6*G + B,
                             ansi_format([bg8(N)], "  ", [])
                           ))),
             nl
           )).

%!  gray_256 is det.
%
%   The 24-step grayscale ramp (xterm indices 232..255).

gray_256 :-
    forall(between(232, 255, N),
           ansi_format([bg8(N)], "  ", [])),
    nl.

%!  truecolor_channels is det.
%
%   A 64-step ramp per channel using fg(R,G,B).

truecolor_channels :-
    channel_ramp("R", 1, 0, 0),
    channel_ramp("G", 0, 1, 0),
    channel_ramp("B", 0, 0, 1).

channel_ramp(Label, RM, GM, BM) :-
    format("~s: ", [Label]),
    forall(between(0, 63, I),
           ( V is I*4,
             R is V*RM, G is V*GM, B is V*BM,
             ansi_format([bg(R,G,B)], " ", [])
           )),
    nl.

%!  truecolor_rainbow is det.
%
%   80-cell HSV rainbow at full saturation/value, demonstrating that
%   distinct interned truecolor entries don't fight for palette slots.

truecolor_rainbow :-
    forall(between(0, 79, I),
           ( H is I/80,
             hsv_to_rgb(H, 1.0, 1.0, R, G, B),
             ansi_format([bg(R,G,B)], " ", [])
           )),
    nl.

hsv_to_rgb(H, S, V, R, G, B) :-
    Hi is floor(H * 6) mod 6,
    F  is H * 6 - floor(H * 6),
    P  is V * (1 - S),
    Q  is V * (1 - F * S),
    T  is V * (1 - (1 - F) * S),
    sector_rgb(Hi, V, T, P, Q, Rf, Gf, Bf),
    R is round(Rf * 255),
    G is round(Gf * 255),
    B is round(Bf * 255).

sector_rgb(0, V, T, P, _, V, T, P).
sector_rgb(1, V, _, P, Q, Q, V, P).
sector_rgb(2, V, T, P, _, P, V, T).
sector_rgb(3, V, _, P, Q, P, Q, V).
sector_rgb(4, V, T, P, _, T, P, V).
sector_rgb(5, V, _, P, Q, V, P, Q).

%!  combinations is det.
%
%   Mix styles with extended color to exercise the bitfield packing.

combinations :-
    ansi_format([bold,fg(255,128,0)], "bold orange ", []),
    ansi_format([underline,fg8(51)], "underline cyan8 ", []),
    ansi_format([negative,bg(64,0,128)], "inverse purple-bg ", []),
    ansi_format([bold,underline,fg(0,255,128)], "all + green ", []),
    nl.

%!  hyperlink_demo is det.
%
%   Emit an OSC 8 hyperlink — the xpce terminal stores this as the
%   `link` bit on each cell and the painter renders the run as a
%   pickable link.

hyperlink_demo :-
    ansi_hyperlink(current_output,
                   'https://www.swi-prolog.org',
                   'SWI-Prolog homepage'),
    nl.
