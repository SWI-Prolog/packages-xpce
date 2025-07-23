/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org/projects/xpce/
    Copyright (c)  1996-2025, University of Amsterdam
                              SWI-Prolog Solutions b.v.
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

:- module(pce_colour_browser,
          [ colour_browser/0
          ]).
:- use_module(library(pce)).
:- require([ send_list/3
           ]).

/** <module> Colour browse demo

This demo implements a browser for the predefined colours.
*/

colour_browser :-
    make_colour_browser(CB),
    send(CB, open).

make_colour_browser(CB) :-
    make_colour_browser(CB, @colour_list).

make_colour_browser(CB, DataBase) :-
    new(D, dialog),
    send(D, append, new(M, menu(which, choice))),
    send_list(M, append, [foreground, background]),
    send(M, selection, background),
    send(new(CB, browser('XPCE (CSS) named colours')), below, D),
    send(CB, width, 40),
    send(CB, font, fixed),

    send(CB?key_binding, function,  % pretend arrow up/down not only
         cursor_down,               % do the preview, but also show
         and(message(CB, next_line),% the colour immediately
             message(CB, enter))),
    send(CB?key_binding, function,
         cursor_up,
         and(message(CB, previous_line),
             message(CB, enter))),

                                    % Make a picture with some different
                                    % graphicals for preview
    send(new(P, picture(size := size(100, 25))), below, CB),
    send(P, ver_stretch, 0),
    send(P, ver_shrink, 0),
    send(P, format, new(Fmt, format(vertical, 1, @on))),
    send(Fmt, adjustment, vector(center)),
    send(P, display, graphical(0, 0, 0, 25)),
    send(P, display, text(hello, left, normal)),
    send(P, display, text(hello, left, bold)),
    send(P, display, new(Box, box(30, 12))),
    send(P, display, line(0, 0, 30, 0)),
    send(Box, fill_pattern, foreground),

                                    % A colour is selected; call
                                    % selected_colour/3, using the
                                    % picture, fore/back ground and
                                    % the description of the colour
    send(CB, select_message,
         message(@prolog, selected_colour, P, M?selection, @arg1?label)),

                                    % set tab-stops for nice alignment
    send(CB?image, tab_stops, vector(200)),
                                    % scan de database
    send(DataBase, for_all,
         message(@prolog, append_colour, CB, @arg1)).

append_colour(CB, Name) :-
    get(@colour_names, member, Name, RGB),
%   A is (RGB >> 24),
    R is (RGB >> 16) /\ 255,
    G is (RGB >> 8) /\ 255,
    B is (RGB /\ 255),
    format(atom(HexName), '#~|~`0t~16r~2+~`0t~16r~2+~`0t~16r~2+', [R,G,B]),
    send(CB, append, string('%s\t%3d %3d %3d %s', Name, R, G, B, HexName)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
A colour was selected.  Change the colour attribute of the picture window.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

selected_colour(P, Which, Label) :-
    get(Label, scan, '%s%d%d%d', vector(NameStr, R, G, B)),
    send(@display, cut_buffer, 0, NameStr),
    send(P, Which, colour(@default, R, G, B)).
