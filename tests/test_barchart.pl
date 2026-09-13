/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org
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



:- module(test_barchart, [test_barchart/0]).
:- encoding(utf8).

/** <module> Bar labels of library(plot/barchart)

A bar_label is a figure holding the name of its bar, placed left of the
bar's <-center_base with a gap.  For a vertical bar the same layout is
rotated anti-clockwise by <-rotation degrees, so the name reads
bottom-to-top under the X-axis.  The rotation used to be done by drawing
the text into an image and rotating that, which limited the angle to
multiples of 90 and lost the anti-aliasing.

Run with:

    swipl -g test_barchart -t halt \
          packages/xpce/tests/test_barchart.pl
*/

:- set_prolog_flag('SDL_VIDEODRIVER', dummy).

:- use_module(library(pce)).
:- use_module(library(plunit)).
:- use_module(library('plot/barchart')).

test_barchart :-
    run_tests([ barchart_labels,
                barchart_label_rotation
              ]).

%!  chart(+Orientation, -Chart) is det.
%
%   An open window holding a bar_chart with two named bars.

chart(HV, BC) :-
    new(F, frame('test_barchart')),
    send(F, append, new(P, picture)),
    send(P, display, new(BC, bar_chart(HV, 0, 100, 200, 2))),
    send(BC, append, bar(short, 30, red)),
    send(BC, append, bar(longer, 60, green)),
    send(F, open),
    send(BC, compute).

%!  label(+Chart, +Name, -Label, -Area, -TextSize) is det.
%
%   The label of bar Name, its area in the chart and the size of the
%   text it holds.

label(BC, Name, L, area(X,Y,W,H), size(TW,TH)) :-
    send(BC, compute),
    get(BC, member, Name, Bar),
    get(Bar, label, L),
    get(L, area, area(X,Y,W,H)),
    get(L, text, T),
    get(T, width, TW),
    get(T, height, TH).

:- begin_tests(barchart_labels).

% Appending a bar to a vertical chart creates its rotated label.  This
% used to raise `no implementation for: ->transparent' and left the
% chart without any bars at all.

test(vertical_append, Names == [short, longer]) :-
    chart(vertical, BC),
    get(BC, bars, Bars),
    chain_list(Bars, List),
    maplist(bar_name, List, Names).

bar_name(Bar, Name) :-
    get(Bar, name, Name).

test(vertical_geometry) :-
    chart(vertical, BC),
    get(BC, member, longer, Bar),
    get(Bar, center_base, point(CX, CY)),
    label(BC, longer, _, area(X,Y,W,H), size(TW,TH)),
    assertion(W == TH),                     % text height across the bar
    assertion(H == TW),                     % text width down the chart
    assertion(X + W//2 =:= CX),             % centred on the bar
    assertion(Y =:= CY+5).                  % gap below the base

test(horizontal_geometry) :-
    chart(horizontal, BC),
    get(BC, member, longer, Bar),
    get(Bar, center_base, point(CX, CY)),
    label(BC, longer, L, area(X,Y,W,H), size(TW,TH)),
    assertion(get(L, transform, @nil)),     % not rotated
    assertion(W == TW),
    assertion(H == TH),
    assertion(X + W =:= CX-5),              % gap left of the base
    assertion(Y + H//2 =:= CY).

% The label is one graphical of the chart, so a click on it reaches it
% and can select the bar.  Its area is the bounding box of the rotated
% text, which is what hit detection needs.

test(pointed, Pointed == true) :-
    chart(vertical, BC),
    label(BC, longer, L, area(X,Y,W,H), _),
    PX is X+W//2, PY is Y+H//2,
    get(BC, pointed_objects, point(PX,PY), Chain),
    chain_list(Chain, List),
    (   memberchk(L, List)
    ->  Pointed = true
    ;   Pointed = List
    ).

:- end_tests(barchart_labels).

:- begin_tests(barchart_label_rotation).

rotation(Deg) :-
    send(class(bar_label), class_variable_value, rotation, Deg).

% ->font re-runs ->update_label.  The transform is reset first, so the
% rotation does not accumulate over updates.

test(font_update) :-
    chart(vertical, BC),
    label(BC, longer, L, _, _),
    forall(between(1, 3, _),
           send(L, font, font(screen, roman, 13))),
    label(BC, longer, _, area(_,_,W,H), size(TW,TH)),
    assertion(W == TH),
    assertion(H == TW).

% Any angle works now.  A label rotated by 45 degrees is as wide as it
% is high, give or take rounding.

test(rotate_45) :-
    setup_call_cleanup(
        rotation(45),
        ( chart(vertical, BC),
          label(BC, longer, _, area(_,_,W,H), size(TW,TH)),
          Expected is round((TW+TH)*sqrt(2)/2),
          assertion(abs(W-Expected) =< 2),
          assertion(abs(H-Expected) =< 2)
        ),
        rotation(90)).

% Rotating by 0 degrees is the horizontal layout.

test(rotate_0) :-
    setup_call_cleanup(
        rotation(0),
        ( chart(vertical, BC),
          label(BC, longer, _, area(_,_,W,H), size(TW,TH)),
          assertion(W == TW),
          assertion(H == TH)
        ),
        rotation(90)).

:- end_tests(barchart_label_rotation).
