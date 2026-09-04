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

:- module(tab_frame_demo,
          [ tab_frame_demo/0
          ]).
:- encoding(utf8).
:- use_module(library(pce)).
:- use_module(library(tabbed_window)).
:- use_module(library(tab_frame)).
:- use_module(library(pce_util), [send_list/3]).

/** <module> Tabs holding split windows

A tab_frame (see library(tab_frame)) is a  tab that lays its windows out
with a tile hierarchy, the way class  frame does for its members.  A tab
can therefore hold more than one window,  and this demo is there to play
with that: split a pane left/right or above/below, drag the line between
two panes to redistribute the space, move  a pane into another tab or out
into a window of its own, and drop it back.

The grip in the top-right corner of a pane   drags it onto another one: the
receiver splits and the dropped pane takes  the half the pointer is nearest
(see class split_handle).

Every pane carries a popup with all it  can do; the buttons at the top do
the same to the pane that has the  focus.  The split buttons go through
`window ->right' and `window ->below'.  Those work on a tab_frame for the
same reason they work on a frame: tile <-manager says who owns a hierarchy
and both managers answer ->attach_window and ->detach_window.

    ?- tab_frame_demo.
*/

tab_frame_demo :-
    send(new(tab_frame_demo_frame), open).


                 /*******************************
                 *            FRAME             *
                 *******************************/

:- pce_begin_class(tab_frame_demo_frame, frame,
                   "Demo of tabs holding split windows").

initialise(F) :->
    send_super(F, initialise, 'Tabs and splitting'),
    send(F, append, new(D, tab_frame_demo_dialog)),
    send(new(tab_frame_demo_tabs('', size(700,420))), below, D),
    send(F, tab),
    send(F, tab).

tabs(F, TW:tab_frame_demo_tabs) :<-
    "The tabbed window holding the tab_frames"::
    get(F, member, tab_frame_demo_tabs, TW).

tab(F, Tab:tab_frame) :<-
    "Add a tab holding a single pane"::
    get(F, tabs, TW),
    new_pane(pane, Pane),
    next_number(tab, N),
    format(atom(Label), 'Tab ~d', [N]),
    send(TW, tab, new(Tab, tab_frame(Pane, Label))).

tab(F) :->
    "Add a tab holding a single pane"::
    get(F, tab, _).

current(F, Pane:tab_frame_demo_pane) :<-
    "Pane that has the focus"::
    get(F, tabs, TW),
    get(TW, current, Pane).

:- pce_end_class(tab_frame_demo_frame).


:- pce_begin_class(tab_frame_demo_tabs, tabbed_window,
                   "Never leave the demo without a tab").

empty(TW) :->
    "The last tab was closed: start over"::
    send(TW?frame, tab).

:- pce_end_class(tab_frame_demo_tabs).


                 /*******************************
                 *           TOOLBAR            *
                 *******************************/

:- pce_begin_class(tab_frame_demo_dialog, dialog,
                   "Act on the pane that has the focus").

initialise(D) :->
    send_super(D, initialise),
    send(D, pen, 0),
    send_list(D, append,
              [ button(split_right, message(D, act, right)),
                button(split_below, message(D, act, below)),
                button(new_tab,     message(D, act, new_tab)),
                button(detach,      message(D, act, detach)),
                button(close_pane,  message(D, act, close)),
                button(new_window,  message(@prolog, tab_frame_demo))
              ]),
    send(D, append,
         label(hint, 'Drag the line between two panes to resize them'),
         next_row).

act(D, What:name) :->
    "Run What on the pane that has the focus"::
    (   get(D?frame, current, Pane)
    ->  act(What, Pane)
    ;   send(D, report, warning, 'No pane has the focus')
    ).

act(right,   Pane) :- new_pane(split, New), send(New, right, Pane).
act(below,   Pane) :- new_pane(split, New), send(New, below, Pane).
act(new_tab, Pane) :- send(Pane, new_tab).
act(detach,  Pane) :- send(Pane, detach).
act(close,   Pane) :- send(Pane, close).

:- pce_end_class(tab_frame_demo_dialog).


                 /*******************************
                 *             PANE             *
                 *******************************/

:- pce_begin_class(tab_frame_demo_pane, picture,
                   "One pane of the demo").

variable(caption, text,         get, "Text naming me").
variable(handle,  split_handle, get, "Grip to drag me onto another pane").
variable(bin,     pane_handle,  get, "Button to throw me away").

:- pce_global(@tab_frame_demo_popup, make_pane_popup).

make_pane_popup(P) :-
    new(P, popup),
    Pane = @arg1,
    send_list(P, append,
              [ menu_item(split_right,        message(Pane, split, right)),
                menu_item(split_below,        message(Pane, split, below)),
                menu_item(new_tab,            message(Pane, new_tab)),
                menu_item(move_to_new_window, message(Pane, detach)),
                menu_item(close,              message(Pane, close))
              ]).

initialise(P, Label:name, Colour:[colour]) :->
    send_super(P, initialise, Label),
    (   Colour == @default
    ->  true
    ;   send(P, background, Colour)
    ),
    send(P, display, new(T, text(Label, center, bold))),
    send(T, colour, black),                % the tints are light
    send(P, slot, caption, T),
    send(P, display, new(H, split_handle)),
    send(P, slot, handle, H),
    send(P, display, new(B, pane_handle('tool/trashcan.svg',
                                        'Delete this pane'))),
    send(B, recogniser,
         click_gesture(left, '', single, message(P, close))),
    send(P, slot, bin, B),
    send(P, recogniser, popup_gesture(@tab_frame_demo_popup)),
    send(P, recogniser,
         click_gesture(left, '', single, message(P, expose_pane))).

resize(P) :->
    "Keep the caption in the middle and the buttons in the corner"::
    send_super(P, resize),
    get(P, size, size(W, H)),
    CX is W//2,
    CY is H//2,
    send(P?caption, center, point(CX, CY)),
    send(P?bin, place, P),              % the bin in the very corner, the
    get(P?bin, size, size(BW, _)),      % grip beside it
    Inset is BW+2,
    send(P?handle, place, P, Inset).

expose_pane(P) :->
    "Make me the pane the buttons act on"::
    get(P, container, tab_frame, TF),
    send(TF, current, P),
    send(P, report, status, 'Current pane: %s', P?name).

:- pce_group(actions).

split(P, Where:{right,below}) :->
    "Put a new pane next to me"::
    get(P, container, tab_frame, TF),
    split_direction(Where, Direction),
    new_pane(split, New),
    send(TF, split, New, P, Direction).

split_direction(right, vertically).
split_direction(below, horizontally).

new_tab(P) :->
    "Open another tab"::
    send(P?frame, tab).

detach(P) :->
    "Move me out into a window of my own"::
    new(F, frame(P?name)),
    send(F, append, P),
    send(F, open).

close(P) :->
    "Throw me away"::
    send(P, destroy).

:- pce_end_class(tab_frame_demo_pane).


                 /*******************************
                 *           HELPERS            *
                 *******************************/

:- dynamic number/2.

%!  next_number(+Kind, -N) is det.

next_number(Kind, N) :-
    (   retract(number(Kind, N0))
    ->  N is N0+1
    ;   N = 1
    ),
    assertz(number(Kind, N)).

%!  new_pane(+Label, -Pane) is det.
%
%   A fresh pane, named Label plus a serial number so that every pane in
%   the demo has a name and a colour of its own.

new_pane(Label, Pane) :-
    next_number(pane, N),
    format(atom(Name), '~w ~d', [Label, N]),
    pane_colour(N, Colour),
    new(Pane, tab_frame_demo_pane(Name, Colour)).

%!  pane_colour(+N, -Colour) is det.
%
%   Cycle over a few soft tints, so that panes are told apart at a glance
%   while they are moved around.

:- pce_global(@tab_frame_demo_colours, make_pane_colours).

make_pane_colours(Colours) :-
    new(Colours, chain),
    forall(member(rgb(R,G,B), [ rgb(238,243,250),
                                rgb(247,238,243),
                                rgb(238,247,239),
                                rgb(250,245,234),
                                rgb(240,238,248)
                              ]),
           send(Colours, append, colour(@default, R, G, B))).

pane_colour(N, Colour) :-
    get(@tab_frame_demo_colours, size, Len),
    I is (N-1) mod Len + 1,
    get(@tab_frame_demo_colours, nth1, I, Colour).
