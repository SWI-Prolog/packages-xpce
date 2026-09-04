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


:- module(test_scrollbar, [test_scrollbar/0]).
:- encoding(utf8).

/** <module> Tests for the scrollbars of a decorated window

A window that carries scrollbars hands them the extent of what it holds,
and they hide themselves when all of it is in view.  What they are handed
is <-bounding_box, the union of the graphicals: these check that it is
brought up to date first.  A union that is not costs more than a wrong
bubble, because a scrollbar that shows itself takes space off the window
it belongs to.

Run with:

    swipl -g test_scrollbar -t halt \
          packages/xpce/tests/test_scrollbar.pl
*/

%  Set before library(pce) is loaded: the driver is picked when the
%  display is initialised, which loading xpce already does.

:- set_prolog_flag('SDL_VIDEODRIVER', dummy).

:- use_module(library(pce)).
:- use_module(library(plunit)).

test_scrollbar :-
    run_tests([ scrollbar_bubble
              ]).

                 /*******************************
                 *            HELPERS           *
                 *******************************/

%!  scrolling_picture(-Frame, -Picture, -Box) is det.
%
%   An open picture with scrollbars, holding a box near its origin.  The
%   frame is leaked: destroying it is unreliable on dummy-SDL.

scrolling_picture(F, P, B) :-
    new(F, frame('Test')),
    send(F, append, new(P, picture('', size(200,100)))),
    send(F, open),
    send(P, display, new(B, box(20,20)), point(10,10)).

%!  bubble(+Picture, -Displayed, -Length, -View) is det.
%
%   Ask the horizontal scrollbar of Picture what it makes of the picture.
%   It is computed here rather than waited for: a headless display paints
%   nothing, so nothing else ever asks.

bubble(P, Displayed, Length, View) :-
    get(P, decoration, D),
    get(D, horizontal_scrollbar, SB),
    send(SB, request_compute),
    send(SB, compute),
    get(SB, displayed, Displayed),
    get(SB, length, Length),
    get(SB, view, View).


:- begin_tests(scrollbar_bubble).

test(all_in_view_hides_the_bar) :-
    scrolling_picture(_F, P, _B),
    bubble(P, @off, _, _).

test(a_graphical_out_of_view_shows_it) :-
    scrolling_picture(_F, P, B),
    send(B, x, 400),
    bubble(P, @on, _, _).

test(taking_it_back_in_hides_it_again) :-
    scrolling_picture(_F, P, B),
    send(B, x, 400),
    bubble(P, @on, _, _),
    send(B, x, 10),
    bubble(P, @off, _, _).

%   The union is what the bubble is made of, so it has to be the union as
%   it is now.  A stale one leaves the bar on screen for good: it takes
%   space off the window, which resizes it, which does not put the union
%   back.

test(the_bubble_follows_the_graphicals) :-
    scrolling_picture(_F, P, B),
    bubble(P, _, L0, _),
    send(B, size, size(60,20)),
    bubble(P, _, L1, _),
    L1 =:= L0+40.

:- end_tests(scrollbar_bubble).
