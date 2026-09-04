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


:- module(test_fixed_graphicals, [test_fixed_graphicals/0]).
:- encoding(utf8).

/** <module> Graphicals of a window that do not scroll

`window ->display_fixed' puts a graphical in a layer painted after the
content and in the coordinates of what is on screen rather than of what
is being shown.  It is a graphical of the window in every other way --
<-device, ->compute, events -- but it stays where it is put however far
the window is scrolled, and it is not part of what the window can scroll
over.

That is what a grip in the corner of a pane needs: see class split_handle
in library(tab_frame).

Run with:

    swipl -g test_fixed_graphicals -t halt \
          packages/xpce/tests/test_fixed_graphicals.pl
*/

:- set_prolog_flag('SDL_VIDEODRIVER', dummy).

:- use_module(library(pce)).
:- use_module(library(plunit)).
:- use_module(library(pce_util), [chain_list/2]).
:- use_module(library(lists), [member/2]).

test_fixed_graphicals :-
    run_tests([ fixed_graphicals ]).

:- dynamic hit/1.

:- pce_begin_class(tfg_box, box, "Records where it was clicked").

variable(tag, name := anonymous, both, "Which one I am").

event(B, Ev:event) :->
    (   send(Ev, is_a, ms_left_down)
    ->  get(B, tag, Tag),
        get(Ev, position, B, point(X, Y)),
        assertz(test_fixed_graphicals:hit(Tag-X-Y))
    ;   send_super(B, event, Ev)
    ).

:- pce_end_class(tfg_box).

%!  window(-Picture, -Ordinary, -Fixed) is det.
%
%   An open window scrolling over a tall box, with one ordinary
%   graphical at 100,2 and one fixed graphical at 200,2.

window(P, A, B) :-
    new(F, frame('test_fixed_graphicals')),
    send(F, append, new(P, picture)),
    send(P, display, new(_, box(400, 800)), point(10, 10)),
    send(P, display, new(A, tfg_box(16, 16)), point(100, 2)),
    send(A, tag, ordinary),
    send(P, display_fixed, new(B, tfg_box(16, 16)), point(200, 2)),
    send(B, tag, fixed),
    send(F, open).

%!  click(+Picture, +X, +Y, -What) is det.
%
%   Post a click at a position on screen and say what it reached.

click(P, X, Y, What) :-
    retractall(hit(_)),
    ignore(send(P, post_event, event(ms_left_down, P, X, Y))),
    (   hit(What) ->  true ;  What = nothing ).

classes(Chain, Names) :-
    chain_list(Chain, List),
    findall(N, (member(G, List), get(G, class_name, N)), Names).


:- begin_tests(fixed_graphicals).

test(it_goes_into_a_chain_of_its_own, true(Names == [tfg_box])) :-
    window(P, _A, _B),
    get(P, fixed_graphicals, Chain),
    classes(Chain, Names).

test(and_not_among_the_ones_that_scroll, true(Names == [box, tfg_box])) :-
    window(P, _A, _B),
    get(P, graphicals, Chain),
    classes(Chain, Names).

test(the_window_is_still_its_device, true(Device == P)) :-
    window(P, _A, B),
    get(B, device, Device).

%       The point of the whole thing.

test(it_does_not_move_when_the_window_scrolls, true(After == Before)) :-
    window(P, _A, B),
    get(B, area, area(_, Before, _, _)),
    send(P, scroll_to, point(0, 300)),
    get(B, area, area(_, After, _, _)),
    get(P, visible, area(_, 300, _, _)).   % the viewport really moved

test(the_window_cannot_scroll_over_it, true(After == Before)) :-
    window(P, _A, B),
    get(P, bounding_box, area(_, _, _, Before)),
    send(B, set, @default, 4000),       % far below anything there is
    get(P, bounding_box, area(_, _, _, After)).

test(a_click_reaches_it, true(What == fixed-5-4)) :-
    window(P, _A, _B),
    click(P, 205, 6, What).

test(and_still_reaches_it_at_the_same_place_when_scrolled,
     true(What == fixed-5-4)) :-
    window(P, _A, _B),
    send(P, scroll_to, point(0, 300)),
    click(P, 205, 6, What).

test(while_a_graphical_that_scrolls_moves_away, true(What == nothing)) :-
    window(P, _A, _B),
    send(P, scroll_to, point(0, 300)),
    click(P, 105, 6, What).             % where the ordinary one used to be

test(erasing_it_takes_it_out_of_the_layer, true(Names == [])) :-
    window(P, _A, B),
    send(P, erase, B),
    get(P, fixed_graphicals, Chain),
    classes(Chain, Names).

%       <-content_area is where such a graphical belongs: what is visible
%       less any scrollbar the window draws itself.

test(content_area_is_visible_when_the_window_draws_no_bar,
     true(Same == true)) :-
    window(P, _A, _B),
    get(P, visible, area(_, _, VW, _)),
    get(P, content_area, area(_, _, CW, _)),
    (   VW =:= CW ->  Same = true ;  Same = false ).

test(and_less_the_bar_when_it_does, true(Narrower == true)) :-
    new(F, frame('test_fixed_graphicals')),
    send(F, append, new(P, picture)),
    send(P, display, new(SB, scroll_bar(P, vertical))),
    send(F, open),
    get(SB, width, BarW),               % a fraction of a pixel wide on a
    get(P, visible, area(_, _, VW, _)),  % scaled display
    get(P, content_area, area(_, _, CW, _)),
    (   CW < VW, VW-CW =< BarW+1
    ->  Narrower = true
    ;   Narrower = false
    ).

:- end_tests(fixed_graphicals).
