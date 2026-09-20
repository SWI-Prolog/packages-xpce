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


:- module(test_popup_event, [test_popup_event/0]).
:- encoding(utf8).

/** <module> Asking for a popup menu

The right mouse button going down asks for a popup (context) menu.  On
MacOS, Control-left-click is the documented alternative for the
secondary click, so `event ->is_popup' accepts that as well and class
popup_gesture activates on it.  Code that opens a popup without using a
gesture (class list_browser, the toc and inspector recognisers, ...)
uses the same test.

Run with:

    swipl -g test_popup_event -t halt \
          packages/xpce/tests/test_popup_event.pl
*/

:- use_module(library(pce)).
:- use_module(library(plunit)).
:- use_module(library(apply)).
:- use_module(library(yall)).
:- use_module(library(debug)).

setup_headless :-
    set_prolog_flag('SDL_VIDEODRIVER', dummy).
:- initialization(setup_headless, now).

test_popup_event :-
    run_tests([ popup_event,
                popup_gesture,
                popup_handler,
                popup_toc,
                popup_terminal
              ]).

		 /*******************************
		 *	      SUPPORT		*
		 *******************************/

%!  buttons(+Spec, -Mask) is det.
%
%   Button mask for an event from a list of modifier and button names.

button(control, 0x001).
button(shift,   0x002).
button(meta,    0x004).
button(command, 0x008).
button(left,    0x010).
button(middle,  0x020).
button(right,   0x040).

buttons(Spec, Mask) :-
    foldl([N,M0,M]>>(button(N,B), M is M0\/B), Spec, 0, Mask).

%!  emulates_popup is semidet.
%
%   True if Control-left-click asks for a popup on this platform.

emulates_popup :-
    current_prolog_flag(apple, true).

make_picture(P, B) :-
    make_plain_picture(P, B),
    send(B, popup, new(Popup, popup(actions))),
    send(Popup, append, menu_item(hello)).

%!  make_plain_picture(-Picture, -Box) is det.
%
%   As make_picture/2, but the box has no popup and thus no popup
%   gesture attached to it.

make_plain_picture(P, B) :-
    new(P, picture),
    send(P, open),
    send(P, display, new(B, box(100,100)), point(10,10)).

event(Id, Spec, P, Ev) :-
    buttons(Spec, Mask),
    new(Ev, event(Id, P, 20, 20, Mask)).

		 /*******************************
		 *	      ->is_popup	*
		 *******************************/

:- begin_tests(popup_event).

test(right_down, [setup(make_picture(P,_)), cleanup(free(P))]) :-
    event(ms_right_down, [right], P, Ev),
    assertion(send(Ev, is_popup)).

test(right_down_with_modifier, [setup(make_picture(P,_)), cleanup(free(P))]) :-
    event(ms_right_down, [shift,right], P, Ev),
    assertion(send(Ev, is_popup)).

test(left_down, [setup(make_picture(P,_)), cleanup(free(P))]) :-
    event(ms_left_down, [left], P, Ev),
    assertion(\+ send(Ev, is_popup)).

test(control_left_down, [setup(make_picture(P,_)), cleanup(free(P))]) :-
    event(ms_left_down, [control,left], P, Ev),
    (   emulates_popup
    ->  assertion(send(Ev, is_popup))
    ;   assertion(\+ send(Ev, is_popup))
    ).

test(control_left_up, [setup(make_picture(P,_)), cleanup(free(P))]) :-
    event(ms_left_up, [control,left], P, Ev),
    assertion(\+ send(Ev, is_popup)).

test(control_left_drag, [setup(make_picture(P,_)), cleanup(free(P))]) :-
    event(ms_left_drag, [control,left], P, Ev),
    assertion(\+ send(Ev, is_popup)).

:- end_tests(popup_event).

		 /*******************************
		 *	  POPUP_GESTURE		*
		 *******************************/

%!  activates(+Gesture, +Id, +Spec, +Picture, +Box) is semidet.
%
%   True if posting the described event to Box activates Gesture.  The
%   gesture is left inactive.

activates(G, Id, Spec, P, B) :-
    send(G, slot, status, inactive),
    send(G, slot, current, @nil),
    event(Id, Spec, P, Ev),
    (   send(Ev, post, B, G)
    ->  send(G, cancel, Ev)
    ;   fail
    ).

:- begin_tests(popup_gesture).

test(right_down, [setup(make_picture(P,B)), cleanup(free(P))]) :-
    new(G, popup_gesture),
    assertion(activates(G, ms_right_down, [right], P, B)).

test(left_down, [setup(make_picture(P,B)), cleanup(free(P))]) :-
    new(G, popup_gesture),
    assertion(\+ activates(G, ms_left_down, [left], P, B)).

test(control_left_down, [setup(make_picture(P,B)), cleanup(free(P))]) :-
    new(G, popup_gesture),
    (   emulates_popup
    ->  assertion(activates(G, ms_left_down, [control,left], P, B))
    ;   assertion(\+ activates(G, ms_left_down, [control,left], P, B))
    ).

test(slots_restored, [setup(make_picture(P,B)), cleanup(free(P))]) :-
    new(G, popup_gesture),
    get(G, modifier, Modifier),
    ignore(activates(G, ms_left_down, [control,left], P, B)),
    assertion(get(G, button, right)),
    assertion(get(G, modifier, Modifier)).

test(other_modifier_not_emulated,
     [setup(make_picture(P,B)), cleanup(free(P))]) :-
    new(G, popup_gesture(@default, right, s)),
    assertion(\+ activates(G, ms_left_down, [control,left], P, B)),
    assertion(activates(G, ms_right_down, [shift,right], P, B)).

:- end_tests(popup_gesture).

		 /*******************************
		 *	     HANDLERS		*
		 *******************************/

/* Code that triggers a popup without a gesture registers on the
 * `button' event and guards the action with ->is_popup, as the toc
 * nodes and the inspector do.
 */

:- dynamic fired/0.

mark :-
    assertz(fired).

fired(Id, Spec, P, B, Times) :-
    retractall(fired),
    event(Id, Spec, P, Ev),
    ignore(send(Ev, post, B)),
    aggregate_all(count, fired, Times).

:- begin_tests(popup_handler).

test(guard, [setup(make_plain_picture(P,B)), cleanup(free(P))]) :-
    send(B, recogniser,
         handler(button, and(message(@event, is_popup),
                             message(@prolog, mark),
                             new(or)))),
    assertion(fired(ms_right_down, [right],        P, B, 1)),
    assertion(fired(ms_left_down,  [left],         P, B, 0)),
    assertion(fired(ms_left_up,    [control,left], P, B, 0)),
    (   emulates_popup
    ->  assertion(fired(ms_left_down, [control,left], P, B, 1))
    ;   assertion(fired(ms_left_down, [control,left], P, B, 0))
    ).

:- end_tests(popup_handler).


		 /*******************************
		 *	       TOC		*
		 *******************************/

/* The nodes of a toc_window add to the selection using the modifier
 * that is not the platform's popup click.
 */

:- use_module(library(pce_toc)).

:- begin_tests(popup_toc).

test(add_to_selection_modifier) :-
    get(@toc_node_recogniser, members, Members),
    get(Members, find_all,
        message(@arg1, instance_of, click_gesture), Clicks),
    get(Clicks, nth1, 2, Add),		% plain click, add to selection, ...
    get(Add, modifier, Modifier),
    (   emulates_popup
    ->  assertion(get(Modifier, gui, down)),
        assertion(get(Modifier, control, up))
    ;   assertion(get(Modifier, control, down))
    ).

:- end_tests(popup_toc).

		 /*******************************
		 *	     TERMINAL		*
		 *******************************/

/* A terminal image starts a selection on left-down, but must leave the
 * platform's popup click to the popup gesture of its window.
 */

make_terminal(W, TI) :-
    new(TI, terminal_image(200, 100)),
    new(W, window('test_popup_event')),
    send(W, display, TI),
    send(W, size, size(200, 100)),
    send(W, open),
    send(W, wait).

handles(TI, Id, Spec, Ev) :-
    get(TI, window, W),
    event(Id, Spec, W, Ev),
    send(Ev, post, TI).

:- begin_tests(popup_terminal).

test(left_down, [setup(make_terminal(W,TI)), cleanup(free(W))]) :-
    assertion(handles(TI, ms_left_down, [left], _)).

test(control_left_down, [setup(make_terminal(W,TI)), cleanup(free(W))]) :-
    (   emulates_popup
    ->  assertion(\+ handles(TI, ms_left_down, [control,left], _))
    ;   assertion(handles(TI, ms_left_down, [control,left], _))
    ).

test(popup_reaches_window, [setup(make_terminal(W,_)), cleanup(free(W))]) :-
    send(W, popup, new(P, popup(actions))),
    send(P, append, menu_item(hello)),
    assertion(opens_window_popup(W, ms_right_down, [right])),
    (   emulates_popup
    ->  assertion(opens_window_popup(W, ms_left_down, [control,left]))
    ;   assertion(\+ opens_window_popup(W, ms_left_down, [control,left]))
    ).

%!  opens_window_popup(+Window, +Id, +Spec) is semidet.
%
%   True if the event, delivered as the display does, reaches the popup
%   gesture of Window rather than being consumed by the terminal.

opens_window_popup(W, Id, Spec) :-
    event(Id, Spec, W, Ev),
    send(W, post_event, Ev),
    get(@'_popup_gesture', status, active),
    send(@'_popup_gesture', cancel, Ev).

:- end_tests(popup_terminal).
