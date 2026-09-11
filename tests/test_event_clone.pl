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


:- module(test_event_clone, [test_event_clone/0]).
:- encoding(utf8).

/** <module> Cloning an event may not clone the application

`Event <-clone' is used to keep a copy of a transient event around, for
example by the event viewer (library(man/showevent)).  The `frame',
`window' and `receiver' slots must be cloned by reference: cloning them
recursively copies the entire user interface reachable from the event,
which quickly explodes if the copies are stored in a window that is
itself reachable from the event.

Run with:

    swipl -g test_event_clone -t halt \
          packages/xpce/tests/test_event_clone.pl
*/

:- use_module(library(pce)).
:- use_module(library(plunit)).
:- use_module(library(apply)).

setup_headless :-
    set_prolog_flag('SDL_VIDEODRIVER', dummy).
:- initialization(setup_headless, now).

test_event_clone :-
    run_tests([ event_clone
              ]).

%!  created(+Classes, -Counts) is det.
%
%   Number of instances created for each of Classes.

created(Classes, Counts) :-
    maplist(no_created, Classes, Counts).

no_created(Class, Count) :-
    get(@pce, convert, Class, class, ClassObj),
    get(ClassObj, no_created, Count).

%!  make_frame(-Frame, -Picture, -Box) is det.

make_frame(F, P, B) :-
    new(F, frame(test_event_clone)),
    send(F, append, new(P, picture)),
    send(P, display, new(B, box(50,50))),
    send(F, open).

:- begin_tests(event_clone).

test(context_is_shared, [setup(make_frame(F, P, B)), cleanup(free(F))]) :-
    new(Ev, event(ms_left_down, P, 10, 10)),
    send(Ev, slot, receiver, B),
    send(Ev, slot, frame, F),
    get(Ev, clone, Clone),
    assertion(get(Clone, frame, F)),
    assertion(get(Clone, window, P)),
    assertion(get(Clone, receiver, B)).

test(nothing_is_copied, [setup(make_frame(F, P, B)), cleanup(free(F))]) :-
    new(Ev, event(ms_left_down, P, 10, 10)),
    send(Ev, slot, receiver, B),
    send(Ev, slot, frame, F),
    created([frame, window, picture, box], Created0),
    get(Ev, clone, _Clone),
    created([frame, window, picture, box], Created),
    assertion(Created == Created0).

:- end_tests(event_clone).
