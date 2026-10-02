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

:- module(test_display_function,
          [ test_display_function/0
          ]).
:- use_module(library(pce)).
:- use_module(library(plunit)).

/** <module> Test @display as a function of @display_manager

@display is the function ?(@display_manager, current): it evaluates to
the display of the last event or, if there is none, the primary display.
It is evaluated on each use, so it remains valid if displays are added or
removed.  State that belongs to the application rather than to a display
(inspect handlers, the busy cursor, all frames) is kept or reached
through @display_manager.
*/

test_display_function :-
    run_tests([ display_function
              ]).

:- begin_tests(display_function).

test(is_function) :-
    get(@display, '_class_name', ClassName),
    assertion(ClassName == ?).

test(evaluates_to_display) :-
    get(@display, class_name, ClassName),
    assertion(ClassName == display).

test(current_display) :-
    get(@display_manager, current, Current),
    get(@display, self, Display),
    assertion(Display == Current).

test(argument) :-
    get(@display_manager, current, Current),
    new(F, frame(display_function)),
    send(F, display, @display),
    get(F, display, Display),
    free(F),
    assertion(Display == Current).

test(shared_inspect_handlers) :-
    new(H, handler(ms_left_up, message(@prolog, true))),
    send(@display, inspect_handler, H),
    get(@display_manager, inspect_handlers, DMHandlers),
    get(@display_manager, members, DisplayChain),
    chain_list(DisplayChain, Displays),
    forall(member(Display, Displays),
           ( get(Display, inspect_handlers, Handlers),
             assertion(Handlers == DMHandlers))),
    assertion(send(DMHandlers, member, H)),
    send(DMHandlers, delete, H).

test(all_frames) :-
    new(F, frame(display_function)),
    send(F, append, new(_, window)),
    send(F, open),
    get(@display_manager, frames, Frames),
    assertion(send(Frames, member, F)),
    send(F, destroy).

test(busy_cursor) :-
    send(@display_manager, busy_cursor),
    send(@display_manager, busy_cursor, @nil).

:- end_tests(display_function).
