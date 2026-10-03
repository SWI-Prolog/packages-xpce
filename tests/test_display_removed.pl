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

:- module(test_display_removed,
          [ test_display_removed/0
          ]).
:- use_module(library(pce)).
:- use_module(library(plunit)).

/** <module> Test removing a hotplug display

When a display is removed, its frames move to another display.  If it
was the last display, it is kept as a parking place for its frames, so
@display never fails.  This test removes the current display, so after
running it this process has no usable display.
*/

test_display_removed :-
    run_tests([ display_removed
              ]).

:- begin_tests(display_removed).

single_display :-
    get(@display_manager?members, size, 1).

test(park_last, [ condition(single_display),
                  Removed == @on, Current == D, FD == D, Members == 1
                ]) :-
    new(F, frame(display_removed)),
    send(F, append, new(_, window)),
    send(F, open),
    get(@display_manager, current, D),
    send(D, removed),
    get(D, slot, removed, Removed),
    get(@display_manager, current, Current),
    get(@display_manager?members, size, Members),
    get(@display, size, _),
    get(F, display, FD),
    send(F, destroy).

test(open_while_parked, [ condition(single_display),
                          FD == Current
                        ]) :-
    get(@display_manager, current, Current),
    new(F, frame(display_removed)),
    send(F, append, new(_, window)),
    send(F, open),
    get(F, display, FD),
    send(F, destroy).

:- end_tests(display_removed).
