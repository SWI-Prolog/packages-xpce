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

:- module(pce_pin_button, []).
:- use_module(library(pce)).
:- use_module(library(help_message), []).

/** <module> Pin that keeps a tool from being reused

Tools that show the result of the last request -- a hit list, a term,
the key bindings of a window -- reuse the one already open for the next
request.  A pin in the corner keeps the one open: the next request opens
another.  The tool asks <-pinned before reusing itself.

A pin_button is named `pin`, so a tool finds it as `pin` member of the
dialog it is in.
*/

resource(pin,    image, image('pin.svg')).
resource(pinned, image, image('pinned.svg')).

:- pce_begin_class(pin_button, bitmap,
                   "Toggle whether a tool may be reused").

variable(pinned, bool := @off, get, "Pinned: do not reuse").

initialise(P, Help:help=[char_array]) :->
    "Create, unpinned, explaining what pinning does with Help"::
    send_super(P, initialise, image(resource(pin))),
    send(P, name, pin),
    send(P, recogniser,
         click_gesture(left, '', single, message(P, toggle))),
    (   Help == @default
    ->  Tip = 'Pin: keep this view and show the next one in a new pane'
    ;   Tip = Help
    ),
    send(P, help_message, tag, Tip).

toggle(P) :->
    "Toggle the pinned state"::
    get(P, pinned, P0),
    get(P0, negate, P1),
    send(P, pinned, P1).

pinned(P, Pinned:bool) :->
    "Set the pinned state and show it"::
    send(P, slot, pinned, Pinned),
    (   Pinned == @off
    ->  send(P, image, image(resource(pin)))
    ;   send(P, image, image(resource(pinned)))
    ).

:- pce_end_class(pin_button).
