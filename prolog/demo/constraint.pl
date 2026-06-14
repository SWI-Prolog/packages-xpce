/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org/projects/xpce/
    Copyright (c)  1995-2026, University of Amsterdam
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

:- module(constraint_demo,
          [ constraint_demo/0
          ]).
:- use_module(library(pce)).
:- use_module(library(pce_util)).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Create a relation expressing ``The center of both constrained objects is
equal''.  The same relation object may   be  used by multiple constraint
objects (e.i.  is *reusable*).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_global(@demo_center, new(identity(center))).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Create a graphical window with two boxes   that may be moved and resized
using the middle button.  Two sliders are   defined to modify the center
of the two boxes as too.  All relations are expressed using constraints.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

constraint_demo :-
    new(P, picture('Constraint Demo', size(100, 300))),
    send(new(D, dialog), below, P),

    send(P, display,
         new(T, text("Please move/resize either box\n\c
                      using the left mouse button",
                     left, italic)),
         point(10,0)),
    send(T, colour, grey50),

    send(P, display, new(B1, box(100,100))),
    send(P, display, new(B2, box(50, 50))),
    send(B1, fill, blue),
    send(B2, fill, yellow),
    send_list([B1, B2], recogniser,
              handler_group(resize_gesture(left),
                            move_gesture(left))),
    new(_, constraint(B1, B2, @demo_center)),

    send(D, append, new(S1, slider(center_x, 0, 500, 200))),
    send(D, append, new(S2, slider(center_y, 0, 500, 150))),
    send_list([S1, S2], drag, @on),
    send_list([S1, S2], message, message(@receiver, update_constraints)),
    send(D, append, button(quit, message(D, destroy))),

    new(_, constraint(S1, B1, identity(selection, center_x))),
    new(_, constraint(S2, B2, identity(selection, center_y))),

    send(D, open).

