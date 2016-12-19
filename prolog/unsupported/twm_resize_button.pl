/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  1999-2011, University of Amsterdam
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

:- module(twm_resize_button, []).
:- use_module(library(pce)).
:- require([ default/3
           ]).

:- pce_autoload(twm_geometry_box, library(twm_geometry_box)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module defines a resize button as   used by the TWM window manager.
The button resizes the <-device it  is   displayed  on.  This may be any
XPCE device object.  The button is operated using the left-mouse-button.

This class is  used  in  combination   with  class  subframe  to  define
frames-in-windows.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(twm_resize_button, bitmap).

initialise(BM, Image:[image]) :->
    "Create from image"::
    default(Image, 'resize.bm', Img),
    send(BM, send_super, initialise, Img).


:- free(@resize_button_gesture).        % for reconsult
:- pce_global(@resize_button_gesture, make_resize_button_gesture).

make_resize_button_gesture(G) :-
    new(G, gesture),
    send(G, attribute, attribute(outline, new(OutLine, twm_geometry_box))),
    send(G, attribute, hmode),
    send(G, attribute, vmode),
    send(G, cursor, fleur),

    new(Gr, @event?receiver?device),
    new(Dev, Gr?device),
    new(Display, Dev?display),

    send(OutLine, send_method,
         send_method(right_side, vector(int),
                     message(@receiver, set,
                             @default, @default,
                             @arg1 - @receiver?x, @default))),
    send(OutLine, send_method,
         send_method(left_side, vector(int),
                     message(@receiver, set,
                             @arg1, @default,
                             @receiver?right_side - @arg1, @default))),
    send(OutLine, send_method,
         send_method(bottom_side, vector(int),
                     message(@receiver, set,
                             @default, @default,
                             @default, @arg1 - @receiver?y))),
    send(OutLine, send_method,
         send_method(top_side, vector(int),
                     message(@receiver, set,
                             @default, @arg1,
                             @default, @receiver?bottom_side - @arg1))),

    send(G, send_method,
         send_method(initiate, vector(event),
                     and(message(OutLine, attach, Gr),
                         message(G, hmode, @nil),
                         message(G, vmode, @nil)))),

    send(G, send_method,
         send_method(drag, vector(event),
                     and(assign(new(P, var), ?(@event, position, Dev)),
                         assign(new(PX, var), P?x),
                         assign(new(PY, var), P?y),
                         assign(new(DP, var), ?(@event, position, Display)),
                         assign(new(DX, var), DP?x),
                         assign(new(DY, var), DP?y),
                         if(G?hmode \== @nil,
                            message(OutLine, G?hmode, DX),
                            if(PX >= Gr?right_side,
                               and(message(G, hmode, right_side),
                                   message(OutLine, right_side, DX)),
                               if(PX =< Gr?left_side,
                                  and(message(G, hmode, left_side),
                                      message(OutLine, left_side, DX))))),
                         if(G?vmode \== @nil,
                            message(OutLine, G?vmode, DY),
                            if(PY >= Gr?bottom_side,
                               and(message(G, vmode, bottom_side),
                                   message(OutLine, bottom_side, DY)),
                               if(PY =< Gr?top_side,
                                  and(message(G, vmode, top_side),
                                      message(OutLine, top_side, DY)))))))),

    send(G, send_method,
         send_method(terminate, vector(event),
                     and(message(OutLine, detach, Gr)))).


event(_RB, Ev:event) :->
    "Invoke @resize_button_gesture"::
    send(@resize_button_gesture, event, Ev).

:- pce_end_class.



