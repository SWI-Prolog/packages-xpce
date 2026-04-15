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

:- module(pce_backcomp,
          []).
:- use_module(library(pce)).

/** <module> XPCE backward compatibility support

This library provides some Prolog   work-arounds  for functionality that
has been modified or removed with the migration to SDL3.

@tbd Complete
*/

:- pce_extend_class(graphical).

fill_pattern(G, Fill:'colour|image*') :->
    "Deprecated.  Use ->fill"::
    (   send(Fill, instance_of, image)
    ->  pce_backcomp(image_fill(Fill))
    ;   pce_backcomp(deprecated(graphical->fill_pattern,
                                graphical->fill)),
        send(G, fill, Fill)
    ).

:- pce_end_class.

:- pce_extend_class(frame).

icon(_, _) :->
    "Not implemented: frame->icon"::
    pce_backcomp(removed(frame->icon)).

:- pce_end_class.


                /*******************************
                *           MESSAGES           *
                *******************************/

:- multifile
    prolog:message//1.

:- dynamic warned/1 as volatile.

pce_backcomp(Msg) :-
    warned(Msg),
    !.
pce_backcomp(Msg) :-
    asserta(warned(Msg)),
    print_message(warning, pce_backcomp(Msg)).

prolog:message(pce_backcomp(Msg)) -->
    [ 'XPCE compat: '],
    message(Msg).

message(image_fill(Fill)) -->
    [ '->fill_pattern: ~p: filling with images is not supported'-[Fill] ].
message(deprecated(Old, New)) -->
    [ 'Deprecated: ~p (use ~p)'-[Old, New] ].
message(removed(Msg)) -->
    [ 'Removed: ~p'-[Msg] ].
