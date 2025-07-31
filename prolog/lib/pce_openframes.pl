/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2025, SWI-Prolog Solutions b.v.
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

:- module(pce_openframes,
          [ confirm_open_frames/1
          ]).
:- use_module(library(pce)).
:- autoload(library(apply), [maplist/3, include/3]).
:- autoload(library(lists), [nth1/3, append/2]).
:- autoload(library(option), [option/2, option/3]).
:- autoload(library(pce_util), [get_chain/3]).

%!  confirm_open_frames(+Options)
%
%   If there are open frames, show a dialog that offers the choice to
%   close them, continue or quit Prolog.  Options:
%
%       - transient_for(Frame)
%         Open the frame as a transient for Frame and ignore the fact
%         that Frame is open.
%       - message(Message)
%         Additional message to show

confirm_open_frames(Options) :-
    option(transient_for(Frame), Options),
    !,
    open_frames(Frames),
    (   Frames == [Frame]
    ->  true
    ;   option(message(Msg), Options, @default),
        new(D, confirm_open_frames(Msg, Frames)),
        send(D, transient_for, Frame),
        send(D, open_centered, Frame)
    ).
confirm_open_frames(Options) :-
    open_frames(Frames),
    (   Frames == []
    ->  true
    ;   option(message(Msg), Options, @default),
        send(new(confirm_open_frames(Msg, Frames)), open_centered)
    ).

:- pce_begin_class(confirm_open_frames, dialog,
                   "Act on open frames").

initialise(D, Message:[string], Frames:prolog) :->
    "Create from frames"::
    send_super(D, initialise, "Open frames"),
    (   Message == @default
    ->  true
    ;   send(D, append, label(message, Message))
    ),
    forall(nth1(I, Frames, F),
           send(D, append_frame, F, I)),
    send(D, append, button(quit_prolog, message(@prolog, halt)), next_row),
    send(D, append, button(continue, message(D, destroy)), right).

append_frame(D, Frame:frame, I:int) :->
    "Append a frame"::
    get(Frame, label, Label),
    atom_concat(label, I, LabelName),
    send(D, append, new(Lbl, label(LabelName, Label))),
    send(D, append,
         button(close, and(message(Lbl, active, @off),
                           message(@receiver, active, @off),
                           message(Frame, destroy))), right).

:- pce_end_class.

open_frames(Frames) :-
    get_chain(@display_manager, members, Displays),
    maplist(display_open_frames, Displays, FramesPerDsplay),
    append(FramesPerDsplay, Frames).

display_open_frames(Display, Frames) :-
    get_chain(Display, frames, AllFrames),
    include(is_open_frame, AllFrames, Frames).

is_open_frame(Frame) :-
    get(Frame, status, Status),
    Status \== unmapped,
    Status \== hidden.
