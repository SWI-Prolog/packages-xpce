/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org/packages/xpce/
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

:- module(pce_dragdrop_file,
          [ dragdrop_file_demo/0
          ]).
:- use_module(library(pce)).
:- use_module(library(apply)).

/** <module> Demo: receive files dropped from the desktop

The SDL backend forwards OS-level drag-and-drop  as the new `drop_begin'
/ `drop_position' / `drop_file' /  `drop_text' / `drop_complete' events.
This demo opens a small frame with a drop   target on the left and a log
on the right; drag a file from the file manager into the target area and
watch the events arrive.

    ?- dragdrop_file_demo.
*/

dragdrop_file_demo :-
    new(F, frame('Drag-and-drop File Demo')),
    send(F, append, new(Target, picture(target, size(280, 220)))),
    send(new(Log, view(log, size(48, 14))), right, Target),
    send(Target, background, colour(grey90)),
    %   Show "Drop files here" centred in the target.
    send(Target, display,
         new(Hint, text('Drop files or\ntext here', center, bold))),
    send(Hint, center, Target?visible?center),
    %   Install the drop handlers on the target picture. Each handler
    %   appends a line to the log pane.
    maplist(add_log_handler(Target, Log),
            [ drop_begin,
              drop_position,
              drop_file,
              drop_text,
              drop_complete
            ]),
    send(F, open).

add_log_handler(Target, Log, Event) :-
    send(Target, recogniser, handler(Event,
                                     message(@prolog, log_event,
                                             Log, @receiver, @event,
                                             Event))).


%!  log_event(+LogView, +Target, +Event, +Tag) is det.
%
%   Append one line to LogView describing Event. For drop_file the
%   line shows the path stored on the event's `path' attribute; for
%   drop_text the `text' attribute. Position-bearing events also
%   show the (x,y) coordinates relative to Target.

log_event(Log, _Target, Event, Tag) :-
    get(Event, x, X),
    get(Event, y, Y),
    (   get(Event, attribute, path, Path)
    ->  Payload = Path
    ;   get(Event, attribute, text, Text)
    ->  Payload = Text
    ;   Payload = ''
    ),
    new(S, string('%s\t(%d, %d)\t%s\n', Tag, X, Y, Payload)),
    send(Log, append, S).
