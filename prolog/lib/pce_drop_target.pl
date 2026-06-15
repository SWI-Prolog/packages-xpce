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

:- module(pce_drop_target,
          [ drop_target_event/4,           % +Target, +Event, +Hint, :OnComplete
            drop_target_show_hint/3,       % +Target, +Text, +Colour
            drop_target_clear_hint/1,      % +Target
            drop_target_show_rejected/3    % +Target, +Text, +Delay
          ]).
:- use_module(library(pce)).
:- use_module(library(pce_util), [chain_list/2]).

:- pce_autoload(partof_hyper, library(hyper)).

:- meta_predicate
    drop_target_event(+, +, +, 2).

/** <module> Drag-and-drop file/text overlay and multi-file collector

Turn the SDL3 drag-and-drop event lifecycle (drop_position, drop_file,
drop_complete) delivered to an xpce graphical into a single callback
that receives the list of OS file paths dropped during the sequence,
together with a centred hint overlay shown on the target's window
during the drag.

Use from an xpce class's :->event method:

==
event(T, Ev:event) :->
    (   ...class-specific events...
    ;   drop_target_event(T, Ev,
                          'Drop file(s) here',
                          my_open_files)
    ).

my_open_files(Target, Files) :-
    forall(member(F, Files), open_file(Target, F)).
==

The overlay disappears on drop_complete, which SDL3 also fires when
the user drags out of the window without releasing, so no explicit
drag-leave handling is required.

State is kept in xpce attributes on the target, so no class variables
are needed.
*/

%!  drop_target_event(+Target, +Event, +Hint, :OnComplete) is semidet.
%
%   Handle a drop event delivered to Target. Succeeds when Event is one
%   of drop_position, drop_file or drop_complete; fails otherwise so it
%   can sit as a branch in an if-then-else event chain.
%
%   On the first drop_position of a drag, a centred hint with Text
%   appears on the target's window. Each drop_file appends its path to
%   a per-target chain. On drop_complete the hint is removed and, when
%   one or more files were collected, OnComplete is called as
%   `call(OnComplete, Target, Files)` with Files a non-empty list of OS
%   paths.

drop_target_event(Target, Ev, Hint, _) :-
    send(Ev, is_a, drop_position),
    !,
    drop_target_start(Target, Hint).
drop_target_event(Target, Ev, _, _) :-
    send(Ev, is_a, drop_file),
    !,
    get(Ev, attribute, path, Path),
    drop_target_collect(Target, Path).
drop_target_event(Target, Ev, _, OnComplete) :-
    send(Ev, is_a, drop_complete),
    !,
    drop_target_finish(Target, OnComplete).

drop_target_start(Target, Hint) :-
    (   get(Target, attribute, drop_hint, _)
    ->  true
    ;   send(Target, attribute, drop_files, new(chain)),
        drop_target_show_hint(Target, Hint, grey50)
    ).

drop_target_collect(Target, Path) :-
    (   get(Target, attribute, drop_files, Chain)
    ->  true
    ;   new(Chain, chain),
        send(Target, attribute, drop_files, Chain)
    ),
    send(Chain, append, Path).

drop_target_finish(Target, OnComplete) :-
    drop_target_clear_hint(Target),
    (   get(Target, attribute, drop_files, Chain)
    ->  send(Target, delete_attribute, drop_files),
        chain_list(Chain, Files)
    ;   Files = []
    ),
    (   Files \== []
    ->  call(OnComplete, Target, Files)
    ;   true
    ).

%!  drop_target_show_hint(+Target, +Text, +Colour) is det.
%
%   Display a centred bold Text in Colour on the window containing
%   Target, replacing any previous overlay. Colour is a colour name
%   accepted by colour/1 (e.g. grey50, red).

drop_target_show_hint(Target, Text, Colour) :-
    drop_target_clear_hint(Target),
    (   get(Target, window, W), W \== @nil
    ->  new(Hint, text(Text, center, bold)),
        send(Hint, colour, colour(Colour)),
        send(Hint, background, colour(grey90)),
        send(Hint, border, 8),
        send(Hint, pen, 1),
        send(W, display, Hint),
        send(Hint, center, W?visible?center),
        send(Target, attribute, drop_hint, Hint)
    ;   true
    ).

%!  drop_target_clear_hint(+Target) is det.
%
%   Remove the hint overlay (and any scheduled-clear timer) from Target.

drop_target_clear_hint(Target) :-
    ignore(send(Target, send_hyper, drop_hint_timer, free)),
    (   get(Target, attribute, drop_hint, Hint)
    ->  send(Target, delete_attribute, drop_hint),
        send(Hint, free)
    ;   true
    ).

%!  drop_target_show_rejected(+Target, +Text, +Delay) is det.
%
%   Show Text in red on Target's window, then auto-clear after Delay
%   seconds. Use from the OnComplete callback when the dropped files
%   could not be processed.

drop_target_show_rejected(Target, Text, Delay) :-
    drop_target_show_hint(Target, Text, red),
    new(Timer, timer(Delay,
                     message(@prolog, drop_target_clear_hint, Target))),
    new(_, partof_hyper(Target, Timer, drop_hint_timer)),
    send(Timer, start, once).
