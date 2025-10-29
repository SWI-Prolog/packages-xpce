/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org/packages/xpce/
    Copyright (c)  1985-2025, University of Amsterdam
                              VU University Amsterdam
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

:- module(swi_prolog_emacs_binding,
          [ message_ide/1                 % +Bool
          ]).
:- use_module(library(pce)).
:- require([ start_emacs/0
           ]).

/** <module> Add messages related to a source-location to the GUI

This module implements user:message_hook/3 to add messages printed using
print_message/2 that can be related to   a source-location to the window
@prolog_warnings.

This library is always loaded when XPCE is loaded.  Its functionality is
controlled by the Prolog flag `message_ide`.
*/

:- create_prolog_flag(message_ide, true, [keep(true)]).

:- multifile message_ide_disabled/0.
:- dynamic message_ide_disabled/0.

%!  message_ide(+Bool)
%
%   Enable/disable the message IDE.  The message IDE can be disabled
%   permanently by setting the `message_ide` flag to `false` in your
%   `init.pl` file.

message_ide(false) =>
    (   message_ide_disabled
    ->  true
    ;   asserta(message_ide_disabled)
    ).
message_ide(true) =>
    retractall(message_ide_disabled).


                 /*******************************
                 *          WARNINGS            *
                 *******************************/

:- pce_global(@prolog_warnings, make_prolog_warning_list).

make_prolog_warning_list(L) :-
    new(L, emacs_hit_list('SWI-Prolog warnings')),
    send(L, clear_on_append, @on),
    send(L, expose_on_append, @on),
    send(L, message, error_at_location),
    send(L, open).

clear_message_list :-
    (   object(@prolog_warnings)
    ->  send(@prolog_warnings, clear)
    ;   true
    ).

%!  ide_message(+Location, +String)
%
%   Display system messages in a graphical  window. Note that String
%   is locked to avoid XPCE's GC.

ide_message(Path:Line, String) :-
    start_emacs,
    new(Buffer, emacs_buffer(Path)),
    get(Buffer, scan, 0, line, Line-1, start, SOL),
    send(@prolog_warnings, append_hit, Buffer, SOL, @default, String),
    send(String, lock_object, @off).

message_to_pce(Term, Path:Line, String) :-
    (   Term = error(syntax_error(Error),
                     file(Path, Line, _LinePos, _CharPos))
    ->  atom(Path), integer(Line),
        (   atom(Error)
        ->  Msg = Error
        ;   format(atom(Msg), '~p', [Error])
        ),
        new(String, string('Syntax error: %s', Msg))
    ;   Term = error(_, Location),
        nonvar(Location),
        Location = file(Path, Line)
    ->  message_to_string(Term, String)
    ;   source_location(Path, Line),
        message_to_string(Term, String)
    ),
    atom(Path),
    send(String, lock_object, @on).

%   user:message_hook(+Term, +Level, +Lines)
%
%   Hook clauses that direct error messages to   the  (xpce) IDE. We use
%   user:message_hook/3 rather than prolog:message_action/2   because we
%   want to allow e.g. xref_source/1 to silence messages.

:- multifile user:message_hook/3.
:- dynamic   user:message_hook/3.

user:message_hook(Term, Level, _Lines) :-
    current_prolog_flag(message_ide, true),
    \+ message_ide_disabled,
    ide_message_action(Term, Level),
    fail.

ide_message_action(Term, Level) :-
    accept_level(Level),
    !,
    in_pce_thread(pce_message(Term)).
ide_message_action(make(reload(_Files)), _) :-
    in_pce_thread(clear_message_list).
ide_message_action(emacs(consult(_File)), _) :-
    in_pce_thread(clear_message_list).

pce_message(Term) :-
    \+ object(@loading_emacs),
    message_to_pce(Term, Location, String),
    ide_message(Location, String).

accept_level(warning).
accept_level(error).
