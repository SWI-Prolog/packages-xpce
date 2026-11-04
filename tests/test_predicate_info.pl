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

:- module(test_predicate_info, [test_predicate_info/0]).

/** <module> The information sheet of a predicate

`prolog_predicate ->info` opens a sheet on a predicate, which the Info
items of PceEmacs and the profiler use.  Clicking its source location
shows it in PceEmacs, which need not be running.

Run with:

    swipl -g test_predicate_info -t halt \
          packages/xpce/tests/test_predicate_info.pl
*/

%   Starting PceEmacs may save its bookmarks: keep them out of the
%   configuration of the user running the tests.

:- use_module(xpce_test_sandbox).
:- initialization(sandbox_app_config(_), now).

:- use_module(library(pce)).
:- initialization(remove_sandbox_at_exit, now).
:- use_module(library(plunit)).
:- use_module(library(prolog_predicate)).

test_predicate_info :-
    run_tests([ predicate_info ]).

%!  info_location_text(+PI, -Frame, -Text) is det.
%
%   Open the information sheet on PI and find the text that shows its
%   source location.

info_location_text(PI, Frame, Text) :-
    new(PP, prolog_predicate(PI)),
    send(PP, info),
    get(@display, frames, Frames),
    get(Frames, find,
        message(@arg1?label, prefix, 'Info for'), Frame),
    once(sub_graphical(Frame, source_location_text, Text)).

sub_graphical(Obj, Class, Gr) :-
    send(Obj, instance_of, Class),
    !,
    Gr = Obj.
sub_graphical(Obj, Class, Gr) :-
    (   send(Obj, instance_of, frame)
    ->  get(Obj, members, Parts)
    ;   send(Obj, instance_of, device)
    ->  get(Obj, graphicals, Parts)
    ),
    chain_list(Parts, List),
    member(Part, List),
    sub_graphical(Part, Class, Gr).

edited_buffer(Base) :-
    get(@emacs_buffers, members, Items),
    chain_list(Items, List),
    member(Item, List),
    get(Item, key, Base).

:- begin_tests(predicate_info, [cleanup(free_info_frames)]).

free_info_frames :-
    forall(( get(@display, frames, Frames),
             chain_list(Frames, List),
             member(F, List),
             get(F, label, Label),
             sub_atom(Label, 0, _, _, 'Info for')
           ),
           free(F)).

test(edit_source_location, true(edited_buffer('lists.pl'))) :-
    %  Used to raise an existence error on @emacs if PceEmacs was
    %  not running.
    in_pce_thread_sync(( info_location_text(lists:append/3, _, Text),
                         send(Text, edit)
                       )).

:- end_tests(predicate_info).
