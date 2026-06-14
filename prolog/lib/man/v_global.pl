/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  1985-2002, University of Amsterdam
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

:- module(man_object_browser, []).

:- use_module(library(pce)).
:- use_module(library(pldoc/man_index), [manual_object/5]).
:- use_module(util).
:- require([ default/3
           , send_list/3
           ]).

:- pce_begin_class(man_object_browser, man_frame,
                   "Gobal object browser").

initialise(OB, Manual:man_manual) :->
    "Create from Manual"::
    send(OB, send_super, initialise, Manual, 'Object Browser'),

    new(B, man_summary_browser(man_summary, size(70, 15))),
    send(B?image, tab_stops, vector(20, 200)),
    send(B, name, browser),
    dialog(Dialog),

    send(OB, append, B),
    send(Dialog, below, B),
    send(OB, fill, ''),

    send(OB, open).


                /********************************
                *            DIALOG             *
                ********************************/

dialog(D) :-
    new(D, dialog),
    new(OB, D?frame),
    send(D, append, new(A, menu(show, marked, @nil))),
    send(A, layout, horizontal),
    send_list(A, append, [documented, all]),
    send(D, append, new(SS, text_item(search, regex(''),
                                      message(D?apply_member, execute))),
         right),
    send(SS, length, 15),
    send(D, append, button(apply, message(OB, fill,
                                          SS?selection, A?selection))),

    send(D, append, button(help,  message(OB, help))),
    send(D, append, button(quit,  message(OB, quit))).


                /********************************
                *             FILL              *
                ********************************/

fill(OB, Pattern:regex, What:[name]) :->
    "Fill with all global objects matching pattern"::
    default(What, documented, Show),
    get(OB, member, browser, B),
    send(B, clear),
    new(Chain, chain),
    (   Show == documented
    ->  forall(documented_global(Ref),
               append_object(Chain, Pattern, Ref))
    ;   send(@pce, for_name_reference,
             message(@prolog, append_object, Chain, Pattern, @arg1))
    ),
    get(Chain, size, S),
    send(OB, report, progress, 'Sorting %d objects ...', S),
    send(Chain, sort, ?(@arg1?reference, compare, @arg2?reference)),
    send(OB, report, done),
    send(B, members, Chain).


%   The HTML manual's global-object chapter lives under section
%   anchors =|sec:object-<slug>|= (PlDoc strips underscores via
%   =|delete_unsafe_label_chars/2|=, so the slug isn't a clean key).
%   The per-section Summary stores the original =|@ref|= name -- use
%   that as the authoritative reference.
%
%   Only yield refs that actually resolve to a live xpce global; the
%   .md file still lists obsolete sentinels (=|@_not_returned|=) and
%   =|new(man_global(Ref))|= would hang on those.

documented_global(Ref) :-
    manual_object(section(_Level, _Nr, Anchor, _Title),
                  Summary, File, packages, _Off),
    sub_atom(File, _, _, _, '/xpce/man/refmanual/'),
    atom_concat('sec:object-', _, Anchor),
    summary_to_global_ref(Summary, Ref),
    object(@Ref).

summary_to_global_ref(Summary, Ref) :-
    text_atom(Summary, SumAtom),
    atom_concat('@', Ref, SumAtom).

text_atom(S, A) :- string(S), !, atom_string(A, S).
text_atom(A, A).


append_object(Chain, Pattern, Ref) :-
    new(G, man_global(Ref)),
    (   get(G, man_summary, Summary),
        send(Pattern, search, Summary)
    ->  send(Chain, append, G)
    ;   true
    ).

                /********************************
                *          COMMUNICATION        *
                ********************************/

selected(OB, Obj:object*) :->
    "Set the selection"::
    get(OB, member, browser, B),
    send(B, selected, Obj).


release_selection(OB) :->
    "Clear the selection"::
    get(OB, member, browser, B),
    send(B, release_selection).

:- pce_end_class.

