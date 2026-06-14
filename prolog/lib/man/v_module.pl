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

:- module(man_module_browser, []).

:- use_module(library(pce)).
:- use_module(library(pldoc/man_index), [manual_object/5]).
:- use_module(example_summaries, [example_summary/3]).
:- use_module(util).
:- require([ forall/2
           , member/2
           , send_list/3
           ]).

:- pce_begin_class(man_module_browser(module_name), man_frame).

variable(module,        man_module*,    get,    "Currently viewed module").
variable(create_class,  name*,          both,   "Classname to be created").


initialise(MB, Manual:man_manual, ModuleName:name, CreateClass:[name]*,
               Label:[name]) :->
    "Create from Manual"::
    send(MB, send_super, initialise, Manual, Label),

    new(Browser, man_summary_browser(man_summary, size(90, 15))),
    dialog(Dialog),

    send(MB, append, Browser),
    send(Dialog, below, Browser),
    send(MB, view, ModuleName, CreateClass).


module_name(MB, Name:name) :<-
    get(MB, module, Module),
    (   Module \== @nil
    ->  get(Module, name, Name)
    ;   Name = '(nil)'
    ).


browser(MB, Browser) :<-
    "Get the browser"::
    get(MB, member, man_summary_browser, Browser).



                /********************************
                *            DIALOG             *
                ********************************/

dialog(D) :-
    new(D, dialog),
    new(MB, D?frame),

    send(D, append, button(help,   message(MB, help))),
    send(D, append, button(quit,   message(MB, quit))).



                /********************************
                *          COMMUNICATION        *
                ********************************/

selected(MB, Obj:object*) :->
    "Set the selection"::
    send(MB?browser, selected, Obj).


release_selection(MB) :->
    send(MB?browser, selected, @nil).


                /********************************
                *            FILLING            *
                ********************************/

view(MB, ModuleName:name, ClassName:name*) :->
    "Connect to a specified module"::
    get(MB?manual, module, ModuleName, @on, Module),
    send(MB, slot, module, Module),
    (   ClassName == @default
    ->  send(MB, slot, create_class, @nil)
    ;   send(MB, slot, create_class, ClassName)
    ),
    ensure_module_loaded(ModuleName, ClassName, Module),
    get(MB, browser, Browser),
    send(Module?id_table, for_some,
         message(Browser, append_card, @arg2)),
    send(Browser, sort).


%   The .doc tree retired in Phase 8 used to ship serialised modules
%   under =|$PCEHOME/man/reference|=. The HTML manual keeps the same
%   content as =|section(_,_,sec:<kind>-<slug>,_)|= rows in
%   =|manindex.db|=; populate the module from those rows the first
%   time anyone opens this browser.

ensure_module_loaded(ModuleName, ClassName, Module) :-
    get(Module?id_table, size, 0),
    populate_module(ModuleName, ClassName, Module),
    !.
ensure_module_loaded(_, _, _).

populate_module(examples, ClassName, Module) :-
    nonvar(ClassName),
    !,
    forall(manual_example(Title),
           ensure_card(Module, ClassName, examples, Title)).
populate_module(changes, ClassName, Module) :-
    nonvar(ClassName),
    !,
    forall(manual_change(Title),
           ensure_card(Module, ClassName, changes, Title)).
populate_module(_, _, _).

manual_example(Title) :-
    manual_object(section(_Level, _Nr, Anchor, _Tle),
                  Summary, File, packages, _Off),
    sub_atom(File, _, _, _, '/xpce/man/refmanual/'),
    atom_concat('sec:example-', _, Anchor),
    summary_to_title(Summary, Title).

manual_change(Title) :-
    %   No manindex slot for changes yet; rely on summary text in
    %   any =|section(_,_,sec:sec-changes-...,_)|= row to surface
    %   the changelog entries the legacy .doc tree exposed.
    manual_object(section(_Level, _Nr, Anchor, _Tle),
                  Summary, File, packages, _Off),
    sub_atom(File, _, _, _, '/xpce/man/refmanual/'),
    atom_concat('sec:sec-changes-', _, Anchor),
    summary_to_title(Summary, Title).

summary_to_title(S, T) :-
    (   string(S) -> T = S
    ;   atom(S)   -> atom_string(S, T)
    ;   fail
    ).

ensure_card(Module, ClassName, Kind, Title) :-
    Term =.. [ClassName, Module, Title],
    new(Card, Term),
    %   The first paragraph of each example / change has been
    %   extracted by =|tmp/extract_example_summaries.pl|= into
    %   =|man/example_summaries.pl|=. Stash it on the card so the
    %   browser shows real prose instead of =|nil|= next to the
    %   title.
    (   example_summary(Kind, Title, Summary)
    ->  send(Card, store, summary, string(Summary))
    ;   true
    ).


:- pce_end_class.
