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

:- module(man_card, []).

:- use_module(library(pce)).
:- use_module(util).
:- require([ forall/2
           , member/2
           ]).


                /********************************
                *            SPACES             *
                ********************************/

:- pce_begin_class(man_space(name), object,
                   "Collection of man_modules").

:- pce_global(@man_space_table, new(hash_table)).


variable(name,          name,           both,
         "Logical name of the space").
variable(modules,       hash_table,     get,
         "Map module name onto module").
variable(modified,      bool,           both,
         "Indicate some module has modified").

%   Phase 8 retired the on-disk =|$PCEHOME/man/reference/*.doc|=
%   serialised modules. man_space is now just an in-memory namespace
%   the v_global / v_module / v_group browsers populate from
%   manual_object/5; load/save and the directory slot are gone.

initialise(S, Name:name) :->
    "Initialise from name"::
    (   get(@man_space_table, member, Name, _)
    ->  send(@display, inform, @nil, "XPCE Manual",
             'Space %s already exists', Name)
    ;   send(S, slot, name,     Name),
        send(S, slot, modified, @off),
        send(S, slot, modules,  new(hash_table)),
        send(@man_space_table, append, Name, S)
    ).


lookup(_, Name:name, S:man_space) :<-
    "Lookup existing manual space"::
    get(@man_space_table, member, Name, S).


module(S, ModuleName:name, _Load:[bool], Module) :<-
    "Find named module (if present)"::
    get(S?modules, member, ModuleName, Module).


ensure_loaded(_S, _Module:name) :->
    "Stub -- the .doc store retired; on-demand loading is gone"::
    fail.


for_all_cards(S, Msg:code) :->
    "Run code on all cards in every module"::
    send(S?modules, for_all, message(@arg2, for_all_cards, Msg)).

:- pce_end_class.


                /********************************
                *            MODULES            *
                ********************************/

:- pce_begin_class(man_module(name), object,
                   "Group of manual cards (man_card)").

variable(name,          name,           get,    "Name of the module").
variable(space,         name,           none,   "Name of the related space").
variable(id_table,      hash_table,     get,    "Mapping CardId --> Card").
variable(modified,      bool,           get,    "Indicate has changed").
variable(current_id,    number,         both,   "Numeric id for next card").

initialise(M, Space:man_space, Name:name) :->
    "Create from space and name"::
    (   get(Space?modules, member, Name, _)
    ->  send(@display, inform, @nil, "XPCE Manual",
             'Module %s already exists', Name)
    ;   send(M, slot, name,     Name),
        send(M, slot, id_table, new(hash_table)),
        send(M, slot, space,    Space?name),
        send(M, slot, modified, @off),
        send(M, slot, current_id, number(1)),
        send(Space?modules, append, Name, M)
    ).


space(M, Space) :<-
    "Space this module belongs to"::
    get(@man_space_table, member, ?(M, slot, space), Space).


modified(M, Val:bool) :->
    "Set modified value"::
    send(M, slot, modified, Val),
    (   Val == @on
    ->  send(M?space, modified, @on)
    ;   true
    ).


card(M, Id:'int|name', Card) :<-
    "Card from id"::
    get(M?id_table, member, Id, Card).


for_all_cards(M, Msg:code) :->
    "Run code on all cards of module"::
    send(M?id_table, for_all,
         message(Msg, forward, @arg2)).

:- pce_end_class.


                /********************************
                *             CARDS             *
                ********************************/

:- pce_begin_class(man_card(module, name), object,
                   "Card of the online manual").

variable(identifier,    'int|name',     get,    "Unique identifier").
variable(module,        man_module,     get,    "Module I belong to").
variable(last_modified, date,           get,    "Last time a slot was").
variable(name,          name,           get,    "My name").
variable(summary,       string*,        get,    "Half-line summary").
variable(description,   string*,        get,    "Full description").
variable(see_also,      chain*,         none,   "`See Also' references").
variable(inherit,       chain*,         none,   "Inherit descriptions").


initialise(C, Mod:man_module, Name:[name], Id:[name]) :->
    "Initialise from module, name and identifier"::
    (   Id == @default
    ->  get(Mod?current_id, value, Ident),
        send(Mod?current_id, plus, 1)
    ;   Ident = Id
    ),
    send(C, slot, identifier,    Ident),
    send(C, slot, name,          Name),
    send(C, slot, module,        Mod),
    send(C, slot, last_modified, new(date)),
    send(Mod?id_table, append, C?identifier, C),
    send(Mod, modified, @on).


unlink(C) :->
    "Delete id from associated module"::
    send(C?module, modified, @on),
    send(C?module?id_table, delete, C?identifier).


space(C, Space) :<-
    "Space card resides in"::
    get(C?module, space, Space).


identifier(C, Id:name) :->
    "Set named identifier"::
    get(C, identifier, Old),
    get(C?module, id_table, Table),
    send(Table, append, Id, C),
    send(C, slot, identifier, Id),
    send(Table, delete, Old).


        /* SLOTS */

store(C, Slot:name, Value:any) :->
    "Store a slot value (normally a string)"::
    get(C, slot, Slot, OldValue),
    (   send(OldValue, equal, Value)
    ->  true
    ;   send(C?last_modified, current),
        send(C?module, modified, @on),
        send(C, slot, Slot, Value)
    ).

fetch(C, Slot:name, Value:any) :<-
    "Read a slot value (possibly inherit)"::
    get(C, slot, Slot, Value),
    Value \== @nil.


inherited_fetch(C, Slot:name, Tuple:tuple) :<-
    "Read a slot value (possibly inherit)"::
    (   get(C, slot, Slot, Value),
        Value \== @nil,
        new(Tuple, tuple(C?object, Value))
    ->  true
    ;   get(C, related, inherit, Chain),
        get(Chain, find, ?(@arg1, fetch, Slot), From),
        get(From, fetch, Slot, Value),
        Value \== @nil,
        get(From, object, Object),
        new(Tuple, tuple(Object, Value))
    ).


        /* RELATIONS */

rel_id(C, To:man_card, Id:'int|name') :<-
    "Relation id (internal/external)"::
    get(To, module, ToModule),
    (   get(C, module, ToModule)
    ->  get(To, identifier, Id)
    ;   get(ToModule, name, ToName),
        get(To, identifier, ToId),
        atomic_list_concat([$, ToName, $, ToId], Id)
    ).


expand_id(C, Id:'int|name', Card) :<-
    "Expand a relation id to a card"::
    (   atom(Id),
        get(Id, scan, '$%[^$]$%s', vector(ModuleName, LocalId))
    ->  get(C, space, Space),
        get(Space, module, ModuleName, @on, Module),
        get(Module, card, LocalId, Card)
    ;   get(C?module, card, Id, Card)
    ).


relate(C, Type:name, To:man_card) :->
    "Create typed relation to card"::
    get(C, slot, Type, Chain),
    (   Chain == @nil
    ->  send(C, slot, Type, chain(?(C, rel_id, To)))
    ;   send(Chain, add, ?(C, rel_id, To))
    ),
    send(C?module, modified, @on).


move_relation_after(C, Type:name, To:man_card, Before:[man_card]) :->
    "Move relation to be before last argument or first"::
    get(C, slot, Type, Val), Val \== @nil,
    (   Before == @default
    ->  send(Val, move_after, ?(C, rel_id, To))
    ;   send(Val, move_after, ?(C, rel_id, To), ?(C, rel_id, Before))
    ),
    send(C?module, modified, @on).


unrelate(C, Type:name, To:man_card) :->
    "Destroy typed relation to card"::
    get(C, slot, Type, Val),
    (   Val == @nil
    ->  true
    ;   send(Val, delete, ?(C, rel_id, To))
    ).


related(C, Type:name, To:man_card) :->
    "Test if I'm related to card"::
    get(C, slot, Type, Val), Val \== @nil,
    send(Val, member, ?(C, rel_id, To)).


related(C, Type:name, Result) :<-
    "New chain with related cards"::
    get(C, slot, Type, Val), Val \== @nil,
    get(Val, map, new(?(C, expand_id, @arg1)), Result).


renamed_module(C, Old:name, New:name) :->
    "Scan (see-also, inherit) relations for module and rename"::
    forall(member(RelName, [see_also, inherit]),
           renamed_module_relations(C, RelName, Old, New)).

renamed_module_relations(C, RelName, Old, New) :-
    get(C, slot, RelName, Chain),
    Chain \== @nil,
    !,
    send(Chain, for_all,
         message(@prolog, renamed_module_relation,
                 Chain, @arg1, Old, New)).
renamed_module_relations(_, _, _, _).

renamed_module_relation(Ch, Id, Old, New) :-
    atom(Id),
    get(Id, scan, '$%[^$]$%s', vector(OldString, LocalIdString)),
    send(Old, equal, OldString),
    !,
    get(LocalIdString, value, LocalId),
    atomic_list_concat(['$', New, '$', LocalId], NewId),
    send(Ch, replace, Id, NewId).
renamed_module_relation(_, _, _, _).


man_card(C, _Create:[bool], C) :<-
    "The card for a card is the card itself"::
    true.

object(C, C) :<-
    "For a general card, the object itself"::
    true.

has_source(_C) :->
    "Cards don't have source ..."::
    fail.

man_summary(C, S) :<-
    "General summary string"::
    new(S, string('%s\t%s\t%s', C?man_id, C?name, C?summary)),
    (   send(C, has_help)
    ->  send(S, append, ' (+)')
    ;   true
    ).

man_name(C, S) :<-
    "General name string"::
    new(S, string('%s \t%s', C?man_id, C?name)).

delete_unreferenced(C) :->
    "Delete if not referenced"::
    (   pce_catch_error(bad_return_value, get(C, object, _))
    ->  true
    ;   get(C, identifier, Id),
        (   send(Id, sub, '.win_')  % windows-specific card
        ->  true
        ;   format(user_error, 'Deleting card ~w~n', [Id]),
            free(C)
        )
    ).

:- pce_end_class.

