/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org/packages/xpce/
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

:- module(pce_util,
          [ get_object/3,               % +Receiver, +Selector, ..., -Result
            get_object/4,
            get_object/5,
            get_object/6,
            get_object/7,
            get_object/8,

            send_list/2,                % +Receivers, +Methods
            send_list/3,                % +Receivers, +Methods, +Args

            get_chain/3,                % +Receiver, +Selector, -List
            chain_list/2,               % ?Chain, ?List
            member_chain/2,             % ?Elem, +Chain

            default/3,                  % +MethodArg, +Default, -Arg

            pce_text_to_regex/2,        % +Text, -Regex

            center_on/2                 % +Item, +On
          ]).


:- meta_predicate
    get_object(+, :, -),
    get_object(+, :, +, -),
    get_object(+, :, +, +, -),
    get_object(+, :, +, +, +, -),
    get_object(+, :, +, +, +, +, -),
    get_object(+, :, +, +, +, +, +, -),

    send_list(:, +),
    send_list(:, +, +),
    send_list1(:, +),
    send_list1(:, +, +),

    get_chain(+, :, -).


:- use_module(library(pce)).
:- import(pce_principal:'$chain_list'/2).
:- import(pce_principal:'$member_chain'/2).

%%   get_object(+Object, +Selector, +Arg..., -Output) is semidet.
%
%   Succeeds once if Output is the value returned by invoking get method
%   called Selector on Object.  Output is an object description, except for the
%   special objects @nil, @default, @on and @off all of which are both
%   object descriptions and object names.

get_object(Obj, Sel, Out) :-
    get(Obj, Sel, R),
    get_to_object(R, Out).
get_object(Obj, Sel, A1, Out) :-
    get(Obj, Sel, A1, R),
    get_to_object(R, Out).
get_object(Obj, Sel, A1, A2, Out) :-
    get(Obj, Sel, A1, A2, R),
    get_to_object(R, Out).
get_object(Obj, Sel, A1, A2, A3, Out) :-
    get(Obj, Sel, A1, A2, A3, R),
    get_to_object(R, Out).
get_object(Obj, Sel, A1, A2, A3, A4, Out) :-
    get(Obj, Sel, A1, A2, A3, A4, R),
    get_to_object(R, Out).
get_object(Obj, Sel, A1, A2, A3, A4, A5, Out) :-
    get(Obj, Sel, A1, A2, A3, A4, A5, R),
    get_to_object(R, Out).

get_to_object(Ref, Object) :-
    (   atomic(Ref),
        \+ is_object_reference(Ref)  % which is atomic as well
    ->  Object = Ref
    ;   object(Ref, Object)
    ).


%!  send_list(+ListOfObjs, +ListOfSels)
%
%   Send a messages to the carthesian product of ListOfObjs and
%   ListOfSels.

send_list(X, _) :-
    var(X),
    throw(error(instantiation_error, _)).
send_list(_, X) :-
    var(X),
    throw(error(instantiation_error, _)).
pce_ifhostproperty(prolog(quintus), [],
(   send_list([], _) :- !)).
send_list(_, []) :- !.
pce_ifhostproperty(prolog(quintus), [],
(   send_list([Object|Objects], Selectors) :- !,
        send_list(Object, Selectors),
        send_list(Objects, Selectors))).
send_list(Object, [Selector|Selectors]) :-
    !,
    send_list(Object, Selector),
    send_list(Object, Selectors).
send_list(Object, Selector) :-
    send_list1(Object, Selector).

send_list1(Module:Obj, Selector) :-
    atom(Module),
    !,
    send_list_module(Obj, Selector, Module).
send_list1(Object, Selector) :-
    send(Object, Selector).

send_list_module([], _, _) :- !.
send_list_module(_, [], _) :- !.
send_list_module([Object|Objects], Selectors, Module) :-
    !,
    send_list_module(Object, Selectors, Module),
    send_list_module(Objects, Selectors, Module).
send_list_module(Object, [Selector|Selectors], Module) :-
    !,
    send_list_module(Object, Selector, Module),
    send_list_module(Object, Selectors, Module).
send_list_module(Object, Selector, Module) :-
    send(Object, Module:Selector).


%!   send_list(+ListOfObjs, +ListOfSels, +ListOfArgs)
%
%   Send a messages to the carthesian product of ListOfObjs and
%   ListOfSels.

send_list(X, _, _) :-
    var(X),
    throw(error(instantiation_error, _)).
send_list(_, X, _) :-
    var(X),
    throw(error(instantiation_error, _)).
send_list(_, _, X) :-
    var(X),
    throw(error(instantiation_error, _)).
pce_ifhostproperty(prolog(quintus), [],
(   send_list([], _,  _) :- !)).
send_list(_, [], _) :- !.
send_list(_, _, []) :- !.
pce_ifhostproperty(prolog(quintus), [],
(   send_list([Object|Objects], Selectors, Arguments) :- !,
        send_list(Object, Selectors, Arguments),
        send_list(Objects, Selectors, Arguments))).
send_list(Objects, [Selector|Selectors], Arguments) :-
    !,
    send_list(Objects, Selector, Arguments),
    send_list(Objects, Selectors, Arguments).
send_list(Object, Selector, [Argument|Arguments]) :-
    !,
    send_list(Object, Selector, Argument),
    send_list(Object, Selector, Arguments).
send_list(Object, Selector, Argument) :-
    send_list1(Object, Selector, Argument).

send_list1(Module:Obj, Selector, Arg) :-
    atom(Module),
    !,
    send_list_module(Obj, Selector, Arg, Module).
send_list1(Obj, Selector, Arg) :-
    send(Obj, Selector, Arg).

send_list_module([], _, _, _) :- !.
send_list_module(_, [], _, _) :- !.
send_list_module(_, _, [], _) :- !.
send_list_module([Object|Objects], Selectors, Arguments, Module) :-
    !,
    send_list_module(Object, Selectors, Arguments, Module),
    send_list_module(Objects, Selectors, Arguments, Module).
send_list_module(Objects, [Selector|Selectors], Arguments, Module) :-
    !,
    send_list_module(Objects, Selector, Arguments, Module),
    send_list_module(Objects, Selectors, Arguments, Module).
send_list_module(Object, Selector, [Argument|Arguments], Module) :-
    !,
    send_list_module(Object, Selector, Argument, Module),
    send_list_module(Object, Selector, Arguments, Module).
send_list_module(Object, Selector, Argument, Module) :-
    send(Object, Module:Selector, Argument).


%%   get_chain(+Object, +Selector, -List:list) is semidet.
%
%   List is a Prolog list constructed from the PCE chain returned by <-Selector
%   on Object.  get_chain/3 returns a list of object names,

get_chain(Object, Selector, List) :-
    get(Object, Selector, Chain),
    chain_list(Chain, List).


%!  chain_list(+Chain, -List) is det.
%!  chain_list(-Chain, +List) is det.
%
%   Convert between a Prolog list and an XPCE chain object.  If Chain
%   is given, List holds its elements at the moment of the call.  This
%   is safe if other threads use or modify the chain.  Chain may be
%   @nil, which is converted to the empty list, or a function such as
%   `Obj?members`.  Fails if Chain is not a chain.

chain_list(Chain, List) :-
    nonvar(Chain),
    !,
    (   Chain == @nil
    ->  List = []
    ;   to_object(Chain, ChainObject),
        '$chain_list'(ChainObject, List)
    ).
chain_list(Chain, List) :-
    new(Chain, chain),
    send_list(Chain, append, List).

%!  member_chain(?Elem, +Chain) is nondet.
%
%   True when Elem is an element of Chain.  Elements are enumerated in
%   order from a snapshot of Chain taken at the first call, so changes
%   to Chain while enumerating do not affect the result, except that
%   elements that are freed before they are reached are skipped.  This
%   is safe if other threads use or modify the chain.  Chain may be a
%   function such as `Obj?members`.  Fails if Chain is @nil or not a
%   chain.

member_chain(Elem, Chain) :-
    Chain \== @nil,
    to_object(Chain, ChainObject),
    '$member_chain'(Elem, ChainObject).

to_object(Ref, Ref) :-
    object(Ref),
    !.
to_object(Term, Obj) :-
    new(Obj, Term).


                /********************************
                *             DEFAULTS          *
                ********************************/

%!  default(+Argument, +Default, -Value) is det.
%
%   Get the default value for an argument.  Default is either a
%   plain value or a term class_variable(+Object, +Name).

default(@default, Default, Value) :-
    !,
    (   var(Default)
    ->  Value = Default
    ;   (   Default = class_variable(Obj, Name)
        ;   Default = resource(Obj, Name)
        )
    ->  (   get(Obj, class_variable_value, Name, Value)
        ->  true
        ;   pce_error(get_class_variable_failed(Name, Obj))
        )
    ;   Value = Default
    ).
default(Value, _Default, Value).


                /********************************
                *            LAYOUT             *
                ********************************/

%!  center_on(+Item, +On) is det.
%
%   Items in a row of a dialog line up on their reference point, which
%   is at the baseline of the text of most items and at the bottom of a
%   tool bar or picture.  Center Item vertically on On instead.

center_on(Item, On) :-
    get(On, height, OnH),
    get(On, reference, OnRef),
    get(OnRef, y, OnY),
    get(Item, height, H),
    Ref is (H + 2*OnY - OnH)//2,
    send(Item, reference, point(0, Ref)).


                /********************************
                *             REGEX             *
                ********************************/

%!  pce_text_to_regex(+Pattern, -Regex) is semidet.
%
%   Convert text to a pce regular expression object. Fails if Pattern is
%   not a valid regular expression. This   is  intended for filters that
%   are applied while typing, where the text   is incomplete most of the
%   time.

pce_text_to_regex(Pattern, Regex) :-
    send(@pce, last_error, @nil),
    new(Regex, regex(Pattern)),
    ignore(pce_catch_error(_, send(Regex, search, ''))),
    get(@pce, last_error, @nil).
