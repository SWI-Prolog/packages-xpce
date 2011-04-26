/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (C): 1985-2002, University of Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(emacs_logtalk_mode, []).
:- use_module(library(pce)).
:- use_module(prolog_mode).
:- use_module(library(operators)).	% push/pop operators


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module deals with colourisation of .lgt files.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


		 /*******************************
		 *	      LOGTALK MODE		*
		 *******************************/

:- emacs_begin_mode(logtalk, prolog,
		    "Mode for editing Logtalk documents",
		    % BINDINGS
		    [
		    ],
		    % SYNTAX TABLE
		    [
		    ]).

colourise_buffer(M) :->
	"Cross-reference the buffer and set up colours"::
	push_logtalk_operators,
	setup_call_cleanup(
		true,
		send_super(M, colourise_buffer),
		pop_logtalk_operators).

:- emacs_end_mode.


		 /*******************************
		 *	   SYNTAX RULES		*
		 *******************************/

:- multifile
	emacs_prolog_colours:goal_colours/2.

%	goal_colours(+Goal, -Colours)
%
%	Colouring of special goals.

% source file directives
goal_colours(set_logtalk_flag(_, _), built_in-[predicates]).
% conditional compilation directives
goal_colours(if(_), built_in-[classify]).
goal_colours(elif(_), built_in-[classify]).
goal_colours(else, built_in-[]).
goal_colours(endif, built_in-[]).
% entity directives
goal_colours(calls(_), built_in-[identifier]).
goal_colours(category(_), built_in-[identifier]).
goal_colours(category(_, _), built_in-[identifier,classify]).
goal_colours(category(_, _, _), built_in-[identifier,classify,classify]).
goal_colours(dynamic, built_in-[]).
goal_colours(end_category, built_in-[]).
goal_colours(end_object, built_in-[]).
goal_colours(end_protocol, built_in-[]).
goal_colours(info(_), built_in-[classify]).
goal_colours(initialization(_), built_in-[predicates]).
goal_colours(object(_), built_in-[identifier]).
goal_colours(object(_, _), built_in-[identifier,classify]).
goal_colours(object(_, _, _), built_in-[identifier,classify,classify]).
goal_colours(object(_, _, _, _), built_in-[identifier,classify,classify,classify]).
goal_colours(object(_, _, _, _, _), built_in-[identifier,classify,classify,classify,classify]).
goal_colours(protocol(_), built_in-[identifier]).
goal_colours(protocol(_, _), built_in-[identifier,classify]).
goal_colours(synchronized, built_in-[predicates]).
goal_colours(threaded, built_in-[predicates]).
goal_colours(uses(_), built_in-[identifier]).
% predicate directives
goal_colours(alias(_, _, _), built_in-[classify,predicates,predicates]).
goal_colours(annotation(_), built_in-[predicates]).
goal_colours(coinductive(_), built_in-[predicates]).
goal_colours(discontiguous(_), built_in-[predicates]).
goal_colours(dynamic(_), built_in-[predicates]).
goal_colours(info(_, _), built_in-[predicates,classify]).
goal_colours(meta_predicate(_), built_in-[classify]).
goal_colours(mode(_, _), built_in-[classify,classify]).
goal_colours(multifile(_), built_in-[classify]).
goal_colours(op(_, _, _), built_in-[classify,classify,classify]).
goal_colours(private(_), built_in-[predicates]).
goal_colours(protected(_), built_in-[predicates]).
goal_colours(public(_), built_in-[predicates]).
goal_colours(synchronized(_), built_in-[predicates]).
goal_colours(uses(_, _), built_in-[identifier,predicates]).
% enumerating objects, categories and protocols
goal_colours(current_category(_), built_in-[classify]).
goal_colours(current_object(_), built_in-[classify]).
goal_colours(current_protocol(_), built_in-[classify]).
% enumerating objects, categories and protocols properties
goal_colours(category_property(_, _), built_in-[classify,classify]).
goal_colours(object_property(_, _), built_in-[classify,classify]).
goal_colours(protocol_property(_, _), built_in-[classify,classify]).
% creating new objects, categories and protocols
goal_colours(create_category(_, _, _, _), built_in-[predicates]).
goal_colours(create_object(_, _, _, _), built_in-[predicates]).
goal_colours(create_protocol(_, _, _), built_in-[predicates]).
% abolishing objects, categories and protocols
goal_colours(abolish_category(_), built_in-[classify]).
goal_colours(abolish_object(_), built_in-[classify]).
goal_colours(abolish_protocol(_), built_in-[classify]).
% objects, categories and protocols relations
goal_colours(extends_object(_, _), built_in-[classify,classify]).
goal_colours(extends_object(_, _, _), built_in-[classify,classify,classify]).
goal_colours(extends_protocol(_, _), built_in-[classify,classify]).
goal_colours(extends_protocol(_, _, _), built_in-[classify,classify,classify]).
goal_colours(extends_category(_, _), built_in-[classify,classify]).
goal_colours(extends_category(_, _, _), built_in-[classify,classify,classify]).
goal_colours(implements_protocol(_, _), built_in-[classify,classify]).
goal_colours(implements_protocol(_, _, _), built_in-[classify,classify,classify]).
goal_colours(conforms_to_protocol(_, _), built_in-[classify,classify]).
goal_colours(conforms_to_protocol(_, _, _), built_in-[classify,classify,classify]).
goal_colours(imports_category(_, _), built_in-[classify,classify]).
goal_colours(imports_category(_, _, _), built_in-[classify,classify,classify]).
goal_colours(instantiates_class(_, _), built_in-[classify,classify]).
goal_colours(instantiates_class(_, _, _), built_in-[classify,classify,classify]).
goal_colours(specializes_class(_, _), built_in-[classify,classify]).
goal_colours(specializes_class(_, _, _), built_in-[classify,classify,classify]).
goal_colours(complements_object(_, _), built_in-[classify,classify]).
% event handling
goal_colours(abolish_events(_, _, _, _, _), built_in-[classify,classify,classify,classify,classify]).
goal_colours(current_event(_, _, _, _, _), built_in-[classify,classify,classify,classify,classify]).
goal_colours(define_events(_, _, _, _, _), built_in-[classify,classify,classify,classify,classify]).
% multi-threading meta-predicates
goal_colours(threaded(_), built_in-[classify]).
goal_colours(threaded_call(_), built_in-[classify]).
goal_colours(threaded_call(_, _), built_in-[classify,classify]).
goal_colours(threaded_once(_), built_in-[classify]).
goal_colours(threaded_once(_, _), built_in-[classify,classify]).
goal_colours(threaded_ignore(_), built_in-[classify]).
goal_colours(threaded_exit(_), built_in-[classify]).
goal_colours(threaded_exit(_, _), built_in-[classify,classify]).
goal_colours(threaded_peek(_), built_in-[classify]).
goal_colours(threaded_peek(_, _), built_in-[classify,classify]).
goal_colours(threaded_wait(_), built_in-[classify]).
goal_colours(threaded_notify(_), built_in-[classify]).
% compiling and loading objects, categories and protocols
goal_colours(logtalk_compile(_), built_in-[classify]).
goal_colours(logtalk_compile(_, _), built_in-[classify,classify]).
goal_colours(logtalk_load(_), built_in-[classify]).
goal_colours(logtalk_load(_, _), built_in-[classify,classify]).
goal_colours(logtalk_library_path(_, _), built_in-[classify,classify]).
goal_colours(logtalk_load_context(_, _), built_in-[classify,classify]).
% flags
goal_colours(current_logtalk_flag(_, _), built_in-[classify,classify]).
goal_colours(set_logtalk_flag(_, _), built_in-[classify,classify]).
/*
% others
goal_colours(forall(_, _), built_in-[classify,classify]).
goal_colours(retractall(_), built_in-[classify]).
*/
% execution context methods
goal_colours(self(_), built_in-[classify]).
goal_colours(sender(_), built_in-[classify]).
goal_colours(this(_), built_in-[classify]).
goal_colours(parameter(_, _), built_in-[classify,classify]).
/*
% reflection methods
goal_colours(current_predicate(_), built_in-[classify]).
goal_colours(predicate_property(_, _), built_in-[classify,classify]).
% database methods
goal_colours(abolish(_), built_in-[db]).
goal_colours(asserta(_), built_in-[db]).
goal_colours(assertz(_), built_in-[db]).
goal_colours(clause(_, _), built_in-[db,classify]).
goal_colours(retract(_), built_in-[db]).
goal_colours(retractall(_), built_in-[db]).
% meta-call methods
goal_colours(call(_), built_in-[classify]).
goal_colours(ignore(_), built_in-[classify]).
goal_colours(once(_), built_in-[classify]).
goal_colours(\+(_), built_in-[classify]).
% exception-handling methods
goal_colours(catch(_, _, _), built_in-[classify,classify,classify]).
goal_colours(throw(_), built_in-[classify]).
% all solutions methods
goal_colours(findall(_, _, _), built_in-[classify,classify,classify]).
goal_colours(forall(_, _), built_in-[classify,classify]).
goal_colours(bagof(_, _, _), built_in-[classify,setof,classify]).
goal_colours(setof(_, _, _), built_in-[classify,setof,classify]).
*/
% event handler methods
goal_colours(before(_, _, _), built_in-[classify,classify,classify]).
goal_colours(after(_, _, _), built_in-[classify,classify,classify]).
/*
% DCGs rules parsing methods and non-terminals
%call//1
goal_colours(phrase(_, _), built_in-[classify,classify]).
goal_colours(phrase(_, _, _), built_in-[classify,classify,classify]).
% term and goal expansion methods
goal_colours(expand_term(_, _), built_in-[classify,classify]).
goal_colours(term_expansion(_, _), built_in-[classify,classify]).
goal_colours(expand_goal(_, _), built_in-[classify,classify]).
goal_colours(goal_expansion(_, _), built_in-[classify,classify]).
*/
% message sending
goal_colours('::'(_, _), built_in-[classify,classify]).
goal_colours('::'(_), built_in-[classify]).
goal_colours('^^'(_), built_in-[classify]).
% calling external code
goal_colours('{}'(_), built_in-[classify]).
% context-switching calls
goal_colours('<<'(_, _), built_in-[classify,classify]).
% direct calls of imported predicates
goal_colours(':'(_), built_in-[classify]).

%emacs_prolog_colours:term_colours(Term, Colours) :-
%	term_colours(Term, Colours).
emacs_prolog_colours:goal_colours(Term, Colours) :-
	goal_colours(Term, Colours).

		 /*******************************
		 *	   SYNTAX HOOKS		*
		 *******************************/

:- multifile
	emacs_prolog_mode:alternate_syntax/3.


emacs_prolog_mode:alternate_syntax(logtalk,
				   emacs_logtalk_mode:push_logtalk_operators,
				   emacs_logtalk_mode:pop_logtalk_operators).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Note that we could generalise this to deal with all included files.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

push_logtalk_operators :-
	logtalk_operators(Ops),
	'$set_source_module'(SM, SM),
	push_operators(SM:Ops).

pop_logtalk_operators :-
	pop_operators.

logtalk_operators([
	% message sending operators
	op(600, xfy, ::),	% send to object
	op(600,  fy, ::),	% send to self
	op(600,  fy, ^^),	% "super" call (calls an overriden, inherited method definition)
	% mode operators
	op(200,  fy, +),	% input argument (instantiated)
	op(200,  fy, ?),	% input/output argument
	op(200,  fy, @),	% input argument (not modified by the call)
	op(200,  fy, -),	% output argument (not instantiated)
	% imported category predicate call operator
	op(600,  fy, :)
]).
