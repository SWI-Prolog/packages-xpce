/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (C): 1985-2011, University of Amsterdam
			      VU University Amsterdam

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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(prolog_term_view,
	  [ view_term/1,		% +Term
	    view_term/2			% +Term, +Attributes
	  ]).
:- use_module(library(pce)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(pprint).

/** <module> Graphical viewer for Prolog terms

This module implements an XPCE  widget   that  exploits  print_term/2 to
display a Prolog term. The widget provides   buttons to control the most
important options to control the output.
*/

%%	view_term(@Term) is det.
%%	view_term(@Term, +Options) is det.
%
%	Display a Prolog term in  an   XPCE  window. Options defines the
%	following options:
%
%	    * view(?Object)
%	    XPCE object reference to create or re-use.
%	    * clear(+Boolean)
%	    Clear view before showing term (default: =true=)
%	    * open(+Boolean)
%	    Send an ->open message to the XPCE object (default: =true=)
%	    * expose(+Boolean)
%	    Send an ->expose message to the XPCE object (default: =false)
%	    * write_option(+Options)
%	    Options to pass to write_term/3.  Current defaults are:
%	        - quoted(true)
%	        - portray(true)
%	        - numbervars(true)
%	        - attributes(portray)
%	        - max_depth(100)

view_term(Term) :-
	view_term(Term, []).

view_term(Term, _) :-			% TBD: Turn into user hook!
	object(Term), !,
	manpce,
	send(@manual, inspect, Term).
view_term(Term, Attributes0) :-
	defaults(Defs),
	append(Attributes0, Defs, Attributes),
	tv(Term, Attributes).

defaults([ view(@view_term),
	   clear(true),
	   open(true),
	   expose(false),
	   write_options([ quoted(true),
			   portray(true),
			   numbervars(true),
			   attributes(portray),
			   max_depth(100)
			 ])
	 ]).

tv(Term,Opts) :-
	option(view(V),Opts),
	(option(clear(true),Opts)         -> send(V, clear)               ; true),
	(option(open(true),Opts)          -> send(V, open)                ; true),
	(option(expose(true),Opts)        -> send(V, expose)              ; true),
	(option(comment(Comment),Opts)    -> send(V, label, Comment)      ; true),
	(option(source_object(Frag),Opts) -> send(V, source_object, Frag) ; true),
	get(V, text_buffer, TB),
	setup_call_cleanup(pce_open(TB, write, Fd),
			   emit_term(Term, [output(Fd)|Opts]),
			   close(Fd)),
	send(V, caret, 0),
	send(V, editable, @off),
	(   option(write_options(WrtOpts),Opts)
	->  send(V, show_options, WrtOpts)
	;   true
	).


%%	emit_term(+Term, +Options) is det.
%
%	Actually emit the term into @view_term.   One  of the Options is
%	output(+Stream).

emit_term(Term, Options) :-
	is_stream(Term), !,
	option(output(Out), Options, current_output),
	print_stream_properties(Term, Out).
emit_term(Term, Options) :-
	current_blob(Term, record), !,
	option(output(Out), Options, current_output),
	print_record_properties(Term, Out).
emit_term(Term, Options) :-
	current_blob(Term, clause), !,
	option(output(Out), Options, current_output),
	print_clause_properties(Term, Out).
emit_term(Term, Options) :-
	clause_term(Term), !,
	option(output(Out), Options, current_output),
	portray_clause(Out, Term, Options).
emit_term(Term, Options) :-
	print_term(Term, Options).


clause_term(Var) :-
	var(Var), !, fail.
clause_term(_:-_).
clause_term(:-_).
clause_term((_,_)).
clause_term(_;_).
clause_term(_->_).
clause_term(_*->_).


print_stream_properties(Stream, Out) :-
	format(Out, 'Stream ~w~n', [Stream]),
	(   stream_property(Stream, P),
	    (	atom(P)
	    ->	format(Out, '\t~q~n', [P])
	    ;	P =.. [Name,Value],
		format(Out, '\t~w = ~p~n', [Name, Value])
	    ),
	    fail
	;   true
	).

print_record_properties(Record, Out) :-
	format(Out, 'Record reference ~w~n', [Record]),
	(   recorded(Key, Value, Record)
	->  format(Out, '\tKey:   ~p~n', [Key]),
	    format(Out, '\tValue: ~p~n', [Value])
	;   format(Out, '\t<erased>~n', [])
	).

print_clause_properties(Ref, Out) :-
	format(Out, 'Clause reference ~w~n', [Ref]),
	(   clause(Head, Body, Ref)
	->  nl(Out),
	    portray_clause(Out, (Head:-Body))
	;   format(Out, '\t<erased>~n', [])
	).



		 /*******************************
		 *      CLASS TERM-VIEWER	*
		 *******************************/

:- pce_global(@view_term, new(term_viewer)).

:- pce_begin_class(term_viewer, frame,
		   "Pretty-print a Prolog term").

initialise(TV) :->
	send_super(TV, initialise),
	send(TV, append, new(TD, dialog)),
	send(new(view), below, TD),
	send(TD, border, size(0,2)),
	send(TD, append, new(M, menu(options, toggle,
				     message(TV, update)))),
	send(M, layout, horizontal),
	send_list(M, append,
		  [ portray,
		    quoted,
		    max_depth
		  ]),
	send(TD, append, int_item(max_depth, 100,
				  message(TV, update), 1),
	     right).

clear(TV) :->
	get(TV, member, view, View),
	send(View, clear).

text_buffer(TV, TB:text_buffer) :<-
	get(TV, member, view, View),
	get(View, text_buffer, TB).

caret(TV, Caret:int) :->
	get(TV, member, view, View),
	send(View, caret, Caret).

editable(TV, E:bool) :->
	get(TV, member, view, View),
	send(View, editable, E).

source_object(TV, Obj:object) :->
	send(TV, delete_hypers, source),
	new(_, hyper(TV, Obj, source, view)).

update(TV) :->
	get(TV, member, dialog, D),
	get(D, member, options, Menu),
	get(Menu, selection, Options),
	make_options([ portray,
		       quoted
		     ], Options, OptionList0),
	get(D, member, max_depth, DepthItem),
	(   send(Options, member, max_depth),
	    send(DepthItem, active, @on),
	    get(DepthItem, selection, MaxDepth)
	->  OptionList = [max_depth(MaxDepth)|OptionList0]
	;   send(DepthItem, active, @off),
	    OptionList = OptionList0
	),
	get(TV, hypered, source, Source),
	get(Source, value, Term),
	tv(Term,
	   [ view(TV),
	     clear(true),
	     write_options(OptionList)
	   ]).

make_options([], _, [ numbervars(true), attributes(portray) ]).
make_options([H0|T0], Selection, [H|T]) :-
	(   send(Selection, member, H0)
	->  V = true
	;   V = false
	),
	H =.. [H0,V],
	make_options(T0, Selection, T).

show_options(V, Options:prolog) :->
	"Show current option values"::
	get(V, member, dialog, D),
	get(D, member, options, Menu),
	send(Menu, selected, max_depth, @off),
	get(D, member, max_depth, DepthItem),
	send(DepthItem, active, @off),
	(   member(Option, Options),
	    functor(Option, Name, 1),
	    get(Menu, member, Name, Item),
	    arg(1, Option, Value),
	    (	Name == max_depth
	    ->	send(Item, selected, @on),
		send(DepthItem, active, @on),
		send(DepthItem, selection, Value)
	    ;	send(Item, selected, Value)
	    ),
	    fail
	;   true
	).

:- pce_end_class.
