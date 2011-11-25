/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org/projects/xpce/
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


:- module(emacs_prolog_colours, []).
:- use_module(library(pce)).
:- use_module(library(emacs_extend)).
:- use_module(library(pce_prolog_xref)).
:- use_module(library(pce_meta)).
:- use_module(library(predicate_options)).
:- use_module(library(prolog_source)).
:- use_module(library(prolog_colour)).
:- use_module(library(lists)).
:- use_module(library(operators)).
:- use_module(library(debug)).
:- use_module(library(edit)).
:- use_module(library(error)).

/** <module> PceEmacs Prolog mode colour code

This  module  defines  the   PceEmacs    colourization   code.  PceEmacs
colourization  is  based  on  actual    parsing   and  cross-referencing
information gathered at the fly.

The functionality can be extended  using   various  multifile hooks. See
library(emacs/logtalk_mode) for an example.

@tbd	Document this file and share it with PlDoc's colour code.
*/


:- emacs_extend_mode(prolog,
		     [ colourise_or_recenter = key('\\C-l'),
		       colourise_buffer = key(key_top_5)
		     ]).

class_variable(auto_colourise_size_limit, int, 50000,
	       "Auto-colourise if buffer is smaller then this").

colourise_clause(M, From:from=[int]) :->
	"Colourise the current clause"::
	get(M, colourise_clause, From, _End).

colourise_clause(M, From:from=[int], TermPos:prolog) :<-
	"Colourise the current clause, returning clause layout"::
	send(M, setup_styles),
	(   From == @default
	->  get(M, caret, C),
	    get(M, beginning_of_clause, C, Start)
	;   Start = From
	),
	get(M, text_buffer, TB),
	setup_call_cleanup(
	    pce_open(TB, read, Stream),
	    ( seek(Stream, Start, bof, _),
	      prolog_colourise_term(Stream, TB, colour_item(M),
				    [ subterm_positions(TermPos)
				    ])
	    ),
	    close(Stream)).

:- dynamic
	style_name/2.

colour_item(M, Class, Start, Length) :-
	style_name(Class, Name), !,
	make_fragment(Class, M, Start, Length, Name).
colour_item(_, _, _, _).

setup_styles(M) :->
	"Associate defined syntax-styles"::
	get(M, editor, E),
	(   get(E, attribute, styles_initialised, prolog)
	->  true
	;   send(M, reload_styles),
	    send(E, attribute, styles_initialised, prolog)
	).

reload_styles(M) :->
	"Force reloading the styles"::
	retractall(style_name(_,_)),
	(   style(Class, Name, Style),
	    (	style_name(_, Name)
	    ->  true			% redefined
	    ;   assert(style_name(Class, Name)),
		Style \== @default,
		send(M, style, Name, Style)
	    ),
	    fail
	;   true
	).

%%	style(-Class, -Name, -StyleObject) is nondet.
%
%	Enumerate the known styles, assign a name for them and create an
%	XPCE style object.

style(Class, Name, Style) :-
	syntax_colour(Class, Attributes),
	copy_term(Class, Copy),
	numbervars(Copy, 0, _),
	term_to_atom(Copy, Name),
	maplist(style_attribute, Attributes, PceArgs),
	(   PceArgs == []
	->  Style = @default
	;   Style =.. [style|PceArgs]
	).

style_attribute(Attr, Name := Value) :-
	Attr =.. [Name,Value].

colourise_buffer(M) :->
	"Do cross-referencing and colourising of the whole buffer"::
	statistics(runtime, _),
	new(Class, class(emacs_colour_fragment)),
	get(Class, no_created, @on, OldCreated),

	send_super(M, colourise_buffer),
	send(M, setup_styles),
	send(M, xref_buffer),
	send(M, slot, warnings, 0),
	send(M, slot, errors, 0),
	send(M, report, progress, 'Colourising buffer ...'),
	colourise_buffer(M),
	get(M, errors, Errors),
	statistics(runtime, [_,UsedMilliSeconds]),
	Used is UsedMilliSeconds/1000,
	get(Class, no_created, @on, NewCreated),
	Created is NewCreated - OldCreated,
	(   Errors =:= 0
	->  send(M, report, done,
		 'done, %.2f seconds, %d fragments', Used, Created)
	;   send(M, report, status,
		 'File contains %d errors', Errors)
	).

colourise_comments(M, From:[int], To:[int]) :->
	debug(emacs, 'Colourising comments in ~p..~p', [From, To]),
	get(M, text_buffer, TB),
	send(TB, for_all_comments,
	     message(@prolog, colour_item, comment, TB, @arg1, @arg2),
	     From, To).

colourise_or_recenter(M) :->
	"Colour according to syntax and recenter"::
	(   send(M, colourisation_up_to_date)
	->  send(M, recenter)
	;   send(M, colourise_buffer)
	).

xref_buffer(M, Always:[bool]) :->
	"Run the cross-referencer on buffer"::
	get(M, text_buffer, TB),
	get(TB, generation, G),
	(   (   Always == @on
	    ->  true
	    ;   get(TB, xref_generation, GRef),
		GRef \== G
	    )
	->  send(M, report, progress, 'Cross-referencing buffer ...'),
	    xref_source(TB),
	    send(TB, xref_generation, G),
	    send(M, report, done)
	;   true
	).


		 /*******************************
		 *	     PREDICATES		*
		 *******************************/

colourise_buffer(M) :-
	get(M, text_buffer, TB),
	setup_call_cleanup(
	    pce_open(TB, read, Stream),
	    prolog_colourise_stream(Stream, TB, colour_item(M)),
	    close(Stream)).


%%	make_fragment(+SyntaxClass, +Mode, +From, +Len, +StyleName)
%
%	Actually create the fragment.

make_fragment(goal(Class, Goal), M, F, L, Style) :-
	callable(Goal), !,
	get(M, text_buffer, TB),
	new(Fragment, emacs_goal_fragment(TB, F, L, Style)),
	functor(Goal, Name, Arity),
	send(Fragment, name, Name),
	send(Fragment, arity, Arity),
	(   Class =.. [ClassName,Context],
	    atomic(Context)
	->  send(Fragment, classification, ClassName),
	    send(Fragment, context, Context)
	;   functor(Class, ClassName, _),
	    send(Fragment, classification, ClassName)
	).
make_fragment(class(Type, Class), M, F, L, Style) :-
	atom(Class), !,
	get(M, text_buffer, TB),
	new(Fragment, emacs_class_fragment(TB, F, L, Style)),
	functor(Type, Classification, _),
	send(Fragment, classification, Classification),
	send(Fragment, referenced_class, Class).
make_fragment(syntax_error(Message, Start-End), M, F, L, Style) :- !,
	send(M, colourise_comments, Start, End),
	(   \+ get(M, show_syntax_errors, never)
	->  make_simple_fragment(syntax_error(Message), M, F, L, Style)
	;   true
	),
	get(M, errors, E0),
	E1 is E0 + 1,
	send(M, slot, errors, E1),
	debug(emacs, 'Got syntax error: ~w', [Message]).
make_fragment(directive, M, F, L, Style) :- !,
	EndF is F+L,			% extend to the end of the line
	get(M?text_buffer, scan, EndF, line, 0, end, EOL),
	Len is EOL+1-F,
	make_simple_fragment(directive, M, F, Len, Style).
make_fragment(Class, M, F, L, Style) :-
	make_simple_fragment(Class, M, F, L, Style).

make_simple_fragment(Class, M, F, L, Style) :-
	get(M, text_buffer, TB),
	new(Fragment, emacs_prolog_fragment(TB, F, L, Style)),
	functor(Class, Classification, Arity),
	send(Fragment, classification, Classification),
	(   Arity == 1
	->  arg(1, Class, Context),
	    send(Fragment, context, Context)
	;   true
	).

:- emacs_end_mode.			% end Prolog mode.


		 /*******************************
		 *	   GOAL FRAGMENT	*
		 *******************************/

:- pce_begin_class(emacs_goal_fragment, emacs_colour_fragment,
		   "Fragment for a goal in PceEmacs Prolog mode").

variable(name,		 name, both, "Name of the predicate").
variable(arity,		 int,  both, "Arity of the predicate").
variable(classification, name, both, "XREF classification").
variable(context,	 any*, both, "Classification argument").

:- pce_group(popup).

popup(_GF, Popup:popup) :<-
	"Return popup menu"::
	Popup = @prolog_mode_goal_popup.

:- pce_global(@prolog_mode_goal_popup,
	      make_prolog_mode_goal_popup).

%	make_prolog_mode_goal_popup(-Popup)
%
%	Create the popup and define actions for handling the right-menu
%	on goals.

make_prolog_mode_goal_popup(G) :-
	new(G, popup(goal_actions)),
	Fragment = @arg1,
	new(HasSource, message(Fragment, has_source)),
	new(HasListing, message(Fragment, has_listing)),
	send_list(G, append,
		  [ menu_item(edit_in_tab,
			      message(Fragment, edit, tab),
			      condition := HasSource),
		    menu_item(edit_here,
			      message(Fragment, edit, here),
			      condition := HasSource),
		    menu_item(edit_in_window,
			      message(Fragment, edit, window),
			      condition := HasSource),
		    gap,
		    menu_item(listing,
			      message(Fragment, listing),
			      condition := HasListing),
		    gap,
		    menu_item(documentation,
			      message(Fragment, documentation))
		  ]).


module(F, Module:name) :<-
	"Module for Module:Goal references"::
	get(F, classification, extern),
	get(F, context, Module),
	Module \== @nil.

predicate(F, Pred:prolog_predicate) :<-
	"Get referenced predicate"::
	get(F, name, Name),
	get(F, arity, Arity),
	(   get(F, module, Module)
	->  Spec = Name/Arity
	;   Spec = Module:Name/Arity
	),
	new(Pred, prolog_predicate(Spec)).

head(F, Head:prolog) :<-
	"Return goal-head"::
	get(F, name, Name),
	get(F, arity, Arity),
	functor(Head0, Name, Arity),
	(   get(F, module, M)
	->  Head = M:Head0
	;   Head = Head0
	).

loaded_specifier(F, TheHead:prolog) :<-
	"Get predicate specifier for loaded predicate"::
	get(F, head, Head),
	(   Head = _:_
	->  TheHead = Head
	;   get(F, text_buffer, TB),
	    xref_module(TB, M)
	->  TheHead = M:Head
	;   TheHead = _:Head
	),
	current_predicate(_, TheHead).

has_source(F) :->
	"Test if there is source available"::
	get(F, text_buffer, TB),
	get(F, head, Head),
	(   xref_defined(TB, Head, How),
	    xref_definition_line(How, _)
	;   xref_defined(TB, Head, imported(_From))
	;   get(prolog_predicate(Head), source, _)
	), !.

%	->edit
%
%	Find the predicate and invoke ->find_definition on the
%	@emacs_mode, which is the mode object of the current editor.

edit(F, Where:[{here,tab,window}]) :->
	"Open Prolog predicate [in new window]"::
	get(F, predicate, Pred),
	send(@emacs_mode, find_definition, Pred, Where).


%	->listing
%
%	List the predicate in an XPCE buffer

listing(F) :->
	"Generate a listing"::
	get(F, loaded_specifier, Spec),
	new(Tmp, emacs_buffer(@nil, string('*Listing for %N*', F))),
	pce_open(Tmp, write, Out),
	telling(Old), set_output(Out),
	ignore(listing(Spec)),
	tell(Old),
	close(Out),
	send(Tmp, modified, @off),
%	send(Tmp, mode, prolog),
	send(Tmp, open, tab).


has_listing(F) :->
	"Test if we can make a listing"::
	get(F, loaded_specifier, Spec),
	predicate_property(Spec, number_of_clauses(N)),
	N > 0.


documentation(F) :->
	"Invoke Prolog help-system"::
	send(F?predicate, help).


print_name(F, PN:string) :<-
	"Return [Module:]Name/Arity"::
	get(F, name, Name),
	get(F, arity, Arity),
	(   get(F, module, Module)
	->  new(PN, string('%s:%s/%d', Module, Name, Arity))
	;   new(PN, string('%s/%d', Name, Arity))
	).


identify(F) :->
	"Tell the user about the predicate"::
	get(F, text_buffer, TB),
	get(F, classification, Class),
	(   get(F, context, Context),
	    Context \== @nil
	->  Id =.. [Class, Context]
	;   Id = Class
	),
	identify_pred(Id, F, Report),
	send(TB, report, status, Report).

%%	identify_pred(+XrefClass, +Fragment, -Summary)
%
%	Generate an identifying description for the predicate.

identify_pred(Class, F, Summary) :-		% SWI-Prolog documented built-in
	get(F, predicate, Pred),		% & PlDoc summaries
	get(Pred, summary, Summary0), !,
	functor(Class, ClassName, _),
	(   (   Class == autoload,
		autoload_source(F, From)
	    ->	true
	    ;	Class = imported(From)
	    ),
	    file_name_on_path(From, Alias)
	->  term_to_atom(Alias, Atom),
	    new(Summary, string('%N: [%s from %s] %s', Pred, ClassName, Atom, Summary0))
	;   new(Summary, string('%N: [%s] %s', Pred, ClassName, Summary0))
	).
identify_pred(built_in, F, Summary) :-
	get(F, head, Head),
	predicate_property(system:Head, foreign), !,
	new(Summary, string('%N: Built-in foreign predicate', F)).
identify_pred(built_in, F, Summary) :-
	get(F, name, Name),
	sub_atom(Name, 0, _, _, $), !,
	new(Summary, string('%N: SWI-Prolog private built-in', F)).
identify_pred(autoload, F, Summary) :-	% Autoloaded predicates
	autoload_source(F, Source),
	file_name_on_path(Source, Alias),
	term_to_atom(Alias, Atom),
	new(Summary, string('%N: autoload from %s', F, Atom)).
identify_pred(local(Line), F, Summary) :-	% Local predicates
	new(Summary, string('%N: locally defined at line %d', F, Line)).
identify_pred(public(Line), F, Summary) :-	% Public predicates
	new(Summary, string('%N: declared public at line %d', F, Line)).
identify_pred(foreign(Line), F, Summary) :-	% Foreign predicates
	new(Summary, string('%N: foreign (C/C++) loaded at line %d', F, Line)).
identify_pred(constraint(Line), F, Summary) :-	% Local constraint
	new(Summary, string('%N: constraint defined at line %d', F, Line)).
identify_pred(imported(From), F, Summary) :-
	file_name_on_path(From, Alias),
	term_to_atom(Alias, Atom),
	new(Summary, string('%N: imported from %s', F, Atom)).
identify_pred(recursion, _, 'Recursive reference') :- !.
identify_pred(dynamic(_Line), F, Summary) :-
	get(F, loaded_specifier, Spec),
	(   predicate_property(Spec, number_of_clauses(N))
	->  new(Summary, string('%N: dynamic predicate with %d clauses',
				prolog_predicate(Spec),
				N))
	;   new(Summary, string('%N: dynamic predicate', F))
	).
identify_pred(global, _, 'Predicate defined in global module user') :- !.
identify_pred(Class, _, ClassName) :-
	term_to_atom(Class, ClassName).

autoload_source(F, Source) :-
	get(F, name, Name),
	get(F, arity, Arity),
	'$find_library'(_Module, Name, Arity, _LoadModule, Library), !,
	absolute_file_name(Library, Source,
			   [ file_type(prolog),
			     access(read),
			     file_errors(fail)
			   ]).

:- pce_end_class(emacs_goal_fragment).


		 /*******************************
		 *	  CLASS FRAGMENT	*
		 *******************************/

:- pce_begin_class(emacs_class_fragment, emacs_colour_fragment,
		   "Represent an XPCE class in PceEmacs Prolog mode").


variable(classification,   name, both, "XREF classification").
variable(referenced_class, name, both, "Name of referenced class").

:- pce_group(popup).

popup(_GF, Popup:popup) :<-
	"Return popup menu"::
	Popup = @prolog_mode_class_popup.

:- pce_global(@prolog_mode_class_popup,
	      make_prolog_mode_class_popup).

make_prolog_mode_class_popup(G) :-
	new(G, popup(class_actions)),
	Fragment = @arg1,
	send_list(G, append,
		  [ menu_item(edit_in_tab,
			      message(Fragment, edit, tab),
			      condition := message(Fragment, has_source)),
		    menu_item(edit_here,
			      message(Fragment, edit, here),
			      condition := message(Fragment, has_source)),
		    menu_item(edit_in_window,
			      message(Fragment, edit, window),
			      condition := message(Fragment, has_source)),
		    gap,
		    menu_item(documentation,
			      message(Fragment, documentation))
		  ]).

has_source(F) :->
	"See if we can find the source-location"::
	get(F, referenced_class, ClassName),
	get(F, text_buffer, TB),
	class_source(TB, ClassName, _Source).


class_source(TB, ClassName, line(Line)) :-
	xref_defined_class(TB, ClassName, local(Line, _, _)).
class_source(_, ClassName, Source) :-
	get(@pce, convert, ClassName, class, Class),
	get(Class, source, Source),
	Source \== @nil, !.
class_source(_, ClassName, Source) :-
	pce_library_class(ClassName, _, _Summary, Source).


edit(F, Where:[{here,tab,window}]) :->
	"Open XPCE class"::
	get(F, referenced_class, ClassName),
	get(F, text_buffer, TB),
	class_source(TB, ClassName, Source),
	(   Source = line(Line)
	->  get(F, text_buffer, TB),
	    get(TB, open, Where, Frame),
	    send(Frame?editor, goto_line, Line)
	;   ensure_loaded(library(edit)),
	    prolog_edit:locate(Source, _, Location),
	    memberchk(file(File), Location),
	    (	memberchk(line(Line), Location)
	    ->	true
	    ;	Line = @default
	    ),
	    send(@emacs, goto_source_location,
		 source_location(File, Line), Where)
	).

documentation(F) :->
	"Open XPCE manual"::
	get(F, referenced_class, ClassName),
	manpce(ClassName).

identify(F) :->
	"Provide identification"::
	get(F, text_buffer, TB),
	get(F, referenced_class, ClassName),
	classify_class(TB, ClassName, Classification),
	identify_class(F, ClassName, Classification).

identify_class(F, ClassName, built_in) :-
	class_summary(ClassName, Summary),
	send(F, report, status,
	     string('XPCE system class %s: %s',
		    ClassName, Summary)).
identify_class(F, ClassName, local(Line, _Super, Summary)) :-
	send(F, report, status,
	     string('XPCE local (line %d) class %s: %s',
		    Line, ClassName, Summary)), !.
identify_class(F, ClassName, library(File)) :-
	file_name_extension(Base, _, File),
	pce_library_class(ClassName, _, Summary, _),
	send(F, report, status,
	     string('XPCE library(%s) class %s: %s',
		    Base, ClassName, Summary)), !.
identify_class(F, ClassName, user(File)) :-
	class_summary(ClassName, Summary),
	send(F, report, status,
	     string('XPCE user class %s: %s (from %s)',
		    ClassName, Summary, File)).
identify_class(F, ClassName, user) :-
	class_summary(ClassName, Summary),
	send(F, report, status,
	     string('XPCE user class %s: %s',
		    ClassName, Summary)).
identify_class(F, ClassName, _Classification) :-
	send(F, report, status, 'Class %s doesn''t exist', ClassName).


class_summary(ClassName, Summary) :-
	get(@pce, convert, ClassName, class, Class),
	(   get(Class, summary, Summary)
	->  true
	;   Summary = '<no summary>'
	).


%%	classify_class(+TB, +ClassName, -Classification).

classify_class(TB, Name, Class) :-
	xref_defined_class(TB, Name, Class), !.
classify_class(_, Name, Class) :-
	classify_class(Name, Class).

:- pce_end_class(emacs_class_fragment).


		 /*******************************
		 *      GENERIC FRAGMENTS	*
		 *******************************/

:- pce_begin_class(emacs_prolog_fragment, emacs_colour_fragment,
		   "Colour fragment in Prolog mode").

variable(classification, name,	  both,	"XREF classification").
variable(context,	 prolog*, both,	"Classification argument").

:- pce_group(popup).

popup(F, Popup:popup) :<-
	get(F, context, Context),
	Context \== @nil,
	(   get(F, classification, file)
	;   get(F, classification, module)
	),
	Popup = @prolog_mode_file_popup.

:- pce_global(@prolog_mode_file_popup,
	      make_prolog_mode_file_popup).

make_prolog_mode_file_popup(G) :-
	new(G, popup(file_actions)),
	send_list(G, append,
		  [ menu_item(open_in_tab,
			      message(@emacs, open_file, @arg1?file, tab)),
		    menu_item(open_in_window,
			      message(@emacs, open_file, @arg1?file, window)),
		    menu_item(open_here,
			      message(@emacs, open_file, @arg1?file, here))
		  ]).

file(F, File:name) :<-
	"Return associated file"::
	get(F, context, Context),
	(   get(F, classification, file)
	->  File = Context
	;   get(F, classification, module)
	->  module_property(Context, file(File))
	).


identify(F) :->
	"Identify in status window"::
	get(F, classification, Class),
	(   get(F, context, Context),
	    Context \== @nil
	->  Term =.. [Class, Context]
	;   Term = Class
	),
	identify_fragment(Term, F, Summary), !,
	send(F, report, status, Summary).

identify_fragment(var,  _, 'Variable').
identify_fragment(file(Path), _, Summary) :-
	new(Summary, string('File %s', Path)).
identify_fragment(directory(Path), _, Summary) :-
	new(Summary, string('Directory %s', Path)).
identify_fragment(type_error(Type), _, Summary) :-
	identify_type_error(Type, Summary).
identify_fragment(syntax_error(Message), _, Summary) :-
	new(Summary, string('%s', Message)).
identify_fragment(module(Module), _, Summary) :-
	module_property(Module, file(Path)),
	new(Summary, string('Module %s loaded from %s', Module, Path)).
identify_fragment(method(send), _, 'XPCE send method').
identify_fragment(method(get), _, 'XPCE get method').
identify_fragment(head(unreferenced), _, 'Unreferenced predicate (from this file)').
identify_fragment(head(exported), _, 'Exported predicate').
identify_fragment(head(public), _, 'Public predicate').
identify_fragment(head(multifile), _, 'Multifile predicate').
identify_fragment(head(constraint), _, 'Constraint').
identify_fragment(prolog_data, _, 'Pass Prolog term unmodified').
identify_fragment(keyword(except), _, 'Import all except given').
identify_fragment(keyword(as), _, 'Import under a different name').
identify_fragment(Class, _, Summary) :-
	term_to_atom(Class, Summary).

identify_type_error(oneof(List), Summary) :- !,
	new(Summary, string('Type error: argument must be one of ')),
	add_one_ofs(List, Summary).
identify_type_error(Type, Summary) :-
	new(Summary, string('Type error: argument must be a %s', Type)).

add_one_ofs([], _).
add_one_ofs([H|T], Summary) :-
	send(Summary, append, H),
	(   T = [Last]
	->  send(Summary, append, ' or '),
	    send(Summary, append,  Last)
	;   send(Summary, append, ', '),
	    add_one_ofs(T, Summary)
	).

:- pce_end_class(emacs_prolog_fragment).

