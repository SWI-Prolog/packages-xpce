/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (C): 1985-2012, University of Amsterdam
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

:- module(emacs_prolog_mode, []).
:- use_module(library(pce)).
:- use_module(library(debug)).
:- use_module(library(make)).			% for reloading files
:- use_module(library(emacs_extend)).
:- use_module(library(lists)).
:- use_module(library(pce_meta)).
:- use_module(library(pce_prolog_xref)).
:- use_module(library(prolog_colour)).
:- use_module(library(prolog_predicate)).	% class prolog_predicate
:- use_module(library(prolog_source)).

resource(mode_pl_icon, image, image('32x32/doc_pl.xpm')).
resource(breakpoint,   image, image('16x16/stop.xpm')).

:- emacs_begin_mode(prolog, language,
		    "Mode for editing XPCE/Prolog sources",
					% BINDINGS
	[ insert_if_then_else	       = key('(') + key(';') + key('>'),
	  insert_quote		       = key('"'),
	  insert_percent	       = key('%'),
	  insert_exclamation_mark      = key('!'),
	  insert_quasi_quote	       = key('|'),

	  newline_and_indent	       = key('RET'),

					% delete some things
	  manual_entry		       = -button(help),
	  find_tag		       = -button(browse),
	  compile		       = -button(compile),

					% extend the menus
	  prolog_manual		       = button(help),
	  (spy)			       = button(prolog),
	  trace			       = button(prolog),
	  break_at		       = key('\\C-cb') + button(prolog),
	  delete_breakpoint	       = button(prolog),
	  -			       = button(prolog),
	  edit_breakpoints	       = button(prolog),
	  edit_exceptions	       = button(prolog),
	  view_threads		       = button(prolog),
	  view_debug_messages	       = button(prolog),
	  -			       = button(prolog),
	  check_clause		       = key('\\C-c\\C-s') + button(prolog),
	  insert_full_stop	       = key(.),
	  find_definition	       = key('\\e.') + button(browse),
	  -			       = button(prolog),
	  make			       = key('\\C-c\\C-m') + button(compile),
	  compile_buffer	       = key('\\C-c\\C-b') + button(compile),
	  consult_selection	       = button(compile) + button(compile),
	  source_file		       = button(browse,
						@prolog?source_file_chain),
	  loaded_from		       = button(browse,
						@emacs_mode?loaded_from_chain),

	  forward_clause	       = key('\\ee'),
	  backward_clause	       = key('\\ea'),
	  backward_predicate	       = key('\\e['),
	  forward_predicate	       = key('\\e]'),

	  editpce		       = key('\\C-ce') + button(pce),
	  tracepce		       = key('\\C-ct') + button(pce),
	  spypce		       = button(pce),
	  -			       = button(pce),
	  what_class		       = key('\\C-cw') + button(pce),
	  -			       = button(pce),
	  pce_insert_require_directive = key('\\C-c\\C-r') + button(pce),
	  pce_check_require_directives = button(pce),
	  -			       = button(pce),
	  pce_define_class	       = button(pce),
	  -			       = button(browse),
	  prolog_navigator	       = button(browse) + key('\\C-c\\C-n'),

	  colourise_or_recenter	       = key('\\C-l'),
	  colourise_buffer	       = key(key_top_5)
	],
					% SYNTAX TABLE
	[ ($)  = symbol,
	  (@)  = symbol,
	  '%'  = comment_start,
	  '\\n' + comment_end,
	  '/'  + comment_start('*'),
	  '*'  + comment_end('/'),
	  quasi_quotation('||', '|}'),

	  paragraph_end([ '\\s*$',		% empty line
			  '/\\*',		% comment start
			  '[^\n]*\\*/\\s*$',	% comment end
			  '%',			% line comment
			  '\t',			% indented line
			  '[^\n]*:<?->?\\s*$'	% clause head
			])
	]).

class_variable(varmark_style, style*,
	       style(background := honeydew,
		     underline := @on)).
class_variable(show_syntax_errors, {never,typing,pause},
	       typing).
class_variable(auto_colourise_size_limit, int, 100000,
	       "Auto-colourise if buffer is smaller then this").

variable(varmark_style,    style*,       get, "How to mark variables").
variable(has_var_marks,    bool := @off, get, "Optimise a bit").
variable(var_marked_caret, int*,	 get, "Last caret at ->mark_variable").
variable(var_marked_gen,   int*,	 get, "Last generation").
variable(var_mark_enabled, bool := @on,  get, "Do varmark stuff").
variable(show_syntax_errors,
			   {never,typing,pause},
					 get, "When highlight syntax errors").
variable(warnings,	   int := 0,	 get, "Number of warnings").
variable(errors,	   int := 0,	 get, "Number of errors").
variable(body_indentation, int,		 get, "Indentation for body-goals").
variable(cond_indentation, int,		 get, "Indent step for conditional").
variable(quasiquotation_syntax,
			   name*,	 both, "Default quasiquotation syntax").

class_variable(quasiquotation_syntax, name*, @nil).
class_variable(body_indentation,      int,   8).
class_variable(cond_indentation,      int,   4).
class_variable(indent_tabs,           bool,  @on,
	       "Use tabs for indentation").


icon(_, I:image) :<-
	"Return icon for mode"::
	catch(new(I, image(resource(mode_pl_icon))), _, fail).

setup_mode(M) :->
	"Attach styles for errors, warnings, etc."::
	send_super(M, setup_mode),
	send(M,	style, breakpoint, style(icon := resource(breakpoint))),
	send(M,	style, error,	   style(background := red)),
	send(M,	style, warning,	   style(background := orange)),
	send(M,	style, info,	   style(background := grey80)),
	(   get(M, varmark_style, Style),
	    Style \== @nil
	->  send(M, style, varmark, Style)
	;   true
	),
	send(M, setup_styles),
	send(M, setup_margin).


:- send(@class, attribute, outline_regex_list,
	chain(regex('(^\\w+.*:<?->?)([^.]+\\.(\\s*\n)*)\\s'))).

:- public source_file_chain/1.		% Supports source_file in button(browse)

source_file_chain(Ch) :-
	new(Ch, chain),
	forall(user_source_file(X), send(Ch, append, X)),
	send(Ch, sort).

user_source_file(F) :-
	source_file(F),
	\+ (lib_dir(D), atom_concat(D, _, F)).

ignore_paths_from(library).
ignore_paths_from(pce_boot).

lib_dir(D) :-
	ignore_paths_from(Category),
	user:file_search_path(Category, X),
	expand_path(X, D0),
	absolute_file_name(D0, D).	% canonicalise

expand_path(X, X) :-
	atomic(X), !.
expand_path(Term, D) :-
	Term =.. [New, Sub],
	user:file_search_path(New, D0),
	expand_path(D0, D1),
	atomic_list_concat([D1, /, Sub], D).


:- pce_group(indent).

:- pce_global(@prolog_neck_regex,
	      new(regex(':-|:->|:<-|-->'))).
:- pce_global(@prolog_full_stop,
	      new(regex('[^-#$&*+./:<=>?@\\\\^`~]\\.($|\\s)'))).
:- pce_global(@prolog_decl_regex,
	      new(regex('^:-\\s*[a-z_]+'))).

indent_line(E) :->
	"Indent current line (Prolog indentation)"::
	send(E, beginning_of_text_on_line),
	get(E, caret, Caret),
	get(E, beginning_of_clause, Caret, Base),
	(   send(E, indent_comment_line),			W = comment
	;   send(E, indent_close_bracket_line, ')}]', Base),	W = close_bracket
	;   send(E, indent_if_then_else),			W = if_then_else
	;   send(E, indent_expression_line, ')}]', Base),	W = expression
	;   send(E, indent_clause_line),			W = clause
	;   get(E, body_indentation, Indent),			W = body,
	    send(E, align_line, Indent)
	),
	atom(W),				% avoid semantic singleton
	debug(emacs(indent), 'Indented line as ~w', [W]),
	send(E, fixup_if_then_else).


% ->fixup_if_then_else fixes changed alignment due to tabs
% that expand to a different number of spaces.  Now only
% does this for the first one.  Probably should do this
% for the entire line.

fixup_if_then_else(E) :->
	"Fixup [(;->]\t"::
	get(E, caret, C),
	get(E, column, C, Indent),
	(   get(E, looking_at, '(\\(|;|->)\t', Len)
	->  get(E, caret, Caret),
	    get(E, column, Caret+Len, Col),
	    get(E, cond_indentation, Extra),
	    (   Col =:= Indent+Extra
	    ->  true
	    ;   send(E, align, Indent+Extra, Caret+Len)
	    )
	;   true
	).

beginning_of_clause(E, Start:int, BOP:int) :<-
	"Find start of the a clause that contains Start"::
	new(Here, number(Start)),
	get(E, text_buffer, TB),
	repeat,
	    (	(   send(Here, less_equal, 0)
		;   \+ send(@prolog_full_stop, search, TB, Here, 0)
		)
	    ->	!,
		(   get(TB, character, 0, 0'#)	% Deal with #! first-line
		->  get(TB, scan, 0, line, 1, start, BOP0)
		;   BOP0 = 0
		),
		get(TB, skip_comment, BOP0, BOP)
	    ;   get(@prolog_full_stop, register_start, SReg),
	        get(@prolog_full_stop, register_end, EReg),
		send(Here, value, SReg),
	        get(TB, scan_syntax, 0, SReg, tuple(code,_)),
		get(TB, skip_comment, EReg, BOP),
		BOP =< Start,
		get(TB, scan_syntax, SReg, BOP, tuple(code,_))
	    ).
beginning_of_clause(E) :->
	"Goto start of clause"::
	get(E, caret, Caret),
	get(E, beginning_of_clause, Caret, BOC),
	send(E, caret, BOC).

beginning_of_if_then_else(E, New:[bool], OpenPos:int) :<-
	"Beginning of if-then-else construct"::
	get(E, caret, Caret),
	get(E, text_buffer, TB),
	pce_catch_error(mismatched_bracket,
			get(TB, matching_bracket, Caret, ')', OpenPos)),
	get(TB, character, OpenPos-1, Before),
	\+ send(E?syntax, has_syntax, Before, word),
	Before \== 0'?,				% '?(' for xpce
	(   New \== @on
	->  get(TB, scan, OpenPos, line, 0, end, EOL), % see <-argument_indent
	    get(TB, skip_comment, OpenPos+1, EOL, P1),
	    P1 \== EOL
	;   true
	),
	get(E, beginning_of_clause, Caret, BegOfPred),
	BegOfPred < OpenPos,
	get(TB, scan_syntax, BegOfPred, OpenPos, tuple(code,_)).


indent_if_then_else(E) :->
	"Indent subclause in an (if->then;else)"::
	get(E, beginning_of_if_then_else, OpenPos),
	get(E, caret, Caret),
	get(E, text_buffer, TB),
	get(E, skip_comment, Caret-1, OpenPos, EndOfPreviousTerm),
	(   send(regex(','), match, TB, EndOfPreviousTerm)
	->  get(TB, scan, Caret, line, -1, start, StartOfPrevLine),
	    get(regex('\\s*(->|;)\\s*'), match, TB, StartOfPrevLine, L),
	    get(E, column, L+StartOfPrevLine, PrevExprCol),
	    send(E, align_line, PrevExprCol)
	;   get(E, column, OpenPos, OpenCol),
	    send(E, align_line, OpenCol)
	).


indent_clause_line(E) :->
	"Indent current line according to clause"::
	get(E, caret, Caret),
	get(E, text_buffer, TB),
	get(E, skip_comment, Caret-1, 0, Glue),
	debug(emacs(indent), 'Glue at ~d', [Glue]),
	get(E, body_indentation, Indent),
	(   send(regex(\.), match, TB, Glue)		% new clause
	->  send(E, align_line, 0)
	;   send(regex(','), match, TB, Glue)		% Next subclause
	->  get(E, alignment_of_previous_line, N),
	    (	N == 0					% head :- !,
	    ->	send(E, align_line, Indent)
	    ;	send(E, align_line, N)
	    )
	;   send(@prolog_neck_regex, match, TB, Glue+1, 0) % First subclause
	->  send(E, align_line, Indent)			% (seach backward)
	;   send(@prolog_decl_regex, match, TB, Glue+1, 0)
	->  send(E, align_line, Indent)
	;   send(E, align_with_previous_line)
	).


insert_if_then_else(E, Times:[int], Char:char) :->
	"Indent after typing (, > or ;"::
	send(E, insert_self, Times, Char),
	get(E, caret, Caret),
	get(E, text_buffer, TB),
	get(TB, scan, Caret, line, 0, start, SOL),
	(   get(regex('\\s*(\\(|->|;)'), match, TB, SOL, L),
	    Caret =:= SOL + L,
	    get(E, beginning_of_if_then_else, @on, OpenPos)
	->  get(E, column, OpenPos, Col),
	    get(E, cond_indentation, Indent),
	    send(E, align, Col+Indent)
	;   true
	).


indent_clause(E) :->
	"Indent current clause"::
	get(E, text_buffer, TB),
	get(E, beginning_of_clause, E?caret, Start),
	send(E, caret, Start),
	between(0, 1000, _),		% avoid loops on errors
	    send(E, indent_line),
	    get(E, caret, Caret),
	    (	get(regex('[^\n]*[^[:punct:]]\\.[[:space:]]'), match,
		    TB, Caret, Size0),
		Size is Size0 - 1,	% subtract matched space
		End is Caret + Size,
		get(TB, scan_syntax, Start, End, tuple(code,_))
	    ->	!
	    ;	get(TB, size, Caret)
	    ->	!,
		Size = 0
	    ;   send(E, next_line),
		fail
	    ),
	send(E, forward_char, Size),
	send(E, electric_caret, Start).


fill_paragraph(M, Justify:[int]) :->
	"Fill paragraph or indent clause"::
	get(M, caret, Caret),
	(   get(M, beginning_of_clause, Caret, BOC),
	    get(M, forward_clause, BOC, EOC),
	    between(BOC, EOC, Caret)
	->  send(M, indent_clause)
	;   send_super(M, fill_paragraph, Justify)
	).


fill_comment(M,
	     Start:from=int, End:to=int,
	     Re:leading=regex, Justify:justify=[bool|int],
	     LeadCont:lead_continuation=[char_array]) :->
	"Fill/justify comments"::
	send(M, slot, var_mark_enabled, @off),
	call_cleanup(send_super(M, fill_comment, Start, End, Re, Justify,
				LeadCont),
		     send(M, slot, var_mark_enabled, @on)),
	(   send(M, has_send_method, colourise_comments)
	->  send(M, colourise_comments, Start, End)
	;   true
	).


insert_quote(E, Times:[int], Char:char) :->
	"Complete quote"::
	send(E, insert_self, Times, Char),
	get(E, caret, Here),
	(   send(E, looking_at, ':->\n\\s*"', Here, 0)
	->  send(E, insert, '"::'),
	    send(E, caret, Here)
	;   true
	).


insert_percent(E, Times:[int], Char:char) :->
	"Deal with %% comments"::
	send(E, insert_self, Times, Char),
	get(E, caret, Here),
	(   send(E, looking_at, '\n%%', Here, 0),
	    send(E, looking_at, '\\s*$', Here)
	->  send(E, insert, '\t')
	;   true
	).


insert_exclamation_mark(E, Times:[int], Char:char) :->
	"Deal with %! comments"::
	send(E, insert_self, Times, Char),
	get(E, caret, Here),
	(   send(E, looking_at, '\n%!', Here, 0),
	    send(E, looking_at, '\\s*$', Here)
	->  send(E, insert, '\t')
	;   true
	).


indent_comment_line(E) :->
	"Deal with %% comments"::
	send_super(E, indent_comment_line),
	get(E, caret, Here),
	(   send(E, looking_at, '\n%[%!][^\n]*\n%\\s*\n%', Here, 0)
	->  send(E, insert, '\t')
	;   true
	).


insert_quasi_quote(E) :->
	"Deal with {|Syntax||Quoted|}>"::
	send(E, insert, '|'),
	get(E, caret, Here),
	(   send(E, looking_at, '{\\|', Here, 0)
	->  (   get(E, quasiquotation_syntax, Syntax),
	        Syntax \== @nil
	    ->	send(E, insert, Syntax),
		send(E, insert, '|||}'),
		send(E, backward_char, 2)
	    ;	send(E, insert, '|||}'),
		send(E, backward_char, 4)
	    )
	;   true
	).


		 /*******************************
		 *          COMPILATION		*
		 *******************************/

make(E) :->				% SWI-Prolog specific
	"Run `make/0' in the Prolog window"::
	send(E, close_warning_window),
	send(@emacs, save_some_buffers),
	make,
	send(E, report, status, 'Make done').

compile_buffer(E) :->
	"Save current buffer and (re)consult its file"::
	send(E, close_warning_window),
	get(E?text_buffer, file, File),
	(   send(File, instance_of, file)
	->  send(E, save_if_modified),
	    get(File, absolute_path, Path0),
	    absolute_file_name(Path0, Path),
	    master_load_file(Path, [], ToLoad),
	    print_message(silent, emacs(consult(user:ToLoad))),
	    make:reload_file(ToLoad),
	    print_message(silent, emacs(consulted(user:ToLoad))),
	    send(E, report, status, '%s compiled', ToLoad)
	;   send(E, report, error,
		 'Buffer is not connected to a file')
	).

%%	master_load_file(+File, +Seen, -MasterFile) is det.
%
%	If file is included into another  file, find the outermost file.
%	This is the file that needs to  be reloaded instead of reloading
%	File.

master_load_file(File0, Seen, File) :-
	source_file_property(File0, included_in(File1, _Line)),
	\+ memberchk(File1, Seen), !,
	master_load_file(File1, [File0|Seen], File).
master_load_file(File, _, File).


close_warning_window(_E) :->
	"Destroy compilation error window"::
	(   object(@prolog_warnings)
	->  send(@prolog_warnings, destroy)
	;   true
	).


		/********************************
		*       FINDING PREDICATES	*
		********************************/

default(M, For:type, Default:unchecked) :<-
	"Provide default for prompter"::
	(   send(For, includes, prolog_predicate)
	->  get(M, caret, Caret),
	    get(M, name_and_arity, Caret, tuple(Name, Arity)),
	    atomic_list_concat([Name, /, Arity], Default)
	;   get_super(M, default, For, Default)
	).


find_definition(M, For:prolog_predicate, Where:[{here,tab,window}]) :->
	"Find definition of predicate [in new window]"::
	get(M, text_buffer, TB),
	get(For, head, @off, Head),
	(   (   xref_defined(TB, Head, local(Location))		% local
	    ;	xref_defined(TB, Head, constraint(Location))
	    ;   xref_defined(TB, Head, foreign(Location))
	    )
	->  get(TB, open, Where, Frame),
	    get(Frame, editor, Editor),
	    (	integer(Location)
	    ->	send(Editor, goto_line, Location, title := For?print_name)
	    ;	Location = (File:Line)
	    ->  send(@emacs, goto_source_location,
		     source_location(File, Line), tab)
	    )
	;   xref_defined(TB, Head, imported(File))	% imported
	->  new(B, emacs_buffer(File)),
	    get(B, open, Where, EmacsFrame),
	    get(EmacsFrame, mode, Mode),
	    send(Mode, instance_of, emacs_prolog_mode),
	    send(Mode, find_local_definition, For)
	;   get(For, source, SourceLocation)		% From Prolog DB
	->  send(@emacs, goto_source_location,
		 SourceLocation, Where, For?print_name)
	;   send(For, has_property, foreign)
	->  send(M, report, warning,
		 'Predicate is defined in a foreign language')
	;   send(M, report, warning,
		 'Cannot find source')
	).


find_local_definition(M, For:prolog_predicate) :->
	"Find Prolog predicate in local buffer"::
	get(M, text_buffer, TB),
	get(For, head, @off, Head),
	(   (   xref_defined(TB, Head, local(Location))
	    ->  true
	    ;   send(M, xref_buffer),
		get(M, xref_source_id, SourceID),
		xref_defined(SourceID, Head, local(Location))
	    )
	->  (   integer(Location)
	    ->	send(M, goto_line, Location, title := For?print_name)
	    ;	Location = (File:Line)
	    ->	send(@emacs, goto_source_location,
		     source_location(File, Line), tab)
	    )
	;   send(M, report, warning, 'Cannot find %N', For)
	).


		 /*******************************
		 *	   LOAD CONTEXT		*
		 *******************************/

loaded_from(_M, LoadedFrom:source_location) :->
	"Jump to position I'm loaded from"::
	send(@emacs, goto_source_location, LoadedFrom, tab).

loaded_from_chain(M, LoadedFrom:chain) :<-
	"Chain with files and locations I'm loaded from"::
	get(M, file, File), File \== @nil,
	get(File, absolute_path, Path0),
	absolute_file_name(Path0, Path),
	findall(SLoc, loaded_from(Path, SLoc), Locations),
	Term =.. [chain|Locations],
	new(LoadedFrom, Term).

loaded_from(Path, source_location(File, Line)) :-
	(   source_file_property(Path, load_context(_Module, File:Line, _Options))
	;   source_file_property(Path, included_in(File, Line))
	).


		 /*******************************
		 *	       INFO		*
		 *******************************/

file_module(M, Module:name) :<-
	"Module used for the file"::
	get(M, text_buffer, TB),
	(   xref_module(TB, Module)
	->  true
	;   get(TB, file, File), File \== @nil,
	    get(File, absolute_path, Path0),
	    absolute_file_name(Path0, Path),
	    module_context(Path, [], Module)
	).

properties(M, V:view) :<-
	get_super(M, properties, V),
	(   get(M, file_module, Module)
	->  send(V, appendf, 'Prolog module:\t%s\n', Module)
	;   true
	).


		 /*******************************
		 *	    PCE CLASSES		*
		 *******************************/

class_regex(':-\\s*pce_begin_class\\((\\w+)',
	    ':-\\s*pce_end_class\\s*.',
	    A-[A]).
class_regex(':-\\s*emacs_begin_mode\\((\\w+)',
	    ':-\\s*emacs_end_mode\\s*.',
	    A-[emacs_, A, '_mode']).

what_class(E, ClassName:name) :<-
	"Find current XPCE class"::
	get(E, caret, Caret),
	get(E, text_buffer, TB),
	class_regex(Begin, End, Raw-Parts),
	new(BG, regex(Begin)),
	get(BG, search, TB, Caret, 0, BeginClass),
	(   get(regex(End), search, TB, Caret, 0, EndClass)
	->  EndClass < BeginClass
	;   true
	), !,
	get(BG, register_value, TB, 1, name, Raw),
	atomic_list_concat(Parts, ClassName).

what_class(E) :->
	"Display current class"::
	(   get(E, what_class, ClassName)
	->  send(E, report, inform, 'Caret is in XPCE class "%s"', ClassName)
	;   send(E, report, inform,
		 'Not between :- pce_begin_class and :- pce_end_class')
	).


source_file(E, F:file) :->
	"Switch to named source_file"::
	send(E, find_file, F).


prolog_module(M, Module:name) :<-
	"Return module defined in this class"::
	get(M, prolog_term, 0, ModuleTerm),
	ModuleTerm = (:- module(Module, _)).


what_module(M) :->
	"Report the Prolog module defined in this file"::
	(   get(M, prolog_module, Module)
	->  send(M, report, status,
		 'File defines Prolog module "%s"', Module)
	;   send(M, report, status,
		 'Not a module file')
	).

pce_define_class(M, Name:name,
		 SuperClass:super='class|name', Comment:comment=string) :->
	"Insert XPCE class definition"::
	(   atom(SuperClass)
	->  Super = SuperClass
	;   get(SuperClass, name, Super)
	),
	format(string(QName), '~q', Name),
	format(string(QSuper), '~q', Super),
	send(Comment, strip),
	get(Comment, value, C),
	(   C == ''
	->  send(M, format,
		 ':- pce_begin_class(%s, %s).\n\n\c
		  :- pce_end_class(%s).\n',
		 QName, QSuper, QName)
	;   atom_codes(C, Codes),
	    string_codes(S, Codes),
	    format(string(QComment), '~q', S),
	    send(M, format,
		 ':- pce_begin_class(%s, %s, %s).\n\n\c
		  :- pce_end_class(%s).\n',
		 QName, QSuper, QComment, QName)
	),
	send(M, previous_line, 2).


		 /*******************************
		 *	   BROWSE STUFF		*
		 *******************************/

prolog_navigator(M) :->
	"Open source-file browser"::
	(   get(M, file, File), File \== @nil
	->  get(File, absolute_path, Path),
	    get(M, line_number, Line),
	    prolog_ide(open_navigator(source_location(Path, Line)))
	;   prolog_ide(open_navigator)
	).


edit_breakpoints(_M) :->
	"Open Prolog debug settings window"::
	prolog_ide(open_debug_status).


edit_exceptions(_M) :->
	"Open Prolog Exception editor"::
	prolog_ide(open_exceptions(@on)).


view_threads(_M) :->
	"View running threads"::
	prolog_ide(thread_monitor).


view_debug_messages(_M) :->
	"View debug/3 messages"::
	prolog_ide(debug_monitor).


		 /*******************************
		 *	   COMPILATION		*
		 *******************************/

consult_region(M, From:[int], To:[int]) :->
	"Consult region between indices"::
	default(From, M?mark, F),
	default(To, M?caret, T),
	get(T-F, value, S),
	(   S >= 0
	->  Start = F, Size = S
	;   Start = T, Size is -S
	),
	new(File, file),		% temporary file
	send(File, open, write),
	send(File, append, ?(M, contents, Start, Size)),
	send(File, newline),		% make sure it ends with a newline
	send(File, close),
	get(File, name, TmpNam),
	consult(user:TmpNam),
	send(M, report, status, 'Region consulted'),
	send(File, remove).


consult_selection(M) :->
	"Consult selected text"::
	get(M, selection, point(From, To)),
	send(M, consult_region, From, To).


		 /*******************************
		 *	       PCE		*
		 *******************************/

pce_insert_require_directive(M) :->
	"Insert :-require/1 directive"::
	send(M, save_if_modified),
	get(M, file, File),
	get(File, name, Name),
	auto_call(pce_require(Name, Directive, Message)),
	send(M, insert, Directive),
	(   Message \== ''
	->  send(M, report, status, Message)
	;   true
	).


pce_check_require_directives(M, Dir:directory) :->
	"Mark :- require's that are out-of-date"::
	get(Dir, files, '.*\\.pl$', PlFiles),
	send(PlFiles, for_some,
	     message(M, pce_check_require, ?(Dir, file, @arg1))),
	get(Dir, directories, SubDirs),
	send(SubDirs, for_some,
	     message(M, pce_check_require_directives,
		     ?(Dir, directory, @arg1))).


no_check(library(pce)).
no_check(library('xref/common')).
no_check(library('xref/mkcommon')).
no_check(library('xref/quintus')).
no_check(library('xref/sicstus')).

do_not_check(File) :-
	no_check(Spec),
	absolute_file_name(Spec, [access(read), extensions([pl])], Expanded),
	send(File, same, Expanded).

pce_check_require(M, File:file) :->
	"Open of there is no :- require"::
	(   do_not_check(File)
	->  true
	;   get(File, name, Name),
	    send(M, report, status, 'Checking %s', Name),
	    send(M, synchronise),
	    auto_call(pce_require(Name, _Directive, Message)),
	    (   send(Message, sub, 'up-to-date')
	    ->  true
	    ;   new(B, emacs_buffer(File)),
		(   get(regex('^:-\\s*require\\('), search, B, Index)
		->  true
		;   Index = 0
		),
		send(@emacs_mark_list, append_hit, B, Index)
	    ),
	    send(M, report, done)
	).

spy(M) :->
	"Set spy-point on implementation"::
	get(M, prolog_term, Term),
	(   do_spy(Term, M, Feedback)
	->  term_to_atom(Feedback, Atom),
	    send(M, report, status,
		 'Placed spy-point on "%s"', Atom)
	;   send(M, report, warning,
		 'Can''t find anything to spy from caret location')
	).

do_spy((Head :-> _Body), M, (Class->Name)) :- !,
	get(M, what_class, Class),
	functor(Head, Name, _Arity),
	spypce((Class->Name)).
do_spy((Head :<- _Body), M, <-(Class, Name)) :- !,
	get(M, what_class, Class),
	functor(Head, Name, _Arity),
	spypce(<-(Class, Name)).
do_spy(variable(Name, _Type, _Access), M, (Class-Name)) :-
	get(M, what_class, Class),
	spypce((Class-Name)).
do_spy(variable(Name, _Type, _Access, _Doc), M, (Class-Name)) :-
	get(M, what_class, Class),
	spypce((Class-Name)).
do_spy((Head :- _Body), M, Spec) :-
	prolog_debug_spec(M, Head, Spec),
	user:spy(Spec).
do_spy((Head --> _Body), M, Spec) :-
	dcg_debug_spec(M, Head, Spec),
	user:spy(Spec).
do_spy(Head, M, Spec) :-
	prolog_debug_spec(M, Head, Spec),
	user:spy(Spec).

trace(M) :->
	"Set trace-point on implementation"::
	get(M, prolog_term, Term),
	(   do_trace(Term, M, Feedback)
	->  term_to_atom(Feedback, Atom),
	    send(M, report, status,
		 'Placed trace-point on "%s"', Atom)
	;   send(M, report, warning,
		 'Can''t find anything to trace from caret location')
	).

do_trace((Head :-> _Body), M, (Class->Name)) :- !,
	get(M, what_class, Class),
	functor(Head, Name, _Arity),
	tracepce((Class->Name)).
do_trace((Head :<- _Body), M, <-(Class, Name)) :- !,
	get(M, what_class, Class),
	functor(Head, Name, _Arity),
	tracepce(<-(Class, Name)).
do_trace(variable(Name, _Type, _Access), M, (Class-Name)) :-
	get(M, what_class, Class),
	tracepce((Class-Name)).
do_trace(variable(Name, _Type, _Access, _Doc), M, (Class-Name)) :-
	get(M, what_class, Class),
	tracepce((Class-Name)).
do_trace((Head :- _Body), M, Spec) :-
	prolog_debug_spec(M, Head, Spec),
	user:trace(Spec).
do_trace(Head, M, Spec) :-
	prolog_debug_spec(M, Head, Spec),
	user:trace(Spec).

prolog_debug_spec(M, Head, Spec) :-
	catch(functor(Head, Name, Arity), _, fail),
	(   get(M, prolog_module, Module)
	->  Spec = (Module:Name/Arity)
	;   Spec = Name/Arity
	).

dcg_debug_spec(M, Head, Spec) :-
	catch(functor(Head, Name, Arity), _, fail),
	(   get(M, prolog_module, Module)
	->  Spec = (Module:Name//Arity)
	;   Spec = Name//Arity
	).

		 /*******************************
		 *	       DROP		*
		 *******************************/

preview_drop(M, Obj:object*) :->
	"Preview the upcomming drop action"::
	(   Obj == @nil
	->  send(M, report, status, '')
	;   get(Obj, get_method, prolog_source, tuple(_, Method))
	->  (	get(Method, summary, Summary), Summary \== @nil
	    ->	send(M, report, status, 'Drop to include %s', Summary)
	    ;   send(M, report, status,
		     'Please drop to include source at caret')
	    )
	;   send(M, send_super, preview_drop, Obj)
	).

drop(M, Obj:object) :->
	"Import source-code from object"::
	(   send(Obj, has_get_method, prolog_source)
	->  send(M, insert, Obj?prolog_source),
	    send(M, mark_undo),
	    send(M, report, status, 'Source included')
	;   send(M, send_super, drop, Obj)
	).

		 /*******************************
		 *	  SYNTAX CHECKING	*
		 *******************************/

error_at_location(M, Caret:int) :->
	"Goto error at location"::
	send(M, caret, Caret),
	send(M, check_clause).


symbol_chars('-#$&*+./:<=>?@\\^~').

term_expansion(symbol_char(_), Clauses) :-
	symbol_chars(String),
	atom_codes(String, Codes),
	findall(symbol_char(C), member(C,Codes), Clauses).

symbol_char(_Code).


insert_full_stop(M, Arg:[int]) :->
	"Check clause after typing '.'"::
	send(M, insert_self, Arg, 0'.),
	get(M, text_buffer, TB),
	get(TB, size, Len),
	get(M, caret, Here),
	(   Here == Len
        ->  send(M, open_line)
	;   true
	),
	(   Arg == @default,
	    get(M, caret, Caret),
	    get(M, character, Caret-2, Prev),
	    \+ symbol_char(Prev),
	    get(M, scan_syntax, 0, Caret, tuple(code,_))
	->  get(M, check_clause, repair := @off, _End)
	;   true
	).

check_clause(M, From:from=[int], Repair:repair=[bool], End:int) :<-
	"Check clause, returning the end of it"::
        (   From == @default
	->  get(M, caret, C),
	    get(M, beginning_of_clause, C, Start),
	    ignore(send(M, electric_caret, Start))
	;   Start = From
	),
	get(M, colourise_clause, Start, TermPos),
	(   TermPos = error_position(_StartClause, _EndClause, ErrorPos)
	->  (	Repair \== @off
	    ->	send(M, caret, ErrorPos)
	    ;	true
	    )
	;   arg(2, TermPos, End0),
	    get(M, text_buffer, TB),
	    get(TB, find, End0, '.', 1, end, End),
	    (	Repair \== @off
	    ->	send(M, replace_singletons, Start, End)
	    ;	true
	    )
	).


%%	read_term_from_stream(+TextBuffer, +Stream, +Start,
%%			      -Start, -Term, -Error, -Singletons, -TermPos,
%%			      -Comments) is det.
%
%	@param	Comments is list of comments or (-) if the parser cannot
%		produce information about comments.

read_term_from_stream(TB, Fd, Start,
		      Term, Error,
		      Singletons, TermPos, Comments) :-
	findall(Op, xref_op(TB, Op), Ops),
	read_source_term_at_location(
	    Fd, Term,
	    [ offset(Start),
	      module(emacs_prolog_mode),
	      operators(Ops),
	      error(Error),
	      singletons(Singletons),
	      subterm_positions(TermPos),
	      comments(Comments)
	    ]).


check_clause(M, From:from=[int], Repair:repair=[bool]) :->
	"Check syntax of clause"::
	get(M, check_clause, From, Repair, _).


replace_singletons(M, Start:int, End:int) :->
	"Replace singletion variables in range"::
	new(Pt, point(Start, End)),
	get(M, find_all_fragments,
	    and(message(@arg1, overlap, Pt),
		@arg1?style == singleton),
	    Frags),
	(   send(Frags, empty)
	->  true
	;   send(M, attribute, singletons, Frags),
	    get(M, caret, C),
	    send(M, internal_mark, C),
	    send(M, focus_function, '_replace_singletons'),
	    prepare_replace_singletons(M)
	).

'_replace_singletons'(M, Id:event_id) :->
	get(M, attribute, singletons, Frags),
	get(Frags, delete_head, Frag),
	(   (   Id == 0'y
	    ->  send(Frag, insert, 0, '_'),
	        send(Frag, free)
	    ;   Id == 0'_
	    ->  send(Frag, string, '_'),
		send(Frag, free)
	    ;   Id == 0'n
	    ->  true
	    )
	->  (   send(Frags, empty)
	    ->  send(M, caret, M?internal_mark),
		cancel_replace_singletons(M)
	    ;   prepare_replace_singletons(M)
	    )
	;   cancel_replace_singletons(M),
	    Id == 27			% ESC: succeed
	).

cancel_replace_singletons(M) :-
	send(M, focus_function, @nil),
	send(M, mark_status, inactive),
	send(M, delete_attribute, singletons),
	send(M, report, status, '').

prepare_replace_singletons(M) :-
	get(M, attribute, singletons, Frags),
	get(Frags, head, F0),
	get(F0, start, S),
	get(F0, end, E),
	send(M, selection, S, E, highlight),
	send(M, report, status,
	     'Replace singleton? (''y'' --> _Name, ''_'' --> _, ''n'')').


		 /*******************************
		 *        TERM-READING		*
		 *******************************/

%	<-prolog_term
%
%	Read a Prolog term from the buffer. If From is specified, this
%	is taken to be the start of the clause rather than using
%	<-beginning_of_clause from <-caret. If Silent is @off, error
%	messages are not printed. If a variable is passed into TermPos,
%	it is unified with the subterm-position specification a
%	specified in read_term/3.

prolog_term(M, From:[int], Silent:[bool], TermPos:[prolog], Clause:prolog) :<-
	  "Read clause start at <From> or around caret"::
	  (   From == @default
	  ->  get(M, caret, Caret),
	      get(M, beginning_of_clause, Caret, Start)
	  ;   Start = From
	  ),
	  get(M, text_buffer, TB),
	  setup_call_cleanup(
	      pce_open(TB, read, Fd),
	      ( set_stream_file(TB, Fd),
		read_term_from_stream(TB, Fd, Start, Clause, Error, _S, P, _C)
	      ),
	      close(Fd)),
	  ignore(P = TermPos),
	  (   var(Error)
	  ->  true
	  ;   (   Silent \== @on
	      ->  Error = EPos:Msg,
		  send(M, caret, EPos),
		  send(M, report, warning, 'Syntax error: %s', Msg)
	      ),
	      fail
	  ).


		 /*******************************
		 *	   MARK VARIABLE	*
		 *******************************/

typed(M, Id:'event|event_id', Editor:editor) :->
	"Extend variable marks"::
	send_super(M, typed, Id, Editor),
	(   object(M)			% Control-x k destroys the mode
	->  (   get(M, varmark_style, Style),
		Style \== @nil,
					% Otherwise the singleton fragments
					% are deleted
		\+ get(M, focus_function, '_replace_singletons')
	    ->  send(M, mark_variable, @on)
	    ;   true
	    )
	;   true
	).

new_caret_position(M, NewCaret:int) :->
	"Mark variables around caret"::
	send_super(M, new_caret_position, NewCaret),
	(   get(M, varmark_style, Style),
	    Style \== @nil,
	    send(M, mark_variable, @on),
	    true
	;   true
	).

varmark_style(M, Style:style*) :->
	"Set the style for marking variables"::
	send(M, slot, varmark_style, Style),
	send(M, style, varmark, Style).

%	(*)  Creating  fragments  finally  calls  ChangedRegionEditor(),
%	which clears the kill-location. We  preserve   it  over the mark
%	variables as this only concerns  colouring.   We  should fix the
%	editor/fragment/textbuffer  interfaces  to  have  two  types  of
%	changes  and  skip  resetting  the   kill-location  on  fragment
%	changes. This however involves a lot of  changes while it is not
%	likely to be a frequent  problem.   Therefore  this hack for the
%	moment.

mark_variable(M, Check:[bool]) :->
	"Mark variable around caret"::
	get(M, caret, Caret),
	get(M, text_buffer, TB),
	get(M, generation, Gen),
	(   get(M, var_mark_enabled, @off)
	->  true
	;   get(M, var_marked_caret, Caret),
	    get(M, var_marked_gen, Gen)
	->  true
	;   send(M, slot, var_marked_caret, Caret),
	    send(M, slot, var_marked_gen, Gen),

	    get(M, editor, E),
	    get(E, slot, kill_location, KillLocation),		%  (*)
	    send(M, unmark_variables),
	    get(M, beginning_of_clause, Caret, Start),
	    (   Check == @on
	    ->  check_clauses(M, Start, Caret)
	    ;   true
	    ),
	    (   get(M, prolog_term, Start, @on, Pos, Clause)
	    ->  (   find_variable(Pos, Clause, Caret, Var)
		->  get(M, text_buffer, TB),
		    send(M, slot, has_var_marks, @on),
		    (   subterm_position(Var, Clause, Pos, F-T),
			Len is T - F,
			new(_, emacs_colour_fragment(TB, F, Len, varmark)),
			fail
		    ;   true
		    )
		;   true
		)
	    ;   true
	    ),
	    send(E, slot, kill_location, KillLocation)
	).

check_clauses(M, Start, Caret) :-
	debug(emacs, '~p: Checking ~w..~w', [M, Start, Caret]),
	ignore(get(M, check_clause, Start, @off, End)),
	(   integer(End),
	    End > Start,
	    End < Caret+5
	->  check_clauses(M, End, Caret)
	;   true
	).


unmark_variables(M) :->
	"Remove all variable-mark fragments"::
	(   get(M, has_var_marks, @on)
	->  send(M, remove_syntax_fragments, style := varmark),
	    send(M, slot, has_var_marks, @off)
	;   true
	).

%	find_variable(+TermPos, +Clause, +Caret, -Var)
%
%	Find the variable around the caret and return it in Var. If the
%	caret is not on a variable, fail.

find_variable(F-T, Var, Caret, Var) :-
	between(F, T, Caret), !,
	var(Var).
find_variable(term_position(_,_,_,_,ArgPos), Compound, Caret, Var) :-
	nth1(N, ArgPos, P),
	arg(N, Compound, Arg),
	find_variable(P, Arg, Caret, Var).
find_variable(list_position(_,_,EP,TP), List, Caret, Var) :-
	list_pos(EP, TP, List, P, E),
	find_variable(P, E, Caret, Var).
find_variable(brace_term_position(_,_,TP), {Term}, Caret, Var) :-
	find_variable(TP, Term, Caret, Var).

list_pos([], P, T, P, T) :-
	P \== none, !.
list_pos([PH|_],  _, [EH|_], PH, EH).
list_pos([_|PT], TP, [_|ET],  P,  E) :-
	list_pos(PT, TP, ET, P, E).

%	subterm_position(+Term, +Clause, +TermPos, -Pos)
%
%	Find all positions at which Term appears in Clause.

subterm_position(Search, Term, Pos, Pos) :-
	Search == Term.
subterm_position(Search, Term, term_position(_,_,_,_,ArgPos), Pos) :- !,
	nth1(N, ArgPos, P2),
	arg(N, Term, A),
	subterm_position(Search, A, P2, Pos).
subterm_position(Search, List, list_position(_,_,EP,TP), Pos) :-
	list_pos(EP, TP, List, P, E),
	subterm_position(Search, E, P, Pos).
subterm_position(Search, {Term}, brace_term_position(_,_,TP), Pos) :-
	subterm_position(Search, Term, TP, Pos).


		 /*******************************
		 *       SOURCE DEBUGGER	*
		 *******************************/

break_at(M) :->
	"Set a Prolog break-point at this location"::
	send(M, save_buffer),
	get(M, text_buffer, TB),
	get(TB, file, File),
	(   source_file(Source),
	    send(File, same, Source)
	->  get(M, caret, Caret),
	    get(M, line_number, M?caret, Line),
	    (	guitracer,
	        auto_call(set_breakpoint(Source, Line, Caret, _))
	    ->  tdebug,			% debug all threads
		(   get(TB, margin_width, 0)
		->  send(TB, margin_width, 22)
		;   true
		),
		send(M, report, status, 'Break-point set')
	    ;	send(M, report, warning, 'Failed to set break-point')
	    )
	;   send(M, report, error, 'Source file is not loaded')
	).


delete_breakpoint(M) :->
	"Delete selected breakpoint"::
	(   get(M, selected_fragment, F),
	    send(F, instance_of, break_fragment),
	    get(F, breakpoint_id, Id)
	->  delete_breakpoint(Id)
	;   send(M, report, warning, 'No selected breakpoint')
	).

setup_margin(M) :->
	"Enable the editor margin if necessary"::
	(   get(M, margin_width, 0),
	    get(M, text_buffer, TB),
	    get(TB, file, File), File \== @nil,
	    get(File, absolute_path, Path),
	    absolute_file_name(Path, Canonical),
	    breakpoint_property(_, file(Canonical))
	->  send(M, margin_width, 22)
	;   true
	).


		 /*******************************
		 *	  CLAUSE FWD/BWD	*
		 *******************************/

forward_clause(M, Start:int, EOC:int) :<-
	"Find end of first clause after Start"::
	new(Here, number(Start)),
	repeat,
	(   send(@prolog_full_stop, search, M, Here)
	->  get(@prolog_full_stop, register_start, 1, Stop),
	    (   get(M, scan_syntax, 0, Stop, tuple(code,_))
	    ->	!,
	        EOC = Stop
	    ;	send(Here, value, Stop),
		fail
	    )
	;   !,
	    fail
	).

at_start_of_clause(M, Pos:[int]) :->
	"Succeeds if this is the start of a clause"::
	(   Pos == @default
	->  get(M, caret, C)
	;   C = Pos
	),
	get(M, scan, C, word, 0, start, SOW),
	SOW == C,
	(   send(M, looking_at, ':-', C)
	;   get(M, text_buffer, TB),
	    get(TB, scan, C, term, 1, end, TE),
	    get(TB, skip_comment, TE, Neck),
	    send(M, looking_at, ':-|-->|:<-|\\.', Neck)
	).

backward_clause(M, Start:int, BOC:int) :<-
	"Find start of clause or previous clause"::
	get(M, beginning_of_clause, Start, BOC0),
	(   BOC0 == Start
	->  Start2 is max(0, BOC0-1),
	    get(M, beginning_of_clause, Start2, BOC)
	;   BOC = BOC0
	).

forward_clause(M, Arg:[int]) :->
	"Go forwards by <arg> clauses"::
	default(Arg, 1, Times),
	get(M, caret, Caret),
	(   Times > 0
	->  do_n_times(Times, M, forward_clause, Caret, Pos)
	;   NegTimes is -Times,
	    do_n_times(NegTimes, M, backward_clause, Caret, Pos)
	),
	send(M, caret, Pos).

backward_clause(M, Arg:[int]) :->
	"Go backwards by <arg> clauses"::
	default(Arg, 1, Times),
	Forward is -Times,
	send(M, forward_clause, Forward).

do_n_times(0, _, _, Pos, Pos) :- !.
do_n_times(N, M, Sel, Here, End) :-
	get(M, Sel, Here, Pos), !,
	NN is N - 1,
	do_n_times(NN, M, Sel, Pos, End).
do_n_times(_, _, _, Pos, Pos).


		 /*******************************
		 *	 PREDICATE FWD/BWD	*
		 *******************************/

at_start_of_predicate(M, Start:[int]) :->
	(   Start == @default
	->  get(M, caret, P0)
	;   P0 = Start
	),
	send(M, at_start_of_clause, P0),
	get(M, name_and_arity, P0, tuple(Name, Arity)),
	(   get(M, backward_clause, P0, P1)
	->  \+ get(M, name_and_arity, P1, tuple(Name, Arity))
	;   true
	).


backward_predicate(M, P0:int, BPred:int) :<-
	"Find start of this/previous predicate"::
	(   send(M, at_start_of_predicate, P0)
	->  P1 is P0-1
	;   P1 is P0
	),
	get(M, beginning_of_clause, P1, BOC),
	(   get(M, name_and_arity, BOC, tuple(Name, Arity))
	->  new(BP, number(BOC)),	% clause
	    repeat,
		(   get(M, backward_clause, BP, BPC),
		    send(BP, larger, BPC)
		->  (   get(M, name_and_arity, BPC, tuple(Name, Arity))
		    ->  send(BP, value, BPC),
			fail
		    ;   !,
		        get(BP, value, BPred)
		    )
		;   !,
		    fail
		)
	;   get(M, backward_clause, P0-1, P2),
	    (	get(M, name_and_arity, P2, _)
	    ->	get(M, backward_predicate, P2+1, BPred)
	    ;	BPred = P2
	    )
	).

forward_predicate(M, P0:int, End:int) :<-
	"Find end of predicate"::
	get(M, forward_clause, P0, EOC),
	get(M, backward_clause, EOC, BOC),
	(   get(M, name_and_arity, BOC, tuple(Name, Arity))
	->  new(Here, number(EOC)),
	    repeat,
		get(M, skip_comment, Here, BONC),
		(   get(M, name_and_arity, BONC, tuple(Name, Arity))
		->  get(M, forward_clause, BONC, EONC),
		    send(Here, value, EONC),
		    fail
		;   !,
		    get(Here, value, End)
		)
	;   End = EOC
	).


forward_predicate(M, Arg:[int]) :->
	"Move forwards by <arg> predicates"::
	default(Arg, 1, Times),
	get(M, caret, P0),
	do_n_times(Times, M, forward_predicate, P0, P),
	send(M, caret, P).

backward_predicate(M, Arg:[int]) :->
	"Move backwards by <arg> predicates"::
	default(Arg, 1, Times),
	get(M, caret, P0),
	do_n_times(Times, M, backward_predicate, P0, P),
	send(M, caret, P).

		 /*******************************
		 *	SYNTAX HIGHLIGHTING	*
		 *******************************/

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
	get(M, xref_source_id, SourceID),
	setup_call_cleanup(
	    pce_open(TB, read, Stream),
	    ( set_stream_file(TB, Stream),
	      seek(Stream, Start, bof, _),
	      prolog_colourise_term(
		  Stream, SourceID, colour_item(M),
		  [ subterm_positions(TermPos)
		  ])
	    ),
	    close(Stream)).

set_stream_file(TB, Stream) :-
	get(TB, file, File), File \== @nil, !,
	get(File, absolute_path, Path0),
	absolute_file_name(Path0, Path),	% Make sure it is canonical
	set_stream(Stream, file_name(Path)).
set_stream_file(_, _).


:- dynamic
	style_name/2.

colour_item(M, range, Start, Length) :- !,
	End is Start+Length,
	send(M, remove_syntax_fragments, Start, End).
colour_item(M, atom, Start, Length) :-
	get(M, text_buffer, TB),
	get(TB, character, Start, 0'\'), !,
	colour_item(M, quoted_atom, Start, Length).
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

:- public
	style/3.			% Used by completion extension

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
	->  xref_source_id(TB, SourceId),
	    (	TB == SourceId
	    ->	send(M, report, progress, 'Cross-referencing buffer ...')
	    ;	send(M, report, progress, 'Cross-referencing %s ...', SourceId),
	        send(TB, attribute, xref_source_id, SourceId)
	    ),
	    xref_source(SourceId, [silent(true)]),
	    send(TB, xref_generation, G),
	    send(M, report, done)
	;   true
	).

xref_source_id(M, SourceId:any) :<-
	"Xref source identifier"::
	get(M, text_buffer, TB),
	(   get(TB, attribute, xref_source_id, SourceId)
	->  true
	;   SourceId = TB
	).

%%	xref_source_id(+TextBuffer, -SourceID) is det.
%
%	Find the object we need  to   examine  for cross-referencing. If
%	this is an included file, this is the corresponding main file.

xref_source_id(TB, SourceId) :-
	get(TB, file, File), File \== @nil,
	get(File, absolute_path, Path0),
	absolute_file_name(Path0, Path),
	master_load_file(Path, [], Master),
	(   Master == Path
	->  SourceId = TB
	;   SourceId = Master
	).


		 /*******************************
		 *	     PREDICATES		*
		 *******************************/

colourise_buffer(M) :-
	get(M, text_buffer, TB),
	get(M, xref_source_id, SourceID),
	setup_call_cleanup(
	    pce_open(TB, read, Stream),
	    ( set_stream_file(TB, Stream),
	      prolog_colourise_stream(Stream, SourceID, colour_item(M))
	    ),
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
	set_xref_fragment_context(Fragment, Class).
make_fragment(head(Class, Head), M, F, L, Style) :-
	callable(Head), !,
	get(M, text_buffer, TB),
	new(Fragment, emacs_head_fragment(TB, F, L, Style)),
	functor(Head, Name, Arity),
	send(Fragment, name, Name),
	send(Fragment, arity, Arity),
	set_xref_fragment_context(Fragment, Class).
make_fragment(class(Type, Class), M, F, L, Style) :-
	atom(Class), !,
	get(M, text_buffer, TB),
	new(Fragment, emacs_class_fragment(TB, F, L, Style)),
	functor(Type, Classification, _),
	send(Fragment, classification, Classification),
	send(Fragment, referenced_class, Class).
make_fragment(syntax_error(Message, _Range), M, F, L, Style) :- !,
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
	(   Arity >= 1,
	    arg(1, Class, Context),
	    atomic(Context)
	->  send(Fragment, context, Context)
	;   true
	).

set_xref_fragment_context(Fragment, Class) :-
	functor(Class, Classification, Arity),
	send(Fragment, classification, Classification),
	(   Arity == 1
	->  arg(1, Class, Context),
	    (   atomic(Context)
	    ->	send(Fragment, context, Context)
	    ;	ground(Context),
		Context = (Include:Line)
	    ->	send(Fragment, context, source_location(Include, Line))
	    ;	true
	    )
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

%%	make_prolog_mode_goal_popup(-Popup)
%
%	Create the popup and define actions for handling the right-menu
%	on goals.

make_prolog_mode_goal_popup(G) :-
	new(G, popup(goal_actions)),
	Fragment = @arg1,
	new(HasSource,  message(Fragment, has_source)),
	new(HasListing, message(Fragment, has_listing)),
	new(HasInfo,    message(Fragment, has_info)),
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
		    menu_item(info,
			      message(Fragment, info),
			      condition := HasInfo),
		    menu_item(listing,
			      message(Fragment, listing),
			      condition := HasListing),
		    gap,
		    menu_item(documentation,
			      message(Fragment, documentation))
		  ]).


module(F, Module:name) :<-
	"Module for Module:Goal references"::
	(   get(F, classification, extern),
	    get(F, context, Module),
	    Module \== @nil
	->  true
	;   get(F, file_module, Module)
	).


file_module(F, Module:name) :<-
	"Module used for the file"::
	get(F, text_buffer, TB),
	(   xref_module(TB, Module)
	->  true
	;   get(TB, file, File), File \== @nil,
	    get(File, absolute_path, Path0),
	    absolute_file_name(Path0, Path),
	    module_context(Path, [], Module)
	).


module_context(File, _, Module) :-
	source_file_property(File, module(Module)), !.
module_context(File, Seen, Module) :-
	source_file_property(File, included_in(File2, _Line)),
	\+ memberchk(File, Seen), !,
	module_context(File2, [File|Seen], Module).
module_context(File, _, Module) :-
	source_file_property(File, load_context(Module, _, _)).


predicate(F, Pred:prolog_predicate) :<-
	"Get referenced predicate"::
	get(F, name, Name),
	get(F, arity, Arity),
	(   get(F, module, Module)
	->  Spec = Module:Name/Arity
	;   Spec = Name/Arity
	),
	new(Pred, prolog_predicate(Spec)).

head(F, Qualify:[bool], Head:prolog) :<-
	"Return goal-head"::
	get(F, name, Name),
	get(F, arity, Arity),
	functor(Head0, Name, Arity),
	(   Qualify == @off			% @off: only if qualification is
	->  (   get(F, classification, extern),  % explicit.
	        get(F, context, Module),
		Module \== @nil
	    ->  Head = Module:Head0
	    ;	Head = Head0
	    )
	;   (   get(F, module, M)
	    ->  Head = M:Head0
	    ;   Head = Head0
	    )
	).

loaded_specifier(F, Head:prolog) :<-
	"Get predicate specifier for loaded predicate"::
	get(F, head, Head),
	current_predicate(_, Head).

has_source(F) :->
	"Test if there is source available"::
	get(F, text_buffer, TB),
	get(F, head, @off, Head),
	(   xref_defined(TB, Head, How),
	    xref_definition_line(How, _)
	;   Head = Module:Plain,
	    xref_module(Src, Module),
	    xref_defined(Src, Plain, How),
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
	send(Tmp, mode, prolog),
	setup_call_cleanup(
	    pce_open(Tmp, write, Out),
	    with_output_to(Out, listing(Spec)),
	    close(Out)),
	send(Tmp, modified, @off),
	send(Tmp, open, tab).


has_listing(F) :->
	"Test if we can make a listing"::
	get(F, loaded_specifier, Spec),
	predicate_property(Spec, number_of_clauses(N)),
	N > 0.

info(F) :->
	"Provide all know information about P"::
	get(F, predicate, P),
	send(P, info).

has_info(F) :->
	"Provide all know information about P"::
	get(F, predicate, P),
	get(P, head, Head),
	predicate_property(Head, _), !.

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
identify_pred(include(SrcLoc), F, Summary) :-
	get(SrcLoc, file_name, File),
	get(SrcLoc, line_no, Line),
	new(Summary, string('%N: included from %s:%d', F, File, Line)).
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
	functor(Head, Name, Arity),
	predicate_property(Head, autoload(Library)),
	absolute_file_name(Library, Source,
			   [ file_type(prolog),
			     access(read),
			     file_errors(fail)
			   ]).

:- pce_end_class(emacs_goal_fragment).


:- pce_begin_class(emacs_head_fragment, emacs_goal_fragment,
		   "Fragment for a predicate head in PceEmacs").

:- pce_group(popup).

popup(_GF, Popup:popup) :<-
	"Return popup menu"::
	Popup = @prolog_mode_head_popup.

:- pce_global(@prolog_mode_head_popup,
	      make_prolog_mode_head_popup).

%%	make_prolog_mode_head_popup(-Popup)
%
%	Create the popup and define actions for handling the right-menu
%	on predicate heads.

make_prolog_mode_head_popup(G) :-
	new(G, popup(head_actions)),
	Fragment = @arg1,
	new(HasListing, message(Fragment, has_listing)),
	new(HasInfo,    message(Fragment, has_info)),
	send_list(G, append,
		  [ menu_item(info,
			      message(Fragment, info),
			      condition := HasInfo),
		    menu_item(listing,
			      message(Fragment, listing),
			      condition := HasListing),
		    gap,
		    menu_item(documentation,
			      message(Fragment, documentation))
		  ]).

identify(F) :->
	"Tell the user about the predicate"::
	get(F, text_buffer, TB),
	get(F, classification, Class),
	(   get(F, context, Context),
	    Context \== @nil
	->  Id =.. [Class, Context]
	;   Id = Class
	),
	identify_head(Id, F, Report),
	send(TB, report, status, Report).

identify_head(Class, F, Summary) :-
	get(F, print_name, Name), !,
	(   get(F, loaded_specifier, Head)
	->  findall(Prop, head_property(Head, Prop), Props),
	    atomic_list_concat(Props, ', ', Text),
	    new(Summary, string('%s: (loaded) %s', Name, Text))
	;   term_to_atom(Class, Text),
	    new(Summary, string('%s: (not loaded) %s', Name, Text))
	).

head_property(Head, Text) :-
	predicate_property(Head, Prop),
	\+ hidden_property(Prop, Head),
	(   atomic(Text)
	->  Text = Prop
	;   property_text(Prop, Text)
	).

hidden_property(file(_), _).
hidden_property(line_count(_), _).
hidden_property(nodebug, _).
hidden_property(interpreted, _).
hidden_property(visible, _).
hidden_property(transparent, Head) :-
	predicate_property(Head, meta_predicate(_)).

property_text(number_of_clauses(N), Text) :- !,
	(   N == 1
	->  Text = '1 clause'
	;   atomic_list_concat([N, ' clauses'], Text)
	).
property_text(indexed(List), Text) :- !,
	(   List = [1-_]
	->  Text = 'hashed on first argument'
	;   List = [N-_]
	->  int_postfix(N, PostFix),
	    format(atom(Text), 'hashed on ~d-~w argument', [N, PostFix])
	;   pairs_keys(List, Args),
	    atomic_list_concat(Args, ', ', ArgText),
	    format(atom(Text), 'hashed on arguments ~w', [ArgText])
	).
property_text(meta_predicate(Head), Text) :- !,
	Head =.. [_|Args],
	Meta =.. [meta_predicate|Args],
	term_to_atom(Meta, Text).
property_text(Term, Text) :-
	term_to_atom(Term, Text).

int_postfix(1, st) :- !.
int_postfix(2, nd) :- !.
int_postfix(3, rd) :- !.
int_postfix(_, th).

:- pce_end_class(emacs_head_fragment).


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
	get(F, classification, FragClass),
	fragment_popup(FragClass, Popup).

fragment_popup(file,	       @prolog_mode_file_popup).
fragment_popup(file_no_depend, @prolog_mode_file_popup).
fragment_popup(module,	       @prolog_mode_module_popup).

:- pce_global(@prolog_mode_file_popup,
	      make_prolog_mode_file_popup).
:- pce_global(@prolog_mode_module_popup,
	      make_prolog_mode_module_popup).

make_prolog_mode_module_popup(G) :-
	new(G, popup(actions)),
	send_list(G, append,
		  [ menu_item(open_in_tab,
			      message(@emacs, open_file, @arg1?file, tab)),
		    menu_item(open_in_window,
			      message(@emacs, open_file, @arg1?file, window)),
		    menu_item(open_here,
			      message(@emacs, open_file, @arg1?file, here))
		  ]).

make_prolog_mode_file_popup(G) :-
	make_prolog_mode_module_popup(G),
	send_list(G, append,
		  [ gap,
		    new(R, popup(resolves, message(@arg1, edit)))
		  ]),
	send(R, update_message,
	     message(@arg1, add_resolve_items, R)).


file(F, File:name) :<-
	"Return associated file"::
	get(F, context, Context),
	(   (   get(F, classification, file)
	    ;	get(F, classification, file_no_depend)
	    )
	->  File = Context
	;   get(F, classification, module)
	->  module_property(Context, file(File))
	).

add_resolve_items(F, Popup:popup) :->
	"Add entries for resolved predicates"::
	send(Popup, clear),
	get(F, context, File),
	get(F, text_buffer, TB),
	(   get(TB, attribute, xref_source_id, SourceId)
	->  true
	;   SourceId = TB
	),
	findall(Head, ( xref_defined(SourceId, Head, imported(File)),
			xref_called(SourceId, Head, _)
		      ),
		UsedHeads0),
	(   UsedHeads0 == []
	->  (   \+ xref_defined(SourceId, Head, imported(File))
	    ->	Comment = 'File has no exports'
	    ;	Comment	= 'File resolves no predicates'
	    ),
	    send(Popup, append, menu_item(Comment, @nil))
	;   (   source_file_property(File, module(FromModule))
	    ->  maplist(head_pi(FromModule), UsedHeads0, UsedPreds0)
	    ;   maplist(head_pi, UsedHeads0, UsedPreds0)
	    ),
	    sort(UsedPreds0, UsedPreds),
	    forall(member(Pred, UsedPreds),
		   (   new(PredObj, prolog_predicate(Pred)),
		       send(Popup, append,
			    menu_item(PredObj, @default, PredObj?print_name))
		   ))
	).

head_pi(M0, Head, M:Name/Arity) :-
	strip_module(M0:Head, M, Term),
	functor(Term, Name, Arity).

head_pi(M0:Head, M:Name/Arity) :- !,
	strip_module(M0:Head, M, Term),
	functor(Term, Name, Arity).
head_pi(Term, Name/Arity) :- !,
	functor(Term, Name, Arity).


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

:- multifile
	identify/2.

identify_fragment(Term, _F, Text) :-
	phrase(syntax_message(Term), List), !,
	elements_to_string(List, Text).
identify_fragment(var,  _, 'Variable').
identify_fragment(file(Path), _, Summary) :-
	new(Summary, string('File %s', Path)).
identify_fragment(file_no_depend(Path), _, Summary) :-
	new(Summary, string('File %s (does not resolve any predicates)', Path)).
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
identify_fragment(head(public(_)), _,
		  'Public predicate (not exported, but called from elsewhere)').
identify_fragment(head(multifile), _, 'Multifile predicate').
identify_fragment(head(constraint), _, 'Constraint (CHR)').
identify_fragment(head(iso), _, 'ISO built-in (cannot be redefined)').
identify_fragment(head(built_in), _, 'SWI-Prolog built-in (cannot be redefined)').
identify_fragment(head(def_iso), _, 'Definition of an ISO built-in').
identify_fragment(head(def_swi), _, 'Definition of an SWI-Prolog built-in').
identify_fragment(head(imported(From)), _, Summary) :-
	new(Summary, string('Also imported from %s', From)).
identify_fragment(prolog_data, _, 'Pass Prolog term unmodified').
identify_fragment(keyword(except), _, 'Import all except given').
identify_fragment(keyword(as), _, 'Import under a different name').
identify_fragment(unused_import, _, 'Imported predicate is not used').
identify_fragment(undefined_import, _, 'Predicate is not exported').
identify_fragment(dcg_right_hand_ctx, _, 'right-hand-context (DCG `push-back\')').
identify_fragment(goal(not_callable), _, 'Goal is not callable (type error)').
identify_fragment(structured_comment, _, 'PlDoc comment').
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

elements_to_string(List, String) :-
	maplist(element_to_string, List, Parts),
	atomic_list_concat(Parts, String).

element_to_string(Fmt-Args, String) :- !,
	format(string(String), Fmt, Args).
element_to_string(nl, '\n') :- !.
element_to_string(Atom, Atom).

:- pce_end_class(emacs_prolog_fragment).
