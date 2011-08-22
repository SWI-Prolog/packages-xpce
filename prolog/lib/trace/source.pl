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

:- module(prolog_source_view,
	  [ mark_stop_point/2,		% +ClauseRef, +PC
	    unmark_stop_point/2,	% +ClauseRef, +PC
	    current_source_buffer/2	% +File, -Buffer
	  ]).
:- use_module(library(pce)).
:- use_module(library(pce_emacs)).
:- use_module(break).
:- use_module(util).
:- use_module(library(emacs_extend)).
:- use_module(library(pce_template)).
:- use_module(emacs_debug_modes).


		 /*******************************
		 *	       STYLES		*
		 *******************************/

style(call,		style(background := green,
			      icon := 'call.xpm')).
style(break,		style(background := cyan)).
style(exit,		style(background := green,
			      icon := 'exit.xpm')).
style(redo,		style(background := yellow,
			      icon := 'redo.xpm')).
style(fail,		style(background := '#ff8080',
			      icon := 'fail.xpm')).
style(exception,	style(background := magenta,
			      icon := 'except.xpm')).
style(unify,		style(background := sky_blue)).
style(choice,		style(background := yellow,
			      icon := 'ndet.xpm')).
style(frame,		style(background := '#d6dc5e',
			      icon := 'stack.xpm')).
style(breakpoint,	style(icon := 'stop.xpm')).


% If you define an alternative mode as a subclass of the Prolog mode
% and you use this mode in the debugger, you must create a mode
% <X>_debug.  The minimal example for this mode is below.  The
% predicate buffer/2 below associates the debug mode with the
% source view.

:- emacs_begin_mode(prolog_debug, prolog,
		    "Submode for the debugger",
		    [], []).
:- use_class_template(prolog_debug_methods).
:- emacs_end_mode.

:- initialization
   declare_emacs_mode(prolog_debug, []).


		 /*******************************
		 *	    SOURCE VIEW		*
		 *******************************/

:- pce_begin_class(prolog_source_view, emacs_view,
		   "Prolog GUI source viewer").

class_variable(size,	size,	size(80,20), "Default size in characters").

variable(source,	'name|emacs_buffer*', get, "Currently shown source").

initialise(V) :->
	send(V, send_super, initialise),
	send(V, mode, prolog_debug),
	send(V, margin_width, 22),
	forall(style(Name, Style), send(V, style, Name, Style)),
	send(V, editable, @off),
	send(V, update_label).

lost_text_buffer(V) :->
	"The textbuffer has been destroyed, replace by a new one"::
	new(Scratch, emacs_buffer(@nil, '*scratch*')),
	send(V, text_buffer, Scratch).

update_label(V) :->
	"Create label from <-editable and <-source"::
	get(V, source, Source),
	(   atom(Source)
	->  Label0 = Source
	;   Source == @nil
	->  Label0 = '<no source>'
	;   get(Source, attribute, comment, Label0)
	->  true
	;   get(Source, name, Label0)
	),
	(   get(V, editable, @on)
	->  send(V, label, string('[edit] %s', Label0))
	;   send(V, label, Label0)
	).

:- pce_group(event).

post_event(V, Ev:event) :->
	(   send(Ev, is_a, keyboard),
	    get(V, editable, @off),
	    get(V, frame, Tracer),
	    send(Tracer, has_send_method, source_typed),
	    send(Tracer, source_typed, Ev)
	->  true
	;   send_super(V, post_event, Ev)
	).

:- pce_group(edit).

edit(V, Val:[bool]) :->
	"Toggle read-only mode"::
	(   Val == @default
	->  get(V?editable, negate, NewVal)
	;   NewVal = Val
	),
	send(V, editable, NewVal),
	send(V, update_label).

:- pce_group(stop).

stop_at(V) :->
	"Set stop-point at location"::
	(   get(V, source_file, File)
	->  get(V, caret, Caret),
	    get(V, line_number, Line),
	    break_at(File, Line, Caret)
	;   send(V, report, error, 'No source'),
	    fail
	).

delete_selected_stop(V) :->
	"Deleted selected stop or stop overlapping <-caret"::
	(   get(V, selected_fragment, F),
	    send(F, instance_of, break_fragment)
	->  send(F, remove)
	;   get(V, caret, Caret),
	    get(V?text_buffer, find_fragment,
		and(message(@arg1, instance_of, break_fragment),
		    message(@arg1, overlap_caret, Caret)), F)
	->  send(F, remove)
	).

:- pce_group(source).

:- pce_global(@gui_last_change_check, new(date)).

not_recently_checked :-
	new(D, date),
	get(D, difference, @gui_last_change_check, Secs),
	(   Secs < 5
	->  free(D),
	    fail
	;   send(@gui_last_change_check, copy, D)
	).

source(V, Source:'name|emacs_buffer*') :->
	"Attach to indicated file"::
	debug('Attaching source ~p ...', [Source]),
	(   get(V, source, Source)
	->  send(V, check_modified)
	;   (   Source == @nil
	    ->  send(V, text_buffer, emacs_buffer(@nil, '<no source>'))
	    ;   send(Source, instance_of, emacs_buffer)
	    ->  send(Source, margin_width, 22),
	        send(V, text_buffer, Source)
	    ;   absolute_file_name(Source, Canonical),
		buffer(Canonical, B),
		send(V, text_buffer, B),
		send(V, check_modified)
	    ),
	    send(V, slot, source, Source),
	    send(V, update_label),
	    send(V?editor, auto_colourise_buffer)
	),
	debug('ok~n', []).

source_file(V, File:name) :<-
	"Currently shown sourcefile"::
	get(V, source, Source),
	atom(Source),
	canonical_source_file(Source, File).

check_modified(V) :->
	"Check for possibly modified file"::
	(   not_recently_checked
	->  get(V, text_buffer, TB),
	    send(TB, check_modified_file, V?frame, @off)
	;   true
	).

:- pce_group(show).

show_range(V, File:'name|emacs_buffer', From:int, To:int, Style:name) :->
	"Show indicated region using Style"::
	send(V, source, File),
	send(V, caret, To),
	new(F, fragment(V, From, To-From, Style)),
	ignore(send(V?frame, send_hyper, fragment, free)),
	new(_, trace_hyper(V?frame, F, fragment, tracer)),
	send(V, normalise, From, To).

show_line(V, File:'name|emacs_buffer', Line:int, Style:name) :->
	"Show numbered line"::
	debug('Show ~w:~w, style = ~w~n', [File, Line, Style]),
	send(V, source, File),
	get(V, text_buffer, TB),
	get(TB, scan, 0, line, Line - 1, start, SOL),
	get(TB, scan, SOL, line, 0, end, EOL),
	debug('Char range ~w ... ~w~n', [SOL, EOL]),
	send(V, show_range, File, SOL, EOL, Style).

listing(V, Module:name, Predicate:name, Arity:int) :->
	"List the specified predicate"::
	functor(Head, Predicate, Arity),
	send(V, source, @nil),
	get(V, text_buffer, TB),
	setup_call_cleanup(open(TB, write, Fd),
			   with_output_to(Fd, listing(Module:Head)),
			   close(Fd)).

:- pce_end_class.

		 /*******************************
		 *      BUFFER MANAGEMENT	*
		 *******************************/

current_source_buffer(Buffer, Buffer) :-
	object(Buffer),
	send(Buffer, instance_of, emacs_buffer), !.
current_source_buffer(File, Buffer) :-
	get(@emacs, file_buffer, File, Buffer).

buffer(File, Buffer) :-
	new(Buffer, emacs_buffer(File)),
	get(Buffer, mode, Mode),
	(   sub_atom(Mode, _, _, 0, '_debug')
	->  true
	;   atom_concat(Mode, '_debug', DebugMode),
	    get(@pce, convert, Mode, emacs_mode, _), % force loading the code
	    get(@pce, convert, DebugMode, emacs_mode, _)
	->  send(Buffer, mode, DebugMode)
	;   send(Buffer, mode, prolog_debug)
	),
	mark_special(File, Buffer).

mark_special(_, Buffer) :-
	get(Buffer, attribute, debugger_marks_done, @on), !.
mark_special(File, Buffer) :-
	canonical_source_file(File, Source),
	send(Buffer, attribute, debugger_marks_done, @on),
	send(Buffer, margin_width, 22),
	mark_stop_points(Buffer, Source).

mark_stop_points(_, Source) :-
	'$current_break'(ClauseRef, PC),
	clause_property(ClauseRef, file(Source)),
	mark_stop_point(ClauseRef, PC),
	fail.
mark_stop_points(_, _).

:- pce_global(@prolog_debugger, new(object)).

%%	mark_stop_point(+ClauseRef, +PC)
%
%	Mark stop-points using a breakpoint fragment.

mark_stop_point(ClauseRef, PC) :-
	break_fragment(ClauseRef, PC, _), !.
mark_stop_point(ClauseRef, PC) :-
	break_location(ClauseRef, PC, File, A-Z),
	current_source_buffer(File, Buffer),
	new(F, break_fragment(Buffer, A, Z)),
	assertz(break_fragment(ClauseRef, PC, F)),
	new(_, hyper(@prolog_debugger, F, break, debugger)).

unmark_stop_point(ClauseRef, PC) :-
	forall(break_fragment(ClauseRef, PC, Fragment),
	       free(Fragment)).


		 /*******************************
		 *	      TRACE HYPER	*
		 *******************************/

:- pce_begin_class(trace_hyper, hyper).

unlink_from(H) :->
	get(H, to, Fragment),
	free(Fragment),
	free(H).

:- pce_end_class.

:- pce_begin_class(break_fragment, fragment,
		   "Visualise a break-point").

:- dynamic
	break_fragment/3.

initialise(F, TB:text_buffer, Start:int, End:int) :->
	"Indicate the location of a break-point"::
	Len is End - Start,
	send_super(F, initialise, TB, Start, Len, breakpoint).

unlink(F) :->
	retractall(break_fragment(_,_,F)),
	send_super(F, unlink).

remove(F) :->
	"Remove the associated break-point"::
	break_fragment(ClauseRef, PC, F),
	'$break_at'(ClauseRef, PC, false).

overlap_caret(F, Caret:int) :->
	"True if F overlaps with position"::
	(   send(F, overlap, Caret)
	->  true
	;   get(F, end, E),
	    get(F, start, S),
	    Caret >= S, Caret =< E+1
	).

:- pce_end_class.
