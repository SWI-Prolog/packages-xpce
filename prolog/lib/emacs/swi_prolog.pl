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


:- module(swi_prolog_emacs_binding, []).
:- use_module(library(pce)).
:- require([ start_emacs/0
	   ]).

:- multifile
	user:message_hook/3.
:- dynamic
	user:message_hook/3.

/** <module> Add messages related to a source-location to the GUI

This module implements user:message_hook/3 to add messages printed using
print_message/2 that can be related to   a source-location to the window
@prolog_warnings.

This library is always loaded when XPCE is loaded.  Its functionality is
controlled by the Prolog flag message_ide.
*/

:- create_prolog_flag(message_ide, true, []).


		 /*******************************
		 *          WARNINGS		*
		 *******************************/

:- pce_global(@prolog_warnings, make_prolog_warning_list).

make_prolog_warning_list(L) :-
	new(L, emacs_hit_list('SWI-Prolog warnings')),
	send(L, clear_on_append, @on),
	send(L, expose_on_append, @on),
	send(L, message, error_at_location),
	send(L, open).

clear_message_list :-
	(   object(@prolog_warnings)
	->  send(@prolog_warnings, clear)
	;   true
	).

%%	ide_message(+Location, +String)
%
%	Display system messages in a graphical  window. Note that String
%	is locked to avoid XPCE's GC.

ide_message(Path:Line, String) :-
	start_emacs,
	new(Buffer, emacs_buffer(Path)),
	get(Buffer, scan, 0, line, Line-1, start, SOL),
	send(@prolog_warnings, append_hit, Buffer, SOL, @default, String),
	send(String, lock_object, @off).

message_to_pce(Term, Lines, Path:Line, String) :-
	(   Term = error(syntax_error(Error),
			 file(Path, Line, _LinePos, _CharPos))
	->  atom(Path), integer(Line),
	    (	atom(Error)
	    ->	Msg = Error
	    ;	format(atom(Msg), '~p', [Error])
	    ),
	    new(String, string('Syntax error: %s', Msg))
	;   Term = error(_, Location),
	    nonvar(Location),
	    Location = file(Path, Line)
	->  make_message(Lines, String)
	;   source_location(Path, Line),
	    make_message(Lines, String)
	),
	atom(Path),
	send(String, lock_object, @on).

%%	make_message(+MessageLine, -String) is det.
%
%	Translate a list of message lines into  a PCE string. The string
%	is  locked  because  it  is  send    to  the  IDE  thread  using
%	in_pce_thread/1.

make_message(Lines, String) :-
	phrase(make_message(Lines), Chars), !,
	new(String, string(Chars)).

make_message([]) -->
	[].
make_message([nl|T]) -->
	" ",
	make_message(T).
make_message([Fmt-Args|T]) --> !,
	{ format(codes(Codes, Tail), Fmt, Args)
	},
	dlist(Codes, Tail),
	make_message(T).
make_message([Fmt|T]) -->
	make_message([Fmt-[]|T]).

dlist(Codes, Tail, Codes, Tail).

%%	user:message_hook(+Term, +Level, +Lines)
%
%	Hook clauses that direct error messages to the (xpce) IDE.

user:message_hook(Term, Level, Lines) :-
	current_prolog_flag(message_ide, true),
	ide_message(Term, Level, Lines),
	fail.

:- meta_predicate
	pce(0).

ide_message(Term, Level, Lines) :-
	accept_level(Level), !,
	pce(pce_message(Term, Lines)).
ide_message(make(reload(_Files)), _, _) :-
	pce(clear_message_list).
ide_message(emacs(consult(_File)), _, _) :-
	pce(clear_message_list).

pce_message(Term, Lines) :-
	\+ object(@loading_emacs),
	message_to_pce(Term, Lines, Location, String),
	ide_message(Location, String).

accept_level(warning).
accept_level(error).

pce(Goal) :-
	pce_thread(PceThread),
	thread_self(PceThread), !,
	Goal.
pce(Goal) :-
	in_pce_thread(Goal).
