/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.nl/projects/xpce/
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

:- module(start_emacs,
	  [ emacs/0
	  , emacs/1				% x File
	  , start_emacs/0
	  , emacs_server/0
	  , emacs_toplevel/0
	  ]).
:- use_module(library(pce)).
:- require([ append/3
	   , maplist/3
	   , unix/1
	   ]).

:- pce_autoload(emacs,	    library('emacs/emacs')).
:- pce_autoload(emacs_view, library('emacs/emacs')).

:- pce_global(@emacs_buffers, new(dict)).
:- pce_global(@emacs, new(emacs(@emacs_buffers))).


/** <module> PceEmacs toplevel

This module provides predicates to start  PceEmacs. PceEmacs is an clone
of GNU-Emacs written in  XPCE.  Modes  are   XPCE  classes  that  can be
extended in Prolog.

@see	Set Prolog flag editor to pce_emacs to make PceEmacs the default
	for edit/1.
*/

%%	start_emacs is det.
%
%	Create PceEmacs, but no buffers nor windows.

start_emacs :-
	register_emacs,
	(   object(@emacs)
	->  true
	;   in_pce_thread_sync(send(@emacs, start))
	).


%%	register_emacs is det.
%
%	If the user has not specified a specific editor and has started
%	PceEmacs, make it the default editor.

register_emacs :-
	(   current_prolog_flag(editor, '$EDITOR')
	->  set_prolog_flag(editor, pce_emacs)
	;   true
	).


%%	emacs_server is det.
%
%	Create a PceEmacs, ready to run as an unattended background
%	server.

emacs_server :-
	start_emacs,
	send(@pce, trap_errors, @off),
	send(@pce, console_label, 'PceEmacs Server').

%%	emacs is det.
%
%	Create PceEmacs and open the *scratch* buffer.

emacs :-
	start_emacs,
	in_pce_thread((new(Scratch, emacs_buffer(@nil, '*scratch*')),
		       send(Scratch, open, tab))).

%%	emacs(+Location) is det.
%
%	Create PceEmacs and edit  Location.  Location   is  one  of  the
%	following, where File must be an atom   and Line and LinePos are
%	integers.
%
%	  - File:Line:LinePos
%	  - File:Line
%	  - File

emacs(File:Line:LinePos) :-
	integer(Line),
	integer(LinePos),
	atom(File), !,
	start_emacs,
	new(Loc, source_location(File, Line)),
	send(Loc, attribute, linepos, LinePos),
	in_pce_thread(send(@emacs, goto_source_location, Loc, tab)).
emacs(File:Line) :-
	integer(Line),
	atom(File), !,
	start_emacs,
	in_pce_thread(send(@emacs, goto_source_location,
			   source_location(File, Line), tab)).
emacs(File) :-
	atom(File), !,
	start_emacs,
	in_pce_thread(send(@emacs, goto_source_location,
			   source_location(File), tab)).
emacs(File) :-
	domain_error(location, File).


%%	emacs_toplevel is det.
%
%	Prepare to run PceEmacs as a stand-alone executable.

emacs_toplevel :-
	send(@pce, trap_errors, @off),
	current_prolog_flag(argv, Files),
	(   Files = [_|_]
	->  start_emacs,
	    maplist(make_buffer, Files, [B0|_]),
	    send(B0, open)
	;   emacs
	).

make_buffer(File, Buffer) :-
	new(Buffer, emacs_buffer(File)).
