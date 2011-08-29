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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(prolog_break,
	  [ break_at/3,			% +File, +Line, +CharPos
	    break_location/4		% +ClauseRef, +PC, -File, -A-Z
	  ]).
:- use_module(trace).
:- use_module(util).
:- use_module(clause).
:- use_module(source).
:- use_module(library(debug)).

/** <module> Manage Prolog break-points

*/

:- dynamic
	user:prolog_event_hook/1.
:- multifile
	user:prolog_event_hook/1.

%%	break_at(+File, +Line, +Char) is det.
%
%	Put a breakpoint at the  indicated   source-location.  File is a
%	current sourcefile (as reported by   source_file/1). Line is the
%	1-based line in which Char  is.  Char   is  the  position of the
%	break.
%
%	First, '$clause_from_source'/3 uses the SWI-Prolog clause-source
%	information to find  the  last   clause  starting  before  Line.
%	'$break_pc' generated (on backtracking),  a   list  of  possible
%	break-points.
%
%	Note that in addition to  setting   the  break-point, the system
%	must be in debug mode. With threading enabled, there are various
%	different ways this may  be  done.   See  debug/0,  tdebug/0 and
%	tdebug/1. Therefore, this predicate  does   *not*  enable  debug
%	mode.

break_at(File, Line, Char) :-
	debug(break, 'break_at(~q, ~d, ~d).~n', [File, Line, Char]),
	'$clause_from_source'(File, Line, ClauseRef),
	pce_clause_info(ClauseRef, InfoFile, TermPos, _NameOffset),
	(   InfoFile == File
	->  '$break_pc'(ClauseRef, PC, NextPC),
	    debug(break, 'Clause ~p, NextPC = ~w~n', [ClauseRef, NextPC]),
	    '$clause_term_position'(ClauseRef, NextPC, List),
	    debug(break, 'Location = ~w~n', [List]),
	    range(List, TermPos, A, Z),
	    debug(break, 'Term from ~w-~w~n', [A, Z]),
	    Z >= Char, !
	;   format('Failed to unify clause ~p, using first break~n',
		   [ClauseRef]),
	    '$break_pc'(ClauseRef, PC, _), !
	),
	debug(break, 'Break at clause ~w, PC=~w~n', [ClauseRef, PC]),
	'$break_at'(ClauseRef, PC, true).

range([], Pos, A, Z) :-
	arg(1, Pos, A),
	arg(2, Pos, Z).
range([H|T], term_position(_, _, _, _, PosL), A, Z) :-
	nth1(H, PosL, Pos),
	range(T, Pos, A, Z).

		 /*******************************
		 *	      FEEDBACK		*
		 *******************************/

user:prolog_event_hook(break(ClauseRef, PC, Set)) :-
	break(Set, ClauseRef, PC).

break(SetClear, ClauseRef, PC) :-
	print_message(informational, break(SetClear, ClauseRef, PC)),
	(   SetClear == true
	->  debug(break, 'Trap in Clause ~p, PC ~d~n', [ClauseRef, PC]),
	    clause_property(ClauseRef, file(File)),
	    current_source_buffer(File, _Buffer),
	    mark_stop_point(ClauseRef, PC)
	;   debug(break, 'Deleted break at clause ~p, PC ~d~n', [ClauseRef, PC]),
	    unmark_stop_point(ClauseRef, PC)
	).

%%	break_location(+ClauseRef, +PC, -File, -A-Z)
%
%	Determine source-code location of a break-point.

break_location(ClauseRef, PC, File, A-Z) :-
	pce_clause_info(ClauseRef, File, TermPos, _NameOffset),
	'$fetch_vm'(ClauseRef, PC, NPC, _VMI),
	'$clause_term_position'(ClauseRef, NPC, List),
	debug(break, 'ClausePos = ~w~n', [List]),
	range(List, TermPos, A, Z),
	debug(break, 'Range: ~d .. ~d~n', [A, Z]).


		 /*******************************
		 *	      MESSAGES		*
		 *******************************/

:- multifile
	prolog:message/3.

prolog:message(break(SetClear, ClauseRef, _PC)) -->
	setclear(SetClear),
	clause_location(ClauseRef).

setclear(true) -->
	['Breakpoint at '].
setclear(false) -->
	['Cleared breakpoint from '].

clause_location(ClauseRef) -->
	{ clause_property(ClauseRef, file(File)),
	  clause_property(ClauseRef, line(Line)), !,
	  clause_name(ClauseRef, Name)
	},
	['~w at ~w:~d'-[Name, File, Line] ].
clause_location(ClauseRef) -->
	{ clause_name(ClauseRef, Name)
	},
	['~w'-[Name] ].


