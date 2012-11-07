/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org/projects/xpce/
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

:- module(gui_tracer,
	  [ guitracer/0,
	    noguitracer/0,		% Switch it off
	    gtrace/0,			% Start tracer and trace
	    gtrace/1,			% :Goal
	    gspy/1,			% Start tracer and set spypoint
	    gdebug/0			% Start tracer and debug
	  ]).
:- set_prolog_flag(generate_debug_info, false).
:- meta_predicate
	gtrace(0),
	gspy(:).

/** <module> Graphical debugger utilities

This module provides utilities that use   the  graphical debugger rather
than the conventional 4-port commandline debugger.  This library is part
of XPCE.

@see	library(threadutil) provides another set t* predicates that
	deal with threads.
*/

%%	guitracer is det.
%
%	Enable the graphical debugger.  A   subsequent  call  to trace/0
%	opens the de debugger window. The   tranditional debugger can be
%	re-enabled using noguitracer/0.

guitracer :-
	current_prolog_flag(gui_tracer, true), !.
guitracer :-
	current_prolog_flag(gui_tracer, _), !,
	set_prolog_flag(gui_tracer, true),
	visible(+cut_call),
	print_message(informational, gui_tracer(true)).
guitracer :-
	load_files([library('trace/trace')],
		   [ silent(true),
		     if(not_loaded)
		   ]),
	set_prolog_flag(gui_tracer, true),
	visible(+cut_call),
	print_message(informational, gui_tracer(true)).

%%	noguitracer is det.
%
%	Disable the graphical debugger.
%
%	@see guitracer/0

noguitracer :-
	current_prolog_flag(gui_tracer, true), !,
	set_prolog_flag(gui_tracer, false),
	visible(-cut_call),
	print_message(informational, gui_tracer(false)).
noguitracer.

%%	gtrace is det.
%
%	Like trace/0, but uses the graphical tracer.

:- '$hide'(gtrace/0).			% don't trace it

gtrace :-
	guitracer,
	trace.

%%	gtrace(:Goal) is det.
%
%	Trace Goal in a separate thread,  such that the toplevel remains
%	free for user interaction.

gtrace(Goal) :-
	guitracer,
	thread_create(trace_goal(Goal), Id, [detached(true)]),
	print_message(informational, gui_tracer(in_thread(Id, Goal))).

:- meta_predicate trace_goal(0).

trace_goal(Goal) :-
	catch(trace_goal_2(Goal), _, true), !.
trace_goal(_).

trace_goal_2(Goal) :-
	setup_call_catcher_cleanup(
	    trace,
	    Goal,
	    Catcher,
	    finished(Catcher, Det)),
	notrace,
	(   Det == true
	->  true
	;   in_pce_thread_sync(send(@(display), confirm, 'Retry goal?'))
	->  trace, fail
	;   !
	).

:- '$hide'(finished/2).

finished(Reason, Det) :-
	notrace,
	print_message(informational, gui_tracer(completed(Reason))),
	(   Reason == exit
	->  Det = true
	;   Det = false
	).

%%	gspy(:Spec) is det.
%
%	Same as spy/1, but uses the graphical debugger.

gspy(Predicate) :-
	guitracer,
	spy(Predicate).

%%	gdebug is det.
%
%	Same as debug/0, but uses the graphical tracer.

gdebug :-
	guitracer,
	debug.


		 /*******************************
		 *	      MESSAGES		*
		 *******************************/

:- multifile
	prolog:message/3.

prolog:message(gui_tracer(true)) -->
	[ 'The graphical front-end will be used for subsequent tracing' ].
prolog:message(gui_tracer(false)) -->
	[ 'Subsequent tracing uses the commandline tracer' ].
prolog:message(gui_tracer(in_thread(Id, _Goal))) -->
	[ 'Debugging goal in new thread ~q'-[Id] ].
prolog:message(gui_tracer(completed(Reason))) -->
	[ 'Goal completed: ~q~n'-[Reason] ].
