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

:- module(pce_swi_hooks, []).

/** <module> Hook XPCE based graphics tools into IDE

Loading this file enables the graphical  frontends for the online manual
and profiler. This file is normally   loaded from swipl.rc (swipl-win.rc
on Windows); the file that makes XPCE known to Prolog.

This file uses call/1 to call the real work to avoid undefined predicate
messages when using make/0 (calling list_undefined/0).

Since  the  introduction   of   the    more   advanced   autoloader   in
library(prolog_autoload), using call/1 no longer   suffices to stop this
file from being loaded.
*/

:- set_module(class(development)).

:- multifile
	prolog:debug_control_hook/1,
	prolog:help_hook/1,
	prolog:show_profile_hook/1,		% new
	prolog:show_profile_hook/2.		% compatibility


		 /*******************************
		 *	    DEBUG HOOKS		*
		 *******************************/

prolog:debug_control_hook(spy(Method)) :-
	call(spypce(Method)).
prolog:debug_control_hook(nospy(Method)) :-
	call(nospypce(Method)).


		 /*******************************
		 *	     HELP HOOK		*
		 *******************************/

prolog:help_hook(help) :- !,
	call(prolog_help).
prolog:help_hook(apropos(What)) :- !,
	call(prolog_apropos(What)).
prolog:help_hook(help(What)) :- !,
	call((   in_pce_thread_sync(pce_to_method(What, Method))
	     ->  manpce(Method)
	     ;	 current_prolog_flag(pldoc_collecting, _),
		 doc_browser(What)
	     ->	 true
	     ;   prolog_help(What)
	     )).


		 /*******************************
		 *	     PROFILING		*
		 *******************************/

prolog:show_profile_hook(_Options) :-
	call(pce_show_profile).
prolog:show_profile_hook(_Style, _Top) :-
	call(pce_show_profile).


		 /*******************************
		 *	       SOURCE		*
		 *******************************/

%%	prolog:alternate_syntax(?Syntax, +Module, -Setup, -Restore)
%
%	Implements operator handling for reading   arbitrary  terms from
%	XPCE classes.

:- multifile
	prolog:alternate_syntax/4.

prolog:alternate_syntax(pce_class, M, pce_expansion:push_compile_operators(M),
				      pce_expansion:pop_compile_operators) :-
	current_prolog_flag(xpce, true).

