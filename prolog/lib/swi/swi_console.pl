/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org/packages/xpce/
    Copyright (C): 2011, University of Amsterdam

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

:- module(pce_swi_console,
	  [
	  ]).
:- use_module(library(pce)).
:- use_module(library(toolbar)).
:- use_module(library(persistent_frame)).

/** <module> Overall controller window for SWI-Prolog
*/

:- pce_begin_class(swi_console, persistent_frame,
		   "SWI-Prolog graphical console").

initialise(Con) :->
	send_super(Con, initialise, 'SWI-Prolog console'),
	send(Con, append, new(TD, tool_dialog(Con))),
	send(Con, fill_tool_dialog, TD).

fill_tool_dialog(Con, TD:tool_dialog) :->
	"Fill menu-bar"::
	send_list(TD, append,
		  [ new(File, popup(file)),
		    new(Tools, popup(tools)),
		    new(Compile, popup(compile)),
		    new(Help, popup(help))
		  ]),
	send_list(File, append,
		  [ menu_item(exit, message(Con, destroy))
		  ]),
	send_list(Tools, append,
		  [ menu_item(navigator,
			      message(@prolog_ide, open_navigator)),
		    menu_item(threads,
			      message(@prolog_ide, thread_monitor)),
		    menu_item(debug,
			      message(@prolog_ide, open_debug_status)),
		    menu_item(exceptions,
			      message(@prolog_ide, open_exceptions)),
		    menu_item(cross_referencer,
			      message(@prolog_ide, xref))
		  ]),
	send_list(Compile, append,
		  [ menu_item(make, message(@prolog, make))
		  ]),
	send_list(Help, append,
		  [ menu_item(about),
		    menu_item('help (on www)', message(Con, help))
		  ]).


		 /*******************************
		 *	      ACTIONS		*
		 *******************************/

about([ 'SWI-Prolog ~w'+Version-boldhuge,
	'Copyright 1986-2011',
        'University of Amsterdam, VU University Amsterdam',
	'SWI-Prolog comes with ABSOLUTELY NO WARRANTY.',
	'This is free software (LGPL), and you are welcome to',
	'redistribute it under certain conditions.',
	url('http://www.swi-prolog.org')
      ]) :-
	(   current_prolog_flag(version_git, Version)
	->  true
	;   current_prolog_flag(version, Version),
	    Major is Version // 10000,
	    Minor is (Version // 100) mod 100,
	    Patch is Version mod 100,
	    atomic_list_concat([Major, Minor, Patch], '.', Version)
	).

about(M) :->
	"Print about and licence info"::
	new(D, dialog('About SWI-Prolog')),
	send(D, transient_for, M),
	about(List),
	maplist(add_about(D), List),
	send(D, append, button(ok, message(D, destroy))),
	send(D, open_centered).

add_about(D, X-Font) :- !,
	add_about(X, Font, D).
add_about(D, X) :-
	add_about(X, normal, D).

add_about([], _, _) :- !.
add_about([H|T], Font, D) :- !,
	add_about(H, Font, D),
	add_about(T, Font, D).
add_about(url(Url), Font, D) :- !,
	send(D, append, new(T, text(Url, center, Font))),
	send(T, underline, @on),
	send(T, colour, blue),
	send(T, recogniser,
	     click_gesture(left, '', single,
			   message(@prolog, goto_url, T?string?value))),
	send(T, cursor, hand2),
	send(T, alignment, center).
add_about(Fmt+Args, Font, D) :- !,
	format(atom(Text), Fmt, Args),
	send(D, append, new(T, text(Text, center, Font))),
	send(T, alignment, center).
add_about(Text, Font, D) :-
	send(D, append, new(T, text(Text, center, Font))),
	send(T, alignment, center).

goto_url(Url) :-
	send(@display, busy_cursor),
	(   catch(www_open_url(Url), _, fail)
	->  true
	;   send(@display, inform, 'Failed to open URL')
	),
	send(@display, busy_cursor, @nil).

help(_Con) :->
	"Open help (www)"::
	URL = 'http://www.swi-prolog.org/pldoc/index.html',
	(   catch(www_open_url(URL), _, fail)
	->  true
	;   send(@display, inform, 'Failed to open: ', URL)
	).

:- pce_end_class.
