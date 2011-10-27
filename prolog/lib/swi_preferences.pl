/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        J.Wielemaker@cs.vu.nl
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

:- module(prolog_preferences,
	  [ prolog_edit_preferences/1	% +What
	  ]).
:- use_module(library(pce)).
:- use_module(library(pce_tick_box)).

/** <module> Edit preferences files

This  module  provides  prolog_edit_preferences/1,  which   is  used  to
simplify locating the preference files and provide a default if the user
has no such file.

@see	library(win_menu) binds this to the Settings menu of the console on
	the MS-Windows version.
*/

%%	prolog_edit_preferences(+What) is det.
%
%	Edit the specified user preference file.  What is one of
%
%	    * =xpce=
%	    * =prolog=
%
%	The UI components are started asynchronously in the XPCE thread.

prolog_edit_preferences(What) :-
	in_pce_thread(pce_edit_preferences(What)).

pce_edit_preferences(What) :-
	locate_preferences(What, File),
	auto_call(start_emacs),
	(   \+ access_file(File, exist)
	->  send(@display, confirm,
		 'Preferences file %s doesn''t exist.\nCreate it?', File),
	    (	default_preferences(What, DefFile)
	    ->	copy_file(DefFile, File)
	    ;	true
	    )
	;   access_file(File, write)
	->  true
	;   send(@display, inform,
		 'You cannot modify the preferences file %s', File)
	),
	send(@emacs, goto_source_location, File).

locate_preferences(xpce, File) :-
	ensure_xpce_config_dir(Dir),
	get(string('%s/Defaults', Dir), value, File).
locate_preferences(prolog, File) :-
	'$option'(init_file, Base), % should be in current_prolog_flag!
	(   absolute_file_name(user_profile(Base), File,
			       [ access(read),
				 file_errors(fail)
			       ])
	->  true
	;   absolute_file_name(app_preferences(Base), File,
			       [ access(write),
				 file_errors(fail)
			       ])
	).

%%	default_preferences(+Id, -File)
%
%	If there is a default file for the preferences, return a path to
%	it, so the user can be presented a starting point.

default_preferences(prolog, File) :-
	member(Location,
	       [ swi('customize/pl.ini'),
		 swi('customize/dotplrc')
	       ]),
	absolute_file_name(Location, File,
			   [ access(read),
			     file_errors(fail)
			   ]), !.
default_preferences(xpce, File) :-
	absolute_file_name(pce('Defaults.user'), File,
			   [ access(read),
			     file_errors(fail)
			   ]), !.


%%	ensure_xpce_config_dir(-Dir:atom)
%
%	Ensure existence of the personal XPCE config directory.

ensure_xpce_config_dir(Dir) :-
	get(@pce, application_data, AppDir),
	(   send(AppDir, exists)
	->  true
	;   send(AppDir, make)
	),
	get(AppDir, path, Dir).


copy_file(From, To) :-
	send(file(To), copy, From).
