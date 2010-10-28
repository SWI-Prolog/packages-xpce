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
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(emacs_application, []).
:- use_module(library(pce)).
:- (   current_prolog_flag(windows, true)
   ->  use_module(dde_server)
   ;   true
   ).
:- require([ ignore/1
	   , pce_help_file/2
	   ]).

:- pce_global(@finder, new(finder)).
:- pce_autoload(finder, library(find_file)).


:- pce_begin_class(emacs, application,
		   "PceEmacs main object").

variable(buffer_list,	dict,	get, "List of buffers maintained").
variable(exit_message,	message,get, "Registered exit message").


		 /*******************************
		 *	      CREATE		*
		 *******************************/

initialise(Emacs, Buffers:dict) :->
	send(Emacs, send_super, initialise, emacs),
	send(Emacs, leader, frame('PceEmacs')),
	send(Emacs, kind, service),
	send(Emacs, slot, buffer_list, Buffers),
	new(Msg, message(Emacs, check_saved_at_exit)),
	send(@pce, exit_message, Msg),
	send(Emacs, slot, exit_message, Msg),
	new(@emacs_mark_list, emacs_bookmark_editor),
	ignore(send(Emacs, server_start)),
	ignore(send(Emacs, load_user_init_file)).

unlink(Emacs) :->
	(   get(Emacs, exit_message, Msg),
	    send(@pce?exit_messages, delete, Msg)
	;   true
	),
	send(Emacs, send_super, unlink).

start(_Emacs) :->
	true.

		 /*******************************
		 *	   BUFFER MENU		*
		 *******************************/

:- pce_group(buffer).

show_buffer_menu(Emacs) :->
	"Show the buffer menu"::
	(   get(Emacs, member, buffer_menu, Menu)
	->  send(Menu, expose)
	;   send(emacs_buffer_menu(Emacs), open)
	).


selection(Emacs, B:emacs_buffer*) :->
	"Select emacs buffer"::
	(   get(Emacs, member, buffer_menu, Menu)
	->  send(Menu, selection, B)
	;   true
	).


		 /*******************************
		 *      BUFFERS AND FILES	*
		 *******************************/

:- pce_group(file).

buffer(Emacs, Name:name, B:emacs_buffer) :<-
	"Find named buffer"::
	get(Emacs, buffer_list, Dict),
	get(Dict, member, Name, DI),
	get(DI, object, B).

file_buffer(_, File:file, Buffer:emacs_buffer) :<-
	"Find existing buffer holding file"::
	get(File, base_name, Base),
	get(@emacs_base_names, member, Base, Chain),
	get(Chain, find, message(@arg1?file, same, File), Buffer).

buffers(Emacs, Buffers:chain) :<-
	"Chain with all emacs-buffers"::
	get(Emacs?buffer_list?members, map, @arg1?object, Buffers).


open_file(_Emacs, File:file, How:[{here,tab,window}]) :->
	"Open a file"::
	new(B, emacs_buffer(File)),
	send(B, open, How).


find_file(Emacs, Dir:[directory]) :->
	"Find and edit file"::
	get(@finder, file, @on, @default, Dir, FileName),
	send(Emacs, open_file, FileName).

goto_source_location(_Emacs,
		     Location:source_location,
		     Where:where=[{here,tab,window}]) :->
	"Visit the indicated source-location"::
	get(Location, file_name, File),
	new(B, emacs_buffer(File)),
	get(B, open, Where, Frame),
	send(B, check_modified_file),
	(   get(Location, line_no, Line),
	    Line \== @nil
	->  get(Frame, editor, Editor),
	    send(Editor, mark_status, inactive),
	    send(Editor?mode, select_line, Line)
	;   true
	).


edit(Emacs, Location:source_location) :->
	"Equivalent to ->goto_source_location"::
	send(Emacs, goto_source_location, Location).


existing_file(_Emacs, Dir:[directory], File:file) :<-
	"Find existing file in directory"::
	get(@finder, file, @on, @default, Dir, FileName),
	new(File, file(FileName)).

open_object(_Emacs, Object:prolog, _NewWindow:new_window=[bool]) :->
	"Open from description"::
	edit(Object).

show_bookmarks(_) :->
	"Show PceEmacs bookmarks window"::
	send(@emacs_mark_list, expose).


		 /*******************************
		 *	       SAVE		*
		 *******************************/
:- pce_group(save).

save_some_buffers(BM, Confirm:[bool]) :->
	"Save all modified buffers"::
	new(ModifiedItem,
	    and(@arg1?object?file \== @nil,
		@arg1?object?modified == @on)),
	(   get(BM?buffer_list, find, ModifiedItem, _)
	->  send(BM?buffer_list, for_some,
		 and(ModifiedItem,
		     or(Confirm == @off,
			message(@display, confirm, 'Save %s?',
				@arg1?object?file?name)),
		     message(@arg1?object, save, @arg1?object?file)))
	;   send(@pce, report, status, 'No buffers need saving')
	).


check_saved_at_exit(BM) :->
	"Check for unsaved buffers when called from exit"::
	send(BM, save_some_buffers, @on),
	new(ModifiedItem,
	    and(@arg1?object?file \== @nil,
		@arg1?object?modified == @on)),
	(   get(BM?buffer_list, find, ModifiedItem, _)
	->  (   send(@display, confirm, 'Discard modified buffers?')
	    ->	true
	    ;	repeat,
			send(@display, dispatch),
			format('Dispatch running; discarding input~n', []),
			get0(_),
			fail
	    )
	;   true
	).

		 /*******************************
		 *	      WINDOWS		*
		 *******************************/
:- pce_group(window).

current_frame(Emacs, Frame:emacs_frame) :<-
	"PceEmacs frame the user is working in"::
	(   send(@event, instance_of, event),
	    get(@event, window, Window),
	    get(Window, frame, Frame),
	    send(Frame, instance_of, emacs_frame)
	->  true
	;   get(Emacs?members, find,
		and(message(@arg1, instance_of, emacs_frame),
		    message(@arg1, on_current_desktop)),
		Frame)
	).


		 /*******************************
		 *	       MODE		*
		 *******************************/

:- pce_group(mode).

modes(_Emacs, ModeNames:chain) :<-
	"Return chain with known modes"::
	get(@mode_name_type, context, ModeNames).


		 /*******************************
		 *	       HELP		*
		 *******************************/

:- pce_group(help).

:- pce_help_file(emacs, pce_help('emacs.hlp')).
:- pce_help_file(emacs_customise, pce_help('customise.hlp')).

help(_Emacs) :->
	"Display general help"::
	send(@helper, give_help, emacs, main).

customise(_Emacs) :->
	"Display customisation help"::
	send(@helper, give_help, emacs_customise, main).


		 /*******************************
		 *		SERVER		*
		 *******************************/

:- pce_group(server).

server_start(Emacs, Force:[bool]) :->
	"Start server-mode (xpce-client interface)"::
	(   (	\+ get(class(socket), send_method, listen, _)
	    ;	\+ send(class(socket), has_feature, unix_domain)
	    ;	get(@emacs_server, status, listen)
	    )
	->  (   current_prolog_flag(windows, true)
	    ->	Goal = start_emacs_dde_server(false), % fool xref
	        catch(Goal, _, true)
	    ;	true
	    )
	;   (	send(@emacs_server_address, exists, @off)
	    ->  (   Force \== @on,
		    pce_catch_error(socket, send(@emacs_server, connect))
		->  free(@emacs_server),
		    send(Emacs, report, status, 'Server on other PceEmacs'),
		    fail
		;   free(@emacs_server), % will recreate!
		    ignore(send(Emacs, report, status, 'Restarted server')),
		    send(@emacs_server_address, remove)
		)
	    ;	true
	    ),
	    ignore(send(@emacs_server, listen))
	).


:- pce_group(customise).


		 /*******************************
		 *	 USER EXTENSIONS	*
		 *******************************/

load_user_extension(_Emacs, Base:name) :->
	"Load Prolog user file with this base-name"::
	(   absolute_file_name(emacs_user_library(Base),
			       [ access(read),
				 file_type(prolog),
				 file_errors(fail)
			       ],
			       Extension)
	->  ignore(load_files(user:Extension, [autoload(true)]))
	;   true
	).


load_user_init_file(_Emacs) :->
	"Load user_profile('.pceemacsrc') or user_profile('pceemacs.ini')"::
	(   get(@pce, operating_system, win32)
	->  Base = 'pceemacs.ini'
	;   Base = '.pceemacsrc'
	),
	(   absolute_file_name(user_profile(Base),
			       [ access(read),
				 file_errors(fail)
			       ],
			       Profile)
	->  ignore(load_files(user:Profile, [autoload(true)]))
	;   true
	).

:- pce_end_class(emacs).

