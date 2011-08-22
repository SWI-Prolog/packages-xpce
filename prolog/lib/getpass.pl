/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org/packages/xpce/
    Copyright (C): 1995-2011, University of Amsterdam
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

:- module(getpass,
	  [ getpass/1
	  ]).
:- use_module(library(pce)).
:- require([ between/3
	   , default/3
	   , forall/2
	   ]).

/** <module> Request passwords using XPCE

This module provides getpass/1 as a simple  way to prompt for a password
and  the  class  =passwd_item=,  which  defines  a  text_item  with  ***
feedback.
*/

%%	getpass(-Passwd)
%
%	Asks the user for a password.  Provides feedback as `*' characters
%	The password typed is returned as a Prolog list.  All intermediate
%	results use XPCE strings rather than atoms to avoid finding the
%	typed password by inspecting the Prolog or XPCE symbol-table.

getpass(Pass) :-
	send(new(D, dialog('Enter Password')), append, new(I, passwd_item)),
	send(D, append, button(ok, message(D, return, I?selection))),
	send(D, append, button(cancel, message(D, return, @nil))),
	send(D, default_button, ok),
	(   send(@event, instance_of, event)
	->  get(@event, position, @display, Pointer)
	;   Pointer = @default
	),
	get(D, confirm_centered, Pointer, RVal),
	(   RVal == @nil
	->  send(D, destroy),
	    fail
	;   pce_string_to_list(RVal, RawPass),
	    send(D, destroy),
	    Pass = RawPass
	).

pce_string_to_list(String, List) :-
	get(String, size, Size),
	pce_string_to_list(0, Size, String, List).

pce_string_to_list(N, N, _, []) :- !.
pce_string_to_list(I, N, S, [C|T]) :-
	get(S, character, I, C),
	NI is I + 1,
	pce_string_to_list(NI, N, S, T).


		 /*******************************
		 *	 CLASS PASSWD_ITEM	*
		 *******************************/

:- pce_begin_class(passwd_item, text_item, "text-item for entering a passwd").

variable(shadow,	text_item,	get, "The real (invisible) item").

initialise(I, Name:[name], Message:[message]) :->
	default(Name, password, TheName),
	send(I, send_super, initialise, TheName, string('')),
	send(I, slot, shadow, text_item(TheName, string(''), Message)).


unlink(I) :->
	get(I, shadow, Shadow),
	free(Shadow),
	send(I, send_super, unlink).


event(I, Ev:event) :->
	get(I, shadow, Shadow),
	(   get(Shadow, message, @default),
	    get(Ev, id, 13)
	->  send(I, send_super, event)
	;   send(Shadow, event, Ev),
	    get(Shadow, selection, String),
	    get(Shadow, caret, Caret),
	    get(String, size, Size),
	    make_star_string(Size, Stars),
	    send(I, selection, Stars),
	    send(I, caret, Caret),
	    (   send(Ev, is_a, keyboard)
	    ->  true
	    ;   send(I, send_super, event, Ev)
	    )
	).


selection(I, Passwd:string) :<-
	get(I, shadow, Shadow),
	get(Shadow, selection, Passwd).


make_star_string(Size, S) :-
	new(S, string),
	forall(between(1, Size, _), send(S, append, '*')).

:- pce_end_class.
