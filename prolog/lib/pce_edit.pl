/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org/packages/xpce/
    Copyright (C): 1985-2014, University of Amsterdam
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

:- module(pce_edit,
	  [ editpce/1
	  ]).
:- use_module(library(pce)).

/** <module> Find and edit the source location of an XPCE object
*/

%%	editpce(+Spec)
%
%	Edit an xpce `object' from Spec using PceEmacs. Spec is one of:
%
%	  - An xpce object that implements <-source
%	  - An xpce object, taking its <-class
%	  - The name of a class
%	  - A term Object->selector
%	  - A term Object<-selector
%
%	@see	edit/1 provides the same functionality.

editpce(Spec) :-
	in_pce_thread(editpce_sync(Spec)).

editpce_sync(Spec) :-
	method(Spec, Obj),
	(   get(Obj, source, Location),
	    Location \== @nil
	->  use_module(library(pce_emacs)),
	    Goal = start_emacs, Goal,	% fool xref
	    send(@emacs, goto_source_location, Location)
	;   send(Obj, report, warning, 'Can''t find source')
	).


method(Object, Object) :-
	object(Object),
	send(Object, has_get_method, source), !.
method(Object, Class) :-
	object(Object), !,
	get(Object, class, Class).
method(ClassName, Class) :-
	atom(ClassName), !,
	get(@pce, convert, ClassName, class, Class).
method(->(Receiver, Selector), Method) :- !,
	(   atom(Receiver)
	->  get(@pce, convert, Receiver, class, Class),
	    get(Class, send_method, Selector, Method)
	;   object(Receiver)
	->  get(Receiver, send_method, Selector, tuple(_, Method))
	).
method(<-(Receiver, Selector), Method) :- !,
	(   atom(Receiver)
	->  get(@pce, convert, Receiver, class, Class),
	    get(Class, get_method, Selector, Method)
	;   object(Receiver)
	->  get(Receiver, get_method, Selector, tuple(_, Method))
	).


