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

:- module(emacs_debug_modes, []).
:- use_module(library(pce)).
:- use_module(library(pce_template)).

/** <module> Support for embedding PceEmacs into the debugger
*/

:- pce_begin_class(prolog_debug_methods, template,
		   "Mode extensions for running under the debugger").

save_text(M) :->
	"Switch to non-edit mode after saving the buffer"::
	send_super(M, save_text),
	send(M?editors, for_all,
	     if(message(@arg1?window, instance_of, prolog_source_view),
		message(@arg1?window, edit, @off))).

quit(M) :->
	"Destroy the editor"::
	ignore(send(M?text_buffer, save_if_modified)),
	(   get(M, frame, Frame),
	    send(Frame, has_send_method, quitted)
	->  send(Frame, quitted, @on),
	    send(Frame, nodebug)
	;   send_super(M, quit)
	).

:- pce_end_class.



