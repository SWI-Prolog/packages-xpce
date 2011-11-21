/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
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


:- module(pce_history, []).
:- use_module(library(pce)).
:- use_module(library(toolbar)).
:- use_module(library(hyper)).

resource(back, image, image('16x16/back.xpm')).
resource(forw, image, image('16x16/forward.xpm')).

:- pce_begin_class(history, object,
		   "Manage a location history").

variable(backward_list,	chain,		     get,  "Backward history").
variable(forward_list,	chain,		     get,  "Forward history").
variable(message,	code*,		     both, "Message executed").
variable(action,	{forward,backward}*, get,  "Current action").
variable(current,	any*,		     get,  "Current location").


initialise(H, Msg:[code]*) :->
	default(Msg, @nil, Message),
	send_super(H, initialise),
	send(H, slot, message, Message),
	send(H, slot, backward_list, new(chain)),
	send(H, slot, forward_list, new(chain)).

:- pce_group(history).

location(H, Loc:any) :->
	"Tell history we go some location"::
	get(H, current, Current),
	(   Current \== @nil
	->  (   get(H, action, backward)
	    ->	send(H?forward_list, prepend, Current)
	    ;	send(H?backward_list, prepend, Current)
	    )
	;   true
	),
	send(H, slot, current, Loc),
	ignore(send(H, send_hyper, button, activate)).

backward(DW, Obj:any) :<-
	"Return previous location"::
	get(DW, backward_list, L),
	get(L, delete_head, Obj).

forward(DW, Obj:any) :<-
	"Return next location"::
	get(DW, forward_list, L),
	get(L, delete_head, Obj).

forward(DW) :->
	"Forward into history"::
	get(DW, forward, Next),
	send(DW, goto, forward, Next).

backward(DW) :->
	"Backward into history"::
	get(DW, backward, Next),
	send(DW, goto, backward, Next).

goto(DW, Dir:{forward,backward}, Obj:any) :->
	get(DW, message, Msg),
	Msg \== @nil,
	send(DW, slot, action, Dir),
	call_cleanup(send(Msg, forward, Obj),
		     send(DW, slot, action, @nil)).

can_backward(H) :->
	"Test whether there is backward history available"::
	\+ send(H?backward_list, empty).

can_forward(H) :->
	"Test whether there is forward history available"::
	\+ send(H?forward_list, empty).

:- pce_group(gui).

button(DW, Dir:{forward,backward}, B:tool_button) :<-
	"Create tool-buttons for manipulating the history"::
	(   Dir == forward
	->  Img = forw,
	    Tag = forward_history
	;   Img = back,
	    Tag = backward_history
	),
	atom_concat(can_, Dir, Can),
	new(B, tool_button(message(DW, Dir),
			   image(resource(Img)),
			   Tag,
			   message(DW, Can))),
	new(_, partof_hyper(DW, B, button, history)).
			   
:- pce_end_class(history).

