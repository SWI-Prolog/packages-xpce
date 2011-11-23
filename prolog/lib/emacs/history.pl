/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org/packages/xpce/
    Copyright (C): 2011, VU University Amsterdam

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

:- module(emacs_history, []).
:- use_module(library(pce)).

		 /*******************************
		 *	      HISTORY		*
		 *******************************/

:- pce_begin_class(emacs_history_entry, object,
		   "Entry in the PceEmacs history").

source_location(HE, Loc:source_location) :<-
	"Compute location"::
	get(HE, get_hyper, fragment, source_location, Loc).

unlink(HE) :->
	"Remove from history"::
	send(@emacs?history, delete, HE),
	send_super(HE, unlink).

print_name(HR, Label:char_array) :<-
	"Get human-readable representation of the entry"::
	get(HR, get_hyper, fragment, print_name, Label).

equal(HE, HE2:emacs_history_entry) :->
	"True if both entries start and the same location"::
	get(HE, get_hyper, fragment, text_buffer, TB),
	get(HE2, get_hyper, fragment, text_buffer, TB),
	get(HE, get_hyper, fragment, start, Start),
	get(HE2, get_hyper, fragment, start, Start).

:- pce_end_class.

		 /*******************************
		 *	     FRAGMENT		*
		 *******************************/

:- pce_begin_class(emacs_history_fragment, fragment,
		   "Location in the PceEmacs history").

variable(title,	char_array*, get, "Title for the menu item").

initialise(HF, TB:text_buffer, Start:int, Len:[int], Title:[char_array]) :->
	"Add fragment to PceEmacs history"::
	(   Len == @default
	->  get(TB, scan, Start, line, 0, end, End),
	    FLen is End+1-Start
	;   FLen = Len
	),
	send_super(HF, initialise, TB, Start, FLen, history),
	default(Title, @nil, TheTitle),
	send(HF, slot, title, TheTitle),
	new(HE, emacs_history_entry),
	new(_, mutual_dependency_hyper(HF, HE, history, fragment)),
	send(@emacs?history, location, HE).

source_location(HF, Loc:source_location) :<-
	"Compute location"::
	get(HF, text_buffer, TB),
	get(TB, line_number, HF?start, Line),
	get(TB?file, name, File),
	new(Loc, source_location(File, Line)).

print_name(HF, Label:char_array) :<-
	"Get human-readable representation of the entry"::
	(   get(HF, title, Title), Title \== @nil
	->  Label = Title
	;   get(HF, string, Label),
	    send(Label, strip),
	    get(Label, size, Size),
	    Size > 0
	->  true
	;   get(HF, source_location, source_location(File, Line)),
	    file_base_name(File, FileBase),
	    new(Label, string('%s:%d', FileBase, Line))
	).

:- pce_end_class.

