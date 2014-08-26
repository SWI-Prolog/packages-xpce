/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org/projects/xpce/
    Copyright (C): 2011-2013, VU University Amsterdam

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

:- module(emacs_javascript_mode, []).
:- use_module(library(pce)).

:- emacs_begin_mode(javascript, c,
		    "Mode for JavaScript programs",
		    [
		    ],
		    []).

/* Seems JavaScript works best by simply indenting relative to
   the first non-comment/blank on the line.
*/

back_skip_if_etc(E, Pos:int, Start:int) :<-
	"Find indent for stuff before {"::
	get(E, scan, Pos, line, 0, start, SOL),
	get(E, scan, Pos, line, 0, end, EOL),
	get(E, skip_comment, SOL, EOL, Start).

:- emacs_end_mode.
