/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org/projects/xpce/
    Copyright (C): 2011 VU University Amsterdam

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

:- module(emacs_turtle_mode, []).
:- use_module(library(pce)).
:- use_module(library(emacs_extend)).
:- use_module(sgml_mode).
:- use_module(library(rdf)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_turtle)).

:- pce_autoload(rdf_diagram, library(rdf_diagram)).

:- emacs_begin_mode(
       turtle, language,
       "Mode for editing Turtle documents",
       [ -		     = button(turtle),
	 show_diagram	     = button(turtle),
	 rdf_make	     = key('\\C-c\\C-m') + button(compile),
	 rdf_load	     = key('\\C-c\\C-b') + button(compile),
	 open_document	     = button(turtle)
       ],
       []).

open_document(M) :->
	"Insert document header"::
	send(M, format,
	     '@prefix  rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .\n\c
	      @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .\n\c
	      @prefix  owl: <http://www.w3.org/2002/07/owl#> .\n\c
	      \n').

show_diagram(M) :->
	"Show diagram of file"::
	get(M, text_buffer, TB),
	setup_call_cleanup(
	    pce_open(TB, read, In),
	    rdf_read_turtle(stream(In), Triples, []),
	    close(In)),
	new(D, rdf_diagram(string('RDF triple diagram'))),
	send(new(report_dialog), below, D),
	send(D, triples, Triples),
	send(D, open).

rdf_make(M) :->
	"Run rdf_make/0"::
	send(@emacs, save_some_buffers),
	rdf_make,
	send(M, report, status, 'RDF Make done').

rdf_load(M) :->
	"Run rdf_load on the file"::
	get(M?text_buffer, file, File),
	(   send(File, instance_of, file)
	->  send(M, save_if_modified),
	    get(File, name, Path),
	    rdf_load(Path),
	    send(M, report, status, '%s loaded', Path)
	;   send(M, report, error,
		 'Buffer is not connected to a file')
	).

:- emacs_end_mode.



