/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (C): 1985-2012, University of Amsterdam
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

:- module(emacs_rdf_mode, []).
:- use_module(library(pce)).
:- use_module(library(emacs_extend)).
:- use_module(sgml_mode).
:- use_module(library(rdf)).
:- use_module(library(semweb/rdf_db)).

:- pce_autoload(rdf_diagram, library(rdf_diagram)).

:- emacs_begin_mode(
       rdf, xml,
       "Mode for editing RDF documents",
       [ -			     = button(sgml),
	 show_diagram	     = button(sgml),
	 rdf_make	     = key('\\C-c\\C-m') + button(compile),
	 rdf_load	     = key('\\C-c\\C-b') + button(compile)
       ],
       []).

open_document(M) :->
	"Insert document header"::
	send(M, format,
	     '<?xml version="1.0" encoding="iso-8859-1"?>\n\n\c
	      <!DOCTYPE rdf [\n  \c
	      <!ENTITY rdf  "http://www.w3.org/1999/02/22-rdf-syntax-ns#">\n  \c
              <!ENTITY xsd  "http://www.w3.org/2000/10/XMLSchema#">\n\c
              ]>\n\n\c
	      <rdf:RDF\n  \c
	      xmlns:rdf ="&rdf;"\n  \c
	      xmlns:xsd ="&xsd;"\n\c
	      >\n\n\c
	      </rdf:RDF>\n').

show_diagram(M) :->
	"Show diagram of file"::
	get(M, text_buffer, TB),
	pce_open(TB, read, In),
	load_rdf(stream(In), Triples,
		 [ expand_foreach(true)
		 ]),
	close(In),
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


:- emacs_begin_mode(rdfs, rdf,
		    "Mode for editing RDFS documents",
		    [],
		    []).

open_document(M) :->
	"Insert document header"::
	send(M, format,
	     '<?xml version="1.0" encoding="iso-8859-1"?>\n\n\c
	      <!DOCTYPE rdfs [\n  \c
	      <!ENTITY rdf  "http://www.w3.org/1999/02/22-rdf-syntax-ns#">\n  \c
	      <!ENTITY rdfs "http://www.w3.org/2000/01/rdf-schema#">\n  \c
              <!ENTITY xsd  "http://www.w3.org/2000/10/XMLSchema#">\n\c
              ]>\n\n\c
	      <rdf:RDF\n  \c
	      xmlns:rdf ="&rdf;"\n  \c
	      xmlns:rdfs="&rdfs;"\n  \c
	      xmlns:xsd ="&xsd;"\n\c
	      >\n\n\c
	      </rdf:RDF>\n').

:- emacs_end_mode.



:- emacs_begin_mode(owl, rdfs,
		    "Mode for editing OWL documents",
		    [],
		    []).

open_document(M) :->
	"Insert document header"::
	send(M, format,
	     '<?xml version="1.0" encoding="iso-8859-1"?>\n\n\c
	      <!DOCTYPE owl [\n  \c
	      <!ENTITY rdf  "http://www.w3.org/1999/02/22-rdf-syntax-ns#">\n  \c
	      <!ENTITY rdfs "http://www.w3.org/2000/01/rdf-schema#">\n  \c
	      <!ENTITY owl  "http://www.w3.org/2002/7/owl#">\n  \c
              <!ENTITY xsd  "http://www.w3.org/2000/10/XMLSchema#">\n  \c
	      <!ENTITY dc   "http://purl.org/dc/elements/1.1/">\n\c
              ]>\n\n\c
	      <rdf:RDF\n  \c
	      xmlns:rdf ="&rdf;"\n  \c
	      xmlns:rdfs="&rdfs;"\n  \c
	      xmlns:owl ="&owl;"\n  \c
	      xmlns:xsd ="&xsd;"\n  \c
	      xmlns:dc  ="&dc;"\n\c
	      >\n\n\c
	      <Ontology rdf:about="">\n\c
	      </Ontology>\n\n\c
	      </rdf:RDF>\n').

:- emacs_end_mode.



