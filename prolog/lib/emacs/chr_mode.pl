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

:- module(emacs_chr_mode, []).
:- use_module(library(pce)).
:- use_module(prolog_mode).
:- use_module(library(operators)).	% push/pop operators
:- use_module(library(chr)).		% get CHR operators.


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module deals with colourisation of  .chr files. CHR introduces many
operators and requires different rules for colouring objects.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


		 /*******************************
		 *	      CHR MODE		*
		 *******************************/

:- emacs_begin_mode(chr, prolog,
		    "Mode for editing Constraint Handling Rules (CHR) documents",
		    % BINDINGS
		    [
		    ],
		    % SYNTAX TABLE
		    [
		    ]).

colourise_buffer(M) :->
	"Cross-reference the buffer and set up colours"::
	push_chr_operators,
	call_cleanup(send_super(M, colourise_buffer),
		     pop_chr_operators).

:- emacs_end_mode.


		 /*******************************
		 *	   SYNTAX HOOKS		*
		 *******************************/

:- multifile
	emacs_prolog_mode:alternate_syntax/3.


emacs_prolog_mode:alternate_syntax(chr,
				   emacs_chr_mode:push_chr_operators,
				   emacs_chr_mode:pop_chr_operators).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Note that we could generalise this to deal with all included files.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- dynamic
	chr_operators/1.

push_chr_operators :-
	(   chr_operators(Ops)
	->  true
	;   init_chr_operators(Ops),
	    assert(chr_operators(Ops))
	),
	'$set_source_module'(SM, SM),
	push_operators(SM:Ops).

pop_chr_operators :-
	pop_operators.

init_chr_operators(Ops) :-
	absolute_file_name(library('chr/chr_op'),
			   [ file_type(prolog),
			     access(read),
			     file_errors(fail)
			   ],
			   OpFile),
	open(OpFile, read, In),
	read(In, Term),
	read_ops(Term, In, Ops),
	close(In).

read_ops(end_of_file, _, []) :- !.
read_ops((:- op(Pre, Ass, Ops)), In, [ op(Pre, Ass, Ops) |T]) :- !,
	read(In, T2),
	read_ops(T2, In, T).
read_ops(_, In, Ops) :-
	read(In, T2),
	read_ops(T2, In, Ops).
