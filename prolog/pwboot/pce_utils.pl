/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  1995-2011, University of Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(pce_utils, [
	  pce_error/1,
	  pce_warn/1,
	  pce_info/1,
	  ignore/1,
	  strip_module/3,
	  source_location/2,
	  term_to_atom/2
	]).

:- meta_predicate ignore(:).

:- use_module(library(charsio), [chars_to_stream/2, with_output_to_chars/2]).
:- use_module(library(fromonto), [onto_chars/2]).

%       pce_error(Error)
%       pce_warn(Warning)
%       pce_info(Info)
%       Provide (prolog-part) PCE interface messages

pce_error(Error) :-
	print_message(error, Error).

pce_warn(Warning) :-
	print_message(warning, Warning).

pce_info(Info) :-
	print_message(silent, Info).


%       ignore(+Goal)
%       Call goal once, succeed always

ignore(Goal) :-
	Goal, !.
ignore(_).


%       strip_module(+RawTerm, -Term, -Module).
%       If a term is of the form Module:Term, strip of the module part,
%       return the plain term in Term and the Module in Module.

strip_module(RT, M, T) :-
	strip_module(RT, T, M, user).

strip_module(Module:RT2, T, M, _) :- !,
	strip_module(RT2, T, M, Module).
strip_module(T, T, M, M).


%       source_location(-Path, -LineNo)
%       Unify Path and LineNo with the filename and line number of the
%       location where the last term has been read.  Used inside
%       term_expansion.

source_location(File, Line) :-
	prolog_load_context(file, File),
	prolog_load_context(term_position,
		'$stream_position'(_, Line, _, _, _)).


%	term_to_atom(+Term, -Atom)
%	convert a term to a single atom by characters

term_to_atom(Term, Atom) :-    % convert term to single atom by characters
	copy_term(Term, TempTerm),
	numbervars(TempTerm, 0, _),
	onto_chars(print(TempTerm), Chars),
	atom_chars(Atom, Chars).
