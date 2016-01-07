/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  1996-2011, University of Amsterdam
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

:- module(pce_host, []).
:- use_module(language(pce_messages)).
:- use_module(library(strings), [atomic_list_concat/2]).

pwversion('3.2').

:- multifile 'QU_messages':generate_message/3.
:- multifile 'QU_messages':context_type/3.

'QU_messages':generate_message(Spec, Out, Tail) :-
	pce_message(Spec, Out, Tail).
'QU_messages':context_type(Spec, Out, Tail) :-
	pce_message_context(Spec, Out, Tail).

:- initialization
	user:asserta((term_expansion(:-(require(Preds)),
				     :-(require:require(Module:Preds))):-
		     prolog_load_context(module, Module), !)).

:- multifile user:file_search_path/2.
:- dynamic   user:file_search_path/2.

user:file_search_path(pce, quintus(Dir)) :-
	pwversion(PwVersion),
	atomic_list_concat([prowindows, PwVersion], Dir).
user:file_search_path(contrib, 	pce(contrib)).

		 /*******************************
		 *	    C-EXCEPTIONS	*
		 *******************************/

pce_error(E) :-
	print_message(error, E).

:- extern(pce_error(+term)).

		 /*******************************
		 *	      HELP HOOK		*
		 *******************************/

:- multifile user:user_help/0.
:- dynamic user:user_help/0.

user:user_help :-
	(   pce_qux:callable_predicate(manpce)
	->  true
	;   pce_qux:pce_info(loading(manpce))
	),
	pce_qux:auto_call(manpce),
	(   predicate_property(qui:init_qui, _) % dubious
	->  pce_qux:xpce_loop
	;   true
	).

		 /*******************************
		 *	       ABOUT		*
		 *******************************/

about('ProWindows version %s'+[Version], boldhuge) :-
	pwversion(Version).
about('Based on', italic).
about('XPCE version %s'+[@pce?version], huge).
about('Copyright 1992-1998, SICS / University of Amsterdam', normal).
about('Contact:', italic).
about('Swedish Institute of Computer Science', huge).
about('P.O. Box 1263\nSE-164 29 Kista\nSweden', normal).
about('Tel: +46-8-752-1500, Fax: +46-8-751-7230', normal).
about('Support: qpsupport@sics.se', normal).
about('Sales: qpsales@sics.se', normal).
about('WWW:', italic).
about('http://www.sics.se/quintus/', normal).

		 /*******************************
		 *	     PROPERTIES		*
		 *******************************/

property(prolog(quintus)).
property(file_extensions([qof, pl])).
property(repeat_meta_declaraction).
property(need_extern_declaration).
property(use_predicate_references).
property(register_source_locations).
property(system_source_prefix(Package)) :-
	pwversion(Version),
	atomic_list_concat(['/prowindows', Version, '/'], Package).


		/********************************
		*       REINITIALISATION	*
		********************************/

pce_load_rcfile :-
	absolute_file_name('~/.pwrc', [access(read), file_errors(fail)], F),
	user:ensure_loaded(F), !.
pce_load_rcfile.

:- initialization
	pce_load_rcfile.
