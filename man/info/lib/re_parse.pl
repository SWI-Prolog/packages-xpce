/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org/packages/xpce/
    Copyright (c)  2010, University of Amsterdam
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

:- module(re_parse,
	  [ re_parse_loop/5
	  ]).


:- meta_predicate
	re_parse_loop(+, +, +, :, +),
	parse_line(+, +, +, :).

%	re_parse_loop(+File, +RegExVar, +ActionVar, +Generator, +EndRegEx)

re_parse_loop(File, ReVar, AVar, PatternGenerator, End) :-
	regex(End, EndRe),
	repeat,
	    (	get(File, read_line, L)
	    ->	(   EndRe \== @nil,
		    send(EndRe, match, L)
		->  !
		;   (	parse_line(L, ReVar, AVar, PatternGenerator)
		    ->	fail		% force next line
		    ;   send(File, report, warning,
			     'Failed to handle line ``%s''''', L),
		        fail
		    )
		)
	    ;	!,
		send(File, close)
	    ).


regex(@nil, @nil) :- !.
regex(String, Regex) :-
	get(string(String), value, Expanded),
	new(Regex, regex(Expanded)).


parse_line(Line, ReVar, AVar, PatternGenerator) :-
	PatternGenerator,
	regex(ReVar, Re),
	send(Re, match, Line),
	pattern_action(Re, Line, AVar, Goal),
	Goal, !.


pattern_action(Re, L, Template, Goal) :-
	functor(Template, Name, Arity),
	functor(Goal,     Name, Arity),
	End is Arity + 1,
	pattern_action(1, End, Re, L, Template, Goal).


pattern_action(N, N, _, _, _, _) :- !.
pattern_action(N, Arity, Re, L, Template, Goal) :-
	arg(N, Template, Arg),
	(   integer(Arg)
	->  get(Re, register_value, L, Arg, Value),
	    arg(N, Goal, Value)
	;   nonvar(Arg),
	    Arg = Index:Type
	->  get(Re, register_value, L, Index, RawValue),
	    get(@pce, convert, RawValue, Type, Value),
	    arg(N, Goal, Value)
	;   arg(N, Goal, Arg)
	),
	NN is N + 1,
	pattern_action(NN, Arity, Re, L, Template, Goal).



