/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org
    Copyright (c)  2026, SWI-Prolog Solutions b.v.
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

:- module(test_font_alias, [test_font_alias/0]).

/** <module> Test completion of partially defined font class variables

A user that overrules font.system_fonts or font.pango_families in a
Defaults file normally only mentions the entries they want to change.
The ones they leave out must keep their built-in value.

Both tables are filled once per process, from a file that is read
before the first class variable is looked up.  The tests therefore work
on what a child process, started on a Defaults file we write, reports
about them.

Run with:

    swipl -g test_font_alias -t halt \
          packages/xpce/tests/test_font_alias.pl
*/

:- use_module(library(pce)).
:- use_module(library(plunit)).
:- use_module(library(process)).

test_font_alias :-
    run_tests([font_alias]).

		 /*******************************
		 *	     THE CHILD		*
		 *******************************/

%!  partial_defaults(-Text) is det.
%
%   Defaults file that defines one new entry and redefines one existing
%   entry for both font class variables, leaving all others out.

partial_defaults(Text) :-
    atomic_list_concat(
        [ 'font.system_fonts:   [ normal := font(serif, normal, 20), \\',
          '                       myown  := font(serif, italic, 11)  \\',
          '                     ]',
          'font.pango_families: [ sans  := \'My Sans\', \\',
          '                       myown := \'My Own\'  \\',
          '                     ]',
          ''
        ], '\n', Text).

%!  report_tables is det.
%
%   Goal for the child: print the font alias and family tables as
%   alias/4 and family/2 terms.

report_tables :-
    get(@pce, convert, normal, font, _),        % fills both tables
    send(@font_aliases, for_all,
         message(@prolog, report_alias, @arg1, @arg2)),
    send(@font_families, for_all,
         message(@prolog, report_family, @arg1, @arg2)).

report_alias(Name, Font) :-
    get(Font, family, Family),
    get(Font, style, Style),
    get(Font, points, Points),
    format("~q.~n", [alias(Name, Family, Style, Points)]).

report_family(Name, Pango) :-
    format("~q.~n", [family(Name, Pango)]).

%!  font_tables(-Terms) is det.
%
%   Terms describing the font tables of a child that read our partial
%   Defaults file.  Computed once.

:- dynamic font_tables_cache/1.

font_tables(Terms) :-
    font_tables_cache(Terms),
    !.
font_tables(Terms) :-
    setup_call_cleanup(
        tmp_file_stream(text, Defaults, Out),
        ( partial_defaults(Text),
          write(Out, Text),
          close(Out),
          child_tables(Defaults, Terms)
        ),
        delete_file(Defaults)),
    assertz(font_tables_cache(Terms)).

child_tables(Defaults, Terms) :-
    current_prolog_flag(executable, Exe),
    module_property(test_font_alias, file(Me)),
    atom_concat('-Dxpce_defaults=', Defaults, DefaultsOpt),
    setup_call_cleanup(
        process_create(Exe,
                       [ '-q', DefaultsOpt,
                         '-g', 'test_font_alias:report_tables',
                         '-t', halt, Me
                       ],
                       [ stdout(pipe(Pipe)),
                         environment(['SDL_VIDEODRIVER'=dummy]),
                         process(PID)
                       ]),
        read_terms(Pipe, Terms),
        ( close(Pipe),
          process_wait(PID, exit(0))
        )).

read_terms(Pipe, Terms) :-
    read_term(Pipe, T, []),
    (   T == end_of_file
    ->  Terms = []
    ;   Terms = [T|Rest],
        read_terms(Pipe, Rest)
    ).

		 /*******************************
		 *	       TESTS		*
		 *******************************/

%!  builtin_alias(?Alias)
%
%   The aliases class font itself defines.  See rc_font in font.c.

builtin_alias(normal).
builtin_alias(bold).
builtin_alias(italic).
builtin_alias(small).
builtin_alias(large).
builtin_alias(boldlarge).
builtin_alias(huge).
builtin_alias(boldhuge).
builtin_alias(fixed).
builtin_alias(tt).
builtin_alias(boldtt).
builtin_alias(itt).
builtin_alias(bitt).

:- begin_tests(font_alias).

test(all_builtin_aliases_defined, Missing == []) :-
    font_tables(Terms),
    findall(A, ( builtin_alias(A),
                 \+ memberchk(alias(A,_,_,_), Terms)
               ), Missing).

test(omitted_alias_uses_builtin, Alias == alias(tt, mono, normal, 14)) :-
    font_tables(Terms),
    memberchk(alias(tt, Family, Style, Points), Terms),
    Alias = alias(tt, Family, Style, Points).

test(redefined_alias_wins, Alias == alias(normal, serif, normal, 20)) :-
    font_tables(Terms),
    memberchk(alias(normal, Family, Style, Points), Terms),
    Alias = alias(normal, Family, Style, Points).

test(added_alias_defined, Alias == alias(myown, serif, italic, 11)) :-
    font_tables(Terms),
    memberchk(alias(myown, Family, Style, Points), Terms),
    Alias = alias(myown, Family, Style, Points).

test(omitted_family_uses_builtin) :-
    font_tables(Terms),
    memberchk(family(serif, Pango), Terms),
    Pango \== 'My Sans'.

test(redefined_family_wins) :-
    font_tables(Terms),
    memberchk(family(sans, 'My Sans'), Terms).

test(added_family_defined) :-
    font_tables(Terms),
    memberchk(family(myown, 'My Own'), Terms).

:- end_tests(font_alias).
