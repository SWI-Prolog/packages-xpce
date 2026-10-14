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

:- module(test_refman_signatures, [test_refman_signatures/0]).

/** <module> Check the reference manual cards against the classes

Each card in the markdown files of man/refmanual/md starts with a line
such as

    - regex->initialise: pattern=[char_array], case_sensitive=[bool]
    - regex<-match: in=char_array, start=[int] -> length=int
    - regex<-syntax: {basic,extended,advanced}

This test verifies that the signature on such a line is the signature
of the live behaviour, so a change to a method's arguments that is not
reflected in the manual is noticed.

Run with:

    swipl -g test_refman_signatures -t halt \
          packages/xpce/tests/test_refman_signatures.pl
*/

:- use_module(library(pce)).
:- use_module(library(plunit)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(readutil)).
:- use_module(library(pcre)).

test_refman_signatures :-
    run_tests([ refman_signatures
              ]).

:- prolog_load_context(directory, Dir),
   asserta(test_dir(Dir)).

md_dir(MdDir) :-
    test_dir(Dir),
    directory_file_path(Dir, '../man/refmanual/md', MdDir0),
    absolute_file_name(MdDir0, MdDir, [file_type(directory)]).

md_file(File) :-
    md_dir(Dir),
    directory_files(Dir, Entries),
    msort(Entries, Sorted),
    member(File, Sorted),
    file_name_extension(_, md, File).

%!  card_errors(+File, -Errors) is det.
%
%   Errors is a list of messages =|File:Line: Message|= for cards in
%   File whose signature differs from the live behaviour.  A card in
%   the "Instance variables" section describes the variable, also if a
%   method with the same name overrules it.

card_errors(File, Errors) :-
    md_dir(Dir),
    directory_file_path(Dir, File, Path),
    read_file_to_string(Path, Text, []),
    split_string(Text, "\n", "", Lines),
    card_errors(Lines, 1, behaviour, File, Errors).

card_errors([], _, _, _, []).
card_errors([String|T], Line, Section0, File, Errors) :-
    (   sub_string(String, 0, _, _, "#")
    ->  section(String, Section)
    ;   Section = Section0
    ),
    (   card_error(String, Section, Message)
    ->  format(string(Error), "~w:~w: ~s", [File, Line, Message]),
        Errors = [Error|Errors1]
    ;   Errors = Errors1
    ),
    Line1 is Line+1,
    card_errors(T, Line1, Section, File, Errors1).

section(Heading, variable) :-
    sub_string(Heading, 0, _, _, "## Instance variables"),
    !.
section(_, behaviour).

card_error(String, Section, Error) :-
    re_matchsub("^- (?<c>\\w+)(?<a><->|->|<-|-)(?<m>\\w+): ?(?<s>.*)$",
                String, Card, []),
    atom_string(ClassName0, Card.c),
    atom_string(Arrow, Card.a),
    atom_string(Selector, Card.m),
    normalize_space(string(Documented), Card.s),
    class_name(ClassName0, ClassName),
    (   get(@pce, convert, ClassName, class, Class)
    ->  (   live(Section, Class, Arrow, Selector, Live)
        ->  \+ memberchk(Documented, Live),
            Live = [Signature|_],
            format(string(Error), "~w~w~w: ~s, should be ~s",
                   [ClassName0, Arrow, Selector, Documented, Signature])
        ;   format(string(Error), "~w~w~w: no such ~w",
                   [ClassName0, Arrow, Selector, Section])
        )
    ;   format(string(Error), "~w: no such class", [ClassName0])
    ).

%   Classes whose name is not a valid word in a card.

class_name(binding, :=) :- !.
class_name(obtain, ?) :- !.
class_name(Name, Name).

%!  live(+Section, +Class, +Arrow, +Selector, -Signatures) is semidet.
%
%   Signatures are the accepted ways to write the signature of
%   Class<Arrow>Selector, the preferred one first.  A get method
%   without arguments may be written as `-> Type` or as `Type`.  The
%   arrow of a variable card is not checked: a variable's access is
%   often extended by a method with the same name.

live(variable, Class, Arrow, Selector, Sigs) :-
    !,
    get(Class, instance_variable, Selector, Var),
    type_signature(Var, Sig),
    (   Arrow == '<-'
    ->  string_concat("-> ", Sig, Alt),
        Sigs = [Sig, Alt]
    ;   Sigs = [Sig]
    ).
live(behaviour, Class, '->', Selector, [Sig]) :-
    get(Class, send_method, Selector, Impl),
    !,
    (   send(Impl, instance_of, variable)
    ->  type_signature(Impl, Sig)
    ;   argument_signature(Impl, Sig)
    ).
live(behaviour, Class, '<-', Selector, Sigs) :-
    get(Class, get_method, Selector, Impl),
    !,
    (   send(Impl, instance_of, variable)
    ->  type_signature(Impl, Sig),
        string_concat("-> ", Sig, Alt),
        Sigs = [Sig, Alt]
    ;   argument_signature(Impl, Args),
        get(Impl, return_type, Type),
        get(Type, fullname, Return),
        (   Args == ""
        ->  format(string(Sig), "-> ~w", [Return]),
            atom_string(Return, Alt),
            Sigs = [Sig, Alt]
        ;   format(string(Sig), "~s -> ~w", [Args, Return]),
            Sigs = [Sig]
        )
    ).
live(behaviour, Class, Arrow, Selector, Sigs) :-
    memberchk(Arrow, ['<->', '-']),
    live(variable, Class, Arrow, Selector, Sigs).

type_signature(Var, Sig) :-
    get(Var, type, Type),
    get(Type, fullname, Name),
    atom_string(Name, Sig).

argument_signature(Method, Sig) :-
    get(Method, types, Types),
    get(Types, size, Size),
    findall(Name,
            ( between(1, Size, I),
              get(Types, element, I, Type),
              get(Type, fullname, Name)
            ),
            Names),
    atomic_list_concat(Names, ', ', Atom),
    atom_string(Atom, Sig).

:- begin_tests(refman_signatures).

test(signatures, [forall(md_file(File)), Errors == []]) :-
    card_errors(File, Errors).

:- end_tests(refman_signatures).
