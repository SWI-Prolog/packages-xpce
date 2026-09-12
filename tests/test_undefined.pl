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


:- module(test_undefined, [test_undefined/0]).
:- encoding(utf8).

/** <module> No XPCE library calls a predicate that is not there

A method body is compiled like any other clause, so a helper predicate
that is deleted along with the class it sat next to goes unnoticed until
somebody picks that menu item.  Refactoring class hierarchies is exactly
when that happens, and it happened four times over while class
pane_frame was being given the work of emacs_frame and epilog_frame.

This walks the code of every module defined under packages/xpce and
fails if it reaches a predicate that is neither defined nor autoloadable.

Run with:

    swipl -g test_undefined -t halt \
          packages/xpce/tests/test_undefined.pl
*/

:- set_prolog_flag('SDL_VIDEODRIVER', dummy).

:- use_module(library(pce)).
:- use_module(library(plunit)).
:- use_module(library(prolog_codewalk)).
:- use_module(library(lists), [member/2, subtract/3]).

%  The libraries to walk.  Loading them is most of what this test does:
%  between them they pull in every mode of PceEmacs and the whole of
%  Epilog.

:- use_module(library(epilog)).
:- use_module(library(emacs/emacs)).
:- use_module(library(pane_frame)).

test_undefined :-
    run_tests([ undefined ]).

:- dynamic undef/2.

collect_undef(To, _Caller, From) :-
    To = M:Head,
    functor(Head, Name, Arity),
    (   undef(M:Name/Arity, _)
    ->  true
    ;   assertz(undef(M:Name/Arity, From))
    ).

%!  xpce_module(+Module) is semidet.
%
%   True when Module is defined by a file of this package.  Every other
%   module the walk reaches is somebody else's business.

xpce_module(M) :-
    module_property(M, file(File)),
    sub_atom(File, _, _, _, '/xpce/').

%!  undefined_in_xpce(-Undefined) is det.
%
%   Every predicate this package's code calls that is neither defined nor
%   autoloadable, less the ones that are meant to be that way.
%
%   A plain predicate rather than the body of the test: a dynamic
%   predicate asserted from inside a plunit unit lands in the module
%   plunit made for that unit, not in this one, and the test would then
%   read an empty table and pass whatever the code says.

undefined_in_xpce(Undefined) :-
    retractall(undef(_,_)),
    prolog_walk_code([ undefined(trace),
                       on_trace(test_undefined:collect_undef),
                       module_class([library])
                     ]),
    findall(PI,
            ( undef(PI, _),
              PI = M:_,
              xpce_module(M)
            ),
            Found0),
    sort(Found0, Found),
    findall(PI, known_undefined(PI), Known0),
    sort(Known0, Known),
    subtract(Found, Known, Undefined).

%!  known_undefined(?PI) is nondet.
%
%   References that are meant to be there.  The first two are reached only
%   if the library that defines them is loaded first, which is what
%   auto_call/1 and the autoloader are for.  The last is only there if the
%   documentation is installed.

known_undefined(emacs_fundamental_mode:editpce/1).  % auto_call/1, pce_debug
known_undefined(epilog:attach_terminal/4).          % library(editline)
% library(help) is only installed if the documentation is built.  See
% the use of ``INSTALL_DOCUMENTATION`` in src/CMakeLists.txt.
known_undefined(emacs_language_mode:help/1) :-
    \+ exists_source(library(help)).

:- begin_tests(undefined).

test(no_library_calls_a_predicate_that_is_not_there, Undefined == []) :-
    undefined_in_xpce(Undefined).

:- end_tests(undefined).
