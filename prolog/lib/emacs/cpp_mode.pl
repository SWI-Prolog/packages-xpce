/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org/packages/xpce/
    Copyright (c)  1985-2002, University of Amsterdam
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

:- module(emacs_cpp_mode, []).
:- use_module(library(pce)).
:- use_module(library(emacs_extend), []).
:- use_module(c_mode, [c_keyword/2]).

:- multifile
    cpp_keyword/2.

%!  cpp_keyword(?Keyword, ?Type)
%
%   True when Keyword is a C++ keyword (that is not a C keyword) of Type.
%   The types are those of c_keyword/2.

cpp_keyword(try,              control).
cpp_keyword(catch,            control).
cpp_keyword(throw,            control).
cpp_keyword(co_await,         control).
cpp_keyword(co_return,        control).
cpp_keyword(co_yield,         control).
cpp_keyword(bool,             type).
cpp_keyword(auto,             type).
cpp_keyword(wchar_t,          type).
cpp_keyword(char8_t,          type).
cpp_keyword(char16_t,         type).
cpp_keyword(char32_t,         type).
cpp_keyword(class,            definition).
cpp_keyword(namespace,        definition).
cpp_keyword(template,         definition).
cpp_keyword(typename,         definition).
cpp_keyword(using,            definition).
cpp_keyword(concept,          definition).
cpp_keyword(requires,         definition).
cpp_keyword(public,           qualifier).
cpp_keyword(private,          qualifier).
cpp_keyword(protected,        qualifier).
cpp_keyword(virtual,          qualifier).
cpp_keyword(friend,           qualifier).
cpp_keyword(explicit,         qualifier).
cpp_keyword(mutable,          qualifier).
cpp_keyword(constexpr,        qualifier).
cpp_keyword(consteval,        qualifier).
cpp_keyword(constinit,        qualifier).
cpp_keyword(noexcept,         qualifier).
cpp_keyword(thread_local,     qualifier).
cpp_keyword(export,           qualifier).
cpp_keyword(new,              operator).
cpp_keyword(delete,           operator).
cpp_keyword(operator,         operator).
cpp_keyword(this,             operator).
cpp_keyword(nullptr,          operator).
cpp_keyword(true,             operator).
cpp_keyword(false,            operator).
cpp_keyword(decltype,         operator).
cpp_keyword(typeid,           operator).
cpp_keyword(alignas,          operator).
cpp_keyword(alignof,          operator).
cpp_keyword(static_assert,    operator).
cpp_keyword(static_cast,      operator).
cpp_keyword(dynamic_cast,     operator).
cpp_keyword(const_cast,       operator).
cpp_keyword(reinterpret_cast, operator).

%   set_keywords
%
%   Add the C++ keywords to the cpp syntax table.  This table is created
%   as a copy of the c table when the class is realised (see
%   emacs_extend:emacs_mode_bindings/4), so this must run after that.

set_keywords :-
    get(@syntax_tables, member, cpp, ST),
    findall(Kwd, ( c_keyword(Kwd, _)
                 ; cpp_keyword(Kwd, _)
                 ), Kwds0),
    sort(Kwds0, Kwds),
    Obj =.. [vector|Kwds],
    send(ST, keywords, Obj).

:- emacs_begin_mode(cpp, c,
                    "Mode for (XPCE) C++ programs",
                    [
                    ],
                    []).

:- pce_class_directive(set_keywords).

keyword_type(M, Keyword:name, Type:name) :<-
    "Classify a C++ keyword"::
    (   cpp_keyword(Keyword, Type)
    ->  true
    ;   get_super(M, keyword_type, Keyword, Type)
    ).

collect(M, Re:regex, Reg:[int], Result) :<-
    "Collect specified register of regex matches"::
    get(M, text_buffer, TB),
    new(Result, chain),
    send(Re, for_all, TB,
         message(Result, append, ?(@arg1, register_value,
                                   @arg2, Reg, name))),
    send(Result, sort),
    send(Result, unique).

:- emacs_end_mode.
