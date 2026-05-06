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

:- module(test_font, [test_font/0]).
:- encoding(utf8).

/** <module> Tests for xpce font queries (->member, <-domain)

Run with:

    swipl -g test_font -t halt \
          packages/xpce/tests/test_font.pl
*/

:- use_module(library(pce)).
:- use_module(library(plunit)).

setup_headless :-
    set_prolog_flag('SDL_VIDEODRIVER', dummy).
:- initialization(setup_headless, now).

test_font :-
    run_tests([font_member, font_domain]).

emoji(0x1F600).                         % 😀

:- begin_tests(font_member).

test(ascii_default) :-
    new(F, font(sans, normal, 12)),
    send(F, member, 0'a).

test(ascii_main_only) :-
    new(F, font(sans, normal, 12)),
    send(F, member, 0'a, @off).

test(emoji_default_finds_via_fallback) :-
    new(F, font(sans, normal, 12)),
    emoji(C),
    send(F, member, C).

test(emoji_main_only_fails, [fail]) :-
    new(F, font(sans, normal, 12)),
    emoji(C),
    send(F, member, C, @off).

test(emoji_family_explicit) :-
    new(F, font(sans, normal, 12)),
    emoji(C),
    send(F, member, C, @on).

:- end_tests(font_member).

:- begin_tests(font_domain).

test(default_envelope_covers_emoji) :-
    new(F, font(sans, normal, 12)),
    emoji(C),
    get(F, domain, tuple(A, Z)),
    A =< C, C =< Z.

test(family_explicit_matches_default) :-
    new(F, font(sans, normal, 12)),
    get(F, domain, tuple(A1, Z1)),
    get(F, domain, @on, tuple(A2, Z2)),
    A1 == A2, Z1 == Z2.

test(main_only_envelope_excludes_emoji) :-
    new(F, font(sans, normal, 12)),
    emoji(C),
    get(F, domain, @off, tuple(_A, Z)),
    Z < C.

test(domain_consistent_with_member) :-
    %% Outside the family domain, ->member must fail.
    new(F, font(sans, normal, 12)),
    get(F, domain, tuple(_A, Z)),
    Outside is Z + 1,
    ( Outside =< 0x10FFFF
    -> \+ send(F, member, Outside)
    ;  true
    ).

:- end_tests(font_domain).
