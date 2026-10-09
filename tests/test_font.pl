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
    run_tests([font_member, font_domain, font_fixed_width, mono_font,
               font_reload]).

emoji(0x1F600).                         % 😀

%   The emoji tests assume the sans font has no emoji and another
%   installed font has, which is the normal situation on Linux.  This
%   need not be the case elsewhere, e.g., under Wine.

emoji_by_fallback :-
    emoji(C),
    new(F, font(sans, normal, 12)),
    \+ send(F, member, C, @off),
    send(F, member, C).

:- begin_tests(font_member).

test(ascii_default) :-
    new(F, font(sans, normal, 12)),
    send(F, member, 0'a).

test(ascii_main_only) :-
    new(F, font(sans, normal, 12)),
    send(F, member, 0'a, @off).

test(emoji_default_finds_via_fallback, [condition(emoji_by_fallback)]) :-
    new(F, font(sans, normal, 12)),
    emoji(C),
    send(F, member, C).

test(emoji_main_only_fails, [fail]) :-
    new(F, font(sans, normal, 12)),
    emoji(C),
    send(F, member, C, @off).

test(emoji_family_explicit, [condition(emoji_by_fallback)]) :-
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

test(main_only_envelope_excludes_emoji, [condition(emoji_by_fallback)]) :-
    new(F, font(sans, normal, 12)),
    emoji(C),
    get(F, domain, @off, tuple(_A, Z)),
    Z < C.

test(domain_consistent_with_member, Members == []) :-
    %% Outside the family domain, ->member must fail.  Probe all code
    %% points just above the domain and every 256th beyond.
    new(F, font(sans, normal, 12)),
    get(F, domain, tuple(_A, Z)),
    Z1 is Z+1,
    findall(C, ( between(Z1, 0x10FFFF, C),
                 ( C - Z =< 0x20000 -> true ; C mod 0x100 =:= 0 ),
                 send(F, member, C)
               ), Members).

:- end_tests(font_domain).

:- begin_tests(font_fixed_width).

test(mono, [W == @on]) :-
    new(F, font(mono, normal, 12)),
    get(F, fixed_width, W).
test(sans, [W == @off]) :-
    new(F, font(sans, normal, 12)),
    get(F, fixed_width, W).

:- end_tests(font_fixed_width).

:- begin_tests(mono_font).

mono(Spec) :-
    get(@pce, convert, Spec, mono_font, _).

test(mono_font) :-
    mono(font(mono, normal, 12)).
test(alias) :-
    mono(fixed).
test(proportional, [fail]) :-
    mono(font(sans, normal, 12)).
test(proportional_alias, [fail]) :-
    mono(normal).

:- end_tests(mono_font).


%   `display_manager ->fonts_changed` reloads the fonts after changing
%   font.scale or font.pango_families.  The font objects stay the same.

set_font_cv(Name, Value, Old) :-
    get(@font_class, class_variable, Name, CV),
    get(CV, value, Old),
    send(@font_class, class_variable_value, Name, Value),
    send(@display_manager, fonts_changed).

font_height(Font, Height) :-
    get(Font, height, Height).

:- begin_tests(font_reload).

test(scale, [true(H1 > H0), cleanup(set_font_cv(scale, Old, _))]) :-
    new(F, font(sans, normal, 12)),
    font_height(F, H0),
    set_font_cv(scale, 2, Old),
    font_height(F, H1).
test(family_table, [Mapped == 'serif-test',
                    cleanup(set_font_cv(pango_families, Old, _))]) :-
    new(New, chain(mono := 'serif-test', sans := 'Noto Sans')),
    set_font_cv(pango_families, New, Old),
    get(@font_families, member, mono, Mapped).

%   An editor with a mono font uses a copy of the bold font with the
%   same ascent.  This copy is not a shared font, so ->fonts_changed has
%   to make it again.

test(editor_bold_font, [Bold == Plain, cleanup(set_font_cv(scale, Old, _))]) :-
    new(E, editor),
    send(E, font, font(mono, normal, 12)),
    set_font_cv(scale, 2, Old),
    send(E, fonts_changed),             % E is not displayed
    get(E?bold_font, ascent, Bold),
    get(E?font, ascent, Plain).

%   The old families screen, helvetica and times follow mono, sans and
%   serif.

test(old_families, Same == [true, true, true]) :-
    findall(B,
            ( member(Old-New, [screen-mono, helvetica-sans, times-serif]),
              new(O, font(Old, normal, 12)),
              new(N, font(New, normal, 12)),
              get(O, pango_property, family, OF),
              get(N, pango_property, family, NF),
              ( OF == NF -> B = true ; B = false )
            ),
            Same).

:- end_tests(font_reload).
