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
               font_reload, theme_font, terminal_variant_fonts]).

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


%   Font aliases are theme fonts: fonts whose properties are those of
%   the font they are derived from.  Changing that font changes the
%   theme font in place, so all its users follow.

alias(Name, Font) :-
    get(@pce, convert, Name, font, Font).

font_props(Font, font(Family, Style, Points)) :-
    get(Font, family, Family),
    get(Font, style, Style),
    get(Font, points, Points).

restore_alias(Name, From) :-
    alias(Name, F),
    send(F, derived_from, From),
    send(@display_manager, fonts_changed).

:- begin_tests(theme_font).

test(alias_is_theme_font, Props == font(sans, normal, 12)) :-
    alias(normal, F),
    send(F, instance_of, theme_font),
    get(F, name, normal),
    font_props(F, Props).
test(differs_from_its_font, true(F \== P)) :-
    alias(normal, F),
    new(P, font(sans, normal, 12)),
    get(F, derived_from, P).
test(derived_from_in_place, [ Props == font(serif, bold, 20), F == F2,
                              true(H1 > H0),
                              cleanup(restore_alias(normal, Old)) ]) :-
    alias(normal, F),
    get(F, derived_from, Old),
    get(F, height, H0),
    send(F, derived_from, font(serif, bold, 20)),
    alias(normal, F2),
    font_props(F, Props),
    get(F, height, H1).
test(new_returns_existing, [ Props == font(mono, normal, 16), F == F2,
                             cleanup(restore_alias(bold, Old)) ]) :-
    alias(bold, F),
    get(F, derived_from, Old),
    new(F2, theme_font(bold, font(mono, normal, 16))),
    font_props(F, Props).
test(derived_from_theme_font, [ Props == font(mono, italic, 14),
                                cleanup(free(F)) ]) :-
    alias(itt, ITT),
    new(F, theme_font(test_theme_font, ITT)),
    get(F, derived_from, From),
    \+ send(From, instance_of, theme_font),
    font_props(F, Props).
test(new_alias, F == F2, [cleanup(free(F))]) :-
    new(F, theme_font(test_theme_font, font(serif, normal, 9))),
    alias(test_theme_font, F2).
test(rescale_is_plain_font, Props == font(sans, normal, 24)) :-
    alias(normal, F),
    get(F, rescale, 2, R),
    \+ send(R, instance_of, theme_font),
    font_props(R, Props).
test(class_variable, [ Props == font(serif, normal, 20)-font(sans, bold, 12),
                       F == F2,
                       cleanup(set_font_cv(system_fonts, Old, _)) ]) :-
    alias(normal, F),
    set_font_cv(system_fonts, chain(normal := font(serif, normal, 20)), Old),
    alias(normal, F2),
    alias(bold, B),                     % omitted: built-in value
    font_props(F, P1),
    font_props(B, P2),
    Props = P1-P2.
test(text_follows, [ true(W1 > W0),
                     cleanup(( restore_alias(normal, Old), free(Fr) )) ]) :-
    alias(normal, F),
    get(F, derived_from, Old),
    new(Fr, frame),
    send(Fr, append, new(P, picture)),
    send(P, display, new(T, text(hello, left, normal))),
    send(Fr, wait),
    get(T, width, W0),
    send(F, derived_from, font(sans, normal, 24)),
    send(@display_manager, fonts_changed),
    get(T, width, W1).

:- end_tests(theme_font).


%   A terminal paints bold and italic text in variants of its <-font that
%   must have the same pitch.  The class variables name the variants by
%   the aliases `boldtt`, `itt` and `bitt`, which have the default size.
%   A user who changed the size of the font (and of the bold font, which
%   existed long before the italic ones) still gets italic variants of
%   the right size: they are derived from <-font.

set_terminal_cv(Name, Value, Old) :-
    get(class(terminal_image), class_variable, Name, CV),
    get(CV, value, Old),
    send(class(terminal_image), class_variable_value, Name, Value).

:- begin_tests(terminal_variant_fonts,
               [ setup(( set_terminal_cv(font, font(mono, normal, 9), OldF),
                         set_terminal_cv(bold_font, font(mono, bold, 9), OldB),
                         asserta(old_terminal_fonts(OldF, OldB))
                       )),
                 cleanup(( retract(old_terminal_fonts(OldF, OldB)),
                           set_terminal_cv(font, OldF, _),
                           set_terminal_cv(bold_font, OldB, _)
                         ))
               ]).

:- dynamic old_terminal_fonts/2.

variant_points(Variant, Points) :-
    new(TI, terminal_image),
    get(TI, Variant, Font),
    (   Font == @nil
    ->  Points = @nil
    ;   get(Font, points, Points)
    ),
    send(TI, free).

test(bold, [Points =:= 9]) :-
    variant_points(bold_font, Points).
test(italic, [Points =:= 9]) :-
    variant_points(italic_font, Points).
test(bold_italic, [Points =:= 9]) :-
    variant_points(bold_italic_font, Points).
test(italic_style, [Style-Weight == italic-normal]) :-
    new(TI, terminal_image),
    get(TI?italic_font, style, Style),
    get(TI?italic_font, weight, Weight),
    send(TI, free).
test(bold_italic_style, [Style-Weight == italic-bold]) :-
    new(TI, terminal_image),
    get(TI?bold_italic_font, style, Style),
    get(TI?bold_italic_font, weight, Weight),
    send(TI, free).

:- end_tests(terminal_variant_fonts).
