/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
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

:- module(test_theme,
          [ test_theme/0
          ]).
:- use_module(library(pce)).
:- use_module(library(pce_theme)).
:- use_module(library(plunit)).

/** <module> Test semantic colours and themes

The themes `test_dark` and `test_bad` and the semantic colours
`test_theme_*` are defined here, so the tests do not depend on the
installed theme files.
*/

test_theme :-
    run_tests([ theme
              ]).

:- multifile
    pce_theme:semantic_colour/3,
    pce_theme:colour/3.

pce_theme:semantic_colour(test_theme_fg,    red,    "Test foreground").
pce_theme:semantic_colour(test_theme_bg,    white,  "Test background").
pce_theme:semantic_colour(test_theme_alias, test_theme_fg, "Test alias").
pce_theme:semantic_colour(test_theme_late,  black,  "Test first use").
pce_theme:semantic_colour(test_theme_alias2, test_theme_target2, "Test alias").
pce_theme:semantic_colour(test_theme_target2, '#030303', "Test alias target").

pce_theme:colour(test_dark, test_theme_fg, blue).
pce_theme:colour(test_dark, test_theme_bg, '#101010').
pce_theme:colour(test_dark, test_theme_late, '#202020').
pce_theme:colour(test_dark, test_theme_target2, '#040404').

pce_theme:colour(test_bad,  test_theme_fg, no_such_colour_name).
pce_theme:colour(test_bad,  test_theme_bg, black).
pce_theme:colour(test_bad,  test_theme_bg, white).
pce_theme:colour(test_bad,  test_theme_no_such_name, black).

:- begin_tests(theme, [cleanup(apply_theme(light))]).

test(light_default, [RGB == rgb(255,0,0), Class == theme_colour,
                     Locked == @on]) :-
    apply_theme(light),
    theme_colour(test_theme_fg, C),
    colour_rgb(C, RGB),
    get(C, class_name, Class),
    get(C, lock_object, Locked).
test(named_reference, Same == true) :-
    %  Referring to the colour by name gives the same object, so
    %  graphicals using the name follow the theme.
    theme_colour(test_theme_bg, C),
    get(@pce, convert, test_theme_bg, colour, C2),
    ( C == C2 -> Same = true ; Same = false ).
test(switch_in_place, [Dark == rgb(0,0,255), Light == rgb(255,0,0),
                       Same == true]) :-
    theme_colour(test_theme_fg, C),
    apply_theme(test_dark),
    theme_colour(test_theme_fg, C2),
    colour_rgb(C, Dark),
    apply_theme(light),
    colour_rgb(C, Light),
    ( C == C2 -> Same = true ; Same = false ).
test(alias, [Light == rgb(255,0,0), Dark == rgb(0,0,255)]) :-
    apply_theme(light),
    theme_colour(test_theme_alias, C),
    colour_rgb(C, Light),
    apply_theme(test_dark),
    colour_rgb(C, Dark),
    apply_theme(light).
test(alias_before_target, [Light == rgb(3,3,3), Dark == rgb(4,4,4)]) :-
    %  The alias is used before its target colour object exists
    apply_theme(light),
    theme_colour(test_theme_alias2, C),
    colour_rgb(C, Light),
    apply_theme(test_dark),
    colour_rgb(C, Dark),
    apply_theme(light).
test(created_in_theme, RGB == rgb(32,32,32)) :-
    %  A colour first used while a theme is active gets its theme value
    apply_theme(test_dark),
    theme_colour(test_theme_late, C),
    colour_rgb(C, RGB),
    apply_theme(light).
test(current_theme, [Dark == test_dark, Light == light]) :-
    apply_theme(test_dark),
    current_theme(Dark),
    apply_theme(default),
    current_theme(Light).
test(unknown_theme, error(existence_error(theme, test_no_such_theme))) :-
    apply_theme(test_no_such_theme).
test(system_colours_message, RGB == rgb(0,0,255)) :-
    %  The message installed by init_theme re-applies a fixed theme
    %  after the system colours are reloaded.
    theme_colour(test_theme_fg, C),
    setup_call_cleanup(
        ( set_prolog_flag(theme, test_dark),
          pce_theme:init_theme,
          send(C, value, green),
          send(@display_manager, system_colours_changed)
        ),
        colour_rgb(C, RGB),
        ( set_prolog_flag(theme, light),
          apply_theme(light)
        )).

test(syntax_name, Name == syntax_goal_built_in) :-
    syntax_colour_name(goal(built_in,_), colour, Name).
test(syntax_name, Name == syntax_lsp_enum) :-
    syntax_colour_name(lsp(enum), colour, Name).
test(syntax_name, Name == syntax_goal_dynamic) :-
    syntax_colour_name(goal(dynamic(_),_), colour, Name).
test(syntax_name, Name == syntax_comment) :-
    syntax_colour_name(comment(_), colour, Name).
test(syntax_name, Name == syntax_unused_import_bg) :-
    syntax_colour_name(unused_import, background, Name).

test(syntax_default, RGB == RGB0) :-
    %  The light value of a syntax colour is the def_style/2 colour
    use_module(library(prolog_colour), []),
    prolog_colour:def_style(comment(_), Attrs),
    memberchk(colour(Value), Attrs),
    get(@pce, convert, Value, colour, C0),
    colour_rgb(C0, RGB0),
    theme_colour(syntax_comment, C),
    colour_rgb(C, RGB).

test(issues_dark, Issues == [ missing(test_theme_alias),
                              missing(test_theme_alias2)
                            ]) :-
    theme_issues(test_dark, Issues0),
    test_issues(Issues0, Issues).
test(issues_bad, Issues == [ unknown(test_theme_no_such_name),
                             duplicate(test_theme_bg),
                             invalid(test_theme_fg, no_such_colour_name),
                             missing(test_theme_alias),
                             missing(test_theme_alias2),
                             missing(test_theme_late),
                             missing(test_theme_target2),
                             redundant(test_theme_bg)
                           ]) :-
    theme_issues(test_bad, Issues0),
    test_issues(Issues0, Issues).

test(dark_theme_complete, Errors == []) :-
    %  library(theme/dark) defines all semantic colours and nothing
    %  else.  If this fails after adding a style to
    %  library(prolog_colour) or a semantic colour to a library, add its
    %  colour to library(theme/dark).  See check_theme/1.
    load_theme_libraries,
    use_module(library(theme/dark), []),
    theme_issues(dark, Issues),
    exclude(informational_issue, Issues, Issues1),
    exclude(test_issue, Issues1, Errors).
test(prolog_mode_style, [Class == theme_colour, Same == true]) :-
    %  PceEmacs styles refer to the theme colour of their class
    use_module(library(emacs/prolog_mode), []),
    emacs_prolog_mode:style(comment(_), _, Style),
    get(Style, colour, C),
    get(C, class_name, Class),
    get(@colours, member, syntax_comment, C2),
    ( C == C2 -> Same = true ; Same = false ).

informational_issue(redundant(_)).

%   Only consider the issues about the test colours, as the other
%   semantic colours depend on the loaded libraries.

test_issues(Issues0, Issues) :-
    include(test_issue, Issues0, Issues).

test_issue(Issue) :-
    arg(1, Issue, Name),
    atom(Name),
    sub_atom(Name, 0, _, _, test_theme_).

theme_colour(Name, C) :-
    ensure_theme_colours,
    get(@colours, member, Name, C).

colour_rgb(C, rgb(R,G,B)) :-
    get(C, red, R),
    get(C, green, G),
    get(C, blue, B).

:- end_tests(theme).
