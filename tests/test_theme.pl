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
    run_tests([ theme,
                theme_files
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
pce_theme:colour(test_dark, ui_window_background, '#010101').

pce_theme:colour(test_bad,  test_theme_fg, no_such_colour_name).
pce_theme:colour(test_bad,  test_theme_bg, black).
pce_theme:colour(test_bad,  test_theme_bg, white).
pce_theme:colour(test_bad,  test_theme_no_such_name, black).

:- pce_begin_class(test_theme_device, device).
variable(notified, int := 0, both, "Times ->colours_changed was received").
colours_changed(D) :->
    get(D, notified, N0),
    N is N0+1,
    send(D, notified, N).
:- pce_end_class(test_theme_device).

:- pce_begin_class(test_theme_frame, frame).
variable(notified, int := 0, both, "Times ->colours_changed was received").
colours_changed(F) :->
    get(F, notified, N0),
    N is N0+1,
    send(F, notified, N).
:- pce_end_class(test_theme_frame).

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
          send(C, derived_from, green),
          send(@display_manager, system_colours_changed)
        ),
        colour_rgb(C, RGB),
        ( set_prolog_flag(theme, light),
          apply_theme(light)
        )).

test(dark_on_light, [Dark == rgb(1,1,1), Dialog == Sys]) :-
    %  The test runs on a light desktop.  A dark theme replaces the
    %  roles it defines and keeps the system colour for the others.
    \+ dark_system,
    apply_theme(test_dark),
    theme_rgb(ui_window_background, Dark),
    theme_rgb(ui_dialog_background, Dialog),
    theme_rgb(sys_dialog_background, Sys),
    apply_theme(light).
test(light_on_light, RGB == Sys) :-
    \+ dark_system,
    apply_theme(light),
    theme_rgb(ui_window_background, RGB),
    theme_rgb(sys_window_background, Sys).
test(light_on_dark, [Window == rgb(255,255,255), Text == rgb(0,0,0)]) :-
    %  Simulate a dark desktop.  The light theme replaces the system
    %  colours by its own.
    setup_call_cleanup(
        fake_dark_system,
        ( apply_theme(light),
          theme_rgb(ui_window_background, Window),
          theme_rgb(ui_window_foreground, Text)
        ),
        restore_system).
test(dark_on_dark, RGB == rgb(16,16,16)) :-
    %  A dark theme on a dark desktop uses the system colours
    setup_call_cleanup(
        fake_dark_system,
        ( apply_theme(test_dark),
          theme_rgb(ui_window_background, RGB)
        ),
        ( restore_system,
          apply_theme(light)
        )).
test(select_theme, [Sel1 == test_dark, Cur1 == test_dark,
                    Sel2 == system, Cur2 == light]) :-
    select_theme(test_dark),
    current_theme_selection(Sel1),
    current_theme(Cur1),
    select_theme(system),
    current_theme_selection(Sel2),
    current_theme(Cur2).
test(restored_system, Dark == false) :-
    %  The tests above restored the light system colours
    ( dark_system -> Dark = true ; Dark = false ).
test(available_theme, nondet) :-
    available_theme(light),
    available_theme(dark).

test(adaptive_light_colour, [Light == rgb(250,250,210), Dark == rgb(36,36,16)]) :-
    %  A light background becomes a dark one of the same hue in a dark
    %  theme.
    adaptive_colour(test_adaptive_bg, lightgoldenrodyellow,
                    ui_window_background),
    apply_theme(light),
    theme_rgb(test_adaptive_bg, Light),
    apply_theme(test_dark),
    theme_rgb(test_adaptive_bg, Dark),
    apply_theme(light).
test(adaptive_dark_colour, [Light == rgb(230,230,230), Dark == rgb(0,0,0)]) :-
    %  A dark background is kept in a dark theme and lightened in a
    %  light one, where the text is dark.
    adaptive_colour(test_adaptive_dark, black, ui_window_background),
    apply_theme(light),
    theme_rgb(test_adaptive_dark, Light),
    apply_theme(test_dark),
    theme_rgb(test_adaptive_dark, Dark),
    apply_theme(light).
test(adaptive_explicit, [Light == rgb(250,250,210), Dark == rgb(1,2,3)]) :-
    adaptive_colour(test_adaptive_explicit,
                    [light=lightgoldenrodyellow, test_dark='#010203'],
                    ui_window_background),
    apply_theme(light),
    theme_rgb(test_adaptive_explicit, Light),
    apply_theme(test_dark),
    theme_rgb(test_adaptive_explicit, Dark),
    apply_theme(light).
test(adaptive_not_checked, Issues == []) :-
    %  Adaptive colours adapt to any theme, so a theme need not define
    %  them.
    adaptive_colour(test_adaptive_bg, lightgoldenrodyellow,
                    ui_window_background),
    theme_issues(test_dark, Issues0),
    include(adaptive_issue, Issues0, Issues).

test(colours_changed_notifies, [FN == 1, DN == 1]) :-
    %  ->colours_changed tells frames and graphicals in nested devices
    %  that define it, so applications can update colours they painted
    %  into images.
    new(F, test_theme_frame),
    send(F, append, new(P, picture)),
    send(P, display, new(Outer, device)),
    send(Outer, display, new(D, test_theme_device)),
    send(F, open),
    send(@display_manager, colours_changed),
    get(F, notified, FN),
    get(D, notified, DN),
    send(F, destroy).

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


%   theme_colour_origin/2 tells why a theme colour has its value.

test(origin_role, [condition(\+ dark_system),
                   Origin == desktop(light, matches)]) :-
    apply_theme(light),
    theme_colour_origin(ui_window_background, Origin).
test(origin_theme, [condition(\+ dark_system),
                    Origin == theme(test_dark)]) :-
    setup_call_cleanup(
        apply_theme(test_dark),
        theme_colour_origin(ui_window_background, Origin),
        apply_theme(light)).
test(origin_not_in_theme, [condition(\+ dark_system),
                           Origin == desktop(test_dark, undefined)]) :-
    setup_call_cleanup(
        apply_theme(test_dark),
        theme_colour_origin(ui_dialog_background, Origin),
        apply_theme(light)).
test(origin_matching_theme, Origin == desktop(test_dark, matches)) :-
    setup_call_cleanup(
        ( fake_dark_system,
          apply_theme(test_dark)
        ),
        theme_colour_origin(ui_window_background, Origin),
        ( restore_system,
          apply_theme(light)
        )).
test(origin_builtin, Origin == default(xpce)) :-
    apply_theme(light),
    theme_colour_origin(ui_cursor, Origin).
test(origin_hook, Origin == default(hook)) :-
    apply_theme(light),
    theme_colour_origin(test_theme_fg, Origin).
test(origin_program, Origin == program) :-
    apply_theme(light),
    theme_colour(test_theme_bg, C),
    setup_call_cleanup(
        send(C, derived_from, green),
        theme_colour_origin(test_theme_bg, Origin),
        send(C, derived_from, white)).
test(origin_unknown, fail) :-
    theme_colour_origin(test_theme_no_such_name, _).
test(system_origin, true(memberchk(Origin, [gnome,kde,windows,macos,xpce]))) :-
    theme_rgb(sys_window_background, _),
    get(@system_colour_origins, member, sys_window_background, Origin).
test(system_origin_tint,
     true(memberchk(Origin, [gnome,kde,windows,macos,tint]))) :-
    theme_rgb(sys_text_selection_background, _),
    get(@system_colour_origins, member, sys_text_selection_background,
        Origin).

%   The theme colour chooser shows the trace as tooltip

test(colour_trace, [ condition(\+ dark_system),
                     Lines = [ 'ui_margin_background = ui_window_background (default of xpce)',
                               'ui_window_background = sys_window_background (desktop colour, as the light theme matches the desktop)',
                               Sys
                             ],
                     Source == desktop,
                     true(sub_atom(Sys, 0, _, _, 'sys_window_background = #'))
                   ]) :-
    use_module(library(pce_colour_item), []),
    apply_theme(light),
    theme_colour(ui_margin_background, C),
    pce_colour_item:colour_trace(C, Lines, Source).

informational_issue(redundant(_)).

adaptive_issue(Issue) :-
    arg(1, Issue, Name),
    atom(Name),
    sub_atom(Name, 0, _, _, test_adaptive_).

%   Only consider the issues about the test colours, as the other
%   semantic colours depend on the loaded libraries.

test_issues(Issues0, Issues) :-
    include(test_issue, Issues0, Issues).

test_issue(Issue) :-
    arg(1, Issue, Name),
    atom(Name),
    sub_atom(Name, 0, _, _, test_theme_).

theme_rgb(Name, RGB) :-
    get(@pce, convert, Name, colour, C),
    colour_rgb(C, RGB).

dark_system :-
    get(@pce, convert, sys_window_background, colour, C),
    get(C, intensity, I),
    I < 128.

%   Make the system window colours dark.  ->system_colours_changed
%   reloads the real values, as they differ from the faked values in
%   @colour_names.

fake_dark_system :-
    get(@pce, convert, sys_window_background, colour, Bg),
    get(@pce, convert, sys_window_foreground, colour, Fg),
    get(@pce, convert, '#101010', colour, Dark),
    get(@pce, convert, '#f0f0f0', colour, Light),
    get(Dark, rgba, DarkRGBA),
    get(Light, rgba, LightRGBA),
    send(@colour_names, append, sys_window_background, DarkRGBA),
    send(@colour_names, append, sys_window_foreground, LightRGBA),
    send(Bg, slot, rgba, DarkRGBA),
    send(Fg, slot, rgba, LightRGBA).

restore_system :-
    send(@display_manager, system_colours_changed).

theme_colour(Name, C) :-
    ensure_theme_colours,
    get(@colours, member, Name, C).

colour_rgb(C, rgb(R,G,B)) :-
    get(C, red, R),
    get(C, green, G),
    get(C, blue, B).

:- end_tests(theme).

%   user_theme_dir(-Dir)
%
%   Create a library directory Dir with theme/mytheme.pl, a theme that
%   only sets console colours, a second theme/dark.pl and a file that
%   is not a Prolog source, and add it to the library search path.

user_theme_dir(Dir) :-
    tmp_file(themes, Dir),
    directory_file_path(Dir, theme, ThemeDir),
    make_directory_path(ThemeDir),
    forall(member(File-Content,
                  [ 'mytheme.pl' -
                    ":- module(prolog_theme_mytheme, []).\n\c
                     :- multifile prolog:theme/1.\n\c
                     prolog:theme(mytheme).\n",
                    'dark.pl'    - ":- module(my_dark, []).\n",
                    'notes.txt'  - "Not a theme\n"
                  ]),
           ( directory_file_path(ThemeDir, File, Path),
             setup_call_cleanup(open(Path, write, Out),
                                write(Out, Content),
                                close(Out))
           )),
    asserta(user:file_search_path(library, Dir)).

remove_user_theme_dir(Dir) :-
    retractall(user:file_search_path(library, Dir)),
    delete_directory_and_contents(Dir).

:- begin_tests(theme_files,
               [ setup(user_theme_dir(Dir)),
                 cleanup(remove_user_theme_dir(Dir))
               ]).

test(user_theme, nondet) :-
    available_theme(mytheme).
test(once, [Themes == [light,dark,mytheme]]) :-
    findall(T, ( available_theme(T),
                 memberchk(T, [light,dark,mytheme,auto,notes])
               ), Themes).
test(theme_item, true) :-
    new(TI, theme_item),
    get(TI, member, mytheme, _).

:- end_tests(theme_files).
