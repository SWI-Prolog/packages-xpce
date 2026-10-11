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

:- module(test_theme_editor,
          [ test_theme_editor/0
          ]).

/** <module> Test the theme editor

Tests library(pce_theme_editor) by calling the methods that the
controls of the editor call.  The editor edits the theme `test_edited`,
created in memory as a variation of `dark`.
*/

%   The editor is a persistent_frame: keep what it saves out of the
%   configuration of the user running the tests.  Must be done before
%   xpce is initialised.

:- use_module(xpce_test_sandbox).
:- initialization(sandbox_app_config(_), now).

:- use_module(library(pce)).
:- initialization(remove_sandbox_at_exit, now).
:- use_module(library(plunit)).
:- use_module(library(pce_theme)).
:- use_module(library(pce_theme_editor)).
:- use_module(library(pce_theme_file)).
:- use_module(library(pce_theme_samples)).
:- use_module(library(pce_colour_item), [colour_trace_steps/3]).
:- use_module(library(readutil)).

test_theme_editor :-
    run_tests([ theme_editor_looks,
                theme_samples,
                theme_editor
              ]).

:- begin_tests(theme_editor_looks).

test(xterm_grey, Colour == '#444444') :-
    pce_theme_editor:xterm_colour(238, Colour).
test(xterm_cube, Colour == '#ff0000') :-
    pce_theme_editor:xterm_colour(196, Colour).
test(xterm_bright, Colour == ansi_bright_cyan) :-
    pce_theme_editor:xterm_colour(14, Colour).
test(look, Look == look(ansi_bright_yellow, '#444444', @on, @off, @off)) :-
    pce_theme_editor:look([bold, hfg(yellow), bg8(238)], Look).
test(look_default, Look == look(@default, @default, @off, @off, @off)) :-
    pce_theme_editor:look([fg(default)], Look).
test(pldoc_look, Attrs == [hfg(cyan), bg(red), bold]) :-
    pce_theme_editor:look_attributes(
                         pldoc,
                         [ color(bright_cyan), background(red),
                           font_weight(bold), margin_left(2)
                         ],
                         Attrs).
test(index_attr, Attrs == [fg(red), hbg(cyan), fg8(100)]) :-
    pce_theme_editor:index_attr(foreground, 1, A1),
    pce_theme_editor:index_attr(background, 14, A2),
    pce_theme_editor:index_attr(foreground, 100, A3),
    Attrs = [A1,A2,A3].
test(current_colour, Indexes == [100, 9, 3, @nil]) :-
    findall(I,
            ( member(Which-Attrs,
                     [ foreground-[bold, fg8(100)],
                       background-[hbg(red)],
                       foreground-[fg8(yellow)],
                       foreground-[fg('#123456')]
                     ]),
              pce_theme_editor:current_colour(console, Which, Attrs, _, I)
            ),
            Indexes).
test(xterm_palette, [Size-First == 256-ansi_black]) :-
    pce_theme_editor:xterm_palette(P),
    get(P, size, Size),
    get(P, element, 1, C),
    get(C, name, First).
test(status_symbol, Status == '⚠') :-
    %  The most severe issue of the row
    pce_theme_editor:row_status(light, colour(foo),
                                [ redundant(foo), missing(foo), unknown(bar) ],
                                Status).
test(status_none, Status == '') :-
    pce_theme_editor:row_status(light, colour(foo), [unknown(bar)], Status).
test(status_tooltip, true(sub_atom(Tip, _, _, _, 'no value for foo'))) :-
    pce_theme_editor:row_tooltip(light, colour(foo), [missing(foo)], Tip).
test(key_roundtrip, Row =@= console(goal(_,odd))) :-
    pce_theme_editor:row_key(console(goal(_,odd)), Key),
    pce_theme_editor:key_row(Key, Row).

:- end_tests(theme_editor_looks).

%   Samples that show what a colour is used for.

:- begin_tests(theme_samples, [setup(load_theme_libraries)]).

test(ui_samples, Missing == []) :-
    findall(Name, ( semantic_colour_name(Name, _),
                    sub_atom(Name, 0, _, _, ui_),
                    \+ colour_sample(Name, _, _)
                  ), Missing).
test(syntax_samples, Missing == []) :-
    findall(Name, ( syntax_colour(Name, _, _),
                    \+ colour_sample(Name, _, _)
                  ), Missing).
test(syntax_style, Colour == syntax_goal_built_in) :-
    %  The fragment uses the style PceEmacs uses
    once(colour_sample(syntax_goal_built_in, _, E)),
    get(E, styles, Styles),
    get(Styles, value, sample, Style),
    get(Style, colour, C),
    get(C, name, Colour).
test(class_variable, nondet) :-
    new(B, button(test)),
    colour_class_variable(ui_button_background, button, face_colour),
    \+ colour_class_variable(ui_button_background, pce, _),
    send(B, destroy).
test(isearch_class_variables,
     Classes == [editor, list_browser, terminal_image]) :-
    %  Editors, lists and terminals refer to the same theme colour for
    %  the hit of an incremental search
    forall(member(Class, [editor, list_browser, terminal_image]),
           get(@pce, convert, Class, class, _)),
    findall(Class, colour_class_variable(ui_isearch_background,
                                         Class, isearch_style),
            Classes0),
    sort(Classes0, Classes).
test(console_sample, true(memberchk(s('Call'), Segments))) :-
    console_sample(port(call), Segments).
test(console_sample_default, Segments == [s('?- member(X, [a,b]).')]) :-
    console_sample(no_such_class, Segments).

:- end_tests(theme_samples).

%   The theme is saved, so the editor does not ask to save it when it
%   stops editing.  The file is in the sandbox configuration.

open_editor :-
    create_theme(test_edited, dark),
    save_theme(test_edited),
    apply_theme(test_edited),
    new(@test_theme_editor, theme_editor),
    send(@test_theme_editor, theme, test_edited).

close_editor :-
    send(@test_theme_editor, slot, theme, @nil),
    send(@test_theme_editor, destroy),
    forall(edited_theme(Theme, _), end_theme_edit(Theme)),
    apply_theme(light).

modified(Theme, Modified) :-
    (   theme_modified(Theme)
    ->  Modified = true
    ;   Modified = false
    ).

file_contains(Theme, Text) :-
    theme_source(Theme, File),
    read_file_to_string(File, Content, []),
    sub_string(Content, _, _, _, Text),
    !.

detail(E, Name, Item) :-
    get(E, member, detail, D),
    get(D, member, Name, Item).

rows(E, Keys) :-
    get(E, member, rows, B),
    get(B?members, map, @arg1?key, Chain),
    chain_list(Chain, Keys).

usage_captions(E, Captions) :-
    detail(E, usage, G),
    get(G?graphicals, find_all, @arg1?name == caption, Chain),
    chain_list(Chain, Labels),
    maplist(caption, Labels, Captions).

caption(Label, Caption) :-
    get(Label, selection, String),
    get(String, value, Caption).

select_group(E, Group) :-
    get(E, member, top, Top),
    get(Top, member, group, M),
    send(M, selection, Group),
    send(E, fill).

select_status(E, Status) :-
    get(E, member, top, Top),
    get(Top, member, status, M),
    send(M, selection, Status),
    send(E, fill).

:- begin_tests(theme_editor,
               [ setup(open_editor),
                 cleanup(close_editor)
               ]).

editor(@test_theme_editor).

test(edit_file_active, Active == @on) :-
    %  The theme is saved, so it has a file to edit
    editor(E),
    get(E, member, bottom, Bottom),
    get(Bottom, member, edit_file, Button),
    get(Button, active, Active).
test(rows, true) :-
    editor(E),
    rows(E, Keys),
    memberchk('colour:syntax_comment', Keys),
    memberchk('colour:ui_window_background', Keys),
    memberchk('console:goal(_,odd)', Keys),
    memberchk('pldoc:span-class(autoload)', Keys).
test(set_colour, [Setting == colour(syntax_comment, red), RGB == 255]) :-
    editor(E),
    send(E, select, 'colour:syntax_comment'),
    send(E, set_colour, colour(red)),
    theme_setting(test_edited, Setting),
    get(@pce, convert, syntax_comment, colour, C),
    get(C, red, RGB),
    detail(E, reset, Reset),
    get(Reset, active, @on).
test(theme_colour_reference, Setting == colour(syntax_comment, syntax_string)) :-
    editor(E),
    %  Selecting a theme colour refers to it by name
    get(@pce, convert, syntax_string, colour, String),
    send(E, set_colour, String),
    theme_setting(test_edited, Setting).
test(self_reference, Setting == colour(syntax_comment, syntax_string)) :-
    editor(E),
    get(@pce, convert, syntax_comment, colour, Self),
    send(E, set_colour, Self),
    theme_setting(test_edited, Setting).
test(changed_filter, Keys == ['colour:syntax_comment']) :-
    editor(E),
    select_status(E, changed),
    rows(E, Keys),
    select_status(E, all).
test(group_filter, true(forall(member(K, Keys),
                               sub_atom(K, 0, _, _, 'console:')))) :-
    editor(E),
    select_group(E, console),
    rows(E, Keys),
    Keys \== [],
    select_group(E, all).
test(regex_filter, Keys == ['colour:syntax_comment']) :-
    editor(E),
    send(E, filter, regex(syntax_comm)),
    rows(E, Keys),
    send(E, filter, @nil).
test(reset, Settings == []) :-
    editor(E),
    send(E, select, 'colour:syntax_comment'),
    send(E, reset),
    findall(S, theme_setting(test_edited, S), Settings).
test(console_look, Setting =@= console(goal(_,odd), [bold])) :-
    editor(E),
    send(E, select, 'console:goal(_,odd)'),
    detail(E, look, L),
    send(L, user_selection, [bold]),
    theme_setting(test_edited, Setting).
test(console_controls, Setting =@= console(goal(_,odd), [bold, fg(red)])) :-
    editor(E),
    %  Changing the foreground keeps the other attributes
    detail(E, look, L),
    get(L, member, foreground, M),
    send(M, selection, 'fg(red)'),
    send(L, controls_changed),
    theme_setting(test_edited, Setting).
test(console_text, Setting =@= console(goal(_,odd), [underline])) :-
    editor(E),
    detail(E, look, L),
    send(L, text_changed, "[underline]"),
    theme_setting(test_edited, Setting).
test(console_bad_text, Setting =@= console(goal(_,odd), [underline])) :-
    editor(E),
    detail(E, look, L),
    send(L, text_changed, "[underline"),
    theme_setting(test_edited, Setting).
test(console_reset, Settings == []) :-
    editor(E),
    send(E, reset),
    findall(S, theme_setting(test_edited, S), Settings).
test(live_cancel_inherited, Settings == []) :-
    %  The colour editor changes the setting; Cancel removes it again
    editor(E),
    send(E, select, 'colour:syntax_comment'),
    send(E, edit_session, start),
    send(E, set_colour, colour(red)),
    send(E, set_colour, colour(blue)),
    send(E, edit_session, cancel),
    findall(S, theme_setting(test_edited, S), Settings).
test(live_cancel_own, Settings == [colour(syntax_comment, red)]) :-
    editor(E),
    send(E, set_colour, colour(red)),
    send(E, edit_session, start),
    send(E, set_colour, colour(blue)),
    send(E, edit_session, cancel),
    findall(S, theme_setting(test_edited, S), Settings).
test(live_ok, Settings == [colour(syntax_comment, blue)]) :-
    editor(E),
    send(E, edit_session, start),
    send(E, set_colour, colour(blue)),
    send(E, edit_session, ok),
    findall(S, theme_setting(test_edited, S), Settings),
    send(E, reset).
test(live_no_fill, Same == true) :-
    %  While the colour editor is open, a change does not refill the
    %  list: the rows are the same objects.  The editor must be open
    %  to receive ->colours_changed
    editor(E),
    send(E, open),
    send(E, select, 'colour:syntax_comment'),
    get(E, member, rows, B),
    get(B, member, 'colour:ui_accent', Before),
    send(E, edit_session, start),
    send(E, set_colour, colour(red)),
    get(B, member, 'colour:ui_accent', After),
    send(E, edit_session, cancel),
    (   Before == After
    ->  Same = true
    ;   Same = false
    ).
test(live_row, [true(sub_string(Label, _, _, _, "✎"))]) :-
    %  During the session only the row of the setting is updated
    editor(E),
    send(E, edit_session, start),
    send(E, set_colour, colour(@default, 0x12, 0x34, 0x56)),
    get(E, member, rows, B),
    get(B, member, 'colour:syntax_comment', DI),
    get(DI?label, value, Label),
    send(E, edit_session, cancel).
test(console_palette, Settings =@= [ console(goal(_,odd), [fg8(100)]),
                                     console(goal(_,odd), [hfg(red)]),
                                     console(goal(_,odd), [fg('#010203')])
                                   ]) :-
    editor(E),
    send(E, select, 'console:goal(_,odd)'),
    detail(E, look, L),
    send(L, user_selection, []),
    findall(S,
            ( member(Colour-Index,
                     [ red-100,
                       red-9,
                       colour(@default, 1, 2, 3)-(@nil)
                     ]),
              send(L, custom_colour, foreground, Colour, Index),
              theme_setting(test_edited, S)
            ),
            Settings),
    send(E, reset).
test(trace_html, true(memberchk(element(a, [href='colour:ui_window_background'],
                                    _), Lines))) :-
    editor(E),
    send(E, select, 'colour:ui_margin_background'),
    get(@pce, convert, ui_margin_background, colour, C),
    colour_trace_steps(C, Steps, _),
    pce_theme_editor:trace_html(Steps, Lines).
test(trace_link, Current == 'colour:ui_window_background') :-
    %  A link in the trace selects the colour
    editor(E),
    send(E, select, 'colour:ui_margin_background'),
    get(E, member, detail, D),
    send(D, goto_url, 'colour:ui_window_background'),
    get(E, current, Current),
    get(E, member, rows, B),
    get(B?selection, key, Current).
test(class_variable_link, Title == 'tab.indicator_colour') :-
    %  A class variable in "Used for" links to its manual entry
    editor(E),
    send(E, select, 'colour:ui_accent'),
    get(E, member, detail, D),
    send(D, goto_url, 'classvar:tab.indicator_colour'),
    get(@display?frames, find,
        message(@arg1, instance_of, class_variable_help_window), Help),
    get(Help, label, Title),
    send(Help, destroy).
test(usage_ui, Captions == ['A switch that is on',
                             'Focus ring of a text field',
                             'The current tab']) :-
    editor(E),
    send(E, select, 'colour:ui_accent'),
    usage_captions(E, Captions).
test(usage_via, true(memberchk('Margin of an editor (as ui_margin_background)',
                               Captions))) :-
    %  The dark theme sets ui_margin_background to ui_window_background
    editor(E),
    theme_colour_value(test_edited, ui_margin_background,
                       ui_window_background, _),
    send(E, select, 'colour:ui_window_background'),
    usage_captions(E, Captions).
test(usage_ansi, true(memberchk('Console output of class error', Captions))) :-
    editor(E),
    send(E, select, 'colour:ansi_red'),
    usage_captions(E, Captions).
test(console_sample, Texts == ['   ', 'Call', ': (12) member(X, [a,b]) ? ']) :-
    editor(E),
    send(E, select, 'console:port(call)'),
    detail(E, look_sample, S),
    get(S?graphicals, find_all, message(@arg1, instance_of, text), Ch),
    get(Ch, map, @arg1?string?value, Strings),
    chain_list(Strings, Texts).
test(pldoc_look, [ Setting == pldoc(span, class(autoload), [color(cyan)]),
                   true(sub_string(Style, _, _, _, "color(cyan)"))
                 ]) :-
    editor(E),
    send(E, select, 'pldoc:span-class(autoload)'),
    detail(E, look, L),
    send(L, user_selection, [color(cyan)]),
    theme_setting(test_edited, Setting),
    detail(E, effective, Effective),
    get(Effective, selection, Style0),
    get(Style0, value, Style).

test(create_invalid_name, Theme == test_edited) :-
    editor(E),
    send(E, create_theme, 'Bad name', dark),
    get(E, theme, Theme).
test(save, [Before,After] == [true,false]) :-
    editor(E),
    modified(test_edited, Before),
    send(E, save),
    modified(test_edited, After),
    assertion(file_contains(test_edited,
                            "pldoc_theme(span, class(autoload), \c
                             [color(cyan)])")).
test(revert, [Settings,Modified] == [[pldoc(span, class(autoload),
                                            [color(cyan)])], false]) :-
    editor(E),
    send(E, select, 'colour:syntax_comment'),
    send(E, set_colour, colour(red)),
    send(E, revert),
    findall(S, theme_setting(test_edited, S), Settings),
    modified(test_edited, Modified).
test(create_existing_name, Theme == test_edited) :-
    editor(E),
    send(E, create_theme, dark, light),
    get(E, theme, Theme).
test(create_theme, [ [Theme,Current,Base,Loaded] ==
                     [test_edited2,test_edited2,dark,true],
                     cleanup(( end_theme_edit(test_edited2),
                               edit_theme(test_edited),
                               send(@test_theme_editor, theme, test_edited),
                               apply_theme(test_edited)
                             ))
                   ]) :-
    %  The saved theme is loaded from its file again
    editor(E),
    send(E, create_theme, test_edited2, dark),
    get(E, theme, Theme),
    current_theme(Current),
    edited_theme(Theme, Base),
    (   \+ edited_theme(test_edited, _),
        pce_theme:colour(test_edited, _, _) ;
        prolog:theme(test_edited)
    ->  Loaded = true
    ;   Loaded = false
    ).
test(new_theme_name, Name == my_dark) :-
    pce_theme_editor:new_theme_name(dark, Name).

:- end_tests(theme_editor).
