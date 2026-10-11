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

:- module(pce_theme_editor,
          [ theme_editor/0,
            theme_editor/1              % +Theme
          ]).
:- use_module(library(pce)).
:- use_module(library(pce_theme),
              [ apply_theme/1, current_theme/1, available_theme/1,
                create_theme/2, edited_theme/2, set_theme_setting/2,
                reset_theme_setting/2, theme_setting/2, theme_issues/2,
                theme_colour_value/4, theme_colour_origin/2,
                theme_ancestor/2, semantic_colour_name/2, issue_level/2,
                load_theme_libraries/0, ensure_theme_colours/0
              ]).
:- use_module(library(pce_colour_item), [colour_trace_steps/3]).
:- use_module(library(doc/load)).
:- use_module(library(pce_class_variable_editor),
              [class_variable_help/3, class_variable_doc/3]).
:- use_module(doc(html)).
:- use_module(library(pce_theme_samples),
              [ colour_sample/3, colour_class_variable/3, console_sample/2 ]).
:- use_module(library(pce_theme_file),
              [ editable_theme/1, edit_theme/1, end_theme_edit/1,
                revert_theme/1, save_theme/1, save_theme_as/2,
                theme_modified/1, theme_source/2
              ]).
:- use_module(library(persistent_frame)).
:- use_module(library(help_message)).
:- use_module(library(pce_filter_item)).
:- use_module(library(lynx/pldoc_style), []).
:- pce_autoload(colour_editor, library(pce_colour_editor)).
:- autoload(library(apply), [exclude/3, convlist/3, foldl/4]).
:- autoload(library(lists), [member/2, append/2, append/3, max_list/2]).
:- autoload(library(solution_sequences), [limit/2, distinct/2]).
:- autoload(library(option), [merge_options/3]).
:- autoload(library(pairs), [pairs_values/2]).
:- autoload(library(pce_util), [default/3, chain_list/2]).

/** <module> Edit a colour theme

The theme editor creates a theme as a variation of another theme (see
create_theme/2) and edits its settings: the semantic colours of xpce
and the development tools, the colours of the Prolog console and the
styles of the online help.  The theme is applied while it is edited,
so every change is visible immediately.

The editor shows a list of all settings on the left and the selected
setting on the right.  The list can be restricted to a group of
settings, to the settings that are changed by the theme or that have
an issue (see theme_issues/2) and by a regular expression.
*/

:- multifile
    prolog:console_color/3,
    pldoc_style:theme/4.

%!  theme_editor is det.
%!  theme_editor(+Theme) is det.
%
%   Open the theme editor on Theme, by default the current theme.  If
%   Theme is a theme of the user (see editable_theme/1), edit it.
%   Otherwise ask for the name of a new theme based on Theme.

theme_editor :-
    current_theme(Theme),
    theme_editor(Theme).

theme_editor(Theme) :-
    new(E, theme_editor),
    send(E, open),
    (   edited_theme(Theme, _)
    ->  send(E, theme, Theme)
    ;   editable_theme(Theme)
    ->  edit_theme(Theme),
        send(E, theme, Theme)
    ;   send(E, new_theme, Theme)
    ).


                 /*******************************
                 *             ROWS             *
                 *******************************/

%   The rows of the editor are identified by a key, an atom that is
%   Kind:Text, where Kind is `colour`, `console` or `pldoc`.  Text is
%   the name of the colour, the console class or Element-Condition of
%   the online help.

row_key(colour(Name), Key) :-
    atom_concat('colour:', Name, Key).
row_key(console(Class), Key) :-
    term_text(Class, Text),
    atom_concat('console:', Text, Key).
row_key(pldoc(Element, Condition), Key) :-
    term_text(Element-Condition, Text),
    atom_concat('pldoc:', Text, Key).

key_row(Key, Row) :-
    sub_atom(Key, B, _, A, :),
    !,
    sub_atom(Key, 0, B, _, Kind),
    sub_atom(Key, _, A, 0, Text),
    text_row(Kind, Text, Row).

text_row(colour, Name, colour(Name)).
text_row(console, Text, console(Class)) :-
    term_to_atom(Class, Text).
text_row(pldoc, Text, pldoc(Element, Condition)) :-
    term_to_atom(Element-Condition, Text).

term_text(Term, Text) :-
    copy_term(Term, Copy),
    numbervars(Copy, 0, _, [singletons(true)]),
    format(atom(Text), '~W', [Copy, [quoted(true), numbervars(true)]]).

%!  theme_keys(+Theme, -Keys) is det.
%
%   Keys are the keys of all rows for editing Theme, ordered by group.

theme_keys(Theme, Keys) :-
    findall(K, ( editable_colour(Name),
                 row_key(colour(Name), K)
               ), CKeys0),
    sort(CKeys0, CKeys1),
    findall(G-K, ( member(K, CKeys1),
                   key_row(K, Row),
                   row_group(Row, G0),
                   group_rank(G0, G)
                 ), Pairs),
    keysort(Pairs, Sorted),
    pairs_values(Sorted, CKeys),
    findall(K, ( console_class(Theme, Class),
                 row_key(console(Class), K)
               ), ConsoleKeys0),
    sort(ConsoleKeys0, ConsoleKeys),
    findall(K, ( pldoc_element(Theme, E, C),
                 row_key(pldoc(E, C), K)
               ), PldocKeys0),
    sort(PldocKeys0, PldocKeys),
    append([CKeys, ConsoleKeys, PldocKeys], Keys).

%   editable_colour(?Name)
%
%   Name is a semantic colour that a theme can define.  Colours chosen
%   by the user (see adaptive_colour/3) are not.

editable_colour(Name) :-
    semantic_colour_name(Name, _),
    \+ theme_colour_origin(Name, adaptive(_)).

%   console_class(+Theme, -Class)
%
%   Class is a class of the console colours (prolog:console_color/3).
%   These are the classes of the default theme and those defined by
%   Theme and the themes it is based on.

console_class(_, Class) :-
    '$messages':default_theme(Class, _),
    nonvar(Class).
console_class(Theme, Class) :-
    theme_ancestor(Theme, Ancestor),
    prolog:console_color(Ancestor, Class, _),
    nonvar(Class).

%   pldoc_element(+Theme, -Element, -Condition)
%
%   The online help has a style for Element if Condition holds that
%   determines the colour or weight of the text.

pldoc_element(Theme, Element, Condition) :-
    html_text:style(Element, Condition, CSS),
    (   member(Property, CSS),
        look_property(Property)
    ->  true
    ;   theme_ancestor(Theme, Ancestor),
        pldoc_style:theme(Ancestor, Element, Condition, _)
    ->  true
    ).

look_property(color(_)).
look_property(background(_)).
look_property(font_weight(_)).

%   Groups of rows.

group(all,     'All settings').
group(ui,      'User interface').
group(ansi,    'Terminal colours').
group(syntax,  'PceEmacs syntax').
group(tools,   'Development tools').
group(console, 'Console').
group(pldoc,   'Online help').

group_rank(ui,     1).
group_rank(ansi,   2).
group_rank(syntax, 4).
group_rank(tools,  3).

row_group(colour(Name), Group) :-
    !,
    colour_group(Name, Group).
row_group(console(_), console).
row_group(pldoc(_,_), pldoc).

colour_group(Name, ui)     :- sub_atom(Name, 0, _, _, ui_), !.
colour_group(Name, ansi)   :- sub_atom(Name, 0, _, _, ansi_), !.
colour_group(Name, syntax) :- sub_atom(Name, 0, _, _, syntax_), !.
colour_group(_,    tools).

status(all,     'Any status').
status(changed, '✎ Changed').
status(issues,  '⚠ Issues').

%   own_setting(+Theme, +Row, -Setting)
%
%   Setting is the setting of Theme for Row.

own_setting(Theme, colour(Name), Setting) :-
    Setting = colour(Name, _),
    theme_setting(Theme, Setting).
own_setting(Theme, console(Class), Setting) :-
    Setting = console(C, _),
    theme_setting(Theme, Setting),
    C =@= Class.
own_setting(Theme, pldoc(Element, Condition), Setting) :-
    Setting = pldoc(Element, C, _),
    theme_setting(Theme, Setting),
    C =@= Condition.

%   reset_setting(+Row, -Setting)
%
%   Setting is the setting for Row with an unbound value.

reset_setting(colour(Name), colour(Name, _)).
reset_setting(console(Class), console(Class, _)).
reset_setting(pldoc(Element, Condition), pldoc(Element, Condition, _)).

%   row_status(+Theme, +Row, +Issues, -Status)
%
%   Status shows whether Theme changes Row and the most severe of its
%   issues as symbols.  See row_tooltip/4 for the explanation.

row_status(Theme, Row, Issues, Status) :-
    (   own_setting(Theme, Row, _)
    ->  Own = [changed]
    ;   Own = []
    ),
    row_issue_terms(Row, Issues, RowIssues),
    (   most_severe(RowIssues, Level)
    ->  Levels = [Level]
    ;   Levels = []
    ),
    append(Own, Levels, All),
    maplist(status_symbol, All, Symbols),
    atomic_list_concat(Symbols, ' ', Status).

status_symbol(changed,       '✎').
status_symbol(error,         '✖').
status_symbol(warning,       '⚠').
status_symbol(informational, 'ⓘ').

level_rank(error,         1).
level_rank(warning,       2).
level_rank(informational, 3).

most_severe(Issues, Level) :-
    findall(R-L, ( member(I, Issues),
                   issue_level(I, L),
                   level_rank(L, R)
                 ), Pairs),
    keysort(Pairs, [_-Level|_]).

row_issue_terms(colour(Name), Issues, RowIssues) :-
    !,
    include(issue_about(Name), Issues, RowIssues).
row_issue_terms(_, _, []).

issue_about(Name, Issue) :-
    arg(1, Issue, Name).

%   row_tooltip(+Theme, +Row, +Issues, -Tip)
%
%   Tip explains the status of Row in words.

row_tooltip(Theme, Row, Issues, Tip) :-
    (   own_setting(Theme, Row, _)
    ->  Own = ['✎ Changed by this theme']
    ;   Own = []
    ),
    row_issue_terms(Row, Issues, RowIssues),
    findall(Line, ( member(I, RowIssues),
                    issue_level(I, L),
                    status_symbol(L, Sym),
                    message_to_codes(pce_theme(Theme, I), Msg),
                    format(string(Line), '~w ~s', [Sym, Msg])
                  ), IssueLines),
    append(Own, IssueLines, Lines),
    atomic_list_concat(Lines, '\n', Tip).

row_name(colour(Name), Name).
row_name(console(Class), Text) :-
    term_text(Class, Text).
row_name(pldoc(Element, Condition), Text) :-
    (   Condition == true
    ->  term_text(Element, Text)
    ;   term_text(Condition, CText),
        format(atom(Text), '~w ~w', [Element, CText])
    ).

%   colour_summary(+Name, -Summary) is semidet.
%
%   Summary describes the theme colour Name.  Fails if Name is not (yet)
%   a theme colour, which happens for the colours of a library that is
%   loaded after the theme was applied.

colour_summary(Name, Summary) :-
    get(@colours, member, Name, Colour),  % convert warns if undefined
    send(Colour, instance_of, theme_colour),
    get(Colour, summary, Summary),
    Summary \== @nil.

%   row_info(+Theme, +Row, -Info)
%
%   Info describes Row: the summary of a colour or the attributes of
%   a console class or online help element.

row_info(_, colour(Name), Info) :-
    colour_summary(Name, Summary),
    !,
    get(Summary, value, Info).
row_info(_, colour(_), '') :-
    !.
row_info(Theme, console(Class), Info) :-
    !,
    console_value(Theme, Class, Attrs, _),
    term_text(Attrs, Info).
row_info(Theme, pldoc(Element, Condition), Info) :-
    pldoc_value(Theme, Element, Condition, CSS, _),
    term_text(CSS, Info).


                 /*******************************
                 *            VALUES            *
                 *******************************/

%!  console_value(+Theme, +Class, -Attributes, -Source) is det.
%
%   Attributes are the console attributes for Class in Theme.  Source
%   is theme(Definer), `default` or `none`.  Unlike the lookup when
%   printing, the class is matched as a variant.

console_value(Theme, Class, Attrs, theme(Ancestor)) :-
    theme_ancestor(Theme, Ancestor),
    prolog:console_color(Ancestor, C, Attrs0),
    C =@= Class,
    !,
    Attrs = Attrs0.
console_value(_, Class, Attrs, default) :-
    '$messages':default_theme(C, Attrs0),
    C =@= Class,
    !,
    Attrs = Attrs0.
console_value(_, _, [], none).

%!  pldoc_value(+Theme, +Element, +Condition, -CSS, -Source) is det.
%
%   CSS are the properties Theme adds to the online help style for
%   Element if Condition holds.  Source is theme(Definer) or `none`.

pldoc_value(Theme, Element, Condition, CSS, theme(Ancestor)) :-
    theme_ancestor(Theme, Ancestor),
    pldoc_style:theme(Ancestor, Element, C, CSS0),
    C =@= Condition,
    !,
    CSS = CSS0.
pldoc_value(_, _, _, [], none).

%   pldoc_effective(+Theme, +Element, +Condition, -CSS)
%
%   CSS is the style of Element if Condition holds in Theme: the
%   properties of the theme merged with the default style.

pldoc_effective(Theme, Element, Condition, CSS) :-
    pldoc_value(Theme, Element, Condition, ThemeCSS, _),
    (   html_text:style(Element, C, Current),
        C =@= Condition
    ->  true
    ;   Current = []
    ),
    current_theme(CurrentTheme),
    pldoc_value(CurrentTheme, Element, Condition, CurrentCSS, _),
    exclude(theme_property(CurrentCSS), Current, Default),
    merge_options(ThemeCSS, Default, CSS).

theme_property(ThemeCSS, Property) :-
    memberchk(Property, ThemeCSS).

%   inherited_text(+Theme, +Row, -Text)
%
%   Text tells where Row gets its value if Theme does not define it.

inherited_text(Theme, Row, Text) :-
    edited_theme(Theme, Base),
    (   own_setting(Theme, Row, _)
    ->  Own = 'Changed by this theme.  '
    ;   Own = ''
    ),
    inherited_value(Base, Row, Value, Source),
    source_text(Source, SourceText),
    format(string(Text), '~wInherited: ~w (~w)', [Own, Value, SourceText]).

inherited_value(Base, colour(Name), Value, Source) :-
    theme_colour_value(Base, Name, Value, Source).
inherited_value(Base, console(Class), Text, Source) :-
    console_value(Base, Class, Attrs, Source),
    term_text(Attrs, Text).
inherited_value(Base, pldoc(Element, Condition), Text, Source) :-
    pldoc_value(Base, Element, Condition, CSS, Source),
    term_text(CSS, Text).

source_text(theme(Theme), Text) :-
    format(atom(Text), '~w theme', [Theme]).
source_text(default, 'default').
source_text(none, 'not set').


                 /*******************************
                 *            LOOKS             *
                 *******************************/

%   The look of console text and online help is a list of attributes.
%   The console uses the ANSI attributes of ansi_format/3, the online
%   help CSS properties.  These are translated to ANSI attributes as
%   library(lynx/html_style) does, so we can show both the same way.

ansi_name(0, black).
ansi_name(1, red).
ansi_name(2, green).
ansi_name(3, yellow).
ansi_name(4, blue).
ansi_name(5, magenta).
ansi_name(6, cyan).
ansi_name(7, white).

%!  look_attributes(+Dialect, +Attrs, -AnsiAttrs) is det.

look_attributes(console, Attrs, Attrs).
look_attributes(pldoc, CSS, Attrs) :-
    convlist(css_ansi, CSS, Attrs).

css_ansi(font_weight(bold), bold).
css_ansi(color(BC), hfg(C)) :-
    atom(BC), atom_concat(bright_, C, BC), !.
css_ansi(color(C), fg(C)).
css_ansi(background(BC), hbg(C)) :-
    atom(BC), atom_concat(bright_, C, BC), !.
css_ansi(background(C), bg(C)).

%!  look(+AnsiAttrs, -Look) is det.
%
%   Look is look(Fg, Bg, Bold, Italic, Underline), where Fg and Bg are
%   colour names or @default.

look(Attrs, look(Fg, Bg, Bold, Italic, Underline)) :-
    look_colour(foreground, Attrs, Fg),
    look_colour(background, Attrs, Bg),
    flag(bold, Attrs, Bold),
    flag(italic, Attrs, Italic),
    flag(underline, Attrs, Underline).

look_colour(Which, Attrs, Colour) :-
    member(A, Attrs),
    attr_colour(Which, A, Colour0),
    !,
    Colour = Colour0.
look_colour(_, _, @default).

flag(Flag, Attrs, Value) :-
    (   memberchk(Flag, Attrs)
    ->  Value = @on
    ;   Value = @off
    ).

attr_colour(foreground, fg(C), Colour) :- ansi_colour(C, normal, Colour).
attr_colour(foreground, hfg(C), Colour) :- ansi_colour(C, bright, Colour).
attr_colour(foreground, fg8(S), Colour) :- xterm_colour(S, Colour).
attr_colour(foreground, fg(R,G,B), Colour) :- rgb_colour(R, G, B, Colour).
attr_colour(background, bg(C), Colour) :- ansi_colour(C, normal, Colour).
attr_colour(background, hbg(C), Colour) :- ansi_colour(C, bright, Colour).
attr_colour(background, bg8(S), Colour) :- xterm_colour(S, Colour).
attr_colour(background, bg(R,G,B), Colour) :- rgb_colour(R, G, B, Colour).

ansi_colour(default, _, @default) :- !.
ansi_colour(Name, normal, Colour) :-
    ansi_name(_, Name), !,
    atom_concat(ansi_, Name, Colour).
ansi_colour(Name, bright, Colour) :-
    ansi_name(_, Name), !,
    atom_concat(ansi_bright_, Name, Colour).
ansi_colour(Name, _, Name) :-
    atom(Name).

%   xterm_colour(+Spec, -Colour)
%
%   Colour of the xterm 256-colour palette.

xterm_colour(h(Name), Colour) :- !,
    ansi_colour(Name, bright, Colour).
xterm_colour(Name, Colour) :-
    atom(Name), !,
    ansi_colour(Name, normal, Colour).
xterm_colour(N, Colour) :-
    integer(N), N >= 0, N < 8, !,
    ansi_name(N, Name),
    ansi_colour(Name, normal, Colour).
xterm_colour(N, Colour) :-
    integer(N), N >= 8, N < 16, !,
    I is N - 8,
    ansi_name(I, Name),
    ansi_colour(Name, bright, Colour).
xterm_colour(N, Colour) :-
    integer(N), N >= 16, N < 232, !,
    I is N - 16,
    cube_level(I // 36, R),
    cube_level((I // 6) mod 6, G),
    cube_level(I mod 6, B),
    rgb_colour(R, G, B, Colour).
xterm_colour(N, Colour) :-
    integer(N), N >= 232, N < 256,
    V is 8 + 10*(N-232),
    rgb_colour(V, V, V, Colour).

cube_level(Expr, V) :-
    I is Expr,
    (   I =:= 0
    ->  V = 0
    ;   V is 55 + 40*I
    ).

rgb_colour(R, G, B, Colour) :-
    format(atom(Colour), '#~|~`0t~16r~2+~|~`0t~16r~2+~|~`0t~16r~2+',
           [R,G,B]).

%   look_icon(+AnsiAttrs, -Image)
%
%   Image shows the colours of AnsiAttrs as a small text sample.

look_icon(Attrs, Image) :-
    look(Attrs, look(Fg0, Bg0, Bold, _, _)),
    default_colour(Fg0, ui_window_foreground, Fg),
    default_colour(Bg0, ui_window_background, Bg),
    new(Image, image(@nil, 22, 16, pixmap)),
    send(Image, fill, Bg),
    (   Bold == @on
    ->  Font = bold
    ;   Font = normal
    ),
    new(T, text('Ab', center, Font)),
    send(T, colour, Fg),
    get(T?area, width, TW),
    get(T?area, height, TH),
    X is (22-TW)//2,
    Y is (16-TH)//2,
    send(Image, draw_in, T, point(X, Y)),
    send(Image, draw_in, new(Border, box(22, 16))),
    send(Border, colour, grey50).

%   colour_icon(+Name, -Image)
%
%   Image shows the colour Name.  If Name is not a colour, e.g., a
%   colour of a library that is not loaded, the image is empty.

colour_icon(Name, Image) :-
    new(Image, image(@nil, 22, 16, pixmap)),
    (   get(@colours, member, Name, Colour)
    ->  send(Image, fill, Colour)
    ;   true
    ),
    send(Image, draw_in, new(Border, box(22, 16))),
    send(Border, colour, grey50).

default_colour(@default, Default, Default) :- !.
default_colour(Colour, _, Colour).


                 /*******************************
                 *          LOOK ITEM           *
                 *******************************/

%   Choices for the foreground and background colours.  The value of a
%   menu item is the text of the attribute or `none`.

colour_choice(Dialect, Which, Value, Label) :-
    Value = none, Label = 'None'
    ;   default_choice(Dialect, Which, Attr),
        term_text(Attr, Value),
        Label = 'Default'
    ;   member(Kind, [normal, bright]),
        ansi_name(_, Name),
        choice_attr(Dialect, Which, Kind, Name, Attr),
        term_text(Attr, Value),
        colour_label(Kind, Name, Label).

default_choice(console, foreground, fg(default)).
default_choice(console, background, bg(default)).

choice_attr(console, foreground, normal, C, fg(C)).
choice_attr(console, foreground, bright, C, hfg(C)).
choice_attr(console, background, normal, C, bg(C)).
choice_attr(console, background, bright, C, hbg(C)).
choice_attr(pldoc,   foreground, normal, C, color(C)).
choice_attr(pldoc,   foreground, bright, C, color(BC)) :-
    atom_concat(bright_, C, BC).
choice_attr(pldoc,   background, normal, C, background(C)).
choice_attr(pldoc,   background, bright, C, background(BC)) :-
    atom_concat(bright_, C, BC).

%   custom_attr(+Dialect, +Which, +Hex, -Attr)

custom_attr(console, foreground, Hex, fg(Hex)).
custom_attr(console, background, Hex, bg(Hex)).
custom_attr(pldoc,   foreground, Hex, color(Hex)).
custom_attr(pldoc,   background, Hex, background(Hex)).

colour_label(Kind, Name, Label) :-
    upcase_first(Name, Name1),
    (   Kind == bright
    ->  atom_concat('Bright ', Name, Label)
    ;   Label = Name1
    ).

upcase_first(Name, Up) :-
    sub_atom(Name, 0, 1, _, F),
    sub_atom(Name, 1, _, 0, R),
    upcase_atom(F, UF),
    atom_concat(UF, R, Up).

%   colour_attr(+Dialect, +Which, +Attr)
%
%   Attr sets the foreground or background colour.

colour_attr(console, foreground, fg(_)).
colour_attr(console, foreground, hfg(_)).
colour_attr(console, foreground, fg8(_)).
colour_attr(console, foreground, fg(_,_,_)).
colour_attr(console, background, bg(_)).
colour_attr(console, background, hbg(_)).
colour_attr(console, background, bg8(_)).
colour_attr(console, background, bg(_,_,_)).
colour_attr(pldoc,   foreground, color(_)).
colour_attr(pldoc,   background, background(_)).

%   flag_attr(?Dialect, ?Flag, ?Attr)

flag_attr(console, bold,      bold).
flag_attr(console, italic,    italic).
flag_attr(console, underline, underline).
flag_attr(console, faint,     faint).
flag_attr(pldoc,   bold,      font_weight(bold)).

controlled_attr(Dialect, Attr) :-
    (   colour_attr(Dialect, _, Attr)
    ;   flag_attr(Dialect, _, Attr)
    ),
    !.

:- pce_begin_class(text_look_item, dialog_group,
                   "Edit the colours and style of text").

variable(dialect,   {console,pldoc}, get,  "Syntax of the attributes").
variable(selection, prolog,          get,  "List of attributes").
variable(message,   code*,           both, "Called with the new attributes").
variable(edit_message, code*,        both, "Called with start, ok or cancel").

initialise(G, Name:name, Dialect:{console,pldoc}, Msg:[code]*) :->
    "Create from name, attribute syntax and message"::
    send_super(G, initialise, Name, group),
    send(G, slot, dialect, Dialect),
    default(Msg, @nil, TheMsg),
    send(G, message, TheMsg),
    send(G, slot, selection, []),
    forall(member(Which, [foreground, background]),
           ( new(M, menu(Which, cycle, message(G, controls_changed))),
             forall(colour_choice(Dialect, Which, Value, Label),
                    send(M, append, menu_item(Value, @default, Label))),
             send(M, append, menu_item(other, @default, 'Other…')),
             send(G, append, M)
           )),
    send(G, append, new(F, menu(style, toggle,
                                message(G, controls_changed)))),
    forall(flag_attr(Dialect, Flag, _),
           send(F, append, Flag)),
    send(F, columns, 2),
    send(G, append,
         new(TI, text_item(attributes, '',
                           message(G, text_changed, @arg1)))),
    send(TI, length, 30),
    (   Dialect == console
    ->  send(TI, help_message, tag,
             'ANSI attributes as used by ansi_format/3')
    ;   send(TI, help_message, tag,
             'CSS properties, e.g., color(bright_cyan)')
    ).

selection(G, Attrs:prolog) :->
    "Set the attributes"::
    send(G, slot, selection, Attrs),
    send(G, show_controls).

show_controls(G) :->
    "Make the controls show the attributes"::
    get(G, dialect, Dialect),
    get(G, selection, Attrs),
    forall(member(Which, [foreground, background]),
           ( get(G, member, Which, M),
             (   member(A, Attrs),
                 colour_attr(Dialect, Which, A)
             ->  term_text(A, Value),
                 (   get(M, member, Value, _)
                 ->  true
                 ;   get(M?members, size, N),
                     Before is N,           % before `other`
                     send(M, insert_before,
                          menu_item(Value, @default, Value),
                          ?(M?members, nth1, Before))
                 )
             ;   Value = none
             ),
             send(M, selection, Value)
           )),
    get(G, member, style, F),
    findall(Flag, ( flag_attr(Dialect, Flag, A),
                    memberchk(A, Attrs)
                  ), Flags),
    (   Flags == []
    ->  send(F, selection, @nil)
    ;   FlagChain =.. [chain|Flags],
        send(F, selection, FlagChain)
    ),
    get(G, member, attributes, TI),
    term_text(Attrs, Text),
    send(TI, selection, Text).

controls_changed(G) :->
    "The user changed a control"::
    (   member(Which, [foreground, background]),
        get(G, member, Which, M),
        get(M, selection, other)
    ->  send(G, show_controls),
        send(G, ask_colour, Which)
    ;   get(G, dialect, Dialect),
        get(G, selection, Old),
        exclude(controlled_attr(Dialect), Old, Others),
        get(G, member, style, F),
        get(F, selection, FlagChain),
        chain_list(FlagChain, Flags),
        findall(A, ( member(Flag, Flags),
                     flag_attr(Dialect, Flag, A)
                   ), FlagAttrs),
        findall(A, ( member(Which, [foreground, background]),
                     get(G, member, Which, M),
                     get(M, selection, Value),
                     Value \== none,
                     term_to_atom(A, Value)
                   ), ColourAttrs),
        append([FlagAttrs, ColourAttrs, Others], New),
        send(G, user_selection, New)
    ).

%   The colour editor changes the look while the user edits the colour.
%   As colour_item, we call <-edit_message with `start`, `ok` or
%   `cancel`, or restore the attributes ourselves on `cancel`.  For the
%   console, the editor shows the 256 colours of the terminal.

ask_colour(G, Which:{foreground,background}) :->
    "Ask for a colour that is not one of the ANSI colours"::
    get(G, dialect, Dialect),
    get(G, selection, Old),
    current_colour(Dialect, Which, Old, Colour, Index),
    get(G, edit_message, EditMsg),
    (   EditMsg == @nil
    ->  EndMsg = message(G, end_edit, @arg1, prolog(Old))
    ;   send(EditMsg, forward, start),
        EndMsg = EditMsg
    ),
    new(E, colour_editor(Colour,
                         message(G, custom_colour, Which, @arg1, @arg2),
                         EndMsg)),
    (   Dialect == console
    ->  xterm_palette(Palette),
        send(E, palette, Palette),
        (   Index == @nil
        ->  true
        ;   send(E, palette_selection, Index)
        )
    ;   true
    ),
    (   get(G, frame, Frame)
    ->  send(E, transient_for, Frame),
        send(E, modal, transient)
    ;   true
    ),
    send(E, open).

%   current_colour(+Dialect, +Which, +Attrs, -Colour, -Index)
%
%   Colour is the foreground or background colour of Attrs and Index
%   its index in the 256 colours of the terminal or @nil.

current_colour(Dialect, Which, Attrs, Colour, Index) :-
    member(A, Attrs),
    colour_attr(Dialect, Which, A),
    attr_colour(Which, A, Name),
    Name \== @default,
    catch(get(@pce, convert, Name, colour, Colour), _, fail),
    !,
    (   Dialect == console,
        attr_index(A, Index0)
    ->  Index = Index0
    ;   Index = @nil
    ).
current_colour(_, Which, _, Colour, @nil) :-
    which_default(Which, Name),
    get(@pce, convert, Name, colour, Colour).

which_default(foreground, ui_window_foreground).
which_default(background, ui_window_background).

attr_index(fg(Name), I)  :- ansi_name(I, Name).
attr_index(bg(Name), I)  :- ansi_name(I, Name).
attr_index(hfg(Name), I) :- ansi_name(I0, Name), I is I0+8.
attr_index(hbg(Name), I) :- ansi_name(I0, Name), I is I0+8.
attr_index(fg8(Spec), I) :- xterm_index(Spec, I).
attr_index(bg8(Spec), I) :- xterm_index(Spec, I).

xterm_index(h(Name), I) :- !, ansi_name(I0, Name), I is I0+8.
xterm_index(Name, I)    :- atom(Name), !, ansi_name(I, Name).
xterm_index(I, I)       :- integer(I), between(0, 255, I).

%   xterm_palette(-Colours)
%
%   Colours is a vector of the 256 colours of the terminal.  The first
%   16 are the theme colours ansi_black ... ansi_bright_white.

xterm_palette(Palette) :-
    new(Palette, vector),
    forall(between(0, 255, I),
           ( xterm_colour(I, Name),
             get(@pce, convert, Name, colour, Colour),
             send(Palette, append, Colour)
           )).

custom_colour(G, Which:{foreground,background}, Colour:colour,
              Index:int*) :->
    "The user selected a colour from the colour editor"::
    get(G, dialect, Dialect),
    (   Index \== @nil
    ->  index_attr(Which, Index, Attr)
    ;   get(Colour, red, R), get(Colour, green, Gr), get(Colour, blue, B),
        rgb_colour(R, Gr, B, Hex),
        custom_attr(Dialect, Which, Hex, Attr)
    ),
    get(G, selection, Old),
    exclude(colour_attr(Dialect, Which), Old, Rest),
    send(G, user_selection, [Attr|Rest]).

%   index_attr(+Which, +Index, -Attr)
%
%   Attr selects colour Index of the terminal palette.  We use the
%   colour names for the first 16.

index_attr(Which, Index, Attr) :-
    (   Index < 16
    ->  I is Index mod 8,
        ansi_name(I, Name),
        (   Index < 8
        ->  Kind = normal
        ;   Kind = bright
        ),
        once(choice_attr(console, Which, Kind, Name, Attr))
    ;   xterm_attr(Which, Index, Attr)
    ).

xterm_attr(foreground, Index, fg8(Index)).
xterm_attr(background, Index, bg8(Index)).

end_edit(G, How:{ok,cancel}, Old:prolog) :->
    "The colour editor closed; Cancel restores the attributes"::
    (   How == cancel,
        get(G, selection, Current),
        Current \== Old
    ->  send(G, user_selection, Old)
    ;   true
    ).

text_changed(G, Text:name) :->
    "The user typed the attributes"::
    (   catch(term_string(Attrs, Text), _, fail),
        is_list(Attrs)
    ->  send(G, user_selection, Attrs)
    ;   send(G, report, error, 'Not a list of attributes: %s', Text)
    ).

user_selection(G, Attrs:prolog) :->
    "The user changed the attributes"::
    send(G, selection, Attrs),
    get(G, message, Msg),
    (   Msg == @nil
    ->  true
    ;   send(Msg, forward, prolog(Attrs))
    ).

:- pce_end_class(text_look_item).


%   Sample of a text look.  The look is given as ANSI attributes.  The
%   sample is a line of console output, where the segments s(Text) use
%   the look and the others the default colours (see console_sample/2).

:- pce_begin_class(look_sample, device,
                   "Show a text in a look").

variable(segments, prolog, get, "Segments of the sample line").

initialise(S, Segments:[prolog]) :->
    send_super(S, initialise),
    default(Segments, [s('?- member(X, [a,b]).')], Segs),
    send(S, slot, segments, Segs),
    send(S, display, new(B, box(400, 28))),
    send(B, name, background),
    send(B, pen, 1),
    send(B, colour, grey50),
    send(B, fill, ui_window_background),
    forall(member(Segment, Segs),
           display_segment(S, Segment)),
    send(S, look, []).

%   A segment in the look is a text with a box behind it for the
%   background colour.  The box is displayed just before the text.

display_segment(S, s(Text)) :-
    !,
    send(S, display, new(Bg, box)),
    send(Bg, pen, 0),
    send(Bg, name, look_background),
    send(S, display, new(T, text(Text, left, fixed))),
    send(T, name, look_text).
display_segment(S, Text) :-
    send(S, display, new(T, text(Text, left, fixed))),
    send(T, name, plain_text),
    send(T, colour, ui_window_foreground).

look(S, Attrs:prolog) :->
    "Show the look of Attrs"::
    look(Attrs, look(Fg0, Bg0, Bold, Italic, Underline)),
    default_colour(Fg0, ui_window_foreground, Fg),
    (   Bold == @on
    ->  Style = bold
    ;   Italic == @on
    ->  Style = italic
    ;   Style = normal
    ),
    get(@pce, convert, fixed, font, Fixed),
    get(Fixed, family, Family),
    get(Fixed, points, Points),
    new(Font, font(Family, Style, Points)),
    (   Bg0 == @default
    ->  Fill = @nil
    ;   Fill = Bg0
    ),
    get(S?graphicals, copy, Grs),
    chain_list(Grs, List),
    foldl(look_graphical(Fg, Fill, Font, Underline), List, 6-[], X-_),
    get(S, member, background, B),
    W is max(400, X+6),
    foldl(text_height, List, 0, TH),
    H is max(28, TH+10),
    send(B, size, size(W, H)).

text_height(Gr, H0, H) :-
    (   send(Gr, instance_of, text)
    ->  get(Gr, height, TH),
        H is max(H0, TH)
    ;   H = H0
    ).

%   look_graphical(+Fg, +Fill, +Font, +Underline, +Gr, +X0-Bgs0, -X-Bgs)
%
%   Place the graphicals from left to right, giving the look to the
%   look_text segments.  Bgs holds the box that goes behind the next
%   text.

look_graphical(Fg, Fill, Font, Underline, Gr, X0-Bgs0, X-Bgs) :-
    get(Gr, name, Name),
    look_graphical(Name, Gr, Fg, Fill, Font, Underline, X0, Bgs0, X, Bgs).

look_graphical(background, _, _, _, _, _, X, Bgs, X, Bgs).
look_graphical(look_background, Bg, _, Fill, _, _, X, _, X, [Bg]) :-
    send(Bg, fill, Fill).
look_graphical(look_text, T, Fg, _, Font, Underline, X0, Bgs, X, []) :-
    send(T, colour, Fg),
    send(T, font, Font),
    send(T, underline, Underline),
    place_text(T, Bgs, X0, X).
look_graphical(plain_text, T, _, _, _, _, X0, Bgs, X, []) :-
    place_text(T, Bgs, X0, X).

place_text(T, Bgs, X0, X) :-
    send(T, position, point(X0, 5)),
    get(T, width, W),
    get(T, height, H),
    forall(member(Bg, Bgs), send(Bg, area, area(X0, 5, W, H))),
    X is X0+W.

:- pce_end_class(look_sample).


                 /*******************************
                 *            DETAILS           *
                 *******************************/

%   The details of a setting.  Parts of it are HTML, whose links call
%   ->goto_url on the window (see library(doc/html)).  A link is the
%   key of a row or classvar:Class.Name for a class variable.

:- pce_begin_class(theme_detail, dialog,
                   "Details of a setting of the theme editor").

goto_url(D, URL:name, _Dir:[{forward,backward}]) :->
    "Select the row of Key or show a class variable"::
    (   atom_concat('classvar:', Var, URL)
    ->  atomic_list_concat([Class, Name], '.', Var),
        (   class_variable_help(Class, Name, D?frame)
        ->  true
        ;   send(D, report, warning, 'No manual entry for %s', Var)
        )
    ;   send(D?frame, goto_key, URL)
    ).

:- pce_end_class(theme_detail).


                 /*******************************
                 *            EDITOR            *
                 *******************************/

:- pce_begin_class(theme_editor, persistent_frame,
                   "Edit a colour theme").

variable(theme,   name*,  get, "Theme being edited").
variable(current, name*,  get, "Key of the selected row").
variable(filter,  regex*, get, "Filter for the rows").
variable(edit_origin, prolog, get, "Setting before the colour editor opened").
variable(updating, bool := @off, get, "We are changing a setting").

initialise(F) :->
    "Create the editor"::
    send_super(F, initialise, 'Theme editor'),
    send(F, slot, edit_origin, none),
    load_theme_libraries,
    ensure_theme_colours,
    send(F, append, new(Top, dialog)),
    send(Top, name, top),
    send(Top, gap, size(5, 5)),
    send(Top, append, new(L, label(theme, 'No theme'))),
    send(L, font, bold),
    send(Top, append, new(New, button(new_theme, message(F, new_theme))),
         right),
    send(New, label, 'New theme…'),
    send(New, help_message, tag, 'Create a new theme based on a theme'),
    send(New, alignment, right),
    send(Top, append, new(G, menu(group, cycle, message(F, fill)))),
    send(G, show_label, @off),
    forall(group(Id, Label), send(G, append, menu_item(Id, @default, Label))),
    send(Top, append, new(S, menu(status, cycle, message(F, fill))), right),
    send(S, show_label, @off),
    forall(status(Id, Label), send(S, append, menu_item(Id, @default, Label))),
    send(Top, append,
         new(Filter, filter_item(filter, message(F, filter, @arg1),
                                 'Filter')), right),
    send(Filter, show_label, @off),
    new(B, browser(@default, size(70, 25))),
    send(B, name, rows),
    send(B, select_message, message(F, select, @arg1?key)),
    new(D, theme_detail),
    send(D, name, detail),
    send(D, size, size(540, 400)),
    get(D, tile, Tile),                 % a dialog does not resize by
    send(Tile, hor_stretch, 100),       % default, which makes the gap
    send(Tile, hor_shrink, 100),        % with the list fixed.  Set this
    send(Tile, ver_stretch, 100),       % before relating the tiles, as
    send(Tile, ver_shrink, 100),        % that computes the layout
    send(D, right, B),                  % first, so Top and Bottom span
                                        % the list and the details
    send(B, below, Top),
    send(new(Bottom, dialog), below, B),
    send(Bottom, name, bottom),
    send(Bottom, append, new(Save, button(save, message(F, save)))),
    send(Save, help_message, tag, 'Save the theme to its file'),
    send(Bottom, append, new(SaveAs, button(save_as, message(F, save_as)))),
    send(SaveAs, label, 'Save as…'),
    send(SaveAs, help_message, tag, 'Save the theme as a new theme'),
    send(Bottom, append, new(Revert, button(revert, message(F, revert)))),
    send(Revert, help_message, tag, 'Undo the changes since the last save'),
    send(Bottom, append, new(EditFile, button(edit_file,
                                               message(F, edit_file)))),
    send(EditFile, label, 'Edit theme file'),
    send(EditFile, help_message, tag, 'Open the file of the theme in the editor'),
    send(Bottom, append, button(quit, message(F, quit))),
    send(Bottom, append, label(reporter), right),
    send(F, done_message, message(F, quit)).

theme(F, Theme:name) :->
    "Edit Theme, created using create_theme/2"::
    edited_theme(Theme, _),
    send(F, slot, theme, Theme),
    send(F, slot, current, @nil),
    send(F, update_title),
    send(F, fill),
    send(F, show_detail).

update_title(F) :->
    "Show the theme, its base and whether it is saved"::
    get(F, theme, Theme),
    edited_theme(Theme, Base),
    (   Base == none
    ->  format(string(Title0), 'Theme ~w', [Theme])
    ;   format(string(Title0), 'Theme ~w, based on ~w', [Theme, Base])
    ),
    (   \+ theme_source(Theme, _)
    ->  string_concat(Title0, ' (not saved)', Title)
    ;   theme_modified(Theme)
    ->  string_concat(Title0, ' (modified)', Title)
    ;   Title = Title0
    ),
    get(F, member, top, Top),
    get(Top, member, theme, L),
    send(L, selection, Title),
    get(F, member, bottom, Bottom),
    get(Bottom, member, edit_file, EditFile),
    (   theme_source(Theme, _)
    ->  send(EditFile, active, @on)
    ;   send(EditFile, active, @off)
    ).

edit_file(F) :->
    "Open the file of the theme in the editor"::
    get(F, theme, Theme),
    (   theme_source(Theme, File)
    ->  (   theme_modified(Theme)
        ->  send(F, report, warning,
                 'The file does not have the changes that are not saved')
        ;   true
        ),
        edit(file(File))
    ;   send(F, report, warning, 'The theme has not been saved')
    ).

filter(F, Filter:regex*) :->
    "Show only the rows that match Filter"::
    send(F, slot, filter, Filter),
    send(F, fill).

fill(F) :->
    "Fill the list of settings"::
    get(F, member, rows, B),
    send(B, clear),
    get(F, theme, Theme),
    (   Theme == @nil
    ->  true
    ;   get(F, member, top, Top),
        get(Top, member, group, GM),
        get(GM, selection, Group),
        get(Top, member, status, SM),
        get(SM, selection, Status),
        get(F, filter, Filter),
        theme_issues(Theme, Issues),
        theme_keys(Theme, Keys),
        findall(Key-Row,
                ( member(Key, Keys),
                  key_row(Key, Row),
                  show_row(Theme, Group, Status, Filter, Issues, Row)
                ), Shown),
        pairs_values(Shown, Rows),
        tab_stops(B, Theme, Issues, Rows),
        forall(member(Key-Row, Shown),
               send(F, append_row, Key, Row, Issues)),
        get(F, current, Current),
        (   Current \== @nil,
            get(B, member, Current, _)
        ->  send(B, selection, Current),
            send(B, normalise, Current)
        ;   true
        )
    ).

%   tab_stops(+Browser, +Theme, +Issues, +Rows)
%
%   Set the tab stops of Browser for the columns name, status and info
%   of Rows.  If no row has a status, the info follows the name.  The
%   second stop must be after the first, as a tab moves to the first
%   stop past the current position.

tab_stops(B, Theme, Issues, Rows) :-
    get(B, font, Font),
    get(Font, width, '  ', Gap),
    icon_width(IconW),
    findall(W, ( member(Row, Rows),
                 row_name(Row, Name),
                 atom_concat('  ', Name, Text),
                 get(Font, width, Text, W)
               ), NameWs),
    max_list([0|NameWs], NameW),
    findall(W, ( member(Row, Rows),
                 row_status(Theme, Row, Issues, Status),
                 Status \== '',
                 get(Font, width, Status, W)
               ), StatusWs),
    max_list([0|StatusWs], StatusW),
    Tab1 is IconW + NameW + Gap,
    (   StatusW > 0                     % symbols may come from another
    ->  Tab2 is Tab1 + StatusW + 2*Gap  % font, wider than measured
    ;   Tab2 is Tab1 + 1
    ),
    send(B?text_image, tab_stops, vector(Tab1, Tab2)).

icon_width(24).                         % icon of look_icon/2 and the gap

show_row(Theme, Group, Status, Filter, Issues, Row) :-
    (   Group == all
    ->  true
    ;   row_group(Row, Group)
    ),
    (   Status == all
    ->  true
    ;   Status == changed
    ->  own_setting(Theme, Row, _)
    ;   row_issue_terms(Row, Issues, [_|_])
    ),
    (   Filter == @nil
    ->  true
    ;   row_name(Row, Name),
        row_info(Theme, Row, Info),
        format(string(Text), '~w ~w', [Name, Info]),
        send(Filter, search, Text)
    ).

append_row(F, Key:name, Row:prolog, Issues:prolog) :->
    "Append the row for Key"::
    get(F, theme, Theme),
    get(F, member, rows, B),
    row_text(Theme, Row, Issues, Label, Icon),
    send(B, style, Key, style(icon := Icon)),
    send(B, append, new(DI, dict_item(Key, Label, @default, Key))),
    row_tooltip(Theme, Row, Issues, Tip),
    (   Tip == ''
    ->  true
    ;   send(DI, help_message, tag, Tip)
    ).

row_text(Theme, Row, Issues, Label, Icon) :-
    row_name(Row, Name),
    row_status(Theme, Row, Issues, Status),
    row_info(Theme, Row, Info),
    row_icon(Theme, Row, Icon),
    format(string(Label), '  ~w\t~w\t~w', [Name, Status, Info]).

row_icon(_, colour(Name), Icon) :-
    !,
    colour_icon(Name, Icon).
row_icon(Theme, console(Class), Icon) :-
    !,
    console_value(Theme, Class, Attrs, _),
    look_icon(Attrs, Icon).
row_icon(Theme, pldoc(Element, Condition), Icon) :-
    pldoc_effective(Theme, Element, Condition, CSS),
    look_attributes(pldoc, CSS, Attrs),
    look_icon(Attrs, Icon).

select(F, Key:name) :->
    "Show the setting of Key"::
    send(F, slot, current, Key),
    send(F, show_detail).

goto_key(F, Key:name) :->
    "Select the row of Key and show it"::
    get(F, member, rows, B),
    (   get(B, member, Key, DI)
    ->  send(B, selection, DI),
        send(B, normalise, DI)
    ;   send(B, selection, @nil)
    ),
    send(F, select, Key).

show_detail(F) :->
    "Show the selected setting in the detail window"::
    get(F, member, detail, D),
    get(D?graphicals, copy, Grs),
    send(Grs, for_all, message(@arg1, destroy)),
    get(F, current, Key),
    (   Key == @nil
    ->  send(D, append, label(none, 'Select a setting', italic))
    ;   key_row(Key, Row),
        detail_items(Row, F, D),
        send(F, update_detail)
    ),
    send(D, layout).

detail_items(colour(Name), F, D) :-
    send(D, append, new(T, label(title, Name))),
    send(T, font, bold),
    get(@pce, convert, Name, colour, Colour),
    (   colour_summary(Name, Summary)
    ->  send(D, append, new(S, text(Summary, left))),
        send(S, margin, 500, wrap)
    ;   true
    ),
    send(D, append,
         new(CI, colour_item(value, Colour, message(F, set_colour, @arg1)))),
    send(CI, edit_message, message(F, edit_session, @arg1)),
    detail_common(F, D),
    send(D, append, new(G, dialog_group(derivation, box))),
    send(G, label, 'Derivation'),
    send(G, append, new(Trace, pbox(480, left))),
    send(Trace, name, trace),
    get(F, theme, Theme),
    usage_items(Theme, Name, D).
detail_items(console(Class), F, D) :-
    term_text(Class, Text),
    send(D, append, new(T, label(title, Text))),
    send(T, font, bold),
    send(D, append, new(S, text('Colours of the Prolog console for \c
                                 messages of this class.', left))),
    send(S, margin, 500, wrap),
    console_sample(Class, Segments),
    send(D, append, look_sample(Segments)),
    send(D, append, new(L, text_look_item(look, console,
                                          message(F, set_look, @arg1)))),
    send(L, edit_message, message(F, edit_session, @arg1)),
    detail_common(F, D).
detail_items(pldoc(Element, Condition), F, D) :-
    row_name(pldoc(Element, Condition), Text),
    send(D, append, new(T, label(title, Text))),
    send(T, font, bold),
    send(D, append, new(S, text('Style of the online help (help/1) \c
                                 for this element.  The properties \c
                                 are merged with the default style.',
                                 left))),
    send(S, margin, 500, wrap),
    send(D, append, new(look_sample)),
    send(D, append, new(L, text_look_item(look, pldoc,
                                          message(F, set_look, @arg1)))),
    send(L, edit_message, message(F, edit_session, @arg1)),
    send(D, append, label(effective, '')),
    detail_common(F, D).

%   usage_items(+Theme, +Name, +Dialog)
%
%   Show what the colour Name is used for: a few samples of the user
%   interface and the class variables that refer to it.

max_samples(4).

usage_items(Theme, Name, D) :-
    max_samples(Max),
    findall(Sample, limit(Max, usage_sample(Theme, Name, Sample)), Samples),
    findall(Var, limit(3, usage_class_variable(Name, Var)), Vars),
    (   Samples == [],
        Vars == []
    ->  true
    ;   send(D, append, new(G, dialog_group(usage, box))),
        send(G, label, 'Used for'),
        forall(member(Caption-Gr, Samples),
               ( send(G, append, new(L, label(caption, Caption))),
                 send(L, font, italic),
                 send(G, append, Gr)
               )),
        (   Vars == []
        ->  true
        ;   class_variables_html(Vars, HTML),
            send(G, append, new(PB, pbox(450, left))),
            show_html(PB, HTML)
        )
    ).

%   usage_sample(+Theme, +Name, -Sample) is nondet.
%
%   Sample is Caption-Graphical, showing a use of the colour Name.
%   This is a sample of the user interface, a sample of a colour whose
%   value is Name, or console output in the theme if Name is a terminal
%   colour.

usage_sample(_, Name, Caption-Gr) :-
    colour_sample(Name, Caption, Gr).
usage_sample(Theme, Name, Caption-Gr) :-
    semantic_colour_name(Other, _),
    Other \== Name,
    theme_colour_value(Theme, Other, Value, _),
    Value == Name,
    colour_sample(Other, Caption0, Gr),
    format(atom(Caption), '~w (as ~w)', [Caption0, Other]).
usage_sample(Theme, Name, Caption-Gr) :-
    sub_atom(Name, 0, _, _, ansi_),
    distinct(Class, console_class(Theme, Class)),
    console_value(Theme, Class, Attrs, _),
    \+ \+ ( member(A, Attrs),
             attr_colour(_, A, Name)
           ),
    console_sample(Class, Segments),
    new(Gr, look_sample(Segments)),
    send(Gr, look, Attrs),
    format(atom(Caption), 'Console output of class ~p', [Class]).

usage_class_variable(Name, Class-Variable) :-
    colour_class_variable(Name, Class, Variable).

%   class_variables_html(+Vars, -HTML)
%
%   HTML lists the class variables Vars.  A class variable with a
%   manual entry is a link to it.

class_variables_html(Vars, ['Class variables: '|Links]) :-
    foldl(class_variable_link, Vars, Links0, []),
    Links0 = [_Sep|Links].              % drop the leading ", "

class_variable_link(Class-Name) -->
    { atomic_list_concat([Class, Name], '.', Var),
      Code = element(code, [], [Var])
    },
    (   { class_variable_doc(Class, Name, _) }
    ->  { atom_concat('classvar:', Var, URL) },
        [ ', ', element(a, [href=URL], [Code]) ]
    ;   [ ', ', Code ]
    ).

detail_common(F, D) :-
    send(D, append, label(inherited, '')),
    send(D, append, new(R, button(reset, message(F, reset)))),
    send(R, label, 'Use inherited value'),
    send(R, help_message, tag,
         'Remove the setting from this theme').

update_detail(F) :->
    "Update the detail window after a change"::
    get(F, current, Key),
    (   Key == @nil
    ->  true
    ;   get(F, theme, Theme),
        get(F, member, detail, D),
        key_row(Key, Row),
        update_detail(Row, Theme, D),
        inherited_text(Theme, Row, Inherited),
        get(D, member, inherited, IL),
        send(IL, selection, Inherited),
        get(D, member, reset, R),
        (   own_setting(Theme, Row, _)
        ->  send(R, active, @on)
        ;   send(R, active, @off)
        )
    ).

update_detail(colour(Name), Theme, D) :-
    get(D, member, value, CI),
    theme_colour_value(Theme, Name, Value, _),
    (   catch(get(@pce, convert, Value, colour, Colour), _, fail)
    ->  send(CI, selection, Colour)
    ;   true
    ),
    get(@pce, convert, Name, colour, TC),
    colour_trace_steps(TC, Steps, _),
    trace_html(Steps, HTML),
    get(D, member, derivation, G),
    get(G, member, trace, T),
    send(T, clear),
    show_html(T, HTML).

update_detail(console(Class), Theme, D) :-
    console_value(Theme, Class, Attrs, _),
    get(D, member, look, L),
    send(L, selection, Attrs),
    get(D, member, look_sample, S),
    send(S, look, Attrs).
update_detail(pldoc(Element, Condition), Theme, D) :-
    pldoc_value(Theme, Element, Condition, CSS, _),
    get(D, member, look, L),
    send(L, selection, CSS),
    pldoc_effective(Theme, Element, Condition, Effective),
    look_attributes(pldoc, Effective, Attrs),
    get(D, member, look_sample, S),
    send(S, look, Attrs),
    term_text(Effective, EText),
    format(string(Label), 'Style: ~w', [EText]),
    get(D, member, effective, E),
    send(E, selection, Label).

%   show_html(+PBox, +HTML)
%
%   Render HTML in PBox, a pbox of library(doc/load).

show_html(PB, HTML) :-
    new(Mode, doc_mode),
    send(Mode, link_colour, ui_link),
    send(Mode, alignment, left),
    send(PB, show, HTML, Mode).

%   trace_html(+Steps, -HTML)
%
%   HTML describes how a colour gets its value (see colour_trace_steps/3)
%   for the pbox renderer of library(doc/load).  Each step is a line.  The
%   theme colours that are steps are links that select the colour.  The
%   HTML is not a paragraph, which would start with an empty line.

trace_html(Steps, Lines) :-
    Steps = [step(First, _, _)|_],
    foldl(step_html(First), Steps, Lines0, []),
    Lines0 = [_BR|Lines].               % drop the leading <br>

step_html(First, step(Name, Value, Why)) -->
    [ element(br, [], []) ],
    colour_html(First, Name),
    [ ' = ' ],
    colour_html(First, Value),
    swatch(Value),
    (   { Why == '' }
    ->  []
    ;   [ ' ', element(i, [], ['(', Why, ')']) ]
    ).

colour_html(First, Name) -->
    { Name \== First,
      editable_colour(Name),
      !,
      row_key(colour(Name), Key)
    },
    [ element(a, [href=Key], [element(code, [], [Name])]) ].
colour_html(_, Name) -->
    [ element(code, [], [Name]) ].

swatch(Value) -->
    { catch(get(@pce, convert, Value, colour, Colour), _, fail),
      !,
      new(B, box(12, 12)),
      send(B, fill, Colour),
      send(B, colour, grey50),
      new(G, grbox(B, center))
    },
    [ ' ', G ].
swatch(_) -->
    [].

set_colour(F, Colour:colour) :->
    "The user selected a colour for the current setting"::
    get(F, current, Key),
    key_row(Key, colour(Name)),
    colour_value(Colour, Value),
    (   Value == Name
    ->  send(F, report, error, 'A colour cannot refer to itself')
    ;   send(F, set_setting, colour(Name, Value))
    ).

colour_value(Colour, Value) :-
    get(Colour, name, Value0),
    atom(Value0),
    !,
    Value = Value0.
colour_value(Colour, Value) :-
    get(Colour, red, R), get(Colour, green, G), get(Colour, blue, B),
    rgb_colour(R, G, B, Value).

set_look(F, Attrs:prolog) :->
    "The user changed the look of the current setting"::
    get(F, current, Key),
    key_row(Key, Row),
    look_setting(Row, Attrs, Setting),
    send(F, set_setting, Setting).

look_setting(console(Class), Attrs, console(Class, Attrs)).
look_setting(pldoc(Element, Condition), CSS, pldoc(Element, Condition, CSS)).

set_setting(F, Setting:prolog) :->
    "Change a setting of the theme"::
    get(F, theme, Theme),
    (   catch(own_change(F, set_theme_setting(Theme, Setting)), E, true)
    ->  (   var(E)
        ->  send(F, changed)
        ;   message_to_codes(E, Msg),
            send(F, report, error, '%s', Msg),
            send(F, update_detail)
        )
    ;   send(F, report, error, 'Could not change the setting')
    ).

reset(F) :->
    "Remove the current setting from the theme"::
    get(F, theme, Theme),
    get(F, current, Key),
    key_row(Key, Row),
    reset_setting(Row, Setting),
    own_change(F, reset_theme_setting(Theme, Setting)),
    send(F, changed).

%   own_change(+Frame, :Goal)
%
%   Goal changes a setting.  If this changes a colour of the active
%   theme, all frames receive ->colours_changed.  We update ourselves
%   using ->changed, which, while the colour editor is open, only
%   updates the row of the setting.

own_change(F, Goal) :-
    setup_call_cleanup(
        send(F, slot, updating, @on),
        Goal,
        send(F, slot, updating, @off)).

%   While the colour editor is open, it changes the setting with every
%   move of the mouse.  We only update the row of the setting then and
%   fill the list when the editor closes.  Cancel restores the setting
%   as it was when the editor opened.

edit_session(F, What:{start,ok,cancel}) :->
    "The colour editor for the current setting opened or closed"::
    get(F, theme, Theme),
    get(F, current, Key),
    key_row(Key, Row),
    edit_session(What, F, Theme, Row).

edit_session(start, F, Theme, Row) :-
    (   own_setting(Theme, Row, Setting)
    ->  Origin = origin(Row, Setting)
    ;   Origin = origin(Row, none)
    ),
    send(F, slot, edit_origin, Origin).
edit_session(ok, F, _, _) :-
    send(F, slot, edit_origin, none),
    send(F, changed).
edit_session(cancel, F, Theme, Row) :-
    get(F, edit_origin, Origin),
    send(F, slot, edit_origin, none),
    (   Origin = origin(Row, Setting)
    ->  own_change(F, restore_setting(Theme, Row, Setting))
    ;   true
    ),
    send(F, changed).

restore_setting(Theme, Row, none) :-
    !,
    reset_setting(Row, Setting),
    reset_theme_setting(Theme, Setting).
restore_setting(Theme, _, Setting) :-
    set_theme_setting(Theme, Setting).

changed(F) :->
    "A setting of the theme changed"::
    send(F, update_title),
    (   get(F, edit_origin, origin(_,_))
    ->  get(F, current, Key),
        send(F, update_row, Key)
    ;   send(F, fill)
    ),
    send(F, update_detail).

update_row(F, Key:name) :->
    "Update the row of Key in the list"::
    get(F, member, rows, B),
    (   get(B, member, Key, DI)
    ->  get(F, theme, Theme),
        key_row(Key, Row),
        theme_issues(Theme, Issues),
        row_text(Theme, Row, Issues, Label, Icon),
        send(B, style, Key, style(icon := Icon)),
        send(DI, label, Label),
        row_tooltip(Theme, Row, Issues, Tip),
        (   Tip == ''
        ->  send(DI, help_message, tag, @nil)
        ;   send(DI, help_message, tag, Tip)
        )
    ;   true
    ).

save(F) :->
    "Save the theme"::
    get(F, theme, Theme),
    (   catch(save_theme(Theme), E, true)
    ->  (   var(E)
        ->  send(F, update_title),
            theme_source(Theme, File),
            send(F, report, status, 'Saved to %s', File)
        ;   message_to_codes(E, Msg),
            send(F, report, error, '%s', Msg),
            fail
        )
    ;   send(F, report, error, 'Could not save the theme'),
        fail
    ).

save_as(F) :->
    "Save the theme under a new name"::
    get(F, theme, Theme),
    new_theme_name(Theme, Default),
    (   ask_name(F, 'Save theme as', Default, Name)
    ->  (   \+ valid_theme_name(Name)
        ->  send(F, report, error,
                 'A theme name consists of lowercase letters, digits and _')
        ;   \+ unused_theme_name(Name)
        ->  send(F, report, error, 'Theme %s already exists', Name)
        ;   catch(save_theme_as(Theme, Name), E, true)
        ->  (   var(E)
            ->  send(F, theme, Name),
                theme_source(Name, File),
                send(F, report, status, 'Saved to %s', File)
            ;   message_to_codes(E, Msg),
                send(F, report, error, '%s', Msg)
            )
        ;   send(F, report, error, 'Could not save the theme')
        )
    ;   true
    ).

revert(F) :->
    "Undo the changes since the theme was saved"::
    get(F, theme, Theme),
    revert_theme(Theme),
    send(F, changed).

quit(F) :->
    "Stop editing and close the editor"::
    (   send(F, close_theme)
    ->  send(F, destroy)
    ;   true
    ).

close_theme(F) :->
    "Stop editing the theme; fail if the user cancels"::
    get(F, theme, Theme),
    (   Theme == @nil
    ->  true
    ;   (   theme_modified(Theme)
        ->  ask_save(F, Theme, Answer),
            (   Answer == save
            ->  send(F, save)
            ;   Answer == discard
            )
        ;   true
        ),
        end_theme_edit(Theme),
        send(F, slot, theme, @nil)
    ).

ask_save(F, Theme, Answer) :-
    new(D, dialog('Save theme')),
    format(string(Text), 'Theme ~w has changes that are not saved.', [Theme]),
    send(D, append, label(message, Text)),
    send(D, append, button(save, message(D, return, save))),
    send(D, append, button(discard, message(D, return, discard))),
    send(D, append, button(cancel, message(D, return, cancel))),
    send(D, default_button, save),
    send(D, transient_for, F),
    get(D, confirm_centered, F?area?center, Answer),
    send(D, destroy).

ask_name(F, Title, Default, Name) :-
    new(D, dialog(Title)),
    send(D, append, new(N, text_item(name, Default))),
    send(D, append, button(ok, message(D, return, ok))),
    send(D, append, button(cancel, message(D, return, cancel))),
    send(D, default_button, ok),
    send(D, transient_for, F),
    get(D, confirm_centered, F?area?center, Answer),
    get(N, selection, NameS),
    send(D, destroy),
    Answer == ok,
    atom_string(Name, NameS).

colours_changed(F) :->
    "The colours changed: update the icons"::
    (   get(F, updating, @off),         % see own_change/2
        get(F, theme, Theme), Theme \== @nil
    ->  send(F, fill),
        send(F, update_detail)
    ;   true
    ).

message_to_codes(E, Msg) :-
    message_to_codes(E, [], Codes),
    string_codes(Msg, Codes).

message_to_codes(E, _, Codes) :-
    catch(message_to_codes_(E, Codes), _, fail),
    !.
message_to_codes(E, _, Codes) :-
    format(codes(Codes), '~p', [E]).

message_to_codes_(E, Codes) :-
    '$messages':translate_message(E, Lines, []),
    with_output_to(codes(Codes), print_message_lines(current_output, '', Lines)).

new_theme(F, Base0:[name]) :->
    "Ask for a new theme based on Base0 and edit it"::
    (   Base0 == @default
    ->  (   get(F, theme, Base), Base \== @nil
        ->  true
        ;   current_theme(Base)
        )
    ;   Base = Base0
    ),
    new_theme_name(Base, Default),
    new(D, dialog('New theme')),
    send(D, append, new(N, text_item(name, Default))),
    send(D, append, new(M, menu(based_on, cycle))),
    forall(base_theme(T), send(M, append, T)),
    (   get(M, member, Base, _)
    ->  send(M, selection, Base)
    ;   true
    ),
    send(D, append, button(create, message(D, return, create))),
    send(D, append, button(cancel, message(D, return, cancel))),
    send(D, default_button, create),
    send(D, transient_for, F),
    get(D, confirm_centered, F?area?center, Answer),
    (   Answer == create
    ->  get(N, selection, NameS),
        get(M, selection, TheBase),
        send(D, destroy),
        atom_string(Name, NameS),
        send(F, create_theme, Name, TheBase)
    ;   send(D, destroy),
        (   get(F, theme, @nil)
        ->  send(F, destroy)
        ;   true
        )
    ).

create_theme(F, Name:name, Base:name) :->
    "Create a new theme Name based on Base and edit it"::
    (   \+ valid_theme_name(Name)
    ->  send(F, report, error,
             'A theme name consists of lowercase letters, digits and _')
    ;   \+ unused_theme_name(Name)
    ->  send(F, report, error, 'Theme %s already exists', Name)
    ;   send(F, close_theme)
    ->  (   catch(create_theme(Name, Base), E, true)
        ->  (   var(E)
            ->  apply_theme(Name),
                send(F, theme, Name)
            ;   message_to_codes(E, Msg),
                send(F, report, error, '%s', Msg)
            )
        ;   true
        )
    ;   true
    ).

valid_theme_name(Name) :-
    atom_codes(Name, [H|T]),
    code_type(H, lower),
    forall(member(C, T),
           ( code_type(C, csym),
             \+ code_type(C, upper)
           )).

base_theme(Theme) :-
    available_theme(Theme).

new_theme_name(Base, Name) :-
    atom_concat(my_, Base, Name0),
    (   unused_theme_name(Name0)
    ->  Name = Name0
    ;   between(2, infinite, I),
        format(atom(Name), '~w_~w', [Name0, I]),
        unused_theme_name(Name)
    ->  true
    ).

unused_theme_name(Name) :-
    \+ available_theme(Name),
    \+ edited_theme(Name, _).

:- pce_end_class(theme_editor).
