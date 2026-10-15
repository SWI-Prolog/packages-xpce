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


:- module(pce_theme_samples,
          [ colour_sample/3,            % +Name, -Caption, -Graphical
            colour_class_variable/3,    % +Name, -Class, -Variable
            console_sample/2            % +Class, -Segments
          ]).
:- use_module(library(pce)).
:- use_module(library(pce_report), []).    % class reporter
:- use_module(library(pce_theme), [syntax_colour/3, syntax_colour_name/3]).
:- autoload(library(apply), [maplist/3, exclude/3]).
:- autoload(library(lists),
            [member/2, max_list/2, nth1/3, append/3]).
:- autoload(library(pce_util), [chain_list/2, member_chain/2]).

/** <module> Show what a theme colour is used for

The theme editor shows, for the selected colour, a few small samples of
the user interface that use it: a button for the face of buttons, a few
lines of Prolog code for a syntax colour, etc.  The samples are real
xpce objects that refer to the colour by name, so they change with the
colour while the user edits it.
*/

%!  colour_sample(+Name, -Caption, -Graphical) is nondet.
%
%   Graphical is a sample of the user interface that uses the theme
%   colour Name.  Caption explains the sample.

colour_sample(Name, Caption, Gr) :-
    sample(Name, Kind, Caption),
    build(Kind, Gr).
colour_sample(Name, Caption, Gr) :-
    sub_atom(Name, 0, _, _, syntax_),
    syntax_colour(Name, Class, _),
    syntax_sample(Name, Before, Fragment, After, Caption),
    code_sample(Before, Fragment, After, syntax(Class), Gr).


                 /*******************************
                 *         USER INTERFACE       *
                 *******************************/

%   sample(?Name, ?Kind, ?Caption)
%
%   Name is shown by the sample Kind.  Kind is built by build/2.

sample(ui_accelerator,          accelerator,    'Accelerator key of a label').
sample(ui_accent,               toggle,         'A switch that is on').
sample(ui_accent,               text_item,      'Focus ring of a text field').
sample(ui_accent,               tabs,           'The current tab').
sample(ui_button_background,    button,         'Face of a button').
sample(ui_button_background,    choice,         'Segmented choice').
sample(ui_button_foreground,    button,         'Label of a button').
sample(ui_button_pressed,       pressed_button, 'A button while pressed').
sample(ui_cursor,               cursor,         'Text cursor').
sample(ui_danger,               danger,         'Buttons that start a destructive action').
sample(ui_danger_foreground,    danger,         'Label of the default danger button').
sample(ui_cursor_inactive,      inactive_cursor,
       'Text cursor of a window without the keyboard focus').
sample(ui_error,                report(error),  'An error message').
sample(ui_warning,              report(warning), 'A warning message').
sample(ui_code_background,      code_text,      'Code in running text').
sample(ui_dialog_background,    dialog,         'A dialog window').
sample(ui_dialog_foreground,    dialog,         'Text of a dialog window').
sample(ui_fold,                 terminal(fold), 'Folded output of a command in a terminal').
sample(ui_inactive,             inactive,       'Items that cannot be used').
sample(ui_isearch_background,   isearch,        'Incremental search').
sample(ui_isearch_other_background, isearch,    'Incremental search').
sample(ui_isearch_failed_background, failed_isearch,
       'Incremental search for text that cannot be found').
sample(ui_link,                 terminal(link), 'A hyperlink in a terminal').
sample(ui_margin_background,    margin,         'Margin of an editor').
sample(ui_placeholder,          placeholder,    'An empty text field').
sample(ui_scrollbar_background, list,           'Trough of the scroll bar').
sample(ui_selection_background, list,           'Selected item of a list').
sample(ui_selection_background, choice,         'Selected segment').
sample(ui_selection_foreground, list,           'Selected item of a list').
sample(ui_separator,            group,          'Border of a group').
sample(ui_separator,            list,           'Border of a list').
sample(ui_shadow,               shadow,         'Shadow').
sample(ui_text_selection_background, selection, 'Selected text').
sample(ui_text_selection_background, terminal(selection),
       'Selected text in a terminal').
sample(ui_tile_gap,             panes,          'Gap between two panes').
sample(ui_tile_border,          panes,          'Border around the panes').
sample(ui_tile_separator,       panes,          'Line in the gap between two panes').
sample(ui_tooltip_background,   tooltip,        'Balloon help').
sample(ui_tooltip_foreground,   tooltip,        'Balloon help').
sample(ui_window_background,    editor,         'An editor').
sample(ui_window_background,    list,           'A list').
sample(ui_window_foreground,    editor,         'Text in an editor').
sample(ui_window_background,    terminal(text), 'A terminal').
sample(ui_window_foreground,    terminal(text), 'Text in a terminal').
sample(help_link,               help_link,      'Link in the xpce help').
sample(emacs_bookmark_hit,      bookmark,       'Bookmark found by a search').
sample(emacs_bookmark_filter,   bookmark_filter, 'Text of a hit matching the filter').
sample(debug_enabled_background, debug_topic,   'Enabled debug topic').
sample(xref_node_background,    xref_node,      'File in the dependency graph').
sample(xref_node_foreground,    xref_node,      'File in the dependency graph').
sample(xref_header_background,  xref_header,    'Header of a file overview').
sample(xref_header_foreground,  xref_header,    'Header of a file overview').
sample(Name, port(Port), Caption) :-
    port_colour(Port, Name),
    format(atom(Caption), 'Source line at the ~w port of the debugger',
           [Port]).
sample(Name, xref_predicate(Colour), Caption) :-
    xref_colour(Colour, Name, Caption).
sample(Name, thread_graph(Name), 'Memory and CPU of a thread') :-
    sub_atom(Name, 0, _, _, thread_graph_).

report_sample(error,   'Cannot open "data.csv": no such file').
report_sample(warning, 'File "data.csv" is not saved').

port_colour(call,      debug_port_call).
port_colour(break,     debug_port_break).
port_colour(exit,      debug_port_exit).
port_colour(redo,      debug_port_redo).
port_colour(fail,      debug_port_fail).
port_colour(exception, debug_port_exception).
port_colour(unify,     debug_port_unify).
port_colour(choice,    debug_port_choice).
port_colour(frame,     debug_port_frame).

xref_colour(xref_predicate,  xref_predicate,  'Predicate in a file overview').
xref_colour(xref_autoload,   xref_autoload,   'Autoloaded predicate').
xref_colour(xref_global,     xref_global,     'Predicate in module user').
xref_colour(xref_undefined,  xref_undefined,  'Undefined predicate').
xref_colour(xref_not_called, xref_not_called, 'Predicate that is not called').

%   build(+Kind, -Graphical)

build(button, B) :-
    new(B, button(button, @nil)),
    send(B, label, 'Button').
build(danger, D) :-
    new(D, dialog_group(danger, group)),
    send(D, append, new(B1, button(delete, @nil))),
    send(B1, danger, @on),
    send(D, append, new(B2, button(delete_all, @nil)), right),
    send(B2, danger, @on),
    send(B2, slot, default_button, @on).
build(report(Kind), R) :-
    new(R, reporter),
    report_sample(Kind, Message),
    send(R, report, Kind, Message).
build(code_text, T) :-
    new(T, markup_text('Bound to <tt>forward_char</tt> in \
<tt>key_binding.editor</tt>')).
build(failed_isearch, R) :-             % as editor.c
    new(R, reporter),
    new(M, markup('Failing ISearch: \
<span foreground="ui_window_foreground" \
background="ui_isearch_background">%s</span>\
<span foreground="ui_window_foreground" \
background="ui_isearch_failed_background">%s</span>',
                  'bar(', 'Z')),
    send(R, report, warning, M).
build(pressed_button, B) :-
    new(B, button(pressed, @nil)),
    send(B, status, preview).
build(inactive, D) :-
    new(D, dialog_group(inactive, group)),
    send(D, append, new(B, button(inactive, @nil))),
    send(B, active, @off),
    send(D, append, new(M, menu(option, toggle, @nil)), right),
    send(M, show_label, @off),
    send(M, append, option),
    send(M, active, @off).
build(toggle, M) :-
    new(M, menu(options, toggle, @nil)),
    send(M, show_label, @off),
    send(M, append, on),
    send(M, append, off),
    send(M, selection, on).
build(choice, M) :-
    new(M, menu(choice, choice, @nil)),
    send(M, show_label, @off),
    send(M, append, one),
    send(M, append, two),
    send(M, append, three),
    send(M, selection, two).
build(text_item, TI) :-
    new(TI, text_item(name, 'Some text', @nil)),
    send(TI, length, 20).
build(placeholder, TI) :-
    new(TI, text_item(search, '', @nil)),
    send(TI, show_label, @off),
    send(TI, placeholder, 'Search…'),
    send(TI, length, 20).
build(tabs, T) :-
    new(T, tab_stack),
    send(T, append, new(T1, tab(first))),
    send(T, append, tab(second)),
    send(T1, size, size(200, 10)).
build(list, LB) :-
    new(LB, list_browser(@default, 20, 3)),
    forall(member(Fruit, [apple, banana, cherry, date, elderberry]),
           send(LB, append, Fruit)),
    send(LB, selection, banana).
build(group, G) :-
    new(G, dialog_group(group, box)),
    send(G, label, 'Group'),
    send(G, append, label(text, 'Items of the group')).
build(editor, E) :-
    code_editor('foo(X) :-\n    bar(X, Y),\n    baz(Y).', E).
build(selection, E) :-
    code_sample('foo(X) :-\n    ', 'bar(X, Y)', ',\n    baz(Y).',
                style(background := ui_text_selection_background), E).
build(isearch, E) :-
    code_editor('foo(X) :-\n    bar(X, Y),\n    bar(Y, Z).', E),
    send(E, style, current, style(background := ui_isearch_background)),
    send(E, style, other, style(background := ui_isearch_other_background)),
    get(E, text_buffer, TB),
    new(_, fragment(TB, 14, 3, current)),
    new(_, fragment(TB, 30, 3, other)).
build(margin, E) :-
    code_editor('foo(X) :-\n    bar(X).', E),
    send(E, margin_width, 22).
build(cursor, D) :-
    new(D, device),
    send(D, display, new(T, text('Some text', left, normal))),
    get(T?area, width, W),
    get(T?area, height, H),
    X is W+1,
    send(D, display, new(C, box(2, H)), point(X, 0)),
    send(C, pen, 0),
    send(C, fill, ui_cursor).
build(inactive_cursor, E) :-
    code_editor('foo(X).', E),
    send(E, caret, 3),
    send(E?text_cursor, displayed, @on).
build(accelerator, D) :-
    new(D, device),
    send(D, display, new(T, text('Open', left, normal))),
    get(T?font, width, 'O', W),
    get(T?font, ascent, A),
    Y is round(A)+2,
    send(D, display, new(L, line(0, Y, W, Y))),
    send(L, colour, ui_accelerator).
build(dialog, D) :-
    new(D, device),
    send(D, display, new(B, box(200, 30))),
    send(B, fill, ui_dialog_background),
    send(B, colour, ui_separator),
    send(D, display, new(T, text('Text in a dialog', left, normal)),
         point(8, 6)),
    send(T, colour, ui_dialog_foreground).
build(shadow, B) :-
    new(B, box(120, 24)),
    send(B, fill, ui_window_background),
    send(B, shadow, 3).
build(panes, D) :-
    new(D, device),
    send(D, display, new(Border, box(220, 76))),
    send(Border, pen, 0),
    send(Border, fill, ui_tile_border),
    send(D, display, new(G, box(204, 60)), point(8, 8)),
    send(G, pen, 0),
    send(G, fill, ui_tile_gap),
    send(D, display, new(L, box(96, 60)), point(8, 8)),
    send(D, display, new(R, box(96, 60)), point(116, 8)),
    forall(member(B, [L, R]),
           ( send(B, fill, ui_window_background),
             send(B, colour, ui_separator)
           )),
    send(D, display, new(Sep, line(110, 12, 110, 64))),
    send(Sep, colour, ui_tile_separator).
build(tooltip, D) :-
    new(D, device),
    new(T, text('Help on an item', left, normal)),
    send(T, colour, ui_tooltip_foreground),
    get(T?area, width, TW),
    get(T?area, height, TH),
    W is TW+12, H is TH+6,
    send(D, display, new(B, box(W, H))),
    send(B, fill, ui_tooltip_background),
    send(B, colour, ui_separator),
    send(D, display, T, point(6, 3)).
build(help_link, T) :-
    link_text(help_link, T).
build(bookmark, E) :-
    code_sample('foo(X) :- ', 'bar(X)', '.',
                style(background := emacs_bookmark_hit), E).
build(bookmark_filter, E) :-
    code_sample('foo(X) :- ', 'bar(X)', '.',
                style(background := emacs_bookmark_filter), E).
build(debug_topic, LB) :-
    new(LB, list_browser(@default, 20, 2)),
    send(LB, style, enabled, style(background := debug_enabled_background)),
    send(LB, append, dict_item(http, http, @default, enabled)),
    send(LB, append, dict_item(lists, lists)).
build(port(Port), E) :-
    port_colour(Port, Name),
    code_sample('foo(X) :-\n    ', 'bar(X, Y)', ',\n    baz(Y).',
                style(background := Name), E).
build(xref_node, D) :-
    new(D, device),
    new(T, text('lists.pl', left, normal)),
    send(T, colour, xref_node_foreground),
    get(T?area, width, TW),
    get(T?area, height, TH),
    W is TW+10, H is TH+6,
    send(D, display, new(B, box(W, H))),
    send(B, fill, xref_node_background),
    send(D, display, T, point(5, 3)).
build(xref_header, D) :-
    new(D, device),
    send(D, display, new(B, box(200, 22))),
    send(B, fill, xref_header_background),
    send(B, pen, 0),
    send(D, display, new(T, text('Module lists', left, bold)), point(5, 3)),
    send(T, colour, xref_header_foreground).
build(xref_predicate(Colour), T) :-
    new(T, text('append/3', left, normal)),
    send(T, colour, Colour).
build(thread_graph(Colour), D) :-
    new(D, device),
    send(D, display, new(B, box(200, 60))),
    send(B, fill, ui_window_background),
    send(B, colour, ui_separator),
    send(D, display, new(Axis, line(8, 52, 192, 52))),
    send(Axis, colour, ui_window_foreground),
    findall(C, thread_graph_colour(C), Colours0),
    exclude(==(Colour), Colours0, Others),
    append(Others, [Colour], Colours),          % the sample on top
    forall(nth1(I, Colours, C),
           ( graph_points(I, Points),
             new(P, path),
             forall(member(X-Y, Points), send(P, append, point(X, Y))),
             send(P, colour, C),
             send(D, display, P)
           )).

%   A terminal shows what a command wrote, using the styles of class
%   terminal_image.  The OSC 8 escape sequence makes a hyperlink and
%   the OSC 133 marks make the commands that can be folded.

build(terminal(What), T) :-
    terminal_lines(What, Lines),
    length(Lines, N),
    get(@pce, convert, tt, font, Font),
    get(Font, height, FH),
    get(Font, avg_char_width, CW),
    W is round(40*CW),
    H is (N+1)*FH,
    new(T, terminal_image(W, H)),
    terminal_output(What, T).

thread_graph_colour(thread_graph_local).
thread_graph_colour(thread_graph_global).
thread_graph_colour(thread_graph_trail).
thread_graph_colour(thread_graph_cpu).

%   graph_points(+Nth, -Points)
%
%   Points of the Nth line of the graph of a thread, as the thread
%   monitor draws them.

graph_points(Nth, Points) :-
    findall(X-Y, ( between(0, 23, I),
                   X is 8 + I*8,
                   Y is round(44 - 6*Nth - 8*sin(I/3 + Nth))
                 ), Points).

terminal_lines(text,      ['?- X = 42.', 'X = 42.']).
terminal_lines(link,      ['?- help(member/2).', 'See https://www.swi-prolog.org']).
terminal_lines(selection, ['?- atom_length(abc, L).', 'L = 3.']).
terminal_lines(fold,      ['?- numlist(1, 3, L).', '?- ']).

terminal_output(text, T) :-
    send(T, insert, '?- X = 42.\r\nX = 42.\r\n').
terminal_output(selection, T) :-
    send(T, insert, '?- atom_length(abc, L).\r\nL = 3.\r\n'),
    send(T, selection, 3, 22).
terminal_output(link, T) :-
    URL = 'https://www.swi-prolog.org',
    format(atom(Link), '\e]8;;~w\e\\~w\e]8;;\e\\', [URL, URL]),
    atomic_list_concat(['?- help(member/2).\r\nSee ', Link, '\r\n'], Text),
    send(T, insert, Text).
terminal_output(fold, T) :-
    send(T, prompt_mark, prompt),
    send(T, insert, '?- '),
    send(T, prompt_mark, input),
    send(T, insert, 'numlist(1, 3, L).\r\n'),
    send(T, prompt_mark, output),
    send(T, insert, 'L = [1,2,3].\r\n'),
    send(T, prompt_mark, end),
    send(T, prompt_mark, prompt),
    send(T, insert, '?- '),
    send(T, fold_all).

link_text(Colour, T) :-
    new(T, text('https://www.swi-prolog.org', left, normal)),
    send(T, colour, Colour),
    send(T, underline, @on).


                 /*******************************
                 *             CODE             *
                 *******************************/

%   code_sample(+Before, +Fragment, +After, +Style, -Editor)
%
%   Editor shows Before, Fragment and After, where Fragment uses Style.
%   Style is a style object or syntax(Class), the style PceEmacs uses
%   for Class.

code_sample(Before, Fragment, After, Style, E) :-
    atomic_list_concat([Before, Fragment, After], Text),
    code_editor(Text, E),
    atom_length(Before, Start),
    atom_length(Fragment, Len),
    sample_style(Style, StyleObj),
    send(E, style, sample, StyleObj),
    get(E, text_buffer, TB),
    new(_, fragment(TB, Start, Len, sample)).

code_editor(Text, E) :-
    split_string(Text, "\n", "", Lines),
    length(Lines, Height),
    maplist(string_length, Lines, Lengths),
    max_list(Lengths, Max),
    Width is max(Max+2, 20),
    new(E, editor(@default, Width, Height)),
    send(E, contents, Text),
    send(E, editable, @off),
    send(E?text_cursor, displayed, @off).

%   sample_style(+Style, -StyleObject)
%
%   As emacs_prolog_mode:style/3, the colours of a syntax style refer
%   to the theme colours, so the sample follows the theme.

sample_style(syntax(Class), Style) :-
    !,
    (   current_predicate(prolog_colour:syntax_colour/2),
        prolog_colour:syntax_colour(Class, Attributes)
    ->  maplist(style_attribute(Class), Attributes, PceArgs)
    ;   PceArgs = []
    ),
    Style =.. [style|PceArgs].
sample_style(Style, Style).

style_attribute(Class, Attr, Name := Value) :-
    Attr =.. [Name, Value0],
    (   memberchk(Name, [colour, background])
    ->  syntax_colour_name(Class, Name, Value)
    ;   Value = Value0
    ).

%   syntax_sample(?Name, ?Before, ?Fragment, ?After, ?Caption)
%
%   A piece of code where Fragment is coloured using the syntax colour
%   Name.

syntax_sample(syntax_goal_built_in, 'p(X) :- ', atom_length, '(X, L).',
              'Call to a built-in predicate').
syntax_sample(syntax_goal_imported, 'p(X) :- ', append, '(X, Y, Z).',
              'Call to an imported predicate').
syntax_sample(syntax_goal_autoload, 'p(X) :- ', last, '(X, Y).',
              'Call to a predicate that is autoloaded').
syntax_sample(syntax_goal_global, 'p :- ', my_user_pred, '.',
              'Call to a predicate in module user').
syntax_sample(syntax_goal_global_dynamic, 'p :- ', my_user_fact, '(X).',
              'Call to a dynamic predicate in module user').
syntax_sample(syntax_goal_undefined, 'p :- ', not_defined, '(X).',
              'Call to an undefined predicate').
syntax_sample(syntax_goal_thread_local, 'p :- ', tl_fact, '(X).',
              'Call to a thread-local predicate').
syntax_sample(syntax_goal_dynamic, 'p :- ', counter, '(N).',
              'Call to a dynamic predicate').
syntax_sample(syntax_goal_multifile, 'p :- ', my_hook, '(X).',
              'Call to a multifile predicate').
syntax_sample(syntax_goal_expanded, 'p :- ', expanded_goal, '(X).',
              'Call that is rewritten by goal expansion').
syntax_sample(syntax_goal_extern, 'p :- m:', helper, '(X).',
              'Call to a predicate of another module').
syntax_sample(syntax_goal_extern_private, 'p :- m:', helper, '(X).',
              'Call to a private predicate of another module').
syntax_sample(syntax_goal_extern_public, 'p :- lists:', append, '(X, Y, Z).',
              'Call to an exported predicate of another module').
syntax_sample(syntax_goal_meta, 'p(G) :- ', 'G', '.',
              'Call to a goal passed as argument').
syntax_sample(syntax_goal_foreign, 'p(X) :- ', c_pred, '(X).',
              'Call to a predicate defined in C').
syntax_sample(syntax_goal_constraint, 'p(X) :- ', leq, '(X, 3).',
              'Call to a CHR constraint').
syntax_sample(syntax_goal_not_callable_bg, 'p :- ', '42', '.',
              'A goal that is not callable').
syntax_sample(syntax_function, 'X is ', max, '(A, B).',
              'Arithmetic function').
syntax_sample(syntax_no_function, 'X is ', foo, '(A).',
              'Not an arithmetic function').
syntax_sample(syntax_option_name, 'open(F, read, S, [', encoding, '(utf8)]).',
              'Name of a known option').
syntax_sample(syntax_no_option_name, 'open(F, read, S, [', encodng,
              '(utf8)]).', 'Name of an unknown option').
syntax_sample(syntax_head_exported, '', api, '(X) :- helper(X).',
              'Head of an exported predicate').
syntax_sample(syntax_head_public, '', callback, '(X) :- helper(X).',
              'Head of a public predicate').
syntax_sample(syntax_head_extern, 'm:', helper, '(X) :- true.',
              'Head of a clause for another module').
syntax_sample(syntax_head_dynamic, '', counter, '(0).',
              'Head of a dynamic predicate').
syntax_sample(syntax_head_multifile, 'user:', portray, '(X) :- fail.',
              'Head of a multifile predicate').
syntax_sample(syntax_head_unreferenced, '', unused, '(X) :- helper(X).',
              'Head of a predicate that is not called').
syntax_sample(syntax_head_hook, 'user:', message_hook, '(_,_,_) :- fail.',
              'Head of a hook').
syntax_sample(syntax_head_constraint, '', leq, '(X, Y) <=> X == Y | true.',
              'Head of a CHR rule').
syntax_sample(syntax_head_imported, '', append, '(X, Y, Z) :- true.',
              'Head that redefines an imported predicate').
syntax_sample(syntax_head_built_in_bg, '', atom_length, '(A, L) :- true.',
              'Head that redefines a built-in predicate').
syntax_sample(syntax_head_iso_bg, '', atom_length, '(A, L) :- true.',
              'Head that redefines an ISO built-in predicate').
syntax_sample(syntax_head_def_iso, '', atom_length, '(A, L) :- true.',
              'Definition of an ISO built-in predicate').
syntax_sample(syntax_head_def_swi, '', succ_or_fail, '(A) :- true.',
              'Definition of a built-in predicate').
syntax_sample(syntax_head_test, 'test(', my_test, ') :- true.',
              'Head of a unit test').
syntax_sample(syntax_rule_condition_bg, 'max(X, Y, Z), ', 'X >= Y',
              ' => Z = X.', 'Condition of a single sided unification rule').
syntax_sample(syntax_module, 'p :- ', lists, ':append(X, Y, Z).',
              'Module qualification').
syntax_sample(syntax_comment, 'p. ', '% A comment', '',
              'Comment').
syntax_sample(syntax_directive_bg, '', ':- use_module(library(lists)).', '',
              'Directive').
syntax_sample(syntax_var, 'p(', 'X', ') :- q(X).',
              'Variable').
syntax_sample(syntax_singleton, 'p(', 'X', ') :- q.',
              'Variable that appears only once').
syntax_sample(syntax_unbound, 'p :- format("~w", [', 'X', ']).',
              'Variable that is not bound').
syntax_sample(syntax_quoted_atom, 'X = ', '\'Hello world\'', '.',
              'Quoted atom').
syntax_sample(syntax_string, 'X = ', '"Hello world"', '.',
              'String').
syntax_sample(syntax_rational, 'X = ', '1r3', '.',
              'Rational number').
syntax_sample(syntax_codes, 'X = ', '`abc`', '.',
              'List of character codes').
syntax_sample(syntax_chars, 'X = ', '"abc"', '.',
              'String read as a list of characters').
syntax_sample(syntax_nofile, ':- use_module(', 'no_such_file', ').',
              'File that does not exist').
syntax_sample(syntax_file, ':- use_module(', 'library(lists)', ').',
              'File').
syntax_sample(syntax_file_no_depend, ':- use_module(', 'library(debug)', ').',
              'File that is loaded but not used').
syntax_sample(syntax_file_no_depend_bg, ':- use_module(', 'library(debug)',
              ').', 'File that is loaded but not used').
syntax_sample(syntax_directory, 'X = ', 'library(http)', '.',
              'Directory').
syntax_sample(syntax_class_built_in, 'new(X, ', point, '(1, 2)).',
              'Built-in xpce class').
syntax_sample(syntax_class_library, 'new(X, ', toc_window, ').',
              'xpce class from a library').
syntax_sample(syntax_class_undefined, 'new(X, ', no_such_class, ').',
              'Undefined xpce class').
syntax_sample(syntax_prolog_data, 'send(X, append, ', 'prolog(f(Y))', ').',
              'Prolog data passed to xpce').
syntax_sample(syntax_flag_name, 'set_prolog_flag(', my_flag, ', true).',
              'Name of a Prolog flag').
syntax_sample(syntax_known_flag_name, 'set_prolog_flag(', double_quotes,
              ', codes).', 'Name of a known Prolog flag').
syntax_sample(syntax_known_flag_name_bg, 'set_prolog_flag(', double_quotes,
              ', codes).', 'Name of a known Prolog flag').
syntax_sample(syntax_no_flag_name, 'current_prolog_flag(', no_such_flag,
              ', X).', 'Name of an unknown Prolog flag').
syntax_sample(syntax_unused_import, ':- use_module(library(lists), [',
              'last/2', ']).', 'Import that is not used').
syntax_sample(syntax_unused_import_bg, ':- use_module(library(lists), [',
              'last/2', ']).', 'Import that is not used').
syntax_sample(syntax_undefined_import, ':- use_module(library(lists), [',
              'no_pred/1', ']).', 'Import of a predicate that does not exist').
syntax_sample(syntax_constraint, ':- chr_constraint ', 'leq/2', '.',
              'Declaration of a CHR constraint').
syntax_sample(syntax_keyword, 'a(X) ', '==>', ' b(X).',
              'Keyword').
syntax_sample(syntax_expanded, 'X = ', expanded, '.',
              'Term that is rewritten by term expansion').
syntax_sample(syntax_hook, 'prolog:', message, '(my_msg) --> [].',
              'Hook').
syntax_sample(syntax_op_type, ':- op(700, ', xfx, ', ===>).',
              'Type of an operator').
syntax_sample(syntax_qq, 'X = {|', html, '||<p>Hello</p>|}.',
              'Quasi quotation').
syntax_sample(syntax_qq_content, 'X = {|html||', '<p>Hello</p>', '|}.',
              'Content of a quasi quotation').
syntax_sample(syntax_dict_function, 'X = Dict.', get, '(name).',
              'Function on a dict').
syntax_sample(syntax_dict_return_op, 'M.name() ', ':=', ' M.first.',
              'Definition of a function on dicts').
syntax_sample(syntax_dcg_right_hand_ctx_bg, 'a, ', '[x]', ' --> b.',
              'Pushback of a grammar rule').
syntax_sample(syntax_error_bg, 'p :- ', 'q(', '.',
              'Error').
syntax_sample(syntax_type_error_bg, 'X is ', 'foo', ' + 1.',
              'Argument of the wrong type').
syntax_sample(syntax_domain_error_bg, 'open(F, ', reed, ', S).',
              'Argument that is not in its domain').
syntax_sample(syntax_syntax_error_bg, 'p :- q(X', ',', ').',
              'Syntax error').
syntax_sample(syntax_instantiation_error_bg, 'atom_length(', 'A', ', L).',
              'Argument that must be bound').
syntax_sample(syntax_macro, 'X = ', '#max', '.',
              'Macro').


                 /*******************************
                 *        CLASS VARIABLES       *
                 *******************************/

%!  colour_class_variable(+Name, -Class, -Variable) is nondet.
%
%   The default value of the class variable Variable of Class refers
%   to the theme colour Name.  Only classes that are loaded are
%   considered.  Class `pce` is skipped: its variable `initialise`
%   holds code that runs when xpce starts.

colour_class_variable(Name, Class, Variable) :-
    new(Classes, chain),
    send(@classes, for_all, message(Classes, append, @arg2)),
    chain_list(Classes, List),
    send(Classes, done),
    member(C, List),
    get(C, realised, @on),
    \+ get(C, name, pce),
    get(C, class_variables, Vars),
    member_chain(V, Vars),
    get(V, context, C),
    get(V, default, Default),
    atomic_default(Default, Text),
    mentions(Text, Name),
    get(C, name, Class),
    get(V, name, Variable).

atomic_default(Default, Text) :-
    (   object(Default)
    ->  send(Default, instance_of, char_array),
        get(Default, value, Text)
    ;   atom(Default)
    ->  Text = Default
    ).

mentions(Text, Name) :-
    sub_atom(Text, B, L, A, Name),
    \+ ( B > 0, B1 is B-1, sub_atom(Text, B1, 1, _, C1), csym_char(C1) ),
    \+ ( A > 0, A1 is B+L, sub_atom(Text, A1, 1, _, C2), csym_char(C2) ),
    !.

csym_char(C) :-
    char_type(C, csym).


                 /*******************************
                 *            CONSOLE           *
                 *******************************/

%!  console_sample(+Class, -Segments) is det.
%
%   Segments is a line of console output that shows the console colour
%   Class.  It is a list of s(Text) for text in the colour of Class and
%   Text for other text.

console_sample(Class, Segments) :-
    console_sample_(Class, Segments0),
    !,
    Segments = Segments0.
console_sample(_, [s('?- member(X, [a,b]).')]).

console_sample_(var,             ['foo(', s('X'), ', ', s('Y'), ')']).
console_sample_(code,            ['Use ', s('listing(foo/1)'),
                                  ' to list the clauses']).
console_sample_(comment,         [s('% Loaded library(lists)')]).
console_sample_(warning,         [s('Warning: foo.pl:12: Singleton variables: [X]')]).
console_sample_(error,           [s('ERROR: Unknown procedure: foo/0')]).
console_sample_(truth(false),    [s('false.')]).
console_sample_(truth(true),     [s('true.')]).
console_sample_(truth(undefined), [s('undefined.')]).
console_sample_(wfs(residual_program),
                                 [s('% WFS residual program'), '  p :- tnot(p).']).
console_sample_(frame(level),    ['   Call: ', s('(12)'), ' member(X, [a,b]) ? ']).
console_sample_(port(Port),      ['   ', s(Label), ': (12) member(X, [a,b]) ? ']) :-
    port_label(Port, Label).
console_sample_(goal(_, _),      ['   Call: (12) ', s('member(X, [a,b])'), ' ? ']).
console_sample_(prompt,          [s('?- ')]).
console_sample_(input,           ['?- ', s('member(X, [a,b]).')]).
console_sample_(answer(_),       [s('X = a ;')]).
console_sample_(binding(name),   [s('X'), ' = a ;']).
console_sample_(predicate(_),    ['Predicate ', s('append/3')]).
console_sample_(message(informational),
                                 [s('% Loaded library(lists) in 0.01 sec')]).
console_sample_(message(information),
                                 [s('% Started server at http://localhost:8080/')]).
console_sample_(message(silent), [s('% A silent message')]).
console_sample_(message(debug(_)), [s('% [Thread 1] Debug message')]).
console_sample_(message(Level),  [s(Text)]) :-
    message_level_text(Level, Text).

port_label(call,      'Call').
port_label(exit,      'Exit').
port_label(fail,      'Fail').
port_label(redo,      'Redo').
port_label(unify,     'Unify').
port_label(exception, 'Exception').

message_level_text(error,   'ERROR: Unknown procedure: foo/0').
message_level_text(warning, 'Warning: foo.pl:12: Singleton variables: [X]').
message_level_text(help,    'Use ?- help(Topic). for help').
