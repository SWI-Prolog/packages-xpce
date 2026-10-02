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

:- module(pce_theme,
          [ apply_theme/1,              % +Theme
            select_theme/1,             % +Theme
            current_theme_selection/1,  % -Theme
            available_theme/1,          % ?Theme
            ensure_theme_colours/0,
            theme_colours/1,            % +List
            adaptive_colour/3,          % +Name, +Spec, +Reference
            current_theme/1,            % -Theme
            syntax_colour_name/3,       % +Class, +Attribute, -Name
            check_theme/1,              % +Theme
            load_theme_libraries/0,
            theme_issues/2              % +Theme, -Issues
          ]).
:- use_module(library(pce)).
:- autoload(library(apply), [maplist/3, exclude/3]).
:- autoload(library(error), [must_be/2, existence_error/2]).
:- autoload(library(lists), [member/2, append/2, append/3]).
:- autoload(library(ordsets), [ord_subtract/3]).
:- autoload(library(pairs), [pairs_keys/2, group_pairs_by_key/2]).

/** <module> Semantic colours and themes

A _semantic colour_ is a colour whose name describes its role, such as
`syntax_comment`, rather than its value.  Its value depends on the
_theme_.  The colour is an xpce `theme_colour` object of that name, so
everything that refers to the colour, directly or by name, follows a
change of the theme after the windows are redrawn.  The value of a
theme colour is the name of another colour, which may be a system
colour such as `sys_window_background` or another theme colour.  The
RGB value is computed when it is needed, so theme colours may refer to
each other in any order.

Semantic colours are declared with their value for the default `light`
theme.  There are three sources:

  - The theme colours of xpce itself, used by the class variable
    defaults of the xpce classes.  These are the `ui_*` colours for
    the basic user interface elements and the `ansi_*` colours of the
    terminal.  See builtin_colour/2.
  - theme_colours/1 directives and semantic_colour/3 clauses, used by
    the library that uses the colours.
  - The PceEmacs syntax highlighting styles of syntax_colour/2 in
    library(prolog_colour).  The names are derived from the style
    class by syntax_colour_name/3.

A theme is a set of colour/3 facts that map a semantic colour name to a
value.  Names a theme does not map keep their `light` value.  Theme
files therefore do not change anything when they are loaded and
apply_theme/1 can switch between themes at any time.
*/

:- multifile
    semantic_colour/3,                  % ?Name, ?Default, ?Comment
    colour/3.                           % ?Theme, ?Name, ?Value

:- dynamic
    current_theme_/1,
    adaptive_colour_/3,                 % Name, Spec, Reference
    theme_selection_/1,
    builtin_colours_/1,
    declared_colour/3.                  % Name, Default, Module

:- meta_predicate
    theme_colours(:).

%!  semantic_colour(?Name, ?Default, ?Comment) is nondet.
%
%   Multifile hook that declares the semantic colour Name.  Default is
%   its value in the `light` theme.  Comment describes its role and is
%   used for documentation and the theme checker.  Default is a colour
%   name, a `#rrggbb` string or another semantic colour.

%!  colour(?Theme, ?Name, ?Value) is nondet.
%
%   Multifile hook that defines the value of the semantic colour Name
%   in Theme.  Value is the same as for the default of
%   semantic_colour/3.  This is normally defined in the theme file,
%   e.g., library(theme/dark).

%!  theme_colours(+List) is det.
%
%   Declare the semantic colours used by a library.  List is a list of
%   `Name = Default`, where Default is the value in the `light` theme.
%   The colours are created immediately, so the library can refer to
%   them by name, for example in a class variable default.  Use as a
%   directive:
%
%   ```
%   :- theme_colours([ prof_header_background = khaki1 ]).
%
%   class_variable(header_background, colour, prof_header_background).
%   ```

theme_colours(M:List) :-
    must_be(list, List),
    forall(member(Name = Default, List),
           ( must_be(atom, Name),
             retractall(declared_colour(Name, _, _)),
             assertz(declared_colour(Name, Default, M))
           )),
    ensure_theme_colours.

%!  adaptive_colour(+Name, +Spec, +Reference) is det.
%
%   Define or redefine the semantic colour Name from a colour chosen by
%   the user, e.g., the background of an Epilog profile.  Spec is a
%   colour or a list `Theme = Colour`.  A theme that appears in the
%   list uses its colour.  Otherwise the colour for the `light` theme
%   (or the first colour or Spec itself) is _adapted_: if it is not as
%   dark or as light as the semantic colour Reference in the theme, its
%   lightness is mirrored, keeping its hue.  For example, a light yellow
%   background with Reference `ui_window_background` becomes a dark
%   olive in a dark theme.  The colour is created immediately.

adaptive_colour(Name, Spec, Reference) :-
    must_be(atom, Name),
    must_be(atom, Reference),
    with_mutex(pce_theme,
               ( retractall(adaptive_colour_(Name, _, _)),
                 assertz(adaptive_colour_(Name, Spec, Reference)),
                 current_theme(Theme),
                 update_colours(Theme)
               )).

adaptive_value(Theme, Match, Name, Value) :-
    adaptive_colour_(Name, Spec, Reference),
    (   is_list(Spec),
        memberchk(Theme=Value0, Spec)
    ->  Value = Value0
    ;   spec_base(Spec, Base),
        resolve_rgb(Theme, Match, Reference, RefRGB),
        resolve_rgb(Theme, Match, Base, BaseRGB),
        (   is_dark(RefRGB, Dark),
            is_dark(BaseRGB, Dark)
        ->  Value = Base
        ;   mirror_rgb(BaseRGB, Mirrored),
            rgb_name(Mirrored, Value)
        )
    ).

spec_base(Spec, Base) :-
    is_list(Spec),
    !,
    (   memberchk(light=Base, Spec)
    ->  true
    ;   Spec = [_=Base|_]
    ).
spec_base(Spec, Spec).

%!  resolve_rgb(+Theme, +Match, +Value, -RGB) is det.
%
%   RGB is rgb(R,G,B) for Value in Theme.  If Value is a semantic colour
%   we use its value in Theme rather than the colour object, which may
%   still have the value of the previous theme.

resolve_rgb(Theme, Match, Value, RGB) :-
    resolve_rgb(Theme, Match, Value, 10, RGB).

resolve_rgb(Theme, Match, Value, Depth, RGB) :-
    Depth > 0,
    atom(Value),
    semantic_colour_name(Value, _),
    !,
    theme_value(Theme, Match, Value, Value1),
    Depth1 is Depth - 1,
    resolve_rgb(Theme, Match, Value1, Depth1, RGB).
resolve_rgb(_, _, Value, _, rgb(R,G,B)) :-
    get(@pce, convert, Value, colour, Colour),
    get(Colour, red, R),
    get(Colour, green, G),
    get(Colour, blue, B).

is_dark(rgb(R,G,B), Dark) :-
    (   0.299*R + 0.587*G + 0.114*B < 128
    ->  Dark = true
    ;   Dark = false
    ).

%!  mirror_rgb(+RGB0, -RGB) is det.
%
%   Mirror the lightness of a colour in the HSL model, keeping the hue.
%   The saturation is reduced, such that a pastel becomes a muted dark
%   colour rather than a deep one.  The lightness is kept between 0.1
%   and 0.9, such that we do not produce black or white.

mirror_rgb(rgb(R0,G0,B0), rgb(R,G,B)) :-
    R1 is R0/255, G1 is G0/255, B1 is B0/255,
    Max is max(R1, max(G1, B1)),
    Min is min(R1, min(G1, B1)),
    L0 is (Max+Min)/2,
    Chroma0 is Max-Min,
    L is max(0.1, min(0.9, 1-L0)),
    Chroma is min(Chroma0/2, 1-abs(2*L-1)),
    (   Chroma0 =:= 0
    ->  R2 = L, G2 = L, B2 = L
    ;   Scale is Chroma/Chroma0,
        Mid0 is (Max+Min)/2,
        R2 is L + (R1-Mid0)*Scale,
        G2 is L + (G1-Mid0)*Scale,
        B2 is L + (B1-Mid0)*Scale
    ),
    R is round(255*max(0, min(1, R2))),
    G is round(255*max(0, min(1, G2))),
    B is round(255*max(0, min(1, B2))).

rgb_name(rgb(R,G,B), Name) :-
    format(atom(Name), '#~|~`0t~16r~2+~`0t~16r~2+~`0t~16r~2+',
           [R,G,B]).

%!  current_theme(-Theme) is det.
%
%   Theme is the active theme.  This is `light` if no theme has been
%   applied.

current_theme(Theme) :-
    current_theme_(Theme0),
    !,
    Theme = Theme0.
current_theme(light).

%!  apply_theme(+Theme) is det.
%
%   Make Theme the active theme.  This loads library(theme/Theme) if
%   it exists, sets the value of all semantic colours and redraws all
%   windows.  The `light` theme uses the default values of
%   the semantic colours and does not load library(theme/light), which
%   only defines colours for the Prolog console.

apply_theme(Theme) :-
    update_theme(Theme),
    send(@display_manager, colours_changed).

update_theme(Theme0) :-
    must_be(atom, Theme0),
    canonical_theme(Theme0, Theme),
    load_theme(Theme),
    with_mutex(pce_theme, update_colours(Theme)).

update_colours(Theme) :-
    retractall(current_theme_(_)),
    assertz(current_theme_(Theme)),
    (   matches_system(Theme)
    ->  Match = true
    ;   Match = false
    ),
    forall(semantic_colour_name(Name, _),
           ( theme_value(Theme, Match, Name, Value),
             new(_, theme_colour(Name, Value))
           )).

%!  theme_value(+Theme, +MatchesSystem, +Name, -Value) is det.
%
%   Value is the value of the semantic colour Name in Theme.  The
%   _roles_ `ui_<role>` are the system colours `sys_<role>` if the
%   brightness of the theme matches the system colours.  Otherwise the
%   theme replaces them, for example to use a dark theme on a light
%   desktop.

theme_value(Theme, Match, Name, Value) :-
    adaptive_colour_(Name, _, _),
    !,
    adaptive_value(Theme, Match, Name, Value).
theme_value(Theme, Match, Name, Value) :-
    role(Name, System),
    !,
    (   Match == true
    ->  Value = System
    ;   colour(Theme, Name, Value0)
    ->  Value = Value0
    ;   Value = System
    ).
theme_value(Theme, _, Name, Value) :-
    colour_value(Theme, Name, Value).

%!  role(?Name, ?System) is nondet.
%
%   Name is the theme colour `ui_<role>` that is derived from the
%   system colour System, `sys_<role>`.

role(Name, System) :-
    builtin_colour(Name, System),
    atom_concat(sys_, Role, System),
    atom_concat(ui_, Role, Name).

%!  matches_system(+Theme) is semidet.
%
%   True if Theme and the system colours are both dark or both light.
%   A theme is dark if its `ui_window_background` is dark.  A theme that
%   does not define `ui_window_background` matches any system.

matches_system(Theme) :-
    (   colour(Theme, ui_window_background, Value)
    ->  dark_colour(Value, ThemeDark),
        dark_colour(sys_window_background, SystemDark),
        ThemeDark == SystemDark
    ;   true
    ).

dark_colour(Spec, Dark) :-
    get(@pce, convert, Spec, colour, Colour),
    get(Colour, intensity, I),
    (   I < 128
    ->  Dark = true
    ;   Dark = false
    ).

%   The light theme uses the system colours, unless these are dark.
%   In that case it uses these.

colour(light, ui_window_background,     white).
colour(light, ui_window_foreground,     black).
colour(light, ui_dialog_background,     '#f0f0f0').
colour(light, ui_dialog_foreground,     black).
colour(light, ui_button_background,     '#e1e1e1').
colour(light, ui_button_foreground,     black).
colour(light, ui_button_pressed,        '#cccccc').
colour(light, ui_selection_background,  '#0078d7').
colour(light, ui_selection_foreground,  white).
colour(light, ui_tooltip_background,    '#ffffe1').
colour(light, ui_tooltip_foreground,    black).
colour(light, ui_inactive,              grey50).
colour(light, ui_link,                  '#0066cc').
colour(light, ui_accent,                '#0078d7').
colour(light, ui_separator,             '#c0c0c0').
colour(light, ui_shadow,                grey50).

%!  ensure_theme_colours is det.
%
%   Make sure all known semantic colours exist as xpce `theme_colour`
%   objects.  apply_theme/1 creates the semantic colours that are known
%   at that moment.  Libraries that declare semantic colours or load
%   library(prolog_colour) later call this before using their colours.
%   Creating an existing theme colour with the same value does nothing.

ensure_theme_colours :-
    current_theme(Theme),
    with_mutex(pce_theme, update_colours(Theme)).

%!  select_theme(+Theme) is det.
%
%   Select the theme from the user interface.  Theme is the name of a
%   theme or `system` to follow the light or dark setting of the
%   desktop.  This sets the class variable `display.theme`, so the
%   selection holds for this session.  To make it permanent, set
%   `display.theme` in the xpce Defaults file.

select_theme(Selection) :-
    must_be(atom, Selection),
    get(@pce, convert, display, class, Class),
    (   Selection == system
    ->  send(Class, class_variable_value, theme, @default),
        display_theme(Theme)
    ;   send(Class, class_variable_value, theme, Selection),
        Theme = Selection
    ),
    retractall(theme_selection_(_)),
    assertz(theme_selection_(Selection)),
    apply_theme(Theme).

%!  current_theme_selection(-Selection) is det.
%
%   Selection is `system` if the theme follows the desktop or the name
%   of the selected theme.

current_theme_selection(Selection) :-
    (   fixed_theme
    ->  current_theme(Selection)
    ;   Selection = system
    ).

%!  available_theme(?Theme) is nondet.
%
%   True when Theme can be selected.  These are `light` and the themes
%   in library(theme) that define colours for xpce.

available_theme(light).
available_theme(Theme) :-
    absolute_file_name(library(theme), Dir,
                       [ file_type(directory),
                         solutions(all),
                         file_errors(fail)
                       ]),
    directory_files(Dir, Files),
    member(File, Files),
    file_name_extension(Theme, pl, File),
    Theme \== light,
    directory_file_path(Dir, File, Path),
    xpce_theme_file(Path).

xpce_theme_file(Path) :-
    setup_call_cleanup(
        open(Path, read, In),
        read_string(In, _, String),
        close(In)),
    sub_string(String, _, _, _, "pce_theme:colour"),
    !.

canonical_theme(default, light) :- !.
canonical_theme(Theme, Theme).

load_theme(light) :-
    !.
load_theme(Theme) :-
    exists_source(library(theme/Theme)),
    !,
    use_module(library(theme/Theme)).
load_theme(Theme) :-
    colour(Theme, _, _),
    !.
load_theme(Theme) :-
    existence_error(theme, Theme).

colour_value(Theme, Name, Value) :-
    colour(Theme, Name, Value),
    !.
colour_value(_, Name, Value) :-
    default_colour(Name, Value).

default_colour(Name, Value) :-
    builtin_colour(Name, Value),
    !.
default_colour(Name, Value) :-
    declared_colour(Name, Value, _),
    !.
default_colour(Name, Value) :-
    semantic_colour(Name, Value, _),
    !.
default_colour(Name, Value) :-
    syntax_colour(Name, _Class, Value),
    !.

%!  syntax_colour_name(+Class, +Attribute, -Name) is det.
%
%   Name is the semantic colour for Attribute of the PceEmacs syntax
%   highlighting style Class.  Attribute is one of `colour` or
%   `background`.  Name is `syntax_` followed by the name and the
%   atomic arguments of Class, separated by `_`.  Variables are
%   skipped.  A background colour gets the suffix `_bg`.  For example,
%   goal(built_in,_) becomes `syntax_goal_built_in`.

syntax_colour_name(Class, Attribute, Name) :-
    phrase(class_parts(Class), Parts),
    attribute_suffix(Attribute, Suffix),
    append([syntax|Parts], Suffix, AllParts),
    atomic_list_concat(AllParts, '_', Name).

class_parts(Var) -->
    { var(Var) },
    !.
class_parts(Atomic) -->
    { atomic(Atomic) },
    !,
    [Atomic].
class_parts(Compound) -->
    { compound_name_arguments(Compound, Name, Args) },
    [Name],
    args_parts(Args).

args_parts([]) --> [].
args_parts([H|T]) --> class_parts(H), args_parts(T).

attribute_suffix(colour,     []).
attribute_suffix(background, [bg]).

%!  syntax_colour(?Name, -Class, -Default) is nondet.
%
%   Name is the semantic colour for a colour of the PceEmacs syntax
%   highlighting style for Class, whose value in the light theme is
%   Default.  This enumerates syntax_colour/2 of library(prolog_colour)
%   if this library is loaded.  This includes def_style/2 and the hook
%   prolog_colour:style/2 that is used by language modes to add
%   classes.  If two classes map to the same name, the first wins, as
%   syntax_colour/2 is used with first-match semantics.

syntax_colour(Name, Class, Default) :-
    findall(N-(C-D), syntax_colour_(N, C, D), Pairs),
    first_per_key(Pairs, Unique),
    member(Name-(Class-Default), Unique).

syntax_colour_(Name, Class, Default) :-
    current_predicate(prolog_colour:syntax_colour/2),
    prolog_colour:syntax_colour(Class, Attributes),
    member(Attr, Attributes),
    Attr =.. [Attribute, Default],
    attribute_suffix(Attribute, _),
    syntax_colour_name(Class, Attribute, Name).

first_per_key(Pairs, Unique) :-
    first_per_key(Pairs, [], Unique).

first_per_key([], _, []).
first_per_key([K-V|T0], Seen, T) :-
    (   memberchk(K, Seen)
    ->  first_per_key(T0, Seen, T)
    ;   T = [K-V|T1],
        first_per_key(T0, [K|Seen], T1)
    ).

%!  semantic_colour_name(?Name, ?Default) is nondet.
%
%   Enumerate all known semantic colours with their default value.

semantic_colour_name(Name, Default) :-
    builtin_colour(Name, Default).
semantic_colour_name(Name, Default) :-
    adaptive_colour_(Name, Spec, _),
    spec_base(Spec, Default).
semantic_colour_name(Name, Default) :-
    declared_colour(Name, Default, _),
    \+ builtin_colour(Name, _).
semantic_colour_name(Name, Default) :-
    semantic_colour(Name, Default, _),
    \+ builtin_colour(Name, _),
    \+ declared_colour(Name, _, _).
semantic_colour_name(Name, Default) :-
    syntax_colour(Name, _, Default),
    \+ builtin_colour(Name, _),
    \+ declared_colour(Name, _, _),
    \+ semantic_colour(Name, _, _).

%!  builtin_colour(?Name, ?Default) is nondet.
%
%   True when Name is a theme colour defined by xpce itself with
%   Default as value in the `light` theme.  These are used by the class
%   variable defaults of the xpce classes and are available in the
%   xpce hash table @theme_colour_defaults.

builtin_colour(Name, Default) :-
    builtin_colours(Pairs),
    member(Name-Default, Pairs).

builtin_colours(Pairs) :-
    builtin_colours_(Pairs0),
    !,
    Pairs = Pairs0.
builtin_colours(Pairs) :-
    get(@pce, convert, white, colour, _), % realise class theme_colour
    new(Chain, chain),
    send(@theme_colour_defaults, for_all,
         message(Chain, append, create(tuple, @arg1, @arg2))),
    chain_list(Chain, Tuples),
    findall(Name-Default,
            ( member(T, Tuples),
              get(T, first, Name),
              get(T, second, Default)
            ), Pairs0),
    free(Chain),
    msort(Pairs0, Pairs),
    asserta(builtin_colours_(Pairs)).


		 /*******************************
		 *        SYSTEM CHANGES	*
		 *******************************/

%!  init_theme
%
%   Select the initial theme and make the theme follow the system if
%   the user did not fix the theme.  This runs when this library is
%   loaded, which library(pce) does after setting up the theme.

:- public init_theme/0.
:- initialization(init_theme).

init_theme :-
    initial_theme(Theme),
    update_theme(Theme),
    send(@display_manager, system_colours_message,
         message(@prolog, system_colours_changed)).

initial_theme(Theme) :-
    current_prolog_flag(theme, Theme),
    !.
initial_theme(Theme) :-
    catch(prolog:theme(Theme), error(_,_), fail),
    !.
initial_theme(Theme) :-
    display_theme(Theme).

display_theme(Theme) :-
    get(@display, theme, Theme0),
    !,
    canonical_theme(Theme0, Theme).
display_theme(light).

%!  system_colours_changed
%
%   Called through `display_manager <-system_colours_message` after the
%   system colours changed.  If the theme follows the system, select
%   the theme for the new system settings.  Otherwise, apply the
%   current theme again, as semantic colours may be defined from the
%   system colours.  The display manager redraws the windows.

:- public system_colours_changed/0.

system_colours_changed :-
    (   fixed_theme
    ->  current_theme(Theme)
    ;   display_theme(Theme)
    ),
    catch(update_theme(Theme), Error,
          print_message(error, Error)).

fixed_theme :-
    theme_selection_(Selection),
    !,
    Selection \== system.
fixed_theme :-
    current_prolog_flag(theme, _),
    !.
fixed_theme :-
    get(@display, class_variable_value, theme, Theme),
    Theme \== @default.


		 /*******************************
		 *            CHECK		*
		 *******************************/

%!  check_theme(+Theme) is semidet.
%
%   Verify the colour/3 facts of Theme against the known semantic
%   colours.  Prints the issues found by theme_issues/2 and fails if
%   there are errors.  Note that only semantic colours of loaded
%   libraries are known.  This predicate loads the libraries of the
%   development tools that declare semantic colours (see
%   theme_library/1).

check_theme(Theme) :-
    load_theme_libraries,
    load_theme(Theme),
    theme_issues(Theme, Issues),
    forall(member(Issue, Issues),
           ( issue_level(Issue, Level),
             print_message(Level, pce_theme(Theme, Issue))
           )),
    \+ ( member(Issue, Issues),
         issue_level(Issue, error)
       ).

%!  theme_library(?Library) is nondet.
%
%   Libraries that declare semantic colours.  These are loaded by
%   check_theme/1 and load_theme_libraries/0.

theme_library(library(prolog_colour)).
theme_library(library(trace/trace)).            % graphical debugger
theme_library(library(swi/pce_profile)).
theme_library(library(swi/pce_debug_monitor)).
theme_library(library(pce_xref)).
theme_library(library(emacs/bookmarks)).
theme_library(library(pce_helper)).

%!  load_theme_libraries is det.
%
%   Load all libraries that declare semantic colours, such that
%   theme_issues/2 knows all of them.

load_theme_libraries :-
    forall(theme_library(Lib),
           use_module(Lib, [])).

issue_level(missing(_),        warning).
issue_level(unknown(_),        error).
issue_level(duplicate(_),      error).
issue_level(invalid(_,_),      error).
issue_level(redundant(_),      informational).
issue_level(collision(_,_),    warning).

%!  theme_issues(+Theme, -Issues) is det.
%
%   Issues is a list of problems with the colour/3 facts for Theme.
%   Each issue is one of:
%
%     - missing(Name)
%       Theme does not define the semantic colour Name.  It uses the
%       `light` value, which is likely wrong for a dark theme.  Not
%       reported for the `light` theme.
%     - unknown(Name)
%       Theme defines Name, which is not a semantic colour.
%     - duplicate(Name)
%       Theme defines Name more than once.
%     - invalid(Name, Value)
%       Value is not a semantic colour nor a known colour.
%     - redundant(Name)
%       Theme defines Name as its `light` value.
%     - collision(Name, Classes)
%       Several syntax highlighting classes map to Name and have
%       different values.

theme_issues(Theme0, Issues) :-
    canonical_theme(Theme0, Theme),
    findall(N-D,
            ( semantic_colour_name(N, D),
              \+ adaptive_colour_(N, _, _)
            ), Known0),
    sort(1, @<, Known0, Known),
    pairs_keys(Known, Names),
    findall(N-V, colour(Theme, N, V), Defined),
    pairs_keys(Defined, DefinedNames0),
    msort(DefinedNames0, DefinedNames),
    sort(DefinedNames, DefinedSet),
    (   Theme == light
    ->  Missing = []
    ;   ord_subtract(Names, DefinedSet, MissingNames),
        maplist(wrap(missing), MissingNames, Missing)
    ),
    ord_subtract(DefinedSet, Names, UnknownNames),
    maplist(wrap(unknown), UnknownNames, Unknown),
    duplicates(DefinedNames, DupNames),
    maplist(wrap(duplicate), DupNames, Duplicate),
    findall(invalid(N,V),
            ( member(N-V, Defined),
              \+ valid_value(Theme, V)
            ), Invalid),
    findall(redundant(N),
            ( member(N-V, Defined),
              memberchk(N-V, Known)
            ), Redundant),
    findall(collision(N, Classes), syntax_collision(N, Classes), Collision),
    append([Unknown, Duplicate, Invalid, Missing, Collision, Redundant],
           Issues).

wrap(Functor, Arg, Term) :-
    Term =.. [Functor, Arg].

duplicates([], []).
duplicates([H,H|T0], [H|T]) :-
    !,
    exclude(==(H), T0, T1),
    duplicates(T1, T).
duplicates([_|T0], T) :-
    duplicates(T0, T).

valid_value(Theme, Value) :-
    atom(Value),
    colour_value(Theme, Value, _),
    !.
valid_value(_, Value) :-
    atom(Value),
    (   get(@colours, member, Value, _)
    ;   get(@colour_names, member, Value, _)
    ;   hex_colour(Value)
    ),
    !.

hex_colour(Value) :-
    atom_codes(Value, [0'#|Hex]),
    length(Hex, Len),
    memberchk(Len, [3,4,6,8]),
    forall(member(C, Hex), code_type(C, xdigit(_))).

syntax_collision(Name, Classes) :-
    findall(N-(C-D), syntax_colour_(N, C, D), Pairs),
    keysort(Pairs, Sorted),
    group_pairs_by_key(Sorted, Groups),
    member(Name-CDs, Groups),
    findall(D, member(_-D, CDs), Ds0),
    sort(Ds0, Ds),
    Ds = [_,_|_],
    findall(C, member(C-_, CDs), Classes).


		 /*******************************
		 *           MESSAGES		*
		 *******************************/

:- multifile prolog:message//1.

prolog:message(pce_theme(Theme, Issue)) -->
    [ 'Theme ~q: '-[Theme] ],
    issue(Issue).

issue(missing(Name)) -->
    [ 'no value for ~q (uses the light value)'-[Name] ].
issue(unknown(Name)) -->
    [ '~q is not a semantic colour'-[Name] ].
issue(duplicate(Name)) -->
    [ '~q is defined more than once'-[Name] ].
issue(invalid(Name, Value)) -->
    [ '~q: ~q is not a colour'-[Name, Value] ].
issue(redundant(Name)) -->
    [ '~q has the same value as in the light theme'-[Name] ].
issue(collision(Name, Classes)) -->
    [ 'syntax classes ~q share ~q but have different values'-
      [Classes, Name] ].
