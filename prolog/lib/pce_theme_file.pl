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

:- module(pce_theme_file,
          [ theme_file/2,               % +Theme, -File
            editable_theme/1,           % +Theme
            edit_theme/1,               % +Theme
            end_theme_edit/1,           % +Theme
            revert_theme/1,             % +Theme
            save_theme/1,               % +Theme
            save_theme_as/2,            % +Theme, +NewTheme
            theme_modified/1,           % +Theme
            theme_source/2,             % ?Theme, ?File
            read_theme_file/3,          % +File, -Base, -Settings
            update_theme_file/3         % +Theme, +File, +Settings
          ]).
:- use_module(library(pce)).
:- use_module(library(pce_theme),
              [ create_theme/3, edited_theme/2, theme_setting/2,
                discard_theme/1, current_theme/1, apply_theme/1,
                reset_theme_setting/2
              ]).
:- autoload(library(apply),
            [foldl/4, include/3, maplist/3, convlist/3, partition/4]).
:- autoload(library(error), [permission_error/3, existence_error/2]).
:- autoload(library(lists),
            [member/2, append/2, append/3, last/2, reverse/2, list_to_set/2]).
:- autoload(library(readutil), [read_file_to_string/3]).
:- autoload(library(pairs), [pairs_values/2, map_list_to_pairs/3]).

/** <module> Load and save theme files

This library connects the themes that are edited in memory (see
create_theme/2) to their theme file.  edit_theme/1 replaces a loaded
theme file by an edited theme with the same settings.  save_theme/1
writes the settings back to the file and end_theme_edit/1 loads the
file again.

A theme file defines its settings as facts.  These are either facts of
the hooks, e.g., pce_theme:colour(Theme, Name, Value), or facts of a
local predicate that is called by the hook, as library(theme/dark)
does:

```
pce_theme:colour(dark, Name, Value) :-
    colour(Name, Value).

colour(ui_window_background, '#1e1e1e').
```

Saving changes the file in place: only the values that changed are
replaced, settings that were removed are deleted and new settings are
added after the setting whose name is the most similar.  Comments and
other clauses are preserved.  A new theme gets a file in the user's
library directory, ``lib/theme`` in the user configuration directory
(see file_search_path/2, `user_app_config`).
*/

:- dynamic
    theme_source_/2,                    % Theme, File
    saved_settings_/2.                  % Theme, Settings

%!  theme_file(+Theme, -File) is semidet.
%
%   File is the theme file library(theme/Theme).

theme_file(Theme, File) :-
    once(absolute_file_name(library(theme/Theme), File,
                            [ file_type(prolog),
                              access(read),
                              file_errors(fail)
                            ])).

%!  editable_theme(+Theme) is semidet.
%
%   True if Theme has a theme file that the user may change.  The files
%   of the system are not editable.

editable_theme(Theme) :-
    theme_file(Theme, File),
    access_file(File, write),
    \+ system_file(File).

system_file(File) :-
    once(absolute_file_name(swi(.), Home, [file_type(directory)])),
    atom_concat(Home, /, Prefix),
    sub_atom(File, 0, _, _, Prefix).

%!  theme_source(?Theme, ?File) is nondet.
%
%   True when the edited theme Theme is saved in File.

theme_source(Theme, File) :-
    theme_source_(Theme, File).

%!  edit_theme(+Theme) is det.
%
%   Make Theme editable in memory.  If Theme is already edited, do
%   nothing.  Otherwise Theme must have a theme file that the user may
%   change (see editable_theme/1).  The file is unloaded and Theme is
%   created using create_theme/3 with the settings of the file.
%
%   @error permission_error(edit, theme, Theme)

edit_theme(Theme) :-
    edited_theme(Theme, _),
    !.
edit_theme(Theme) :-
    editable_theme(Theme),
    !,
    theme_file(Theme, File),
    read_theme_file(File, Base, Settings),
    (   source_file(File)
    ->  unload_file(File)
    ;   true
    ),
    catch(create_theme(Theme, Base, Settings), E,
          ( load_files(File, [if(not_loaded)]),
            throw(E)
          )),
    findall(S, theme_setting(Theme, S), Loaded),
    assertz(theme_source_(Theme, File)),
    assertz(saved_settings_(Theme, Loaded)).
edit_theme(Theme) :-
    permission_error(edit, theme, Theme).

%!  end_theme_edit(+Theme) is det.
%
%   Stop editing Theme without saving.  If Theme has a theme file, it
%   is loaded again.  If Theme is the active theme, it is applied
%   again, such that the windows use the values of the file.  Otherwise
%   Theme no longer exists.

end_theme_edit(Theme) :-
    current_theme(Current),
    discard_theme(Theme),
    (   retract(theme_source_(Theme, File))
    ->  retractall(saved_settings_(Theme, _)),
        load_files(File, [if(not_loaded)]),
        (   Current == Theme
        ->  apply_theme(Theme)
        ;   true
        )
    ;   true
    ).

%!  revert_theme(+Theme) is det.
%
%   Undo the changes to the edited theme Theme since it was loaded or
%   saved.

revert_theme(Theme) :-
    theme_source_(Theme, _),
    !,
    end_theme_edit(Theme),
    edit_theme(Theme).
revert_theme(Theme) :-
    forall(theme_setting(Theme, Setting),
           reset_theme_setting(Theme, Setting)).

%!  theme_modified(+Theme) is semidet.
%
%   True if the edited theme Theme has no file or its settings differ
%   from the file.

theme_modified(Theme) :-
    \+ theme_source_(Theme, _),
    !.
theme_modified(Theme) :-
    saved_settings_(Theme, Saved),
    findall(S, theme_setting(Theme, S), Current),
    \+ same_settings(Saved, Current).

same_settings(S1, S2) :-
    msort_variant(S1, Sorted1),
    msort_variant(S2, Sorted2),
    Sorted1 =@= Sorted2.

msort_variant(List, Sorted) :-
    maplist(variant_key, List, Keyed),
    keysort(Keyed, SortedKeyed),
    pairs_values(SortedKeyed, Sorted).

variant_key(Term, Key-Term) :-
    copy_term(Term, Key),
    numbervars(Key, 0, _).

%!  save_theme(+Theme) is det.
%
%   Save the edited theme Theme.  If it was loaded from a file, the
%   file is updated.  Otherwise it is saved to a new file in the user's
%   library directory.  Theme remains edited in memory.

save_theme(Theme) :-
    edited_theme(Theme, _),
    findall(S, theme_setting(Theme, S), Settings),
    (   theme_source_(Theme, File)
    ->  update_theme_file(Theme, File, Settings)
    ;   new_theme_file(Theme, File),
        assertz(theme_source_(Theme, File))
    ),
    retractall(saved_settings_(Theme, _)),
    assertz(saved_settings_(Theme, Settings)).

%!  save_theme_as(+Theme, +NewTheme) is det.
%
%   Save the settings of the edited theme Theme as a new theme
%   NewTheme with the same base theme and edit NewTheme.  Theme
%   reverts to its file or, if it has no file, is removed.  If Theme
%   was active, NewTheme becomes active.

save_theme_as(Theme, New) :-
    edited_theme(Theme, Base),
    (   ( edited_theme(New, _) ; theme_file(New, _) )
    ->  permission_error(create, theme, New)
    ;   true
    ),
    findall(S, theme_setting(Theme, S), Settings),
    user_theme_file(New, File),
    write_new_theme_file(New, Base, File, Settings),
    current_theme(Current),
    end_theme_edit(Theme),
    edit_theme(New),
    (   Current == Theme
    ->  apply_theme(New)
    ;   true
    ).

new_theme_file(Theme, File) :-
    edited_theme(Theme, Base),
    findall(S, theme_setting(Theme, S), Settings),
    user_theme_file(Theme, File),
    (   exists_file(File)
    ->  permission_error(create, theme_file, File)
    ;   true
    ),
    write_new_theme_file(Theme, Base, File, Settings).

%   user_theme_file(+Theme, -File)
%
%   File is the theme file for Theme in the user's library directory,
%   ``lib/theme`` in the first writable user configuration directory.
%   The directory is created if needed.

user_theme_file(Theme, File) :-
    user_theme_dir(Dir),
    file_name_extension(Theme, pl, Base),
    directory_file_path(Dir, Base, File).

user_theme_dir(Dir) :-
    once(absolute_file_name(user_app_config('.'), Config,
                            [ file_type(directory),
                              access(write),
                              file_errors(fail)
                            ])),
    !,
    directory_file_path(Config, 'lib/theme', Dir),
    make_directory_path(Dir).
user_theme_dir(_) :-
    existence_error(directory, user_app_config('.')).


                 /*******************************
                 *          READ A FILE         *
                 *******************************/

%   hook(?Kind, ?Theme, ?Head, ?Args)
%
%   Head is the hook that defines settings of Kind for Theme.  Args are
%   the arguments of Head after the theme.

hook(colour,  Theme, pce_theme:colour(Theme, N, V),          [N,V]).
hook(console, Theme, prolog:console_color(Theme, C, A),      [C,A]).
hook(pldoc,   Theme, pldoc_style:theme(Theme, E, C, CSS),    [E,C,CSS]).

%   kind(?Kind, ?Local, ?Title)
%
%   A theme file defines the settings of Kind in a section titled Title
%   as facts of the local predicate Local.  The hook calls it.  Kinds
%   are listed in the order of their sections.

kind(console, console_color, 'PROLOG CONSOLE').
kind(pldoc,   pldoc_theme,   'ONLINE HELP').
kind(colour,  colour,        'IDE TOOLS').

setting_args(colour,  colour(N, V),       [N,V]).
setting_args(console, console(C, A),      [C,A]).
setting_args(pldoc,   pldoc(E, C, CSS),   [E,C,CSS]).

%!  read_theme_file(+File, -Base, -Settings) is det.
%
%   Read the settings of a theme file.  Base is the theme it is based
%   on (see prolog:theme_base/2) or `none`.  Settings is a list of
%   settings as accepted by set_theme_setting/2.

read_theme_file(File, Base, Settings) :-
    file_theme(File, Theme),
    file_clauses(File, _Text, Clauses),
    (   member(clause(prolog:theme_base(Theme, Base0), _, _), Clauses)
    ->  Base = Base0
    ;   Base = none
    ),
    file_facts(Theme, Clauses, Facts),
    findall(S, member(fact(_, S, _, _), Facts), Settings).

file_theme(File, Theme) :-
    file_base_name(File, Base),
    file_name_extension(Theme, _, Base).

%   file_clauses(+File, -Text, -Clauses)
%
%   Text is the content of File.  Clauses is a list of
%   clause(Term, Pos, Dot), where Pos are the subterm positions and Dot
%   is the offset of the full stop.

file_clauses(File, Text, Clauses) :-
    read_file_to_string(File, Text, [encoding(utf8)]),
    setup_call_cleanup(
        open_string(Text, In),
        read_clauses(In, Text, Clauses),
        close(In)).

read_clauses(In, Text, Clauses) :-
    read_term(In, Term, [ subterm_positions(Pos),
                          module(pce_theme_file)
                        ]),
    (   Term == end_of_file
    ->  Clauses = []
    ;   arg(2, Pos, End),
        full_stop(Text, End, Dot),
        Clauses = [clause(Term, Pos, Dot)|More],
        read_clauses(In, Text, More)
    ).

full_stop(Text, From, Dot) :-
    sub_string(Text, From, _, 0, Rest),
    sub_string(Rest, B, 1, _, "."),
    !,
    Dot is From+B.

%   file_facts(+Theme, +Clauses, -Facts)
%
%   Facts is a list of fact(Kind, Setting, Style, Clause) for the
%   settings defined by Clauses, in the order of the file.  Style is
%   qualified or local(Name).

file_facts(Theme, Clauses, Facts) :-
    findall(Kind-Name, delegation(Theme, Clauses, Kind, Name, _), Locals),
    convlist(clause_fact(Theme, Locals), Clauses, Facts).

delegation(Theme, Clauses, Kind, Name, Clause) :-
    member(Clause, Clauses),
    Clause = clause((Head :- Body), _, _),
    hook(Kind, Theme, Head, Args),
    nonvar(Theme),
    callable(Body),
    Body =.. [Name|BodyArgs],
    BodyArgs == Args.

clause_fact(Theme, _, Clause, fact(Kind, Setting, qualified, Clause)) :-
    Clause = clause(Head, _, _),
    Head \= (_ :- _),
    hook(Kind, Theme0, Head, Args),
    Theme0 == Theme,
    !,
    setting_args(Kind, Setting, Args).
clause_fact(_, Locals, Clause, fact(Kind, Setting, local(Name), Clause)) :-
    Clause = clause(Head, _, _),
    Head \= (_ :- _),
    Head \= (:- _),
    callable(Head),
    Head =.. [Name|Args],
    memberchk(Kind-Name, Locals),
    setting_args(Kind, Setting, Args).


                 /*******************************
                 *          UPDATE A FILE       *
                 *******************************/

%!  update_theme_file(+Theme, +File, +Settings) is det.
%
%   Update File such that it defines Settings.  The value of a setting
%   that is in the file is replaced, a setting that is new is added
%   after the most similar setting of the same kind and a setting of
%   the file that is not in Settings is deleted if it was loaded when
%   editing started (see edit_theme/1).  Settings in the file that
%   could not be loaded, e.g., because they are not valid, are kept.

update_theme_file(Theme, File, Settings) :-
    file_clauses(File, Text, Clauses),
    file_facts(Theme, Clauses, Facts0),
    first_facts(Facts0, Facts),
    (   saved_settings_(Theme, Loaded)
    ->  true
    ;   Loaded = []
    ),
    partition(has_place(Theme, Clauses, Facts), Settings, Placed, New),
    foldl(setting_edit(Text, Theme, Clauses, Facts), Placed, Edits0, []),
    section_edits(Text, Theme, New, Sections),
    findall(Fact,
            ( member(Fact, Facts),
              Fact = fact(_, FS, _, _),
              \+ ( member(S, Settings), same_key(S, FS) ),
              member(L, Loaded), same_key(L, FS)
            ), Deleted),
    maplist(delete_fact_edit(Text), Deleted, Deletes),
    unused_link_edits(Text, Theme, Clauses, Facts, Deleted, Placed, Unlinks),
    append([Edits0, Sections, Deletes, Unlinks], Edits),
    apply_edits(Edits, Text, NewText),
    write_file(File, NewText).

delete_fact_edit(Text, fact(_, _, _, Clause), Edit) :-
    delete_edit(Text, Clause, Edit).

%   has_place(+Theme, +Clauses, +Facts, +Setting) is semidet.
%
%   True if the file has a place for Setting: a fact for it or for
%   another setting of its kind, or the clause that links the hook to
%   the local predicate of its kind.

has_place(_, _, Facts, Setting) :-
    setting_kind(Setting, Kind),
    memberchk(fact(Kind, _, _, _), Facts),
    !.
has_place(Theme, Clauses, _, Setting) :-
    setting_kind(Setting, Kind),
    delegation(Theme, Clauses, Kind, _, _),
    !.

%   section_edits(+Text, +Theme, +Settings, -Edits)
%
%   Edits add a section to the end of the file for each kind of
%   Settings, which the file has no place for.

section_edits(Text, Theme, Settings, Edits) :-
    findall(Section,
            ( kind(Kind, _, _),
              include(setting_kind_is(Kind), Settings, KindSettings),
              KindSettings \== [],
              section_text(Theme, Kind, KindSettings, Section)
            ), Sections),
    (   Sections == []
    ->  Edits = []
    ;   string_length(Text, End),           % one edit: inserts at the
        atomic_list_concat(Sections, All0), % same offset are sorted
        atom_string(All0, All),
        Edits = [edit(End, 0, All)]
    ).

setting_kind_is(Kind, Setting) :-
    setting_kind(Setting, Kind).

section_text(Theme, Kind, Settings, Text) :-
    kind(Kind, Local, _),
    default_column(Kind, Column),
    maplist(fact_text(Theme, local(Local), Column), Settings, Facts),
    atomic_list_concat(Facts, FactsText),
    with_output_to(string(Header), section(Theme, Kind, true)),
    format(string(Text), '~s~n~s', [Header, FactsText]).

%   unused_link_edits(+Text, +Theme, +Clauses, +Facts, +Deleted,
%                     +Placed, -Edits)
%
%   Edits delete the clause that links the hook to the local predicate
%   of a kind if all its facts are deleted and no fact is added.  The
%   local predicate then no longer exists, so it may not be called.

unused_link_edits(Text, Theme, Clauses, Facts, Deleted, Placed, Edits) :-
    findall(Edit,
            ( delegation(Theme, Clauses, Kind, Local, Clause),
              \+ ( member(F, Facts),
                    F = fact(Kind, _, local(Local), _),
                    \+ memberchk(F, Deleted)
                  ),
              \+ ( member(S, Placed),
                    setting_kind(S, Kind),
                    \+ ( member(fact(Kind, FS, _, _), Facts),
                          same_key(FS, S) )
                  ),
              delete_edit(Text, Clause, Edit)
            ), Edits).

first_facts(Facts0, Facts) :-
    foldl(first_fact, Facts0, [], RevFacts),
    reverse(RevFacts, Facts).

first_fact(Fact, Seen, [Fact|Seen]) :-
    Fact = fact(_, S, _, _),
    \+ ( member(fact(_, S0, _, _), Seen), same_key(S0, S) ),
    !.
first_fact(_, Seen, Seen).

same_key(S1, S2) :-
    setting_key(S1, K1),
    setting_key(S2, K2),
    K1 =@= K2.

setting_key(colour(Name, _), colour(Name)).
setting_key(console(Class, _), console(Class)).
setting_key(pldoc(Element, Condition, _), pldoc(Element, Condition)).

setting_kind(colour(_,_),  colour).
setting_kind(console(_,_), console).
setting_kind(pldoc(_,_,_), pldoc).

setting_value(colour(_, V), V).
setting_value(console(_, V), V).
setting_value(pldoc(_, _, V), V).

%   setting_edit(+Text, +Theme, +Clauses, +Facts, +Setting)//
%
%   Edits to the file for Setting.  An edit is edit(Offset, Delete,
%   Insert): delete Delete characters at Offset and insert Insert.

setting_edit(_, _, _, Facts, Setting) -->
    { member(Fact, Facts),
      Fact = fact(_, FS, _, _),
      same_key(FS, Setting),
      !
    },
    (   { setting_value(FS, V0),
          setting_value(Setting, V),
          V0 =@= V
        }
    ->  []
    ;   { value_range(Fact, From, To),
          setting_value(Setting, V),
          value_text(V, VText),
          Len is To-From
        },
        [ edit(From, Len, VText) ]
    ).
setting_edit(Text, Theme, Clauses, Facts, Setting) -->
    { setting_kind(Setting, Kind),
      include(fact_kind(Kind), Facts, KindFacts),
      insert_edit(Text, Theme, Clauses, KindFacts, Setting, Edit)
    },
    [ Edit ].

fact_kind(Kind, fact(Kind, _, _, _)).

%   value_range(+Fact, -From, -To)
%
%   From-To is the character range of the value of the setting, the
%   last argument of the fact.

value_range(fact(_, _, qualified, clause(_, Pos, _)), From, To) :-
    Pos = term_position(_, _, _, _, [_, HeadPos]),
    head_value_range(HeadPos, From, To).
value_range(fact(_, _, local(_), clause(_, Pos, _)), From, To) :-
    head_value_range(Pos, From, To).

head_value_range(term_position(_, _, _, _, ArgsPos), From, To) :-
    last(ArgsPos, ValuePos),
    arg(1, ValuePos, From),
    arg(2, ValuePos, To).

%   insert_edit(+Text, +Theme, +Clauses, +KindFacts, +Setting, -Edit)
%
%   Edit inserts a fact for Setting.  It is inserted after the fact of
%   the same kind whose key is most similar, after the clause that
%   calls the local predicate or at the end of the file.

insert_edit(Text, Theme, _, KindFacts, Setting, edit(Offset, 0, Line)) :-
    KindFacts \== [],
    !,
    key_text(Setting, KeyText),
    foldl(best_anchor(KeyText), KindFacts, none-(-1), Anchor-_),
    Anchor = fact(_, _, Style, clause(_, _, Dot)),
    line_end(Text, Dot, Offset),
    value_column(Text, Anchor, Column),
    fact_text(Theme, Style, Column, Setting, Line).
insert_edit(Text, Theme, Clauses, _, Setting, edit(Offset, 0, Line)) :-
    setting_kind(Setting, Kind),
    delegation(Theme, Clauses, Kind, Name, clause(_, _, Dot)),
    !,
    line_end(Text, Dot, Offset),
    default_column(Kind, Column),
    fact_text(Theme, local(Name), Column, Setting, Line0),
    string_concat("\n", Line0, Line).

best_anchor(KeyText, Fact, Best0-Len0, Best-Len) :-
    Fact = fact(_, S, _, _),
    key_text(S, Text),
    common_prefix_length(KeyText, Text, Len1),
    (   Len1 >= Len0
    ->  Best = Fact, Len = Len1
    ;   Best = Best0, Len = Len0
    ).

common_prefix_length(A, B, Len) :-
    atom_codes(A, CA),
    atom_codes(B, CB),
    common_prefix_length_(CA, CB, 0, Len).

common_prefix_length_([H|T1], [H|T2], L0, L) :-
    !,
    L1 is L0+1,
    common_prefix_length_(T1, T2, L1, L).
common_prefix_length_(_, _, L, L).

key_text(Setting, Text) :-
    setting_key(Setting, Key),
    arg(1, Key, First),
    term_text(First, Text).

%   value_column(+Text, +Fact, -Column)
%
%   Column is the column of the value in Fact, used to align a new fact
%   with it.

value_column(Text, Fact, Column) :-
    value_range(Fact, From, _),
    line_start(Text, From, Start),
    Column is From - Start.

default_column(colour,  40).
default_column(console, 38).
default_column(pldoc,   0).

%   fact_text(+Theme, +Style, +Column, +Setting, -Line)
%
%   Line is the text of a fact for Setting, ending in a newline.  If
%   possible, the value starts at Column.

fact_text(Theme, Style, Column, Setting, Line) :-
    setting_kind(Setting, Kind),
    setting_args(Kind, Setting, Args),
    append(KeyArgs, [Value], Args),
    (   Style = local(Name)
    ->  Prefix0 = Name
    ;   hook(Kind, _, M:H, _),
        functor(H, HName, _),
        format(atom(Prefix0), '~q:~q', [M, HName]),
        KeyArgs1 = [Theme|KeyArgs]
    ),
    (   var(KeyArgs1)
    ->  KeyArgs1 = KeyArgs
    ;   true
    ),
    maplist(term_text, KeyArgs1, KeyTexts),
    atomic_list_concat(KeyTexts, ', ', KeyText),
    format(string(Start), '~w(~w,', [Prefix0, KeyText]),
    string_length(Start, Len),
    Pad is max(1, Column-Len),
    value_text(Value, VText),
    format(string(Line), '~s~t~*|~s).\n', [Start, Len+Pad, VText]).

value_text(Value, Text) :-
    term_text(Value, Text).

term_text(Term, Text) :-
    copy_term(Term, Copy),
    numbervars(Copy, 0, _, [singletons(true)]),
    format(string(Text), '~W',
           [ Copy,
             [ quoted(true), numbervars(true), spacing(next_argument),
               portray(false)
             ]
           ]).

%   delete_edit(+Text, +Clause, -Edit)
%
%   Edit deletes Clause.  If the clause is the only thing on its lines,
%   apart from a comment, the lines are deleted.

delete_edit(Text, clause(_, Pos, Dot), edit(From, Len, "")) :-
    arg(1, Pos, Start),
    line_start(Text, Start, LineStart),
    line_end(Text, Dot, LineEnd),
    BL is Start-LineStart,
    sub_string(Text, LineStart, BL, _, Before),
    After0 is Dot+1,
    AL is LineEnd-After0,
    sub_string(Text, After0, AL, _, After),
    (   blank_string(Before),
        blank_or_comment(After)
    ->  From = LineStart,
        Len is LineEnd-LineStart
    ;   From = Start,
        Len is Dot+1-Start
    ).

blank_string(S) :-
    split_string(S, "", " \t", [""]).

blank_or_comment(S) :-
    split_string(S, "", " \t\n", [S1]),
    (   S1 == ""
    ->  true
    ;   sub_string(S1, 0, 1, _, "%")
    ).

line_start(_, 0, Start) :-
    !,
    Start = 0.
line_start(Text, Offset, Start) :-
    Prev is Offset-1,
    (   sub_string(Text, Prev, 1, _, "\n")
    ->  Start = Offset
    ;   line_start(Text, Prev, Start)
    ).

line_end(Text, Offset, End) :-
    sub_string(Text, Offset, _, 0, Rest),
    (   sub_string(Rest, B, 1, _, "\n")
    ->  End is Offset+B+1
    ;   string_length(Text, End)
    ).

%   apply_edits(+Edits, +Text0, -Text)
%
%   Apply the edits.  Inserts at the same offset are applied in the
%   order of their text.

%   The edits are applied from the end of the text, such that the
%   offsets of the other edits remain valid.  An insert at the offset
%   of another edit is applied last, such that it is not deleted.

apply_edits(Edits, Text0, Text) :-
    group_edits(Edits, Grouped),
    map_list_to_pairs(edit_order, Grouped, Keyed),
    sort(1, @>=, Keyed, SortedKeyed),
    pairs_values(SortedKeyed, Sorted),
    foldl(apply_edit, Sorted, Text0, Text).

edit_order(edit(Offset, 0, _), Offset-0) :- !.
edit_order(edit(Offset, _, _), Offset-1).

group_edits(Edits, Grouped) :-
    partition(is_insert, Edits, Inserts, Others),
    findall(Offset, member(edit(Offset, 0, _), Inserts), Offsets0),
    sort(Offsets0, Offsets),
    findall(edit(Offset, 0, Joined),
            ( member(Offset, Offsets),
              findall(T, member(edit(Offset, 0, T), Inserts), Ts0),
              sort_inserts(Ts0, Ts),
              atomic_list_concat(Ts, Joined0),
              atom_string(Joined0, Joined)
            ), Joined),
    append(Others, Joined, Grouped).

is_insert(edit(_, 0, _)).

%   Inserts after a delegation start with a blank line.  Keep one.

sort_inserts(Ts0, Ts) :-
    maplist(strip_blank, Ts0, Stripped, Blanks),
    msort(Stripped, Sorted),
    (   memberchk(true, Blanks)
    ->  Sorted = [H|T],
        string_concat("\n", H, H1),
        Ts = [H1|T]
    ;   Ts = Sorted
    ).

strip_blank(T0, T, true) :-
    string_concat("\n", T, T0),
    !.
strip_blank(T, T, false).

apply_edit(edit(Offset, Len, Insert), Text0, Text) :-
    sub_string(Text0, 0, Offset, _, Before),
    After0 is Offset+Len,
    sub_string(Text0, After0, _, 0, After),
    string_concat(Before, Insert, T1),
    string_concat(T1, After, Text).

write_file(File, Text) :-
    atom_concat(File, '.new', Tmp),
    setup_call_cleanup(
        open(Tmp, write, Out, [encoding(utf8)]),
        write(Out, Text),
        close(Out)),
    rename_file(Tmp, File).


                 /*******************************
                 *          NEW FILE            *
                 *******************************/

%   write_new_theme_file(+Theme, +Base, +File, +Settings)
%
%   Write a new theme file for Theme based on Base.  We write the
%   skeleton and add the settings as update_theme_file/3.

write_new_theme_file(Theme, Base, File, Settings) :-
    (   Base == none
    ->  true
    ;   theme_file(Base, _)
    ->  true
    ;   existence_error(theme_file, Base)
    ),
    findall(Kind, ( kind(Kind, _, _),
                    member(S, Settings),
                    setting_kind(S, Kind)
                  ), Kinds0),
    list_to_set(Kinds0, Kinds),
    skeleton(Theme, Base, Kinds, Skeleton),
    write_file(File, Skeleton),
    update_theme_file(Theme, File, Settings).

%   skeleton(+Theme, +Base, +Kinds, -Text)
%
%   Text is a theme file without settings, which has a section for each
%   of Kinds.

skeleton(Theme, Base, Kinds, Text) :-
    format(atom(Module), 'prolog_theme_~w', [Theme]),
    with_output_to(
        string(Text),
        ( format(':- module(~q, []).~n', [Module]),
          (   Base == none
          ->  true
          ;   format(':- use_module(library(theme/~q)).~n', [Base])
          ),
          format('~n/** <module> SWI-Prolog theme file -- ~w~n~n', [Theme]),
          (   Base == none
          ->  true
          ;   format('This theme is based on the ~w theme.  ', [Base])
          ),
          format('It was created using the theme~n\c
                  editor (see theme_editor/0).  To enable this theme, \c
                  start Prolog using~n\c
                  `swipl -Dtheme=~w` or add the directive below to \c
                  your init file.~n~n\c
                  ~4|:- set_prolog_flag(theme, ~q).~n*/~n~n',
                 [Theme, Theme]),
          findall(PI, ( PI = prolog:theme/1
                      ; Base \== none, PI = prolog:theme_base/2
                      ; member(Kind, Kinds), hook_pi(Kind, PI)
                      ), PIs),
          format(':- multifile~n', []),
          multifile_list(PIs),
          format('~nprolog:theme(~q).~n', [Theme]),
          (   Base == none
          ->  true
          ;   format('prolog:theme_base(~q, ~q).~n', [Theme, Base])
          ),
          forall(member(Kind, Kinds),
                 section(Theme, Kind, false))
        )).

multifile_list([PI]) :-
    !,
    format('~4|~q.~n', [PI]).
multifile_list([PI|T]) :-
    format('~4|~q,~n', [PI]),
    multifile_list(T).

hook_pi(Kind, M:Name/Arity) :-
    hook(Kind, _, M:Head, _),
    functor(Head, Name, Arity).

%   section(+Theme, +Kind, +Multifile)
%
%   Write the banner of the section for Kind and the clause that links
%   the hook to the local predicate.  If Multifile is `true`, also
%   declare the hook, as for a section added to an existing file.

section(Theme, Kind, Multifile) :-
    kind(Kind, _, Title),
    banner(Title),
    (   Multifile == true
    ->  hook_pi(Kind, PI),
        format(':- multifile ~q.~n~n', [PI])
    ;   true
    ),
    link_clause(Kind, Theme).

link_clause(console, Theme) :-
    format('prolog:console_color(~q, Class, Attributes) :-~n\c
            ~4|console_color(Class, Attributes).~n', [Theme]).
link_clause(pldoc, Theme) :-
    format('pldoc_style:theme(~q, Element, Condition, CSS) :-~n\c
            ~4|pldoc_theme(Element, Condition, CSS).~n', [Theme]).
link_clause(colour, Theme) :-
    format('pce_theme:colour(~q, Name, Value) :-~n\c
            ~4|colour(Name, Value).~n', [Theme]).

banner(Title) :-
    format('~n~t~17|/~`*t~49|~n', []),
    format('~t~17|*~t~w~t~48|*~n', [Title]),
    format('~t~17|~`*t~48|/~n~n', []).
