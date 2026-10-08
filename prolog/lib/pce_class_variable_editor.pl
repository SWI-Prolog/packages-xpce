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

:- module(pce_class_variable_editor,
          [ class_variable_editor/0,
            class_variable_editor/1,    % +Object
            save_class_variable/3       % +Class, +Name, +Value
          ]).
:- use_module(library(pce)).
:- use_module(library(pce_configeditor), [config_item/4]).
:- use_module(library(pce_style_item), [style_term/2]).
:- use_module(library(pce_preferences),
              [ preference_component/3, preference_sections/3,
                container_components/2, general_preferences/1
              ]).
:- use_module(library(tabbed_window)).
:- use_module(library(man/classification), [scope/2]).
:- pce_autoload(man_html_card, library(pce_html_manual)).
:- pce_autoload(ansi_colours_item, library(pce_colour_item)).
:- pce_autoload(pango_families_item, library(pce_font_item)).
:- pce_autoload(theme_item, library(pce_colour_item)).
:- autoload(library(pce_theme), [select_theme/1]).
:- use_module(library(persistent_frame)).
:- use_module(library(help_message)).
:- use_module(library(swi_preferences), [prolog_edit_preferences/1]).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(readutil)).
:- use_module(library(filesex)).
:- use_module(library(yall)).
:- use_module(library(pairs)).

:- pce_autoload(select_graphical, library('man/v_select')).

/** <module> Edit class variables of GUI objects

This library provides a tool to edit the  class variables that apply to
a graphical object and to the objects that  contain it.  The user picks
the object from the screen.  If the object is part of a component with
a preferences specification (see library(pce_preferences)), the tool
shows the preferences of this component.  Otherwise it shows the
commonly used class variables of the object's class, grouped by the
class that declares them.  Each value uses an editor that suits its
type.  The component is shown in the first tab.  The other tabs show
the general preferences, such as the fonts and the layout of the
windows (see pce_preferences:general/2).

Changes are applied to the running session as soon as they are
entered.  Objects that still hold the old value of a class variable are
updated, and objects created later use the new value.  Saving writes the
changes to the user's Defaults file, so they also apply to later
sessions.

A value is saved for the class of the selected object, for the class
that declares the class variable or as `*.name` for all classes.
*/

%!  class_variable_editor is det.
%!  class_variable_editor(+Object) is det.
%
%   Open the class variable editor, optionally on Object.

class_variable_editor :-
    new(E, class_variable_editor),
    send(E, open).

class_variable_editor(Object) :-
    new(E, class_variable_editor),
    send(E, open),
    send(E, edit, Object).


                 /*******************************
                 *        CLASS VARIABLES       *
                 *******************************/

%!  declared_class_variables(+Declarer, +Seen:chain, -CVs:chain) is det.
%
%   CVs are the class variables declared by the class Declarer whose
%   name is not in Seen, ordered by type and then by name, so similar
%   values are edited next to each other.  Their names are added to
%   Seen, so walking from a class up to class object shows each name
%   only with the most specific class that declares it.

declared_class_variables(Declarer, Seen, CVs) :-
    get(Declarer?class_variables, find_all,
        not(message(Seen, member, @arg1?name)), CVs),
    send(CVs, sort, ?(@prolog, compare_class_variables, @arg1, @arg2)),
    send(CVs, for_all, message(Seen, append, @arg1?name)).

compare_class_variables(CV1, CV2, Result) :-
    sort_key(CV1, Key1),
    sort_key(CV2, Key2),
    compare(Order, Key1, Key2),
    order_name(Order, Result).

sort_key(CV, TypeName-Name) :-
    get(CV, type, Type),
    base_type_name(Type, TypeName),
    get(CV, name, Name).

order_name(<, smaller).
order_name(=, equal).
order_name(>, larger).

%!  base_type_name(+Type, -Name) is det.
%
%   Name is the name of Type for ordering, ignoring that Type may allow
%   for `@default` or `@nil`: `[font]`, `font*` and `font` all have the
%   name `font`.  A range orders with its kind of number.

base_type_name(Type, Name) :-
    get(Type, kind, Kind),
    range_number_type(Kind, Name0),
    !,
    Name = Name0.
base_type_name(Type, Name) :-
    get(Type, fullname, FullName),
    atom_codes(FullName, Codes0),
    strip_optional(Codes0, Codes),
    atom_codes(Atom, Codes),
    atomic_list_concat(Alts0, '|', Atom),
    subtract(Alts0, [nil, default], Alts),
    atomic_list_concat(Alts, '|', Name).

range_number_type(int_range,  int).
range_number_type(real_range, real).

strip_optional(Codes0, Codes) :-
    append(Codes1, `*`, Codes0),
    !,
    strip_optional(Codes1, Codes).
strip_optional([0'[|Codes0], Codes) :-
    append(Codes1, `]`, Codes0),
    !,
    strip_optional(Codes1, Codes).
strip_optional(Codes, Codes).

%!  class_variable_value(+Class, +Name, -Value) is semidet.
%
%   Value is the current value of the class variable Name for Class.

class_variable_value(Class, Name, Value) :-
    get(Class, class_variable, Name, CV),
    pce_catch_error(_, get(CV, value, Value)).


%!  program_default(+Class, +Name, -Value) is semidet.
%
%   Value is the value the class variable Name for Class was declared
%   with, ignoring the Defaults files.  A default in Defaults syntax is
%   a string.  A default declared in Prolog may also be a string that
%   is not in this syntax, e.g., 'SWI-Prolog -- %s' for a name.

program_default(Class, Name, Value) :-
    get(Class, class_variable, Name, CV),
    get(CV, default, Default),
    (   send(Default, instance_of, char_array),
        pce_catch_error(_, get(CV, convert_string, Default, Value0))
    ->  Value = Value0
    ;   get(CV, type, Type),
        get(Type, check, Default, Value)
    ).


                 /*******************************
                 *             TYPES            *
                 *******************************/

%!  cv_config_type(+Type, -ConfigType, -Specials) is det.
%
%   Map the xpce Type of a class variable to a type of
%   library(pce_config).  Specials is a list of the constants `none`
%   (`@nil`) and `default` (`@default`) that Type also allows for.
%   Types we cannot map are edited as text, using `generic`.

cv_config_type(Type, ConfigType, Specials) :-
    get(Type, supers, Supers),
    (   Supers == @nil
    ->  Alts0 = []
    ;   get(Supers, map, @arg1?fullname, AltNames),
        chain_list(AltNames, Alts0)
    ),
    partition(special_type, Alts0, SpecialTypes, Alts),
    maplist(special_type, SpecialTypes, Specials),
    get(Type, kind, Kind),
    get(Type, context, Context),
    (   kind_config_type(Kind, Context, Alts, ConfigType0)
    ->  ConfigType = ConfigType0
    ;   ConfigType = generic
    ).

special_type(nil, none).
special_type(default, default).

special_type(TypeName) :-
    special_type(TypeName, _).

special_value(none, @nil).
special_value(default, @default).

kind_config_type(int, _, [], int).
kind_config_type(mono_font, _, [], mono_font).
kind_config_type(num, _, [], float).
kind_config_type(Kind, Tuple, [], between(Low, High)) :-
    range_kind(Kind),
    get(Tuple, first, L),
    get(Tuple, second, H),
    bound(Kind, L, -inf, Low),
    bound(Kind, H, inf, High).
kind_config_type(name_of, Chain, [], {}(Conj)) :-
    chain_list(Chain, Names),
    Names = [_|_],
    list_conj(Names, Conj).
kind_config_type(class, Class, Alts, ConfigType) :-
    get(Class, name, ClassName),
    class_config_type(ClassName, Alts, ConfigType).

class_config_type(bool,   [],       bool).
class_config_type(real,   [],       float).
class_config_type(font,   Alts,     font) :- subset(Alts, [name]).
class_config_type(colour, Alts,     colour) :- subset(Alts, [pixmap]).
class_config_type(image,  [],       image).
class_config_type(cursor, [],       cursor).
class_config_type(style,  [],       style).

range_kind(int_range).
range_kind(real_range).

%   bound(+Kind, +Bound, +Open, -Value)
%
%   Value is Open if Bound denotes the open end of a range.  The bounds
%   of a real range are floats, also if they are integral, such that the
%   item edits a real number.

bound(_, @nil, Open, Open) :- !.
bound(_, N, Open, Open) :-
    (   get(@pce, max_integer, N)
    ;   get(@pce, min_integer, N)
    ),
    !.
bound(real_range, N, _, F) :-
    !,
    F is float(N).
bound(_, N, _, N).

list_conj([X], X) :- !.
list_conj([H|T], (H,C)) :-
    list_conj(T, C).

%!  cv_item(+ConfigType, +Label, +CV, +Value, -Item) is det.
%
%   Create the item for editing a class variable.

%   cv_ui_type(+Class, +Name, +CV, -Type, -SpecialsType)
%
%   Type is the type used to select the item for the class variable.  This
%   is the type of the class variable, unless class_variable_ui_type/3
%   or pce_preferences:edit_type/3 gives a narrower type that makes a
%   better item, e.g., a slider.  The type of the class variable still
%   decides what is valid.  SpecialsType decides whether @nil or
%   @default are allowed, i.e., whether the row has a switch.  A type
%   from edit_type/3 is only about the item, so the class variable
%   still decides this.

cv_ui_type(Class, Name, _CV, Type, Type) :-
    class_variable_ui_type(ClassName, Name, TypeName),
    send(Class, is_a, ClassName),
    !,
    get(@pce, convert, TypeName, type, Type).
cv_ui_type(Class, Name, CV, Type, CVType) :-
    pce_preferences:edit_type(ClassName, Name, TypeName),
    send(Class, is_a, ClassName),
    !,
    get(@pce, convert, TypeName, type, Type),
    get(CV, type, CVType).
cv_ui_type(_, _, CV, Type, Type) :-
    get(CV, type, Type).

%   class_variable_ui_type(?Class, ?Name, ?Type)
%
%   The class variable Name of Class is edited as Type by the editor
%   itself.

class_variable_ui_type(display, theme, name).   % @default: see theme_item

%   dedicated_item(+Class, +Name, +Value, -Item) is semidet.
%
%   Item is an editor for a class variable whose type does not tell
%   enough about it, e.g., the ANSI colours of a terminal are a vector.

dedicated_item(Class, ansi_colours, Value, Item) :-
    send(Class, is_a, terminal_image),
    (   special_value(_, Value)
    ->  Initial = @nil
    ;   Initial = Value
    ),
    new(Item, ansi_colours_item(ansi_colours, Initial)).
dedicated_item(Class, theme, Value, Item) :-
    send(Class, is_a, display),
    new(Item, theme_item(theme, Value)).
dedicated_item(Class, Name, Value, Item) :-
    get(Class, name, ClassName),
    pce_preferences:edit_item(ClassName, Name, Value, Item),
    !.
dedicated_item(Class, pango_families, Value, Item) :-
    send(Class, is_a, font),
    \+ special_value(_, Value),
    new(Item, pango_families_item(pango_families, Value)).

cv_item(generic, Label, CV, Value, Item) :-
    !,
    new(Item, cv_text_item(Label, CV, Value)).
cv_item(Type, Label, _CV, Value, Item) :-
    (   special_value(_, Value)
    ->  config_item(Type, Label, @default, Item)
    ;   config_item(Type, Label, Value, Item)
    ).


:- pce_begin_class(cv_text_item, text_item,
                   "Edit a class variable value as Defaults text").

variable(class_variable, class_variable, get, "Class variable edited").

initialise(I, Label:name, CV:class_variable, Value:any) :->
    send(I, slot, class_variable, CV),
    (   value_to_defaults_text(Value, Text)
    ->  true
    ;   Text = ''
    ),
    send_super(I, initialise, Label, Text),
    send(I, length, 30).

selection(I, Value:any) :<-
    "Value converted as a Defaults entry"::
    get_super(I, selection, Text),
    get(I, class_variable, CV),
    get(CV, convert_string, Text, Value).

selection(I, Value:any) :->
    (   value_to_defaults_text(Value, Text)
    ->  send_super(I, selection, Text)
    ;   send_super(I, selection, '')
    ).

:- pce_end_class(cv_text_item).


                 /*******************************
                 *       VALUES AS DEFAULTS     *
                 *******************************/

%!  value_to_defaults_text(+Value, -Text:string) is semidet.
%
%   Text is the representation of Value as used in a Defaults file.

value_to_defaults_text(Value, Text) :-
    defaults_term(Value, Term),
    with_output_to(string(Text),
                   write_term(Term, [ quoted(true),
                                      spacing(next_argument),
                                      module(pce_class_variable_editor)
                                    ])).

defaults_term(Value, Value) :-
    number(Value),
    !.
defaults_term(Value, Value) :-
    atom(Value),
    !.
defaults_term(Obj, Name) :-
    send(Obj, instance_of, theme_colour),
    !,
    get(Obj, name, Name).
defaults_term(Obj, Term) :-
    send(Obj, instance_of, style),
    !,
    style_term(Obj, Term0),
    Term0 =.. [style|Args0],
    maplist(named_defaults_term, Args0, Args),
    Term =.. [style|Args].
defaults_term(Obj, Name) :-             % system cursor
    send(Obj, instance_of, cursor),
    get(Obj, image, @nil),
    get(Obj, name, Name),
    Name \== @nil,
    !.
defaults_term(@Ref, @Ref) :-
    atom(Ref),
    send(@Ref, instance_of, constant),
    !.
defaults_term(Obj, List) :-
    send(Obj, instance_of, chain),
    !,
    chain_list(Obj, Members),
    maplist(defaults_term, Members, List).
defaults_term(Obj, Text) :-
    send(Obj, instance_of, char_array),
    !,
    get(Obj, value, Name),
    atom_string(Name, Text).
defaults_term(Obj, Term) :-
    object(Obj, Term0),
    Term0 =.. [Name|Args0],
    strip_default_args(Args0, Args1),
    maplist(defaults_term, Args1, Args2),
    (   coordinates(Name)
    ->  maplist(round_coordinate, Args2, Args)
    ;   Args = Args2
    ),
    Term =.. [Name|Args].

%   Coordinates are often the result of a computation, e.g., from
%   millimeters, which results in many digits.  We round them to one
%   decimal.

coordinates(size).
coordinates(point).
coordinates(area).

round_coordinate(F, R) :-
    float(F),
    !,
    R0 is round(F*10)/10,
    (   R0 =:= truncate(R0)
    ->  R is truncate(R0)
    ;   R is float(R0)
    ).
round_coordinate(V, V).

named_defaults_term(Name := Value, Name := Term) :-
    defaults_term(Value, Term).

strip_default_args(Args0, Args) :-
    reverse(Args0, Rev0),
    drop_defaults(Rev0, Rev),
    reverse(Rev, Args).

drop_defaults([@default|T0], T) :-
    !,
    drop_defaults(T0, T).
drop_defaults(L, L).

%!  defaults_text(+CV, +Value, -Text) is semidet.
%
%   As value_to_defaults_text/2, but also verify that CV can read Text
%   back.  If the term representation cannot be read back, try the
%   object's reference.

defaults_text(CV, Value, Text) :-
    value_to_defaults_text(Value, Text),
    reads_back(CV, Text),
    !.
defaults_text(CV, @Ref, Text) :-
    atom(Ref),
    format(string(Text), '@~w', [Ref]),
    reads_back(CV, Text).

reads_back(CV, Text) :-
    pce_catch_error(_, get(CV, convert_string, Text, _)).


                 /*******************************
                 *        DEFAULTS FILE         *
                 *******************************/

%!  user_defaults_file(-File) is semidet.
%
%   File is the user's Defaults file.  Fails if xpce does not use a
%   user Defaults file.

user_defaults_file(File) :-
    get(@pce, user_defaults, Spec),
    Spec \== @nil,
    get(Spec, value, Spec1),
    get(@pce, application_data, Dir),
    get(Dir, path, DirPath),
    atomic_list_concat(Parts, '$PCEAPPDATA', Spec1),
    atomic_list_concat(Parts, DirPath, File).

%!  save_class_variable(+Class, +Name, +Value) is semidet.
%
%   Save Value for the class variable Name of Class in the user's
%   Defaults file, so it applies to later sessions.  This does not
%   change the value in the running session.  Fails if xpce does not
%   use a user Defaults file or Value cannot be represented.

save_class_variable(ClassSpec, Name, Value) :-
    get(@pce, convert, ClassSpec, class, Class),
    user_defaults_file(File),
    get(Class, name, ClassName),
    atomic_list_concat([ClassName, Name], '.', Key),
    save_value(File, Class, Key, Name, Value).

%   save_value(+File, +Target, +Key, +Name, +Value) is semidet.
%
%   Save Value for the class variable Name of Target as Key in the
%   Defaults file File.  If Value is the program default, the entry is
%   deleted.  Fails if Value cannot be represented.

save_value(File, Target, Key, Name, Value) :-
    (   program_default(Target, Name, Default),
        \+ changed(Default, Value)
    ->  delete_defaults(File, Key)          % reverted: no entry needed
    ;   get(Target, class_variable, Name, CV),
        defaults_text(CV, Value, Text),
        save_defaults(File, Key, Text)
    ).

%!  save_defaults(+File, +Key, +Text) is det.
%
%   Set Key (`class.name`) to Text in the Defaults file File.  If File
%   has an entry for Key, it is replaced.  Otherwise the entry is added
%   at the end.  Other lines are preserved.

save_defaults(File, Key, Text) :-
    ensure_defaults_file(File),
    defaults_lines(File, Lines1),
    format(string(Entry), "~w:\t~w", [Key, Text]),
    (   replace_entry(Lines1, Key, [Entry], Lines)
    ->  true
    ;   append(Lines1, [Entry], Lines)
    ),
    write_lines(File, Lines).

%!  delete_defaults(+File, +Key) is det.
%
%   Remove the entry for Key from the Defaults file File, if any.

delete_defaults(File, Key) :-
    (   exists_file(File)
    ->  defaults_lines(File, Lines0),
        (   replace_entry(Lines0, Key, [], Lines)
        ->  write_lines(File, Lines)
        ;   true
        )
    ;   true
    ).

defaults_lines(File, Lines) :-
    (   exists_file(File)
    ->  read_file_to_string(File, String, [encoding(utf8)]),
        split_string(String, "\n", "", Lines0),
        (   append(Lines1, [""], Lines0)
        ->  true
        ;   Lines1 = Lines0
        )
    ;   Lines1 = []
    ),
    Lines = Lines1.

%   ensure_defaults_file(+File)
%
%   If File does not exist, start it from the commented template of
%   user preferences.

ensure_defaults_file(File) :-
    exists_file(File),
    !.
ensure_defaults_file(File) :-
    absolute_file_name(pce('Defaults.user'), Template,
                       [ access(read),
                         file_errors(fail)
                       ]),
    !,
    file_directory_name(File, Dir),
    make_directory_path(Dir),
    copy_file(Template, File).
ensure_defaults_file(_).

%   replace_entry(+Lines0, +Key, +NewLines, -Lines) is semidet.
%
%   Replace the (possibly continued) entry for Key by NewLines.  Fails
%   if there is no entry for Key.

replace_entry([Line|Lines], Key, New, Result) :-
    entry_key(Line, Key),
    !,
    skip_continuation(Line, Lines, Rest),
    append(New, Rest, Result).
replace_entry([Line|Lines], Key, New, [Line|Rest]) :-
    skip_continuation(Line, Lines, Rest0, Cont),
    append(Cont, Rest1, Rest),
    replace_entry(Rest0, Key, New, Rest1).

entry_key(Line, Key) :-
    split_string(Line, ":", "", [KeyPart|_]),
    normalize_space(atom(Key), KeyPart),
    \+ sub_atom(Key, 0, _, _, '!').

skip_continuation(Line, Lines, Rest) :-
    skip_continuation(Line, Lines, Rest, _).

%   skip_continuation(+Line, +Lines, -Rest, -Continuation)
%
%   Continuation is the list of lines in Lines that continue Line and
%   Rest the remainder.

skip_continuation(Line, [Next|Lines], Rest, [Next|Cont]) :-
    string_concat(_, "\\", Line),
    !,
    skip_continuation(Next, Lines, Rest, Cont).
skip_continuation(_, Lines, Lines, []).

write_line(Out, Line) :-
    format(Out, '~s~n', [Line]).

write_lines(File, Lines) :-
    file_directory_name(File, Dir),
    make_directory_path(Dir),
    file_base_name(File, Base),
    format(atom(Tmp), '~w/.~w.tmp', [Dir, Base]),
    setup_call_cleanup(
        open(Tmp, write, Out, [encoding(utf8)]),
        maplist(write_line(Out), Lines),
        close(Out)),
    rename_file(Tmp, File).


                 /*******************************
                 *            REFRESH           *
                 *******************************/

%!  refresh_class_variable(+Class, +Name, +Old, +New) is det.
%
%   Update the live graphical objects that are an instance of Class,
%   get the class variable Name from Class and still hold the old
%   value Old in the slot Name.  These are the frames of all displays,
%   their windows and the graphicals displayed on these.
%
%   This is a heuristic: a slot that was set explicitly to a value
%   equal to Old is updated as well.

refresh_class_variable(Class, Name, Old, New) :-
    get(Class, class_variable, Name, CV),
    for_all_graphicals(message(@prolog, refresh_object,
                               @arg1, Class, CV, Name, Old, New)),
    refresh_users(Class, Name, Old, New).

%   for_all_graphicals(+Message)
%
%   Send Message to all frames of all displays, their windows and the
%   graphicals displayed on these.

for_all_graphicals(Refresh) :-
    send(@display_manager?members, for_all,
         message(@arg1?frames, for_all,
                 message(@prolog, refresh_frame, @arg1, Refresh))).

refresh_frame(Frame, Refresh) :-
    ignore(send(Refresh, forward, Frame)),
    send(Frame?members, for_all,
         message(@prolog, refresh_graphical, @arg1, Refresh)).

refresh_graphical(Gr, Refresh) :-
    ignore(send(Refresh, forward, Gr)),
    (   send(Gr, instance_of, device)
    ->  send(Gr?graphicals, for_all,
             message(@prolog, refresh_graphical, @arg1, Refresh))
    ;   true
    ).

%   refresh_object(+Obj, +Class, +CV, +Name, +Old, +New) is semidet.
%
%   Give Obj the value New for the slot Name if it still has the value
%   Old from the class variable CV.

refresh_object(Obj, Class, CV, Name, Old, New) :-
    send(Obj, instance_of, Class),
    get(Obj, class, ObjClass),
    get(ObjClass, class_variable, Name, CV),
    (   get(ObjClass, instance_variable, Name, _)
    ->  refresh_slot(Obj, ObjClass, Name, Old, New)
    ;   refresh_delegated(Obj, ObjClass, Name, Old, New)
    ),
    relayout(Obj).

refresh_slot(Obj, ObjClass, Name, Old, New) :-
    get(Obj, slot, Name, Current),
    Current == Old,
    (   get(ObjClass, send_method, Name, _),
        pce_catch_error(_, send(Obj, Name, New))
    ->  true
    ;   send(Obj, slot, Name, New),
        (   send(Obj, has_send_method, request_compute)
        ->  send(Obj, request_compute)
        ;   true
        ),
        (   send(Obj, has_send_method, redraw)
        ->  send(Obj, redraw)
        ;   true
        )
    ).

%   An object may hand the value of a class variable to a part it
%   delegates to, e.g., a view gives its font to its editor.  If that
%   part still holds the old value, the object is sent the new value,
%   which it handles itself or delegates to the part.  Note that
%   <-Name of the object itself answers the new class variable value.

refresh_delegated(Obj, ObjClass, Name, Old, New) :-
    send(Obj, has_send_method, Name),
    get(ObjClass, delegate, Delegates),
    get(Delegates, find,
        message(@prolog, holds_value, Obj, @arg1?name, Name, Old), _),
    pce_catch_error(_, send(Obj, Name, New)).

holds_value(Obj, Var, Name, Value) :-
    get(Obj, slot, Var, Part),
    object(Part),
    get(Part, class, PartClass),
    get(PartClass, instance_variable, Name, _),
    get(Part, slot, Name, Current),
    Current == Value.

%   refresh_users(+Class, +Name, +Old, +New)
%
%   Some class variables are not copied into a slot of a graphical, but
%   are used when drawing or are kept by objects that are not
%   graphicals.  Run the refresh of each such user class that inherits
%   the changed class variable from Class.  Only classes that are
%   loaded can have instances.

refresh_users(Class, Name, Old, New) :-
    forall(( class_variable_user(UserName, Refresh),
             get(@classes, member, UserName, User),
             send(User, is_a, Class)
           ),
           for_all_graphicals(message(@prolog, Refresh,
                                      @arg1, Name, Old, New))),
    forall(( class_variable_global_user(UserName, Name, Goal),
             get(@classes, member, UserName, User),
             send(User, is_a, Class)
           ),
           call(Goal, New)).

%   class_variable_global_user(?ClassName, ?Name, ?Goal)
%
%   call(Goal, New) updates the session after the class variable Name
%   of ClassName changed to New.  Used if there is one action for all
%   users:
%
%     - Fonts are shared objects.  `display_manager ->fonts_changed`
%       reloads them (font.scale, font.pango_families) and makes all
%       windows recompute their layout.
%     - display.theme selects the colour theme, see library(pce_theme).

class_variable_global_user(font,    _,     fonts_changed).
class_variable_global_user(display, theme, theme_changed).

fonts_changed(_New) :-
    send(@display_manager, fonts_changed).

theme_changed(New) :-
    (   New == @default
    ->  select_theme(system)
    ;   select_theme(New)
    ).

%   class_variable_user(?ClassName, ?Refresh)
%
%   Refresh(+Graphical, +Name, +Old, +New) updates Graphical after a
%   class variable of ClassName changed:
%
%     - The class variables of text_cursor define the caret of editors,
%       texts and terminals.  An editor sets the style of its caret from
%       its font and the others use them when drawing the caret.
%     - Tiles hold their border.  Update the tiles of frames and
%       tab_frames that still have the old value and lay them out.
%       Frames and tab_frames draw the separators between the tiles
%       from tile.separator_pen and tile.separator_colour.  A frame
%       draws them when its windows are redrawn.

class_variable_user(text_cursor, refresh_caret).
class_variable_user(tile,        refresh_tiles).
class_variable_user(tile,        refresh_separators).

refresh_caret(Gr, _Name, _Old, _New) :-
    (   send(Gr, instance_of, editor)
    ->  get(Gr, text_cursor, Caret),
        get(Gr, font, Font),
        send(Caret, font, Font)
    ;   (   send(Gr, instance_of, terminal_image)
        ;   send(Gr, instance_of, text_item)
        ;   send(Gr, instance_of, text)
        )
    ->  send(Gr, redraw)
    ;   true
    ).

refresh_tiles(Obj, Name, Old, New) :-
    (   send(Obj, instance_of, frame)
    ->  get(Obj, tile, Tile),
        refresh_tile(Tile, Name, Old, New),
        send(Obj, resize),
        send(Obj?members, for_all, message(@arg1, redraw))
    ;   send(Obj, instance_of, tab_frame),
        get(Obj, tile, Tile)
    ->  refresh_tile(Tile, Name, Old, New),
        send(Obj, layout)
    ;   true
    ).

%   Assign the slot: tile->border also sets the border around the root.

refresh_tile(Tile, Name, Old, New) :-
    (   get(Tile, slot, Name, Current),
        Current == Old
    ->  send(Tile, slot, Name, New)
    ;   true
    ),
    get(Tile, members, Members),
    (   Members == @nil
    ->  true
    ;   send(Members, for_all,
             message(@prolog, refresh_tile, @arg1, Name, Old, New))
    ).

refresh_separators(Gr, _Name, _Old, _New) :-
    (   send(Gr, instance_of, tab_frame)
    ->  send(Gr, update_separators)
    ;   true
    ).

relayout(Obj) :-
    send(Obj, instance_of, dialog_item),
    get(Obj, device, Dialog),
    send(Dialog, instance_of, dialog),
    !,
    send(Dialog, layout).
relayout(_).


                 /*******************************
                 *              ROW             *
                 *******************************/

:- pce_begin_class(cv_row, object,
                   "Editor for a single class variable").

variable(name,     name,         get, "Name of the class variable").
variable(class,    class,        get, "Class of the edited object").
variable(declarer, class,        get, "Class that declares the variable").
variable(item,     graphical,    get, "Item that edits the value").
variable(scope,    menu,         get, "Class to set the value for").
variable(special,  name*,        get, "Value if switched off: none or default").
variable(switch,   bool_item,    get, "Off for @nil or @default").
variable(revert,   link_label,   get, "Link to revert to the default").
variable(help,     link_label,   get, "Link to the manual entry").
variable(saved,    any,          both, "Value in the Defaults file").
variable(applied,  any,          both, "Value applied to the session").
variable(relevant, bool := @on,  get, "Off if another value makes it unused").

initialise(R, Class:class, Declarer:class, Name:name, Scope:[name]) :->
    send(R, slot, class, Class),
    send(R, slot, declarer, Declarer),
    send(R, slot, name, Name),
    get(Class, class_variable, Name, CV),
    cv_ui_type(Class, Name, CV, Type, SpecialsType),
    class_variable_value(Class, Name, Value),
    cv_config_type(Type, ConfigType, _),
    cv_config_type(SpecialsType, _, Specials),
    (   dedicated_item(Class, Name, Value, Item)
    ->  true
    ;   pce_catch_error(_, cv_item(ConfigType, Name, CV, Value, Item))
    ->  true
    ;   cv_item(generic, Name, CV, Value, Item)
    ),
    (   item_tooltip(Class, Name, CV, Tooltip)
    ->  send(Item, help_message, tag, Tooltip)
    ;   true
    ),
    value_tooltips(Item, Class, Name),
    send(R, slot, item, Item),
    send(Item, message, message(R, item_changed)),
    send(R, slot, revert,
         new(Link, link_label(revert, 'Revert',
                              message(R, revert_to_default)))),
    (   program_default(Class, Name, Default),
        value_to_defaults_text(Default, DefText)
    ->  format(string(Help), 'Revert to the default: ~w', [DefText]),
        send(Link, help_message, tag, Help)
    ;   true
    ),
    send(R, slot, help,
         new(HelpLink, link_label(help, '?', message(R, show_help)))),
    (   class_variable_doc(Class, Name, _)
    ->  send(HelpLink, help_message, tag, 'Show the manual entry')
    ;   send(HelpLink, show, @off)
    ),
    make_switch(R, Specials),
    send(R, show_special, Value),
    send(R, slot, scope, new(Menu, menu(scope, cycle))),
    send(Menu, show_label, @off),
    get(Class, name, ClassName),
    get(Declarer, name, DeclName),
    send(Menu, append, menu_item(ClassName, @default, ClassName)),
    (   DeclName == ClassName
    ->  true
    ;   send(Menu, append, menu_item(DeclName, @default, DeclName))
    ),
    send(Menu, append, menu_item(*, @default, *)),
    (   Scope == @default
    ->  true
    ;   send(Menu, selection, Scope)
    ),
    (   Scope == @default,              % all choices set the same
        DeclName == ClassName,
        declaring_classes(Name, [_])
    ->  send(Menu, active, @off),
        send(Menu, help_message, tag,
             'Only this class has this class variable')
    ;   send(Menu, help_message, tag,
             'Class for which to set the value.  * sets it for all classes')
    ),
    send(R, saved, Value),
    send(R, applied, Value),
    send(R, update_relevant),
    send(R, update_revert).

%   value_tooltips(+Item, +Class, +Name)
%
%   If Item offers a set of values (a cycle menu or a combo box) and
%   the manual describes these values, show the descriptions as
%   tooltips of the values.

value_tooltips(Item, Class, Name) :-
    send(Item, instance_of, menu),
    !,
    (   class_variable_value_docs(Class, Name, Docs)
    ->  send(Item?members, for_all,
             message(@prolog, menu_item_tooltip, @arg1, prolog(Docs)))
    ;   true
    ).
value_tooltips(Item, Class, Name) :-
    (   send(Item, instance_of, text_item),
        get(Item, value_set, Set),
        send(Set, instance_of, chain),
        class_variable_value_docs(Class, Name, Docs)
    ->  new(Items, chain),
        send(Set, for_all,
             message(@prolog, value_item, Items, @arg1, prolog(Docs))),
        send(Item, value_set, Items)
    ;   true
    ).

menu_item_tooltip(MI, Docs) :-
    get(MI, value, Value),
    (   memberchk(Value-Doc, Docs)
    ->  send(MI, help_message, tag, Doc)
    ;   true
    ).

value_item(Items, Value, Docs) :-
    new(DI, dict_item(Value)),
    (   memberchk(Value-Doc, Docs)
    ->  send(DI, help_message, tag, Doc)
    ;   true
    ),
    send(Items, append, DI).

item_changed(R, _Value:[any]) :->
    "The user changed the value in the item"::
    send(R, value_changed).

%   ->value_changed applies a value as soon as the user enters it, such
%   that the effect is visible immediately.  An invalid value, e.g., in
%   a text item, is not applied.

value_changed(R) :->
    "Apply the value and update the revert link"::
    ignore(pce_catch_error(_, send(R, apply))),
    send(R, update_revert),
    (   get(R?item, frame, F),
        send(F, has_send_method, sync_rows)
    ->  send(F, sync_rows)              % the same variable in another row
    ;   true
    ).

%   ->sync shows the current value of the class variable if it was
%   changed elsewhere: by another row for the same class variable or
%   outside the editor, e.g., the theme from the menu of the IDE.  The
%   value is not applied again.

sync(R) :->
    "Show the value of the class variable if it changed elsewhere"::
    get(R, class, Class),
    get(R, name, Name),
    (   class_variable_value(Class, Name, Value),
        get(R, applied, Applied),
        changed(Applied, Value)
    ->  send(R, value, Value),
        send(R, applied, Value)
    ;   true
    ),
    send(R, update_relevant).

update_revert(R) :->
    "Show the revert link if the value differs from the default"::
    get(R, revert, Link),
    (   get(R, class, Class),
        get(R, name, Name),
        program_default(Class, Name, Default),
        pce_catch_error(_, get(R, value, Value)),
        changed(Default, Value)
    ->  send(Link, show, @on)
    ;   send(Link, show, @off)
    ).

show_help(R) :->
    "Show the manual entry of the class variable"::
    get(R, class, Class),
    get(R, name, Name),
    (   class_variable_entry(Class, Name, Entry, _)
    ->  get(Class, name, ClassName),
        format(atom(Title), '~w.~w', [ClassName, Name]),
        new(W, class_variable_help_window(Title, Entry)),
        (   get(R?item, frame, Frame)
        ->  send(W, transient_for, Frame)
        ;   true
        ),
        send(W, open)
    ;   send(R?item, report, warning, 'No manual entry')
    ).

revert_to_default(R) :->
    "Show the program default value"::
    get(R, class, Class),
    get(R, name, Name),
    program_default(Class, Name, Default),
    send(R, value, Default),
    send(R, value_changed).

%   make_switch(+Row, +Specials)
%
%   Create the switch that is off if the value is @nil or @default.  No
%   class variable allows for both.  If the type allows for neither,
%   the switch is on and inactive.

make_switch(R, Specials) :-
    new(Switch, bool_item(switch, @on, message(R, switched, @arg1))),
    send(Switch, show_label, @off),
    send(R, slot, switch, Switch),
    (   Specials = [Special|_]
    ->  send(R, slot, special, Special),
        special_value(Special, Value),
        special_help(Special, Help),
        format(string(Tag), 'Off: ~w (~p)', [Help, Value]),
        send(Switch, help_message, tag, Tag)
    ;   send(R, slot, special, @nil),
        send(Switch, active, @off),
        send(Switch, help_message, tag, 'This value is always defined')
    ).

special_help(none,    'no value').
special_help(default, 'use the default').

switched(R, _On:bool) :->
    "The user flipped the switch"::
    send(R, activate_item),
    send(R, value_changed).

show_special(R, Value:any) :->
    "Show whether Value is the special value"::
    get(R, switch, Switch),
    (   get(R, special, Special), Special \== @nil,
        special_value(Special, Value)
    ->  send(Switch, selection, @off)
    ;   send(Switch, selection, @on)
    ),
    send(R, activate_item).

activate_item(R) :->
    "The item is active if the switch is on and the value is used"::
    get(R?switch, selection, On),
    get(R, relevant, Relevant),
    (   On == @on, Relevant == @on
    ->  send(R?item, active, @on)
    ;   send(R?item, active, @off)
    ).

update_relevant(R) :->
    "Deactivate the row if the value it edits is not used"::
    get(R, class, Class),
    get(R, name, Name),
    (   requirement(Class, Name, OnClass, OnName, Required)
    ->  (   class_variable_value(OnClass, OnName, Current),
            \+ changed(Current, Required)
        ->  Relevant = @on
        ;   Relevant = @off
        )
    ;   Relevant = @on
    ),
    (   get(R, relevant, Relevant)
    ->  true
    ;   send(R, slot, relevant, Relevant),
        send(R, activate_item)
    ).

%   item_tooltip(+Class, +Name, +CV, -Tooltip) is semidet.
%
%   Tooltip is the summary of the class variable CV and the class
%   variable it requires, if any.

item_tooltip(Class, Name, CV, Tooltip) :-
    (   get(CV, summary, Summary),
        Summary \== @nil, Summary \== @default
    ->  get(Summary, value, Lines0),
        Lines = [Lines0]
    ;   Lines = []
    ),
    (   requirement(Class, Name, OnClass, OnName, Required)
    ->  get(OnClass, name, OnClassName),
        value_to_defaults_text(Required, RequiredText),
        format(string(Req), 'Only used if ~w.~w is ~w',
               [OnClassName, OnName, RequiredText]),
        append(Lines, [Req], All)
    ;   All = Lines
    ),
    All \== [],
    atomic_list_concat(All, '\n', Tooltip).

%   requirement(+Class, +Name, -OnClass, -OnName, -Value) is semidet.
%
%   The class variable Name of Class is only used if the class variable
%   OnName of OnClass has the value Value.

requirement(Class, Name, OnClass, OnName, Value) :-
    class_variable_requires(ClassName, Name, OnClassName, OnName, Value),
    send(Class, is_a, ClassName),
    !,
    get(@pce, convert, OnClassName, class, OnClass).

%   class_variable_requires(?Class, ?Name, ?OnClass, ?OnName, ?Value)
%
%   The class variable Name of Class is only used if OnClass.OnName is
%   Value.  The editor deactivates the row of Name otherwise.  The bell
%   is only rung by `graphical ->alert` if the visual bell is off.

class_variable_requires(display,   volume,        graphical, visual_bell, @off).
class_variable_requires(display,   bell_pitch,    graphical, visual_bell, @off).
class_variable_requires(display,   bell_duration, graphical, visual_bell, @off).
class_variable_requires(graphical, visual_bell_duration,
                                                  graphical, visual_bell, @on).

append(R, D:dialog) :->
    "Append the items of the row as a single line to dialog D"::
    get(R, name, Name),
    new(G, dialog_group(Name, group)),
    send(G, gap, size(8, 0)),
    send(G, append, R?scope),
    send(G, append, R?switch, right),
    send(G, append, R?help, right),        % a column: fixed width
    get(R, item, Item),
    (   send(Item, has_send_method, hor_stretch)
    ->  send(Item, hor_stretch, 0)       % stretched items grow without bound
    ;   true
    ),
    send(G, append, Item, right),
    send(G, append, R?revert, right),      % acts on the value: next to it
    send(D, append, G),
    send(G, alignment, left),
    send(R, update_revert).

value(R, Value:any) :<-
    "Value from the editor"::
    (   get(R?switch, selection, @off),
        get(R, special, Special), Special \== @nil
    ->  special_value(Special, Value)
    ;   get(R, item, Item),
        get(Item, selection, Value)
    ).

value(R, Value:any) :->
    "Show Value in the editor"::
    send(R, show_special, Value),
    (   get(R, special, Special),       % shown by the switch
        Special \== @nil,
        special_value(Special, Value)
    ->  true
    ;   get(R, item, Item),             % e.g., @default for theme_item
        send(Item, selection, Value)
    ),
    send(R, update_revert).

target(R, Target:class) :<-
    "Class to set the value for"::
    get(R?scope, selection, Scope),
    (   Scope == (*)
    ->  get(R, declarer, Target)
    ;   get(@pce, convert, Scope, class, Target)
    ).

key(R, Key:name) :<-
    "Key in the Defaults file"::
    get(R?scope, selection, Scope),
    get(R, name, Name),
    atomic_list_concat([Scope, Name], '.', Key).

changed(Old, New) :-
    Old == New,
    !,
    fail.
changed(Old, New) :-
    number(Old), number(New),
    !,
    Old =\= New.
changed(Old, New) :-
    \+ ( value_to_defaults_text(Old, Text),
         value_to_defaults_text(New, Text)
       ).

apply(R) :->
    "Apply the value to the session"::
    get(R, value, New),
    get(R, applied, Old),
    (   changed(Old, New)
    ->  get(R, name, Name),
        get(R, targets, Targets),
        maplist(apply_class_variable(Name, Old, New), Targets),
        send(R, applied, New)
    ;   true
    ).

targets(R, Targets:prolog) :<-
    "Classes to apply the value to"::
    get(R?scope, selection, Scope),
    get(R, name, Name),
    (   Scope == (*)
    ->  star_classes(Name, Targets)
    ;   get(R, target, Target),
        Targets = [Target]
    ).

apply_class_variable(Name, Old, New, Target) :-
    (   class_variable_value(Target, Name, Current)
    ->  true
    ;   Current = Old
    ),
    send(Target, class_variable_value, Name, New),
    refresh_class_variable(Target, Name, Current, New),
    get(Target, name, TargetName),
    forall(pce_preferences:class_variable_changed(TargetName, Name, New),
           true).

%   star_classes(+Name, -Classes) is det.
%
%   Classes are the loaded classes for which `*.Name` in the Defaults
%   file defines the class variable Name: the classes that have their
%   own class variable Name and for which neither the class itself nor
%   one of its super classes has an entry `Class.Name` in a Defaults
%   file, as such an entry takes precedence.

star_classes(Name, Classes) :-
    declaring_classes(Name, Classes0),
    defaults_keys(Keys),
    exclude(specific_default(Keys, Name), Classes0, Classes).

%   declaring_classes(+Name, -Classes) is det.
%
%   Classes are the loaded classes that have their own class variable
%   Name.

declaring_classes(Name, Classes) :-
    new(All, chain),
    send(@classes, for_all, message(All, append, @arg2)),
    get(All, find_all,
        message(@prolog, own_class_variable, @arg1, Name), Own),
    chain_list(Own, Classes).

own_class_variable(Class, Name) :-
    get(Class, slot, class_variables, CVs),
    send(CVs, instance_of, chain),
    get(CVs, find, @arg1?name == Name, _).

specific_default(Keys, Name, Class) :-
    get(Class, name, ClassName),
    atomic_list_concat([ClassName, Name], '.', Key),
    memberchk(Key, Keys),
    !.
specific_default(Keys, Name, Class) :-
    get(Class, super_class, Super),
    Super \== @nil,
    specific_default(Keys, Name, Super).

%   defaults_keys(-Keys) is det.
%
%   Keys are the keys of the entries in the system and user Defaults
%   files.

defaults_keys(Keys) :-
    findall(Key,
            ( defaults_file(File),
              defaults_lines(File, Lines),
              member(Line, Lines),
              entry_key(Line, Key)
            ),
            Keys).

defaults_file(File) :-
    absolute_file_name(pce('Defaults'), File,
                       [ access(read),
                         file_errors(fail)
                       ]).
defaults_file(File) :-
    user_defaults_file(File).

revert(R) :->
    "Restore the saved value"::
    get(R, saved, Saved),
    send(R, value, Saved),
    send(R, apply).

save(R, File:name) :->
    "Write the value to the Defaults file File"::
    send(R, apply),
    get(R, value, New),
    get(R, saved, Old),
    (   changed(Old, New)
    ->  get(R, target, Target),
        get(R, name, Name),
        get(R, key, Key),
        (   save_value(File, Target, Key, Name, New)
        ->  send(R, saved, New)
        ;   send(R?item, report, error,
                 'Cannot represent value for %s', Name),
            fail
        )
    ;   true
    ),
    send(R, update_revert).

:- pce_end_class(cv_row).


                 /*******************************
                 *         DOCUMENTATION        *
                 *******************************/

%!  class_variable_doc(+Class, +Name, -Markdown:string) is semidet.
%
%   Markdown is the description of the class variable Name of Class
%   from the reference manual.  See class_variable_entry/4.

class_variable_doc(Class, Name, Markdown) :-
    class_variable_entry(Class, Name, _Entry, Markdown).

%!  class_variable_entry(+Class, +Name, -Entry, -Markdown:string) is semidet.
%
%   Entry is the class_variable or variable object whose manual entry
%   describes the class variable Name of Class and Markdown is this
%   description.  If the manual does not describe the class variable
%   itself, use the description of the instance variable with the same
%   name.  If Class does not describe either, try its super classes.

class_variable_entry(Class, Name, Entry, Markdown) :-
    get(Class, name, ClassName),
    (   class_doc_lines(ClassName, Lines),
        (   entry_body(Lines, ClassName, '.', Name, Body),
            \+ see_only(Body)
        ->  get(Class, class_variable, Name, Entry)
        ;   member(Op, ['<->', '<-', '->']),
            entry_body(Lines, ClassName, Op, Name, Body)
        ->  get(Class, instance_variable, Name, Entry)
        )
    ->  atomic_list_concat(Body, '\n', Markdown)
    ;   get(Class, super_class, Super),
        Super \== @nil,
        class_variable_entry(Super, Name, Entry, Markdown)
    ).

class_doc_lines(ClassName, Lines) :-
    absolute_file_name(swi('xpce/man/refmanual/md'), Dir,
                       [ file_type(directory), access(read),
                         file_errors(fail)
                       ]),
    format(atom(File), '~w/~w.md', [Dir, ClassName]),
    exists_file(File),
    read_file_to_string(File, String, [encoding(utf8)]),
    split_string(String, "\n", "", Lines).

%   entry_body(+Lines, +Class, +Op, +Name, -Body) is semidet.
%
%   Body are the (dedented) lines that describe the manual entry
%   `- Class Op Name: ...`.

entry_body(Lines, Class, Op, Name, Body) :-
    format(string(Prefix), "- ~w~w~w:", [Class, Op, Name]),
    append(_, [Head|Rest], Lines),
    string_concat(Prefix, _, Head),
    !,
    body_lines(Rest, Body0),
    exclude(==(""), Body0, NonEmpty),
    NonEmpty \== [],
    Body = Body0.

body_lines([Line|Lines], [Text|Body]) :-
    (   Line == ""
    ;   string_concat("    ", _, Line)
    ),
    !,
    (   string_concat("    ", Text, Line)
    ->  true
    ;   Text = Line
    ),
    body_lines(Lines, Body).
body_lines(_, []).

see_only(Body) :-
    forall(member(Line, Body),
           ( normalize_space(string(L), Line),
             ( L == "" ; string_concat("@see", _, L) )
           )).

%!  class_variable_value_docs(+Class, +Name, -Docs:list) is semidet.
%
%   Docs is a list Value-Description, describing the values of the
%   class variable Name of Class.  The descriptions come from a list of
%   the values in its manual entry.  If the entry has no such list, it
%   is taken from the entry of the same class it refers to as <-Ref,
%   <->Ref or ->Ref, e.g., "See <-style for the values".

class_variable_value_docs(Class, Name, Docs) :-
    class_variable_entry(Class, Name, Entry, Markdown),
    split_string(Markdown, "\n", "", Lines),
    (   value_docs(Lines, Docs),
        Docs \== []
    ->  true
    ;   get(Entry?context, name, ClassName),
        class_doc_lines(ClassName, ClassLines),
        referenced_member(Markdown, Op, Ref),
        entry_body(ClassLines, ClassName, Op, Ref, Body),
        value_docs(Body, Docs),
        Docs \== []
    ->  true
    ).

referenced_member(Markdown, Op, Name) :-
    member(Op, ['<->', '<-', '->']),
    sub_string(Markdown, B, L, _, Op),
    A is B+L,
    sub_string(Markdown, A, _, 0, After),
    string_codes(After, Codes),
    phrase(csyms(NameCodes), Codes, _),
    NameCodes \== [],
    atom_codes(Name, NameCodes).

csyms([H|T]) --> [H], { code_type(H, csym) }, !, csyms(T).
csyms([]) --> [].

%   value_docs(+Lines, -Docs) is det.
%
%   Docs are the Value-Description pairs of the bullets in Lines that
%   consist of a single word.  The description are the lines below the
%   bullet that are indented more than the bullet.

value_docs([], []).
value_docs([Line|Lines], Docs) :-
    (   value_bullet(Line, Indent, Value)
    ->  description_lines(Lines, Indent, DescLines, Rest),
        atomic_list_concat(DescLines, ' ', Desc0),
        normalize_space(string(Desc), Desc0),
        Docs = [Value-Desc|Docs1],
        value_docs(Rest, Docs1)
    ;   value_docs(Lines, Docs)
    ).

value_bullet(Line, Indent, Value) :-
    indentation(Line, Indent, Text),
    string_concat("- ", Word0, Text),
    normalize_space(atom(Value), Word0),
    Value \== '',
    atom_codes(Value, Codes),
    forall(member(C, Codes), code_type(C, csym)).

description_lines([Line|Lines], Indent, [Text|Desc], Rest) :-
    indentation(Line, LineIndent, Text),
    Text \== "",
    LineIndent > Indent,
    !,
    description_lines(Lines, Indent, Desc, Rest).
description_lines(Lines, _, [], Lines).

%   indentation(+Line, -Indent, -Text)
%
%   Indent is the column where Text starts.  A tab moves to the next
%   multiple of 8.

indentation(Line, Indent, Text) :-
    string_codes(Line, Codes),
    indentation(Codes, 0, Indent, TextCodes),
    string_codes(Text, TextCodes).

indentation([0' |T], I0, I, Text) :-
    !,
    I1 is I0+1,
    indentation(T, I1, I, Text).
indentation([0'\t|T], I0, I, Text) :-
    !,
    I1 is (I0//8+1)*8,
    indentation(T, I1, I, Text).
indentation(Text, I, I, Text).

:- pce_begin_class(class_variable_help_window, frame,
                   "Show the manual entry of a class variable").

initialise(F, Title:name, Entry:behaviour) :->
    send_super(F, initialise, Title),
    send(F, append, new(TD, dialog)),
    send(TD, pen, 0),
    send(TD, gap, size(5, 5)),
    send(TD, append, new(TB, tool_bar)),
    send(new(Card, man_html_card), below, TD),
    send(Card, size, size(500, 200)),
    send(Card, scrollbars, vertical),
    get(Card, history, History),
    get(History, button, backward, Back),
    get(History, button, forward, Forward),
    send(TB, append, Back),
    send(TB, append, Forward),
    send(Card, selection, Entry),
    send(new(D, dialog), below, Card),
    send(D, append, button(close, message(F, destroy))).

:- pce_end_class(class_variable_help_window).


                 /*******************************
                 *             LINK             *
                 *******************************/

:- pce_begin_class(link_label, label,
                   "Label that acts as a hyperlink").

variable(message,   code*, both, "Executed on click").
variable(link_text, name,   get,  "Text shown if <-show is @on").
variable(hovered,   bool := @off, get, "Pointer is over the link").

%   The label has a fixed width, so hiding the link by showing no text
%   does not move the items right of it.

initialise(L, Name:name, Text:name, Message:[code]*) :->
    "Create from name, text and message"::
    send_super(L, initialise, Name, Text),
    send(L, slot, link_text, Text),
    default(Message, @nil, Msg),
    send(L, message, Msg),
    send(L, colour, ui_link),
    send(L, cursor, hand2),
    send(L, length, 0),
    send(L, compute),
    get(L, width, W),                   % includes the label's border
    send(L, width, W).

show(L, Show:bool) :->
    "Show or hide the link, keeping its width"::
    (   Show == @on
    ->  send(L, selection, L?link_text)
    ;   send(L, selection, ''),
        send(L, slot, hovered, @off)
    ).

shown(L) :->
    "True if the link is shown"::
    \+ get(L, selection, '').

event(L, Ev:event) :->
    "Underline on hover and execute the message on click"::
    (   \+ send(L, shown)
    ->  fail
    ;   send(Ev, is_a, area_enter)
    ->  send(L, slot, hovered, @on),
        send(L, redraw)
    ;   send(Ev, is_a, area_exit)
    ->  send(L, slot, hovered, @off),
        send(L, redraw)
    ;   send(Ev, is_a, ms_left_up)
    ->  get(L, message, Msg),
        (   Msg == @nil
        ->  true
        ;   send(Msg, forward, L)
        )
    ;   send(Ev, is_a, ms_left)          % do not pass clicks on
    ).

'_redraw_area'(L, A:area) :->
    "Draw the label and, while hovered, an underline"::
    send_super(L, '_redraw_area', A),
    (   get(L, hovered, @on)
    ->  get(L, area, area(X, Y, _, _)),
        get(L, reference, point(_, RY)),
        get(L?font, width, L?link_text, W),
        LY is Y+RY+2,
        X2 is X+W,
        send(L, graphics_state, 1, @default, L?colour), % our pen is 0
        send(L, draw_line, X, LY, X2, LY)
    ;   true
    ).

:- pce_end_class(link_label).


                 /*******************************
                 *             FRAME            *
                 *******************************/

:- pce_begin_class(class_variable_editor, persistent_frame,
                   "Edit class variables of GUI objects").

variable(object,    visual*,   get,  "Object being edited").
variable(path,      chain,     get,  "Containment path, from the frame down").
variable(component, cv_dialog, get,  "Tab with the current object").
variable(general,   chain,     get,  "Tabs with general preferences").
variable(target,    cv_dialog*, none, "Tab being filled").

initialise(F) :->
    send_super(F, initialise, 'Class variable editor'),
    send(F, slot, path, new(chain)),
    send(F, slot, general, new(chain)),
    send(F, append, new(Top, dialog)),
    send(Top, name, top_dialog),
    send(Top, pen, 0),
    send(Top, gap, size(5, 5)),
    send(Top?tile, hor_stretch, 100),   % stacked on the editor, so it must
                                        % stretch for the editor to do so
    fixed_height(Top),
    send(Top, append,
         new(All, bool_item(show_all, @off, message(F, refresh)))),
    send(All, label, 'Show all class variables'),
    send(All, help_message, tag,
         'Show all class variables of the components rather than their preferences'),
    send(All, alignment, left),         % not in a column with the path
    send(Top, append,
         new(Select, button(select_object, message(F, grab))), right),
    send(Select, help_message, tag, 'Select an object from the screen'),
    send(Select, alignment, right),     % flush right
    send(Top, resize_message, message(F, show_path)),
    send(Top, append, new(Path, dialog_group(path, group))),
    send(Path, gap, size(4, 0)),
    send(Path, alignment, left),
    send(Path, append, new(None, label(none, 'No object selected', italic))),
    send(None, length, 0),
    new(Tabs, cv_tabbed_window),
    send(Tabs, name, tabs),
    send(Tabs, size, size(760, 450)),
    send(Tabs, below, Top),
    send(Tabs, append, new(V, cv_dialog(editor)), 'Components'),
    send(F, slot, component, V),
    general_preferences(General),
    forall(member(tab(Name, Label, Sections), General),
           ( send(Tabs, append, new(G, cv_dialog(Name)), Label),
             send(G, attribute, sections, prolog(Sections)),
             send(F?general, append, G)
           )),
    send(Tabs, on_top, V),
    send(F, fill_general),
    send(new(Bottom, dialog), below, Tabs),
    send(Bottom, name, bottom_dialog),
    send(Bottom?tile, hor_stretch, 100),
    fixed_height(Bottom),
    send(Bottom, append, new(Save, button(save, message(F, save)))),
    send(Save, help_message, tag,
         'Save the changed values in your Defaults file'),
    send(Bottom, append, new(Revert, button(revert, message(F, revert)))),
    send(Revert, help_message, tag,
         'Restore the values from your Defaults file'),
    send(Bottom, append, new(Edit, button(edit_defaults_file,
                                          message(F, edit_defaults_file)))),
    send(Edit, help_message, tag,
         'Edit your Defaults file in the editor'),
    send(Bottom, append, button(quit, message(F, destroy))),
    send(Bottom, append, label(reporter), right).

fixed_height(Window) :-
    get(Window, tile, Tile),
    send(Tile, ver_stretch, 0),
    send(Tile, ver_shrink, 0).

grab(F) :->
    "Select an object from the screen"::
    new(D, select_graphical('Select object to configure')),
    send(D, attribute, report_to, F),
    get(D, select, @arg1?frame \== F, F?area?center, Obj),
    send(D, destroy),
    Obj \== @nil,
    send(F, edit, Obj).

edit(F, Obj:visual) :->
    "Show the containment path of Obj and edit it"::
    send(F, tab, F?component),
    containment_path(Obj, Path0),
    reverse(Path0, Path),
    get(F, path, Chain),
    send(Chain, clear),
    maplist(append_chain(Chain), Path),
    send(F, select_path, Obj).

append_chain(Chain, Obj) :-
    send(Chain, append, Obj).

select_path(F, Obj:visual) :->
    "Edit Obj from the containment path"::
    send(F, show_class_variables, Obj),
    send(F, show_path).

%   ->show_path shows the containment path.  If it does not fit, the
%   leading elements are replaced by "...", whose tooltip shows them.

show_path(F) :->
    "Show the containment path, the current object in bold"::
    get(F, member, top_dialog, Top),
    get(Top, member, path, G),
    get(F, object, Current),
    get(F, path, Chain),
    \+ send(Chain, empty),               % keep "No object selected"
    !,
    chain_list(Chain, Path),
    get(Top?area, width, TopWidth),
    get(Top, border, size(BW, _)),
    Avail is TopWidth - 2*BW,
    show_path(Path, [], F, G, Current, Avail),
    send(Top, layout, Top?visible?size).   % flush the button right
show_path(F) :->
    get(F, member, top_dialog, Top),
    send(Top, layout, Top?visible?size).

show_path(Path, Hidden, F, G, Current, Avail) :-
    get(G?graphicals, copy, Old),
    send(Old, for_all, message(@arg1, destroy)),
    (   Hidden == []
    ->  true
    ;   hidden_tooltip(Hidden, Tip),
        send(G, append, new(Dots, label(hidden, '...'))),
        send(Dots, length, 0),
        send(Dots, help_message, tag, Tip)
    ),
    forall(member(Obj, Path),
           append_path_item(F, G, Obj, Current)),
    send(G, layout_dialog),
    get(G?area, width, Width),
    (   Width > Avail,
        Path = [First|Rest],
        Rest = [_,_|_]                  % keep the last two
    ->  append(Hidden, [First], Hidden1),
        show_path(Rest, Hidden1, F, G, Current, Avail)
    ;   true
    ).

hidden_tooltip(Hidden, Tip) :-
    maplist(class_name, Hidden, Names),
    atomic_list_concat(Names, ' > ', Tip).

class_name(Obj, Name) :-
    get(Obj, class_name, Name).

show_class_variables(F, Obj:visual) :->
    "Fill the editor with the class variables of Obj"::
    send(F, slot, object, Obj),
    get(F, component, D),
    send(F, fill, D,
         message(@prolog, append_object, F, Obj)).

%   The preferences of the component Obj is part of or, if there is no
%   such component, the commonly used class variables of Obj, followed
%   by the preferences of the containers of Obj (e.g., where a tool is
%   placed in the IDE).

append_object(F, Obj) :-
    (   preference_component(Obj, Root, Spec)
    ->  preference_sections(Root, Spec, Sections0)
    ;   get(Obj, class, Class),
        new(Seen, chain),
        append_groups(F, Class, Class, Seen),
        Sections0 = []
    ),
    container_components(Obj, Containers),
    foldl(container_sections, Containers, Sections0, Sections1),
    unique_sections(Sections1, Sections),
    maplist(append_section(F), Sections).

container_sections(Root-Spec, Sections0, Sections) :-
    preference_sections(Root, Spec, New),
    append(Sections0, New, Sections).

%   A class may appear twice, e.g., for a tool inside another
%   pane_stack.  Keep the first.

unique_sections([], []).
unique_sections([section(Class, Names)|T0], [section(Class, Names)|T]) :-
    exclude(same_section_class(Class), T0, T1),
    unique_sections(T1, T).

same_section_class(Class, section(Class2, _)) :-
    Class2 == Class.

fill_general(F) :->
    "Fill the tabs with general preferences"::
    send(F?general, for_all,
         message(F, fill, @arg1,
                 message(@prolog, append_sections, F,
                         @arg1?sections))).

append_sections(F, Sections) :-
    maplist(append_section(F), Sections).

fill(F, D:cv_dialog, Filler:code) :->
    "Fill D with the rows Filler appends"::
    send(D, clear),
    get(D, rows, Rows),
    send(Rows, clear),
    send(F, slot, target, D),
    call_cleanup(send(Filler, forward),
                 send(F, slot, target, @nil)),
    align_labels(Rows),
    align_scope_menus(Rows),            % so the switches form a column
    send(D, layout),
    send(D, scroll_to, point(0,0)).

rows(F, Rows:chain) :<-
    "Rows of the current object"::
    get(F?component, rows, Rows).

tab(F, D:cv_dialog) :->
    "Show the tab D"::
    get(F, member, tabs, Tabs),
    send(Tabs, on_top, D).

tab_changed(F, D:window) :->
    "The tab D is shown; the path only applies to the components"::
    get(F, member, top_dialog, Top),
    get(Top, member, path, Path),
    (   get(F, component, D)
    ->  send(Path, displayed, @on),
        send(F, show_path)
    ;   send(Path, displayed, @off),
        send(Top, layout, Top?visible?size)
    ).

sync_rows(F) :->
    "Show the values of class variables that changed elsewhere"::
    send(F?tab_dialogs, for_all,
         message(@arg1?rows, for_all, message(@arg1, sync))).

input_focus(F, Val:bool) :->
    "Show changes made while I did not have the focus"::
    send_super(F, input_focus, Val),
    (   Val == @on
    ->  send(F, sync_rows)
    ;   true
    ).

tab_dialogs(F, Dialogs:chain) :<-
    "All tabs"::
    get(F?general, copy, Dialogs),
    send(Dialogs, prepend, F?component).

show_all(F) :->
    "True if all class variables are shown rather than the preferences"::
    get(F, member, top_dialog, Top),
    get(Top, member, show_all, Switch),
    get(Switch, selection, @on).

refresh(F) :->
    "Show the class variables of all tabs again"::
    get(F, object, Obj),
    (   Obj == @nil
    ->  true
    ;   send(F, show_class_variables, Obj)
    ),
    send(F, fill_general).

append_group(F, Class:class, Declarer:class, Seen:chain) :->
    "Append the class variables declared by Declarer not in Seen"::
    declared_class_variables(Declarer, Seen, CVs0),
    (   send(F, show_all)
    ->  CVs = CVs0
    ;   get(CVs0, find_all,             % only the commonly used ones
            message(@prolog, basic_class_variable, Class, @arg1), CVs)
    ),
    (   send(CVs, empty)
    ->  true
    ;   get(F, editor_dialog, D),
        append_section_label(D, Declarer),
        send(CVs, for_all,
             message(F, append_row, Class, Declarer, @arg1?name))
    ).

append_row(F, Class:class, Declarer:class, Name:name, Scope:[name]) :->
    "Append a row for the class variable Name, if we can edit it"::
    (   new_row(Class, Declarer, Name, Scope, Row)
    ->  get(F, editor_dialog, D),
        send(D?rows, append, Row),
        send(Row, append, D)
    ;   true
    ).

editor_dialog(F, D:cv_dialog) :<-
    "Tab being filled"::
    get(F, slot, target, D),
    D \== @nil.

%   align_labels(+Rows)
%
%   Give the items of all rows the same label width, such that the
%   values start in the same column.

align_scope_menus(Rows) :-
    get(Rows, map, @arg1?scope, Menus),
    send(Menus, for_all, message(@arg1, compute)),
    new(Max, number(0)),
    send(Menus, for_all, message(Max, maximum, @arg1?value_width)),
    send(Menus, for_all, message(@arg1, value_width, Max?value)).

align_labels(Rows) :-
    get(Rows, map, @arg1?item, Items),
    get(Items, find_all, message(@arg1, has_get_method, label_width),
        Labelled),
    new(Max, number(0)),
    send(Labelled, for_all, message(Max, maximum, @arg1?label_width)),
    send(Labelled, for_all, message(@arg1, label_width, Max?value)).

%   append_section(+Frame, +Section)
%
%   Append the rows for a section of a preferences specification.  If
%   all class variables are to be shown, these are all class variables
%   of the class of the section rather than those it specifies.  The
%   rows of a section `*(Class)` set the value for all classes.  A
%   heading(Title, Sections) shows the rows of Sections under Title.
%   These are always the rows specified: all class variables of their
%   classes would not belong under Title.

append_section(F, heading(Title, Sections)) :-
    !,
    get(F, editor_dialog, D),
    send(D, append, new(L, label(heading, Title, bold))),
    send(L, alignment, left),
    maplist(append_specified_rows(F), Sections).
append_section(F, Section) :-
    get(F, editor_dialog, D),
    append_section_title(D, Section),
    append_section_rows(F, Section).

append_section_title(D, section(*(_), _)) :-
    !,
    send(D, append, new(L, label(class, 'All classes', bold))),
    send(L, help_message, tag, 'These values apply to all classes'),
    send(L, alignment, left).
append_section_title(D, section(Class, _)) :-
    append_section_label(D, Class).

append_section_rows(F, section(Class, _)) :-
    Class \= *(_),
    send(F, show_all),
    !,
    all_class_variables(Class, CVs),
    send(CVs, for_all,
         message(@prolog, append_preference, F, Class, @arg1?name)).
append_section_rows(F, Section) :-
    append_specified_rows(F, Section).

append_specified_rows(F, section(*(Class), Names)) :-
    !,
    forall(member(Name, Names),
           ( declaring_class(Class, Name, Declarer)
           ->  send(F, append_row, Class, Declarer, Name, *)
           ;   true
           )).
append_specified_rows(F, section(Class, Names)) :-
    maplist(append_preference(F, Class), Names).

%   all_class_variables(+Class, -CVs:chain) is det.
%
%   CVs are the class variables of Class, including the inherited ones,
%   ordered by type and then by name.

all_class_variables(Class, CVs) :-
    new(CVs, chain),
    new(Seen, chain),
    add_class_variables(Class, Seen, CVs),
    send(CVs, sort, ?(@prolog, compare_class_variables, @arg1, @arg2)).

add_class_variables(Declarer, Seen, CVs) :-
    declared_class_variables(Declarer, Seen, Declared),
    send(CVs, merge, Declared),
    get(Declarer, super_class, Super),
    (   Super == @nil
    ->  true
    ;   add_class_variables(Super, Seen, CVs)
    ).

%   append_section_label(+Dialog, +Class)
%
%   Append the title of a section with the class variables of Class.
%   This is the summary of Class, which tells the user more than the
%   class name.  The tooltip gives the class name.

append_section_label(D, Class) :-
    get(Class, name, ClassName),
    (   class_summary(Class, Summary)
    ->  send(D, append, new(L, label(class, Summary, bold))),
        send(L, help_message, tag, ClassName)
    ;   send(D, append, new(L, label(class, ClassName, bold)))
    ),
    send(L, alignment, left).

%!  class_summary(+Class, -Summary:string) is semidet.
%
%   Summary is the non-empty summary of Class.

class_summary(Class, Summary) :-
    get(Class, summary, String),
    String \== @nil,
    get(String, value, Value),
    Value \== '',
    atom_string(Value, Summary).

%   A spec may name class variables of several subclasses, e.g., the
%   indentation of the Prolog mode for a PceEmacs mode, so we silently
%   skip the names Class does not have.

append_preference(F, Class, Name) :-
    (   declaring_class(Class, Name, Declarer)
    ->  send(F, append_row, Class, Declarer, Name)
    ;   true
    ).

%   declaring_class(+Class, +Name, -Declarer) is semidet.
%
%   Declarer is the class from Class up that declares the class
%   variable Name.

declaring_class(Class, Name, Declarer) :-
    get(Class?class_variables, find, @arg1?name == Name, _),
    !,
    Declarer = Class.
declaring_class(Class, Name, Declarer) :-
    get(Class, super_class, Super),
    Super \== @nil,
    declaring_class(Super, Name, Declarer).

%   basic_class_variable(+Class, +CV) is semidet.
%
%   True if the manual classifies the class variable CV of Class or a
%   super class of it as `basic`.

basic_class_variable(Class, CV) :-
    get(CV, name, Name),
    basic_class_variable_(Class, Name).

basic_class_variable_(Class, Name) :-
    get(Class, name, ClassName),
    atomic_list_concat(['R', ClassName, Name], '.', Id),
    scope(Id, Scope),
    !,
    Scope == basic.
basic_class_variable_(Class, Name) :-
    get(Class, super_class, Super),
    Super \== @nil,
    basic_class_variable_(Super, Name).

%   append_groups(+Frame, +Class, +Declarer, +Seen)
%
%   Append the class variables of Class, walking from Declarer up to
%   class object.

append_groups(F, Class, Declarer, Seen) :-
    send(F, append_group, Class, Declarer, Seen),
    get(Declarer, super_class, Super),
    (   Super == @nil
    ->  true
    ;   append_groups(F, Class, Super, Seen)
    ).

%   append_path_item(+Frame, +Group, +Obj, +Current)
%
%   Append Obj to the containment path shown in Group.  Obj is a link
%   to edit Obj, unless it is the Current object.  The tooltip is the
%   summary of its class.

append_path_item(F, G, Obj, Current) :-
    (   send(G?graphicals, empty)
    ->  true
    ;   send(G, append, new(Sep, label(separator, '>')), right),
        send(Sep, length, 0)
    ),
    get(Obj, class_name, Label),
    (   Obj == Current
    ->  new(Item, label(current, Label, bold)),
        send(Item, length, 0)
    ;   new(Item, link_label(link, Label, message(F, select_path, Obj)))
    ),
    send(G, append, Item, right),
    (   path_tooltip(Obj, Tip)
    ->  send(Item, help_message, tag, Tip)
    ;   true
    ).

%   The tooltip is the summary of the class, followed by the name of
%   Obj if it has one.

path_tooltip(Obj, Tip) :-
    get(Obj, class, Class),
    get(Obj, class_name, ClassName),
    (   class_summary(Class, Summary)
    ->  true
    ;   Summary = ClassName
    ),
    (   get(Obj, name, Name),
        atom(Name),
        Name \== ClassName
    ->  format(string(Tip), '~w (~w)', [Summary, Name])
    ;   Tip = Summary
    ).

new_row(Class, Declarer, Name, Scope, Row) :-
    pce_catch_error(_, new(Row, cv_row(Class, Declarer, Name, Scope))).

revert(F) :->
    "Restore the saved values"::
    send(F?tab_dialogs, for_all,
         message(@arg1?rows, for_all, message(@arg1, revert))),
    send(F, report, status, 'Reverted').

save(F) :->
    "Save all changes to the Defaults file"::
    (   user_defaults_file(File)
    ->  send(F?tab_dialogs, for_all,
             message(@arg1?rows, for_all, message(@arg1, save, File))),
        send(F, report, status, 'Saved to %s', File)
    ;   send(F, report, error, 'No user Defaults file')
    ).

edit_defaults_file(_F) :->
    "Edit the Defaults file"::
    prolog_edit_preferences(xpce).

:- pce_end_class(class_variable_editor).


:- pce_begin_class(cv_dialog, dialog,
                   "Tab of the class variable editor").

variable(rows, chain, get, "Rows shown").

initialise(D, Name:name) :->
    send_super(D, initialise),
    send(D, name, Name),
    send(D, slot, rows, new(chain)),
    send(D, scrollbars, vertical),
    send(D, restrict_scroll, @on).

sections(D, Sections:prolog) :<-
    "Sections of a tab with general preferences"::
    get(D, attribute, sections, Sections).

:- pce_end_class(cv_dialog).


:- pce_begin_class(cv_tabbed_window, tabbed_window,
                   "Tabs of the class variable editor").

tab_stack_class(_W, Class:name) :<-
    "Tell the editor which tab is shown"::
    Class = cv_tab_stack.

:- pce_end_class(cv_tabbed_window).


:- pce_begin_class(cv_tab_stack, tab_stack,
                   "Tab stack of the class variable editor").

on_top(TS, Tab:tab) :->
    "Tell the editor that another tab is shown"::
    send_super(TS, on_top, Tab),
    (   get(TS, frame, F),
        send(F, has_send_method, tab_changed)
    ->  send(F, tab_changed, Tab?window)
    ;   true
    ).

:- pce_end_class(cv_tab_stack).

%!  containment_path(+Obj, -Path) is det.
%
%   Path is a list from Obj up to the frame that holds it.

containment_path(Obj, [Obj|Path]) :-
    \+ send(Obj, instance_of, frame),
    get(Obj, contained_in, Parent),
    send(Parent, instance_of, visual),
    \+ send(Parent, instance_of, display),
    !,
    containment_path(Parent, Path).
containment_path(Obj, [Obj]).
