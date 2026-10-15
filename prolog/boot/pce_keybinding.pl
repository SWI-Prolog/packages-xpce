/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org/packages/xpce/
    Copyright (c)  2002-2025, University of Amsterdam
                              VU University Amsterdam
                              SWI-Prolog Solutions b.v.
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materits provided with the
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

:- encoding(utf8).
:- module(pce_keybinding, []).
:- use_module(pce_principal).
:- use_module(pce_realise).
:- use_module(library(lists)).
:- use_module(library(pce_util)).

:- multifile
    binding/3,                                    % +Style, +TableName, -Bindings
    alt_binding_function/2.                       % +Func, -AltFunc

message_level(silent).
%message_level(informational).


                 /*******************************
                 *      STYLE PREFERENCES       *
                 *******************************/

%!  binding(+ModeName, +TableName, +Modifications)
%
%   Specify bindings for alternative key-binding-styles.   Note  that on
%   Apple, the command key is  mapped  to   the  SDL3  GUI key, which is
%   mapped to Emacs ``\s-`` (super).
%
%   The tables for text entry in dialogs (see dialog_table/1) follow the
%   class variable `key_binding.dialog_style`; all others, notably those
%   of PceEmacs, follow `key_binding.style`.  Both overlay the Emacs
%   bindings: a key a style does not bind keeps its Emacs function.
%
%   @arg ModeName         Name of the key-binding-style
%   @arg TableName        Syntax table to modify
%   @arg Modifications    List of Key-Method

%   Text entry in dialogs.  On Windows and Linux (`cua`), native wins
%   over Emacs for Control-A, which selects all.  Alt-<letter> belongs
%   to the accelerators of the dialog, so the Emacs Meta keys of the
%   basic tables are hidden (`none`), and Escape to the `cancel` button,
%   so it is no Meta prefix.

binding(cua, text_item,
        [ '\\e'                   = none,
          '\\C-a'                 = select_all,
          '\\eb'                  = none,
          '\\ef'                  = none,
          '\\ed'                  = none,
          '\\C-x'                 = cut,
          '\\C-<cursor_left>'     = backward_word,
          '\\C-<cursor_right>'    = forward_word,
          '\\C-\\S-<cursor_left>'  = backward_word,
          '\\C-\\S-<cursor_right>' = forward_word,
          '\\C-BS'                = backward_kill_word,
          '\\C-DEL'               = kill_word
        ]).
binding(cua, text_item_view,
        [ '\\e'                   = none,
          '\\C-a'                 = select_all,
          '\\eb'                  = none,
          '\\ef'                  = none,
          '\\C-<cursor_left>'     = backward_word,
          '\\C-<cursor_right>'    = forward_word,
          '\\C-\\S-<cursor_left>'  = backward_word,
          '\\C-\\S-<cursor_right>' = forward_word
        ]).
binding(cua, text,
        [ '\\e'                   = none,
          '\\C-a'                 = select_all,
          '\\eb'                  = none,
          '\\ef'                  = none,
          '\\ed'                  = none,
          '\\C-x'                 = cut,
          '\\C-<cursor_left>'     = backward_word,
          '\\C-<cursor_right>'    = forward_word,
          '\\C-\\S-<cursor_left>'  = backward_word,
          '\\C-\\S-<cursor_right>' = forward_word,
          '\\C-BS'                = backward_kill_word,
          '\\C-DEL'               = kill_word
        ]).

%   On MacOS the Emacs control keys are native too, so the two styles
%   only meet on Option, which moves by word as \e<cursor_left>.  As
%   with `cua`, Option-<letter> is left to the dialog accelerators.

binding(apple, text_item,
        [ '\\e'                   = none,
          '\\s-c'                 = copy,
          '\\eb'                  = none,
          '\\ef'                  = none,
          '\\ed'                  = none,
          '\\s-x'                 = cut,
          '\\s-v'                 = paste,
          '\\s-a'                 = select_all,
          '\\s-<cursor_left>'     = beginning_of_line,
          '\\s-<cursor_right>'    = end_of_line,
          '\\S-\\s-<cursor_left>'  = beginning_of_line,
          '\\S-\\s-<cursor_right>' = end_of_line,
          '\\e<cursor_left>'      = backward_word,
          '\\e<cursor_right>'     = forward_word,
          '\\e\\S-<cursor_left>'   = backward_word,
          '\\e\\S-<cursor_right>'  = forward_word
        ]).
binding(apple, text_item_view,
        [ '\\e'                   = none,
          '\\s-c'                 = copy,
          '\\eb'                  = none,
          '\\ef'                  = none,
          '\\s-a'                 = select_all,
          '\\s-<cursor_left>'     = beginning_of_line,
          '\\s-<cursor_right>'    = end_of_line,
          '\\S-\\s-<cursor_left>'  = beginning_of_line,
          '\\S-\\s-<cursor_right>' = end_of_line,
          '\\e<cursor_left>'      = backward_word,
          '\\e<cursor_right>'     = forward_word,
          '\\e\\S-<cursor_left>'   = backward_word,
          '\\e\\S-<cursor_right>'  = forward_word
        ]).
binding(apple, text,
        [ '\\e'                   = none,
          '\\s-c'                 = copy,
          '\\eb'                  = none,
          '\\ef'                  = none,
          '\\ed'                  = none,
          '\\s-x'                 = cut,
          '\\s-v'                 = paste,
          '\\s-a'                 = select_all,
          '\\s-<cursor_left>'     = beginning_of_line,
          '\\s-<cursor_right>'    = end_of_line,
          '\\S-\\s-<cursor_left>'  = beginning_of_line,
          '\\S-\\s-<cursor_right>' = end_of_line,
          '\\e<cursor_left>'      = backward_word,
          '\\e<cursor_right>'     = forward_word,
          '\\e\\S-<cursor_left>'   = backward_word,
          '\\e\\S-<cursor_right>'  = forward_word
        ]).

%   Everything else, notably PceEmacs.

binding(cua, editor,
        [ '\\C-v' = paste,
          '\\C-s' = save_buffer
        ]).
binding(cua, 'emacs$fundamental',
        [ '\\C-f' = isearch_forward,
          '\\C-o' = open,
          '\\C-n' = new,
          '\\C-p' = print
        ]).
binding(apple, editor,
        [ '\\s-s'      = save_buffer,
          '\\S-\\s-s'  = save_as,
          '\\s-w'      = close,
          '\\s-z'      = undo,
          '\\s-a'      = select_all,
          '\\s-f'      = find,
%         '\\s-g'      = find_next,
%         '\\S-\\s-g'  = find_previous,
	'\\s-<cursor_left>'  = beginning_of_line,
	'\\s-<cursor_right>' = end_of_line,
	'\\s-<cursor_up>'    = point_to_top_of_file,
	'\\s-<cursor_down>'  = point_to_bottom_of_file,
          '\\s-/'	         = comment_region,
          '\\s-['	         = undent_region, % TODO: If no region, current line
          '\\s-]'	         = indent_region,
          '\\s-k'	         = kill_line,
          '\\s-t'	         = transpose_chars
        ]).
binding(apple, 'emacs$fundamental',
        [ '\\s-c'     = copy,
          '\\s-x'     = cut,
          '\\s-v'     = paste,

          '\\s-0'     = font_default,
          '\\s--'     = font_reduce,
          '\\s-='     = font_magnify,

          '\\s-n'     = new,
          '\\s-o'     = open
        ]).
binding(apple, terminal,
        [ '\\C-c'     = interrupt,
          '\\s-c'     = copy,
          '\\s-x'     = cut,
          '\\s-v'     = paste,
          '\\s-a'     = select_all,

          '\\s-0'     = font_default,
          '\\s--'     = font_reduce,
          '\\s-='     = font_magnify
        ]).
binding(apple, epilog,
        [ '\\s-z'     = undo,
          '\\S-\\s-d' = split_horizontally,
          '\\s-d'     = split_vertically,
          '\\s-n'     = new_window,
          '\\s-t'     = new_tab,
          '\\s-k'     = clear_screen,
          '\\s-w'     = close
        ]).


                 /*******************************
                 *       CHANGE BINDINGS        *
                 *******************************/

%!  dialog_table(?TableName) is nondet.
%
%   Key-binding tables for text entry in dialogs.  These follow
%   `key_binding.dialog_style` rather than `key_binding.style`.

dialog_table(text_item).
dialog_table(text_item_view).
dialog_table(text).

%!  table_domain(+TableName, -Domain) is det.
%
%   Domain is `dialog` for a dialog_table/1 and `editor` otherwise.  The
%   domain decides which class variable holds the style of a table.

table_domain(Table, Domain) :-
    (   dialog_table(Table)
    ->  Domain = dialog
    ;   Domain = editor
    ).

domain_variable(editor, style).
domain_variable(dialog, dialog_style).

%!  set_keybinding_style(+Id) is det.
%!  set_keybinding_style(+Domain, +Id) is det.
%
%   Runtime modification of the current key-binding style.  The first
%   form sets the style of the `editor` domain.

set_keybinding_style(Style) :-
    set_keybinding_style(editor, Style).

set_keybinding_style(Domain, Style) :-
    current_style(Domain, Style),
    !.
set_keybinding_style(Domain, Style) :-
    new(Tables, chain),
    send(@key_bindings, for_all, message(Tables, append, @arg2)),
    chain_list(Tables, KBs),
    free(Tables),
    forall(member(KB, KBs), restyle(Domain, Style, KB)),
    set_style(Domain, Style).

%   restyle(+Domain, +Style, +KeyBinding)
%
%   Undo the modifications of the style of KeyBinding if it is in
%   Domain, and apply those of Style.

restyle(Domain, Style, KB) :-
    get(KB, name, Table),
    (   table_domain(Table, Domain)
    ->  send(KB, unapply_user_bindings),
        send(KB, unmodify),
        (   binding(Style, Table, Modifications)
        ->  modify(Modifications, KB)
        ;   true
        ),
        send(KB, apply_user_bindings)
    ;   true
    ).


modify([], _).
modify([Mod|T], KB) :-
    modify1(Mod, KB),
    modify(T, KB).

modify1(Key = Command, KB) :-
    get(KB?bindings, value, Key, Command),
    !.
modify1(Key = Command, KB) :-
    send(KB, save_default, Key),
    send(KB, function, Key, Command),
    get(KB, name, Table),
    message_level(Level),
    print_message(Level, format('~w (~p): ~w --> ~w',
                                [Table, KB, Key, Command])).
modify1(delete(Key), KB) :-
    \+ get(KB?bindings, value, Key, _),
    !.
modify1(delete(Key), KB) :-
    send(KB, save_default, Key),
    get(KB, bindings, Bindings),
    send(Bindings, delete, Key),
    get(KB, name, Table),
    message_level(Level),
    print_message(Level, format('~w: deleted ~w', [Table, Key])).


                 /*******************************
                 *        DYNAMIC TABLES        *
                 *******************************/

:- pce_extend_class(key_binding).

class_variable(style, name,
               [ unix(emacs),
                 windows(cua),
                 apple(apple)
               ],
               "Basic binding style (emacs,cua,apple)").
class_variable(dialog_style, name,
               [ unix(cua),
                 windows(cua),
                 apple(apple)
               ],
               "Binding style for text entry in dialogs (emacs,cua,apple)").

%!  current_style(-Style) is det.
%!  current_style(+Domain, -Style) is det.
%!  set_style(+Domain, +Style) is det.
%
%   Manipulate the style of a domain (see table_domain/2).  The style
%   is stored in the class-variable key_binding.style or
%   key_binding.dialog_style, so it can be set in the users
%   preferences file.

current_style(Style) :-
    current_style(editor, Style).

current_style(Domain, Style) :-
    domain_variable(Domain, Name),
    get(@pce, convert, key_binding, class, Class),
    get(Class, class_variable, Name, Var),
    get(Var, value, Style).

set_style(Domain, Style) :-
    domain_variable(Domain, Name),
    get(@pce, convert, key_binding, class, Class),
    get(Class, class_variable, Name, Var),
    send(Var, value, Style).


apply_preferences(KB) :->
    "Apply CUA-mode preferences"::
    send(KB, apply_cua),
    send(KB, apply_user_bindings).  % bind from ~/.xpce/Defaults

apply_cua(KB) :->
    "Apply our local overrides"::
    get(KB, name, Name),
    (   atom(Name),
        table_domain(Name, Domain),
        current_style(Domain, Mode),
        Mode \== emacs,
        binding(Mode, Name, Modifications)
    ->  modify(Modifications, KB)
    ;   true
    ).

save_default(KB, Key:name) :->
    "Save default binding for Key"::
    (   get(KB, attribute, modified, Undo)
    ->  true
    ;   send(KB, attribute, modified, new(Undo, sheet))
    ),
    (   get(Undo, value, Key, _)
    ->  true                        % Already saved this one
    ;   get(KB, bindings, Bindings),
        (   get(Bindings, value, Key, Command)
        ->  send(Undo, value, Key, Command)
        ;   send(Undo, value, Key, @nil)
        )
    ).

unmodify(KB) :->
    "Replay recorded modifications"::
    (   get(KB, attribute, modified, Undo)
    ->  send(Undo, for_all,
             message(KB, unbind, @arg1?name, @arg1?value)),
        send(KB, delete_attribute, modified)
    ;   true
    ).

unbind(KB, Key:name, Command:[name|code]*) :->
    "Restore saved binding for Key"::
    get(KB, name, Table),
    message_level(Level),
    (   Command == @nil
    ->  get(KB, bindings, Sheet),
        send(Sheet, delete, Key),
        print_message(Level,
                      format('~w: deleted ~w', [Table, Key]))
    ;   send(KB, function, Key, Command),
        print_message(Level,
                      format('~w (~p): ~w --> ~w',
                             [Table, KB, Key, Command]))
    ).


                 /*******************************
                 *         USER BINDINGS        *
                 *******************************/

%   The keys the user binds in a named table are kept in the class
%   variable key_binding.<table>, a chain of `Key := Function`, where
%   Function `none` unbinds Key.  Class key_binding declares these for
%   the tables it defines itself, e.g., key_binding.editor.  For other
%   tables the class variable is created when the table asks for it,
%   so a table that is created later, e.g., when a PceEmacs mode is
%   loaded, finds the keys in the Defaults file.  A `$` in the name of
%   the table, as in `emacs$prolog`, is written as `_`, as the Defaults
%   file does not accept it.  The keys are applied on
%   top of the bindings of the table and its style.  The binding they
%   replace is kept in the attribute `user_modified`, a sheet that maps
%   the key to the replaced function or @nil, to revert them.

:- dynamic
    table_class_variable/1.             % Name

apply_user_bindings(KB) :->
    "Apply the keys the user bound in me"::
    (   get(KB, user_bindings, Pairs)
    ->  forall(member(Key-Function, Pairs),
               send(KB, apply_user_binding, Key, Function))
    ;   true
    ).

user_bindings_variable(KB, Var:class_variable) :<-
    "Class variable that holds the keys the user bound in me"::
    get(KB, name, Table),
    atom(Table),
    atomic_list_concat(Parts, '$', Table),
    atomic_list_concat(Parts, '_', Name),
    get(@pce, convert, key_binding, class, Class),
    (   get(Class, class_variable, Name, Var)
    ->  get(Var, type, Type),           % not, e.g., key_binding.style
        get(Type, name, chain)
    ;   new(Var, class_variable(Class, Name, [], chain,
                                'Keys bound by the user')),
        assertz(table_class_variable(Name))
    ).

user_bindings(KB, Pairs:prolog) :<-
    "List of Key-Function the user bound in me"::
    get(KB, user_bindings_variable, Var),
    get(Var, value, Chain),
    chain_list(Chain, Bindings),
    Bindings \== [],
    maplist(binding_pair, Bindings, Pairs).

binding_pair(Binding, Key-Function) :-
    get(Binding, name, Key0),
    get(Binding, value, Function0),
    to_name(Key0, Key),
    to_name(Function0, Function).

to_name(Atom, Atom) :-
    atom(Atom),
    !.
to_name(Text, Atom) :-
    get(Text, value, Atom).

user_bindings(KB, Pairs:prolog) :->
    "Make the user bind the Key-Function pairs, replacing the old ones"::
    get(KB, user_bindings_variable, Var),
    new(Chain, chain),
    forall(member(Key-Function, Pairs),
           ( new(Binding, ':='(Key, Function)), % not a named argument
             send(Chain, append, Binding)
           )),
    send(Var, value, Chain),
    send(KB, unapply_user_bindings),
    send(KB, apply_user_bindings).

apply_user_binding(KB, Key:name, Function:name) :->
    "Bind Key to Function, remembering the old binding"::
    (   get(KB, attribute, user_modified, Undo)
    ->  true
    ;   send(KB, attribute, user_modified, new(Undo, sheet))
    ),
    (   get(Undo, value, Key, _)
    ->  true
    ;   get(KB?bindings, value, Key, Old)
    ->  send(Undo, value, Key, Old)
    ;   send(Undo, value, Key, @nil)
    ),
    send(KB, function, Key, Function).

unapply_user_bindings(KB) :->
    "Restore the bindings the user changed"::
    (   get(KB, attribute, user_modified, Undo)
    ->  send(KB, delete_attribute, user_modified),
        send(Undo, for_all,
             message(KB, restore_binding, @arg1?name, @arg1?value))
    ;   true
    ).

restore_binding(KB, Key:name, Function:'name|code*') :->
    "Bind Key to Function; @nil deletes the binding"::
    (   Function == @nil
    ->  send(KB?bindings, delete, Key)
    ;   send(KB, function, Key, Function)
    ).

user_binding(KB, Key:name, Function:name) :<-
    "The function the user bound to Key in me; fails if none"::
    get(KB, user_bindings, Pairs),
    memberchk(Key-Function, Pairs).

user_binding(KB, Key:name, Function:name*) :->
    "Make the user bind Key to Function in me; @nil reverts Key"::
    (   get(KB, user_bindings, Pairs0)
    ->  true
    ;   Pairs0 = []
    ),
    (   Function == @nil
    ->  delete_key(Pairs0, Key, Pairs)
    ;   memberchk(Key-_, Pairs0)
    ->  replace_key(Pairs0, Key, Function, Pairs)
    ;   append(Pairs0, [Key-Function], Pairs)
    ),
    send(KB, user_bindings, Pairs).

delete_key([], _, []).
delete_key([K-_|T0], Key, T) :-
    K == Key,
    !,
    delete_key(T0, Key, T).
delete_key([H|T0], Key, [H|T]) :-
    delete_key(T0, Key, T).

replace_key([], _, _, []).
replace_key([K-_|T0], Key, F, [K-F|T]) :-
    K == Key,
    !,
    replace_key(T0, Key, F, T).
replace_key([H|T0], Key, F, [H|T]) :-
    replace_key(T0, Key, F, T).

accelerator_label(KB, Cmd:'name|code', Accell:name) :<-
    "Get a string to use as accelerator label in a menu"::
    findall(KeySet,
            ( alt_function(Cmd, Command),
              get(KB, binding, Command, KeyChain),
              chain_list(KeyChain, KeySet)
            ), KeySets),
    append(KeySets, Keys),
    preferred_key(Keys, Key),
    human_accelerator(Key, Accell),
    !.

%       In a menu, a thin space starts each modifier.  A list of keys
%       reads better with the modifiers of a key joined (`Ctrl-Shift-A`,
%       not `Ctrl- Shift-A`) and a space between the keys of a sequence,
%       also after a prefix that is a plain key (`Ctrl-X 4 f`).

key_label(_KB, Key:name, Label:name) :<-
    "Human readable label for Key, e.g. Ctrl-X Ctrl-S"::
    key_sequence(Key, Keys),
    maplist(single_key_label, Keys, Labels),
    atomic_list_concat(Labels, ' ', Label).

single_key_label('\\e', Label) :-
    !,
    single_key_label('ESC', Label).
single_key_label(Key, Label) :-
    human_accelerator(Key, Accel),
    atomic_list_concat(Parts, ' ', Accel),
    atomic_list_concat(Parts, Label).

%!  key_sequence(+Key, -Keys) is det.
%
%   Split a key sequence as written by characterName() into the keys
%   that are typed one after the other.  A key is a sequence of
%   modifiers (`\e`, `\C-`, `\S-`, `\s-`) followed by a named key
%   (`<cursor_up>`, `RET`) or a character.  A lone `\e` is Escape.

key_sequence(Key, Keys) :-
    atom_codes(Key, Codes),
    phrase(keys(Keys), Codes).

keys([Key|Keys]) -->
    key(Codes),
    !,
    { atom_codes(Key, Codes) },
    keys(Keys).
keys([]) -->
    [].

key(Codes) -->
    key_modifier(M),
    key(K),
    !,
    { append(M, K, Codes) }.
key([0'<|Codes]) -->
    "<",
    string_without(`>`, Name),
    ">",
    !,
    { append(Name, `>`, Codes) }.
key(Codes) -->
    { named_key(Codes) },
    Codes,
    !.
key(`\\e`) -->
    `\\e`,
    !.
key([C]) -->
    [C].

key_modifier(`\\e`)  --> `\\e`.
key_modifier(`\\C-`) --> `\\C-`.
key_modifier(`\\S-`) --> `\\S-`.
key_modifier(`\\s-`) --> `\\s-`.

named_key(`RET`).
named_key(`TAB`).
named_key(`SPC`).
named_key(`LFD`).
named_key(`DEL`).
named_key(`BS`).
named_key(`ESC`).

string_without(End, [C|T]) -->
    [C],
    { \+ memberchk(C, End) },
    !,
    string_without(End, T).
string_without(_, []) -->
    [].

alt_function(Func, Func).
alt_function(Func, Alt) :-
    alt_binding_function(Func, Alt).

preferred_key(Keys, Key) :-             % Prefer returning Apple Command keys
    member(Key, Keys),
    sub_atom(Key, _, _, _, '\\s-'),
    !.
preferred_key(Keys, Key) :-
    member(K, Keys),
    select(Alt, Keys, Keys1),
    preferred(K, Alt),
    !,
    preferred_key(Keys1, Key).
preferred_key([Key|_], Key).

preferred('\\C-z', '\\C-_').
preferred(Short, Long) :-
    atom_length(Short, ShortLen),
    atom_length(Long, LongLen),
    ShortLen < LongLen.

%!      human_accelerator(+Key, -Human)
%
%       Translate XPCE key-sequences in conventional notation.  Should be
%       part of the XPCE kernel someday.

:- dynamic
    accel_cache/2.

human_accelerator(Key, Text) :-
    accel_cache(Key, Text),
    !.
human_accelerator(Key, Text) :-
    human_keys(Key, String),
    atom_string(Text, String),
    assert(accel_cache(Key, Text)).

human_keys(Acc, String) :-
    key_name(Key, Human),
    sub_string(Acc, B, _, A, Key),
    !,
    sub_string(Acc, 0, B, _, Pre),
    sub_string(Acc, _, A, 0, Post0),
    capitalise_key(Post0, Post),
    atomics_to_string([Pre,Human,Post], String0),
    human_keys(String0, String).
human_keys(Acc, Acc).

:- if(current_prolog_flag(apple, true)).
key_name('\\C-',     '⌃').
key_name('\\e',      '⌥').
key_name('\\S-\\s-', '⌘⇧').             % Get Apple ordering
key_name('\\S-',     '⇧').
key_name('\\s-',     '⌘').
:- endif.
key_name('\\C-', '\u2009Ctrl-').
key_name('\\e',  '\u2009Alt-').
key_name('\\S-', '\u2009Shift-').
key_name('\\s-', '\u2009Super-').

%       Named keys, which characterName() writes between angle
%       brackets.  Without these the menu shows e.g. `Ctrl-<cursor_left>'.

:- if(current_prolog_flag(apple, true)).
key_name('<cursor_left>',  '\u2190').
key_name('<cursor_right>', '\u2192').
key_name('<cursor_up>',    '\u2191').
key_name('<cursor_down>',  '\u2193').
key_name('<cursor_home>',  '\u2196').
key_name('<end>',          '\u2198').
key_name('<page_up>',      '\u21DE').
key_name('<page_down>',    '\u21DF').
:- endif.
key_name('<cursor_left>',  'Left').
key_name('<cursor_right>', 'Right').
key_name('<cursor_up>',    'Up').
key_name('<cursor_down>',  'Down').
key_name('<cursor_home>',  'Home').
key_name('<end>',          'End').
key_name('<page_up>',      'PageUp').
key_name('<page_down>',    'PageDown').
key_name('<begin>',        'Begin').
key_name('<insert>',       'Insert').
key_name('<find>',         'Find').
key_name('<select>',       'Select').
key_name('<execute>',      'Execute').
key_name('<print>',        'Print').
key_name('<pause>',        'Pause').
key_name('<break>',        'Break').
key_name('<menu>',         'Menu').
key_name('<help>',         'Help').
key_name('<undo>',         'Undo').
key_name('<redo>',         'Redo').
key_name('<cancel>',       'Cancel').
key_name('<cut>',          'Cut').
key_name('<copy>',         'Copy').
key_name('<paste>',        'Paste').

%       Keys characterName() writes as a bare uppercase name.

:- if(current_prolog_flag(apple, true)).
key_name('RET', '\u21A9').
key_name('TAB', '\u21E5').
key_name('SPC', '\u2423').
key_name('BS',  '\u232B').
key_name('DEL', '\u2326').
key_name('ESC', '\u238B').
:- endif.
key_name('RET', 'Return').
key_name('TAB', 'Tab').
key_name('SPC', 'Space').
key_name('BS',  'Backspace').
key_name('DEL', 'Delete').
key_name('ESC', 'Esc').

key_name(Key, Name) :-                  % <f1> ... <f24>
    between(1, 24, N),
    atomic_list_concat(['<f', N, '>'], Key),
    atomic_list_concat(['F', N], Name).

%!  capitalise_key(+Post0, -Post)
%
%   Given we matched  a  modifier   sequence,  capitalise  the  modified
%   character. We must avoid capitalizing function names.
%
%   @tbd Eventually we probably move to Emacs function names enclosed in
%   angle brackets.

capitalise_key(S0, S) :-
    string_length(S0, 1),
    !,
    string_upper(S0, S).
capitalise_key(S0, S) :-
    sub_string(S0, 0, 1, _, "<"),
    !,                                  % Named function keys.  remove angled brackets?
    S = S0.
capitalise_key(S0, S) :-
    sub_string(S0, 0, 1, _, Char),
    char_type(Char, lower),
    !,
    string_upper(Char, Up),
    sub_string(S0, 1, _, 0, Rest),
    atomics_to_string([Up, '\u2009', Rest], S).
capitalise_key(S, S).

:- pce_end_class(key_binding).




/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Runtime switching is connected to @pce as the operation influences an
unknown number of unknown key_binding objects.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_extend_class(pce).

:- pce_group(preferences).

key_binding_style(_Pce, Style:name) :->
    "Set the key-binding style"::
    set_keybinding_style(Style).

key_binding_style(_Pce, Style:name) :<-
    "Get the key-binding style"::
    current_style(Style).

dialog_key_binding_style(_Pce, Style:name) :->
    "Set the key-binding style for text entry in dialogs"::
    set_keybinding_style(dialog, Style).

dialog_key_binding_style(_Pce, Style:name) :<-
    "Get the key-binding style for text entry in dialogs"::
    current_style(dialog, Style).

key_binding_styles(_Pce, Styles:chain) :<-
    "List of supported styles"::
    findall(Style, binding(Style, _Class, _Mod), StyleList),
    sort([emacs|StyleList], Sorted),
    new(Styles, chain),
    add_styles(Sorted, Styles).

add_styles([], _).
add_styles([H|T], Chain) :-
    send(Chain, append, H),
    add_styles(T, Chain).

:- pce_end_class(pce).


%       Create the type key_binding_style, a dynamic `name-of' type
%       holding the defined key-binding styles.

make_key_binding_style_type :-
    get(@pce, convert, key_binding_style, type, Type),
    send(Type, name_reference, key_binding_style_type),
    send(Type, kind, name_of),
    get(@pce, key_binding_styles, Styles),
    send(Type, slot, context, Styles).

:- initialization make_key_binding_style_type.
