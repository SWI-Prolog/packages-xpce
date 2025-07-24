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
%   @arg ModeName         Name of the key-binding-style
%   @arg TableName        Syntax table to modify
%   @arg Modifications    List of Key-Method

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
	'\\s-cursor_left'  = beginning_of_line,
	'\\s-cursor_right' = end_of_line,
	'\\s-cursor_up'    = point_to_top_of_file,
	'\\s-cursor_down'  = point_to_bottom_of_file,
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

          '\\s-0'     = font_magnify,
          '\\s--'     = font_reduce,
          '\\s-='     = font_default,

          '\\s-n'     = new,
          '\\s-o'     = open
        ]).
binding(apple, terminal,
        [ '\\C-c'     = interrupt,
          '\\s-c'     = copy,
          '\\s-v'     = paste,
          '\\s-a'     = select_all
        ]).
binding(apple, epilog,
        [ '\\S-\\s-d' = split_horizontally,
          '\\s-d'     = split_vertically,
          '\\s-n'     = new_window,
          '\\s-k'     = clear_screen,
          '\\s-w'     = close
        ]).


                 /*******************************
                 *       CHANGE BINDINGS        *
                 *******************************/

%!  set_keybinding_style(+Id)
%
%   Runtime modification of the current key-binding style.

set_keybinding_style(Mode) :-
    current_style(Mode),
    !.
set_keybinding_style(emacs) :-
    !,
    send(@key_bindings, for_all,
         message(@arg2, unmodify)),
    set_style(emacs).
set_keybinding_style(Style) :-
    set_keybinding_style(emacs),
    (   binding(Style, Table, Modifications),
        get(@key_bindings, member, Table, KB),
        modify(Modifications, KB),
        fail
    ;   true
    ),
    set_style(Style).


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
               [ 'X'(emacs),
                 windows(cua),
                 apple(apple)
               ],
               "Basic binding style (emacs,cua,apple)").

%!  current_style(-Style) is det.
%!  set_style(+Style) is det.
%
%   Manipulate the style.  The style is stored in the class-variable
%   key_binding.style, so it can be set in the users preferences
%   file.

current_style(Style) :-
    get(@pce, convert, key_binding, class, Class),
    get(Class, class_variable, style, Var),
    get(Var, value, Style).

set_style(Style) :-
    get(@pce, convert, key_binding, class, Class),
    get(Class, class_variable, style, Var),
    send(Var, value, Style).


apply_preferences(KB) :->
    "Apply CUA-mode preferences"::
    send(KB, apply_cua),
    send(KB, bind_resources).       % bind from ~/.xpce/Defaults

apply_cua(KB) :->
    "Apply our local overrides"::
    current_style(Mode),
    (   Mode == emacs
    ->  true
    ;   get(KB, name, Name),
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
