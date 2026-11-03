/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org/packages/xpce/
    Copyright (c)  1985-2002, University of Amsterdam
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

:- module(emacs_extend,
          [ declare_emacs_mode/2,
            declare_emacs_mode/3,
            lsp_mode_module/2,          % +Mode, -Module
            emacs_mode_style/3,         % +Mode, ?Class, -Attributes
            emacs_mode_pce_style/4,     % +Mode, ?Class, -Name, -Style
            emacs_style_name/2          % ?Class, ?Name
          ]).
:- use_module(library(pce)).
:- require([ atomic_list_concat/2,
             send_list/3
           ]).
:- autoload(library(apply), [exclude/3, maplist/3]).
:- autoload(library(lists), [member/2]).

                /********************************
                *         DECLARE MODES         *
                ********************************/

:- dynamic                              % ensure it is restored over a state
    emacs_mode_name/1.

fix_mode_name_type :-
    get(@pce, convert, mode_name, type, Type),
    send(Type, name_reference, mode_name_type),
    send(Type, kind, name_of),
    send(Type, slot, context, new(Ctx, chain)),
    send_list(Ctx, append,
              [ fundamental
              , prolog
              , shell
              ]),
    forall(emacs_mode_name(Mode), send(Ctx, append, Mode)),
    send(Ctx, sort, unique := @on).

:- initialization fix_mode_name_type.

%!  declare_emacs_mode(+ModeName, +FileSpec).
%
%   Specifies that PceEmacs mode `ModeName' may be defined by
%   (auto)loading `FileSpec'.

declare_emacs_mode(Mode, File) :-
    get(string('emacs_%s_mode', Mode), value, EmacsModeClass),
    (   File == []
    ->  true
    ;   pce_autoload(EmacsModeClass, File)
    ),
    (   \+ special_mode(Mode)
    ->  get(@mode_name_type, context, Ctx),
        send(Ctx, add, Mode),
        send(Ctx, sort, unique := @on),
        assert(emacs_mode_name(Mode))
    ;   true
    ).

special_mode(shell).
special_mode(gdb).
special_mode(annotate).


%!  declare_emacs_mode(+ModeName, +FileSpec, +ListOfPatterns)
%
%   Sames as declare_emacs_mode/2.  `ListOfPatterns' is a list of
%   regular expressions that will automatically start this mode.

declare_emacs_mode(Mode, File, Extensions) :-
    declare_emacs_mode(Mode, File),
    declare_file_patterns(Extensions, Mode, @emacs_mode_list).

declare_file_patterns([], _, _).
declare_file_patterns([Ext|Rest], Mode, Sheet) :-
    send(Sheet, value, regex(Ext), Mode),
    declare_file_patterns(Rest, Mode, Sheet).

%       :- emacs_begin_mode(+Mode, +Super, +Summary, +Bindings, +Syntax).
%
%       Binding:
%
%               Selector = [key(Key)]
%                          [+ button(Button)]
%                          [+ button(Button, Function)]         (pullright)
%                          [+ command]
%
%       `command` declares Selector a command that is not bound to a
%       key or menu.  Commands are offered by M-x and for binding a key
%       in the key binding browser.
%
%       Syntax:
%
%               Char [=+] Category(Args)

emacs_expansion((:- emacs_begin_mode(Mode, Super, Summary, Bindings, Syntax)),
                [(:- pce_begin_class(PceMode, PceSuper, Summary)),
                 (:- pce_class_directive(emacs_extend:emacs_mode_bindings(Mode,
                                                             Module,
                                                             Bindings,
                                                             Syntax))),
                 (:- multifile((ModeModule:style/2,
                                ModeModule:def_style/2)))
                ]) :-
    emacs_mode_class(Mode, PceMode),
    emacs_mode_class(Super, PceSuper),
    prolog_load_context(module, Module),
    lsp_mode_module(Mode, ModeModule).
emacs_expansion((:- emacs_extend_mode(Mode, Bindings)),
                [(:- pce_extend_class(PceMode)),
                 (:- pce_class_directive(emacs_extend:emacs_mode_bindings(Mode,
                                                             Module,
                                                             Bindings,
                                                             [])))
                ]) :-
    emacs_mode_class(Mode, PceMode),
    prolog_load_context(module, Module).
emacs_expansion((:- emacs_end_mode), (:- pce_end_class)).

%!  lsp_mode_module(+Mode, -Module) is det.
%
%   True when Module is the Prolog module for dynamic extensions to
%   Mode, such as its style/2 and def_style/2 rules.

lsp_mode_module(Mode, Module) :-
    atomic_list_concat([emacs_, Mode, '_mode'], Module).


                 /*******************************
                 *            STYLES            *
                 *******************************/

%!  emacs_mode_style(+Mode, +Class, -Attributes) is semidet.
%!  emacs_mode_style(+Mode, -Class, -Attributes) is nondet.
%
%   True when fragments classified as Class are shown using Attributes,
%   a list of Name(Value) terms for class style, in Mode.  The styles
%   are the style/2 rules (for the user) and def_style/2 rules (for the
%   mode) in the module of the mode (see lsp_mode_module/2).  A mode
%   inherits the styles of its super modes, e.g., C++ those of C.  The
%   first rule found wins, where a mode precedes its super mode and
%   style/2 precedes def_style/2.

emacs_mode_style(Mode, Class, Attributes) :-
    nonvar(Class),
    !,
    mode_modules(Mode, Modules),
    member(Module, Modules),
    module_style(Module, Class, Attributes),
    !.
emacs_mode_style(Mode, Class, Attributes) :-
    mode_modules(Mode, Modules),
    findall(C-A,
            ( member(Module, Modules),
              module_style(Module, C, A)
            ),
            Pairs),
    first_styles(Pairs, Unique),
    member(Class-Attributes, Unique).

module_style(Module, Class, Attributes) :-
    (   catch(Module:style(Class, Attributes),
              error(existence_error(procedure, _), _), fail)
    ;   catch(Module:def_style(Class, Attributes),
              error(existence_error(procedure, _), _), fail)
    ).

first_styles([], []).
first_styles([C-A|T0], [C-A|T]) :-
    exclude(same_class(C), T0, T1),
    first_styles(T1, T).

same_class(C, C1-_) :-
    C1 =@= C.

%!  mode_modules(+Mode, -Modules) is det.
%
%   Modules are the modules for Mode and its super modes.

mode_modules(Mode, Modules) :-
    emacs_mode_class(Mode, ClassName),
    (   get(@classes, member, ClassName, Class)
    ->  class_modules(Class, Modules)
    ;   lsp_mode_module(Mode, Module),
        Modules = [Module]
    ).

class_modules(Class, [Module|Modules]) :-
    get(Class, name, ClassName),
    emacs_mode_class(Mode, ClassName),
    Mode \== @default,
    !,
    lsp_mode_module(Mode, Module),
    get(Class, super_class, Super),
    (   Super \== @nil
    ->  class_modules(Super, Modules)
    ;   Modules = []
    ).
class_modules(_, []).

%!  emacs_mode_pce_style(+Mode, ?Class, -Name, -Style) is nondet.
%
%   As emacs_mode_style/3, translated for editor->style: Name is the
%   style name for Class (see emacs_style_name/2) and Style a term
%   style(Name := Value, ...) or `@default`.

emacs_mode_pce_style(Mode, Class, Name, Style) :-
    emacs_mode_style(Mode, Class, Attributes),
    emacs_style_name(Class, Name),
    maplist(style_attribute, Attributes, PceArgs),
    (   PceArgs == []
    ->  Style = @default
    ;   Style =.. [style|PceArgs]
    ).

style_attribute(Attr, Name := Value) :-
    Attr =.. [Name,Value].

%!  emacs_style_name(?Class, ?StyleName:atom) is det.
%
%   Map between the term that classifies a fragment and the (atom) name
%   of its style in the editor.

:- table emacs_style_name/2.
emacs_style_name(Class, StyleName) :-
    nonvar(Class),
    !,
    copy_term(Class, Copy),
    numbervars(Copy, 0, _, [singletons(true)]),
    term_string(Copy, S, [numbervars(true)]),
    atom_string(StyleName, S).
emacs_style_name(Class, StyleName) :-
    term_string(Class, StyleName).

%!  emacs_mode_bindings(+Mode, +Module, +Bindings, +Syntax)

:- public emacs_mode_bindings/4. % called from code expanded by emacs_expansion/2.

emacs_mode_bindings(Mode, Module, Bindings, Syntax) :-
    emacs_mode_class(Mode, PceClass),
    get(@pce, convert, PceClass, class, ClassObject),
    get(ClassObject, super_class, SuperClass),
    get(SuperClass, name, SuperName),
    emacs_mode_class(SuperMode, SuperName),
    new(KB, emacs_key_binding(Mode, SuperMode)),
    new(MM, emacs_mode_menu(Mode, SuperMode)),
    (   get(@syntax_tables, member, Mode, ST)
    ->  true
    ;   new(ST, syntax_table(Mode, SuperMode))
    ),
    retractall(emacs_command(PceClass, Module, _)),
    make_bindings(Bindings, Mode, Module, KB, MM),
    send(KB, apply_preferences),
    make_syntax(Syntax, ST).

%!  emacs_command(?ModeClass, ?Module, ?Name)
%
%   Name is declared a command of ModeClass by Module using `command`.

:- dynamic
    emacs_command/3.

:- multifile key_binding:command_function/2.

key_binding:command_function(Target, Name) :-
    emacs_command(PceClass, _, Name),
    (   send(Target, instance_of, PceClass)
    ->  true
    ;   send(Target, instance_of, class),
        send(Target, is_a, PceClass)
    ),
    !.

make_bindings([], _, _, _, _).
make_bindings([Selector = Term|Rest], Mode, Module, KB, MM) :-
    bind(Term, Selector, Mode, Module, KB, MM),
    make_bindings(Rest, Mode, Module, KB, MM).

make_syntax([], _).
make_syntax([S|Rest], ST) :-
    syntax(S, ST),
    make_syntax(Rest, ST).

bind(key(Key), Selector, _, _, KB, _) :-
    send(KB, function, Key, Selector).
bind(-button(Button), Selector, _, _, _, MM) :-
    send(MM, delete, Button, Selector).
bind(button(Button), Selector, _, _, _, MM) :-
    send(MM, append, Button, Selector).
bind(button(Button, Func), Selector, _, Module, _, MM) :-
    send(MM, Module:append(Button, emacs_argument_item(Selector, Func))).
bind(command, Selector, Mode, Module, _, _) :-
    emacs_mode_class(Mode, PceClass),
    assertz(emacs_command(PceClass, Module, Selector)).
bind(A+B, Selector, Mode, Module, KB, MM) :-
    bind(A, Selector, Mode, Module, KB, MM),
    bind(B, Selector, Mode, Module, KB, MM).

syntax(Char = Term, ST) :-
    Term =.. TermArgs,
    Msg =.. [syntax, Char | TermArgs],
    send(ST, Msg).
syntax(Char + Term, ST) :-
    Term =.. TermArgs,
    Msg =.. [add_syntax, Char | TermArgs],
    send(ST, Msg).
syntax(paragraph_end(End), ST) :-
    (   is_list(End)
    ->  atomic_list_concat(End, '|', Regex)
    ;   Regex = End
    ),
    send(ST, paragraph_end, Regex).
syntax(sentence_end(End), ST) :-
    (   is_list(End)
    ->  atomic_list_concat(End, '|', Regex)
    ;   Regex = End
    ),
    send(ST, sentence_end, Regex).
syntax(quasi_quotation(Start, End), ST) :-
    send(ST, quasi_quotation_start, Start),
    send(ST, quasi_quotation_end, End).
syntax(language(Lang), ST) :-
    send(ST, language, Lang).
syntax(line_comment(Start), ST) :-
    send(ST, line_comment, Start).


                 /*******************************
                 *             UTIL             *
                 *******************************/

%!  emacs_mode_class(?ModeName, ?ClassName)
%
%   Convert between plain PceEmacs modename and the mode class.

emacs_mode_class(@default, emacs_mode) :- !.
emacs_mode_class([], emacs_mode) :- !.
emacs_mode_class(ModeName, ClassName) :-
    atom(ModeName),
    !,
    atomic_list_concat([emacs_, ModeName, '_mode'], ClassName).
emacs_mode_class(ModeName, ClassName) :-
    atom_concat(emacs_, M0, ClassName),
    atom_concat(ModeName, '_mode', M0),
    !.
emacs_mode_class(@default, emacs_mode).


                 /*******************************
                 *         REGISTRATION         *
                 *******************************/

:- multifile
    user:pce_pre_expansion_hook/2.
:- dynamic
    user:pce_pre_expansion_hook/2.

user:pce_pre_expansion_hook(In, Out) :-
    emacs_expansion(In, Out).
