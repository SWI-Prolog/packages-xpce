/*  Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2025-2026, SWI-Prolog Solutions b.v.
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

:- module(lsp_highlight, []).
:- use_module(library(pce)).
:- use_module(library(emacs/emacs), []).    % classes we extend
:- use_module(library(apply)).
:- use_module(library(debug)).

:- use_module(library(emacs/lsp_client)).

/** <module> Generic LSP based highlighting

Highlight a buffer using the `textDocument/semanticTokens/full` request.
A token of type Type gets the style for `lsp(Type)` of the mode, i.e.,
a style/2 or def_style/2 rule in the module of the mode (see
lsp_mode_module/2).  Tokens without a style are not highlighted.
*/

:- pce_extend_class(emacs_buffer).

auto_colourise(TB) :->
    "Run mode ->auto_colourise_buffer on associated mode"::
    (   get(TB?editors, head, Editor)
    ->  send(Editor?mode, auto_colourise_buffer)
    ;   true
    ).

colourise(TB) :->
    "Run mode ->colourise_buffer on associated mode"::
    (   get(TB?editors, head, Editor)
    ->  send(Editor?mode, colourise_buffer)
    ;   true
    ).

%   ->lsp_highlight(+LSP)
%
%   Do LSP based highlighting. Asks  for   the  tokens and applies them.

lsp_highlight(TB, LSP:lsp_client) :->
    "Implement LSP based semantic highlighting"::
    get(LSP, initialized, @on),
    get(TB, attribute, lsp_tracking, URI),
    get(TB, attribute, lsp_version, _Version),
    send(TB, report, progress, 'LSP highlighting'),
    get_time(LSPTime0),
    get(LSP, call,
        'textDocument/semanticTokens/full'(
            #{ textDocument:
                 #{ uri: URI
                  }
             }),
        Result),
    get_time(LSPTime1),
    length(Result.data, Len),
    Tokens is Len//5,
    LSPTime is LSPTime1-LSPTime0,
    send(TB, report, progress,
         'Received %d semantic tokens in %.3f seconds', Tokens, LSPTime),
    send(TB, for_all_fragments,
         if(message(@arg1, instance_of, emacs_colour_fragment),
            message(@arg1, free))),
    get_time(FragmentTime0),
    get(TB, mode, Mode),
    token_styles(LSP, Mode, Styles),
    highlight_tokens(Result.data, Styles, LSP, TB, 0, 0, 0, Count),
    get_time(FragmentTime1),
    FragmentTime is FragmentTime1 - FragmentTime0,
    send(TB, report, progress,
         'Created %d fragments in %.3f seconds', Count, FragmentTime),
    send(TB, report, done).

%!  highlight_tokens(+DeltaTokens, +Styles, +LSP, +Buffer,
%!                   +Line0, +Char0, +Count0, -Count) is det.
%
%   Create a fragment for each token that has a style.  Tokens are
%   relative to the previous one: a line delta and a character delta if
%   on the same line, else the character.  Characters are UTF-16 units,
%   which <-lsp_offset translates.  Styles maps the token type number
%   to a style name or `-` (see token_styles/4).

highlight_tokens([], _, _, _, _, _, C, C).
highlight_tokens([DL,DC,Len,Tid,Mid|More], Styles, LSP, TB, L0, C0, N0, N) :-
    (   DL == 0
    ->  Line = L0,
        Char is C0+DC
    ;   Line is L0+DL,
        Char = DC
    ),
    I is Tid+1,
    (   arg(I, Styles, StyleName),
        StyleName \== (-)
    ->  get(TB, lsp_offset, Line, Char, Start),
        EndChar is Char+Len,
        get(TB, lsp_offset, Line, EndChar, End),
        Length is End-Start,
        debug(lsp(token), '~p:~p[~p]@~p: ~p', [Line,Char,Len,Start,StyleName]),
        new(F, emacs_lsp_fragment(TB, Start, Length, StyleName)),
        send(F, slot, lsp_client, LSP),
        send(F, slot, modifiers, Mid),
        N1 is N0+1
    ;   N1 = N0
    ),
    highlight_tokens(More, Styles, LSP, TB, Line, Char, N1, N).

%!  token_styles(+LSP, +Mode, -Styles) is det.
%
%   Styles is a term with an argument for each token type of LSP that
%   holds the style name for the type in Mode or `-` if Mode has no
%   style for it.

token_styles(LSP, Mode, Styles) :-
    findall(Tid, lsp_token_type(LSP, Tid, _), Tids),
    max_list([-1|Tids], Max),
    Arity is Max+1,
    functor(Styles, styles, Arity),
    forall(between(1, Arity, I),
           ( Tid is I-1,
             (   lsp_token_type(LSP, Tid, TokenType),
                 StyleClass = lsp(TokenType),
                 mode_style(Mode, StyleClass, _)
             ->  style_name(StyleClass, Name)
             ;   Name = (-)
             ),
             nb_setarg(I, Styles, Name)
           )).

:- pce_end_class.


                /*******************************
                *       MODE EXTENSIONS        *
                *******************************/

:- emacs_extend_mode(language, []).

:- pce_group(highlight).


%   ->lsp_setup_highlight()
%
%   Prepare the editor for LSP based highligting.  This serves two
%   roles:
%
%     - Possibly connect to LSP servers
%     - Initialise the style mapping for highlighting tokens.

lsp_setup_highlight(M) :->
    "Prepare using an LSP on this mode"::
    (   styled_role(Role),
        get(M, lsp_client, Role, _)
    ->  send(M, lsp_setup_styles)
    ;   send(M, lsp_setup),
        styled_role(Role),
        get(M, lsp_client, Role, _),
        send(M, lsp_setup_styles),
        % needs to be called in next event cycle
        new(T, timer(0.1,
                     and(message(M, colourise_buffer),
                         message(@receiver, free)))),
        send(T, start, once),
        send(T, lock_object, @on)
    ).

styled_role(highlight).
styled_role(spelling).

lsp_setup_styles(M) :->
    "Initialize the editor style sheet"::
    get(M, editor, E),
    (   get(E, attribute, lsp_styles_assigned, @on)
    ->  true
    ;   get(M, name, ModeName),
        forall(style(ModeName, _Class, Name, Style),
               send(E, style, Name, Style)),
        send(E, attribute, lsp_styles_assigned, @on)
    ),
    send(M, lsp_enable_margin).

colourise_buffer(M) :->
    "Use LSP based highlighting"::
    get(M, text_buffer, TB),
    (   get(M, lsp_client, highlight, LSP),
        (   get(LSP, initialized, @on)
        ->  send(TB, lsp_highlight, LSP)
        ;   send(LSP, register_pending,
                 message(M, colourise_buffer))
        )
    ->  send(M, update_bookmarks)
    ;   send_super(M, colourise_buffer)
    ),
    (   send(M, has_send_method, colourise_buffer_no_lsp)
    ->  send(M, colourise_buffer_no_lsp)
    ;   true
    ).

:- emacs_end_mode.


%!  style(+Mode, ?Class, -Name, -Style) is nondet.
%
%   Define the styles.

style(Mode, Class, Name, Style) :-
    mode_style(Mode, Class, Attributes),
    style_name(Class, Name),
    maplist(style_attribute, Attributes, PceArgs),
    (   PceArgs == []
    ->  Style = @default
    ;   Style =.. [style|PceArgs]
    ).

style_attribute(Attr, Name := Value) :-
    Attr =.. [Name,Value].

%!  mode_style(+Mode, +StyleClass, -Attributes) is semidet.
%!  mode_style(+Mode, -StyleClass, -Attributes) is nondet.

mode_style(Mode, StyleClass, Attributes) :-
    nonvar(StyleClass),
    !,
    lsp_mode_module(Mode, Module),
    (   Module:style(StyleClass, Attributes)
    ->  true
    ;   Module:def_style(StyleClass, Attributes)
    ).
mode_style(Mode, StyleClass, Attributes) :-
    lsp_mode_module(Mode, Module),
    (   Module:style(StyleClass, Attributes)
    ;   Module:def_style(StyleClass, Attributes),
        \+ ( Module:style(UserClass, _),
             UserClass =@= StyleClass )
    ).

%!  style_name(?Term, ?StyleName:atom) is det.
%
%   Map between the term representation for styles and the (atom) name
%   we need to use for the xpce style name.

:- table style_name/2.
style_name(Class, StyleName) :-
    nonvar(Class),
    !,
    copy_term(Class, Copy),
    numbervars(Copy, 0, _, [singletons(true)]),
    term_string(Copy, S, [numbervars(true)]),
    atom_string(StyleName, S).
style_name(Class, StyleName) :-
    term_string(Class, StyleName).


                /*******************************
                *           FRAGMENT           *
                *******************************/

:- pce_begin_class(emacs_lsp_fragment, emacs_colour_fragment,
                   "Represent an LSP highlight fragment").

variable(lsp_client,	lsp_client*, get, "Source LSP client").
variable(modifiers,	int := 0,    get, "LSP token type modifiers").

identify(F) :->
    "Identify LSP fragments"::
    get(F, style, StyleName),
    (   get(F, lsp_client, LSP),
        LSP \== @nil,
        get(F, modifiers, Mask),
        Mask \== 0
    ->  get(LSP, modifiers, Mask, Modifiers)
    ;   Modifiers = []
    ),
    style_name(Style, StyleName),
    get(F, text_buffer, TB),
    (   get(TB?editors, head, Editor),
        get(Editor, mode, Mode),
        get(Mode?class, get_method, lsp_token_label, _),
        get(Mode, lsp_token_label, prolog(Style), Label)
    ->  true
    ;   token_label(Style, Label)
    ),
    (   Modifiers == []
    ->  send(TB, report, status, '%s', Label)
    ;   format(string(Mods), '~w', [Modifiers]),
        send(TB, report, status, '%s %s', Label, Mods)
    ).

token_label(lsp(Type), Label) =>
    format(string(Label), '~w', [Type]).
token_label(Style, Label) =>
    format(string(Label), '~p', [Style]).

:- pce_end_class.
