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

:- module(lsp_symbol_item,
          []).
:- use_module(library(pce)).
:- use_module(library(apply)).
:- use_module(library(pce_util)).

%   The type lsp_tag is defined by library(emacs/lsp_registry).

:- multifile
    emacs_prompt:make_item_hook/6.

emacs_prompt:make_item_hook(Mode, Label, Default, Type, History, Item) :-
    get(Type, name, lsp_tag),
    get(Mode, lsp_client, symbol, LSP),
    new(Item, lsp_symbol_item(Label, Default, @nil, LSP)),
    (   History \== @default
    ->  send(Item, value_set, History)
    ;   true
    ).

:- pce_begin_class(lsp_symbol_item, text_item,
                   "Find a symbol on the LSP server").

variable(lsp_client, lsp_client, get, "Connected LSP client").

initialise(SI, Name:label=[name], Def:default=[char_array],
           Msg:message=[code]*, LSP:lsp_client=[lsp_client]) :->
    send_super(SI, initialise, Name, Def, Msg),
    send(SI, slot, lsp_client, LSP),
    send(SI, style, combo_box).

completions(SI, From:name, Matches:chain) :<-
    "Ask the LSP server for matches"::
    get(SI, lsp_client, LSP),
    get(LSP, call,
        'workspace/symbol'(
            #{ query: From
             }),
        Result),
    convlist(symbol_name(From), Result, Symbols),
    chain_list(Matches, Symbols).

symbol_name(Prefix, Symbol, Name) :-
    Name = Symbol.name,
    sub_atom(Name, 0, _, _, Prefix).

:- pce_end_class.
