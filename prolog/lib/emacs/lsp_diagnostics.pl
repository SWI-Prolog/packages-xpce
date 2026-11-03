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

:- module(lsp_diagnostics,
          [ lsp_diagnostic_style/2,  % ?StyleName, ?StyleProperties
            lsp_severity_type/3,     % ?LSPLevel, ?LSPName, ?FragmentStyle
            same_diagnostics/2       % +Fragment, -Fragments
          ]).
:- use_module(library(pce)).
:- use_module(library(emacs/emacs), []).    % classes we extend
:- use_module(library(doc/objects)). % @br, etc.
:- use_module(library(hyper)).
:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(pce_util)).
:- use_module(library(uri)).

:- use_module(library(emacs/lsp_client)).

/** <module> Handle LSP disagnostic messages

This  module  provides  the  infrastructucture  to  deal  with  the  LSP
initiated ``textDocument/publishDiagnostics()`` method. It extends class
`emacs_buffer` to create `emacs_lsp_diagnostic`  fragments, hovering and
selecting fragments and initiating "fix available" edits.

Two multifile hooks allow servers that do not do this themselves to
deal with, e.g., spelling:

  - suppress_diagnostic(+Buffer, +LSP, +Start, +Length, +Diagnostic)
    If true, no fragment is created for Diagnostic.
  - diagnostic_buttons(+Window, +Fragment)
    Called after adding the fix buttons to the diagnostic Window.  Use
    `Window->action_button` to add buttons.
*/

:- multifile
    suppress_diagnostic/5,              % +Buffer, +LSP, +Start, +Len, +Diag
    diagnostic_buttons/2.               % +Window, +Fragment

%!  lsp_severity_type(?LSPLevel, ?LSPName, ?FragmentStyle).

lsp_severity_type(1, error,   lsp_diag_error).
lsp_severity_type(2, warning, lsp_diag_warning).
lsp_severity_type(3, info,    lsp_diag_info).
lsp_severity_type(4, hint,    lsp_diag_hint).

:- det(style_pce_severity/3).
style_pce_severity(lsp_diag_error,   error,   'Error').
style_pce_severity(lsp_diag_warning, warning, 'Warning').
style_pce_severity(lsp_diag_info,    status,  'Information').
style_pce_severity(lsp_diag_hint,    status,  'Hint').

%!  lsp_diagnostic_style(?StyleName, ?StyleProperties) is nondet.
%
%   Provide style name and properties for the LSP diagnostic fragments.

lsp_diagnostic_style(lsp_diag_error,   [icon(Icon), underline(red)]) :-
    lsp_icon(error, Icon).
lsp_diagnostic_style(lsp_diag_warning, [icon(Icon), underline(orange)]) :-
    lsp_icon(warning, Icon).
lsp_diagnostic_style(lsp_diag_info,    [icon(Icon), underline(yellow)]) :-
    lsp_icon(info, Icon).
lsp_diagnostic_style(lsp_diag_hint,    [icon(Icon), underline(navyblue)]) :-
    lsp_icon(hint, Icon).

lsp_icon(error,   'ide/lsp/error.svg').
lsp_icon(warning, 'ide/lsp/warning.svg').
lsp_icon(info,    'ide/lsp/information.svg').
lsp_icon(hint,    'ide/lsp/hint.svg').


                /*******************************
                *     EXTEND LANGUAGE MODE     *
                *******************************/

:- emacs_extend_mode(language, []).

class_variable(lsp_margin_width, int, 22,
               "Width for diagnostic icon margin").

:- pce_group(diagnostic).

%   ->lsp_enable_margin(+Enable)
%
%   Show or hide the margin with the diagnostic icons.  If Enable is
%   @default, show it if there are diagnostics.  Showing the margin
%   also defines the styles of the diagnostic fragments.

lsp_enable_margin(M, Enable:[bool]) :->
    "Enable the diagnostic margin"::
    (   Enable == @off
    ->  send(M, margin_width, 0)
    ;   (   Enable == @on
        ;   get(M, find_fragment,
                message(@arg1, instance_of, emacs_lsp_diagnostic), _)
        )
    ->  send(M, lsp_setup_diagnostic_styles),
        get(M, class_variable_value, lsp_margin_width, Width),
        send(M, margin_width, Width)
    ;   true
    ).

%   ->lsp_setup_diagnostic_styles
%
%   Define the styles for the diagnostic fragments in the editor.  The
%   style/2 rules of the mode override lsp_diagnostic_style/2.

lsp_setup_diagnostic_styles(M) :->
    "Define the styles for LSP diagnostics"::
    get(M, editor, E),
    (   get(E, attribute, lsp_diagnostic_styles, @on)
    ->  true
    ;   get(M, name, Mode),
        forall(lsp_severity_type(_, _, Name),
               ( diagnostic_style(Mode, Name, Attributes),
                 maplist(style_attribute, Attributes, PceArgs),
                 Style =.. [style|PceArgs],
                 send(E, style, Name, Style)
               )),
        send(E, attribute, lsp_diagnostic_styles, @on)
    ).

diagnostic_style(Mode, Name, Attributes) :-
    lsp_mode_module(Mode, Module),
    catch(Module:style(Name, Attributes), error(existence_error(_,_),_), fail),
    !.
diagnostic_style(_Mode, Name, Attributes) :-
    lsp_diagnostic_style(Name, Attributes).

style_attribute(Attr, Name := Value) :-
    Attr =.. [Name,Value].

selected_fragment(M, Fragment:fragment) :->
    "User selected a fragment in the margin"::
    send(M, show_fragment_note, Fragment).

hover_fragment_icon(M, Fragment:fragment*, _Area:[area]) :->
    "User selected a fragment in the margin"::
    (   Fragment == @nil
    ->  ignore(send(M, send_hyper, note, hover_end))
    ;   send(M, send_hyper, note, explicitly_opened)
    ->  true
    ;   send(M, show_fragment_note, Fragment, @on)
    ).

show_fragment_note(M, Fragment:fragment, Hover:[bool]) :->
    "Show message associated with a fragment below the fragment"::
    (   send(Fragment, instance_of, emacs_lsp_diagnostic)
    ->  get(M, editor, E),
        ignore(send(M, send_hyper, note, destroy)),
        new(W, emacs_lsp_diagnostic_window(E, Fragment, Hover)),
        new(_, partof_hyper(M, W, note, mode)),
        send(W, open)
    ;   true
    ).

:- emacs_end_mode.


                /*******************************
                *     EXTEND EMACS BUFFER      *
                *******************************/

:- pce_extend_class(emacs_buffer).

lsp_publish_diagnostics(Buffer, LSP:lsp=lsp_client,
                        Diagnostics:diagnostics=prolog,
                        Region:region=fragment*) :->
    "Create fragments from diagnostics"::
    send(Buffer, lsp_clear_diagnostics, LSP, Region),
    (   Region == @nil
    ->  LineOffset = 0
    ;   get(Region, start, Start),
        get(Buffer, line_number, Start, Line1),
        LineOffset is Line1-1
    ),
    maplist(show_diagnostic(Buffer, LSP, LineOffset), Diagnostics),
    send(Buffer, report_diagnostic_counts).

show_diagnostic(Buffer, LSP, LineOffset, Diagnostic) :-
    #{range: Range, severity: Severity} :< Diagnostic,
    #{start: Start, end: End} :< Range,
    lsp_offset(Start, Buffer, LineOffset, StartOffset),
    lsp_offset(End, Buffer, LineOffset, EndOffset),
    Length is EndOffset-StartOffset,
    (   suppress_diagnostic(Buffer, LSP, StartOffset, Length, Diagnostic)
    ->  true
    ;   lsp_severity_type(Severity, _Name, Style),
        new(D, emacs_lsp_diagnostic(Buffer, StartOffset, Length,
                                    Diagnostic, Style)),
        send(D, slot, lsp_client, LSP)
    ).

lsp_offset(#{line:Line, character:Char}, Buffer, LineOffset, Offset) =>
    TheLine is Line+LineOffset,
    get(Buffer, lsp_offset, TheLine, Char, Offset).

%   ->report_diagnostic_counts() is det.
%
%   Report diagnostic message count if there are diagnostics.

report_diagnostic_counts(Buffer) :->
    "Report on the number of diagnostics"::
    new(Counts, sheet(attribute(lsp_diag_error,   number(0)),
                      attribute(lsp_diag_warning, number(0)),
                      attribute(lsp_diag_info,    number(0)),
                      attribute(lsp_diag_hint,    number(0)))),
    send(Buffer, for_all_fragments,
         if(message(@arg1, instance_of, emacs_lsp_diagnostic),
            message(Counts?(@arg1?style), plus, 1))),
    get(Counts?lsp_diag_error,   value, E),
    get(Counts?lsp_diag_warning, value, W),
    get(Counts?lsp_diag_info,    value, I),
    get(Counts?lsp_diag_hint,    value, H),
    (   counts(E,W,I,H) == counts(0,0,0,0)
    ->  true
    ;   in_pce_thread((
            send(Buffer?editors, for_all,
                 message(@arg1, lsp_enable_margin, @on)),
            send(Buffer, report, status,
                 'E:%d, W:%d, I:%d, H:%d', E,W,I,H)))
    ).

lsp_clear_diagnostics(Buffer,
                      LSP:lsp_client, Region:region=fragment*) :->
    "Remove emacs_lsp_diagnostic fragments from client [in region]"::
    (   Region == @nil
    ->  send(Buffer, for_all_fragments,
             if(and(message(@arg1, instance_of, emacs_lsp_diagnostic),
                    @arg1?lsp_client == LSP),
                message(@arg1, free)))
    ;   send(Buffer, for_all_fragments,
             if(and(message(@arg1, instance_of, emacs_lsp_diagnostic),
                    @arg1?lsp_client == LSP,
                    message(@arg1, overlap, Region)),
                message(@arg1, free)))
    ).

:- pce_end_class.


                /*******************************
                *           FRAGMENT           *
                *******************************/

:- pce_begin_class(emacs_lsp_diagnostic, fragment,
                   "Represent an LSP diagnostic message").

variable(lsp_client, lsp_client*, get, "Source LSP client").
variable(json,       prolog,      get, "JSON diagnostic message").

initialise(F, Buffer:text_buffer, Start:int, Len:int,
           JSON:prolog, Style:name) :->
    "Create an LSP diagnostic fragment"::
    send_super(F, initialise, Buffer, Start, Len, Style),
    send(F, slot, json, JSON).

message(F, Msg:string) :<-
    "Diagnostic message"::
    get(F, json, Dict),
    Msg = Dict.get(message, "No message").

identify(F) :->
    "Show LSP message"::
    get(F, style, Style),
    style_pce_severity(Style, Severity, Label),
    send(F?text_buffer, report, Severity, '%s: %s', Label, F?message).

source(F, Source:name) :<-
    "Get the origin of the diagnostic"::
    get(F?lsp_client, id, Source).

code(F, Code:name) :<-
    "Get the diagnostic code"::
    get(F, json, Dict),
    atom_string(Code, Dict.get(code)).

range(F, Range:prolog) :<-
    "Get the original range as Prolog dict"::
    get(F, json, Dict),
    Range = Dict.get(range).

lsp_class(F, LspClass:{error,warning,info,hint}) :<-
    "Get severity class"::
    get(F, style, Style),
    style_pce_severity(Style, LspClass, _Label).

icon(F, Icon:image) :<-
    get(F, style, Style),
    lsp_severity_type(_Level, LspClass, Style),
    lsp_icon(LspClass, Icon).

fixes(F, Fixes:prolog) :<-
    "Get fixes from LSP server"::
    get(F, text_buffer, Buffer),
    get(Buffer, attribute, lsp_tracking, URI),
    get(F, json, Diagnostic),
    debug(lsp(code_action), 'Requesting code actions for ~@',
          [print_term(Diagnostic, [output(current_output)])]),
    get(F, lsp_client, LSP),
    Context0 = #{ diagnostics: [Diagnostic] },
    (   get(LSP, code_action_kinds, Kinds)
    ->  Context = Context0.put(only, Kinds)
    ;   Context = Context0
    ),
    get(LSP, call,
        'textDocument/codeAction'(
            #{ textDocument: #{ uri: URI},
               range: Diagnostic.range,
               context: Context
             }),
        Fixes),
    debug(lsp(code_action), 'Code actions: ~@',
          [print_term(Fixes, [output(current_output)])]).

:- pce_end_class.


                /*******************************
                *            WINDOW            *
                *******************************/

:- pce_begin_class(emacs_lsp_diagnostic_window, dialog,
                   "Show LSP diagnostics in modal window").

variable(lsp_client, lsp_client*, get, "Source LSP client").

class_variable(text_width, int, 400,
               "Width for layout of diagnostic message").

initialise(W, Editor:editor, Fragment:emacs_lsp_diagnostic,
           Hover:[bool]) :->
    get(Fragment, start, Offset),
    get(Editor, text_image, TextImage),
    get(TextImage, character_position, Offset, point(X,Y)),
    get(TextImage, frame_position, point(OX,OY)),
    get(Editor, frame, Master),
    send_super(W, initialise, "LSP Feedback"),
    send(W, slot, lsp_client, Fragment?lsp_client),
    send(W, transient_for, Master),
    send(W, kind, popup),
    get(Fragment, icon, Icon),
    send(W, append, new(I, label(icon, image(Icon)))),
    get(W, class_variable_value, text_width, TW),
    send(W, append, new(G, dialog_group(message,group)), right),
    send(G, append, new(M, parbox(TW, left))),
    send(M, name, message),
    send_list([I,G], reference, point(0,0)),
    (   Hover == @on
    ->  true
    ;   send(W, fixes_buttons, Fragment),
        send(W, append, new(Done, button(done, message(W, destroy)))),
        send(W, keyboard_focus, Done)
    ),
    send(W, message, Fragment?message),
    new(_, partof_hyper(Fragment, W, dialog, fragment)),
    send(W?frame, position, point(OX+X, OY+Y+2)).

destroy(W) :->
    "Allow calling from a thread"::
    (   thread_self(Me),
        pce_thread(Me)
    ->  send_super(W, destroy)
    ;   in_pce_thread(send(W, destroy))
    ).

explicitly_opened(W) :->
    "User opened this using a click"::
    get(W, member, done, _Button).

hover_end(W) :->
    "Mouse left the icon"::
    (   send(W, explicitly_opened)
    ->  true
    ;   send(W, destroy)
    ).

message(W, Msg:string) :->
    "Update the message"::
    get(W, member, message, Group),
    get(Group, member, message, PB),
    object(Msg, string(Message)),
    split_string(Message, '\n', '', Lines),
    append_pars(Lines, PB).

append_pars([], _) =>
    true.
append_pars([Last], PB) =>
    send(PB, cdata, Last).
append_pars([H|T], PB) =>
    send(PB, cdata, H),
    send_list(PB, append, [@nbsp,@br]),
    append_pars(T, PB).

%   ->fixes_buttons(Fragment)
%
%   Add buttons for the proposed fixes.

fixes_buttons(W, Fragment:emacs_lsp_diagnostic) :->
    "Add buttons for available fixes"::
    get(Fragment, fixes, Fixes),
    (   Fixes == [], fail
    ->  true
    ;   get(Fragment, range, Range),
        same_diagnostics(Fragment, All),
        get(Fragment, string, string(Text)),
        length(All, Count),
        get(W, member, message, MsgGroup),
        send(MsgGroup, append,
             new(Group, dialog_group(buttons, group)),
             next_row),
        (   member(Fix, Fixes),
            append_fix_button(W, Group, Range, Text, Count, Fix),
            fail
        ;   true
        ),
        forall(diagnostic_buttons(W, Fragment), true)
    ).

action_button(W, Icon:image, Title:char_array, Msg:code) :->
    "Add button with icon"::
    get(W, member, message, MsgGroup),
    get(MsgGroup, member, buttons, Group),
    send(Group, append,
         new(LBL, label(icon, Icon)),
         next_row),
    send(Group, append,
         new(B, button(Title, Msg)),
         right),
    send(LBL, width, 32),
    send(LBL, reference, point(0, B?reference?y)),
    send(B, alignment, left).

append_fix_button(W, Group, Range, Text, Count, Fix),
    fix_command(Fix, Range, Title, Command, Args, Kind) =>
    debug(lsp(fix), "Fix: ~@",
          [print_term(Fix, [output(current_output)])]),
    fix_icon(Command, Kind, Icon),
    send(W, action_button,
         image(Icon), Title,
         message(W, apply_change, Title)),
    add_replace_all(Count, Command, Args, Text, W, Group).
append_fix_button(_W, _Group, _Range, _Text, _Count, Fix) =>
    debug(lsp(unknown_fix), "Unknown fix: ~@",
          [print_term(Fix, [output(current_output)])]).

%!  add_replace_all(+Count, +Command, +Args, +OrgText, +Windog, +Group)
%
%   Add a replace all button if
%
%     - There are more than one equivalent changes
%     - The command just edits the range
%     - It is not a case-change.

add_replace_all(1, _, _, _, _, _) :-
    !.
add_replace_all(N, "pce_emacs.edit", replace(With), Text, W, Group) :-
    \+ ( string_lower(With, Lower),
         string_lower(Text, Lower)
       ),
    !,
    format(string(Label), 'Replace all ~D occurrences', [N]),
    send(Group, append,
         new(B, button(Label, message(W, replace_all, With))),
         right),
    send(B, alignment, column).
add_replace_all(_, _, _, _, _, _).

%!  fix_command(+CodeAction, +Range, -Title, -Command, -Args, -Kind) is
%!              semidet.
%
%   If  the  command  is  an  immediate  edit,  Command  is  unified  to
%   `"pce_emacs.edit"`.

fix_command(Fix, _Range, Title, Command, Args, Kind),
    #{ command: CommandDict, title: Title } :< Fix,
    is_dict(CommandDict),
    #{ command: Command, arguments: Args } :< CommandDict =>
    Kind = Fix.get(kind, "unknown").
fix_command(Fix, Range, Title, Command, Edits, Kind),
    #{ edit: Edit, title: Title } :< Fix,
    fix_edits(Edit, Range, Edits) =>
    Command = "pce_emacs.edit",
    Kind = Fix.get(kind, "unknown").
fix_command(Fix, _Range, Title, Command, Args, Kind),
    #{ command: Command, arguments: Args, title: Title } :< Fix =>
    Kind = Fix.get(kind, "unknown").
fix_command(_Fix, _Range, _Title, _Command, _Args, _Kind) =>
    fail.

%!  fix_edits(+Dict, +Range, -Edits:dict) is semidet.
%
%   Normalize the `edit` field  to  a   dict  holding  `changes`, a dict
%   mapping document URIs to a list  of   changes.  If  the edit exactly
%   replaces  the  diagnostics  fragment,   Edits    is   unified   with
%   replace(NewText). This detection allows for batch replacement of all
%   equivalent diagnostics.

fix_edits(Dict, Range, Edits) :-
    fix_edits(Dict, Edits0),
    (   dict_pairs(Edits0.get(changes), _, [_URI-[Change]]),
        #{newText:Replace, range:Range} :< Change
    ->  Edits = replace(Replace)
    ;   Edits = Edits0
    ).

fix_edits(Dict, Edits),
    is_dict(Dict),
    #{ changes: _ } :< Dict =>
    Edits = Dict.
fix_edits(Dict, Edits),
    is_dict(Dict),
    #{ documentChanges: Edits0 } :< Dict =>
    Edits = Edits0.
fix_edits(_, _) =>
    fail.


%!  fix_icon(+Command:string, +Kind:string, -Icon) is det.

fix_icon("clangd.applyTweak", _, 'ide/lsp/apply-tweak.svg') :- !.
fix_icon("clangd.applyFix",   _, 'ide/lsp/apply-fix.svg')   :- !.
fix_icon(_,                   _, 'ide/lsp/apply-fix.svg').

%   ->apply_change(+Title)
%
%   Apply the selected code action.
%
%   (*) Note that killing the fragment also kills this window.

apply_change(W, TitleObj:string) :->
    "Apply a selected change"::
    get(W, get_hyper, fragment, fixes, Fixes),
    get(W, get_hyper, fragment, range, Range),
    object(TitleObj, string(TitleAtom)),
    atom_string(TitleAtom, Title),
    (   member(Fix, Fixes),
        fix_command(Fix, Range, Title, Command, Args, _Kind)
    ->  true
    ),
    get(W, lsp_client, LSP),
    (   Command = "pce_emacs.edit",
        Args = replace(NewText)
    ->  debug(lsp(replace), 'Direct replace with ~p', [NewText]),
        send(W, send_hyper, fragment, string, NewText),
        send(W, send_hyper, fragment, free)
    ;   send(W, send_hyper, fragment, free),	% see (*)
        send(LSP, execute_command, Command, Args)
    ).

replace_all(W, With:char_array) :->
    "Replace all \"same\" diagnostics"::
    get(W, hypered, fragment, Fragment),
    same_diagnostics(Fragment, All),
    forall(member(F, All),
           ( send(F, string, With),
             send(F, free)
           )).

:- pce_end_class.

%!  same_diagnostics(+To:emacs_lsp_diagnostic, -List) is det.
%
%   Find the diagnostics with the same text, source and code as To,
%   which includes To.

:- det(same_diagnostics/2).
same_diagnostics(To, List) :-
    get(To, string, Text),
    get(To, source, Source),
    get(To, code, Code),
    get(To, text_buffer, TB),
    get(TB, first_fragment, Start),
    same_diagnostics(Start, Text, Source, Code, List).

same_diagnostics(Frag, Text, Source, Code, List) :-
    (   send(Frag, instance_of, emacs_lsp_diagnostic),
        send(Frag?string, equal, Text),
        get(Frag, source, Source),
        get(Frag, code, Code)
    ->  List = [Frag|Tail]
    ;   Tail = List
    ),
    (   get(Frag, next, message(@arg1, instance_of, emacs_lsp_diagnostic),
            Next)
    ->  same_diagnostics(Next, Text, Source, Code, Tail)
    ;   Tail = []
    ).
