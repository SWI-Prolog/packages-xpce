/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org/packages/xpce/
    Copyright (c)  1996-2026, University of Amsterdam
                              VU University Amsterdam
                              CWI, Amsterdam
                              SWI-Prolog Solutions b.v.
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

:- module(emacs_application, []).
:- use_module(library(pce)).
:- use_module(library(pce_history)).
:- use_module(library(pane_frame), []).
:- use_module(library(swi_ide), []).
:- use_module(library(pce_util), [chain_list/2]).
:- use_module(library(toolbar), []).
:- use_module(library(broadcast)).
:- use_module(library(edit)).
:- use_module(library(lists)).
:- use_module(library(pce_help_file)).
:- autoload(bookmarks, [find_references_editor/2]).

:- if(current_prolog_flag(windows, true)).
:- use_module(dde_server).
:- endif.

%!  emacs_server
%
%   PceEmacs listens on a socket (see @emacs_server_address), so that
%   `xpce-client' and edit/1 from another  process reach this PceEmacs
%   rather than starting one of  their  own.   Set  the flag to `false'
%   before PceEmacs is created to  do   without,  which  is what a test
%   wants: it must not take the address of the PceEmacs of whoever runs
%   it, nor leave one behind.  ->server_start still starts the server if
%   it is asked to explicitly.

:- create_prolog_flag(emacs_server, true,
                      [ type(boolean),
                        keep(true)
                      ]).

:- require([ ignore/1
           , pce_help_file/2
           , member/2
           ]).

:- pce_global(@finder, new(finder)).
:- pce_autoload(finder, library(find_file)).


:- pce_begin_class(emacs, application,
                   "PceEmacs main object").

variable(buffer_list,   dict,    get, "List of buffers maintained").
variable(history,       history, get, "History of visited places").


                 /*******************************
                 *            CREATE            *
                 *******************************/

initialise(Emacs, Buffers:dict) :->
    send_super(Emacs, initialise, emacs),
    send(Emacs, kind, service),
    send(Emacs, slot, history,
         history(message(Emacs, goto_history, @arg1, tab))),
    send(Emacs, slot, buffer_list, Buffers),
    get(@emacs_mark_list, class, _), % force loading
    (   current_prolog_flag(emacs_server, false)
    ->  true
    ;   ignore(send(Emacs, server_start))
    ),
    ignore(send(Emacs, load_user_init_file)),
    register_clean_exit(Emacs).

unlink(Emacs) :->
    unregister_clean_exit(Emacs),
    send_super(Emacs, unlink).

start(_Emacs) :->
    true.


                /*******************************
                *          LSP EVENTS          *
                *******************************/

new_buffer(_Emacs, Buffer:emacs_buffer) :->
    "A new buffer was loaded"::
    (   object(@emacs_mark_list)
    ->  ignore(send(@emacs_mark_list, loaded_buffer, Buffer))
    ;   true
    ),
    broadcast(pce_emacs(opened(Buffer))).

free_buffer(_Emacs, Buffer:emacs_buffer) :->
    "A buffer is about to be destroyed"::
    broadcast(pce_emacs(closed(Buffer))).

editor_event(_Emacs, Ev:event) :->
    "Called after an event has been processed"::
    get(Ev, receiver, Editor),
    (   object(Editor),            % may have dropped out
        send(Editor, instance_of, editor)
    ->  get(Editor, text_buffer, TB),
        broadcast(pce_emacs(changed(TB)))
    ;   true
    ).

                 /*******************************
                 *         BUFFER MENU          *
                 *******************************/

:- pce_group(buffer).

show_buffer_menu(Emacs) :->
    "Show the buffer menu"::
    (   get(@prolog_ide, member, buffer_menu, Menu)
    ->  send(Menu, expose)
    ;   send(emacs_buffer_menu(Emacs), open)
    ).


selection(_Emacs, B:emacs_buffer*) :->
    "Select emacs buffer"::
    (   get(@prolog_ide, member, buffer_menu, Menu)
    ->  send(Menu, selection, B)
    ;   true
    ).


                 /*******************************
                 *      BUFFERS AND FILES       *
                 *******************************/

:- pce_group(file).

buffer(Emacs, Name:name, B:emacs_buffer) :<-
    "Find named buffer"::
    get(Emacs, buffer_list, Dict),
    get(Dict, member, Name, DI),
    get(DI, object, B).

file_buffer(_, File:file, Buffer:emacs_buffer) :<-
    "Find existing buffer holding file"::
    get(File, base_name, Base),
    get(@emacs_base_names, member, Base, Chain),
    get(Chain, find, message(@arg1?file, same, File), Buffer).

buffers(Emacs, Buffers:chain) :<-
    "Chain with all emacs-buffers"::
    get(Emacs?buffer_list?members, map, @arg1?object, Buffers).


open_file(_Emacs, File:file, How:[{here,tab,split,window}]) :->
    "Open a file"::
    new(B, emacs_buffer(File)),
    send(B, open, How).


find_file(Emacs, Dir:[directory]) :->
    "Find and edit file"::
    get(@finder, file, @on, @default, Dir, FileName),
    send(Emacs, open_file, FileName).

goto_source_location(Emacs,
                     Location:source_location,
                     Where:where=[{here,tab,split,window}],
                     Title:title=[char_array]*) :->
    "Visit the indicated source-location"::
    (   Title == @nil
    ->  true
    ;   send(Emacs, location_history)
    ),
    get(Location, file_name, File),
    send(Emacs, ensure_source_file, File),
    new(B, emacs_buffer(File)),
    get(B, open, Where, Frame),
    send(B, check_modified_file),
    get(Frame?current_pane, editor, Editor),
    get(Editor, mode, Mode),
    (   get(Location, line_no, Line),
        Line \== @nil
    ->  send(Editor, mark_status, inactive),
        get(Editor, scan, 0, line, Line-1, start, SOL),
        (   get(Location, line_pos, LinePos),
            LinePos \== @nil
        ->  Start is SOL+LinePos
        ;   Start is SOL
        ),
        (   get(Location, length, Length),
            Length \== @nil,
            Length > 0
        ->  End is Start + Length
        ;   get(Editor, scan, Start, line, 0, end, End)
        ),
        send(Editor, selection, End, Start, highlight)
    ;   true
    ),
    (   Title == @nil
    ->  true
    ;   send(Mode, location_history, title := Title)
    ).

%   ->ensure_source_file: File
%
%   Verify the existence of File. In future  versions we can use this to
%   lazily load source files for binary-only distributions.

ensure_source_file(_Emacs, File) :->
    "Verify that File exists"::
    (   exists_file(File)
    ->  true
    ;   send(@pce, report, warning,
             string('No source for %s', File)),
        fail
    ).

location_history(Emacs, Title:title=[char_array]) :->
    "Save current location into history"::
    (   get(Emacs, current_frame, Frame),
        get(Frame?current_pane, editor, Editor),
        get(Editor, mode, Mode)
    ->  send(Mode, location_history, title := Title)
    ;   true
    ).

goto_history(Emacs, HE:emacs_history_entry,
             Where:where=[{here,tab,split,window}]) :->
    "Go back to an old history location"::
    get(HE, get_hyper, fragment, text_buffer, TB),
    get(HE, get_hyper, fragment, start, Start),
    get(HE, get_hyper, fragment, length, Len),
    get(TB, open, Where, Frame),
    send(TB, check_modified_file),
    get(Frame?current_pane, editor, Editor),
    End is Start+Len,
    send(Editor, caret, Start),
    send(Editor, selection, End, Start, highlight),
    send(Emacs?history, location, HE).

edit(Emacs, Location:source_location) :->
    "Equivalent to ->goto_source_location"::
    send(Emacs, goto_source_location, Location).


existing_file(_Emacs, Dir:[directory], File:file) :<-
    "Find existing file in directory"::
    get(@finder, file, @on, @default, Dir, FileName),
    new(File, file(FileName)).

open_object(_Emacs, Object:prolog, _NewWindow:new_window=[bool]) :->
    "Open from description"::
    edit(Object).

show_bookmarks(_) :->
    "Show PceEmacs bookmarks window"::
    send(@emacs_mark_list, expose).

reference_viewer(_, Title:string, BM:emacs_bookmark_editor) :<-
    "Find a view window to show references"::
    find_references_editor(Title, BM).


                 /*******************************
                 *             SAVE             *
                 *******************************/
:- pce_group(save).

save_some_buffers(BM, Confirm:[bool]) :->
    "Save all modified buffers"::
    new(ModifiedItem,
        and(@arg1?object?file \== @nil,
            @arg1?object?modified == @on)),
    (   get(BM?buffer_list, find, ModifiedItem, _)
    ->  send(BM?buffer_list, for_some,
             and(ModifiedItem,
                 or(Confirm == @off,
                    message(@display, confirm, BM, "PceEmacs",
                            'Save %s?', @arg1?object?file?name)),
                 message(@arg1?object, save, @arg1?object?file)))
    ;   send(@event, instance_of, event) % GUI initiated
    ->  send(@pce, report, status, 'No buffers need saving')
    ;   true
    ).


                 /*******************************
                 *          CLEAN EXIT          *
                 *******************************/

:- dynamic
    emacs_application/1,
    registered/0.

register_clean_exit(Emacs) :-
    asserta(emacs_application(Emacs)),
    (   registered
    ->  true
    ;   asserta(registered),
        at_halt(exit_emacs)
    ).

unregister_clean_exit(Emacs) :-
    retractall(emacs_application(Emacs)).

exit_emacs :-
    forall(emacs_application(Emacs),
           exit_emacs(Emacs)).

exit_emacs(Emacs) :-
    (   in_pce_thread_sync(send(Emacs, check_saved_at_exit))
    ->  true
    ;   cancel_halt('Unsaved buffers')
    ).

check_saved_at_exit(BM) :->
    "Check for unsaved buffers when called from exit"::
    send(BM, save_some_buffers, @on),
    new(ModifiedItem,
        and(@arg1?object?file \== @nil,
            @arg1?object?modified == @on)),
    (   get(BM?buffer_list, find, ModifiedItem, _)
    ->  send(@display, confirm, BM, "PceEmacs",
             'Discard modified buffers?')
    ;   true
    ).


                 /*******************************
                 *            WINDOWS           *
                 *******************************/

:- pce_group(window).

%       PceEmacs used to have a frame class of its own, and then an
%       application of its own.  A window of the IDE belongs to
%       @prolog_ide whoever opened it; what makes this one PceEmacs's is
%       the editor in it -- see emacs_view, which answers the pane
%       protocol.

frame(_Emacs, For:'emacs_buffer|emacs_view', Frame:pane_frame) :<-
    "A new frame showing For"::
    (   send(For, instance_of, emacs_view)
    ->  View = For
    ;   new(View, emacs_view(For))
    ),
    new(Frame, pane_frame(@prolog_ide, 'PceEmacs', View, @on)),
    send(View?text_buffer, update_label),
    send(Frame, open),
    get(View, editor, E),
    get(E, mode, Mode),
    ignore(send(Mode, new_buffer)).

%       A window of the IDE holds terminals and tools as well as views,
%       so none of the three routes below can take it that a pane of the
%       frame is an editor.  When the one it needs is not there -- `here'
%       in a window of terminals, `split' beside nothing to split -- the
%       buffer opens in a tab of its own, which every window can do.

show_buffer(_Emacs, Frame:pane_frame, B:emacs_buffer,
            How:[{here,tab,split}]) :->
    "Show B in Frame, here, in a tab of its own or beside the view"::
    (   How == tab,
        view_on_buffer(Frame, B, View)
    ->  send(Frame, current_pane, View)         % it is already open
    ;   How == split,
        editor_pane(Frame, Rel)
    ->  send(Frame, split, new(New, emacs_view(B)), Rel, horizontally),
        setup_view(B, New)
    ;   How == here,
        editor_pane(Frame, View)
    ->  send(View?editor, text_buffer, B),
        send(Frame, current_pane, View)
    ;   send(Frame, append_pane, new(New, emacs_view(B)), B?name, @on),
        setup_view(B, New)
    ).

setup_view(B, View) :-
    send(B, update_label),
    send(View, setup_mode).

%!  editor_pane(+Frame, -View) is semidet.
%
%   An emacs_view of Frame to work in: the pane the user is in if that is
%   one, otherwise the first there is.

editor_pane(Frame, View) :-
    get(Frame, current_pane, Current),
    send(Current, instance_of, emacs_view),
    !,
    View = Current.
editor_pane(Frame, View) :-
    frame_pane(Frame, View),
    send(View, instance_of, emacs_view),
    !.

%!  view_on_buffer(+Frame, +Buffer, -View) is semidet.

view_on_buffer(Frame, B, View) :-
    frame_pane(Frame, View),
    send(View, instance_of, emacs_view),
    get(View, text_buffer, TB),
    TB == B,
    !.

frame_pane(Frame, Pane) :-
    get(Frame, panes, Panes),
    chain_list(Panes, List),
    member(Pane, List).

%       Every window of the IDE belongs to @prolog_ide, so being a member
%       no longer says a window is one of PceEmacs's.  Holding an editor
%       does.  <-members is in most-recently-worked-in order -- see
%       `pane_frame ->input_focus' -- so the first that qualifies is the
%       one to use.

current_frame(_Emacs, Frame:pane_frame) :<-
    "PceEmacs frame the user is working in"::
    (   send(@event, instance_of, event),
        get(@event, window, Window),
        get(Window, frame, Frame),
        send(Frame, instance_of, pane_frame),
        shows_an_editor(Frame)
    ->  true
    ;   get(@prolog_ide, members, Members),
        chain_list(Members, Frames),
        member(Frame, Frames),
        send(Frame, instance_of, pane_frame),
        send(Frame, on_current_desktop),
        shows_an_editor(Frame)
    ->  true
    ).

%!  shows_an_editor(+Frame) is semidet.
%
%   True when Frame holds a view a buffer can be shown in.

shows_an_editor(Frame) :-
    editor_pane(Frame, _).


                 /*******************************
                 *             MODE             *
                 *******************************/

:- pce_group(mode).

modes(_Emacs, ModeNames:chain) :<-
    "Return chain with known modes"::
    get(@mode_name_type, context, ModeNames).


                 /*******************************
                 *             HELP             *
                 *******************************/

:- pce_group(help).

:- pce_help_file(emacs, pce_help('emacs.hlp')).
:- pce_help_file(emacs_customise, pce_help('customise.hlp')).

help(_Emacs) :->
    "Display general help"::
    send(@helper, give_help, emacs, main).

customise(_Emacs) :->
    "Display customisation help"::
    send(@helper, give_help, emacs_customise, main).


                 /*******************************
                 *              SERVER          *
                 *******************************/

:- pce_group(server).

server_start(Emacs, Force:[bool]) :->
    "Start server-mode (xpce-client interface)"::
    server_start(Emacs, Force).

:- if(current_predicate(start_emacs_dde_server/1)).
server_start(_Emacs, _Force) :-
    (   \+ get(class(socket), send_method, listen, _)
    ;   \+ send(class(socket), has_feature, unix_domain)
    ),
    !,
    start_emacs_dde_server(false).
:- endif.
server_start(_Emacs, _Force) :-
    get(@emacs_server, status, listen),
    !.
server_start(Emacs, Force) :-
    (   send(@emacs_server_address, exists, @off)
    ->  (   Force \== @on,
            pce_catch_error(socket, send(@emacs_server, connect))
        ->  free(@emacs_server),
            send(Emacs, report, status, 'Server on other PceEmacs'),
            fail
        ;   free(@emacs_server), % will recreate!
            ignore(send(Emacs, report, status, 'Restarted server')),
            send(@emacs_server_address, remove)
        )
    ;   true
    ),
    ignore(send(@emacs_server, listen)).

chrome_server(_Emacs) :->
    "Start HTTP server on 9292 for Edit With Emacs"::
    use_module(library(emacs/emacs_chrome_server)),
    member(Goal, [emacs_chrome_server]),        % fool xref
    call(Goal).

:- pce_group(customise).


                 /*******************************
                 *       USER EXTENSIONS        *
                 *******************************/

load_user_extension(_Emacs, Base:name) :->
    "Load Prolog user file with this base-name"::
    (   absolute_file_name(emacs_user_library(Base),
                           [ access(read),
                             file_type(prolog),
                             file_errors(fail)
                           ],
                           Extension)
    ->  ignore(load_files(user:Extension, [autoload(true)]))
    ;   true
    ).


load_user_init_file(_Emacs) :->
    "Load user_profile('.pceemacsrc') or user_profile('pceemacs.ini')"::
    (   get(@pce, operating_system, win32)
    ->  Base = 'pceemacs.ini'
    ;   Base = '.pceemacsrc'
    ),
    (   absolute_file_name(user_profile(Base),
                           [ access(read),
                             file_errors(fail)
                           ],
                           Profile)
    ->  ignore(load_files(user:Profile, [autoload(true)]))
    ;   true
    ).

:- pce_end_class(emacs).

