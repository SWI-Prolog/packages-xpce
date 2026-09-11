/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
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

:- module(emacs_buffer_menu, []).
:- use_module(library(pce)).
:- use_module(library(pane_frame)).
:- use_module(library(toolbar)).
:- use_module(library(pce_util), [default/3]).
:- require([ send_list/3
           ]).

resource(open,      image, image('tool/open.svg')).
resource(saveall,   image, image('16x16/saveall.png')).
resource(help,      image, image('tool/help.svg')).
resource(bookmarks, image, image('16x16/bookmarks.png')).

/* The buffer menu as a pane.

It used to be a frame of its own, holding a tool bar, the list of buffers
and a reporter.  It is a `tool_pane' now -- see library(pane_frame) -- so
it drops into a tab of any window of the IDE or, being a list to pick a
buffer from, down the left of the editor it was asked for from.  What it
has to say goes on the status bar of the window it ends up in, so it
carries no reporter of its own.
*/

:- pce_begin_class(emacs_buffer_menu, tool_pane,
                   "List showing all PceEmacs buffers").

class_variable(pane_side, {above,below,left,right}, left,
               "The list of buffers is added down the left").

%       The pane belongs to whichever window of the IDE it lands in; its
%       tool bar still acts on @emacs, whose ->find_file and
%       ->save_some_buffers the buttons send.

initialise(BM, Emacs:[emacs]) :->
    "Create menu for buffer-list"::
    default(Emacs, @emacs, TheEmacs),
    send_super(BM, initialise, buffer_menu),
    send(BM, append_window, new(D, tool_dialog(TheEmacs))),
    send(BM, append_window, new(_B, emacs_buffer_browser(TheEmacs)), D, below),
    send(BM, fill_tool_bar).

                 /*******************************
                 *            MEMBERS           *
                 *******************************/

browser(BM, B:emacs_buffer_browser) :<-
    "The window the buffers are listed in"::
    get(BM, window, emacs_buffer_browser, B).

tool_bar(BM, TB:tool_bar) :<-
    "Get the toolbar"::
    get(BM, window, tool_dialog, D),
    get(D, tool_bar, @on, TB).

                 /*******************************
                 *             PANE             *
                 *******************************/

pane_label(_BM, Label:name) :<-
    "What my tab is called"::
    Label = 'Buffers'.

fill_tool_bar(BM) :->
    "Fill the toolbar"::
    get(BM, tool_bar, TB),
    send_list(TB, append,
              [ tool_button(find_file,
                            resource(open),
                            'Open file for editing'),
                tool_button(save_some_buffers,
                            resource(saveall),
                            'Save all modified buffers'),
                tool_button(show_bookmarks,
                            resource(bookmarks),
                            'Show bookmarks'),
                tool_button(help,
                            resource(help),
                            'Help on PceEmacs')
              ]).

selection(BM, B:emacs_buffer*) :->
    "Select emacs buffer"::
    get(BM, browser, Browser),
    (   B == @nil
    ->  send(Browser, selection, @nil)
    ;   get(B, name, Name),
        get(Browser, member, Name, DictItem),
        send(Browser, insert_after, DictItem, @nil), % move to top
        send(Browser, selection, DictItem)
    ).

:- pce_end_class(emacs_buffer_menu).

:- pce_begin_class(emacs_buffer_browser, browser,
                   "Browse the emacs buffers").

initialise(B, Emacs:emacs) :->
    "Create for Emacs"::
    send_super(B, initialise, 'Emacs buffers'),
    send(B, name, browser),
    send(B, open_message, message(@arg1?object, open, tab)),
    send(B, tab_stops, vector(150)),
    send(B, attach_popup),
    send(B, dict, Emacs?buffer_list).

typed(B, Ev:event, Delegate:[bool]) :->
    "Map DEL and backspace to kill selected buffer"::
    (   (   get(Ev, id, 'DEL')
        ;   get(Ev, id, backspace)
        ),
        get(B, selection, _)
    ->  send(B, kill_selection)
    ;   send_super(B, typed, Ev, Delegate)
    ).

kill_selection(B) :->
    get(B, selection, DI),
    send(DI?object, kill).

attach_popup(B) :->
    "Attach the popup menu"::
    send(B, popup, new(P, popup)),

    new(Buffer, @arg1?object),
    send(P, update_message,
         message(B, selection, @arg1)),
    send_list(P, append,
              [ menu_item(open_buffer,
                          message(Buffer, open, tab)),
                menu_item(open_new_window,
                          message(Buffer, open, window),
                          @default, @on),
                menu_item(properties,
                          message(Buffer, properties),
                          @default, @on),
                menu_item(kill_buffer,
                          message(Buffer, kill))
              ]).


drop_files(_B, Files:chain, _At:point) :->
    "Drag-and-drop interface"::
    send(Files, for_all,             % @emacs, not my <-application: every
         message(@emacs, open_file, @arg1)).  % window of the IDE belongs
                                     % to @prolog_ide, and opening a source
                                     % is PceEmacs's to do

:- pce_end_class(emacs_buffer_browser).




