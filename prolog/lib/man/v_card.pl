/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi-prolog.org
    WWW:           https://www-prolog.org/projects/xpce/
    Copyright (c)  1985-2026, University of Amsterdam
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

:- module(man_card_editor,
        [
        ]).

:- use_module(library(pce)).
:- use_module(library(pce_history)).

:- pce_autoload(behaviour_item, library(man/behaviour_item)).
:- pce_autoload(report_dialog,  library(pce_report)).
:- use_module(library(pce_html_manual), []).

:- pce_begin_class(man_card_editor,     man_frame,
                   "Show HTML manual page for the selected entry").

variable(history, history, get, "Navigation history (library(pce_history))").

                /********************************
                *            CREATE             *
                ********************************/

initialise(CE, Manual:man_manual) :->
    "Create from manual"::
    send(CE, send_super, initialise, Manual, 'Card Viewer'),
    send(CE, slot, history,
         history(message(CE, goto_history, @arg1))),
    send(CE, append, new(D, dialog)),
    send(CE, fill_dialog),
    send(new(TE, man_html_card), below, D),
    send(new(report_dialog), below, TE),
    send(TE, name, html_card),
    send(CE, create),                         % compute layout before
                                              % setting selection
    send(CE, selected, Manual?selection).

fill_dialog(CE) :->
    get(CE, member, dialog, D),
    get(CE, history, H),

    send(D, append,
         new(Goto, behaviour_item(goto, '',
                                  if(@arg1 \== @nil,
                                     message(CE, request_selection,
                                             @arg1)))),
         right),
    send(Goto, advance, clear),
    send(Goto, hor_stretch, 100),
    send(D, append, new(TB, tool_bar), right),
    send(TB, reference, point(0,20)),
    get(H, button, backward, BackBtn),
    get(H, button, forward,  FwdBtn),
    send(TB, append, BackBtn),
    send(TB, append, FwdBtn),
    send(D, resize_message, message(D, layout, @arg2)).


                /********************************
                *           SELECTION          *
                ********************************/

html_card(CE, HC) :<-
    "Inner HTML view"::
    get(CE, member, html_card, HC).

selected(CE, Obj:object*) :->
    "Display selected object"::
    send(CE?html_card, selection, Obj),
    (   Obj == @nil
    ->  true
    ;   send(CE?history, location, Obj)
    ).

%!  ->goto_history(+Obj) is det.
%
%   Invoked by =|library(pce_history)|='s =|->forward|= /
%   =|->backward|= on the history object during navigation. The
%   history's =|action|= slot is non-=|@nil|= during this call so the
%   inner =|->selection|= call's =|->location|= is suppressed -- we
%   only have to update the visible card.

goto_history(CE, Obj:any) :->
    (   atom(Obj)
    ->  send(CE, goto_url, Obj)
    ;   send(CE?html_card, selection, Obj)
    ).

add_history(CE, URL:any) :->
    "doc_window expects this on its frame; no-op for the card view"::
    send(CE?history, location, URL).

goto_url(CE, URL:name) :->
    "Open a link clicked in the rendered chunk"::
    send(CE?html_card, url, URL),
    send(CE?history, location, URL).

:- pce_end_class.
