/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org/packages/xpce/
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

:- module(xdot_demo, [xdot_demo/0]).
:- use_module(library(pce)).
:- use_module(library(xdot), []).
:- autoload(library(pce_emacs), [emacs/1]).
:- pce_autoload(report_dialog, library(pce_report)).

/** <module> xdot renderer demo

Loads a bundled graphviz demo file (`xdot_demo.dot`) into an
xdot_window and adds a menu bar with:

  * File → Open, Reload demo, Fit, Quit
  * Engine → dot, neato, fdp, sfdp, circo, twopi

Started via xdot_demo/0.  The condition on the pce_demo/4 entry
requires graphviz's `dot` on PATH.
*/

xdot_demo :-
    new(F, xdot_demo_frame),
    send(F, open).

:- pce_begin_class(xdot_demo_frame, frame,
                   "Frame hosting the xdot demo").

initialise(F) :->
    send_super(F, initialise, 'xdot demo'),
    send(F, append, new(D, dialog)),
    send(new(W, xdot_window), below, D),
    send(W, name, view),
    send(W, display,
         new(T, text('Left-click a node/edge for info · Right-click for menu',
                     left, italic)),
         point(5,0)),
    send(T, colour, grey50),
    send(new(report_dialog), below, W),
    send(F, build_menu, D),
    send(F, load_demo).

view(F, W:xdot_window) :<-
    "The embedded xdot_window"::
    get(F, member, view, W).

build_menu(F, D:dialog) :->
    "Populate the menu bar in D"::
    send(D, pen, 0),
    send(D, gap, size(0,0)),
    send(D, append, new(MB, menu_bar)),
    send(MB, append, new(File,   popup(file))),
    send(MB, append, new(Engine, popup(engine))),
    send_list(File, append,
              [ menu_item(open,
                          message(F, open_file)),
                menu_item(reload_demo,
                          message(F, load_demo)),
                menu_item(fit,
                          message(F?view, fit)),
                menu_item(view_source,
                          message(F, view_source),
                          end_group := @on),
                menu_item(quit,
                          message(F, destroy))
              ]),
    forall(engine(E),
           send(Engine, append,
                menu_item(E, message(F, engine, E)))),
    send(Engine, selection, dot).

engine(dot).
engine(neato).
engine(fdp).
engine(sfdp).
engine(circo).
engine(twopi).

engine(F, Name:name) :->
    "Switch graphviz layout engine and re-render current source"::
    get(F, view, W),
    get(W, xdot, X),
    send(X, engine, Name),
    (   get(X, source, Src), Src \== @nil
    ->  send(W, load, Src)
    ;   true
    ).

load_demo(F) :->
    "Load the bundled xdot_demo.dot"::
    absolute_file_name(demo('xdot_demo.dot'),
                       Path,
                       [access(read)]),
    get(F, view, W),
    send(W, load, Path),
    send(F, install_interactions).

install_interactions(F) :->
    "Attach click handlers and popups to the current xdot"::
    get(F, view, W),
    get(W, xdot, X),
    send(X, node_popup, F?node_popup),
    send(X, edge_popup, F?edge_popup),
    send(X, node_clicked,
         message(F, report_click, @arg1)),
    send(X, edge_clicked,
         message(F, report_click, @arg1)).

node_popup(F, P:popup) :<-
    "Shared right-click menu for nodes"::
    new(P, popup(node_actions)),
    send_list(P, append,
              [ menu_item(show_info,
                          message(F, report_click, @arg1)),
                menu_item(highlight,
                          message(@arg1, highlight, colour(red))),
                menu_item(unhighlight,
                          message(@arg1, highlight, @nil))
              ]).

edge_popup(F, P:popup) :<-
    "Shared right-click menu for edges"::
    new(P, popup(edge_actions)),
    send_list(P, append,
              [ menu_item(show_info,
                          message(F, report_click, @arg1)),
                menu_item(highlight,
                          message(@arg1, highlight, colour(red))),
                menu_item(unhighlight,
                          message(@arg1, highlight, @nil))
              ]).

report_click(F, G:xdot_group) :->
    "Show info about the clicked node/edge in the report bar"::
    get(G, name, Name),
    (   send(G, instance_of, xdot_edge)
    ->  get(G, tail, T),
        get(G, head, H),
        send(F, report, status,
             'Clicked %O with name %s (%s → %s)', G, Name, T, H)
    ;   send(F, report, status,
             'Clicked %O with name %s', G, Name)
    ).

open_file(F) :->
    "Prompt for a .dot file via the native file browser and load it"::
    working_directory(CWD, CWD),
    (   get(F, open_file,
            chain(tuple('DOT files', 'dot')),
            CWD, Path)
    ->  get(F, view, W),
        send(W, load, Path)
    ;   true
    ).

view_source(F) :->
    "Open the current source .dot file in PceEmacs"::
    get(F, view, W),
    get(W, xdot, X),
    get(X, source, Src),
    (   Src == @nil
    ->  send(F, report, warning, 'No source loaded')
    ;   get(Src, name, Path),
        emacs(Path)
    ).

:- pce_end_class(xdot_demo_frame).
