/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2002-2025, University of Amsterdam
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

:- module(swi_ide,
          [ prolog_ide/0,               %
            prolog_ide/1                % +Action
          ]).
:- use_module(library(pce)).
:- use_module(library(pane_frame), [pane_kind/2]).
:- use_module(library(pane_layouts),
              [pane_placement/3, forget_arrangements/0]).
:- use_module(library(toolbar), []).
:- autoload(library(man/v_visual), [ pce_show_visual_tool/0 ]).
:- autoload(library(www_browser), [www_open_url/1]).
:- autoload(library(swi_preferences), [prolog_edit_preferences/1]).
:- autoload(library(pce_openframes), [confirm_open_frames/1]).
:- use_module(library(pce_util), [chain_list/2]).
:- use_module(library(lists), [member/2, memberchk/2]).
:- require([ pce_image_directory/1,
	     file_directory_name/2
	   ]).

/** <module> SWI-Prolog IDE controller

This module defines  the  application   @prolog_ide  and  the  predicate
prolog_ide(+Action). The major motivation is be   able  to delay loading
the IDE components to the autoloading of one single predicate.
*/

                 /*******************************
                 *    AUTOLOAD OF COMPONENTS    *
                 *******************************/

:- pce_image_directory(library('trace/icons')).

:- pce_autoload(swi_console,            library('swi/swi_console')).
:- pce_autoload(prolog_debug_status,    library('trace/status')).
:- pce_autoload(prolog_navigator,       library('trace/browse')).
:- pce_autoload(prolog_query_frame,     library('trace/query')).
:- pce_autoload(prolog_trace_exception, library('trace/exceptions')).
:- pce_autoload(prolog_thread_monitor,  library('swi/thread_monitor')).
:- pce_autoload(prolog_debug_monitor,   library('swi/pce_debug_monitor')).
:- pce_autoload(xref_tool,              library('pce_xref')).

                 /*******************************
                 *            TOPLEVEL          *
                 *******************************/

%!  prolog_ide(+Action)
%
%   Invoke an action on the (SWI-)Prolog  IDE application. This is a
%   predicate to ensure  optimal  delaying   of  loading  and object
%   creation for accessing the  various   components  of  the Prolog
%   Integrated Development Environment.

prolog_ide :-
    prolog_ide(open_console).

prolog_ide(Action) :-
    in_pce_thread(send(@prolog_ide, Action)).


                 /*******************************
                 *         THE IDE CLASS        *
                 *******************************/

:- pce_global(@prolog_ide, new(prolog_ide)).
:- pce_global(@prolog_exception_window, new(prolog_trace_exception)).

:- pce_begin_class(prolog_ide, application, "Prolog IDE application").

class_variable(tool_placement, {as_arranged,frame,tab,split}, as_arranged,
               "Where a tool the user asks for is put").

initialise(IDE) :->
    "Create as service application"::
    send_super(IDE, initialise, prolog_ide),
    send(IDE, kind, service).

open_console(IDE) :->
    "Open SWI-Prolog Cross-Referencer frontend"::
    (   get(IDE, member, swi_console, Console)
    ->  send(Console, open)
    ;   new(Console, swi_console),
        send(Console, application, IDE),
        send(Console, wait)
    ).

open_debug_status(IDE) :->
    "Open/show the status of the debugger"::
    send(IDE, show_tool, prolog_debug_status).

open_exceptions(_IDE, Gui:[bool]) :->
    "Open/show exceptions"::
    (   Gui == @on
    ->  catch(tdebug, _, guitracer)
    ;   true
    ),
    send(@prolog_exception_window, open).

open_navigator(IDE, Where:[directory|source_location]) :->
    "Open Source Navigator"::
    (   send(Where, instance_of, directory)
    ->  get(IDE, navigator, Where, _)
    ;   send(Where, instance_of, source_location)
    ->  get(Where, file_name, File),
        file_directory_name(File, Dir),
        get(Where, line_no, Line),
        (   integer(Line)
        ->  LineNo = Line
        ;   LineNo = 1
        ),
        get(IDE, navigator, Dir, Navigator),
        send(Navigator, goto, File, LineNo)
    ;   get(IDE, navigator, _)
    ).


navigator(IDE, Dir:[directory], Navigator:prolog_navigator) :<-
    "The navigator pane, made and shown if there is none"::
    (   get(IDE, tool, prolog_navigator, Navigator)
    ->  send(IDE, expose_tool, Navigator)
    ;   new(Navigator, prolog_navigator(Dir)),
        send(IDE, place_tool, Navigator, @default)
    ),
    (   Dir == @default
    ->  true
    ;   send(Navigator, directory, Dir)
    ).

open_query_window(IDE) :->
    "Open window to enter a query"::
    (   get(IDE, member, prolog_query_frame, QF)
    ->  true
    ;   new(QF, prolog_query_frame),
        send(QF, application, IDE)
    ),
    send(QF, expose).

open_interactor(_) :->
    "Create a new interactor window"::
    autoload_call(interactor).

thread_monitor(IDE) :->
    "Open a monitor for running threads"::
    (   current_prolog_flag(threads, true)
    ->  send(IDE, show_tool, prolog_thread_monitor)
    ;   send(@display, report, error,
             'This version of SWI-Prolog is not built \n\c
                  with thread-support')
    ).

debug_monitor(IDE) :->
    "Open monitor for debug messages"::
    send(IDE, show_tool, prolog_debug_monitor).

xref(IDE) :->
    "Open Cross-Referencer frontend"::
    (   get(IDE, tool, xref_tool, XREF)
    ->  send(IDE, expose_tool, XREF)
    ;   get(IDE, show_tool, xref_tool, @default, XREF),
        send(XREF, update)
    ).

visual_hierarchy(_IDE) :->
    "Show the visual hierarchy tool"::
    (   object(@manual)
    ->  send(@manual, start_tool, visual_hierarchy)
    ;   pce_show_visual_tool
    ).

                 /*******************************
                 *          TOOL PANES          *
                 *******************************/

/* A tool that is a pane rather than a window of its own.

Such a tool lives in a tab of a window of the IDE, beside a terminal or
an editor or another tool.  There is one of each: asking for it again
brings the one there is into view rather than making a second.
*/

tool(IDE, Class:name, Pane:window) :<-
    "The tool pane of that class, in whichever window holds it"::
    get(IDE, members, Frames),
    chain_list(Frames, List),
    member(F, List),
    send(F, instance_of, pane_frame),
    get(F, panes, Panes),
    chain_list(Panes, Ps),
    member(Pane, Ps),
    send(Pane, instance_of, Class),
    !.

%       Where a tool the user asks for is put: in a window of its own, in
%       a tab of the window they are working in, or beside what is already
%       there.  `prolog_ide.tool_placement' says which, so it can be set
%       once in a Defaults file and hold for every tool.

show_tool(IDE, Class:name, How:[{frame,tab,split}], Pane:window) :<-
    "Show the tool pane of that class, making one if there is none"::
    (   get(IDE, tool, Class, Pane)
    ->  send(IDE, expose_tool, Pane)
    ;   Term =.. [Class],
        new(Pane, Term),
        send(IDE, place_tool, Pane, How)
    ).

%       A tool that has something to say about how it is made -- the
%       navigator takes the directory to root the tree at -- makes itself
%       and asks for the placing alone.

place_tool(IDE, Pane:window, How:[{as_arranged,frame,tab,split}]) :->
    "Put a new tool pane in a window of the IDE"::
    send(IDE, place_pane, Pane, @default, How),
    send(IDE, expose_tool, Pane).

                 /*******************************
                 *          PLACEMENT           *
                 *******************************/

/* Where a new pane goes.

Everything the IDE is asked to show that it has to find a place for comes
through here: a tool, a source, a terminal.  What decides is
`prolog_ide.tool_placement': one of the three fixed answers, or
`as_arranged', which reads the arrangements of library(pane_layouts) and
answers from the way windows holding these panes have been arranged.

An arrangement can say what no fixed answer can: which of the panes there
already the new one belongs beside, on which edge of them, and how much of
their room it takes.
*/

placement(IDE, Pane:window, Frame:'pane_frame*',
               How:[name], Placement:prolog) :<-
    "Where Pane goes: window, tab or split(Panes, Side, Share)"::
    get(IDE, tool_placement, How, Where),
    (   Where == as_arranged,
        Frame \== @nil,
        arranged_placement(Pane, Frame, Arranged)
    ->  Placement = Arranged
    ;   fixed_placement(Where, Frame, Pane, Placement)
    ).

%!  fixed_placement(+Where, +Frame, +Pane, -Placement) is det.
%
%   What the three answers the user can pin the setting to mean.  A window
%   of its own is also what is left when there is no window to put a pane
%   in, and a tab is what `as_arranged' falls back on when no arrangement
%   has anything to say -- which is what the IDE has always done.

fixed_placement(frame, _Frame, _Pane, window) :-
    !.
fixed_placement(_Where, @nil, _Pane, window) :-
    !.
fixed_placement(split, Frame, Pane, split(Relatives, Side, @default)) :-
    get(Frame, current_pane, Rel),
    !,
    new(Relatives, chain(Rel)),
    get(@prolog_ide, pane_side, Pane, Side).
fixed_placement(_Where, _Frame, _Pane, tab).

%!  arranged_placement(+Pane, +Frame, -Placement) is semidet.
%
%   What the arrangements say about a pane of this kind in this window.
%   The library answers in kinds; the panes of those kinds in the tab the
%   user is in are what the new one is put beside.

arranged_placement(Pane, Frame, Placement) :-
    pane_kind(Pane, Kind),
    frame_kinds(Frame, LiveKinds),
    pane_placement(Kind, LiveKinds, Rule),
    rule_placement(Rule, Frame, Placement).

rule_placement(window, _Frame, window).
rule_placement(tab, _Frame, tab).
rule_placement(split(Kinds, Side, Share), Frame, Placement) :-
    (   tab_panes(Frame, Kinds, Relatives)
    ->  Placement = split(Relatives, Side, Share)
    ;   Placement = tab
    ).

frame_kinds(Frame, Kinds) :-
    get(Frame, panes, Panes),
    chain_list(Panes, List),
    findall(Kind, ( member(Pane, List),
                    pane_kind(Pane, Kind)
                  ), Kinds0),
    sort(Kinds0, Kinds).

%!  tab_panes(+Frame, +Kinds, -Panes) is semidet.
%
%   The panes of the tab the user is in that are of one of these kinds.

tab_panes(Frame, Kinds, Panes) :-
    get(Frame, tab, Tab),
    get(Tab, windows, Windows),
    chain_list(Windows, List),
    findall(Pane, ( member(Pane, List),
                    pane_kind(Pane, Kind),
                    memberchk(Kind, Kinds)
                  ), Selected),
    Selected \== [],
    chain_list(Panes, Selected).

place_pane(IDE, Pane:window,
                Frame:frame=[pane_frame],
                How:how=[name],
                Label:label=[name]) :->
    "Put Pane where the user last put one like it"::
    (   Frame == @default
    ->  (   get(IDE, current_frame, F)
        ->  true
        ;   F = @nil
        )
    ;   F = Frame
    ),
    get(IDE, placement, Pane, F, How, Placement),
    apply_placement(IDE, F, Pane, Placement, Label).

%!  apply_placement(+IDE, +Frame, +Pane, +Placement, +Label) is det.
%
%   Splitting can still turn out to be impossible: the panes an
%   arrangement names may not sit together in the window as it is now.
%   Try them, then the whole tab -- down an edge of everything is a
%   faithful reading of any arrangement -- and failing that take a tab.

apply_placement(IDE, _Frame, Pane, window, _Label) :-
    !,
    send(new(pane_frame(IDE, @default, Pane)), open).
apply_placement(_IDE, Frame, Pane, split(Relatives, Side, Share), Label) :-
    !,
    (   send(Frame, split_beside, Pane, Relatives, Side, Share)
    ->  true
    ;   get(Frame, tab, Tab),
        get(Tab, windows, All),
        send(Frame, split_beside, Pane, All, Side, Share)
    ->  true
    ;   send(Frame, append_pane, Pane, Label, @on)
    ).
apply_placement(_IDE, Frame, Pane, _Tab, Label) :-
    send(Frame, append_pane, Pane, Label, @on).

expose_tool(_IDE, Pane:window) :->
    "Bring the window holding Pane up, with Pane in view"::
    get(Pane, frame, F),
    send(F, current_pane, Pane),
    send(F, open),
    send(F, expose).

show_tool(IDE, Class:name, How:[{frame,tab,split}]) :->
    "Show the tool pane of that class"::
    get(IDE, show_tool, Class, How, _).

pane_side(_IDE, Pane:window, Side:{above,below,left,right}) :<-
    "Which side of what is there a tool is added on"::
    (   send(Pane, has_get_method, pane_side)
    ->  get(Pane, pane_side, Side)
    ;   Side = below
    ).

tool_placement(IDE, How:[{as_arranged,frame,tab,split}], Where:name) :<-
    "Where a new tool goes; How overrules the setting"::
    (   How \== @default
    ->  Where = How
    ;   get(IDE, class_variable_value, tool_placement, Where)
    ).

tool_placement(_IDE, Where:{as_arranged,frame,tab,split}) :->
    "Say where a tool the user asks for is to be put"::
    get(@pce, convert, prolog_ide, class, Class),
    send(Class, class_variable_value, tool_placement, Where).

%       A source the user asks to see -- edit/1, emacs/1, a location
%       picked in a tool -- goes by the same setting.  `emacs_buffer
%       <-open' has words of its own for it: a tool in a window of its
%       own is a frame, a buffer in one is a window.

source_placement(IDE, Where:{as_arranged,here,tab,split,window}) :<-
    "Where a source the user asks to see is opened"::
    get(IDE, tool_placement, @default, Placement),
    (   Placement == frame
    ->  Where = window
    ;   Where = Placement
    ).

update_tool_placement_menu(IDE, Popup:popup) :->
    "Tick where a tool goes now"::
    get(IDE, tool_placement, @default, Where),
    send(Popup, selection, Where).

current_frame(IDE, F:pane_frame) :<-
    "A window of mine to put a tool in"::
    get(IDE, members, Frames),
    chain_list(Frames, List),
    member(F, List),
    send(F, instance_of, pane_frame),
    send(F, on_current_desktop),
    !.

                 /*******************************
                 *          THE WINDOWS         *
                 *******************************/

/* What a pane_frame asks of the application it belongs to.

Every window of the IDE belongs to me, whichever tool opened it, so that
it behaves the same however it was made: dropping a terminal onto an
editor and dropping an editor onto a terminal must leave the same window.
What differs between a terminal and an editor is answered by the pane,
not here -- see library(pane_frame).

Nothing below pulls PceEmacs or Epilog in at load time.  An XPCE class is
found by name when it is asked for, so loading the library the user just
asked for is enough.
*/

label_format(_IDE, Format:name) :<-
    "What a window of mine is called; a pane may overrule it"::
    Format = 'SWI-Prolog -- %s'.

frame_empty(_IDE, F:pane_frame) :->
    "The last pane of a window was closed"::
    ignore(send(F, record_arrangement)),  % it never sees ->close
    (   get(F, attribute, main, @on)
    ->  send(F, destroy),
        confirm_open_frames(
            [ message("The main Prolog console was closed\n\c
                       while there are open windows")
            ])
    ;   send(F, destroy)
    ).

close_frame(_IDE, F:pane_frame, Prolog:prolog=[bool]) :->
    "Close a window.  Optionally terminate Prolog"::
    send(F, destroy),
    (   Prolog == @on
    ->  halt
    ;   true
    ).

                 /*******************************
                 *            PANES             *
                 *******************************/

new_pane(IDE, F:pane_frame, Kind:[name]) :->
    "The new-tab button and File->New: an editor, or a terminal"::
    (   Kind == editor
    ->  send(IDE, new_editor, F)
    ;   send(IDE, new_terminal, F, @off, Kind)
    ).

new_editor(_IDE, F:pane_frame, Split:[bool]) :->
    "Put a PceEmacs editor in this window"::
    use_module(user:library(pce_emacs), []),
    call(start_emacs:start_emacs),      % the module of library(pce_emacs)
    new(B, emacs_buffer(@nil, '*scratch*')),
    new(V, emacs_view(B)),
    (   Split == @on
    ->  send(F, split, V, @default, vertically)
    ;   send(F, append_pane, V, @default, @on)
    ),
    send(B, update_label),
    send(V, setup_mode),
    send(F, keyboard_focus, V).

new_terminal(_IDE, F:pane_frame, Split:[bool], Profile:[name]) :->
    "Put an Epilog terminal in this window"::
    use_module(user:library(epilog), []),
    (   Split == @on
    ->  new(W, epilog_window),
        send(F, split, W, @default, vertically),
        send(F, keyboard_focus, W)
    ;   (   Profile == @default
        ->  TheProfile = prolog
        ;   TheProfile = Profile
        ),
        call(epilog:epilog_tab(F, TheProfile))
    ).

new_window(_IDE) :->
    "Open another window of the IDE"::
    use_module(user:library(epilog), []),
    call(epilog:epilog).

%       Which class makes a pane of a kind named in a description of a
%       window -- see `pane_frame <-pane_term'.  An editor and a terminal
%       are named for what they are rather than by their class, so a
%       description reads and writes by hand; every other pane is its own
%       class name, which pce_autoload/2 above finds when it is asked for.

pane_class(_IDE, Kind:name, Class:name) :<-
    "The class that makes a pane of that kind"::
    (   Kind == editor
    ->  use_module(user:library(pce_emacs), []),
        call(start_emacs:start_emacs),
        Class = emacs_view
    ;   Kind == terminal
    ->  use_module(user:library(epilog), []),
        Class = epilog_window
    ;   Class = Kind
    ).

                 /*******************************
                 *           ACTIONS            *
                 *******************************/

manpce_tool(_IDE, Tool:name) :->
    "Open a manpce/0 tool routed through @manual"::
    use_module(user:library(pce_manual), []),
    send(@manual, start_tool, Tool).

preferences(_IDE, Which:{prolog,xpce}) :->
    "Edit Prolog or GUI preferences"::
    prolog_edit_preferences(Which).

forget_arrangements(_IDE) :->
    "Throw away the arrangements the IDE has learned"::
    forget_arrangements,
    send(@display, inform,
         'Forgotten.  New panes go where the arrangements that come\n\c
          with the system say, until you arrange some windows yourself.').

open_url(_IDE, URL:name) :->
    "Open a URL"::
    www_open_url(URL).

                 /*******************************
                 *           MENU BAR           *
                 *******************************/

%       The menus every window of the IDE has, whatever is in it.  A pane
%       adds its own on top of these -- see `pane_frame ->update_menu_bar'
%       -- so what belongs here is what is about the window and the IDE
%       rather than about any one tool.

fill_menu_bar(IDE, MD:tool_dialog, F:pane_frame) :->
    "Build the menus every pane of a window shares"::
    get(MD, menu_bar, @on, MB),
    send(MB, append, new(File,     pane_popup(file))),
    send(MB, append, new(Settings, pane_popup(settings))),
    send(MB, append, new(Tools,    pane_popup(tools))),
    send(MB, append, new(GUI,      pane_popup('GUI'))),
    send(MB, append, new(Help,     pane_popup(help))),
    send_list(File, append,
              [ menu_item(editor_in_a_new_tab,
                          message(IDE, new_editor, F)),
                menu_item(terminal_in_a_new_tab,
                          message(IDE, new_terminal, F),
                          end_group := @on),
                menu_item(new_window,
                          message(IDE, new_window),
                          end_group := @on),
                menu_item(close_window,
                          message(IDE, close_frame, F),
                          accelerator := 'Shift-Ctrl-W'),
                menu_item(halt_prolog,
                          message(IDE, close_frame, F, @on))
              ]),
    send_list(Settings, append,
              [ menu_item(user_init_file,
                          message(IDE, preferences, prolog)),
                menu_item('GUI_preferences',
                          message(IDE, preferences, xpce),
                          end_group := @on),
                new(Placement, menu_item(new_tools_open, @default,
                                         'New tools and sources open'))
              ]),
    send(Placement, popup,
         new(PlacementPopup,
             popup(tool_placement,
                   message(IDE, tool_placement, @arg1)))),
    send_list(PlacementPopup, append,
              [ menu_item(as_arranged, @default,
                          'As you arranged them before'),
                menu_item(frame, @default, 'In a window of its own'),
                menu_item(tab,   @default, 'In a tab'),
                menu_item(split, @default, 'Beside what is there')
              ]),
    send(PlacementPopup, show_current, @on),
    send(PlacementPopup, update_message,
         message(IDE, update_tool_placement_menu, @receiver)),
    send(Settings, append,
         menu_item(forget_arrangements,
                   message(IDE, forget_arrangements),
                   'Forget how I arranged windows')),
    send_list(Tools, append,
              [ menu_item(navigator,
                          message(IDE, open_navigator)),
                menu_item(view_threads,
                          message(IDE, thread_monitor)),
                menu_item(debug_messages,
                          message(IDE, debug_monitor)),
                menu_item(cross_referencer,
                          message(IDE, xref),
                          end_group := @on),
                menu_item(edit_breakpoints,
                          message(IDE, open_debug_status)),
                menu_item(edit_exceptions,
                          message(IDE, open_exceptions, @on),
                          end_group := @on)
              ]),
    send_list(GUI, append,
              [ menu_item('GUI demo programs',
                          message(IDE, manpce_tool, demos)),
                menu_item(example_XPCE_code_snippets,
                          message(IDE, manpce_tool, examples),
                          end_group := @on),
                menu_item('Explore XPCE classes',
                          message(IDE, manpce_tool, class_browser)),
                menu_item('Explore XPCE class hierarchy',
                          message(IDE, manpce_tool, class_hierarchy)),
                menu_item('Explore XPCE global objects',
                          message(IDE, manpce_tool, global_objects)),
                menu_item('Explore XPCE errors',
                          message(IDE, manpce_tool, errors)),
                menu_item('Explore by function group',
                          message(IDE, manpce_tool, group_overview)),
                menu_item('Search XPCE manual',
                          message(IDE, manpce_tool, search),
                          end_group := @on),
                menu_item('Inspect GUI hierarchy',
                          message(IDE, manpce_tool, visual_hierarchy)),
                menu_item('Inspect XPCE object',
                          message(IDE, manpce_tool, inspector)),
                menu_item('Show XPCE events',
                          message(IDE, manpce_tool, event_viewer))
              ]),
    send_list(Help, append,
              [ menu_item('SWI-Prolog documentation',
                          message(IDE, open_url,
                                  'https://www.swi-prolog.org')),
                menu_item('SWI-Prolog Discourse forum',
                          message(IDE, open_url,
                                  'https://swi-prolog.discourse.group/'),
                          end_group := @on),
                menu_item('SWI-Prolog GUI tools',
                          message(IDE, open_url,
                                  'https://github.com/SWI-Prolog/packages-xpce/wiki'))
              ]),
    send(Settings, show_current, @on),
    send(Settings, multiple_selection, @on).

:- pce_end_class(prolog_ide).
