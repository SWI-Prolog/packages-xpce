/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi-prolog.org
    WWW:           https://www-prolog/projects/xpce/
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

:- module(man_manual, []).

:- use_module(library(pce)).
:- use_module(library(persistent_frame)).
:- use_module(library(pce_help_file)).
:- autoload(library(man/classification), [scope/2]).
:- autoload(library(apply), [maplist/2]).
:- autoload(library(gui_tracer), [guitracer/0]).
:- autoload(library(pce_debug), [checkpce/0]).
:- autoload(library(pce_emacs), [emacs/0, start_emacs/0]).
:- autoload(library(pce_util), [send_list/3]).
:- autoload(library(swi_compatibility), [auto_call/1]).
:- autoload(library(swi_ide), [prolog_ide/1]).
:- autoload(library(swi_preferences), [prolog_edit_preferences/1]).
:- autoload(library(www_browser), [www_open_url/1]).

:- pce_autoload(man_event_viewer, library(man/showevent)).
:- pce_autoload(man_demo_browser, demo(pce_demo)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
                            OVERALL ARCHITECTURE

The following diagram  provides an overall  view of the  design of the
manual tools.

                         ManualTool
                             |
                             | (select)
                             |
                             V           | ClassBrowser
                           Tools         | ClassHierarchy
                             |           | TopicBrowser
                             |           | KeywordBrowser
                             |
                             | (find/browse)
                             V
                   [Type] Name [Summary]
                            /|\
       Examples----/-------- | -----------\
                  /          |             \
                 /           |              \
             Sources      Textual          Relations
                        Attributes    [Type] Name [Summary]


The communication between  the tools is arranged  via messages send to
and possible broadcasted by ManualTool.  These messages are:

    ->request_selection: man_frame, object*, [bool]
        Set the <-selection and <-selection_holder attribute of the
        ManualTool and broadcasts the following messages:

                * SelectionHolder ->release_selection
                * AllTools        ->selected: object*

        If bool == @on, the card viewer is started automatically

    ->tool_focus: object*
        Set the focus of all tools.  Broadcasted to all tools.

    ->relate: object
        Request manual to relate selection to object.

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(man_manual, persistent_frame,
                   "PCE manual main object").

class_variable(geometry,        geometry,               '+0+0').
class_variable(user_scope,      chain,                  chain(basic, user),
         "Default scoping of manual material").

variable(selection,             object*,        get,
         "Currently selected object").
variable(selection_holder,      man_frame*,     get,
         "Tool holding selection").
variable(tool_focus,            object*,        get,
         "Arg of last ->tool_focus").
variable(tools,                 sheet,          get,
         "Tool-name --> tool mapping").
variable(space,                 man_space,      get,
         "Manual module collection").
variable(focus_history,         chain,          get,
         "Chain of focused cards").
variable(selection_history,     chain,          get,
         "Chain of selected cards").
variable(user_scope,            chain,          get,
         "Types in user's scope").
variable(search_patterns,       chain*,         both,
         "Search patterns to be highlighted").


                /********************************
                *            CREATE             *
                ********************************/

initialise(M, _Dir:[directory]) :->
    "Create the manual main object"::
    send(M, send_super, initialise, 'XPCE Manual'),
    send(M, can_resize, @off),
    send(M, done_message, message(M, quit)),
    get(M, class_variable_value, user_scope, Scope),
    %   Phase 8: the .doc tree retired; man_space remains as an
    %   inert container for any tools that still reach for it.
    send(M, slot, space, new(Space, man_space(reference))),
    send(M, slot, tools, new(sheet)),
    send(M, slot, focus_history, new(chain)),
    send(M, slot, selection_history, new(chain)),
    send(M, slot, user_scope, Scope),

    send(Space, attribute, attribute(report_to, M)),
    send(M, append, new(D, dialog)),
    send(M, fill_dialog, D),

    send(M, check_runtime),
    send(M, report, status, 'For help, see `File'' menu').


unlink(M) :->
    "Manual is destroyed"::
    get(M, space, Space),
    send(Space, delete_attribute, report_to),
    send(M, send_super, unlink).


check_runtime(M) :->
    "Check for runtime system"::
    (   get(@pce, is_runtime_system, @on)
    ->  send(@display, inform, M, "XPCE Manual",
             '%s.  %s\n%s %s',
             'This is a runtime version of XPCE',
             'Most of the manual will not work.',
             'Contact xpce-request@swi.psy.uva.nl',
             'for a information on the development version')
    ;   true
    ).


fill_dialog(M, D) :->
    send(D, gap, size(5, 5)),
    send(D, append, new(MB, menu_bar)),
    send(MB, append, new(F, popup(file))),
    send(MB, append, new(V, popup(browsers,
                                  message(M, start_tool, @arg1)))),
    send(MB, append, new(T, popup(tools,
                                  message(M, start_tool, @arg1)))),
    send(MB, append, new(H, popup(history))),

    /* FILE menu */

    send_list(F, append,
              [ menu_item(about,
                          message(M, about)),
                menu_item(help,
                          message(M, help)),
                menu_item(demo_programs,
                          message(M, start_demo),
                          @default, @on),
                menu_item('FAQ',
                          message(M, faq),
                          @default, @on),
                new(Prefs, popup(edit_preferences))
         ]),
    send(Prefs, end_group, @on),
    send_list(Prefs, append,
              [ menu_item('XPCE User Defaults',
                          message(M, edit_preferences, xpce_user)),
                menu_item('XPCE System Defaults',
                          message(M, edit_preferences, xpce),
                          end_group := @on),
                menu_item('Prolog Defaults',
                          message(M, edit_preferences, prolog))
              ]),
    (   get(@pce, window_system, windows)
    ->  send(Prefs, append,
             menu_item('Prolog Stack Limits',
                       message(M, edit_prolog_registry)))
    ;   true
    ),
    send_list(F, append,
              [ menu_item(quit,
                          message(M, quit)),
                menu_item(quit_pce,
                          message(M, quit_pce))
              ]),


    /* BROWSERS menu */

    send_list(V, append,
         [ menu_item(class_hierarchy),
           menu_item(class_browser),
           menu_item(global_objects),
           menu_item(errors,
                     end_group := @on),
           menu_item(search),
           menu_item(group_overview),
           menu_item(examples,          end_group := @on)
         ]),

    /* TOOLS menu */

    send_list(T, append,
         [ visual_hierarchy,
           inspector,
           menu_item(event_viewer,
                     message(M, event_viewer)),
           gap,
           statistics,
           menu_item(check_object_base,
                     message(M, check_object_base))
         ]),

    /* HISTORY menu */

    new(SI, menu_item(selection, @nil, @default, @off,
                      not(message(M?selection_history, empty)))),
    new(FI, menu_item(focus, @nil, @default, @off,
                      not(message(M?focus_history, empty)))),
    send(SI, popup,
         new(SH, popup(selection, message(M, select_history_menu,
                                          selection_history, @arg1)))),
    send(FI, popup,
         new(FH, popup(focus, message(M, select_history_menu,
                                      focus_history, @arg1)))),

    send(SH, update_message, message(M, update_history_menu,
                                     selection_history, @receiver)),
    send(FH, update_message, message(M, update_history_menu,
                                     focus_history, @receiver)),
    send(H, append, SI),
    send(H, append, FI),

    send(D, append, new(label)).


                /********************************
                *         STARTING TOOLS        *
                ********************************/

start_tool(M, ToolName:name, Tool:frame) :<-
    "Start named tool"::
    (   get(M?tools, value, ToolName, Tool)
    ->  send(Tool, expose)
    ;   create_tool(M, ToolName, Tool),
        send(Tool, open)
    ->  send(M, register_tool, ToolName, Tool)
    ;   send(@display, inform, M, "XPCE Manual", 'Failed to start %s', ToolName)
    ).

start_tool(M, ToolName:name) :->
    "Start named tool"::
    get(M, start_tool, ToolName, _).

register_tool(M, Name:name, Tool:man_frame) :->
    "Register frame as a menual tool"::
    send(Tool, slot, tool_name, Name),
    send(M?tools, append, attribute(Name, Tool)).


expose_tool(M, ToolName:name) :->
    "Expose named tool"::
    get(M?tools, value, ToolName, Tool),
    send(Tool, expose).


create_tool(M, Name, Tool) :-
    tool_class(Name, M, Term),
    new(Tool, Term).

tool_class(class_browser,       M, man_class_browser(M)).
tool_class(class_finder,        M, man_class_browser(M)).
tool_class(class_hierarchy,     M, man_class_hierarchy(M)).
tool_class(search,              M, man_search_tool(M)).
tool_class(topics,              M, man_topic_browser(M)).
tool_class(card_viewer,         M, man_card_editor(M)).
tool_class(statistics,          M, man_statistics(M)).
tool_class(inspector,           M, isp_frame(M)).
tool_class(visual_hierarchy,    M, vis_frame(M)).
tool_class(global_objects,      M, man_object_browser(M)).
tool_class(errors,              M, man_error_browser(M)).
tool_class(examples,            M,
           man_module_browser(M, examples, man_example_card, 'XPCE Examples')).
tool_class(changes,             M,
           man_module_browser(M, changes, man_change_card, 'XPCE Changes')).
tool_class(group_overview,      M,
           man_group_browser(M, groups, 'Group Browser')).
tool_class(demos,		M, man_demo_browser(M)).
tool_class(event_viewer,	M, man_event_viewer(M)).


                /********************************
                *          DESTROYING           *
                ********************************/

destroy_tool(M, Tool:man_frame) :->
    "Destroy a tool"::
    (   get(M, selection_holder, Tool)
    ->  ignore(send(Tool, release_selection)),      % TBD: forward
        send(M, slot, selection_holder, @nil)
    ;   true
    ),
    send(M?tools, for_all,
         if(@arg1?value == Tool,
            message(M?tools, delete, @arg1?name))),
    send(Tool, destroy).


quit(M) :->
    "Quit Manual Tool"::
    send(M, status, unmapped).


quit_pce(M) :->
    "Exit from XPCE process"::
    send(@display, confirm, M, "XPCE Manual", 'Really exit XPCE?'),
    send(@pce, die).

                 /*******************************
                 *          PREFERENCES         *
                 *******************************/

edit_preferences(_, What:name) :->
    "Edit preferences file"::
    auto_call(prolog_edit_preferences(What)).

edit_prolog_registry(_M) :->
    "Edit SWI-Prolog registry settings"::
    auto_call(prolog_edit_preferences(stack_sizes)).


                /********************************
                *           MANUAL DATA         *
                ********************************/

module(M, Name:name, Create:[bool], Module) :<-
    "Find/create manual module"::
    get(M, space, Space),
    (   get(Space?modules, member, Name, Module)
    ->  true
    ;   Create == @on
    ->  new(Module, man_module(Space, Name))
    ;   fail
    ).


list_modules(M) :->
    "List associated modules"::
    new(V, view('Loaded Modules')),
    new(D, dialog),
    send(D, append, button(quit, message(D?frame, free))),
    send(D, below, V),
    send(V, tab_stops, vector(200)),
    send(V, font, normal),
    send(V, format, '%s\t%s\n\n', 'Module Name', 'Number of Cards'),
    new(NM, number(0)),
    new(NC, number(0)),
    send(M?space?modules, for_all,
         block(message(NM, plus, 1),
               message(NC, plus, @arg2?id_table?size),
               message(V, format, '%s\t%s\n',
                       @arg2?name, @arg2?id_table?size))),
    send(V, caret, 0),
    send(V, format, '%d cards in %d modules\n\n', NC, NM),
    send(V, caret, 0),
    send(V, open).

list_all_modules(M) :->
    "Load and list all modules from the directory"::
    send(M?space, load_all_modules),
    send(M, list_modules).


                 /*******************************
                 *          VIEW FILES          *
                 *******************************/

:- pce_help_file(pce_faq,     pce_help('pcefaq.hlp')).

faq(_M) :->
    "Start @helper on faq-database"::
    send(@helper, give_help, pce_faq, main).

                /********************************
                *          ABOUT/LICENCE        *
                ********************************/

about([ 'XPCE version %s'+[@pce?version]-boldhuge,
        'Copyright 1992-2026, University of Amsterdam, SWI-Prolog Solutions b.v.',
        'XPCE is covered by the BSD-2 license.',
        'XPCE use the Pango library for font rendering, which is covered by the LGPL 2.1 or later license',
        url('https://www.swi-prolog.org/packages/xpce/'),
        'Jan Wielemaker\nAnjo Anjewierden'-italic
      ]).


about(M) :->
    "Print about and licence info"::
    new(D, dialog('About XPCE')),
    send(D, transient_for, M),
    about(List),
    maplist(add_about(D), List),
    send(D, append, button(ok, message(D, destroy))),
    send(D, open_centered).

add_about(D, X-Font) :-
    !,
    add_about(X, Font, D).
add_about(D, X) :-
    add_about(X, normal, D).

add_about(url(Url), Font, D) :-
    !,
    send(D, append, new(T, text(Url, center, Font))),
    send(T, underline, @on),
    send(T, colour, blue),
    send(T, recogniser,
         click_gesture(left, '', single,
                       message(@prolog, goto_url, T?string?value))),
    send(T, cursor, hand2),
    send(T, alignment, center).
add_about(Fmt+Args, Font, D) :-
    !,
    Term =.. [string, Fmt | Args],
    send(D, append, new(T, text(Term, center, Font))),
    send(T, alignment, center).
add_about(Text, Font, D) :-
    send(D, append, new(T, text(Text, center, Font))),
    send(T, alignment, center).

goto_url(Url) :-
    send(@display, busy_cursor),
    (   catch(www_open_url(Url), _, fail)
    ->  true
    ;   send(@display, inform, @default, "XPCE Manual", 'Failed to open URL')
    ),
    send(@display, busy_cursor, @nil).

                 /*******************************
                 *             HELP             *
                 *******************************/

help(M) :->
    "Give help on the overall manual"::
    give_help(M, @nil, manual).


                /********************************
                *              DEMO             *
                ********************************/

:- multifile
    pce_demo:pcedemo/0.

start_demo(M) :->
    send(M, report, progress, 'Starting demo tool ...'),
    use_module(demo(pce_demo), []),
    pce_demo:pcedemo,
    send(M, report, done).


                /********************************
                *            CHECKING           *
                ********************************/

check_object_base(M) :->
    (   auto_call(checkpce)
    ->  send(@display, inform, M, "Check object base", 'Object base is consistent')
    ;   send(@display, inform, M, "Check object base", '%s\n%s',
             'Object base is corrupted',
             'See Prolog window for details')
    ).


                 /*******************************
                 *     START EXTERNAL TOOLS     *
                 *******************************/

guitracer(M) :->
    "Start the GUI tracer for Prolog"::
    (   catch(guitracer, _, fail)
    ->  true
    ;   send(M, report, error, 'Failed to load GUI tracer')
    ).

prolog_navigator(_M) :->
    "Start the source-code navigator"::
    prolog_ide(open_navigator).

thread_monitor(_M) :->
    "Start the thread monitor"::
    prolog_ide(thread_monitor).

start_emacs(_M) :->
    "Start PceEmacs (*scratch* buffer)"::
    auto_call(emacs).


                /********************************
                *            INSPECTOR          *
                ********************************/

inspect(M, V:object) :->
    "Start inspector on object"::
    send(M, start_tool, inspector),
    send(M?tools?inspector, inspect, V).


                 /*******************************
                 *       EXTERNAL INVOKES       *
                 *******************************/

manual(M, Object:'class|behaviour|object') :->
    "Open manual on object"::
    (   send(Object, instance_of, class)
    ->  send(M, start_tool, class_browser),
        send(M, request_tool_focus, Object)
    ;   (   send(Object, instance_of, behaviour)
        ;   send(Object, instance_of, man_global)
        )
    ->  send(M, request_selection, @nil, Object, @on)
    ;   Object = @Ref,
        atom(Ref)
    ->  send(M, request_selection, @nil, man_global(Ref), @on)
    ;   send(M, report, error, 'Cannot start manual from %O', Object),
        fail
    ).


                 /*******************************
                 *          USER-SCOPING        *
                 *******************************/

%   The user-scope classification table is library('man/classification')
%   (Prolog facts), which exports scope/2.  Loaded above; queried below.

in_scope(M, Obj:object) :->
    "Test if object is in current scope"::
    get(M, user_scope, Scope),
    get(Obj, man_id, Id),
    (   (   scope(Id, Type)
        ->  send(Scope, member, Type)
        ;   send(Scope, member, obscure)
        )
    ;   get(Obj, man_creator, Creator),
        Creator \== built_in,
        send(Scope, member, user)
    ).


user_scope(M, Scope:chain) :->
    "Modify scope and inform tools"::
    (   send(M?user_scope, equal, Scope)
    ->  true
    ;   send(M, slot, user_scope, Scope),
        send(M?tools, for_some,
             message(@arg1?value, user_scope, Scope))
    ).


                /********************************
                *         COMMUNICATION         *
                ********************************/


request_selection(M, Frame:man_frame*, Obj:any*, Open:[bool]) :->
    "Request to become selection holder"::
    get(M, selection_holder, OldHolder),
    (   OldHolder \== @nil
    ->  (   send(OldHolder, release_selection)
        ->  true
        ;   send(@display, inform, M, "XPCE Manual",
                 '%s does not release selection', OldHolder)
        )
    ;   true
    ),
    send(M, slot, selection_holder, Frame),
    send(M, slot, selection, Obj),
    send(M, update_history, selection_history, Obj),
    send(M?tools, for_some, message(@arg1?value, selected, Obj)),
    (   \+ get(M?tools, value, card_viewer, _)
    ->  (   Open == @on
        ->  send(M, report, progress, 'Starting Card Viewer ...'),
            send(M, start_tool, card_viewer),
            send(M, report, done)
        ;   true
        )
    ;   send(M, expose_tool, card_viewer)  % exposes it?
    ).


request_tool_focus(M, Obj:object*, ForceClass:[bool]) :->
    "Change the tool focus"::
    send(M, slot, tool_focus, Obj),
    send(M, update_history, focus_history, Obj),
    send(M?tools, for_some, message(@arg1?value, tool_focus, Obj)),
    (   (   ForceClass == @on
        ;   send(Obj, instance_of, class)
        ),
        \+ get(M?tools, value, class_browser, _)
    ->  send(M, report, progress, 'Starting Class Browser'),
        send(M, start_tool, class_browser),
        send(M, report, done)
    ;   send(M, expose_tool, class_browser)   % exposes it!
    ).


                /********************************
                *             HISTORY           *
                ********************************/

update_history(M, History:name, Obj:object*) :->
    "Add object to the requested history"::
    get(M, History, Chain),
    (   get(Chain, head, Obj)
    ->  true
    ;   ignore(send(Chain, delete, Obj)),
        send(Chain, prepend, Obj),
        (   get(Chain, size, S),
            S > 10
        ->  send(Chain, delete_tail)
        ;   true
        )
    ).


update_history_menu(M, History, Menu) :->
    "Update the contents of the history popup"::
    get(M, History, Chain),
    send(Menu, clear),
    send(Chain, for_some,
         message(Menu, append,
                 create(menu_item,
                        @arg1, @default,
                        when(message(@arg1, instance_of, chain),
                             ?(@pce, instance, string, 'G %s:%s',
                               when(message(@arg1?head, instance_of,
                                            class),
                                    @arg1?head?name,
                                    @arg1?head?context?name),
                               @arg1?head?group),
                             progn(assign(new(X, var),
                                          create(string, '%s',
                                                 @arg1?man_name)),
                                   message(X, translate, '\t', ' '),
                                   X))))).


select_history_menu(M, History:name, Obj) :->
    "Trap selected history item"::
    (   History == selection_history
    ->  send(M, request_selection, @nil, Obj, @on)
    ;   send(M, request_tool_focus, Obj)
    ).


                /********************************
                *            SOURCES            *
                ********************************/

request_source(M, Obj:object) :->
    "Display source of object"::
    (   get(Obj, source, Location)
    ->  auto_call(start_emacs),
        send(@emacs, goto_source_location, Location)
    ;   send(@display, inform, M, "XPCE Manual", 'Can''t find source')
    ).


:- pce_end_class.


                /********************************
                *          TOOL FRAMES          *
                ********************************/

:- pce_begin_class(man_frame(label), persistent_frame).

variable(manual,        man_manual,     get,
         "Manual we are related to").
variable(tool_name,     name,           get,
         "Name of the tool in this frame").


initialise(F, Manual:man_manual, Label:[name]) :->
    "Create from label"::
    send(F, send_super, initialise, Label),
    send(F, slot, manual, Manual),
    send(F, done_message, message(F, quit)).


user_scope(_F, _Scope:chain) :->
    "Generic operation: fail"::
    fail.


tool_focus(_F, _Focus:object*) :->
    "Generic operation: fail"::
    fail.


selected(_F, _Obj:object*) :->
    "Generic operation: fail"::
    fail.


release_selection(_F) :->
    "Generic operation: true"::
    true.


quit(F) :->
    "Destroy a tool"::
    send(F?manual, destroy_tool, F).


                /********************************
                *      GENERIC USER ACTIONS     *
                ********************************/

request_selection(F, Obj:any*, Open:[bool]) :->
    send(F?manual, request_selection, F, Obj, Open).

request_tool_focus(F, Obj:object, Force:[bool]) :->
    send(F?manual, request_tool_focus, Obj, Force).

request_source(F, Obj:object) :->
    send(F?manual, request_source, Obj).

help(F) :->
    "Give help on a manual tool"::
    get(F, manual, Manual),
    get(F, tool_name, ToolName),
    give_help(Manual, F, ToolName).

:- pce_end_class.

                /********************************
                *              HELP             *
                ********************************/

give_help(Manual, Frame, ToolName) :-
    get(Manual, module, tools, @on, Tools),
    (   get(Tools?id_table, find_value, @arg2?tool_name == ToolName, Card)
    ->  send(Manual, request_selection, Frame, Card, @on)
    ;   send(@display, inform, Manual, "XPCE Manual",
             'Sorry, Can''t find help card ...')
    ).

