/*  Figures for online.plx: "Using the online manual"
*/

:- module(fig_online, []).
:- use_module(library(pce)).
:- use_module('../screenshots', [settle/0, settle/1, last_frame/1]).
:- use_module(library(pce_manual), []).
:- use_module(library(swi_ide), []).
:- use_module(library(man/v_manual), []).
:- use_module(library(lists), [member/2]).

:- multifile
    screenshot:figure/2,
    screenshot:figure_option/2.

/* Each tool of the manual is a pane of the development environment.  We
   make the tool ourselves and put it in a window of its own of a fixed
   size, so the result does not depend on the arrangement of tools the
   user who runs this saved.
*/

manual_tool(Name, Term, Size, Tool) :-
    new(Tool, Term),
    send(@manual, register_tool, Name, Tool),
    send(@prolog_ide, place_tool, Tool, frame),
    get(Tool, frame, Frame),
    send(Frame, size, Size),
    settle.


%   \figref{classbrowser}, section "Class browser": the class browser
%   showing the send methods of class editor that have `caret' in their
%   name or summary.

screenshot:figure(classbrowser, Tool) :-
    manual_tool(class_browser, man_class_browser(@manual),
                size(720, 460), Tool),
    send(Tool, tool_focus, class(editor)),
    get(Tool, dialog_member, D),
    get(D, member, filter, Filter),
    send(Filter, selection, chain(basic, advanced, obscure, user)),
    send(@manual, user_scope, Filter?selection),
    get(D, member, display, Display),
    send(Display, selection, chain(send_method)),
    get(D, member, search, Search),
    send(Search, selection, caret),
    send(Tool, search_filter, regex(caret)),
    settle.

%   \figref{card}, section "Reading cards": the card viewer showing the
%   description of a method.

screenshot:figure(card, Tool) :-
    manual_tool(card_viewer, man_card_editor(@manual),
                size(560, 400), Tool),
    get(class(device), send_method, display, Method),
    send(@manual, request_selection, @nil, Method),
    settle.

%   \figref{searchtool}, section "Search tool": the search tool after
%   searching for "graphical and selection".

screenshot:figure(searchtool, Tool) :-
    new(Tool, man_search_tool(@manual)),   % opens itself
    send(@manual, register_tool, search, Tool),
    get(Tool, frame, Frame),
    send(Frame, size, size(720, 400)),
    settle,
    get(Tool, member, dialog, D),
    get(D, member, search, Item),
    Spec = 'graphical and selection',
    send(Item, selection, Spec),
    send(Tool, search, Spec),
    get(Tool, member, wordlist, WL),
    get(WL, member, selection, DI),
    send(WL, selection, DI),
    send(WL, normalise, DI),
    settle.

%   \figref{classhierarchy}, section "Class hierarchy": the class
%   hierarchy tool showing where the classes browser and list_browser
%   are in the hierarchy.  This is what typing a pattern in the Class
%   field does.  We do not use a pattern, as the classes it matches
%   depend on the libraries that are loaded.

screenshot:figure(classhierarchy, Tool) :-
    manual_tool(class_hierarchy, man_class_hierarchy(@manual),
                size(440, 440), Tool),
    get(Tool, member, man_class_hierarchy_window, CH),
    new(Nodes, chain),
    forall(member(Name, [list_browser, browser]),
           ( get(@classes, member, Name, Class),
             send(CH, add_class, Class),
             get(CH, node, Class, Node),
             send(Nodes, append, Node)
           )),
    send(CH, expand_node, class(window)),
    send(CH?tree, selection, Nodes),
    settle,
    get(CH, node, class(visual), Visual),   % scroll to show the
    get(Visual?image, absolute_y, Y),       % hierarchy below visual
    send(CH, scroll_to, point(0, Y-5)),
    settle.
