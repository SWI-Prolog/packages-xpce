/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org/packages/xpce/
    Copyright (c)  1985-2026, University of Amsterdam,
                              VU University Amsterdam
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

:- module(emacs_frame,
          [ emacs_register_closed_tab/1
          ]).
:- use_module(library(pce)).
:- use_module(library(tabbed_window)).
:- use_module(library(tab_frame)).
:- use_module(library(pane_frame),
              [ pane_frame_closed_tab/1,
                pane_tree_term/4,
                build_pane_tree/5
              ]).
:- use_module(prompt).
:- use_module(library(pce_util)).
:- use_module(library(pce_drop_target), [drop_target_event/4]).
:- use_module(library(debug)).
:- use_module(library(lists), [member/2, memberchk/2]).
:- use_module(library(apply), [maplist/3]).

:- require([ between/3,
             atomic_list_concat/2,
             default/3,
             send_list/2,
             send_list/3
           ]).
:- encoding(utf8).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@current_emacs_mode is a variable  pointing   to  the current emacs-mode
object.  Pushed by `emacs_key_binding  ->fill_arguments_and_execute' and
various others.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_global(@current_emacs_mode, new(var)).

%       PceEmacs used to have a frame, a tabbed window, a menu-bar dialog,
%       a menu bar and a mini window of its own.  All five are now
%       library(pane_frame): the frame is a pane_frame whose application
%       is @emacs, and what used to be emacs_frame is split between
%       @emacs (see `<-frame' and `->show_buffer' in application.pl) and
%       emacs_view, which answers the pane protocol below.

%!  emacs_register_closed_tab(+Frame) is det.
%
%   Register that we closed a tab in this frame.  Used to prevent
%   Command-W on MacOS from killing all tabs.

emacs_register_closed_tab(Frame) :-
    pane_frame_closed_tab(Frame).

%       A menu of a mode is added to whatever popup of that name is on the
%       bar already, which on a window shared with another application is
%       one that application made.  So this is on class pane_popup rather
%       than on a popup of PceEmacs's own, and an item carries its own
%       message rather than leaning on the one of the popup it lands in.

:- pce_extend_class(pane_popup).

append_item(P, Mode:emacs_mode, Item:any) :->
    "Append single menu item"::
    (   Item == -
    ->  send(P, append, gap)
    ;   atom(Item)
    ->  send(P, append,
             new(MI, menu_item(Item,
                               message(@emacs_mode, noarg_call, Item)))),
        (   accelerator(Item, Mode, Accell)
        ->  send(MI, accelerator, Accell)
        ;   true
        ),
        (   get(Mode, send_method, Item, tuple(_, Impl))
        ->  (   forall(( between(1, 10, ArgN),
                         get(Impl, argument_type, ArgN, ArgType)),
                send(ArgType, includes, default))
            ->  true
            ;   send(MI, label, string('%s ...', Item?label_name))
            )
        ;   send(MI, active, @off)
        )
    ;   send(P, append, Item?clone)
    ).

%!      accelerator(+Command, +Mode, -Accelerator)
%
%       Copy/cut are hacked due to the tricky combination of CUA and
%       native Emacs mode.

accelerator(Cmd,  Mode, Accell) :-
    get(Mode, bindings, KeyBindings),
    get(KeyBindings, accelerator_label, Cmd, Accell).

:- multifile pce_keybinding:alt_binding_function/2.

pce_keybinding:alt_binding_function(copy, prefix_or_copy). % Ctrl-V can be bound to prefix_or_copy.
pce_keybinding:alt_binding_function(cut,  prefix_or_cut).

:- pce_end_class.


:- pce_begin_class(emacs_view, view,
                   "View running an emacs_editor").
:- use_class_template(pane).

:- pce_global(@emacs_image_recogniser,
              new(handler(button,
                          message(@receiver?device?(mode), event, @event)))).

class_variable(size,         size, size(80,32), "Size of text-field").

variable(label, name*, none, "Label as set by my buffer").

initialise(V, B:buffer=[emacs_buffer], W:width=[int], H:height=[int]) :->
    "Create for buffer"::
    get(V, class_variable_value, size, size(DW, DH)),
    default(W, DW, Width),
    default(H, DH, Height),
    (   B == @default
    ->  new(Buffer, emacs_buffer(@nil, '*scratch*'))
    ;   Buffer = B
    ),
    send_super(V, initialise, @default, @default, @default,
               new(E, emacs_editor(Buffer, Width, Height))),
    send(E?text_image, recogniser, @emacs_image_recogniser),
    send(E, recogniser,
         handler(keyboard,
                 if(message(E?frame, has_send_method, editor_event),
                    message(E?frame, editor_event, @arg1),
                    new(or)))),

    get(Buffer, mode, ModeName),
    send(E, mode, ModeName),
    get(E, mode, Mode),             % the mode object
    ignore(send(Mode, new_buffer)),
    send(V, display_fixed, new(split_handle)).   % puts itself in the corner
%       My editor fills me and draws its own scroll bar, so what is left
%       for the grip in my corner is that much narrower.  See `window
%       <-content_area', which the grip places itself against.

content_area(V, Area:area) :<-
    "What is visible, less the scroll bar my editor draws"::
    get_super(V, content_area, Area),
    (   get(V, editor, E),
        get(E, scroll_bar, SB),
        SB \== @nil
    ->  get(SB, width, SBW),
        get(Area, width, W),
        send(Area, width, W-SBW)
    ;   true
    ).

%       A tab may hold more than one view (see class tab_frame), while
%       it can only show one label.  The label follows the view that has
%       the focus: every view keeps its own and pushes it out to the tab
%       and to the frame while it is the current one.

label(V, Label:name) :->
    "Set label of tab and frame"::
    send(V, slot, label, Label),
    send(V, update_labels).

label(V, Label:name) :<-
    "My label; see also ->update_labels"::
    get(V, slot, label, Label),
    Label \== @nil.

update_labels(V) :->
    "Put my label on my tab if I am the view it is showing"::
    (   get(V, label, Label),
        get(V, container, tab, Tab),
        current_in_tab(Tab, V)
    ->  send(Tab, label, Label),
        update_frame_labels(V)
    ;   true
    ).

%       My tab is the editor's, not the window's: the window makes its
%       title out of the label of *its* tab, so that one has to be told
%       as well.  ->update_tab_label takes the name from the pane the
%       user is working in, which is me if this is about me at all.

update_frame_labels(V) :-
    (   get(V, frame, Frame),
        Frame \== @nil,
        send(Frame, has_send_method, update_tab_label)
    ->  ignore(send(Frame, update_tab_label)),
        ignore(send(Frame, update_label))
    ;   true
    ).

%       current_in_tab(+Tab, +View)
%
%       True while View is the one whose label the tab is to carry.  A
%       window_tab holds a single window and thus always is.

current_in_tab(Tab, V) :-
    (   send(Tab, has_get_method, current)
    ->  get(Tab, current, V)
    ;   true
    ).

drop_files(V, Files:chain, _At:point) :->
    "Accept files dropped on me"::
    send(V?editor, drop_files, Files).


:- pce_group(pane).

%       What my frame asks of me.  See library(pane_frame): every one of
%       these is optional, and the frame guards each with
%       ->has_send_method before it sends it.

pane_label(V, Label:name) :<-
    "What my tab is called"::
    (   get(V, label, Label)
    ->  true
    ;   get(V, name, Label)
    ).

title_format(_V, Format:name) :<-
    "A window showing me is PceEmacs's, whoever else is in it"::
    Format = 'PceEmacs -- %s'.

tab_editable_label(_V, Editable:bool) :<-
    "My tab is named after my buffer, so it is not renamed by hand"::
    Editable = @off.

menu_bar_key(V, Key:name) :<-
    "The menu bar follows the mode, so the mode names it"::
    get(V, mode, Mode),
    get(Mode, name, Key).

keep_alive(V, Keep:bool) :<-
    "@on if my buffer holds unsaved changes to a file"::
    (   get(V, text_buffer, TB),
        get(TB, file, File),
        File \== @nil,
        get(TB, modified, @on)
    ->  Keep = @on
    ;   Keep = @off
    ).

pane_exposed(V) :->
    "I have become the current view"::
    send(V, update_labels),
    (   get(V, mode, Mode),             % the bar was cleared for me: say
        send(Mode, has_send_method, show_caret_line)  % again what is mine
    ->  ignore(send(Mode, show_caret_line))
    ;   true
    ),
    get(V, text_buffer, TB),
    (   get(V, frame, Frame),
        Frame \== @nil
    ->  ignore(send(TB, check_modified_file, Frame))
    ;   ignore(send(TB, check_modified_file))
    ).

frame_active(V, Val:bool) :->
    "My frame was activated or deactivated by the window manager"::
    (   Val == @on                      % `pane_frame ->input_focus' keeps
    ->  send(@emacs, selection, V?text_buffer)  % the frames in order
    ;   send(@emacs, selection, @nil)
    ).

sibling(V, New:emacs_view) :<-
    "A second view on my buffer"::
    new(New, emacs_view(V?text_buffer)).

%       A view is not a pane of a window in its own right -- the editor
%       it is in is -- so wherever the pane template would make a view
%       one, it is the editor that has to be made instead.  ->split is
%       not among them: that splits the tab of the editor I am in, which
%       is where a second view belongs.

new_window(V) :->
    "Put a new view like me in a window of its own"::
    get(V, sibling, New),
    (   get(V, pane_frame, Frame)
    ->  get(Frame, application, App)
    ;   App = @default
    ),
    send(new(pane_frame(App, @default, emacs_pane(New))), open).

detach(V) :->
    "Move me into a window of my own"::
    get(V, pane_frame, F),
    get(F, panes, Panes),
    get(Panes, size, Size),
    Size > 1,                           % alone already: nothing to do
    (   get(F, application, App0),
        App0 \== @nil
    ->  App = App0
    ;   App = @default
    ),
    get(V, display_position, point(X, Y)),
    ignore(send(F, arranged)),
    send(F, delete_pane, V, @off),      % take me out without destroying me
    new(New, pane_frame(App, @default, emacs_pane(V))),
    send(New, open, point(X, Y+20)).

move_to_tab(V) :->
    "Move me out of a split, into a tab of the editor"::
    get(V, pane_tab, Tab),
    get(Tab, windows, Windows),
    get(Windows, size, Size),
    Size > 1,                           % a tab of my own already
    get(V, pane_frame, F),
    editor_of(V, F, Group),
    send(Tab, delete, V),               % take me out without destroying me
    send(Group, append_view, V),
    ignore(send(F, arranged)).

new_tab(V) :->
    "Put a new view like me in a tab of the editor"::
    get(V, sibling, New),
    get(V, pane_frame, F),
    editor_of(V, F, Group),
    send(Group, append_view, New),
    send(F, keyboard_focus, New),
    ignore(send(F, arranged)).

%!  editor_of(+View, +Frame, -Editor) is semidet.
%
%   The editor View is a view of; fails if it is a pane of the window in
%   its own right, which is what it is while it is being moved about.

editor_of(V, F, Group) :-
    get(F, pane_group, V, Group),
    send(Group, instance_of, emacs_pane).

%       What is worth writing down about an editor: the source it shows
%       and where the caret is in it.  A line and a column rather than a
%       caret index: an index means nothing to a reader and is wrong as
%       soon as the file is edited by anything else.

pane_kind(_V, Kind:name) :<-
    "How I am written in a description of a window"::
    Kind = editor.

pane_term(V, Options:prolog) :<-
    "The source I show and where the caret is in it"::
    get(V, text_buffer, TB),
    (   get(TB, file, File),
        File \== @nil
    ->  get(File, absolute_path, Path),
        Where = file(Path)
    ;   get(TB, name, Name),
        Where = buffer(Name)
    ),
    get(V, caret, Caret),
    get(V, line_number, Caret, Line),
    get(V, column, Caret, Column),
    get(TB, mode, Mode),
    Options = [Where, line(Line), column(Column), mode(Mode)].

pane_term(V, Options:prolog) :->
    "Show the source a description asks for"::
    (   memberchk(file(Path), Options)
    ->  (   exists_file(Path)
        ->  true
        ;   print_message(informational, pane_frame(no_such_file(Path)))
        ),
        send(V, text_buffer, emacs_buffer(Path))
    ;   memberchk(buffer(Name), Options),
        get(@emacs, buffer, Name, TB)   % <-member of the dict is the item
    ->  send(V, text_buffer, TB)
    ;   true
    ),
    (   memberchk(mode(Mode), Options),
        get(V, mode, Now),
        \+ get(Now, name, Mode)
    ->  send(V, mode, Mode)
    ;   true
    ),
    (   memberchk(line(Line), Options)
    ->  send(V, line_number, Line),
        (   memberchk(column(Column), Options)
        ->  send(V, column, Column)
        ;   true
        )
    ;   true
    ).

:- pce_group(mode).

setup_mode(V) :->
    "My editor changed mode; the menu bar and my label follow"::
    send(V, update_labels),
    (   get(V, frame, Frame),
        Frame \== @nil,
        send(Frame, has_send_method, update_menu_bar)
    ->  ignore(send(Frame, update_menu_bar))
    ;   true
    ).

%       The whole menu bar of a mode, as `emacs_mode_menu' describes it.
%       The frame has already put the menus every pane shares on the bar;
%       a menu named here that is already there is added to rather than
%       made again.

fill_menu_bar(V, MD:tool_dialog) :->
    "Put the menus of my mode on the bar"::
    ignore(send(V, fill_tool_bar, MD)),
    get(V, mode, Mode),
    get(Mode, mode_menu, ModeMenu),
    get(MD, menu_bar, @on, MB),
    send(ModeMenu, for_all,
         message(V, append_menu_items, MB, Mode, @arg1?name, @arg1?value)).

%       The two history buttons live on the tool bar rather than in a
%       menu.  They are the editor's, not the application's -- a window
%       showing a terminal has no history to walk -- so the bar is shown
%       whenever the menus are rebuilt for me and hidden again when
%       another pane comes into view: see `pane_menu_dialog
%       ->clear_tool_bar'.  The buttons themselves are made once.

fill_tool_bar(_V, MD:tool_dialog) :->
    "Put the history buttons on the tool bar"::
    get(MD, menu_bar, @on, MB),
    (   get(MB, native, @on)
    ->  true                    % the menu bar is not drawn: two buttons
                                % on their own look stranded.  The
                                % history is on the Browse menu and on
                                % Control-Command-Left/Right.
    ;   get(MD, tool_bar, @on, TB)
    ->  (   get(TB?graphicals, size, 0)
        ->  get(@emacs, history, History),
            get(History, button, forward, Forward),
            get(History, button, backward, Backward),
            send_list(TB, append, [Backward,Forward]),
            send_list([Backward,Forward], activate)
        ;   true
        ),
        send(TB, displayed, @on)
    ;   true
    ).

append_menu_items(_V, MB:menu_bar, Mode:emacs_mode,
                  Name:name, Entries:chain) :->
    "Add the entries of one mode menu to the bar"::
    (   get(MB, member, Name, Popup)
    ->  true
    ;   send(MB, append, new(Popup, pane_popup(Name)))
    ),
    send(Entries, for_some, message(Popup, append_item, Mode, @arg1)).

:- pce_group(prompt).

prompt_using(V, Item:dialog_item, Rval:unchecked) :<-
    "Prompt for one value using a dialog-item"::
    (   get(V, frame, Frame),
        send(Frame, has_get_method, prompt_using)
    ->  get(Frame, prompt_using, Item, Rval)
    ;   new(D, dialog),             % very incomplete!
        send(D, transient_for, V),
        send(D, modal, transient),
        send(D, append, Item),
        get(D, confirm_centered, Rval)
    ).

prompt(V, Label:char_array, Default:[any], Type:[type], History:[chain],
       Rval:any) :<-
    "Prompt for a value"::
    get(V, mode, Mode),
    make_item(Mode, Label, Default, Type, History, Item),
    (   send(Item, instance_of, text_item)
    ->  send(Item, length, 60),
        send(Item, pen, 0)
    ),

    get(V, prompt_using, Item, RawRval),

    fix_rval(Type, RawRval, Rval),
    (   object(Rval),
        get(Rval, lock_object, @off)
    ->  send(Rval, lock_object, @on),       % protect during deletion!
        free(Item),
        get(Rval, unlock, Rval)
    ;   free(Item)
    ).


%       If the user entered a directory while requested for a file,
%       start the finder in the specified directory to provide the file.

fix_rval(Type, RawRval, RVal) :-
    send(Type, instance_of, type),
    send(Type, includes, file),
    \+ send(Type, includes, directory),
    atom(RawRval),
    send(directory(RawRval), exists),
    !,
    get(@finder, file, @on, directory := RawRval, RVal).
fix_rval(_, Rval, Rval).


:- pce_global(@emacs_prompt_for,
              new(constant(prompt, 'Prompt for value'))).

interactive_arguments(V, Impl:any, Times:[int], Argv:vector) :<-
    "Prompt for arguments for the given implementation"::
    get(V, mode, Mode),
    make_arg_vector(Impl, Times, Argv),
    (   get(Argv, index, @emacs_prompt_for, _)
    ->  (   get(V, frame, Frame),
            send(Frame, has_get_method, prompt_style),
            get(Frame, prompt_style, mini_window)
        ->  fill_arg_vector(Mode, Impl, Argv)
        ;   new(D, emacs_prompt_dialog(Mode, Impl, Argv)),
            send(D, prompt, V, Argv),
            send(D, destroy)
        )
    ;   true
    ).

fill_arg_vector(Mode, Impl, Argv) :-
    fill_arg_vector(1, Mode, Impl, Argv).

fill_arg_vector(ArgN, Mode, Impl, Argv) :-
    get(Impl, argument_type, ArgN, ArgType),
    get(Argv, element, ArgN, @emacs_prompt_for),
    !,
    get(Mode, interactive_argument, Impl, ArgN, Arg),
    get(ArgType, check, Arg, CheckedArg),
    send(Argv, element, ArgN, CheckedArg),
    Next is ArgN + 1,
    fill_arg_vector(Next, Mode, Impl, Argv).
fill_arg_vector(ArgN, Mode, Impl, Argv) :-
    get(Impl, argument_type, ArgN, _),
    !,
    Next is ArgN + 1,
    fill_arg_vector(Next, Mode, Impl, Argv).
fill_arg_vector(_, _, _, _).


make_arg_vector(Impl, Times, Argv) :-
    new(Argv, code_vector),
    make_arg_vector(1, Impl, Times, Argv).

make_arg_vector(ArgN, Impl, Times, Argv) :-
    get(Impl, argument_type, ArgN, ArgType),
    !,
    (   integer(Times),
        send(ArgType, includes, int)
    ->  send(Argv, element, ArgN, Times),
        NextTimes = @default
    ;   NextTimes = Times,
        (   send(ArgType, includes, default)
        ->  send(Argv, element, ArgN, @default)
        ;   send(Argv, element, ArgN, @emacs_prompt_for)
        )
    ),
    Next is ArgN + 1,
    make_arg_vector(Next, Impl, NextTimes, Argv).
make_arg_vector(_, _, _, _).


:- pce_end_class(emacs_view).


                 /*******************************
                 *        THE EDITOR PANE       *
                 *******************************/

/* PceEmacs keeps its tabs to itself.

A window of the IDE holds tools and terminals beside its sources, and
its tabs are whole layouts: opening a source in one of those would take
whatever sits beside the editor off the screen.  So the editor is a pane
that holds tabs of its own -- one per source -- and the tabs of the
window stay what they are for.  It is a pane_stack, the same class the
tools of the IDE are made of; see library(pane_frame).

Each of those tabs is a tab_frame and so may hold more than one view:
C-x 2 and C-x 3 split inside the tab the user is in, and the other tabs
of the editor keep the views they have.  `emacs_mode <-tab' is
`get(View, container, tab_frame, TF)', which now finds the editor's own
tab rather than the frame's -- which is what makes the window commands
go on working unchanged.
*/

:- pce_begin_class(emacs_tab, tab_frame,
                   "Tab of the editor, holding one or more views").

class_variable(editable_label, bool, @off,
               "I am named after my buffer, so not by the user").
class_variable(closable,       bool, @on,
               "I carry a button to close me").

close_tab(Tab) :->
    "Close my views, which takes me with them"::
    get(Tab, windows, Chain),
    chain_list(Chain, Views),
    forall(member(V, Views), send(V, close_pane)).

%       Not `tab_frame ->close_other_tabs': that destroys the tabs, which
%       would take a modified buffer along without asking.

close_other_tabs(Tab) :->
    "Close the views of every other tab of the editor"::
    get(Tab?device, tabs, Chain),
    chain_list(Chain, Tabs),
    forall(( member(Other, Tabs),
             Other \== Tab,
             object(Other)
           ),
           send(Other, close_tab)).

%       Not `tab_frame ->untab' either: that asks the tabbed_window for a
%       frame, and an editor's view lives in a pane_frame.  Cf. `emacs_view
%       ->detach', which does this for a view that is a pane of the window.

untab(Tab) :->
    "Move the view I show into a window of its own"::
    get(Tab, current, V),
    get(V, pane_frame, F),
    (   get(F, application, App0),
        App0 \== @nil
    ->  App = App0
    ;   App = @default
    ),
    get(V, display_position, point(X, Y)),
    send(Tab, delete, V),               % take it out without destroying it
    ignore(send(F, arranged)),
    new(New, pane_frame(App, @default, emacs_pane(V))),
    send(New, open, point(X, Y+20)).

:- pce_end_class(emacs_tab).


:- pce_global(@emacs_tab_popup, make_emacs_tab_popup).

make_emacs_tab_popup(P) :-
    new(P, popup),
    Tab = @arg1,
    Cond = (Tab?device?tabs?size \== 1),
    send_list(P, append,
              [ menu_item(close_tab,
                          message(Tab, close_tab)),
                menu_item(close_other_tabs,
                          message(Tab, close_other_tabs),
                          condition := Cond),
                menu_item(move_to_new_window,
                          message(Tab, untab),
                          condition := Cond)
              ]).


:- pce_begin_class(emacs_pane, pane_stack,
                   "The editor of a window of the IDE: sources in tabs").

initialise(EP, View:view=[emacs_view], Label:label=[name]) :->
    "Create showing View, or a scratch buffer"::
    send_super(EP, initialise, Label),
    send(EP, hide_single_label, @on),   % one source needs no tab strip
    send(EP, label_popup, @emacs_tab_popup),
    (   View == @default
    ->  new(V, emacs_view)
    ;   V = View
    ),
    send(EP, append_view, V).

%       The frame looks inside me for the pane the user is working in;
%       see `pane_frame <-current_pane'.  Everything it asks of a pane --
%       the menu bar it wants, what it is called, that it has come into
%       view -- is thus answered by the view, as it was when a view was a
%       pane in its own right.

current_pane(EP, View:window) :<-
    "The view I am showing"::
    get(EP, current, View).

pane_kind(_EP, Kind:name) :<-
    "How I am written in a description of a window"::
    Kind = editor.

pane_label(EP, Label:name) :<-
    "What my tab is called: the source I show"::
    get(EP, current_pane, View),
    get(View, pane_label, Label).

tab_editable_label(_EP, Editable:bool) :<-
    "My tab is named after my buffer, so it is not renamed by hand"::
    Editable = @off.

sibling(EP, New:emacs_pane) :<-
    "A second editor on the source I show"::
    get(EP, current_pane, View),
    new(New, emacs_pane(emacs_view(View?text_buffer))).

new_tab(_EP, Window:window, Label:[name], Tab:tab) :<-
    "A tab of mine holds one or more views"::
    (   Label == @default,
        send(Window, has_get_method, pane_label)
    ->  get(Window, pane_label, TheLabel)
    ;   TheLabel = Label
    ),
    new(Tab, emacs_tab(Window, TheLabel)).

append_view(EP, View:emacs_view, Label:[name]) :->
    "Show View in a tab of mine, and in the window I am in"::
    send(EP, append, View, Label, @on),
    (   get(EP, pane_frame, Frame)      % my tab is up; the window's tab
    ->  send(Frame, current_pane, View) % holding me has to come up too
    ;   true
    ).

                 /*******************************
                 *       WRITTEN DOWN           *
                 *******************************/

%       What is worth writing down about an editor: the sources it shows,
%       tab by tab, and how the views of a tab that holds several are
%       tiled.  The grammar is my own -- library(pane_layouts) strips me
%       to the kind `editor' and never looks inside -- but it is written
%       and read with the same walkers `pane_frame' uses for a tab of a
%       window, so there is one place that knows how a tiling is a term.
%
%       An editor showing a single source is written as that source and
%       no more: `editor([file('foo.pl'), line(120)])', which is what a
%       window of one editor has always said and what a term written by
%       hand says.  Only an editor with tabs or a split of its own needs
%       the rest of the grammar.

pane_term(EP, Options:prolog) :<-
    "The sources I show, tab by tab"::
    get(EP, tabs, Chain),
    chain_list(Chain, Tabs),
    (   single_view(EP, Tabs, View)
    ->  get(View, pane_term, Options)
    ;   maplist(view_tab_term(EP), Tabs, Terms),
        Options = [tabs(Terms)]
    ).

pane_term(EP, Options:prolog) :->
    "Show the sources a description asks for"::
    (   memberchk(tabs(Terms), Options)
    ->  get(EP, tabs, Chain0),
        chain_list(Chain0, Had),
        build_view_tabs(EP, Terms, Built),
        Built \== [],                   % nothing restored: keep what I have
        forall(member(Old, Had), send(Old, destroy)),
        expose_view_tab(EP, Built)
    ;   get(EP, current_pane, View),    % one source: my options are its
        send(View, pane_term, Options)
    ).

%!  single_view(+Editor, +Tabs, -View) is semidet.
%
%   The one view of an editor that shows a single source.

single_view(EP, [Tab], View) :-
    get(Tab, windows, Windows),
    get(Windows, size, 1),
    get(EP, current_pane, View).

view_tab_term(EP, Tab, tab(Options, Content)) :-
    findall(O, view_tab_option(EP, Tab, O), Options),
    get(Tab, window_tree, Tree),
    tab_current_view(Tab, Current),
    pane_tree_term(view_term, Tree, Current, Content).

view_tab_option(EP, Tab, current(true)) :-
    get(EP, on_top, Tab).

%       Which view has the keyboard is worth saying only where there is a
%       choice; a tab holding one view has none.

tab_current_view(Tab, Current) :-
    get(Tab, windows, Windows),
    (   get(Windows, size, Size),
        Size > 1
    ->  get(Tab, current, Current)
    ;   Current = @nil
    ).

view_term(View, view(Options)) :-
    get(View, pane_term, Options).

build_view_tabs(_, [], []).
build_view_tabs(EP, [Term|Terms], Built) :-
    (   build_view_tab(EP, Term, Tab, Options)
    ->  Built = [built(Tab, Options)|Rest]
    ;   Built = Rest
    ),
    build_view_tabs(EP, Terms, Rest).

build_view_tab(EP, tab(Options, Content), Tab, Options) :-
    build_pane_tree(make_view, Content, Tree, First, Current),
    send(EP, append, First, @default, @off),
    get(First, container, emacs_tab, Tab),
    send(Tab, window_tree, Tree),
    (   Current == @default
    ->  true
    ;   send(Tab, current, Current)
    ).

make_view(view(Options), View) :-
    new(View, emacs_view),
    send(View, pane_term, Options).

expose_view_tab(EP, Built) :-
    (   member(built(Tab, Options), Built),
        memberchk(current(true), Options)
    ->  true
    ;   Built = [built(Tab, _)|_]
    ),
    get(Tab, current, View),
    View \== @nil,
    !,
    send(EP, current, View).
expose_view_tab(_, _).

:- pce_end_class(emacs_pane).


:- pce_begin_class(emacs_editor, editor, "Generic PceEmacs editor").

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
We use a pool of  modes  recorded  in   <-modes  to  avoid  the need for
destruction of mode objects during the livetime  of the editor.  This is
dangerous as the mode might still be `running' some command.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

variable(mode,            emacs_mode*,  get, "Mode of operation").
variable(modes,           chain,        get, "Modes part of this editor").

initialise(E, TB:[text_buffer], W:[int], H:[int]) :->
    send(E, send_super, initialise, TB, W, H),
    send(E, slot, modes, new(chain)).


unlink(E) :->
    "Unlink from mode object"::
    get(E, modes, Modes),
    send(Modes, for_all, message(@arg1, free)),
    send(E, send_super, unlink).

:- pce_global(@emacs_idle_timer, make_idle_timer).

make_idle_timer(T) :-
    new(T, timer(2)),
    send(T, message,
         new(Msg, message(T, send_hyper, editor, editor_idle_event))),
    send(Msg, debug_class, service).        % non-traceable


editor_idle_event(E) :->
    "Editor is idle"::
    get(E, mode, Mode),
    Mode \== @nil,
    send(Mode, has_send_method, idle),
    get(E, window, Window),
    get(Window, focus, @nil),       % only send event if no recognisers
    send(Mode, idle).               % are active


start_idle_timer(E, Interval:[real]) :->
    "Reset the idle timer to timeout after the specified time"::
    (   Interval == @default
    ->  get(E, mode, Mode),
        (   Mode \== @nil
        ->  get(Mode, idle_timeout, Time)
        ;   Time = 2
        )
    ;   Time = Interval
    ),
    send(@emacs_idle_timer, interval, Time),
    send(@emacs_idle_timer, status, once),
    send(@emacs_idle_timer, delete_hypers),
    new(_, hyper(@emacs_idle_timer, E, editor, idle_timer)).


typed(E, Id:'event|event_id') :->
    "Handle typing via mode"::
    send(E, start_idle_timer),
    get(E, mode, Mode),
    (   send(Mode, typed, Id, E),
        object(Mode)                % may disappear
    ->  send(Mode, highlight_matching_bracket)
    ;   true
    ).


caret(E, Caret:[int]) :->
    "Deal with idle-timing and ->new_caret_position"::
    get(E, caret, Old),
    send_super(E, caret, Caret),
    send(E, start_idle_timer),
    get(E, mode, Mode),
    (                               % Mode can be @nil
        send(Mode, has_send_method, new_caret_position)
    ->  (   Caret == @default
        ->  get(E, caret, NewCaret)
        ;   NewCaret = Caret
        ),
        (   Old \== NewCaret
        ->  send(Mode, new_caret_position, NewCaret)
        ;   true
        )
    ;   true
    ).


%       Whether the pointer entering a pane is enough to give it the
%       focus is one class variable of the frame -- see `pane_frame
%       ->focus_on_enter'.  PceEmacs used to answer that here, on its own,
%       so a terminal in the same window behaved differently.

event(E, Ev:event) :->
    (   send(Ev, is_a, area_enter)
    ->  get(E, frame, Frame),
        Frame \== @nil,
        send(Frame, has_send_method, focus_on_enter),
        ignore(send(Frame, focus_on_enter, E?window))
    ;   drop_target_event(E, Ev,
                          'Drop file(s) to edit in new tab',
                          pceemacs_open_drop)
    ->  true
    ;   send_super(E, event, Ev),
        send(@emacs, editor_event, Ev)
    ).

%!  pceemacs_open_drop(+Editor, +Paths) is det.
%
%   Drop-target callback: open each dropped file as a new tab.

pceemacs_open_drop(_Editor, Paths) :-
    forall(member(P, Paths),
           ignore(send(@emacs, open_file, P))).


paste(E, Which:[{primary,clipboard}]) :->
    send(E, start_idle_timer),
    send_super(E, paste, Which),
    send(E, highlight_matching_bracket).


highlight_matching_bracket(E) :->
    get(E, mode, Mode),
    (   send(Mode, highlight_matching_bracket)
    ->  true
    ;   true                        % avoid delegation
    ).


mode(E, ModeName:mode_name) :->
    "Associate argument mode"::
    get(E, mode, OldMode),
    (   get(OldMode, name, ModeName)
    ->  send(E, syntax, OldMode?syntax)
    ;   (   get(E?modes, find, @arg1?name == ModeName, Mode)
        ->  send(E, slot, mode, Mode)
        ;   send(E, slot, mode, ModeName),  % Converted to object
            get(E, mode, Mode),             % The object
            send(Mode, editor, E),
            send(E?modes, append, Mode)
        ),
        send(E?text_buffer, mode, ModeName),
        send(E, syntax, Mode?syntax),
        send(E, bindings, Mode?bindings),
        send(E, setup_mode),
        send(E?device, setup_mode),
        send(E, report, status, 'Switched to ``%s'''' mode', ModeName)
    ).


preview_drop(E, Obj:object*) :->
    "Delegate to mode"::
    get(E, mode, Mode),
    get(Mode?class, send_method, preview_drop, _), % avoid delegation
    send(Mode, preview_drop, Obj).


drop(E, Obj:object) :->
    "Delegate to mode"::
    get(E, mode, Mode),
    get(Mode?class, send_method, drop, _),         % avoid delegation
    send(Mode, drop, Obj).


selected_fragment(E, Fragment:fragment) :->
    "User selected a fragment in the margin"::
    send_super(E, selected_fragment, Fragment),
    get(E, mode, Mode),
    get(Mode?class, send_method, selected_fragment, _), % avoid delegation
    send(Mode, selected_fragment, Fragment).

hover_fragment_icon(E, Fragment:fragment*, Area:[area]) :->
    "User hovers the fragment icon"::
    send_super(E, hover_fragment_icon, Fragment, Area),
    get(E, mode, Mode),
    get(Mode?class, send_method, hover_fragment_icon, _), % avoid delegation
    send(Mode, hover_fragment_icon, Fragment, Area).

auto_fill(E, Caret:[int], Regex:[regex]) :->
    "Delegate to mode"::
    (   get(E, mode, Mode),
        get(Mode?class, send_method, auto_fill, _)
    ->  send(Mode, auto_fill, Caret, Regex)
    ;   send_super(E, auto_fill, Caret, Regex)
    ).


import_selection(E) :->
    "Import the (primary) selection"::
    get(E, display, Display),
    get(Display, selected_text, String),
    (   get(E, frame, Frame),
        send(Frame, has_get_method, prompter),
        get(Frame, prompter, TI),
        send(TI, instance_of, text_item)
    ->  send(TI, insert, @default, String)
    ;   send(E, insert, String)
    ).


catch_all(E, Selector:name, Args:unchecked ...) :->
    "Delegate to mode"::
    get(E, mode, Mode),
    get(Mode?class, send_method, Selector, _),
    send(@pce, last_error, @nil),
    Msg =.. [Selector|Args],
    send(Mode, Msg).


text_buffer(E, B:emacs_buffer) :->
    "Switch to indicated buffer"::
    get(E, text_buffer, Last),
    (   B == Last
    ->  true
    ;   send_super(E, text_buffer, B),
        get(B, mode, ModeName),
        send(E, mode, ModeName),
        send(B, update_label),
        send(E, report, status, ''),
        get(E, mode, Mode),
        send(Mode, new_buffer),
        (   send(Last, instance_of, emacs_buffer)
        ->  send(E, delete_hypers, last_buffer),
            new(_, hyper(E, Last, last_buffer, last_editor))
        ;   true
        )
    ).


last_buffer(E, TB:text_buffer) :<-
    "Text-buffer we came from (for default)"::
    get(E, hypered, last_buffer, TB).


                 /*******************************
                 *          UTILITIES           *
                 *******************************/

label(E, Label:name) :->
    "Delegate to view"::
    get(E, device, View),
    send(View, label, Label).

size(E, Size:size) :->
    "Set size, resizing the frame"::
    get(E, frame, Frame),
    get(Frame, size, size(FW,FH)),
    get(E?area, size, size(W0,H0)),
    send_super(E, size, Size),
    get(E?area, size, size(W1,H1)),
    FW1 is FW+W1-W0,
    FH1 is FH+H1-H0,
    send(Frame, size, size(FW1, FH1)).

font(E, Font:font) :->
    "Set font, resizing the frame"::
    get(E, frame, Frame),
    get(Frame, size, size(FW,FH)),
    get(E?area, size, size(W0,H0)),
    send_super(E, font, Font),
    get(E?area, size, size(W1,H1)),
    FW1 is FW+W1-W0,
    FH1 is FH+H1-H0,
    send(Frame, size, size(FW1, FH1)).

margin_width(E, Width:int) :->
    "Set margin width"::
    send_super(E, margin_width, Width),
    get(E, margin, Margin),
    (   Margin == @nil
    ->  true
    ;   IconW is Width - 6,  % margin using 3 pixels padding
        send(Margin, icon_size, size(IconW,IconW))
    ).

looking_at(E, Re:regex, Where:[int], End:[int]) :->
    "Test if regex macthes from the caret"::
    (   Where == @default
    ->  get(E, caret, C)
    ;   C = Where
    ),
    get(E, text_buffer, TB),
    send(Re, match, TB, C, End).

looking_at(E, Re:regex, Where:[int], End:[int], Len:int) :<-
    "Test if regex macthes from the caret"::
    (   Where == @default
    ->  get(E, caret, C)
    ;   C = Where
    ),
    get(E, text_buffer, TB),
    get(Re, match, TB, C, End, Len).


		 /*******************************
		 *            DABBREV		*
		 *******************************/

dabbrev_candidates(E, CMode:name, Target:char_array, Completions:chain) :<-
    get(E, mode, Mode),
    Mode \== @nil,
    send(Mode, has_get_method, dabbrev_candidates),
    get(Mode, dabbrev_candidates, CMode, Target, Completions).

:- pce_end_class.


                 /*******************************
                 *          EMACS MODES         *
                 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PceEmacs  modes  are  tricky  stuff.   We    like  to  have  comfortable
programming, which implies we should  represent   a  PceEmacs  mode as a
class.   This  provides  us  with  a   good  programming  interface  and
inheritance as well as all the other goodies of OO programming.

We also would like to be able to   switch PceEmacs windows from one mode
to another.  This conflicts, as instances  cannot be migrated from class
to class.  Therefore, PceEmacs modes are objects  that are attached to a
emacs_editor.  An emacs_mode delegates to the   editor  in this mode and
editors delegate to their mode (with an explicit method to avoid endless
loops  if  the   method   is   defined    on   neither).    The   method
`pce_editor->mode' attaches a mode to an editor.

Next, we would like users to be able  to extend these classes to provide
custom methods and possibly redefine methods.  This has been implemented
using tricky meta-programming: class emacs_mode_class   is a subclass of
class `class' (representing classes).   This   class  defines the method
->load_user_extensions, which is called when an instance of the class is
created.   The  classes  defining  emacs_modes  are  instances  of  this
emacs_mode_class class.  To understand this, try:

        ?- new(X, class(myclass, class)),
           new(Y, myclass(myobject, object)),
           new(Z, myobject).

and verify how the objects and classes are related.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


:- pce_begin_class(emacs_mode_class, class,
                   "Class for emacs modes").

variable(user_extensions_loaded, bool := @off, get,
         "Test if extensions are loaded").

load_user_extensions(C) :->
    "Load mode extensions from ~/lib/xpce/emacs/"::
    (   get(C, user_extensions_loaded, @on)
    ->  true
    ;   get(C, name, Name),
        (   atom_concat(emacs_, Base, Name)
        ->  true
        ;   Base = Name
        ),
        send(@emacs, load_user_extension, Base),
        send(C, slot, user_extensions_loaded, @on),
        get(C, super_class, Super),
        (   send(Super, has_send_method, load_user_extensions)
        ->  send(Super, load_user_extensions)
        ;   true
        )
    ).

:- pce_end_class(emacs_mode_class).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Finally, we have to tell pce_begin_class/3 the  meta-class we want to be
using.  If pce_begin_class/3 makes a subclass, it will make the subclass
of the same  meta-class  as  its   super-class.   Thus,  a  subclass  of
emacs_mode will be an instance   of  class(emacs_mode_class), instead of
class(class).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(emacs_mode_class:emacs_mode(name), object,
                   "Generic PceEmacs mode class").

variable(name,            name,         get,  "Name of the mode").
variable(syntax,          syntax_table, get,  "Syntax for this mode").
variable(bindings,        key_binding,  get,  "Key-binding table").
variable(editor,          editor*,      both, "Associated editor").
variable(m_x_history,     chain*,       both, "Current M-x command history").
variable(m_x_index,       int*,         both, "M-p/M-n current index").
variable(m_x_argn,        int*,         both, "M-p/M-n current argument").
variable(keep_selection,  bool := @off, both, "Keep selection for this method").
variable(idle_timeout,    num,		both, "Auto highlighting timeout").

class_variable(idle_timeout, num, 2).

delegate_to(editor).

                /********************************
                *         CREATE/REUSE          *
                ********************************/

initialise(M) :->
    "Create"::
    get(M, class_name, ClassName),
    mode_name(ClassName, Name),
    send(@mode_name_type?context, add, Name), % make sure
    send(M, send_super, initialise),
    send(M, slot, name, Name),
    send(M, slot, syntax, Name),              % converts to object
    send(M, load_user_extensions),
    send(M, bindings),
    send(M, obtain_class_variables).


icon(_, I:image) :<-
    "Return icon for mode"::
    new(I, image(resource(mode_x_icon))).


mode_name(emacs_mode, default) :- !.
mode_name(Mode, Name) :-
    (   atom_concat(emacs_, M1, Mode)
    ;   M1 = Mode
    ),
    !,
    (   atom_concat(Name, '_mode', M1)
    ;   Name = M1
    ),
    !.


table_name(ClassName, TableName) :-
    atom_concat(TableName, '_mode', ClassName).


new_buffer(M) :->
    "Called if a new buffer is attached to this mode"::
    (   get(M, frame, Frame),
        send(Frame, has_send_method, show_line_number)
    ->  send(Frame, show_line_number, @nil)
    ;   true
    ),
    send(M, new_caret_position, M?caret).


new_caret_position(M, Caret:int) :->
    "Called after any caret movement"::
    get(M, text_buffer, TB),
    get(TB, find_all_fragments,
        message(@arg1, overlap, Caret),
        Fragments),
    (   send(Fragments, empty)
    ->  send(M, report, status, '')
    ;   send(Fragments, for_some,
             message(M, in_fragment, @arg1))
    ).


highlight_matching_bracket(_M, _At:[int]) :->
    "Virtual.  Statically highlight bracket matching caret"::
    true.


in_fragment(M, Fragment:fragment) :->
    "Called after a caret movement brings the caret in fragment"::
    (   send(Fragment, has_send_method, identify)
    ->  send(Fragment, identify)
    ;   (   get(Fragment, attribute, message, Message)
        ->  get(Fragment, style, StyleName),
            send(M, report, status, '%s: %s', StyleName, Message)
        ;   true
        )
    ).


bindings(M) :->
    "Associate key_binding table"::
    get(M, class_name, ClassName),
    binding_name(ClassName, Name),
    get(@key_bindings, member, Name, Table),
    !,
    send(M, slot, bindings, Table).


syntax(M) :->
    "Associate syntax table"::
    get(M, class_name, ClassName),
    binding_name(ClassName, Name),
    get(@syntax_tables, member, Name, Table),
    !,
    send(M, slot, syntax, Table).


binding_name(ClassName, Name) :-
    table_name(ClassName, Name).
binding_name(ClassName, Name) :-
    mode_name(ClassName, ModeName),
    key_binding_name(ModeName, Name).
binding_name(ClassName, Name) :-
    get(@pce, convert, ClassName, class, Class),
    get(Class, super_class, Super),
    send(Super, is_a, emacs_mode),
    get(Super, name, SuperName),
    binding_name(SuperName, Name).


convert(_, Name:name, Mode:emacs_mode) :<-
    "Convert name into a mode object"::
    atomic_list_concat([emacs_, Name, '_mode'], ModeClassName),
    get(@pce, convert, ModeClassName, class, _), % fail silently
    new(Mode, ModeClassName).


                 /*******************************
                 *         THE MODE MENU        *
                 *******************************/

%       @emacs_mode is the mode of the editor the user is working in.  It
%       used to be `@event?window?(mode)', which only holds while the
%       event came from the editor itself: a menu item is chosen in the
%       menu bar or in a popup, so the window it arrives on has no <-mode
%       and the whole menu entry failed -- that is what left the
%       pullrights such as File -> Switch to buffer empty.  The editor is
%       reached through the frame instead.

:- pce_global(@emacs_mode, new(?(@prolog, emacs_current_mode))).

:- public emacs_current_mode/1.

%!  emacs_current_mode(-Mode) is semidet.
%
%   The emacs_mode the user is working in: the mode of the window the
%   event came from if that is an editor, else the mode of the pane its
%   frame is showing, else that of whichever PceEmacs window is current.
%   Fails when they are not in an editor at all, which is what leaves a
%   menu item that asks for it inactive.

emacs_current_mode(Mode) :-
    send(@event, instance_of, event),
    get(@event, window, Window),
    Window \== @nil,
    (   window_mode(Window, Mode)
    ->  true
    ;   get(Window, frame, Frame),
        Frame \== @nil,
        frame_mode(Frame, Mode)
    ),
    !.
emacs_current_mode(Mode) :-
    get(@emacs, current_frame, Frame),
    frame_mode(Frame, Mode).

window_mode(Window, Mode) :-
    send(Window, has_get_method, mode),
    get(Window, mode, Mode),
    send(Mode, instance_of, emacs_mode).

frame_mode(Frame, Mode) :-
    send(Frame, has_get_method, current_pane),
    get(Frame, current_pane, Pane),
    window_mode(Pane, Mode).

mode_menu(M, MM:emacs_mode_menu) :<-
    "Return the mode-menu structure for this mode"::
    get(M, class_name, ClassName),
    mode_menu_name(ClassName, Name),
    get(@emacs_mode_menus, member, Name, MM),
    !.


mode_menu_name(ClassName, Name) :-
    mode_name(ClassName, Name).
mode_menu_name(ClassName, Name) :-
    get(@pce, convert, ClassName, class, Class),
    get(Class, super_class, Super),
    send(Super, is_a, emacs_mode),
    get(Super, name, SuperName),
    mode_menu_name(SuperName, Name).

                 /*******************************
                 *            HISTORY           *
                 *******************************/

%:- pce_global(@c_method, new(var)).    % debugging

open_history(M, Impl:behaviour, Force:[bool]) :->
    "(Initialise) history for behaviour"::
    (   (Force == @on ; get(M, m_x_history, @nil))
    ->  (   get(Impl, attribute, emacs_history, History)
        ->  true
        ;   send(Impl, attribute, emacs_history, new(History, chain))
        ),
%       send(@c_method, assign, Impl, global),
        send(M, m_x_history, History),
        send(M, m_x_index, @nil)
    ;   true
    ).


close_history(M, Argv:[vector]) :->
    "Close open history adding Argv"::
    get(M, m_x_history, History),
    (   Argv \== @default,
        History \== @nil
    ->  get(Argv, copy, Save),
        clean_argv(Save),
        (   get(History, find,
                message(@prolog, same_argv, Save, @arg1),
                Old)
        ->  send(History, move_after, Old)
        ;   send(History, prepend, Argv)
        )
    ;   true
    ),
    send(M, m_x_history, @nil),
    send(M, m_x_index, @nil).


same_argv(V1, V2) :-
    get(V1, size, S1),
    get(V2, size, S1),
    forall(between(1, S1, N),
           (   get(V1, element, N, E1), get(E1, print_name, P1),
               get(V2, element, N, E2), get(E2, print_name, P2),
               send(P1, equal, P2)
           )).


%       clean_argv(+Vector)
%       Replace vector elements with their written version to avoid the
%       risk of illegal-object references.

clean_argv(Vector) :-
    get(Vector, low_index, Low),
    get(Vector, high_index, High),
    (   between(Low, High, Index),
        get(Vector, element, Index, E),
        object(E),
        get(E, protect, @off),
        send(Vector, element, Index, E),
        fail
    ;   true
    ).


noarg_call(M, Selector:name, Times:[int]) :->
    "Invoke method without arguments (prompt)"::
    (   get(M, send_method, Selector, tuple(_, Impl))
    ->  send(@current_emacs_mode, assign, M),
        send(M, open_history, Impl, @on),
        get(M, interactive_arguments, Impl, Times, Argv),
        send(M, report, status, ''),
        send(M, close_history, Argv),
        (   send(M, send_vector, Selector, Argv)
        ->  (   object(M)
            ->  send(M, mark_undo)
            ;   true
            )
        ;   (   object(M)
            ->  send(M, report, status, '%s command failed', Selector)
            ;   true
            )
        )
    ;   send(M, report, error, 'No implementation for ``%s''''', Selector)
    ).


arg_call(M, Selector:name, Arg:any) :->
    "Invoke method from pullright menu"::
    send(M, Selector, Arg).


                 /*******************************
                 *           SETUP              *
                 *******************************/

setup_mode(_M) :->
    "Initialise mode (virtual)"::
    true.


load_user_extensions(M) :->
    "Load mode extensions from ~/lib/xpce/emacs/"::
    send(M?class, load_user_extensions).


                /********************************
                *           TYPING              *
                ********************************/

typed(M, Id:'event|event_id', Editor:editor) :->
    "Handle typed character for editor"::
    get(M, text_buffer, TB),
    send(TB, check_auto_save),

                                    % send to mode rather than editor
    (   get(M, focus_function, F), F \== @nil
    ->  (   send(M, F, Id)
        ->  true
        ;   send(M, focus_function, @nil),
            send(M, typed, Id, Editor)        % failed: unfocus and resent
        )
    ;   get(M, bindings, Binding),
        send(Binding, typed, Id, M)
    ).


                 /*******************************
                 *           SELECTION          *
                 *******************************/

default(_E, _Type:type, _Default:unchecked) :<-
    "[virtual] Provide default for prompting"::
    fail.


                 /*******************************
                 *           EVENTS             *
                 *******************************/

event(M, Ev:event) :->
    "Allow for gestures to be appended to modes"::
    (   get(M, all_recognisers, Chain),
            get(Chain, find,
                message(@arg1, event, Ev), _)
    ;   send(Ev, is_popup),         % show fragment popup (if any)
        get(M?text_image, index, Caret),
        get(M?text_buffer, find_all_fragments,
            message(@arg1, overlap, Caret),
            Fragments),
        send(Fragments, sort, ?(@arg1?length, compare, @arg2?length)),
        get(Fragments, find, message(@arg1, has_get_method, popup), F),
        get(F, popup, P), P \== @nil,
        new(G, popup_gesture(P)),
        send(G, context, F),        % make fragment available as @arg1
        send(G, event, Ev)
    ).


                 /*******************************
                 *        FIX DELEGATION        *
                 *******************************/

file(M, File:file*) :<-
    "Return associated file"::
    get(M?text_buffer, file, File).


                 /*******************************
                 *      PROMPTING ARGUMENTS     *
                 *******************************/

prompt(M, Label:char_array, Default:[any], Type:[type], Rval:any) :<-
    "Prompt for a value in the mini-window"::
    get(M?window, prompt, Label, Default, Type, Rval).


prompt_using(M, Item:graphical, Rval:any) :<-
    "Prompt using dialog item in the mini-window"::
    get(M?window, prompt_using, Item, Rval).


interactive_arguments(M, Implementation:any, Times:[int], Argv:vector) :<-
    "Prompt for arguments for the given implementation"::
    get(M, window, View),
    get(View, interactive_arguments, Implementation, Times, Argv).


interactive_argument(M, Implementation:any, Which:int, Value:any) :<-
    "Prompt for interactive argument of specified type"::
    send(M, open_history, Implementation),
    get(M, window, EmacsWindow),
    get(Implementation, argument_type, Which, Type),
    send(M, m_x_argn, Which),
    get(M, m_x_history, History),
    (   get(M, m_x_index, Idx), Idx \== @nil % use M-x History!
    ->  get(History, nth1, Idx, HistArgv),
        get(HistArgv, element, Which, DefaultValue)
    ->  true
    ;   get(Implementation, name, ImplName),
        \+ send(ImplName, suffix, selection), % not on *_selection
        get(M, selected, Selection),
        get(Type, check, Selection, DefaultValue)
    ->  true
    ;   DefaultValue = @default
    ),
    (   get(Type, argument_name, Label), Label \== @nil
    ->  true
    ;   get(Type, name, Label)
    ),
    (   (   History == @nil
        ;   send(History, empty)
        )
    ->  ValueSet = @default
    ;   get(History, map, ?(@arg1, element, Which), ValueSet)
    ),
    get(EmacsWindow, prompt, Label, DefaultValue, Type, ValueSet, Value).


m_x_previous(M, Value:any) :<-
    "Read next value from the M-x history"::
    get(M, m_x_index, Idx),
    (   Idx == @nil
    ->  Nidx = 1
    ;   Nidx is Idx + 1
    ),
    (   get(M, m_x_history, H), H \== @nil,
        get(H, nth1, Nidx, ArgVector)
    ->  get(ArgVector, element, M?m_x_argn, Value),
        send(M, m_x_index, Nidx)
    ;   send(M, report, warning, 'No (more) history'),
        fail
    ).


m_x_next(M, Value:any) :<-
    "Read previous value from the M-x history"::
    get(M, m_x_index, Idx),
    (   (Idx == @nil ; Idx =< 1)
    ->  send(M, report, warning, 'Back at start'),
        fail
    ;   Nidx is Idx - 1
    ),
    get(M?m_x_history, nth1, Nidx, ArgVector),
    get(ArgVector, element, M?m_x_argn, Value),
    send(M, m_x_index, Nidx).


                 /*******************************
                 *       LOCATION HISTORY       *
                 *******************************/

location_history(M, Start:start=[int], Len:length=[int],
                 Always:always=[bool], Title:title=[char_array]) :->
    "Add location to the editor history"::
    (   Start == @default
    ->  get(M, caret, Caret),
        get(M, scan, Caret, line, 0, start, SOF)
    ;   SOF = Start
    ),
    (   Always \== @on,
        send(M, history_not_interesting, SOF)
    ->  true
    ;   get(M, text_buffer, TB),
        new(_, emacs_history_fragment(TB, SOF, Len, Title))
    ).

history_not_interesting(M, Start:int) :->
    "True if Start is close to the recent history mark"::
    (   Start == 0
    ;   send(M, history_close_to_last, Start)
    ),
    !.

history_close_to_last(M, Start:int, MaxDist:[int]) :->
    "True if Start is within MaxDist lines from last"::
    default(MaxDist, 10, MD),
    get(M, text_buffer, TB),
    get(@emacs?history, current, Current),
    get(Current, get_hyper, fragment, text_buffer, TB),
    get(Current, get_hyper, fragment, start, StartOfCurrent),
    (   Start < StartOfCurrent
    ->  get(TB, count_lines, Start, StartOfCurrent, Lines)
    ;   Start > StartOfCurrent
    ->  get(TB, count_lines, StartOfCurrent, Start, Lines)
    ;   Lines = 0
    ),
    Lines < MD.


                 /*******************************
                 *            REPORT            *
                 *******************************/

report_to(M, E:editor) :<-
    "Send reports to the <-editor"::
    get(M, editor, E).


                /*******************************
                *       LSP INTEGRATION        *
                *******************************/

lsp_client(_M, _Client) :<-
    "LSP client associated to this mode (virtual)"::
    fail.

:- pce_end_class.


:- pce_begin_class(emacs_mode_menu(name), sheet).

:- pce_global(@emacs_mode_menus, new(hash_table)). % name ---> mode_menu object

variable(name,          name,           get,    "Name of the menu").

initialise(MM, Name:name, Super:[emacs_mode_menu]) :->
    "Create from name and super (default) menu"::
    send(MM, send_super, initialise),
    send(MM, slot, name, Name),
    send(@emacs_mode_menus, append, Name, MM),
    send(MM, protect),
    (   Super \== @default
    ->  send(Super, for_all,
             message(MM, value, @arg1?name, @arg1?value?copy))
    ;   true
    ).


lookup(_, Name:name, _Super:[emacs_mode_menu], MM) :<-
    "Reuse existing mode menu"::
    get(@emacs_mode_menus, member, Name, MM).


convert(_, Name:name, MM:emacs_mode_menu) :<-
    "Convert name to mode-menu object"::
    (   get(@emacs_mode_menus, member, Name, MM)
    ->  true
    ;   get(@pce, convert, string('emacs_%s_mode', Name), class, Class),
        get(Class, name, ClassName),
        mode_menu_name(ClassName, MenuName),
        get(@emacs_mode_menus, member, MenuName, MM)
    ).


action_name(Name, Name) :-
    atom(Name),                     % an object reference is atomic too
    !.
action_name(MenuItem, Name) :-
    get(MenuItem?value?print_name, value, Name).

locate_action(Chain, Action, Cell) :-
    action_name(Action, Name),
    get(Chain, find, message(@prolog, action_name, @arg1, Name), Cell).

append(MM, Name:name, Action:'name|menu_item', Before:[name]) :->
    "Append item for specified menu"::
    (   get(MM, value, Name, Chain)
    ->  true
    ;   send(MM, value, Name, new(Chain, chain))
    ),
    (   Action == -
    ->  send(Chain, append, Action)
    ;   locate_action(Chain, Action, Old)
    ->  send(Chain, replace, Old, Action)
    ;   send(Chain, append, Action),
        (   Before \== @default,
            locate_action(Chain, Before, CellValue),
            send(Chain, move_before, Action, CellValue)
        ->  true
        ;   true
        )
    ).

delete(MM, Menu:name, Action:name) :->
    "Delete item from inherited menu"::
    (   get(MM, value, Menu, Chain),
        send(Chain, delete, Action)
    ->  true
    ;   true
    ).

:- pce_end_class.


                 /*******************************
                 *         KEY-BINDINGS         *
                 *******************************/

:- pce_begin_class(emacs_key_binding, key_binding,
                   "Specialised key_binding for history").

key_binding_name(@default, @default) :- !.
key_binding_name(@nil, @nil) :- !.
key_binding_name(KB, Name) :-
    object(KB),
    get(KB, name, KBName),
    !,
    key_binding_name(KBName, Name).
key_binding_name(editor, editor) :- !.
key_binding_name(X, Internal) :-
    atom_concat('emacs$', X, Internal).


initialise(KB, Name:[name]*, Super:[key_binding]) :->
    default(Super, editor, S),
    key_binding_name(Name, IName),
    key_binding_name(S, IS),
    send(KB, send_super, initialise, IName, IS).

convert(_, IName:name, KB:emacs_key_binding) :<-
    "Handle mapped names"::
    key_binding_name(Name, IName),
    get(type(key_binding), convert, Name, KB).

lookup(_, Name:name, _Super:[key_binding], KB:emacs_key_binding) :<-
    "Find existing mappings"::
    key_binding_name(Name, IName),
    get(@key_bindings, member, IName, KB).

:- pce_group(execute).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
These methods define  argument-filling  and   execution  from  arbitrary
methods from a keyboard command.  As  we   want  to  do  fairly advanced
prompting we need to do redefine some things from class key_binding.

First we check whether there is  need   for  prompting.  If so, we check
whether we use the miniwindow or  not.   The  miniwindow prompts for one
argument  at  a  time.  The  system  ->fill_arguments_and_execute  calls
<-interactive_argument on Receiver for each missing argument.

If we are in prompt mode we simply   use  ->noarg_call on the mode, just
like commands comming from the menus.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

fill_arguments_and_execute(KB, EvId:event_id, Receiver:emacs_mode,
                           Selector:name, Argv:any ...) :->
    "Open/close the argument processing"::
     Message =.. [ fill_arguments_and_execute,
                   EvId, Receiver, Selector | Argv],

    (   get(Receiver, send_method, Selector, tuple(_, Impl))
    ->  (   length(Argv, Before),
            First is Before + 1,
            args_available(First, Impl, KB, EvId)
        ->  send_super(KB, Message)                 % no need to prompt
        ;   get(Receiver, frame, Frame),
            send(Frame, has_get_method, prompt_style),
            get(Frame, prompt_style, mini_window)
        ->  send(@current_emacs_mode, assign, Receiver),
            send(Receiver, open_history, Impl, @on),
            send_super(KB, Message)                 % miniwindow
        ;   send(Receiver, noarg_call, Selector)    % prompting
        )
    ;   send_super(KB, Message)                     % generate error
    ).

%       args_available(+I, +Implementation, +KeyBinding, +EventId)
%
%       See whether all arguments are around that allow us to execute
%       the command without prompting.  See the implementation of
%       `key_binding->fill_arguments_and_execute' for reference.

args_available(N, Impl, KB, EvId) :-
    get(Impl, argument_type, N, ArgType),
    !,
    (   send(ArgType, includes, event_id)
    ;   send(ArgType, includes, char),
        integer(EvId)
    ;   send(ArgType, includes, int),
        get(KB, argument, Arg),
        integer(Arg)
    ;   send(ArgType, includes, default)
    ),
    !,
    NN is N + 1,
    args_available(NN, Impl, KB, EvId).
args_available(_, _, _, _).

execute(KB, Receiver:emacs_mode, Selector:name, Argv:any ...) :->
    "Push history if available"::
    (   get(Receiver, m_x_history, @nil)
    ->  true
    ;   VectorTerm =.. [code_vector|Argv], % do not create references!
        send(Receiver, close_history, new(VectorTerm))
    ),
    Message =.. [execute, Receiver, Selector | Argv],
    send_super(KB, Message).

:- pce_end_class.


                 /*******************************
                 *         ARGUMENT ITEM        *
                 *******************************/

:- pce_begin_class(emacs_argument_item, menu_item,
                   "Item with pullright for args").

initialise(I, Name:name, ValueSet:'chain|function') :->
    send(I, send_super, initialise, Name),
    send(I, popup,
         new(P, emacs_argument_popup(Name,
                                     message(@emacs_mode, arg_call,
                                             Name, @arg1)))),
    (   send(ValueSet, '_instance_of', chain)
    ->  send(P, members, ValueSet)
    ;   send(P, update_message,
             message(@receiver, members, ValueSet))
    ).

:- pce_end_class.


:- pce_begin_class(emacs_argument_popup, popup,
                   "Emacs mode menu pullright popup").

members(I, Members:chain) :->
    "->clear and attach new members (do not capitalise)"::
    get(Members, map,
        create(menu_item, @arg1, @default, @arg1?print_name),
        Items),
    send(I, send_super, members, Items).

:- pce_end_class.
