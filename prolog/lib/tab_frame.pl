/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
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

:- module(tab_frame, []).
:- use_module(library(pce)).
:- use_module(library(pce_util), [default/3, chain_list/2]).
:- use_module(library(dragdrop), []).
:- use_module(library(help_message), []).
:- use_module(library(pce_icon_button), []).
:- use_module(library(lists), [member/2]).
:- use_module(library(debug), [debug/3]).

/** <module> Tab holding a tiled hierarchy of windows

A tab_frame is a tab that manages several   windows using a tile, just as
class frame does for its members.  It   is  the building block for a user
interface that combines tabs with window   splitting:  each tab holds one
or more windows, arranged left/right and above/below, with the separating
gaps draggable to redistribute the space.

Where class window_tab (see library(tabbed_window))   holds exactly _one_
window and resizes it by hand, a  tab_frame   keeps  the tiles alive: the
windows in it are related through the same class tile hierarchy the frame
uses, and ->layout runs `tile ->layout' over the tab's content area.

Simple example:

```
test :-
    new(TW, tabbed_window('Tiles in tabs')),
    send(TW, tab, new(TF, tab_frame(new(P1, picture), one))),
    send(P1, display, box(50,50), point(20,20)),
    send(TF, split, new(P2, picture), P1, vertically),
    send(P2, display, circle(40), point(20,20)),
    send(TF, split, new(P3, picture), P2, horizontally),
    send(P3, display, ellipse(60,30), point(20,20)),
    send(TW, tab, tab_frame(new(picture), two)),
    send(TW, open).
```

@see library(tabbed_window) for the tab_stack wrapper.
*/

:- pce_begin_class(tab_frame, tab,
                   "Tab holding a tiled hierarchy of windows").

variable(current,    window*,     get,  "Window that has the focus").
variable(separators, chain,       get,  "Lines drawn between the tiles").
variable(closing,    bool := @off, get, "I am being destroyed").
variable(drop_feedback, box*,     get, "Outline of the drop that would happen").
variable(sizing,     bool := @off, none, "A size is being imposed on me").
variable(window_label, name*,    get,  "Title a window in me asked for").

class_variable(horizontal_resize_cursor, cursor, ew_resize,
               "Cursor for horizontally resizing a tile").
class_variable(vertical_resize_cursor,   cursor, ns_resize,
               "Cursor for vertically resizing a tile").
class_variable(tile_border_root,         int,     0,
               "Border around the tile hierarchy (0: out to my edges)").
class_variable(split_bias,               num,     2,
               "Weight of the top and bottom drop zones (>1: wider sides)").
class_variable(separator_colour,         colour*, @nil,
               "Colour of the tile separators (@nil: <-foreground)").

:- pce_global(@tab_frame_resize_gesture, new(tile_resize_gesture)).

initialise(TF, Window:window=[window], Name:name=[name]) :->
    "Create from a window and a name"::
    (   Window == @default
    ->  new(W, picture)
    ;   W = Window
    ),
    (   Name == @default
    ->  get(W, name, TheName)
    ;   TheName = Name
    ),
    send_super(TF, initialise, TheName),
    send(TF, border, size(0,0)),
    send(TF, slot, separators, new(chain)),
    send(TF, recogniser, @tab_frame_resize_gesture),
    send(TF, append, W).

unlink(TF) :->
    "Tell my <-device if I was the last tab"::
    send(TF, slot, closing, @on),
    get(TF, device, Dev),
    (   Dev \== @nil,
        get(Dev?tabs, size, 1),
        get(TF, container, tabbed_window, TabbedWindow)
    ->  send_super(TF, unlink),
        send(TabbedWindow, empty)
    ;   send_super(TF, unlink)
    ).

                 /*******************************
                 *            MEMBERS           *
                 *******************************/

append(TF, Window:window=window,
           Relative:relative_to=[window],
           Where:where=[{above,below,left,right}]) :->
    "Add a window, optionally next to an existing one"::
    send(Window, '_compute_desired_size'),
    decoration(Window, Decor),
    send(Decor, lock_object, @on),
    (   get(Decor, tile_manager, Manager)
    ->  send(Manager, detach_window, Decor)
    ;   true
    ),
    (   get(TF, tile, _)                   % I already hold windows
    ->  relative_window(TF, Relative, Rel),
        decoration(Rel, RelDecor),
        get(RelDecor, tile, RelTile),
        get(Decor, tile, NewTile),
        default(Where, below, TheWhere),
        send(NewTile, TheWhere, RelTile, @off)
    ;   true
    ),
    send(TF, attach_window, Decor),
    get(Decor, unlock, _).

%       ->attach_window and ->detach_window are my half of the protocol
%       that lets `window ->below' and friends work on a tab_frame.  See
%       tile <-manager; class frame implements the same two methods.

attach_window(TF, Window:window) :->
    "Take Window into my tile hierarchy"::
    decoration(Window, Decor),
    send_super(TF, display, Decor),
    send(TF?tile, manager, TF),
    send(TF, update_current),
    send(TF, layout).

detach_window(TF, Window:window) :->
    "Release Window from my tile hierarchy"::
    decoration(Window, Decor),
    send(TF, erase, Decor).

split(TF, Window:window=window,
          Relative:relative_to=[window],
          Direction:direction=[{horizontally,vertically}]) :->
    "Add Window by splitting Relative"::
    default(Direction, horizontally, Dir),
    (   Dir == horizontally
    ->  Where = below                      % new window below: Terminator
    ;   Where = right                      % compatible naming
    ),
    send(TF, append, Window, Relative, Where).

delete(TF, Window:window) :->
    "Remove a window without destroying it"::
    decoration(Window, Decor),
    send(TF, erase, Decor).

erase(TF, Gr:graphical) :->
    "Remove Gr; keep the tile hierarchy consistent"::
    (   send(Gr, instance_of, window),
        get(Gr, slot, tile, Tile),
        Tile \== @nil
    ->  send(Tile, unrelate),
        send_super(TF, erase, Gr),
        (   get(TF, slot, closing, @on)
        ->  true
        ;   send(TF, update_current),
            (   get(TF, tile, _)
            ->  send(TF, layout)
            ;   send(TF, empty)
            )
        )
    ;   send_super(TF, erase, Gr)
    ).

empty(TF) :->
    "Called if the last window disappeared"::
    send(TF, destroy).

close_other_tabs(TF) :->
    "Destroy all tabs except for me"::
    get(TF, device, Stack),
    send(Stack?graphicals, for_all,
         if(@arg1 \== TF,
            message(@arg1, slot, closing, @on))),
    send(Stack?graphicals, for_all,
         if(@arg1 \== TF,
            message(@arg1, destroy))).

untab(TF) :->
    "Move <-current out into a frame of its own"::
    get(TF, current, Window),
    get(TF, display_position, point(X, Y)),
    get(TF, container, tabbed_window, TabbedWindow),
    get(TabbedWindow, frame_window, Window, Window?name, 1, Frame),
    send(Frame, open, point(X, Y+20)).

windows(TF, Windows:chain) :<-
    "New chain holding my windows"::
    window_list(TF, List),
    new(Windows, chain),
    forall(member(W, List), send(Windows, append, W)).

window(TF, Window:window) :<-
    "The window that has the focus (see <-current)"::
    get(TF, current, Window).

%       <-current answers a window I really hold: the one that has the
%       keyboard focus if that is one of mine, else the one last made
%       current if it is still there, else my first.  Validating on the
%       way out keeps a destroyed window from being handed out: erasing
%       one is not always something we are told about directly, as it is
%       the window_decorator that is my graphical.

current(TF, Window:window) :<-
    "Window holding the keyboard focus"::
    window_list(TF, List),
    (   get(TF, frame, Frame),
        Frame \== @nil,
        get(Frame, keyboard_focus, KF),
        KF \== @nil,
        memberchk_eq(KF, List)
    ->  Window = KF
    ;   get(TF, slot, current, W),
        W \== @nil,
        memberchk_eq(W, List)
    ->  Window = W
    ;   List = [Window|_]
    ).

current(TF, Window:window) :->
    "Make Window the current one"::
    send(TF, slot, current, Window),
    (   get(TF, frame, Frame),
        Frame \== @nil
    ->  send(Frame, keyboard_focus, Window)
    ;   true
    ).

update_current(TF) :->
    "Keep <-current on a window I still hold"::
    window_list(TF, List),
    (   get(TF, slot, current, Cur),
        Cur \== @nil,
        memberchk_eq(Cur, List)
    ->  true
    ;   List = [W|_]
    ->  send(TF, current, W)            % ->current, so that the focus and
    ;   send(TF, slot, current, @nil)   % whatever follows it come along
    ).

                 /*******************************
                 *            LAYOUT            *
                 *******************************/

tile(TF, Tile:tile) :<-
    "Root tile managing my windows"::
    get(TF?graphicals, find,
        message(@arg1, instance_of, window), W),
    get(W, tile, T0),
    get(T0, root, Tile).

size(TF, Size:size) :->
    "Resize the tab and re-layout my windows"::
    send(TF, slot, sizing, @on),
    ignore(send_super(TF, size, Size)),
    send(TF, slot, sizing, @off),
    send(TF, layout).

%       ->layout_dialog is asked twice over: by the tab stack to find out
%       how big the tabs would like to be, and again from ->size once it
%       has told them.  The windows are placed by the tile either way; it
%       is whose size the tile is laid out at that differs.
%
%       ->size rather than the slot, and ->compute after it: the tab stack
%       reads the <-area of a tab to find out how big it asked to be, and
%       that only follows <-size once the tab has been recomputed.

layout_dialog(TF) :->
    "My windows are placed by <-tile, not by dialog layout"::
    (   get(TF, slot, sizing, @on)
    ->  send(TF, layout)
    ;   send(TF, layout_natural)
    ).

layout_natural(TF) :->
    "Take my size from what my tile asks for"::
    (   get(TF, root_tile, Tile)
    ->  send(Tile, enforce, @off),      % so that asking a window for its
        send(Tile, for_all,             % size sets the ideal rather than
             message(@arg1, '_compute_desired_size')),  % freezing the row
        send(Tile, enforce, @on),
        get(Tile, border_root, Border),
        get(Tile, ideal_width, IW),
        get(Tile, ideal_height, IH),
        W is IW+2*Border,
        H is IH+2*Border,
        send(TF, size, size(W, H)),
        send(TF, compute),
        debug(tab_frame, 'natural size ~wx~w', [W, H])
    ;   true
    ).

layout(TF) :->
    "Distribute my area over my windows"::
    get(TF, content_size, size(W, H)),
    (   W > 0, H > 0,
        get(TF, root_tile, Tile)
    ->  send(Tile, layout, 0, 0, W, H),
        send(TF, update_separators)
    ;   true
    ).

root_tile(TF, Tile:tile) :<-
    "My tile hierarchy, holding the outer border I lay it out with"::
    get(TF, tile, Tile),
    get(TF, class_variable_value, tile_border_root, Border),
    send(Tile, border_root, Border).

content_size(TF, Size:size) :<-
    "Size available for the windows"::
    (   get(TF, size, size(W,H))
    ->  true
    ;   get(TF, area, area(_,_,W,H0)),
        get(TF, label_height, LH),
        H is H0-LH
    ),
    new(Size, size(W,H)).

                 /*******************************
                 *          SEPARATORS          *
                 *******************************/

update_separators(TF) :->
    "Draw a line in each resizable gap"::
    get(TF, separators, Lines),
    chain_list(Lines, Old),                % clear first: destroying a line
    send(Lines, clear),                    % takes it out of the chain
    forall(member(L, Old), send(L, destroy)),
    (   get(TF, tile, Tile),
        get(Tile, resize_areas, Areas)
    ->  send(Areas, for_all, message(TF, separator, @arg1))
    ;   true
    ).

separator(TF, A:area) :->
    "Display a separator line in the gap A"::
    get(A, position, point(X, Y)),
    get(A, size, size(W, H)),
    (   W < H
    ->  XM is X + W//2,
        new(L, line(XM, Y, XM, Y+H))
    ;   YM is Y + H//2,
        new(L, line(X, YM, X+W, YM))
    ),
    (   get(TF, class_variable_value, separator_colour, C),
        C \== @nil
    ->  send(L, colour, C)
    ;   true
    ),
    send(TF, display, L),
    send(TF?separators, append, L).

                 /*******************************
                 *          DRAG & DROP         *
                 *******************************/

%       A window is moved by dragging its split_handle onto another one.
%       The half of the receiver the pointer is nearest says where the
%       dropped window goes, and the receiver is split to make room.  The
%       drop is answered here rather than by the windows: the tile is
%       what has to be rearranged, and this way it works for a window of
%       any class.

drop(TF, Window:window, Pos:point) :->
    "Put Window beside the window Pos is over"::
    send(TF, preview_drop, @nil),
    (   drop_zone(TF, Pos, Target, Where),
        Target \== Window
    ->  send(TF, append, Window, Target, Where),
        send(TF, current, Window)
    ;   true
    ).

preview_drop(TF, Window:window*, Pos:[point]) :->
    "Outline the half of the receiver Window would take"::
    send(TF, clear_drop_feedback),
    (   Window \== @nil,
        Pos \== @default,
        drop_zone(TF, Pos, Target, Where),
        Target \== Window
    ->  send(TF, drop_feedback, Target, Where)
    ;   true
    ).

drop_target(TF, Pos:point, Target:window) :<-
    "The window Pos is over"::
    drop_zone(TF, Pos, Target, _).

drop_side(TF, Pos:point, Where:{above,below,left,right}) :<-
    "The side of the receiver a drop at Pos would take"::
    drop_zone(TF, Pos, _, Where).

drop_feedback(TF, Target:window, Where:{above,below,left,right}) :->
    "Show what a drop on Target would do"::
    get(Target, size, size(W, H)),
    feedback_area(Where, W, H, X, Y, FW, FH),
    new(Box, box(FW, FH)),
    send(Box, pen, 0),
    send(Box, fill, colour(@default, 80, 130, 200)),
    send(Box, opacity, 0.3),
    send(Target, display, Box, point(X, Y)),
    send(TF, slot, drop_feedback, Box).

clear_drop_feedback(TF) :->
    "Take the outline away"::
    (   get(TF, slot, drop_feedback, Box),
        Box \== @nil
    ->  send(TF, slot, drop_feedback, @nil),
        free(Box)
    ;   true
    ).

                 /*******************************
                 *            EVENT             *
                 *******************************/

event(TF, Ev:event) :->
    "Update the resize cursor and run the gesture"::
    (   send(Ev, is_a, loc_move)
    ->  ignore(send(TF, update_cursor, Ev))
    ;   true
    ),
    send_super(TF, event, Ev).

resize_tile(TF, Ev:event, Tile:tile) :<-
    "Tile left of/above the gap Ev is in"::
    get(TF, tile, Root),
    get(Ev, position, TF, Pos),
    get(Root, sub_tile_to_resize, Pos, Tile).

update_cursor(TF, Ev:event) :->
    "Show a resize cursor while over a gap"::
    (   get(TF, resize_tile, Ev, Tile),
        get(Tile?super, orientation, Orientation)
    ->  resize_cursor_variable(Orientation, Var),
        get(TF, class_variable_value, Var, Cursor)
    ;   Cursor = @nil
    ),
    (   get(TF, cursor, Cursor)
    ->  true
    ;   send(TF, cursor, Cursor)
    ).

resize_cursor_variable(vertical,   vertical_resize_cursor).
resize_cursor_variable(horizontal, horizontal_resize_cursor).

                 /*******************************
                 *            STATUS            *
                 *******************************/

status(TF, Status:{on_top,hidden}) :->
    "Tell my container I became the current tab"::
    send_super(TF, status, Status),
    (   Status == on_top,
        get(TF, is_displayed, @on),
        get(TF, container, tabbed_window, TabbedWindow),
        get(TF, current, Window)
    ->  send(TabbedWindow, current, Window)
    ;   true
    ),
    (   Status == on_top
    ->  send(TF, update_frame_label)
    ;   true
    ).

                 /*******************************
                 *            TITLE             *
                 *******************************/

%       A window in a tab has no title bar of its own: the tab is where
%       its name is shown.  The frame carries the title of the tab the
%       user is looking at, and only once a window in it has asked for
%       one -- until then the frame keeps the title it was opened with.

window_label(TF, Label:char_array) :->
    "Show the title a window in me asked for on my label"::
    (   send(Label, equal, '')          % asking for no title at all is
    ->  send(TF, slot, window_label, @nil),   % asking for my own name back
        send(TF, label, TF?name)
    ;   send(TF, slot, window_label, Label),
        send(TF, label, Label)
    ).

label(TF, Label:'name|image') :->
    "Set my label and let the frame follow it"::
    send_super(TF, label, Label),
    send(TF, update_frame_label).

%       A frame that makes its own label -- see `pane_frame ->update_label'
%       -- is asked to remake it rather than told what it is: it may want to
%       say more than the tab does, and it is the one place the title is
%       written.  A plain frame is told, and only once a window has asked
%       for a title: until then it keeps the one it was opened with.

update_frame_label(TF) :->
    "Put my title on the frame, if I am the tab in view"::
    (   get(TF, status, on_top),
        get(TF, frame, Frame),
        Frame \== @nil
    ->  (   send(Frame, has_send_method, update_label)
        ->  send(Frame, update_label)
        ;   get(TF, window_label, Label),
            Label \== @nil
        ->  send(Frame, label, Label)
        ;   true
        )
    ;   true
    ).

:- pce_end_class(tab_frame).


                 /*******************************
                 *            HELPERS           *
                 *******************************/

%!  decoration(+Window, -Decoration) is det.
%
%   The graphical that represents Window on a device.  A window that has
%   a label or scrollbars of its own is wrapped in a window_decorator and
%   it is the decorator that carries the tile.

decoration(W, Decor) :-
    (   get(W, decoration, D),
        D \== @nil
    ->  Decor = D
    ;   Decor = W
    ).

%!  user_window(+Graphical, -Window) is det.
%
%   Inverse of decoration/2.

user_window(W0, W) :-
    (   send(W0, instance_of, window_decorator),
        get(W0, window, W1),
        W1 \== @nil
    ->  W = W1
    ;   W = W0
    ).

%!  window_list(+TabFrame, -Windows) is det.
%
%   The windows TabFrame holds, as a Prolog list.

window_list(TF, Windows) :-
    get(TF, graphicals, Graphicals),
    chain_list(Graphicals, List),
    findall(W,
            ( member(Gr, List),
              send(Gr, instance_of, window),
              user_window(Gr, W)
            ),
            Windows).

%!  drop_zone(+TabFrame, +Pos, -Target, -Where) is semidet.
%
%   Target is the window of TabFrame that Pos is over and Where the side
%   of it that Pos is nearest, in the coordinates the windows are laid
%   out in.

drop_zone(TF, Pos, Target, Where) :-
    get(Pos, x, PX),
    get(Pos, y, PY),
    get(TF, class_variable_value, split_bias, Bias),
    window_list(TF, Windows),
    member(W, Windows),
    decoration(W, Decor),
    get(Decor, area, area(X, Y, AW, AH)),
    AW > 0, AH > 0,
    PX >= X, PX =< X+AW,
    PY >= Y, PY =< Y+AH,
    !,
    Target = W,
    Left is (PX-X)/AW,                  % as a fraction of the window: a
    Right is (X+AW-PX)/AW,              % wide one would otherwise be almost
    Top is (PY-Y)/AH*Bias,              % all top and bottom zone, and those
    Bottom is (Y+AH-PY)/AH*Bias,        % are the rarer split anyway
    nearest_side([Left-left, Right-right, Top-above, Bottom-below], Where).

nearest_side([D-W|T], Where) :-
    nearest_side(T, D, W, Where).

nearest_side([], _, Where, Where).
nearest_side([D-W|T], D0, W0, Where) :-
    (   D < D0
    ->  nearest_side(T, D, W, Where)
    ;   nearest_side(T, D0, W0, Where)
    ).

%!  feedback_area(+Where, +W, +H, -X, -Y, -FW, -FH) is det.
%
%   The half of a W x H window a drop on side Where would take.

feedback_area(left,  W, H, 0,    0,    FW, H)  :- FW is W//2.
feedback_area(right, W, H, X,    0,    FW, H)  :- X is W//2, FW is W-X.
feedback_area(above, W, H, 0,    0,    W,  FH) :- FH is H//2.
feedback_area(below, W, H, 0,    Y,    W,  FH) :- Y is H//2, FH is H-Y.

%!  memberchk_eq(+X, +List) is semidet.
%
%   memberchk/2 on identity.  Not member/2: a destroyed object must not
%   be unified with a live one.

memberchk_eq(X, [Y|T]) :-
    (   X == Y
    ->  true
    ;   memberchk_eq(X, T)
    ).

%!  relative_window(+TabFrame, +Spec, -Window) is semidet.
%
%   Window a new window is placed relative to.

relative_window(_TF, Rel, Rel) :-
    Rel \== @default,
    !.
relative_window(TF, _, Rel) :-
    get(TF, current, Rel),
    !.
relative_window(TF, _, Rel) :-
    get(TF, windows, Windows),
    get(Windows, tail, Rel).


                 /*******************************
                 *         SPLIT HANDLE         *
                 *******************************/

/** A grip that drags its window onto another one.

A window is not a good thing to start a drag on: an editor and a terminal
both want the pointer for themselves.  So a window that is to be moved by
hand displays a split_handle, a small grip that does nothing else.  Drag
it onto another window and the receiver splits to make room; which of its
halves is taken follows the pointer.

The window it moves is its <-window, so any window can have one: display
it and place it from ->resize.

    send(W, display, new(H, split_handle)),
    send(H, place, W).
*/

:- pce_begin_class(pane_handle, icon_button,
                   "Small picture in the corner of a pane, acting on it").

class_variable(handle_size, size, size(16,16),
               "Size the picture is drawn at").

initialise(H, Image:name, Help:[name]) :->
    "Show Image, saying Help when the pointer rests on it"::
    get(H, class_variable_value, handle_size, Size),
    send_super(H, initialise, Image, Size),
    (   Help == @default
    ->  true
    ;   send(H, help_message, tag, Help)
    ).

place(H, W:window, Inset:[int]) :->
    "Put me in the upper-right corner of W, Inset from its right edge"::
    default(Inset, 0, TheInset),
    get(W, size, size(WW, _)),
    get(H, size, size(HW, _)),
    X is round(WW-TheInset-HW-2),       % a scrollbar can be a fraction of
    send(H, set, X, 2).                 % a pixel wide on a scaled display

:- pce_end_class(pane_handle).


:- pce_begin_class(split_handle, pane_handle,
                   "Grip to drag a window onto another one").

class_variable(grip_image, name, 'tool/drag-pane.svg',
               "Picture on the grip, drawn at <-handle_size").

:- pce_global(@split_handle_gesture, new(split_handle_gesture)).

initialise(H) :->
    "Create the grip"::
    get(H, class_variable_value, grip_image, Image),
    move_gesture(How),
    handle_help(How, Help),
    send_super(H, initialise, Image, Help),
    send(H, recogniser, @split_handle_gesture).

:- pce_end_class(split_handle).

:- pce_begin_class(split_handle_gesture, drag_and_drop_gesture,
                   "Drag a window by its grip").

class_variable(cursor,      [cursor], @default,
               "@default: a picture of the window being dragged").
class_variable(cursor_size,   size,    size(96,96),
               "Largest picture the cursor is made from").
class_variable(cursor_border, [colour]*, @default,
               "Border around it; @default: the foreground, @nil: none").

initialise(G) :->
    send_super(G, initialise, left, @default, @off, @arg1?window).

cursor(G, Gr:graphical, Cursor:cursor) :<-
    "A picture of the window being dragged, scaled to fit"::
    (   get(Gr, window, W),
        window_cursor(G, W, Cursor)
    ->  true
    ;   get_super(G, cursor, Gr, Cursor)
    ).

%       The drop target is looked for here rather than by the inherited
%       ->drag, which searches for a graphical that leads to something
%       with a ->drop.  A tab is the only thing that takes a window, so
%       ask the tabs of the frame the pointer is over straight away.

drag(G, Ev:event) :->
    "Outline the drop the pointer is over"::
    (   send(G, activate, Ev)
    ->  send(G, update_target, Ev)
    ;   true
    ).

update_target(G, Ev:event) :->
    "Find the tab under the pointer and let it show the drop"::
    (   drop_context(G, Ev, Tab, Pos)
    ->  send(G, forget_target, Tab),
        send(G, slot, target, Tab),
        send(Tab, preview_drop, G?source, Pos)
    ;   send(G, forget_target, @nil),
        send(G, slot, target, @nil)
    ).

forget_target(G, Keep:'tab_frame*') :->
    "Take the outline away, unless Keep is still the target"::
    (   get(G, target, Old),
        Old \== @nil,
        Old \== Keep,
        send(Old, has_send_method, preview_drop)
    ->  send(Old, preview_drop, @nil)
    ;   true
    ).

terminate(G, Ev:event) :->
    "Drop the window on the tab the pointer is over"::
    (   get(G, active_cursor, Cursor),
        Cursor \== @nil                % a click: it never became a drag
    ->  send(G, slot, active_cursor, @nil),
        (   get(G, source, Clicked),
            Clicked \== @nil
        ->  send(@split_move, start, Clicked, Ev?receiver)
        ;   true
        )
    ;   send(Ev?window, focus_cursor, @nil),
        (   drop_context(G, Ev, Tab, Pos)
        ->  send(Tab, drop, G?source, Pos)
        ;   debug(split_handle, 'let go with no target under the pointer', [])
        )
    ),
    send(G, forget_target, @nil),
    send(G, slot, target, @nil),
    send(G, slot, source, @nil).

:- pce_end_class(split_handle_gesture).

%!  window_cursor(+Gesture, +Window, -Cursor) is semidet.
%
%   A cursor carrying a picture of Window, scaled to fit <-cursor_size of
%   Gesture and with its border drawn around it.  Fails when there is no
%   image of Window to be had.

window_cursor(G, W, Cursor) :-
    catch(get(W, image, Image), _, fail),
    get(W, size, size(WW, WH)),
    get(G, class_variable_value, cursor_size, size(MW, MH)),
    scaled_size(WW, WH, MW, MH, SW, SH),
    get(Image, scale, size(SW, SH), Scaled),
    (   border_colour(G, W, Colour)
    ->  new(Border, box(SW-1, SH-1)),
        send(Border, colour, Colour),
        send(Scaled, draw_in, Border, point(0,0))
    ;   true
    ),
    HotX is SW//2,                      % the pointer carries the window
    HotY is SH//2,                      % by its middle
    new(Cursor, cursor(@nil, Scaled, point(HotX, HotY))).

%!  border_colour(+Gesture, +Window, -Colour) is semidet.
%
%   Colour to draw the border of the cursor picture in.  Fails when
%   <-cursor_border says there is to be none.

border_colour(G, W, Colour) :-
    get(G, class_variable_value, cursor_border, Border),
    Border \== @nil,
    (   Border == @default
    ->  get(W, display, Display),
        get(Display, foreground, Colour)
    ;   Colour = Border
    ).

%!  drop_context(+Gesture, +Event, -Tab, -Pos) is semidet.
%
%   Tab is the tab the drop would go to and Pos where the pointer is in
%   the coordinates it lays its windows out in.  The pointer may be over
%   another window of the application, so every tab that is on top of its
%   stack is a candidate, in the frame the drag started in first.

drop_context(G, Ev, Tab, Pos) :-
    get(G, source, Source),
    Source \== @nil,
    pointer_position(Ev, X, Y),
    candidate_tab(Source, Tab),
    tab_position(Tab, X, Y, Pos),
    get(Pos, x, PX),
    get(Pos, y, PY),
    (   get(Tab, drop_target, Pos, Target)
    ->  get(Tab, drop_side, Pos, _0Side)
    ;   Target = none,
        _0Side = none
    ),
    debug(split_handle, 'dragging ~w at ~d,~d of ~w -> ~w ~w',
          [Source, PX, PY, Tab, Target, _0Side]),
    Target \== none,
    Target \== Source,
    !.

%!  pointer_position(+Event, -X, -Y) is semidet.
%
%   Where the pointer is on the display.  A drag is tracked on the window
%   it started on, so the event says where the pointer is relative to that
%   window whatever it has moved over since.  Note that <-x and <-y of an
%   event take a relative_to argument and answer about <-receiver when it
%   is left out, which is the grip rather than the window.

pointer_position(Ev, X, Y) :-
    get(Ev, window, Window),
    Window \== @nil,
    get(Window, display_position, point(WX, WY)),
    get(Ev, slot, x, EX),
    get(Ev, slot, y, EY),
    X is WX+EX,
    Y is WY+EY.

%!  candidate_tab(+Source, -Tab) is nondet.
%
%   A tab that can take a drop: one that is on top of its stack, in the
%   frame the drag started in or in any other frame on the same display.

candidate_tab(Source, Tab) :-
    get(Source, frame, Frame),
    Frame \== @nil,
    (   frame_tab_frame(Frame, Tab)
    ;   window_positions_known,
        get(Frame, display, Display),
        get(Display, frames, Chain),
        chain_list(Chain, Frames),
        member(Other, Frames),
        Other \== Frame,
        frame_tab_frame(Other, Tab)
    ).

%!  move_gesture(-Gesture) is det.
%
%   How a window is taken to another one on this window system.  Dragging
%   it there needs to know where the windows are, which Wayland does not
%   say, so the pointer carries it between two clicks instead.

move_gesture(drag) :-
    window_positions_known,
    !.
move_gesture(click).

%!  handle_help(+Gesture, -Message) is det.
%
%   What the grip says it is for.  Both gestures are always on it, but the
%   one that works everywhere is the one worth naming.

handle_help(drag,  'Drag onto another window to put this one beside it').
handle_help(click, 'Click to pick this window up, then click where it goes').

%!  window_positions_known is semidet.
%
%   True when the window system says where windows are on the display,
%   which is what tells a drag which other frame the pointer is over.
%   Wayland does not: it tells a window neither where it is nor where the
%   pointer is outside it, by design.  Class split_move is the way across
%   frames there, and works everywhere.

window_positions_known :-
    \+ get(@pce, window_system_driver, wayland).

frame_tab_frame(Frame, TF) :-
    get(Frame, members, Members),
    chain_list(Members, Windows),
    member(W, Windows),
    send(W, instance_of, window),
    get(W, graphicals, Graphicals),
    chain_list(Graphicals, Displayed),
    member(Stack, Displayed),
    send(Stack, instance_of, tab_stack),
    get(Stack, graphicals, Tabs),
    chain_list(Tabs, TabList),
    member(TF, TabList),
    send(TF, instance_of, tab_frame),
    get(TF, status, on_top).

%!  tab_position(+Tab, +X, +Y, -Point) is semidet.
%
%   The display position X,Y in the coordinates Tab lays its windows out
%   in.  <-display_position of a tab is where its box starts; its <-offset
%   is what separates that from where its windows are placed.

tab_position(Tab, X, Y, Point) :-
    get(Tab, display_position, point(TX, TY)),
    get(Tab, offset, point(OX, OY)),
    PX is X-TX-OX,
    PY is Y-TY-OY,
    new(Point, point(PX, PY)).

%!  scaled_size(+W, +H, +MaxW, +MaxH, -SW, -SH) is det.
%
%   W x H scaled down to fit MaxW x MaxH, keeping its shape.

scaled_size(W, H, MaxW, MaxH, SW, SH) :-
    Scale is min(1.0, min(MaxW/max(W,1), MaxH/max(H,1))),
    SW is max(1, truncate(W*Scale)),
    SH is max(1, truncate(H*Scale)).


                 /*******************************
                 *          MOVE  MODE          *
                 *******************************/

/** Moving a window to a place picked with the pointer.

Dragging a window onto another one asks the window system where the
pointer is once it has left the window it started on.  Wayland does not
answer that, and no client-side arithmetic can make up for it, so a drag
there reaches no further than the frame it started in.

Clicking the grip instead picks the window up: the pointer takes its
picture and the next click says where it goes.  Nothing is held down in
between, so the pointer is delivered to whichever window it is over in the
ordinary way, in that window's own coordinates -- which every window
system does.  Class display does the routing through <-inspect_handlers,
the same mechanism the Visual Hierarchy tool uses to pick an object out of
any frame.
*/

:- pce_begin_class(split_move, object,
                   "Moving a window to a place picked with the pointer").

variable(source,   window*,    get, "Window waiting to be put somewhere").
variable(target,   tab_frame*, get, "Tab the pointer is over").

:- pce_global(@split_move, new(split_move)).
:- pce_global(@split_move_recogniser, make_split_move_recogniser).

make_split_move_recogniser(R) :-
    new(R, handler_group(handler(loc_move,    message(@split_move, preview, @arg1)),
                         handler(ms_left_up,  message(@split_move, drop,    @arg1)),
                         handler(ms_right_up, message(@split_move, cancel)))).

start(M, Source:window, Handle:graphical) :->
    "Pick Source up; the next click says where it goes"::
    send(M, stop),
    send(M, slot, source, Source),
    (   window_cursor(@split_handle_gesture, Source, Cursor)
    ->  true
    ;   Cursor = @default
    ),
    send(Source, focus, Handle, @split_move_recogniser, Cursor, @nil),
    send(Source, grab_pointer, @on),
    send(Source, report, status,
         'Click the window to put this one beside, or right-click to cancel').

stop(M) :->
    "Put the window down again"::
    (   get(M, slot, source, Source),
        Source \== @nil
    ->  ignore(send(Source, grab_pointer, @off)),   % taking it down must not
        ignore(send(Source, focus, @nil)),          % fail: nothing can be
        ignore(send(Source, report, status, '')),   % done about it
        ignore(send(M, forget_target, @nil)),
        send(M, slot, source, @nil)
    ;   true
    ).

cancel(M) :->
    "Leave the window where it was"::
    send(M, stop).

preview(M, Ev:event) :->
    "Outline the drop the pointer is over"::
    (   move_context(M, Ev, Tab, Pos)
    ->  send(M, forget_target, Tab),
        send(M, slot, target, Tab),
        send(Tab, preview_drop, M?source, Pos)
    ;   send(M, forget_target, @nil)
    ).

drop(M, Ev:event) :->
    "Put the window where the pointer is"::
    get(M, slot, source, Source),
    Source \== @nil,
    (   move_context(M, Ev, Tab, Pos)
    ->  send(M, forget_target, @nil),
        send(M, stop),                  % let go before moving the window
        send(Tab, drop, Source, Pos)
    ;   send(M, stop)
    ).

forget_target(M, Keep:'tab_frame*') :->
    "Take the outline away, unless Keep is still the target"::
    (   get(M, slot, target, Old),
        Old \== @nil,
        Old \== Keep
    ->  send(Old, preview_drop, @nil),
        send(M, slot, target, @nil)
    ;   true
    ).

:- pce_end_class(split_move).

%!  move_context(+Move, +Event, -Tab, -Pos) is semidet.
%
%   Tab is the tab the pointer is over and Pos where it is in the
%   coordinates Tab lays its windows out in.

move_context(M, Ev, Tab, Pos) :-
    get(M, slot, source, Source),
    Source \== @nil,
    grab_position(Ev, Frame, X, Y),
    frame_tab_frame(Frame, Tab),
    tab_position(Tab, X, Y, Pos),
    get(Tab, drop_target, Pos, Target),
    Target \== Source.

%!  grab_position(+Event, -Frame, -X, -Y) is semidet.
%
%   Frame is the frame a grabbed event arrived on and X,Y where it is on
%   the display.  While the pointer is grabbed the event is handed to the
%   window that grabbed it, wherever it comes from, but it still names the
%   frame it arrived on and a position in that frame -- except that the
%   offset of the grabbing window has been taken off again if it happens
%   to be on that same frame.
%
%   Only Frame is asked for a position, and move_context/5 asks nothing
%   else, so the frame position cancels out of the answer.  That matters:
%   under Wayland a frame does not know where it is.

grab_position(Ev, Frame, X, Y) :-
    get(Ev, frame, Frame),
    Frame \== @nil,
    get(Ev, window, W),
    get(Ev, slot, x, EX),
    get(Ev, slot, y, EY),
    (   get(W, frame, Frame)
    ->  get(W, display_position, point(WX, WY)),
        X is WX+EX,
        Y is WY+EY
    ;   get(Frame, area, area(FX, FY, _, _)),
        X is FX+EX,
        Y is FY+EY
    ).


                 /*******************************
                 *           GESTURE            *
                 *******************************/

:- pce_begin_class(tile_resize_gesture, gesture,
                   "Resize tiles by dragging the gap between them").

variable(tile, tile*, both, "Tile being resized").

initialise(G) :->
    send_super(G, initialise, left).

verify(_G, Ev:event) :->
    "Only start in a resizable gap"::
    get(Ev, receiver, TF),
    get(TF, resize_tile, Ev, _).

initiate(G, Ev:event) :->
    get(Ev, receiver, TF),
    get(TF, resize_tile, Ev, Tile),
    send(G, tile, Tile).

drag(G, Ev:event) :->
    send(G, resize, Ev).

terminate(G, Ev:event) :->
    send(G, resize, Ev),
    send(G, tile, @nil).

resize(G, Ev:event) :->
    "Move the gap to the position of Ev"::
    get(G, tile, Tile),
    Tile \== @nil,
    get(Ev, receiver, TF),
    get(Ev, position, TF, point(X, Y)),
    get(Tile, area, area(TX, TY, _, _)),
    (   get(Tile?super, orientation, vertical)
    ->  H is max(0, Y-TY),
        send(Tile, height, H)
    ;   W is max(0, X-TX),
        send(Tile, width, W)
    ),
    send(TF, update_separators).

:- pce_end_class(tile_resize_gesture).
