/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org
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

:- module(pce_style_item,
          [ style_term/2                % +Style, -Term
          ]).
:- use_module(library(pce)).
:- use_module(library(help_message)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- pce_autoload(font_item,   library(pce_font_item)).
:- pce_autoload(colour_item, library(pce_colour_item)).
:- pce_autoload(tick_box,    library(pce_tick_box)).

/** <module> Dialog item to edit a style

Class style_item shows a sample text in a style and a button to edit
the style.  The button opens a style_editor, a dialog with an item for
each attribute of the style and a preview.  Styles are shared, so the
editor creates a new style rather than changing the one it edits.
*/

%!  style_attribute(?Name, ?Kind, ?Default) is nondet.
%
%   The attributes of a style we edit.  Kind is one of `font`, `colour`,
%   `line` (underline, strikethrough), `flag` or `int`.  Default is
%   the value of the attribute in style().

style_attribute(font,          font,   @default).
style_attribute(colour,        colour, @default).
style_attribute(background,    colour, @default).
style_attribute(bold,          flag,   @off).
style_attribute(italic,        flag,   @off).
style_attribute(underline,     line,   @default).
style_attribute(strikethrough, line,   @default).
style_attribute(grey,          flag,   @off).
style_attribute(highlight,     flag,   @off).
style_attribute(hidden,        flag,   @off).
style_attribute(left_margin,   int,    0).
style_attribute(right_margin,  int,    0).

%!  edited_attribute(?Name, ?Kind) is nondet.
%
%   The attributes the style editor has an item for.  It copies the
%   others from the style it edits.

edited_attribute(Name, Kind) :-
    style_attribute(Name, Kind, _),
    \+ not_edited(Name).

not_edited(grey).
not_edited(highlight).
not_edited(left_margin).
not_edited(right_margin).

%!  style_term(+Style, -Term) is det.
%
%   Term is `style(Name := Value, ...)` for the attributes of Style
%   that differ from their default.  This is the representation for a
%   Defaults file.

style_term(Style, Term) :-
    findall(Name := Value,
            ( style_attribute(Name, _, Default),
              get(Style, Name, Value),
              Value \== Default
            ), Args0),
    (   get(Style, icon, Icon), Icon \== @nil
    ->  Args = [icon := Icon|Args0]
    ;   Args = Args0
    ),
    Term =.. [style|Args].


                 /*******************************
                 *            ITEM              *
                 *******************************/

:- pce_begin_class(style_item, label_box,
                   "Show a style and a button to edit it").

variable(selection, style, get, "Current style").

initialise(SI, Name:[name], Selection:[style], Msg:[code]*) :->
    "Create from label, initial style and message"::
    default(Name, style, Nm),
    send_super(SI, initialise, Nm, Msg),
    send(SI, gap, size(8,0)),
    send(SI, append, new(style_sample)),
    send(SI, append, button(edit, message(SI, edit_style)), right),
    get(SI, member, edit, Edit),
    send(Edit, label, 'Edit…'),
    send(Edit, help_message, tag, 'Edit the style'),
    (   Selection == @default
    ->  new(Initial, style)
    ;   Initial = Selection
    ),
    send(SI, selection, Initial).

selection(SI, Style:style) :->
    "Set the style"::
    send(SI, slot, selection, Style),
    get(SI, member, style_sample, Sample),
    send(Sample, style, Style),
    send(SI, modified, @off),
    send(SI, layout_dialog).

user_selection(SI, Style:style) :->
    "The user edited the style"::
    send(SI, selection, Style),
    send(SI, forward).

forward(SI) :->
    send(SI, modified, @on),
    (   send(SI?device, modified_item, SI, @on)
    ->  true
    ;   ignore(send(SI, apply))         % no message is fine
    ).

modified_item(_SI, _Gr:graphical, _Modified:bool) :->
    fail.

clear(_SI) :->
    true.

active(SI, Val:bool) :->
    send_super(SI, active, Val),
    send(SI?graphicals, for_all, message(@arg1, active, Val)).

edit_style(SI) :->
    "Open the style editor"::
    new(E, style_editor(SI?selection, message(SI, user_selection, @arg1))),
    (   get(SI, frame, Frame)
    ->  send(E, transient_for, Frame),
        send(E, modal, transient),
        get(SI, display_position, Pos),
        send(E, open, Pos)
    ;   send(E, open)
    ).

:- pce_end_class(style_item).


:- pce_begin_class(style_sample, device,
                   "Sample text in a style").

initialise(S) :->
    send_super(S, initialise),
    send(S, name, style_sample),
    send(S, display, new(T, text('Sample text'))),
    send(T, name, text).

style(S, Style:style) :->
    "Show the sample in Style"::
    get(S, member, text, T),
    send(T, style, Style).

reference(S, Ref:point) :<-
    "Baseline of the sample"::
    get(S, member, text, T),
    get(T?font, ascent, A),
    get(T, y, Y),
    RY is Y + A,
    new(Ref, point(0, RY)).

:- pce_end_class(style_sample).


                 /*******************************
                 *      LINE DECORATION ITEM    *
                 *******************************/

:- pce_begin_class(line_decoration_item, label_box,
                   "Edit an underline or strikethrough").

%   The value is of type [bool|texture_name|colour]: @off, @on (a solid
%   line in the text colour), a texture name for a textured line or a
%   colour for a solid line in that colour.  If <-allow_default is @on,
%   @default is allowed too, i.e., the attribute is not defined.

variable(allow_default, bool := @off, get, "Allow for @default").

line_kind(default,    'Not defined by this style').
line_kind(off,        'No line').
line_kind(on,         'Solid line in the text colour').
line_kind(dotted,     'Dotted line').
line_kind(dashed,     'Dashed line').
line_kind(dashdot,    'Dash-dot line').
line_kind(dashdotted, 'Dash-dot-dot line').
line_kind(longdash,   'Long dashes').
line_kind(colour,     'Solid line in a colour').

initialise(LI, Name:[name], Selection:[any], Msg:[code]*,
           AllowDefault:[bool]) :->
    "Create from label, value, message and whether @default is allowed"::
    default(Name, line, Nm),
    send_super(LI, initialise, Nm, Msg),
    default(AllowDefault, @off, AD),
    send(LI, slot, allow_default, AD),
    send(LI, gap, size(8, 0)),
    send(LI, append,
         new(M, menu(kind, cycle, message(LI, kind_changed, @arg1)))),
    send(M, show_label, @off),
    forall(( line_kind(Kind, Help),
             \+ ( Kind == default, AD == @off )
           ),
           ( send(M, append, Kind),
             get(M, member, Kind, MI),
             send(MI, help_message, tag, Help)
           )),
    send(LI, append,
         new(CI, colour_item(colour, black,
                             message(LI, colour_changed, @arg1))),
         right),
    send(CI, show_label, @off),
    (   Selection == @default, AD == @off
    ->  send(LI, selection, @off)
    ;   send(LI, selection, Selection)
    ).

selection(LI, Value:any) :<-
    "Value of type [bool|texture_name|colour]"::
    get(LI, member, kind, M),
    get(M, selection, Kind),
    kind_value(Kind, LI, Value).

kind_value(default, _,  @default) :- !.
kind_value(off,     _,  @off) :- !.
kind_value(on,      _,  @on) :- !.
kind_value(colour,  LI, Colour) :- !,
    get(LI, member, colour, CI),
    get(CI, selection, Colour).
kind_value(Texture, _,  Texture).

selection(LI, Value:any) :->
    "Show Value"::
    get(LI, member, kind, M),
    value_kind(Value, Kind),
    send(M, selection, Kind),
    (   Kind == colour
    ->  get(LI, member, colour, CI),
        send(CI, selection, Value)
    ;   true
    ),
    send(LI, update),
    send(LI, modified, @off).

value_kind(@default, default) :- !.
value_kind(@off,     off) :- !.
value_kind(@on,      on) :- !.
value_kind(none,     on) :- !.          % texture none is a solid line
value_kind(Value,    colour) :-
    send(Value, instance_of, colour),
    !.
value_kind(Texture,  Texture).

update(LI) :->
    "Activate the colour item if we need a colour"::
    get(LI, member, kind, M),
    get(M, selection, Kind),
    get(LI, member, colour, CI),
    (   Kind == colour
    ->  send(CI, active, @on)
    ;   send(CI, active, @off)
    ).

kind_changed(LI, _Kind:name) :->
    send(LI, update),
    send(LI, forward).

colour_changed(LI, _Colour:colour) :->
    send(LI, update),
    send(LI, forward).

forward(LI) :->
    send(LI, modified, @on),
    (   send(LI?device, modified_item, LI, @on)
    ->  true
    ;   ignore(send(LI, apply))         % no message is fine
    ).

modified_item(_LI, _Gr:graphical, _Modified:bool) :->
    fail.

clear(_LI) :->
    true.

active(LI, Val:bool) :->
    send_super(LI, active, Val),
    send(LI?graphicals, for_all, message(@arg1, active, Val)),
    (   Val == @on
    ->  send(LI, update)
    ;   true
    ).

:- pce_end_class(line_decoration_item).

                 /*******************************
                 *            EDITOR            *
                 *******************************/

:- pce_begin_class(style_editor, dialog,
                   "Edit the attributes of a style").

variable(style,   style, get,  "Style being edited").
variable(message, code*, both, "Called with the new style").
variable(items,   hash_table, get, "Attribute name -> item").

initialise(E, Style:style, Msg:[code]*) :->
    "Create to edit Style"::
    send_super(E, initialise, 'Edit style'),
    send(E, slot, style, Style),
    send(E, slot, items, new(hash_table)),
    default(Msg, @nil, TheMsg),
    send(E, message, TheMsg),
    send(E, append, new(Sample, style_sample)),
    send(Sample, alignment, left),
    send(Sample, style, Style),
    forall(edited_attribute(Name, Kind),
           append_attribute(E, Style, Name, Kind)),
    align_labels(E),
    (   TheMsg == @nil
    ->  send(E, append, button(close, message(E, destroy)))
    ;   send(E, append, button(ok)),
        send(E, append, button(apply)),
        send(E, append, button(cancel))
    ).

%   append_attribute(+Editor, +Style, +Name, +Kind)
%
%   Append the item to edit attribute Name of Style.  Items for
%   attributes that may be left to the text have a tick box `default`.

append_attribute(E, Style, Name, Kind) :-
    get(Style, Name, Value),
    attribute_item(Kind, Name, Value, Item),
    send(Item, message, message(E, update_sample)),
    send(E?items, append, Name, Item),
    (   ( Kind == font ; Kind == colour )
    ->  (   Value == @default
        ->  Default = @on, Active = @off
        ;   Default = @off, Active = @on
        ),
        new(T, tick_box(default, Default,
                        message(E, default_changed, Item, @arg1))),
        default_tick_name(Name, TickName),
        send(E?items, append, TickName, T),
        send(T, show_label, @off),
        send(T, help_message, tag, 'Use the attribute of the text'),
        send(Item, active, Active),
        new(Row, dialog_group(Name, group)),    % align with Tick below
        send(Row, append, Item),
        send(E, append, Row),
        send(Row, alignment, left),
        new(Tick, dialog_group(TickName, group)), % flush right, so the
        send(Tick, gap, size(8, 0)),              % boxes align
        send(Tick, append, new(L, label(default, 'Default'))),
        send(L, length, 0),
        send(Tick, append, T, right),
        send(E, append, Tick, right),
        send(Tick, alignment, right)
    ;   send(E, append, Item)
    ).

%   align_labels(+Editor)
%
%   Give all attribute items the same label width, so their values
%   start in the same column, also for those in a row group.

align_labels(E) :-
    findall(Name, edited_attribute(Name, _), Names),
    maplist(attribute_item_of(E), Names, Items),
    foldl(max_label_width, Items, 0, Width),
    maplist(set_label_width(Width), Items).

max_label_width(Item, W0, W) :-
    get(Item, label_width, IW),
    W is max(W0, IW).

set_label_width(Width, Item) :-
    send(Item, label_width, Width).

attribute_item_of(E, Name, Item) :-
    get(E?items, member, Name, Item).

default_tick_name(Name, TickName) :-
    atom_concat(Name, '_default', TickName).

attribute_item(font, Name, Value, Item) :-
    (   Value == @default
    ->  new(Item, font_item(Name))
    ;   new(Item, font_item(Name, Value))
    ).
attribute_item(colour, Name, Value, Item) :-
    (   send(Value, instance_of, colour)
    ->  new(Item, colour_item(Name, Value))
    ;   new(Item, colour_item(Name))
    ).
attribute_item(line, Name, Value, Item) :-
    new(Item, line_decoration_item(Name, Value, @default, @on)).
attribute_item(flag, Name, Value, Item) :-
    new(Item, bool_item(Name, Value)).
attribute_item(int, Name, Value, Item) :-
    new(Item, int_item(Name, Value)).

default_changed(E, Item:graphical, Default:bool) :->
    "A default tick box changed"::
    (   Default == @on
    ->  send(Item, active, @off)
    ;   send(Item, active, @on)
    ),
    send(E, update_sample).

update_sample(E) :->
    "Show the current values in the sample"::
    get(E, current_style, Style),
    get(E, member, style_sample, Sample),
    send(Sample, style, Style).

current_style(E, Style:style) :<-
    "New style from the items"::
    get(E, style, Old),
    get(Old, icon, Icon),
    new(Style, style(icon := Icon)),
    forall(style_attribute(Name, Kind, _),
           ( attribute_value(E, Old, Name, Kind, Value),
             send(Style, Name, Value)
           )).

attribute_value(E, Old, Name, Kind, Value) :-
    (   edited_attribute(Name, Kind)
    ->  item_value(E, Old, Name, Kind, Value)
    ;   get(Old, Name, Value)
    ).

item_value(E, Old, Name, Kind, Value) :-
    get(E?items, member, Name, Item),
    (   ( Kind == font ; Kind == colour ),
        default_tick_name(Name, TickName),
        get(E?items, member, TickName, Tick),
        get(Tick, selection, @on)
    ->  Value = @default
    ;   Kind == colour,
        get(Old, Name, OldValue),
        \+ send(OldValue, instance_of, colour),  % e.g., an elevation
        get(Item, modified, @off)
    ->  Value = OldValue
    ;   get(Item, selection, Value)
    ).

apply(E) :->
    "Call <-message with the new style"::
    get(E, current_style, Style),
    send(E, slot, style, Style),
    get(E, message, Msg),
    forward_style(Msg, Style).

ok(E) :->
    "Close and call <-message with the new style"::
    get(E, current_style, Style),
    get(E, message, Msg),
    send(E, destroy),
    forward_style(Msg, Style).

forward_style(@nil, _) :- !.
forward_style(Msg, Style) :-
    ignore(send(Msg, forward, Style)).

cancel(E) :->
    "Close without applying"::
    send(E, destroy).

item(E, Name:name, Item:graphical) :<-
    "Item or tick box for the attribute Name"::
    get(E?items, member, Name, Item).

:- pce_end_class(style_editor).
