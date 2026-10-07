/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org/packages/xpce/
    Copyright (c)  1997-2011, University of Amsterdam
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

:- module(pce_colour_item, []).
:- use_module(library(pce)).
:- use_module(library(help_message)).
:- pce_autoload(colour_editor, library(pce_colour_editor)).
:- autoload(library(pce_theme), [available_theme/1]).
:- require([ between/3
           , default/3
           , forall/2
           , ignore/1
           , send_list/3
           ]).

/** <module> Dialog items for colours

This library defines

  - colour_item
    A compact item showing the colour as a swatch with its name and
    buttons to select a theme colour or edit the colour.
  - colour_palette_item
    An item showing a palette, RGB sliders and a name field.
  - colour_set_item
    A colour_palette_item that edits the palette (a set of colours).
  - colour_name_item
    A text item that completes colour names.
  - ansi_colours_item
    An item showing the 16 ANSI colours of a terminal as squares that
    can be clicked to edit the colour.
  - theme_item
    A cycle menu to select the colour theme (see library(pce_theme))
    for the class variable `display.theme`.
*/

resource(cpalette,      image,  image('16x16/cpalette1.png')).
resource(trash,         image,  image('tool/trashcan.svg')).

default_palette_colour(red).
default_palette_colour(darkorange).
default_palette_colour(blue).
default_palette_colour(navy).
default_palette_colour(green).
default_palette_colour(yellow).
default_palette_colour(navajowhite).
default_palette_colour(brown).
default_palette_colour(white).
default_palette_colour(black).
default_palette_colour(grey80).
default_palette_colour(grey50).

:- pce_begin_class(colour_palette_item, dialog_group,
                   "Item for selecting a colour from a palette").

variable(message,       code*,        both, "Executed message").
variable(modified,      bool := @off, both, "Item was modified").

initialise(CI,
           Name:[name], Selection:[colour], Msg:[code]*,
           Palette:[chain]) :->
    (   Palette == @default
    ->  make_default_palette(Palette1)
    ;   Palette1 = Palette
    ),
    default(Msg, @nil, Msg1),
    send(CI, slot, message, Msg1),
    send(CI, send_super, initialise, Name, box),
    send(CI, append, new(PM, menu(palette, choice,
                                  message(CI, user_selection, @arg1)))),
    send(PM, layout, horizontal),
    send(PM, show_label, @off),
    send(PM, alignment, left),
    send(CI, append, new(RG, dialog_group(right_group, group)), right),
    send_list([PM, RG], reference, point(0,0)),
    send(RG, gap, size(5,5)),
    send(RG, append, box(50,50)),
    send(RG, append, new(Action, button(palette_button))),
    send(Action, label, image(resource(cpalette))),
    send(CI, palette, Palette1),
    send(CI, append, new(R, slider(red,   0, 255, 128))),
    send(CI, append, new(G, slider(green, 0, 255, 128))),
    send(CI, append, new(B, slider(blue,  0, 255, 128))),
    send(CI, append,
         new(NI, colour_name_item(name, @default,
                                  message(CI, user_selection, @arg1)))),
    send(NI, show_label, @off),
    init_slider(R),
    init_slider(G),
    init_slider(B),
    (   Selection \== @default
    ->  send(CI, selection, Selection)
    ;   send(CI, select_first)
    ).


make_default_palette(Palette) :-
    new(Palette, chain),
    forall(default_palette_colour(Colour), send(Palette, append, Colour)).


show_label(CI, Val:bool) :->            % To dialog_group?
    (   Val == @on
    ->  send(CI, label, ?(CI, label_name, name))
    ;   send(CI, label, '')
    ).

proto_box(CI, Box:box) :<-
    "Box used for feedback on selection"::
    get(CI, member, right_group, RG),
    get(RG, member, box, Box).

:- pce_group(selection).

colour_selection(CI, Colour:colour) :->
    "Set the current selection"::
    get(CI, proto_box, Box),
    send(Box, fill, Colour),
    set_slider(CI, Colour, red),
    set_slider(CI, Colour, green),
    set_slider(CI, Colour, blue),
    get(CI, member, name, NI),
    send(NI, selection, Colour),
    get(CI, member, palette, Palette),
    get(CI, member, right_group, RG),
    get(RG, member, palette_button, AB),
    (   get(Palette?members, find,
            message(Colour, equal, @arg1?value),
            Item)
    ->  send(Palette, selection, Item),
        send(AB, message,
             message(CI, delete_palette_colour, Box?fill)),
        send(AB, label, image(resource(trash)))
    ;   send(Palette, clear_selection),
        send(AB, message,
             message(CI, add_palette_colour, Box?fill)),
        send(AB, label, image(resource(cpalette)))
    ),
    send(CI, modified, @off).

colour_selection(CI, Colour:colour) :<-
    "Get the current selection"::
    get(CI, proto_box, Box),
    get(Box, fill, Colour),
    send(CI, modified, @off).

selection(CI, Colour:colour) :->
    send(CI, colour_selection, Colour).
selection(CI, Colour:colour) :<-
    get(CI, colour_selection, Colour).

user_selection(CI, Colour:colour) :->
    "User selected a colour"::
    send(CI, colour_selection, Colour),
    send(CI, modified, @on),
    (   get(CI, device, Dev),
        send(Dev, has_send_method, modified_item)
    ->  send(CI?device, modified_item, CI, @on)
    ;   true
    ).

clear(_CI) :->
    "Clear selection"::
    true.

select_first(CI) :->
    "Select first in palette"::
    get(CI, member, palette, Menu),
    (   get(Menu?members, head, First)
    ->  send(CI, colour_selection, First?value)
    ;   true                        % no colours in palette
    ).

:- pce_group(apply).

apply(SE, Always:[bool]) :->
    "Forward <-selection over <-message"::
    (   (   Always == @on
        ;   get(SE, modified, @on)
        ),
        get(SE, message, Msg),
        Msg \== @nil
    ->  get(SE, selection, Value),
        ignore(send(Msg, forward, Value))
    ;   true
    ).


                 /*******************************
                 *             SLIDERS          *
                 *******************************/

init_slider(Slider) :-
    get(Slider, name, Colour),
    new(I, image(@nil, 16, 16, pixmap)),
    send(I, fill, Colour),
    send(Slider, label, I),
    send(Slider, width, 100),
    send(Slider, drag, @on),
    send(Slider, attribute, hor_stretch, 100),
    get(Slider, device, CI),
    send(Slider, show_value, @off),
    send(Slider, message, message(CI, slider_dragged)).

set_slider(CI, Colour, SliderName) :-
    get(CI, member, SliderName, Slider),
    get(Colour, SliderName, Value),     % 0..255
    send(Slider, selection, Value).

slider_dragged(CI) :->
    current_slider_value(CI, red,   R),
    current_slider_value(CI, green, G),
    current_slider_value(CI, blue,  B),
    send(CI, user_selection, colour(@default, R, G, B)).

current_slider_value(CI, Colour, Value) :-
    get(CI, member, Colour, Slider),
    get(Slider, selection, Value).


                 /*******************************
                 *             PALETTE          *
                 *******************************/

palette(CI, Palette:chain) :->
    "Fill the palette menu"::
    get(CI, member, palette, Menu),
    send(Menu, clear),
    send(Palette, for_all, message(CI, add_palette_colour, @arg1, @off)),
    send(CI, adjust_palette_size).
palette(CI, Palette:chain) :<-
    "Fetch current palette as chain of colours"::
    get(CI, member, palette, Menu),
    get(Menu?members, map, @arg1?value, Palette).


add_palette_colour(CI, Colour:colour, Adjust:[bool]) :->
    "Add colour to the palette"::
    get(CI, member, palette, Menu),
    send(Menu, append, menu_item(Colour)),
    (   Adjust \== @off
    ->  send(CI, adjust_palette_size),
        send(CI, user_selection, Colour)
    ;   true
    ).

delete_palette_colour(CI, Colour:colour, Adjust:[bool]) :->
    "Add colour to the palette"::
    get(CI, member, palette, Palette),
    get(Palette?members, find,
        message(Colour, equal, @arg1?value),
        Item),
    send(Palette, delete, Item),
    (   Adjust \== @off
    ->  send(CI, adjust_palette_size),
        send(CI, user_selection, Palette?selection)
    ;   true
    ).

adjust_palette_size(CI) :->
    get(CI, member, palette, Menu),
    get(Menu?members, size, Entries),
    (   Entries == 0
    ->  true
    ;   palette_dimensions(Entries, NW, NH),
        send(Menu, columns, NH),
        IW is 125 // NW,
        IH is 100 // NH,
        send(Menu?members, for_all,
             message(@prolog, resize_colour_item, @arg1, IW, IH))
    ).

resize_colour_item(Item, IW, IH) :-
    get(Item, value, Colour),
    new(I, image(@nil, IW, IH, pixmap)),
    send(I, fill, Colour),
    send(Item, label, I).


%!  palette_dimensions(+Entries, -Width, -Height)
%
%   Attempts to find a nice 2-dimensional layout for `Entries' cells
%   in a total size of `Width' x `Height'.

palette_dimensions(Entries, Width, Height) :- % perfect divisors
    findall(D, divisor(Entries, D), Ds),
    Ds \== [],
    best_divisor(Ds, Entries, 1000/_, OK/Height),
    OK < 0.4,
    Width is Entries / Height.
palette_dimensions(Entries, Width, Height) :-
    Width is integer(sqrt(Entries)*1.1),
    Height is (Entries+Width-1)//Width.

best_divisor([H|T], Entries, Ok0/I0, R) :-
    W is Entries/H,
    Ok1 is abs(H/W-5/4),
    (   Ok1 < Ok0
    ->  best_divisor(T, Entries, Ok1/H, R)
    ;   best_divisor(T, Entries, Ok0/I0, R)
    ).

divisor(N, D) :-
    Max is ceiling(sqrt(N)),
    between(1, Max, D),
    N mod D =:= 0.

:- pce_end_class.


                 /*******************************
                 *          COLOUR ITEM         *
                 *******************************/

:- pce_begin_class(colour_item, label_box,
                   "Show a colour and buttons to change it").

variable(selection, colour, get, "Current colour").

initialise(CI, Name:[name], Selection:[colour], Msg:[code]*) :->
    "Create from label, initial colour and message"::
    default(Name, colour, Nm),
    send_super(CI, initialise, Nm, Msg),
    send(CI, gap, size(5,0)),
    send(CI, append, new(colour_swatch)),
    send(CI, append,
         button(theme, message(CI, choose_theme_colour)), right),
    send(CI, append,
         button(rgb, message(CI, edit_colour)), right),
    send(CI, append, new(NameLabel, label(colour_name, '')), right),
    send(NameLabel, length, 0),         % as wide as the name
    get(CI, member, theme, Theme),
    send(Theme, label, 'Theme…'),
    send(Theme, compute),
    get(Theme?area, height, BH),
    get(CI, member, colour_swatch, Swatch),
    send(Swatch, size, BH-4),           % about as tall as the buttons
    send(Theme, help_message, tag, 'Select a colour of the theme'),
    get(CI, member, rgb, RGB),
    send(RGB, label, 'Colour…'),
    send(RGB, help_message, tag, 'Edit the colour'),
    default(Selection, black, Initial),
    send(CI, selection, Initial).

selection(CI, Colour:colour) :->
    "Set the colour"::
    send(CI, slot, selection, Colour),
    get(CI, member, colour_swatch, Swatch),
    send(Swatch, colour, Colour),
    get(CI, member, colour_name, Name),
    send(Name, selection, Colour?name),
    send(CI, modified, @off).

user_selection(CI, Colour:colour) :->
    "The user selected Colour"::
    send(CI, selection, Colour),
    send(CI, forward).

forward(CI) :->
    forward_item(CI).

modified_item(_CI, _Gr:graphical, _Modified:bool) :->
    fail.

clear(_CI) :->
    true.

active(CI, Val:bool) :->
    send_super(CI, active, Val),
    send(CI?graphicals, for_all, message(@arg1, active, Val)).

choose_theme_colour(CI) :->
    "Select a theme colour"::
    new(Chooser, theme_colour_chooser(CI?selection,
                                      message(CI, user_selection, @arg1))),
    open_for(Chooser, CI).

edit_colour(CI) :->
    "Edit the colour in the HSV/RGB model"::
    new(Editor, colour_editor(CI?selection,
                              message(CI, user_selection, @arg1))),
    open_for(Editor, CI).

%   open_for(+Window, +Item)
%
%   Open Window as a transient window of the frame of Item.

open_for(Window, Item) :-
    (   get(Item, frame, Frame)
    ->  send(Window, transient_for, Frame),
        send(Window, modal, transient),
        get(Item, display_position, Pos),
        send(Window, open, Pos)
    ;   send(Window, open)
    ).

:- pce_end_class(colour_item).

%   forward_item(+Item)
%
%   The user changed Item.  Tell the dialog, which may handle this as
%   the config editor does, or execute the message of Item.

forward_item(Item) :-
    send(Item, modified, @on),
    (   get(Item, device, Dev),
        Dev \== @nil,
        send(Dev, modified_item, Item, @on)
    ->  true
    ;   ignore(send(Item, apply))       % no message is fine
    ).


                 /*******************************
                 *          ANSI COLOURS        *
                 *******************************/

%   The colours of `terminal_image <-ansi_colours`, a vector of 16
%   colours.  Clicking a colour opens the colour editor for it.  If
%   the selection is @nil, the terminal uses the default (theme)
%   colours, which are shown.

:- pce_begin_class(ansi_colours_item, label_box,
                   "Edit the 16 ANSI colours of a terminal").

variable(selection, vector*, get, "Current colours").

initialise(AI, Name:[name], Selection:[vector]*, Msg:[code]*) :->
    "Create from label, initial colours and message"::
    default(Name, ansi_colours, Nm),
    send_super(AI, initialise, Nm, Msg),
    send(AI, gap, size(3,0)),
    forall(between(1, 16, I),
           ( new(S, ansi_colour_swatch(I)),
             (   I == 1
             ->  send(AI, append, S)
             ;   send(AI, append, S, right)
             )
           )),
    default(Selection, @nil, Initial),
    send(AI, selection, Initial).

selection(AI, Colours:vector*) :->
    "Set the colours"::
    send(AI, slot, selection, Colours),
    send(AI?graphicals, for_all,
         if(message(@arg1, instance_of, ansi_colour_swatch),
            message(@arg1, show_colour, AI))),
    send(AI, modified, @off).

colour(AI, Index:'1..16', Colour:colour) :<-
    "Colour at Index (1-based)"::
    get(AI, selection, Colours),
    (   Colours \== @nil,
        get(Colours, element, Index, Colour0),
        Colour0 \== @nil
    ->  Colour = Colour0
    ;   ansi_colour(Index, _, Name),
        get(@pce, convert, Name, colour, Colour)
    ).

edit_colour(AI, Index:'1..16') :->
    "Edit the colour at Index"::
    get(AI, colour, Index, Colour),
    new(Editor, colour_editor(Colour,
                              message(AI, user_colour, Index, @arg1))),
    open_for(Editor, AI).

user_colour(AI, Index:'1..16', Colour:colour) :->
    "The user set the colour at Index"::
    get(AI, selection, Old),
    (   Old == @nil
    ->  new(New, vector),
        forall(between(1, 16, I),
               ( get(AI, colour, I, C),
                 send(New, element, I, C)
               ))
    ;   get(Old, copy, New)
    ),
    send(New, element, Index, Colour),
    send(AI, selection, New),
    send(AI, forward).

forward(AI) :->
    forward_item(AI).

modified_item(_AI, _Gr:graphical, _Modified:bool) :->
    fail.

clear(_AI) :->
    true.

active(AI, Val:bool) :->
    send_super(AI, active, Val),
    send(AI?graphicals, for_all, message(@arg1, active, Val)).

:- pce_end_class(ansi_colours_item).

%   ansi_colour(?Index, ?Role, ?Default)
%
%   The role and default colour of the ANSI colours.

ansi_colour( 1, 'Black',          ansi_black).
ansi_colour( 2, 'Red',            ansi_red).
ansi_colour( 3, 'Green',          ansi_green).
ansi_colour( 4, 'Yellow',         ansi_yellow).
ansi_colour( 5, 'Blue',           ansi_blue).
ansi_colour( 6, 'Magenta',        ansi_magenta).
ansi_colour( 7, 'Cyan',           ansi_cyan).
ansi_colour( 8, 'White',          ansi_white).
ansi_colour( 9, 'Bright black',   ansi_bright_black).
ansi_colour(10, 'Bright red',     ansi_bright_red).
ansi_colour(11, 'Bright green',   ansi_bright_green).
ansi_colour(12, 'Bright yellow',  ansi_bright_yellow).
ansi_colour(13, 'Bright blue',    ansi_bright_blue).
ansi_colour(14, 'Bright magenta', ansi_bright_magenta).
ansi_colour(15, 'Bright cyan',    ansi_bright_cyan).
ansi_colour(16, 'Bright white',   ansi_bright_white).


:- pce_begin_class(ansi_colour_swatch, colour_swatch,
                   "Square showing one of the ANSI colours").

variable(index, '1..16', get, "Index in the ANSI colours").

initialise(S, Index:'1..16') :->
    send_super(S, initialise),
    send(S, slot, index, Index),
    send(S, size, 18),
    ansi_colour(Index, Role, Default),
    format(string(Tip), '~w (default ~w)', [Role, Default]),
    send(S, help_message, tag, Tip),
    send(S, cursor, hand2),
    send(S, recogniser,
         click_gesture(left, '', single,
                       message(S?device, edit_colour, S?index))).

show_colour(S, AI:ansi_colours_item) :->
    "Show my colour in AI"::
    get(S, index, Index),
    get(AI, colour, Index, Colour),
    send(S, colour, Colour).

:- pce_end_class(ansi_colour_swatch).


:- pce_begin_class(colour_swatch, device,
                   "Square in a colour").

initialise(S) :->
    send_super(S, initialise),
    send(S, name, colour_swatch),
    send(S, display, new(B, box(16, 16))),
    send(B, name, box),
    send(B, colour, grey50).

size(S, Size:int) :->
    "Make the square Size x Size pixels"::
    get(S, member, box, Box),
    send(Box, size, size(Size, Size)).

colour(S, Colour:colour) :->
    "Show Colour"::
    get(S, member, box, Box),
    send(Box, fill, Colour).

%   The reference point aligns the centre of the square with the centre
%   of the buttons next to it.  As the buttons align their label with
%   the item label, we have the offset from the centre of a button to
%   its reference.

reference(S, Ref:point) :<-
    "Align the centre with the centre of the buttons"::
    get(S, member, box, Box),
    get(Box?area, height, H),
    (   get(S, device, Dev), Dev \== @nil,
        get(Dev, member, theme, Button)
    ->  get(Button, reference, BRef),
        get(BRef, y, BY),
        get(Button?area, height, BH),
        Y is H//2 + BY - BH//2
    ;   get(@pce, convert, normal, font, Font),
        get(Font, ascent, A),
        Y is H//2 + A//2
    ),
    new(Ref, point(0, Y)).

:- pce_end_class(colour_swatch).


                 /*******************************
                 *     THEME COLOUR CHOOSER     *
                 *******************************/

:- pce_begin_class(theme_colour_chooser, frame,
                   "Select a theme colour").

variable(message, code*, both, "Called with the selected theme colour").

initialise(F, Current:[colour], Msg:[code]*) :->
    "Create from current colour and message"::
    send_super(F, initialise, 'Select theme colour'),
    default(Msg, @nil, TheMsg),
    send(F, message, TheMsg),
    send(F, append, new(B, browser(size := size(80, 20)))),
    get(B?font, width, "  ui_text_selection_background  ", W),
    send(B?text_image, tab_stops, vector(W+24)),
    send(B, open_message, message(F, ok)),
    get(@theme_colours, copy, Colours),
    send(Colours, sort, ?(@prolog, compare_theme_colours, @arg1, @arg2)),
    send(Colours, for_all, message(F, append_colour, @arg1)),
    send(new(D, dialog), below, B),
    send(D, append, button(ok)),
    send(D, append, button(cancel)),
    (   Current \== @default,
        send(Current, instance_of, theme_colour)
    ->  get(Current, name, Name),
        send(B, selection, Name),
        send(B, normalise, Name)
    ;   true
    ).

append_colour(F, Colour:theme_colour) :->
    "Add an entry for Colour"::
    get(F, member, browser, B),
    get(Colour, name, Name),
    new(Icon, image(@nil, 20, 14, pixmap)),
    send(Icon, fill, Colour),
    send(Icon, draw_in, new(Border, box(20, 14))),
    send(Border, colour, grey50),
    send(B, style, Name, style(icon := Icon)),
    (   get(Colour, summary, Summary), Summary \== @nil
    ->  Label = string('  %s\t%s', Name, Summary)  % spaces: gap after icon
    ;   Label = string('  %s', Name)
    ),
    send(B, append, dict_item(Name, Label, Colour, Name)).

ok(F) :->
    "Call <-message with the selected colour and close"::
    get(F, member, browser, B),
    (   get(B, selection, DI), DI \== @nil
    ->  get(DI, object, Colour),
        get(F, message, Msg),
        send(F, destroy),
        (   Msg == @nil
        ->  true
        ;   send(Msg, forward, Colour)
        )
    ;   send(F, report, warning, 'No colour selected')
    ).

cancel(F) :->
    "Close without selecting"::
    send(F, destroy).

:- pce_end_class(theme_colour_chooser).

%   compare_theme_colours(+C1, +C2, -Order)
%
%   Order the ui_* colours first and the ansi_* colours last, each
%   group by name.

compare_theme_colours(C1, C2, Order) :-
    get(C1, name, N1),
    get(C2, name, N2),
    theme_colour_group(N1, G1),
    theme_colour_group(N2, G2),
    compare(Order0, G1-N1, G2-N2),
    order_name(Order0, Order).

theme_colour_group(Name, 0) :- sub_atom(Name, 0, _, _, ui_), !.
theme_colour_group(Name, 2) :- sub_atom(Name, 0, _, _, ansi_), !.
theme_colour_group(_, 1).

order_name(<, smaller).
order_name(=, equal).
order_name(>, larger).


                 /*******************************
                 *          COLOUR NAMES        *
                 *******************************/

:- pce_global(@colour_rgb_names, make_colour_rgb_names).

%   make_colour_rgb_names(-Table)
%
%   Table maps the 0xRRGGBB value of the named colours of
%   @colour_names (provided by the kernel) to their name.

make_colour_rgb_names(Table) :-
    new(Table, hash_table),
    send(@colour_names, for_all,
         if(not(?(Table, member, @arg2)),
            message(Table, append, @arg2, @arg1))).

:- pce_begin_class(colour_name_item, text_item,
                   "Completing item for colour-names").

initialise(NI, Name:[name], Selection:[colour], Msg:[code]*) :->
    default(Name, colour, TheName),
    send(NI, send_super, initialise, TheName, Selection, Msg),
    send(NI, value_set, @colour_list).

selection(NI, Selection:colour) :<-
    "Get selection as a colour object"::
    get(NI, get_super, selection, ColourName),
    get(@pce, convert, ColourName, colour, Selection).

selection(NI, Selection:colour) :->
    "Display named colour"::
    standardise_colour_name(Selection, TheName),
    send(NI, send_super, selection, TheName).

standardise_colour_name(Colour, XName) :-
    get(Colour, name, In),
    get(In, character, 0, 0'#),
    get(Colour, red, R),
    get(Colour, green, G),
    get(Colour, blue, B),
    RGB is (R<<16) + (G<<8) + B,
    get(@colour_rgb_names, member, RGB, XName),
    !.
standardise_colour_name(Colour, Name) :-
    get(Colour, name, Name).


:- pce_end_class.

:- pce_begin_class(colour_set_item, colour_palette_item,
                   "Editor for a set of colours (a palette)").

initialise(PI, Name:[name], Selection:[chain], Msg:[code]*) :->
    send(PI, send_super, initialise, Name, @default, Msg, Selection).

selection(PI, Selection:chain) :->
    send(PI, palette, Selection),
    send(PI, modified, @off).
selection(PI, Selection:chain) :<-
    get(PI, palette, Selection),
    send(PI, modified, @off).

:- pce_end_class.


test :-
    new(D, dialog),
    send(D, append, colour_palette_item(colour, red)),
    send(D, open).
