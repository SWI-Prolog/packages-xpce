/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi-prolog.org
    WWW:           https://www-swi-prolog.org/packages/xpce/
    Copyright (c)  1985-2025, University of Amsterdam
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

:- module(pce_font_item, []).
:- use_module(library(pce)).
:- use_module(library(help_message)).    % ->help_message
:- require([ send_list/2
           , default/3
           ]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This file defines class font_item.   Class   font_item  is a dialog-item
specialised in entering font-values.  It consists   of three cycle menus
for the family, style and point-size of the font.  The interface is very
similar to the interface of  the   built-in  dialog-items  such as class
text_item and friends.  Summary:

        <->selection:   Access to current font
        <->default:     Default value, which may be function (->restore)
        ->apply:        Execute the item

The family menu offers the generic families `sans`, `serif` and `mono`,
followed by the families installed on the system.  These are shown in
their own font and long names are ellipsized.

The button "Theme…" selects a theme font, such as the font aliases
`normal` and `bold`.  The item then shows the properties of the theme
font and its name, and <-selection is the theme font, until the user
changes the family, style or points.

Though a bit complicated due to all  interaction between the items, this
class may be used as an   example/template  for defining compound dialog
items.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(font_family_menu, menu,
                   "Cycle menu for a font family").

%   There are typically hundreds of installed families, so the list
%   that opens from the menu shows more lines than for other menus.

class_variable(combo_box_height, '1..', 15).

:- pce_end_class(font_family_menu).

:- pce_begin_class(font_item, label_box, "Dialog item for defining a font").

variable(value_set,     [chain],          get,    "List of fonts").
variable(theme_font,    theme_font*,      get,    "Selected theme font").
variable(families,      chain*,           get,    "Generic families offered").

initialise(FI, Name:[name],
           Default:'[font|function]', Message:[code]*,
           ValueSet:[chain], Theme:[bool]) :->
    "Create font-selector"::
    default(Name, font, Nm),
    default(Default, normal, Def),
    send(FI, send_super, initialise, Nm, Message),
    send(FI, gap, size(5,0)),
    send(FI, alignment, column),
    send(FI, append,
         new(Fam, font_family_menu(family, cycle,
                                   message(FI, family, @arg1)))),
    send(FI, append,
         new(Wgt, menu(weight, cycle, message(FI, weight, @arg1))), right),
    (   ValueSet == @default
    ->  send(FI, append,
             new(Pts, int_item(points, 10, message(FI, points, @arg1),
                               5, 36)), right)
    ;   send(FI, append,
             new(Pts, menu(points, cycle, message(FI, points, @arg1))), right)
    ),
    send(Fam, show_label, @off),
    get(Fam?value_font, ex, Ex),
    FamWidth is 15*Ex,
    send(Fam, max_value_width, FamWidth),
    send(Wgt, show_label, @off),
    send(Pts, show_label, @off),
    (   Theme == @off
    ->  true
    ;   send(FI, append,
             new(B, button(theme, message(FI, choose_theme_font))), right),
        send(B, label, 'Theme…'),
        send(B, help_message, tag, 'Select a font of the theme'),
        send(FI, append, new(NL, label(theme_name, '')), right),
        send(NL, length, 0)             % as wide as the name
    ),
    send(FI, slot, theme_font, @nil),
    send(FI, slot, families, @nil),
    send(FI, value_set, ValueSet),
    send(FI, default, Def).

clear(_) :->
    true.

active(FI, Val:bool) :->
    send_super(FI, active, Val),
    send(FI?graphicals, for_all, message(@arg1, active, Val)),
    single_family(FI).

%   single_family(+FontItem)
%
%   There is nothing to choose if we offer a single family, e.g., `mono`
%   for a mono_font, so the family menu is inactive.

single_family(FI) :-
    get(FI, member, family, Fam),
    (   get(Fam?members, size, 1)
    ->  send(Fam, active, @off)
    ;   true
    ).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Assign the item a value set (set of   fonts  to choose from).  This will
make entries in the three menus.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

value_set(FI, ValueSet:[chain]) :->
    "Define set of available fonts"::
    send(FI, slot, value_set, ValueSet),
    get(FI, member, family, Fam),
    get(FI, member, weight, Wgt),
    get(FI, member, points, Pts),
    send_list([Fam, Wgt, Pts], clear),
    (   ValueSet == @default
    ->  send_list(Fam, append, [sans,serif,mono]),
        append_installed_families(Fam, @default),
        send_list(Wgt, append, [normal,bold,italic]),
        (   send(Pts, instance_of, int_item)
        ->  true
        ;   forall(between(8,30,S),
                   send(Pts, append, S))
        )
    ;   send(ValueSet, for_all,
             and(if(not(?(Fam, member, @arg1?family)),
                    message(Fam, append, @arg1?family)),
                 if(not(?(Wgt, member, @arg1?style)),
                    message(Wgt, append, @arg1?style)),
                 if(and(message(Pts, instance_of, int_item),
                        not(?(Pts, member, @arg1?points))),
                    message(Pts, append, @arg1?points)))),
        send(Fam, sort)
    ),
    send(Wgt, sort),
    (   send(Pts, instance_of, int_item)
    ->  true
    ;   send(Pts, sort, ?(@arg1?value, compare, @arg2?value))
    ).


                 /*******************************
                 *            CHANGES           *
                 *******************************/

modified_item(_FI, _Gr:graphical, _Modified:bool) :->
    fail.

forward(FI) :->
    send(FI, modified, @on),
    (   send(FI?device, modified_item, FI, @on)
    ->  true
    ;   send(FI, apply)
    ).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
pick_active(+Menu)  changes  the  selection  of  a  menu  to  an  active
menu-item close to the current one of the current one is not active.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

pick_active(M) :-
    get(M, active_item, M?selection, @on),
    !.
pick_active(M) :-
    get(M, selection, S),
    get(M, member, S, MI),
    get(M, members, Chain),
    (   get(Chain, find, @arg1?active == @on, _)
    ->  get(Chain, index, MI, I),
        pick_active(Chain, I, 0, MIA),
        (   MIA == MI
        ->  true
        ;   send(M, selection, MIA)
        )
    ;   true
    ).


pick_active(Chain, I, Offset, MI) :-
    (   Offset mod 2 =:= 1
    ->  Idx is I + Offset//2
    ;   Idx is I - Offset//2
    ),
    get(Chain, nth1, Idx, MI),
    get(MI, active, @on),
    !.
pick_active(Chain, I, Offset, MI) :-
    NewOffset is Offset+1,
    pick_active(Chain, I, NewOffset, MI).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
After some menu has changed, this  method activates all styles available
to the current family  and  all   point-sizes  available  to the current
family/style combination.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

activate(FI) :->
    (   get(FI, value_set, @default)
    ->  true
    ;   get(FI, member, family, Fam),
        get(FI, member, weight, Wgt),
        get(FI, member, points, Pts),
        get(FI, value_set, ValueSet),
        get(Fam, selection, CFam),
        new(Wgts, chain),
        new(Ptss, chain),
        send(ValueSet, for_all,
             if(@arg1?family == CFam, message(Wgts, append, @arg1?style))),
        send(Wgt?members, for_all,
             message(@arg1, active,
                     when(message(Wgts, member, @arg1?value), @on, @off))),
        pick_active(Wgt),
        get(Wgt, selection, CWgt),
        send(ValueSet, for_all,
             if(and(@arg1?family == CFam,
                    @arg1?style == CWgt),
                message(Ptss, append, @arg1?points))),
        send(Pts?members, for_all,
             message(@arg1, active,
                     when(message(Ptss, member, @arg1?value), @on, @off))),
        pick_active(Pts)
    ).


family(FI, _Fam:name) :->
    "User changed family cycle"::
    send(FI, activate),
    send(FI, user_changed).

weight(FI, _Wgt:name) :->
    send(FI, activate),
    send(FI, user_changed).

points(FI, _Pts:int) :->
    send(FI, user_changed).

user_changed(FI) :->
    "The user changed a property: the font is no longer a theme font"::
    send(FI, show_theme_font, @nil),
    send(FI, forward).

show_theme_font(FI, Font:theme_font*) :->
    "Remember the theme font and show its name"::
    send(FI, slot, theme_font, Font),
    (   get(FI, member, theme_name, Label)
    ->  (   Font == @nil
        ->  send(Label, selection, '')
        ;   get(Font, name, Name),
            send(Label, selection, Name)
        )
    ;   true
    ).

choose_theme_font(FI) :->
    "Select a theme font"::
    (   get(FI, families, Families),
        Families \== @nil,
        get(Families, size, 1)          % e.g., only mono
    ->  get(Families, head, Family)
    ;   Family = @default
    ),
    new(Chooser, theme_font_chooser(FI?theme_font, Family,
                                    message(FI, user_selection, @arg1))),
    (   get(FI, frame, Frame)
    ->  send(Chooser, transient_for, Frame),
        send(Chooser, modal, transient),
        get(FI, display_position, Pos),
        send(Chooser, open, Pos)
    ;   send(Chooser, open)
    ).

user_selection(FI, Font:font) :->
    "The user selected Font"::
    send(FI, selection, Font),
    send(FI, forward).


families(FI, Families:chain) :->
    "Only offer these families and the matching installed ones"::
    send(FI, slot, families, Families),
    get(FI, member, family, Fam),
    get(Fam, selection, Current),
    send(Fam, clear),
    send(Families, for_all, message(Fam, append, @arg1)),
    chain_list(Families, List),
    (   List == [mono]
    ->  append_installed_families(Fam, @on)
    ;   subtract(List, [sans,serif,mono], [])
    ->  append_installed_families(Fam, @default)
    ;   true
    ),
    (   get(Fam, member, Current, _)
    ->  true
    ;   send(Fam, append, Current)      % do not lose the current value
    ),
    send(Fam, selection, Current),
    single_family(FI).

%   append_installed_families(+Menu, +Monospace)
%
%   Append the installed families to the family menu: the monospaced
%   ones if Monospace is @on, all if it is @default.

append_installed_families(Fam, Monospace) :-
    get(Fam?value_font, points, Points),
    installed_families(Monospace, Installed),
    send(Installed, for_all,
         message(@prolog, append_family_item, Fam, @arg1, Points, @off)).

                 /*******************************
                 *  GENERIC DIALOG OPERATIONS   *
                 *******************************/

selection(FI, Font:font) :->
    "Set the selection"::
    get(FI, member, family, Fam),
    get(FI, member, weight, Wgt),
    get(FI, member, points, Pts),
    get(Font, family, Family),
    (   get(Fam, member, Family, _)
    ->  true
    ;   send(Fam, append, Family)       % e.g., not installed
    ),
    send(Fam, selection, Family),
    send(Wgt, selection, Font?style),
    send(Pts, selection, Font?points),
    (   send(Font, instance_of, theme_font)
    ->  send(FI, show_theme_font, Font)
    ;   send(FI, show_theme_font, @nil)
    ),
    send(FI, activate).

selection(FI, Font:font) :<-
    "Get the current font"::
    get(FI, theme_font, Font),
    Font \== @nil,
    !.
selection(FI, Font:font) :<-
    get(FI, member, family, Fam),
    get(FI, member, weight, Wgt),
    get(FI, member, points, Pts),
    get(Fam, selection, Family),
    get(Wgt, selection, Style),
    get(Pts, selection, Points),
    new(Font, font(Family, Style, Points)).

:- pce_end_class.


                 /*******************************
                 *      THEME FONT CHOOSER      *
                 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Class theme_font_chooser lists the theme fonts, each shown in its own
font, and calls its message with the one the user selects.  If a
family is given, it only lists the theme fonts of this family, e.g.,
`mono` for an item that edits a mono_font.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(theme_font_chooser, frame,
                   "Select a theme font").

variable(message, code*, both, "Called with the selected theme font").

initialise(F, Current:theme_font*, Family:[name], Msg:[code]*) :->
    "Create from current font, family and message"::
    send_super(F, initialise, 'Select theme font'),
    default(Msg, @nil, TheMsg),
    send(F, message, TheMsg),
    send(F, append, new(B, browser(size := size(40, 16)))),
    get(B?font, width, "boldlarge  ", W),
    send(B?text_image, tab_stops, vector(W)),
    send(B, open_message, message(F, ok)),
    send(@theme_fonts, for_all,
         if(or(Family == @default, @arg1?family == Family),
            message(F, append_font, @arg1))),
    send(new(D, dialog), below, B),
    send(D, append, button(ok)),
    send(D, append, button(cancel)),
    (   Current \== @nil
    ->  get(Current, name, Name),
        send(B, selection, Name),
        send(B, normalise, Name)
    ;   true
    ).

append_font(F, Font:theme_font) :->
    "Add an entry for Font, shown in this font"::
    get(F, member, browser, B),
    get(Font, name, Name),
    get(Font, family, Family),
    get(Font, style, Style),
    get(Font, points, Points),
    send(B, style, Name, style(font := Font)),
    Label = string('%s\t%s %s %s', Name, Family, Style, Points),
    send(B, append, new(DI, dict_item(Name, Label, Font, Name))),
    (   get(Font, summary, Summary), Summary \== @nil
    ->  send(DI, help_message, tag, Summary)
    ;   true
    ).

ok(F) :->
    "Call <-message with the selected font and close"::
    get(F, member, browser, B),
    (   get(B, selection, DI), DI \== @nil
    ->  get(DI, object, Font),
        get(F, message, Msg),
        send(F, destroy),
        (   Msg == @nil
        ->  true
        ;   send(Msg, forward, Font)
        )
    ;   send(F, report, warning, 'No font selected')
    ).

cancel(F) :->
    "Close without selecting"::
    send(F, destroy).

:- pce_end_class(theme_font_chooser).


                 /*******************************
                 *        PANGO FAMILIES        *
                 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Class pango_families_item edits the class variable font.pango_families,
which maps the generic families mono, sans and serif to a Pango family.
The value is a chain of `Generic := Families`, where Families is a Pango
family or a comma-separated list of families to try in turn.  The item
shows a cycle menu per generic family with the families installed on
this system, each shown in its own font.  A current value that is not
an installed family, e.g., a list of families, is added to the menu.
Other generic families in the value are not shown and dropped.  The old families screen, helvetica and times use the
mapping of mono, sans and serif, see class font.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(pango_families_item, label_box,
                   "Edit the mapping from generic to Pango families").

initialise(PI, Name:[name], Families:chain, Message:[code]*) :->
    "Create from the mapping and a message"::
    default(Name, pango_families, Nm),
    send_super(PI, initialise, Nm, Message),
    send(PI, alignment, column),
    forall(generic_family(Generic),
           ( family_mapping(Families, Generic, Pango),
             generic_monospace(Generic, Mono),
             installed_families(Mono, Installed),
             send(PI, append_family, Generic, Pango, Installed)
           )),
    get(PI, graphicals, Menus),
    send(Menus, for_all, message(@arg1, compute)),
    get(Menus, map, ?(@arg1, value_width), Widths),
    chain_list(Widths, WidthList),
    max_list(WidthList, Width),
    send(Menus, for_all, message(@arg1, value_width, Width)).

generic_family(mono).
generic_family(sans).
generic_family(serif).

%   Pango tells which families are monospaced.  There is no way to tell
%   whether a family is sans or serif, so these offer the others.

generic_monospace(mono,  @on).
generic_monospace(sans,  @off).
generic_monospace(serif, @off).

%   family_mapping(+Families, +Generic, -Pango)
%
%   Pango is the mapping of Generic in Families or, if Families does not
%   map it, the one in use.

family_mapping(Families, Generic, Pango) :-
    member_chain(Binding, Families),
    object(Binding, Generic := Pango0),
    !,
    pango_text(Pango0, Pango).
family_mapping(_, Generic, Pango) :-
    get(@font_families, member, Generic, Pango),
    !.
family_mapping(_, Generic, Generic).

pango_text(Text, Text) :-
    atomic(Text),
    !.
pango_text(Obj, Text) :-
    get(Obj, value, Text).

append_family(PI, Generic:name, Families:name, Installed:chain) :->
    "Add a menu for the families of Generic"::
    new(M, font_family_menu(Generic, cycle, message(PI, forward))),
    get(M, value_font, Font),
    get(Font, points, Points),
    send(Installed, for_all,
         message(@prolog, append_family_item, M, @arg1, Points, @off)),
    send(PI, append, M),
    select_family(M, Families).

%   append_family_item(+Menu, +Family, +Points, +Before)
%
%   Add a menu item for Family.  The combo box shows the item in the
%   font of the attribute `preview_font` and drops it if the font
%   cannot show the name, such as for symbol fonts.  Fonts are only
%   loaded when shown.  Using `menu_item<-font` would load all of
%   them to compute the size of the menu.  Mnemonics make no sense for
%   this many items.

append_family_item(M, Family, Points, Before) :-
    new(MI, menu_item(Family, @default, Family)),
    send(MI, mnemonic, @nil),
    send(MI, attribute, preview_font, font(Family, normal, Points)),
    (   Before == @on
    ->  send(M, prepend, MI)
    ;   send(M, append, MI)
    ).

%   select_family(+Menu, +Families)
%
%   Select Families, adding it if it is not installed, and show the
%   selection in its font.

select_family(M, Families0) :-
    get(@pce, convert, Families0, name, Families),
    (   get(M, member, Families, _)
    ->  true
    ;   get(M, value_font, Font),
        get(Font, points, Points),
        append_family_item(M, Families, Points, @on)
    ),
    send(M, selection, Families),
    show_selection_font(M).

show_selection_font(M) :-
    send(M?members, for_all, message(@arg1, font, @default)),
    (   get(M, selection, Family),
        get(M, member, Family, MI)
    ->  get(MI, attribute, preview_font, Font),
        send(MI, font, Font)
    ;   true
    ).

selection(PI, Families:chain) :<-
    "New chain of Generic := Families"::
    new(Families, chain),
    send(PI?graphicals, for_all,
         if(message(@arg1, instance_of, menu),
            message(Families, append,
                    create(':=', @arg1?name,
                           create(string, '%s', @arg1?selection))))).

selection(PI, Families:chain) :->
    "Show the families of a mapping"::
    forall(generic_family(Generic),
           ( family_mapping(Families, Generic, Pango),
             get(PI, member, Generic, Menu),
             select_family(Menu, Pango)
           )),
    send(PI, modified, @off).

modified_item(_PI, _Gr:graphical, _Modified:bool) :->
    "Our fields call ->forward"::
    fail.

forward(PI) :->
    "A menu changed"::
    send(PI?graphicals, for_all,
         if(message(@arg1, instance_of, menu),
            message(@prolog, show_selection_font, @arg1))),
    send(PI, modified, @on),
    (   get(PI, device, Dev),
        Dev \== @nil,
        send(Dev, modified_item, PI, @on)
    ->  true
    ;   ignore(send(PI, apply))
    ).

clear(_PI) :->
    true.

:- pce_end_class(pango_families_item).


                 /*******************************
                 *         FONT ALIASES         *
                 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Class font_aliases_item edits the class variable font.system_fonts, which
maps the font aliases such as `normal` and `bold` to a font.  The value
is a chain of `Alias := Font`.  The item shows a font_item per alias:
the built-in aliases in their built-in order, followed by the aliases
the value adds.  An alias that the value does not mention uses its
built-in font.  A font_item has no weight, so an alias keeps its font,
e.g., `bitt`, which is bold italic, as long as the user does not change
it.  A sample text in the font follows each font_item.  The aliases
for monospaced fonts, such as `tt`, only offer monospaced families.  The tooltip of
a font_item explains the role of the alias.

Fonts are _related_ if they only differ by their style and weight, such
as `tt`, `boldtt`, `itt` and `bitt`.  If the switch `same_size` is on,
changing the size of a font also changes the size of its related fonts.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(font_aliases_item, label_box,
                   "Edit the fonts of the font aliases").

initialise(AI, Name:[name], Aliases:chain, Message:[code]*) :->
    "Create from the aliases and a message"::
    default(Name, system_fonts, Nm),
    send_super(AI, initialise, Nm, Message),
    send(AI, label, 'Font aliases:'),
    send(AI, alignment, column),
    alias_fonts(Aliases, Pairs),
    forall(member(Alias-Font, Pairs),
           ( new(FI, font_item(Alias, Font, @nil, @default, @off)),
             send(FI, message, message(AI, forward, FI)),
             send(FI, label, string(Alias)),    % no capitalisation
             alias_help(FI, Alias),
             mono_alias_families(FI, Alias, Font),
             send(AI, append, FI),
             send(AI, append, new(L, label(sample, '')), right),
             send(FI, attribute, sample, L)
           )),
    new(G, dialog_group(same_size, group)),
    send(G, show_label, @off),          % keep the label column narrow
    send(G, append, new(Same, bool_item(same_size, @on))),
    send(Same, show_label, @off),
    send(G, append, label(same_size_label,
                          'Same size for related fonts'), right),
    send(Same, help_message, tag,
         'Changing the size of a font also changes the fonts \c
          that only differ by style, such as tt, boldtt and itt'),
    send(AI, append, G),
    send(AI, selection, Aliases).

sample_text('AaBbCc 0O1lI').

%   mono_alias_families(+FontItem, +Alias, +Font)
%
%   A monospaced alias, such as `tt`, only offers the monospaced
%   families.  An alias is monospaced if its built-in font, or for an
%   alias the user added its font, has the family `mono`.

mono_alias_families(FI, Alias, Font) :-
    (   builtin_aliases(Builtin),
        memberchk(Alias-BuiltinFont, Builtin)
    ->  get(BuiltinFont, family, Family)
    ;   get(Font, family, Family)
    ),
    (   Family == mono
    ->  send(FI, families, chain(mono))
    ;   true
    ).

%   alias_help(+FontItem, +Alias)
%
%   Explain the role of Alias in a tooltip, using the <-summary of its
%   theme font.

alias_help(FI, Alias) :-
    get(@pce, convert, Alias, font, Font),
    send(Font, instance_of, theme_font),
    get(Font, summary, Summary),
    Summary \== @nil,
    !,
    send(FI, help_message, tag, Summary).
alias_help(_, _).

%   show_sample(+FontItem)
%
%   Show the sample after FontItem in the font of FontItem.  Also
%   remember this font as the attribute `shown`, so sync_sizes/2 knows
%   the size before the user changes it.

show_sample(FI) :-
    get(FI, attribute, sample, L),
    alias_font(FI, Font),
    sample_text(Text),
    send(L, font, Font),
    send(L, selection, Text),
    send(FI, attribute, shown, Font).

%   sync_sizes(+AliasesItem, +FontItem)
%
%   The user changed FontItem.  If only its size changed, give the
%   fonts that were related to it the same size.

sync_sizes(AI, FI) :-
    get(FI, attribute, shown, Old),
    alias_font(FI, New),
    get(Old, family, Family),
    get(New, family, Family),
    get(Old, style, Style),
    get(New, style, Style),
    get(Old, points, OldPoints),
    get(New, points, Points),
    OldPoints =\= Points,
    !,
    get(AI?graphicals, find_all,
        and(message(@arg1, instance_of, font_item),
            @arg1 \== FI,
            ?(@arg1, attribute, shown)?family == Family,
            ?(@arg1, attribute, shown)?points == OldPoints),
        Related),
    send(Related, for_all,
         message(@prolog, set_points, @arg1, Points)).
sync_sizes(_, _).

set_points(FI, Points) :-
    get(FI, attribute, shown, Old),
    get(Old, family, Family),
    get(Old, style, Style),
    get(Old, weight, Weight),
    new(Font, font(Family, Style, Points, Weight)),
    send(FI, attribute, alias_font, Font),      % keep the weight
    send(FI, selection, Font).

%   alias_fonts(+Aliases:chain, -Pairs) is det.
%
%   Pairs is a list Alias-Font for the built-in aliases, followed by the
%   aliases Aliases adds.

alias_fonts(Aliases, Pairs) :-
    builtin_aliases(Builtin),
    chain_bindings(Aliases, Bindings),
    findall(Alias-Font,
            ( member(Alias-Font0, Builtin),
              (   memberchk(Alias-Font1, Bindings)
              ->  Font = Font1
              ;   Font = Font0
              )
            ; member(Alias-Font, Bindings),
              \+ memberchk(Alias-_, Builtin)
            ),
            Pairs).

builtin_aliases(Pairs) :-
    get(@font_class, class_variable, system_fonts, CV),
    get(CV, default, Default),
    get(CV, convert_string, Default, Chain),
    chain_bindings(Chain, Pairs).

chain_bindings(Chain, Pairs) :-
    chain_list(Chain, Bindings),
    findall(Alias-Font,
            ( member(B, Bindings),
              get(B, name, Alias),
              get(B, value, Font0),
              get(@pce, convert, Font0, font, Font)
            ),
            Pairs).

selection(AI, Aliases:chain) :<-
    "New chain of Alias := Font"::
    get(AI, graphicals, Items),
    chain_list(Items, List),
    findall(Alias := Font,
            ( member(FI, List),
              send(FI, instance_of, font_item),
              get(FI, name, Alias),
              alias_font(FI, Font)
            ),
            Bindings),
    new(Aliases, chain),
    forall(member(Binding, Bindings),
           ( new(B, Binding),
             send(Aliases, append, B)
           )).

%   alias_font(+FontItem, -Font)
%
%   Font is the font shown by FontItem, or the font of the alias if it
%   is the same, so we keep its weight.

alias_font(FI, Font) :-
    get(FI, selection, Font0),
    (   get(FI, attribute, alias_font, Font1),
        same_font(Font0, Font1)
    ->  Font = Font1
    ;   Font = Font0
    ).

same_font(F1, F2) :-
    get(F1, family, Family), get(F2, family, Family),
    get(F1, style, Style),   get(F2, style, Style),
    get(F1, points, P1),     get(F2, points, P2),
    P1 =:= P2.

selection(AI, Aliases:chain) :->
    "Show the fonts of the aliases"::
    alias_fonts(Aliases, Pairs),
    forall(( member(Alias-Font, Pairs),
             get(AI, member, Alias, FI)
           ),
           ( send(FI, attribute, alias_font, Font),
             send(FI, selection, Font),
             show_sample(FI)
           )),
    send(AI, modified, @off).

same_size(AI, Same:bool) :->
    "Whether changing a size also changes the related fonts"::
    get(AI, member, same_size, G),
    get(G, member, same_size, BI),
    send(BI, selection, Same).
same_size(AI, Same:bool) :<-
    "Whether changing a size also changes the related fonts"::
    get(AI, member, same_size, G),
    get(G, member, same_size, BI),
    get(BI, selection, Same).

modified_item(_AI, _Gr:graphical, _Modified:bool) :->
    "Our fields call ->forward"::
    fail.

forward(AI, Changed:[font_item]) :->
    "A font changed"::
    (   Changed \== @default,
        get(AI, same_size, @on)
    ->  sync_sizes(AI, Changed)
    ;   true
    ),
    send(AI?graphicals, for_all,
         if(message(@arg1, instance_of, font_item),
            message(@prolog, show_sample, @arg1))),
    send(AI, modified, @on),
    (   get(AI, device, Dev),
        Dev \== @nil,
        send(Dev, modified_item, AI, @on)
    ->  true
    ;   ignore(send(AI, apply))
    ).

clear(_AI) :->
    true.

:- pce_end_class(font_aliases_item).

%   installed_families(+Monospace, -Families:chain)
%
%   Families is a sorted chain with the names of the installed Pango
%   families that are monospaced (Monospace is @on) or not.

installed_families(Monospace, Families) :-
    get(@font_class, font_families, Monospace, Sheet),
    get(Sheet, attribute_names, Families),
    send(Families, sort).
