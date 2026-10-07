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

Though a bit complicated due to all  interaction between the items, this
class may be used as an   example/template  for defining compound dialog
items.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(font_item, label_box, "Dialog item for defining a font").

variable(value_set,     [chain],          get,    "List of fonts").

initialise(FI, Name:[name],
           Default:'[font|function]', Message:[code]*,
           ValueSet:[chain]) :->
    "Create font-selector"::
    default(Name, font, Nm),
    default(Default, normal, Def),
    send(FI, send_super, initialise, Nm, Message),
    send(FI, gap, size(5,0)),
    send(FI, alignment, column),
    send(FI, append,
         new(Fam, menu(family, cycle, message(FI, family, @arg1)))),
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
    send(Wgt, show_label, @off),
    send(Pts, show_label, @off),
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
                    message(Pts, append, @arg1?points))))
    ),
    send(Fam, sort),
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
    send(FI, forward).

weight(FI, _Wgt:name) :->
    send(FI, activate),
    send(FI, forward).

points(FI, _Pts:int) :->
    send(FI, forward).


families(FI, Families:chain) :->
    "Only offer these families"::
    get(FI, member, family, Fam),
    get(Fam, selection, Current),
    send(Fam, clear),
    send(Families, for_all, message(Fam, append, @arg1)),
    (   send(Families, member, Current)
    ->  true
    ;   send(Fam, append, Current)      % do not lose the current value
    ),
    send(Fam, selection, Current),
    single_family(FI).

                 /*******************************
                 *  GENERIC DIALOG OPERATIONS   *
                 *******************************/

selection(FI, Font:font) :->
    "Set the selection"::
    get(FI, member, family, Fam),
    get(FI, member, weight, Wgt),
    get(FI, member, points, Pts),
    send(Fam, selection, Font?family),
    send(Wgt, selection, Font?style),
    send(Pts, selection, Font?points),
    send(FI, activate).

selection(FI, Font:font) :<-
    "Get the current font"::
    get(FI, member, family, Fam),
    get(FI, member, weight, Wgt),
    get(FI, member, points, Pts),
    get(Fam, selection, Family),
    get(Wgt, selection, Style),
    get(Pts, selection, Points),
    new(Font, font(Family, Style, Points)).

:- pce_end_class.


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
    chain_list(Families, Bindings),
    member(Binding, Bindings),
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
    new(M, menu(Generic, cycle, message(PI, forward))),
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
%   them to compute the size of the menu.  Accelerators make no sense
%   for this many items.

append_family_item(M, Family, Points, Before) :-
    new(MI, menu_item(Family, @default, Family)),
    send(MI, accelerator, @nil),
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

%   installed_families(+Monospace, -Families:chain)
%
%   Families is a sorted chain with the names of the installed Pango
%   families that are monospaced (Monospace is @on) or not.

installed_families(Monospace, Families) :-
    get(@font_class, font_families, Monospace, Sheet),
    get(Sheet, attribute_names, Families),
    send(Families, sort).
