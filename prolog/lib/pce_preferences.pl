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

:- module(pce_preferences,
          [ preference_component/3,     % +Visual, -Root, -Spec
            preference_sections/3,      % +Root, +Spec, -Sections
            container_components/2,     % +Visual, -Components
            general_preferences/1       % -Tabs
          ]).
:- use_module(library(pce)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(pairs)).

/** <module> Declare the preferences of a GUI component

A GUI component, such as the Epilog terminal or a PceEmacs editor,
consists of objects of various classes.  Most of the class variables
of these classes are of no interest to the user of the component.  The
multifile predicate preferences/2 declares the classes that make up a
component and, for each, the class variables a user may want to
change.  This is used by the class variable editor.

The specification is normally placed in the library that defines the
component:

```
:- multifile pce_preferences:preferences/2.

pce_preferences:preferences(epilog_window,
    [ terminal_image - [font, background, colour, save_lines],
      epilog_report  - [placement, hide_after]
    ]).
```

Some class variables of a container concern whatever it holds, for
example the side of the window of the IDE a tool is added on.  These
are declared by the multifile predicate container_preferences/2 and are
shown with the preferences of any object inside the container.

Many class variables apply to the application as a whole rather than
to a component, for example the scale of the fonts or the gaps between
the tiles of a frame.  These are declared by the multifile predicates
general_tab/3 and general/2, which group them in tabs of the class
variable editor.

The multifile predicate edit_type/3 narrows the type a class variable is
edited as, for example to a range that is edited using a slider, and
edit_item/4 provides a dedicated item.  The class variable editor calls
class_variable_changed/3 after it changed a class variable, so a library
can act on the new value.
*/

:- multifile
    preferences/2,
    container_preferences/2,
    general_tab/3,
    general/2,
    edit_type/3,
    edit_item/4,
    class_variable_changed/3.

%!  preferences(?Class, ?Spec) is nondet.
%
%   Spec describes the preferences of a component whose root is an
%   instance of Class or a subclass thereof.  Spec is a list of
%   `Member - Names`, where Names is a list of class variable names
%   and Member is one of
%
%     - Class
%       The first visual in the consists-of tree of the root (see
%       `visual <-contained`) that is an instance of Class.
%       This may be the root itself.
%     - via(Source, Selector, Class)
%       An object that is not part of the consists-of tree, found by
%       sending <-Selector to the member Source.  Class is the class of
%       the object if the root has no such member.  For example, the
%       mode of a PceEmacs editor is via(emacs_editor, mode,
%       emacs_language_mode).
%     - *(Class)
%       The class variables Names for all classes, i.e., `*.Name` in
%       the Defaults file.  Class provides the type, the current value
%       and the documentation.  For example, `*(editor)` for the
%       selection style of all text objects.
%
%   The class variables of a member are those of the class of the
%   member found.  If there is no member, those of the declared class
%   are used.  Names that are not class variables of this class are
%   ignored, so a specification may name class variables of several
%   subclasses.

%!  container_preferences(?Class, ?Spec) is nondet.
%
%   As preferences/2, but the preferences are shown for any object
%   inside an instance of Class, in addition to the preferences of the
%   object itself.

container_preferences(pane_stack, [pane_stack - [pane_side]]).

%!  edit_type(?Class, ?Name, ?Type) is nondet.
%
%   The class variable Name of Class (or a subclass) is edited as Type,
%   a type that is narrower than its own and makes a better editor.
%   Typically, Type is a range that is edited using a slider while the
%   class variable itself accepts any number.  The type of the class
%   variable still decides what is valid.

edit_type(font,      scale,                '0.5..3.0').
edit_type(display,   bell_pitch,           '200..2000').
edit_type(display,   bell_duration,        '0.1..0.5').
edit_type(graphical, visual_bell_duration, '0.1..0.5').

%!  edit_item(+Class, +Name, +Value, -Item) is semidet.
%
%   Item is a dialog item to edit the class variable Name of the
%   class named Class, showing Value.  Use this if the type of the
%   class variable does not tell enough to edit it well.  The item must
%   implement <-selection and ->selection and call its <-message if
%   the user changes it.

%!  class_variable_changed(+Class, +Name, +Value) is nondet.
%
%   Called by the class variable editor after it changed the class
%   variable Name of the class named Class to Value in the running
%   session.  A library can use this hook if the new value only takes
%   effect if it acts on it.

%!  container_components(+Visual, -Components) is det.
%
%   Components is a list of Root-Spec for Visual and the visuals that
%   contain it, from the inside out, that have a container_preferences/2
%   specification Spec.

container_components(Visual, Components) :-
    findall(Root-Spec,
            ( enclosing(Visual, Root),
              get(Root, class, Class),
              class_spec(Class, container_preferences, Spec)
            ),
            Components).

enclosing(Visual, Visual).
enclosing(Visual, Root) :-
    get(Visual, contained_in, Parent),
    send(Parent, instance_of, visual),
    \+ send(Parent, instance_of, display),
    enclosing(Parent, Root).

%!  general_tab(?Tab, ?Label, ?Rank) is nondet.
%
%   Declare a tab of the class variable editor for general preferences.
%   The tabs are ordered by Rank.  A tab without preferences of a loaded
%   class is not shown.

general_tab(text,    'Text',    10).
general_tab(theme,   'Theme',   15).
general_tab(layout,  'Layout',  20).
general_tab(ide,    'IDE',    30).

%!  general(?Tab, ?Spec) is nondet.
%
%   Spec describes general preferences that are shown in the tab Tab.
%   Spec is a list of `Class - Names`, where Names are class variables
%   of Class, or heading(Title, Spec), which shows the preferences of
%   Spec under Title rather than under the summaries of their classes.
%   The specifications of all clauses for Tab are combined,
%   so a library may add preferences to a tab.  For example, the IDE
%   adds its preferences to the tab `ide`.

general(text,
        [ font        - [ scale, pango_families ],
          *(editor)   - [ selection_style ],
          text_cursor - [ fixed_font_style, proportional_font_style,
                          blink, colour, inactive_colour
                        ]
        ]).
general(theme,
        [ display - [ theme ],
          heading('Bell',
                  [ graphical - [ visual_bell, visual_bell_duration ],
                    display   - [ volume, bell_pitch, bell_duration ]
                  ])
        ]).
general(layout,
        [ tile - [ border, fixed_border, gap_colour,
                   separator_pen, separator_colour
                 ]
        ]).

%!  general_preferences(-Tabs) is det.
%
%   Tabs is a list of tab(Tab, Label, Sections), where Sections is a
%   list of section(Class, Names) as preference_sections/3.

general_preferences(Tabs) :-
    findall(Rank-(Tab-Label), general_tab(Tab, Label, Rank), Pairs),
    keysort(Pairs, Sorted),
    pairs_values(Sorted, Labels),
    convlist(general_tab_sections, Labels, Tabs).

general_tab_sections(Tab-Label, tab(Tab, Label, Sections)) :-
    findall(Spec, general(Tab, Spec), Specs),
    append(Specs, Spec),
    preference_sections(@nil, Spec, Sections),
    Sections \== [].

%!  preference_component(+Visual, -Root, -Spec) is semidet.
%
%   Root is the innermost visual that contains Visual (Visual included)
%   and has a preferences/2 specification Spec.

preference_component(Visual, Root, Spec) :-
    get(Visual, container, message(@prolog, has_preferences, @arg1), Root),
    object_spec(Root, Spec).

has_preferences(Visual) :-
    object_spec(Visual, _).

object_spec(Obj, Spec) :-
    get(Obj, class, Class),
    class_spec(Class, preferences, Spec).

%   class_spec(+Class, +Kind, -Spec) is semidet.
%
%   Spec is the specification of Kind (preferences or
%   container_preferences) for Class or its nearest super class.

class_spec(Class, Kind, Spec) :-
    get(Class, name, Name),
    call(Kind, Name, Spec),
    !.
class_spec(Class, Kind, Spec) :-
    get(Class, super_class, Super),
    Super \== @nil,
    class_spec(Super, Kind, Spec).

%!  preference_sections(+Root, +Spec, -Sections) is det.
%
%   Sections is a list of section(Class, Names), where Class is the
%   class whose class variables Names are edited, or `*(Class)` for a
%   member `*(Class)`.  An element heading(Title, Spec) results in
%   heading(Title, Sections) if Sections is not empty.  Members whose class
%   is not loaded are skipped.  If Root is `@nil`, the sections use the
%   declared classes.

preference_sections(Root, Spec, Sections) :-
    foldl(section(Root), Spec, Sections, []).

section(Root, heading(Title, Spec)) -->
    !,
    { preference_sections(Root, Spec, Sections) },
    (   { Sections == [] }
    ->  []
    ;   [ heading(Title, Sections) ]
    ).
section(Root, Member - Names) -->
    (   { member_class(Root, Member, Class) }
    ->  [ section(Class, Names) ]
    ;   []
    ).

member_class(Root, via(Source, Selector, Default), Class) :-
    !,
    (   Root \== @nil,
        source_class(Source, SourceClass),
        get(Root, contained, SourceClass, From),
        get(From, Selector, Obj),
        Obj \== @nil
    ->  get(Obj, class, Class)
    ;   get(@pce, convert, Default, class, Class)
    ).
member_class(Root, *(ClassName), *(Class)) :-
    !,
    member_class(Root, ClassName, Class).
member_class(Root, ClassName, Class) :-
    get(@pce, convert, ClassName, class, Declared),
    (   Root \== @nil,
        get(Root, contained, Declared, Member)
    ->  get(Member, class, Class)
    ;   Class = Declared
    ).

source_class(Name, Class) :-
    get(@pce, convert, Name, class, Class).
