# Examples {#sec-examples}

## Confirm {#example-Confirm}

Enter name while application waits

The example below defines a prompter for a name.  As this prompter may
often be called from many different locations in the program, the
prompter itself is not destroyed after the name has been entered, but
just removed from the screen.

```prolog
%	ask_name(+Prompt, +Label, -Name)
%	Put a prompter on the screen and wait till the user has
%	entered a name.  Pressing cancel makes this predicate fail.
%	Prompt is a long string, giving explanation; Label is a short
%	label displayed for the text entry field.


:- pce_global(@name_prompter, make_name_prompter).

make_name_prompter(P) :-
	new(P, dialog),
	send(P, kind, transient),
	send(P, append, label(prompt)),
	send(P, append,
	        new(TI, text_item(name, '',
			 message(P?ok_member, execute)))),
	send(P, append, button(ok, message(P, return, TI?selection))),
	send(P, append, button(cancel, message(P, return, @nil))).


ask_name(Prompt, Label, Name) :-
	send(@name_prompter?prompt_member, selection, Prompt),
	send(@name_prompter?name_member, label, Label),
	send(@name_prompter?name_member, clear),
	get(@name_prompter, confirm_centered, RawName),
	send(@name_prompter, show, @off),
	RawName \== @nil,
	Name = RawName.

ask_name :-
	ask_name('Street', name, Street),
	writeln(Street).

% Start the demo

:- ask_name.
```

@see frame<-confirm

## Counting {#example-Counting}

Counting long strings in a chain

The following example assumes a chain of string objects.  It will count
all strings that are longer than 80 characters.

```prolog
number_of_long_strings(Chain, N) :-
	new(Number, number(0)),
	send(Chain, for_all,
         if(@arg1?size > 80,
			message(Number, plus, 1))),
	get(Number, value, N),
	send(Number, done).
```

@see class if
@see class number
@see chain->for_all

## Creating objects {#example-Creating-objects}

Basic examples on how to create objects

The examples below show how simple objects can be created.

```prolog
%	Objects with named references

?- new(@my_window, picture('My Window')).	% A graphics window
?- new(@origin, point(0,0)).  				% A point at (0,0)

%	Objects with anonymous (integer) references

?- new(B, box(30,40)).						% A box of 30 x 40
?- new(L, line(0,0,100,200, second)).		% A line with arrow
```

## Finding objects {#example-Finding-objects}

Finding objects in a collection

The following example shows how one can find an object in a chain that
has a certain name.

This example uses the equality code class _==_ to perform the test.  The
method `Chain <-find` executes the code argument successively on the
elements of the chain, forwarding each element via @arg1.  As soon as
the code returns success, the value is returned.

```prolog
find_named_in_chain(Chain, Name, Obj) :-
	get(Chain, find, @arg1?name == Name, Obj).
```

@see node<-find
@see hash_table<-find
@see dict<-find
@see chain<-find
@see topic Conditions
@see class ==

## Graphical relation {#example-Graphical-relation}

Using spatials

The example below opens a picture object with two box objects that are
related though a spatial object.  Both boxes may be resized and moved.
The spatial object ensures B1 is twice as large as B2, centered in
X-direction and 10 pixels above B2.

```prolog
spatial_demo :-
	new(G, handler_group(resize_gesture(left),
			     		 move_gesture(left))),
	new(P, picture('Spatial Demo')),
	send(P, display, new(B1, box(100,100))),
	send(P, display, new(B2, box(50,50))),
	send_list([B1, B2], recogniser, G),
	new(_, constraint(B1, B2,
			  		  spatial(xref = x+w/2, yref = y+h,
							  xref = x+w/2, yref = y-10,
							  w = w2*2, h = h2*2))),
	send(P, open).

% Start the demo

:- spatial_demo.
```

@see class spatial

## Hierarchy {#example-Hierarchy}

Generate a tree from facts

The example below will create a graphical representation from facts of
the form `is_a(Parent, Son)`, which are supposed to span a
single-inheritance tree.

Modern (2002 :-) Explorer-like hierarchies can be created using
this class too.  A high-level library is in library(pce_toc)
(Table Of Contents).  This library is also used by most
hierarchies in the XPCE and Prolog development tools.

```prolog
%	display_hierarchy(+Root)
%	Show a window with the hierarchy described by is_a(Parent, Son)
%	starting from the given root.

display_hierarchy(Root) :-
	new(P, picture(string('Hierarchy from %s', Root))),
	send(P, open),
	send(P, display, tree(new(Node, node(text(Root))))),
	expand_hierarchy(Root, Node).

expand_hierarchy(Root, Node) :-
	forall(is_a(Root, Sub),
	       (send(Node, son, new(Son, node(text(Sub)))),
	        expand_hierarchy(Sub, Son))).

%	Example database

is_a(species,	plant).
is_a(species,	animal).
is_a(animal,	mamal).
is_a(mamal,		monkey).
is_a(mamal,		horse).

% Start the demo

:- display_hierarchy(species).
```

@see class tree

## Linking Graphicals {#example-Linking-Graphicals}

Linking two boxes with a connection

This example demonstrates how two boxes can be linked using connections.
Note that the link is reusable, and is therefore declared as a global
object.

```prolog
:- pce_global(@in_out_link, make_in_out_link).

make_in_out_link(L) :-
	new(L, link(in, out, line(0,0,0,0,second))).

linked_box_demo :-
	new(P, picture('Linked Box demo')),
	send(P, open),
	send(P, display, new(B1, box(50,50)), point(20,20)),
	send(P, display, new(B2, box(25,25)), point(100,100)),
	send(B1, handle, handle(w, h/2, in)),
	send(B2, handle, handle(w/2, 0, out)),
	send(B1, connect, B2, @in_out_link),

			% Add resize/move gestures so you can
			% play

	send_list([B1,B2], recogniser,
			  handler_group(resize_gesture(left),
							move_gesture(left))).

% Start the demo

:- linked_box_demo.
```

@see topic Connections
@see class handle
@see class constraint
@see class connection
@see class link

## Menu Bar {#example-Menu-Bar}

Using a menu_bar in a frame

The example below illustrates how a menu_bar object is used to
present commands for a toolframe to the user.   The demo below
uses a plain menu_bar object.  The library(toolbar) defines more
high-level infrastructure to deal with menu- and button bars.

```prolog
:- set_prolog_flag(xpce_threaded, false).
:- use_module(library(pce)).
:- pce_autoload(finder, library(find_file)).
:- pce_global(@finder, new(finder)).

menu_bar_demo :-
	new(F, frame('Menu Bar demo')),

	send(F, append, new(D, dialog)),
	send(new(V, view), below, D),
	send(D, append, new(MB, menu_bar)),
	send(MB, append, new(File, popup(file))),
	send(MB, append, new(Help, popup(help))),

	send_list(File, append,
			  [ menu_item(load,
						  message(V, load, @finder?file)),
			    menu_item(save,
						  message(V, save_buffer),
						  condition := V?modified == @on,
						  end_group := @on),
				menu_item(quit,
						  message(F, destroy))
			  ]),
	send_list(Help, append,
			  [ menu_item(about,
						  message(@display, inform,
							      'By Jan Wielemaker'))
			  ]),

	send(F, open).

% Start the demo

:- initialization(menu_bar_demo).
```

## Multiple fonts {#example-Multiple-fonts}

Using multiple fonts in an editor

The following example show multiple fonts can be used in an editor.
Suppose want to have an editor that displays descriptions in the
proportional helvetica font family and code fragments in a screen font.

The editor will be given an additional key-binding for \C-c\C-c to
convert the selection into a code fragment.

```prolog
%	Create a view that that allows mixing two fonts: one
%   for normal text and one for program-text.

make_code_view :-
	new(V, view('Code Editor Demo')),
	send(V, font, normal),
	send(V, style, code, style(font := fixed)),
	send(V, key_binding, '\\C-c', prefix),
	send(V, key_binding, '\\C-c\\C-c',
	        message(@prolog, make_fragment, V, code)),
	send(V, open).

%	make_fragment(+View, +Kind)
%	Create a fragment for the region (caret, mark) of
%   kind `Kind'.
%	Note that mark and caret may be in any order.

make_fragment(V, Kind) :-
    get(V, selection_start, Start),
	get(V, selection_end, End),
	Length is End - Start,
	new(_, fragment(V?text_buffer, Start, Length, Kind)).

% Start the demo

:- make_code_view.
```

## Obtainers {#example-Obtainers}

Primitive usage of obtainers

These examples presents some elementary ways to use obtainers.
The same mechanism applies to other function objects.

```prolog
%	Prolog has no functions, PCE does:

make_same_width(Gr1, Gr2) :-
	send(Gr1, width, Gr2?width).

%	Pick up arguments from a dialog window:

create_person_dialog :-
	new(D, dialog('Enter new person')),
	send(D, append, new(label)),	% for reports
	send(D, append, new(Name, text_item(name))),
	send(D, append, new(Age, text_item(age))),
	send(D, append, new(Sex, menu(sex, marked))),

	send(Sex, append, female),
	send(Sex, append, male),
	send(Age, type, int),

	send(D, append,
		 button(create, message(@prolog, create_person,
								Name?selection,
								Age?selection,
								Sex?selection))),

	send(D, default_button, create),
	send(D, open).

create_person(Name, Age, Sex) :-
	format('Creating ~w person ~w of ~d years old~n',
		   [Sex, Name, Age]).

%	Start the demo

:- create_person_dialog.
```

@see frame<-get_catch_all
@see device<-member
@see device<-get_catch_all
@see class ?

## Selection {#example-Selection}

Select Graphicals

This example defines a gesture that allows the user to set the selection
of a graphical device to a specified graphical by left-clicking the
graphical.  With a SHIFT-left-click, the graphical is either removed
from or added to the selection.

```prolog
:- pce_global(@selection_gesture,
	   new(handler_group(
	     click_gesture(left, '', single,
		         message(@event?receiver?device,
			      selection, @event?receiver)),
	     click_gesture(left, s, single,
		         message(@event?receiver,
			      toggle_selected))))).

select_demo :-
	new(P, picture('Select Demo')),
	send(P, display,
		 new(Box, box(100, 100))),
	send(P, display,
		 new(Bitmap, bitmap('pce.bm')), point(150,50)),

	send_list([Box, Bitmap], recogniser,
			  @selection_gesture),
	send(P, open).

%	Start the demo

:- select_demo.
```

@see graphical->toggle_selected

## Text inside box {#example-Text-inside-box}

Creating a compound graphical

An effective way to find the references of elements of a
window/device/dialog is using the `graphical <->name` attribute and the
`device <-member` or `device <-catch_all` methods, which provide a
mechanism to decent the consists_of hierarchy of graphicals by name.

The example below defines a box with text and a method to set the
string-value of the text.  It uses user-defined classes.

```prolog
:- pce_begin_class(text_box(string, width, height), device).

initialise(T, S:string, Width:[int], Height:[int]) :->
	"Initialise from string, width and heigth"::
	default(Width, 100, W),
	default(Height, 50, H),
	send(T, send_super, initialise),
	send(T, display, box(W,H)),
	send(T, display, text(S, center)),
	send(T, recenter).

recenter(T) :->
	"Put text in center of box"::
	get(T, member, text, Txt),
	get(T, member, box, B),
	send(Txt, center, B?center).

string(T, S:string) :->
	"Set the string of the text"::
	get(T, member, text, Txt),
	send(Txt, string, S).

string(T, S) :<-
	"Get the string of the text"::
	get(T, member, text, Txt),
	get(Txt, string, S).

:- pce_end_class.

% Start the demo

:- send(new(P, picture('Text box demo')), open),
   send(P, display,
		text_box('Box with centered text', 200, 50),
	    point(50,50)).
```

@see device<-member
@see device<-get_catch_all
@see graphical-name

## Using Conditions {#example-Using-Conditions}

Tests inside a dialog window

This example demonstrates the use of conditional messages to relate
dialog-items to each other.

Using this technique is to be preferred over tests in the application
code to relate and query values of dialog windows.

The example shows a text entry field (text_item) and a `create` button.
Pressing the create button should create a keyword in the application.

First the code is shown doing almost everything in Prolog, next an
alternative using some PCE primitives is given.

```prolog
%	bad_make_keywords_dialog(D)
%
% 	Creates a dialog to generate keywords, using Prolog
%	to link the dialog together.

bad_make_keywords_dialog :-
	new(D, dialog('Create keyword')),
	send(D, append, new(label)),
	send(D, append,
		 text_item(keyword, '',
				   message(@prolog, create, D))),
	send(D, append,
		 button(create, message(@prolog, create, D))),
	send(D, append,
		 button(cancel, message(@prolog, cancel, D))),
	send(D, open).

create(D) :-
	get(?(D, member, keyword), selection, Kwd),
	(   Kwd == ''
	->  send(D, report, warning,
			 'Please first enter a keyword')
	;   create_keyword(Kwd)
	).

cancel(D) :-
	send(D, destroy).

%	good_make_keywords_dialog(?Dialog)
%
%	Creates the same dialog, but using PCE to glue the
%	things together.  This way the distinction between
%	UI and application become more clear.

good_make_keywords_dialog :-
	new(D, dialog('Create keyword')),
	send(D, append, new(label)),
	send(D, append, new(T, text_item(keyword, ''))),
	send(D, append,
		 button(create,
			    if(T?selection == '',
			       message(D, report, warning,
						   'Please enter a keyword'),
			       message(@prolog, create_keyword,
						   T?selection)))),
	send(D, append,
	     button(cancel, message(D, destroy))),
	send(D, default_button, create),

	send(D, open).

create_keyword(Name) :-
	format('Create keyword ~w~n', Name).

% Only start the good one

:- good_make_keywords_dialog.
```

@see class message
@see window<-get_catch_all
@see class label
@see class if
@see class text_item
@see class button
@see class dialog
@see object->has_value

## Window layout {#example-Window-layout}

Defining a window layout

This is an example on how to define the relative position/sizes of
windows in a frame.

The task is to create a frame that has a browser and a picture on top
and a dialog window at the bottom.

```prolog
%	frame_dialog/0
%	Create a frame with a browser and picture at the top and a dialog
%	at the bottom.

frame_dialog :-
	new(P, picture('Frame layout demo')),
	send(new(B, browser), right, P),
	send(new(D, dialog), below, B),
	send(D, append, button(quit, message(D, destroy))),
	send(P, open).

% Start the demo

:- frame_dialog.
```

@see topic Window Layout
@see class tile
@see class frame

