# Topics {#sec-topics}

## Activating commands {#sec-topics-activating-commands}

Menus, direct manipulation, etc.

*Parent*: User Interface

@see class menu_bar

## Arithmetic {#sec-topics-arithmetic}

Classes to do arithmetic

*Parent*: Class Hierarchy

## Atom {#sec-topics-atom}

Character constant

*Parent*: Primitive Types

## Attic {#sec-topics-attic}

Old Techniques

*Parent*: Programming Style

*Subtopics*: Current

## Attributes {#sec-topics-attributes}

Using attributes to objects

*Parent*: Programming Style

## Automatic layout {#sec-topics-automatic-layout}

Automatic layout of Dialog Items

*Parent*: Dialogs

## Calling Prolog {#sec-topics-calling-prolog}

Object to represent Prolog

PCE can invoke Prolog predicates via the predefined object @prolog by
sending a message there.  When a message is sent to @prolog, the selector
of the message will be used as predicate name and the arguments will be
transformed in `reference` mode (see `T Object -> Term`) to Prolog terms
and then passed as arguments to the predicate.

Normally, the message to Prolog will be activated through a message
object associated with a UI object (similar to call-back in many C-based
window packages).  Examples:

	1 ?- new(B, button(ok, message(@prolog, do_xyz))).
	2 ?- new(T, text_item(file, '', message(@prolog, consult, @arg1))).

The first example invokes the predicate do_xyz/0 when the user presses
the button.  The second invokes consult/1 when the user hits RETURN
with the typed value as argument.

The examples above illustrate how PCE can invoke predicates in Prolog.
Sending the PCE message succeeds or fails, depending on success or
failure of the Prolog predicate invoked.  PCE may also obtain
information from Prolog by invoking a get method on @prolog.  The return
value is passed through the last argument.  Normally, Prolog is called
like this through an obtainer used to implement a get-method of a
Prolog-defined class.  Example:

	area2(Gr, Area) :-
		get(Gr, width, W),
		get(Gr, height, H),
		Area is W * H.

	?(@prolog, area2, Gr)

In this example the obtainer evaluates to the area of the graphical.

*Parent*: Prolog Interface

@see Object -> Term

## Changes {#sec-topics-changes}

Trapping changes to objects

*Parent*: Object Management

@see object->inspect

## Checking {#sec-topics-checking}

Checking a type

*Parent*: Types

@see method-types
@see class ?

## Class Hierarchy {#sec-topics-class-hierarchy}

Conceptual hierarchy of classes

*Parent*: Contents

*Subtopics*: Executable Objects, Graphics, Dialogs, Text Manipulation, Windows, Data Representation, Constraints, Arithmetic, Environment, Programming

## Class-level Programming {#sec-topics-class-level-programming}

Creating and extending Classes

*Parent*: Programming PCE

@see class sheet
@see class class
@see object<-get_super
@see object->send_super

## Classes {#sec-topics-classes}

Using PCE user-defined classes

*Parent*: Programming Style

## Classes {#sec-topics-classes}

How to create new classes

*Parent*: Programming

## Collections {#sec-topics-collections}

Collections of Objects

*Parent*: Data Representation

*Subtopics*: 144

## Commanders {#sec-topics-commanders}

Graphicals with predefined UI

*Parent*: Dialogs

## Compound {#sec-topics-compound}

Compound Statements

*Parent*: Executable Objects

## Compound Graphicals {#sec-topics-compound-graphicals}

Using devices to create compound objects

*Parent*: Graphics

@see class device

## Conditions {#sec-topics-conditions}

Conditions expressions

*Parent*: Executable Objects

@see class primitive_code
@see class >=
@see class =<
@see class =
@see class <
@see class \==
@see class not
@see class or
@see class and
@see class ==

## Connections {#sec-topics-connections}

Connecting graphicals

Connections define a line connecting two graphical objects.  Changing
either of the graphical objects will update the connection
automatically.

The END-POINTS of the connections on each of the graphicals is
determined using a _HANDLE_.  A handle has a `name` and a `kind`.  The
`name` of a handle should be unique over all handles attached to a
graphical.  The `kind` need not be unique.  Each side of a connection
can attach to a handle of specified type and possible only with a given
name.  When only the kind is determined and there are more than one
handle of the specified kind, the connection will attach to that handle
that `looks best`.  Handles can be attached both at the class level
using `Class ->handle` or at the instance level using `Graphical
->handle'.

The initial ATTRIBUTES of the line (arrows, pen, texture) may be
instantiated from a `link`.  See _E Linking Graphicals_.

*Parent*: Graphics

@see class->handle
@see class-handles
@see graphical->layout
@see graphical-handles
@see graphical->handle
@see graphical->disconnect
@see graphical-connections
@see graphical->connected
@see graphical->connect
@see class link
@see class connection

## Constraints {#sec-topics-constraints}

Constraints between object pairs

*Parent*: Class Hierarchy

*Subtopics*: Linking Graphicals, Relating Attributes

## Contents {#sec-topics-contents}

Root of the topic index

*Subtopics*: Host Interface, Class Hierarchy, Global Objects, Prolog Interface, Programming PCE, Object Management, Types, Link to application, Errors, Exceptions, Debugging, Manual, Programming Style, Techniques, User Interface, Events, X-windows

## Conversion {#sec-topics-conversion}

Automatic type conversions

If the type-checker detects a type-error, the type-converter in
activated.   Depending on the required type, PCE does the following
data conversions:

- name
	Translates string-objects, integers and real numbers.

- int
	Translates number objects, real numbers or text (name, string)
	who's string represents an integer.

- bool
	Converts 0/1, and the texts `true/false`.

- type
	Converts the normal type specification into an internal type
	specifier.

- class-name
	Invokes `Object <-convert`.   Consult the <-convert method of
	a class to find out which data-types are converted.

*Parent*: Types

@see Specification
@see class-convert_method
@see method-types
@see pce<-convert

## Creating Objects {#sec-topics-creating-objects}

Create instances of classes

*Parent*: Object Management

@see tool Statistics
@see object->initialise

## Current {#sec-topics-current}

Using the notion of `current`

_Long Time Ago_, there was a system called PCE that could not handle
multiple arguments to methods and had little flexibility in code
objects.  With those restrictions, the notion of `current` (cell,
dialog_item, menu_item, ...) has been introduced to avoid the need for
multiple arguments.

The use of this notion is dubious: mutual recursive procedures both
using this technique will not behave properly.  Also, the current is
not really a describing attribute of the object.  The methods -being
present for so long- have been retained in this version, but please
avoid using them.

*Parent*: Attic

@see chain<-next
@see chain->insert
@see chain->delete_current
@see chain<-current_no
@see chain->current_no
@see chain<-current
@see chain->current
@see chain-current

## Data Representation {#sec-topics-data-representation}

Classes to represent data

*Parent*: Class Hierarchy

*Subtopics*: Text, Primitive Types, Tables, Collections

## Debugging {#sec-topics-debugging}

Tools for debugging PCE/Prolog programs

*Parent*: Contents

*Subtopics*: Finding References, Tracing, Visual Hierarchy, Inspector

@see class pce
@see tool Inspector

## Defining Classes {#sec-topics-defining-classes}

Defining PCE classes from Prolog

This section deals with the definition of classes in Prolog.  Studying
example programs is probably the fastest way to learn about Prolog
defined classes.  There are two large example programs in the
distribution.  The first is PceDraw, a drawing tool.  It's sources are
in the subdirectory `draw` of the PCE/Prolog library.  The second
example is the online manual tools you are reading now.  Its sources are
in a subdirectory `man` of the PCE/Prolog library.

*Parent*: Prolog Interface

*Subtopics*: Overview, Variables, Resources, Handles, Send Methods, Get Method, Delegation, Types, The compiler

@see class class

## Delegation {#sec-topics-delegation}

Adding delegation to related objects

*Parent*: Defining Classes

@see object->send_vector

## Dialog windows {#sec-topics-dialog-windows}

Windows to display Dialog Items

*Parent*: Dialogs

## Dialogs {#sec-topics-dialogs}

Windows and commanders to create dialogs

*Parent*: Class Hierarchy

*Subtopics*: Dialog windows, Commanders, Typing, Automatic layout

## Directives {#sec-topics-directives}

PCE related directives

The PCE/Prolog interface defines a number of special directives:

	pce_global(Ref, Pred|New)	% Declare global object
	pce_autoload(Class, File)	% File defines class
	pce_begin_class(Class, Super)	% Start class definition
	pce_end_class.		% End class definition

*Parent*: Prolog Interface

## Editors {#sec-topics-editors}

EMACS-look alike editing

*Parent*: Text Manipulation

## Enumerate {#sec-topics-enumerate}

Enumerate members of a collection

*Parent*: Techniques

@see class while

## Environment {#sec-topics-environment}

Interface objects to the environment

*Parent*: Class Hierarchy

*Subtopics*: Unix, The PCE environment, The display, Interface to Prolog

## Errors {#sec-topics-errors}

Handling runtime errors

*Parent*: Contents

@see class pce

## Event dispatching {#sec-topics-event-dispatching}

Dispatching events over graphical objects

*Parent*: Graphics

@see object->recogniser

## Event types {#sec-topics-event-types}

Hierarchical definition of event types

*Parent*: Events

@see device->update_pointed
@see class handler

## Events {#sec-topics-events}

User events from mouse and keyboard

*Parent*: Graphics

## Events {#sec-topics-events}

Creating and dispatching user events

*Parent*: Contents

*Subtopics*: Event types, Events vs. Messages, Handling Events

@see class event

## Events vs. Messages {#sec-topics-events-vs-messages}

How do events relate to messages?

*Parent*: Events

## Exceptions {#sec-topics-exceptions}

Trapping interface exceptions

*Parent*: Prolog Interface

*Subtopics*: 18, 17

## Exceptions {#sec-topics-exceptions}

Handling runtime exceptions

*Parent*: Contents

@see class pce

## Executable Objects {#sec-topics-executable-objects}

Classes of objects for programming

*Parent*: Class Hierarchy

*Subtopics*: Parameters, Compound, Conditions, Primitives

## Finding References {#sec-topics-finding-references}

Finding references of graphicals

*Parent*: Debugging

@see display->inspect_handler

## Garbage collection {#sec-topics-garbage-collection}

Incremental and explicit Garbage collection

*Parent*: Object Management

@see tool Statistics
@see tool Inspector
@see object<-references

## Gestures {#sec-topics-gestures}

Handle sequences of events

*Parent*: Handling Events

@see class gesture

## Get Method {#sec-topics-get-method}

defining get methods in Prolog

*Parent*: Defining Classes

## Global Objects {#sec-topics-global-objects}

Predefined global objects

*Parent*: Contents

*Subtopics*: 22, 85, 31, 30

## Global references {#sec-topics-global-references}

Named global object references

*Parent*: Programming Style

@see object->name_reference

## Graphics {#sec-topics-graphics}

Classes to do graphics

*Parent*: Class Hierarchy

*Subtopics*: Connections, Event dispatching, Events, Primitive Graphics, Compound Graphicals, Graphics Devices

## Graphics Devices {#sec-topics-graphics-devices}

Collections of Graphicals

*Parent*: Graphics

## Handles {#sec-topics-handles}

Adding handles for connections

Graphical classes only.

*Parent*: Defining Classes

@see class->handle

## Handling Events {#sec-topics-handling-events}

Creation and dispatching events

*Parent*: Events

*Subtopics*: Gestures

@see class recogniser

## Host Interface {#sec-topics-host-interface}

Interface description for host-language connection

*Parent*: Contents

*Subtopics*: hostAction(), hostQuery()

## Inspector {#sec-topics-inspector}

Inspect individual objects at runtime

*Parent*: Debugging

## Integer {#sec-topics-integer}

Integer number

*Parent*: Primitive Types

## Interface to Prolog {#sec-topics-interface-to-prolog}

Calling Prolog via @prolog

*Parent*: Environment

## Kloning objects {#sec-topics-kloning-objects}

Creating a clone of an object

*Parent*: Object Management

@see class-klone_style
@see class-klone_function

## Link to application {#sec-topics-link-to-application}

Relating UI and application

*Parent*: Contents

## Linking Graphicals {#sec-topics-linking-graphicals}

Creating a link between graphicals

*Parent*: Constraints

## Loading objects {#sec-topics-loading-objects}

Loading objects from file

*Parent*: Object Management

## Manual {#sec-topics-manual}

Online Manual Documentation

*Parent*: Contents

*Subtopics*: 77, 80, 79, 81, 78

@see tool PCE Manual

## Methods {#sec-topics-methods}

Defining methods to individual objects

*Parent*: Programming Style

## Methods {#sec-topics-methods}

Adding methods to classes and objects

*Parent*: Programming

@see object<-get_super
@see object->send_super

## Naming conventions {#sec-topics-naming-conventions}

How to name you objects

*Parent*: Programming Style

## Object -> Term {#sec-topics-object-term}

Transforming a PCE object to a Prolog term

The principal predicates that request a value from PCE can either
request a reference or a Prolog term representing the object.  In either
case, primitive types (integer and name) are given as their
corresponding Prolog primitive types.

Get requests its value in `reference` mode.  The other predicates
request their value in `term` mode.  In both cases, when the prolog
argument is not a variable nor a term @/1, the PCE value is converted to
a Prolog term and unified with the Prolog argument.

When a PCE object is transformed into a Prolog term, the functor
determines the classname.  The arguments normally correspond to the
arguments that must be provided to create the object.

Actually the number of arguments is determined by `Object <-_arity` and
the successive arguments by `Object <-_arg: Nth1`.  These behaviours
are either provided by the class itself, or by class `object`, that uses
the class attribute `term_names`, which is a vector of selectors to be
used to extract the successive arguments.

*Parent*: Prolog Interface

@see Calling Prolog
@see tool Inspector
@see tool ClassBrowser
@see object<-functor
@see object<-_arity
@see object<-_arg
@see class-term_names
@see class-term_functor
@see Term -> Object

## Object Management {#sec-topics-object-management}

Creating, Destroying, etc.

*Parent*: Contents

*Subtopics*: Kloning objects, Creating Objects, Removing Objects, Garbage collection, Loading objects, Saving to File, Changes

## Object-level Programming {#sec-topics-object-level-programming}

Extending individual objects

*Parent*: Programming PCE

@see class sheet
@see class object

## Obtainers {#sec-topics-obtainers}

Functional programming in PCE

*Parent*: Programming Style

@see class ?

## Overview {#sec-topics-overview}

Skeleton of a User-Defined class

*Parent*: Defining Classes

## Parameters {#sec-topics-parameters}

Forwarding arguments

*Parent*: Executable Objects

@see class code

## Predicates {#sec-topics-predicates}

Interfacing Predicates

*Parent*: Prolog Interface

*Subtopics*: Principal, Utilities

## Primitive Graphics {#sec-topics-primitive-graphics}

Primitive graphical objects

*Parent*: Graphics

## Primitive Types {#sec-topics-primitive-types}

Primitive data representation types

*Parent*: Data Representation

*Subtopics*: Integer, Atom

## Primitives {#sec-topics-primitives}

Primitive executable objects

*Parent*: Executable Objects

## Principal {#sec-topics-principal}

Basic interfacing predicates

*Parent*: Predicates

*Subtopics*: 7, 8, 9, 10, 11, 72

## Programming {#sec-topics-programming}

Classes for programming PCE

*Parent*: Class Hierarchy

*Subtopics*: Resources, Variables, Methods, Classes

## Programming PCE {#sec-topics-programming-pce}

Extending objects or classes in PCE

*Parent*: Contents

*Subtopics*: Object-level Programming, Class-level Programming

## Programming Style {#sec-topics-programming-style}

Notes on programming style

*Parent*: Contents

*Subtopics*: Attic, Obtainers, Naming conventions, Global references, Classes, Methods, Attributes

## Prolog Interface {#sec-topics-prolog-interface}

Interface to @prolog

*Parent*: Contents

*Subtopics*: 113, Predicates, Directives, Calling Prolog, Object -> Term, Term -> Object, Defining Classes, Exceptions, Warnings

## Relating Attributes {#sec-topics-relating-attributes}

Constraints to relate attributes

*Parent*: Constraints

## Removing Objects {#sec-topics-removing-objects}

Removing objects from the database

*Parent*: Object Management

@see tool Statistics
@see object<-_flags
@see object->protect
@see object->free

## Resource Conversions {#sec-topics-resource-conversions}

Types and conversions that apply to resources

*Parent*: Resources

@see class resource

## Resources {#sec-topics-resources}

Adding X-resources to classes

*Parent*: Programming

## Resources {#sec-topics-resources}

Adding resources to the class

*Parent*: Defining Classes

## Resources {#sec-topics-resources}

Interface to X resources

*Parent*: X-windows

*Subtopics*: Resource Conversions

@see class resource

## Saving to File {#sec-topics-saving-to-file}

Saving collections of objects to file

*Parent*: Object Management

@see object->save_in_file

## Send Methods {#sec-topics-send-methods}

Defining send methods in Prolog

*Parent*: Defining Classes

## Specification {#sec-topics-specification}

Specifying a type

Type specifiers are used both for variables as for methods.  It consists
of a `primitive type specifier` with optional modifiers.  A primitive
type specifier either specifies a primitive type or a node in the class
hierarchy.  In the latter case, any instance that is a member of the
specified class or one of its subclasses passes the type test.

Type specifiers are no true objects in PCE, but special types similar to
integers, names, etc.  The PCE type conversion system converts names to
types as required.

The specifiers for primitive types are:

	int	Integer
	bool	Boolean
	name	equivalent to `atom`
	atom	atom
	type	a type specifier

The modifiers are:

	[<type>]		Specified type or @default
	<type>*		Specified type or @nil
	<type>?		Specified type, obtainer or @arg1...@arg10

## Examples {#sec-topics-examples}

	'graphical*'		Any graphical object or @nil
	'[point]'		Any point object or @default
	'[name]*'		Name, @default or @nil

NOTE:	Type `object` refers to any valid PCE data-type.  This will be
	changed to:

	object		Any object (i.e. instance of some class)
	any		Any valid PCE data type

*Parent*: Types

@see Conversion
@see class variable
@see class method
@see Types

## Tables {#sec-topics-tables}

Mapping/Association tables

*Parent*: Data Representation

## Techniques {#sec-topics-techniques}

Various programming techniques

*Parent*: Contents

*Subtopics*: Enumerate

## Term -> Object {#sec-topics-term-object}

Transforming a Prolog term to a PCE object

Some of the interface predicates create PCE objects from Prolog terms.
This creation can be done in two modes: either by implicit
transformation if an argument is required (mode `arg`) or explicit
transformation if an object reference is to be produced (mode `new`).

Normally, Prolog primitive data types are transformed into corresponding
PCE data types.  Complex terms are transformed into instances.  For this
transformation, the functor determines the classname and the arguments
serve as arguments to the ->initialise method of the class.

Below is the table of transformations as performed by new/2, send/[2-12]
and get/[3-13].  New/2 does the outermost argument in `new` mode.  All
other arguments are processed in `arg` mode.

      Mode	Prolog Data	Pce Object
      =======================================================
         -	integer		integer
         -	real number	Real object
       new	atom		Instance of the class (no arguments)
        arg	atom		name
         -	Class(...Arguments...)	Instance of Class from arguments
         -	new(Data)		Transform in `new` mode.
         -	new(?Ref, Data)	Transform in `new` mode, unify _Ref_

*Parent*: Prolog Interface

@see Object -> Term

## Text {#sec-topics-text}

Representing text in PCE

PCE has various primitives to represent text.  These are names (atoms),
strings and text_buffers.  Which of them is most suitable depends on the
operations that are needed.

- NAME
	Names are like atoms in Prolog.   They are internally passed
	and manipulated as identifiers into a name-table.  These
	identifiers are unique: the same identifier guarantees the same
	name and different identifiers garantees different names.

	Names cannot be changed and the memory they use is never
	reclaimed.

	Names are a suitable representation for IDENTIFIERS.

  	* STRING
	A string is an object that represents a sequence of ASCII
	values.  Strings can be manipulated, but changing a string
	will often cause a reallocation in memory and a copy, making
	them not suitable for long texts that are often manipulated.

	Strings are often used to store descriptions and not-too-often
	changing texts.

- TEXT_BUFFER
	A text_buffer is used to store the text manipulated by an
	editor.  Text_buffers are designed to deal with long texts and
	changes.  They are rarely used disjoint from an editor.

*Parent*: Data Representation

@see class text_buffer
@see class string

## Text Entry Fields {#sec-topics-text-entry-fields}

Text entry fields for a dialog

*Parent*: Text Manipulation

## Text Manipulation {#sec-topics-text-manipulation}

Editors and other text manipulation issues

*Parent*: Class Hierarchy

*Subtopics*: Text as Graphics, Text Entry Fields, Editors

## Text as Graphics {#sec-topics-text-as-graphics}

Displaying text in pictures

*Parent*: Text Manipulation

## The PCE environment {#sec-topics-the-pce-environment}

Interface to PCE's internals

*Parent*: Environment

## The compiler {#sec-topics-the-compiler}

How the class compiler works

This card provides a brief explanation of the transformations used when
defining a class with :- pce_begin_class, ... :- pce_end_class. This is
especially useful to understand and debug code written with user-defined
classes.


## Term expansion and operators {#sec-topics-term-expansion-and-operators}

The expansion is realised by _:- pce_begin_class/[2,3]_.  This predicate
extends the definition of the Prolog primitive term_expansion/2 using
asserta/2 to make sure it is in front of any other expansion.  The
returned term is a list, containing a directive that should be sent to
PCE and a clause that realised the implementing predicate.
Pce_begin_class/3 also declares a number of operators, notably
:->, :<-, ::, * and ?.


## Constructing the prolog head and the send_method {#sec-topics-constructing-the-prolog-head-and-the-send_method}


The head is used to construct a send_method object.  The selector of
this send method is the functor of the head.  The type specification is
used to create the type-check vector for the send_method.  The
implementing method is a message to @prolog.  The selector (which
equals the predicate name) is

	`send_Selector_Class`

and the arguments are @receiver, @arg1, @arg2, ... (just as many
arguments as the method has).


## Example {#sec-topics-example}

Consider the following code fragment:

:- pce_begin_class(label_text(string, name), text).

resource(label_font, font, '@helvetica_bold_14',
             "Font used for labels").

initialise(Text, String:string, Format:[name]) :->
	"Initialise from string and format"::
	send(Text, send_super, initialise(String, Format,
			               ?(Text, resource_value, label_font))).

:- pce_end_class.

The call to pce_begin_class/2 will create the class and set the
<->term_names variable of the class.  The resource/4 declaration will
attach a resource (e.i.  interface to the X-windows default database)
to the class) using `Class ->resource`.  The `initialise` declaration
will attach a method to the class and assert a clause in the Prolog
database.  The result of the translation will be:

send(@label_text_class, send_method,
	send_method(initialise, vector(string, '[name]'),
		    message(@prolog, send_initialise_label_font,
			  @receiver, @arg1, @arg2))).

and the clause

send_initialise_label_text(Text, String, Format) :-
	send(Text, send_super, initialise(String, Format,
			               ?(Text, resource_value, label_font))).

The call to pce_end_class/0 deletes the term_expansion/2 clause inserted
by pce_begin_class/3 and destroys all operator declarations inserted by
this call.

*Parent*: Defining Classes

## The display {#sec-topics-the-display}

Interface to the X-display (screen)

*Parent*: Environment

## Tracing {#sec-topics-tracing}

Tracing PCE's execution

*Parent*: Debugging

@see pce->debug_subject
@see pce->debug
@see pce->spy
@see pce->trace
@see method<-trace_message
@see method->notrace
@see method->trace

## Types {#sec-topics-types}

Specifying a type

Both the declaration of send- and get methods requires type
declarations.  The type declarations for methods look very similar than
those that can be found in this manual.  A type either denotes a
primitive type or a classname.  In the latter case, any instance of a
the referred class or a subclass thereof is a value satisfying the
typecheck.  While declaring a type using a classname, the class itself
does not need to exist yet.  This facilitate the definition of classes
that are mutually dependent and thus want to use each other's name for
typechecking.

 The available primitive types are:

	bool	a boolean (@on or @off).
	int	Integer value
	name	Atomic name (`atom` is a synonym).

This basic type declaration can be augmented with some modifiers,
indicating what should be done with the various special objects:

	[Type]	Argument may be @default (or, in other words,
		argument is optional).
	Type*	Argument may be @nil.
	Type?	Special arguments (@receiver, @arg1, ... and
		obtainers are not evaluated, but passed themselves

Both _*_ and _?_ are declared as postfix operators, which allows you to
specify the type without using quotes.


## Examples {#sec-topics-examples-2}

	point	An instance of class point
	string*	Either a string object or @nil
	[font]	Either a font or @default
	code?	An executable object; pass obtainers

*Parent*: Defining Classes

@see Variables
@see Specification

## Types {#sec-topics-types}

Defining and using types

*Parent*: Contents

*Subtopics*: Specification, Checking, Conversion, Warnings

@see class method

## Typing {#sec-topics-typing}

Typing in Dialog Boxes

*Parent*: Dialogs

## Unix {#sec-topics-unix}

Interface to Unix

*Parent*: Environment

## User Interface {#sec-topics-user-interface}

Hints on realising user interfaces

*Parent*: Contents

*Subtopics*: Activating commands, Using menu's

## Using menu's {#sec-topics-using-menu-s}

Menu-bars vs. popup menu's

*Parent*: User Interface

@see class popup

## Utilities {#sec-topics-utilities}

Predicates defined in the principal predicates

*Parent*: Predicates

*Subtopics*: 12, 71, 70, 13

## Variables {#sec-topics-variables}

Adding instance variables to classes and objects

*Parent*: Programming

## Variables {#sec-topics-variables}

Adding instance variables to the class

Instance variables (slots, attributes) are attached to the new class
using the variable/[3,4] declaration.  The instance variables of a class
cannot be changed when there are subclasses or instances of the class.
Examples:

	variable(name,	name,	both, "Name of the person").
	variable(photo,	bitmap*,	both, "Image of photo").

This example declares two variables.  The first is the name, which must
always be filled with an atom.  The second is an image which may be @nil
(if not available) or a bitmap object.

*Parent*: Defining Classes

@see Types

## Visual Hierarchy {#sec-topics-visual-hierarchy}

Examine visual consists-of hierarchy

*Parent*: Debugging

## Warnings {#sec-topics-warnings}

Warnings that may come from the interface

*Parent*: Prolog Interface

## Warnings {#sec-topics-warnings}

Typc checking errors/warnings

*Parent*: Types

## Window Layout {#sec-topics-window-layout}

Window layout in a frame

The layout of windows in a frame is normally specified using `Window
->below', `Window ->above`, `Window ->left` and `Window ->right`.

How the windows are resized when the frame is resized is determined by
their stretchabilities and schrinkabilities (`Window <->hor_stretch`,
`Window <->ver_stretch`, `Window <->ver_shrink` and `Window
<->hor_shrink').

The layout of windows in a frame is viewed as an `consists_of` hierarchy
of rows of windows that are either stacked left-to-right or
top-to-bottom.  The layout specification starts by building the
inner-most stack.  Suppose we want to make the following layout:

	-------------------------
	|                 Dialog                      |
	-------------------------
	|              |            Picture           |
	| browser  |-----------------
	|              |             View             |
	-------------------------

This is realised by the following sequence of messages:

	new(F, frame('My Frame')),	% Create the frame
	send(F, new(P, picture)),	% Start with picture (or view)
	send(new(view), below, P),	% Stack the view
	send(new(browser, left, P),	% Put the browser left
	send(new(dialog, above, P).	% And the dialog on top

Thus, if two simple windows are related, a `horizontal` or `vertical`
stack is created.  Each new window that is related in the same direction
to one of the outermost windows enlarges the stack.  If a window is
related in a different direction to any member of the stack, a new stack
is created with the window and the old stack as members.

*Parent*: Windows

@see frame->append
@see window->right
@see window->left
@see window->below
@see window->above
@see class tile
@see class window
@see class frame

## Window Manager {#sec-topics-window-manager}

Interaction with the Window manager

*Parent*: X-windows

## Windows {#sec-topics-windows}

Frames and window classes

*Parent*: Class Hierarchy

*Subtopics*: Window Layout

## X-windows {#sec-topics-x-windows}

Description of X-window interface policy

*Parent*: Contents

*Subtopics*: Window Manager, Resources

@see class display

## hostAction() {#sec-topics-hostaction}

Pce asks host system to perform some action

*Parent*: Host Interface

## hostQuery() {#sec-topics-hostquery}

PCE queries the host system

The host-language interface must define a C-function hostQuery, which
allows PCE to acquire information about the environment.  Below is a
skeleton of this function:

int
hostQuery(what, value)
int what;
PceValue *value;
{ switch(what)
  { case HOST_SYMBOLFILE:
	<Your code>
	return PCE_FAIL;
    case HOST_GETC:
	<Your code>
	return PCE_SUCCEED;
    default:
	fprintf(stderr, "Unknown query from PCE: %d", what);
	return PCE_FAIL;
  }
}

The defined requests are:

- HOST_SYMBOLFILE
	Assign a char * describing a path-name to a Unix symbol-file
	describing the current process.  Used by PCE to produce a
	C-stack-trace when a fatal error occurs

- HOST_GETC
	Read a character from the terminal.

*Parent*: Host Interface

@see pce-print_c_stack

## new/2 {#sec-topics-new-2}

Create a PCE object

## predicates {#sec-topics-predicates}

Prolog predicates to support PCE

