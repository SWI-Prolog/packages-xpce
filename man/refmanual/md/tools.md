# Tools {#sec-tools}

## Card Viewer {#sec-tools-card-viewer}

Examine/edit a manual card

The _Card Viewer_ allows you to read the textual contents of a
card.  It is normally started by double-clicking on a card in
one of the summary lists (for example from the ClassBrowser).

Notation.  The card editor may display multiple cards simultaneously
Each card is started using by a bold-printed header-line.  If multiple
cards with the same description are displayed by the card viewer
this tool will put a header for each displayed card above the actual
description.   The type-indicator of the header of the card the
description originates from is *underlined*.

**User interface**:

**Jumping around**:

- *double-clicking* on a bold-printed `button` in the
	running-text jumps to the related card.

- META-p jumps to the previous card.   Jumping to
	other already-visited cards is achieved using the
	`history` entry of the manual's menu bar.

- The popup menu of the card viewer defines the
	following options:

		Jump				As double-click (follow link)
		Previous			Jump back (META-p)
		Source				View related sourcecode
		Consult				Consult code fragment
		Describe			Describe *this* card (edit)
		Show Key Bindings	All keyboard accelerators

	The options _Source_ and _Describe_ are also active
	on the header lines and allow the user to specify a
	specific object if multiple cards with the same
	description are displayed.

**Tool name**: `card_viewer`

@see Summary List
@see Topic Browser
@see Predicate Browser
@see Object Browser
@see Example Browser
@see ClassBrowser
@see class editor
@see Class Hierarchy
@see PCE Manual

## Class Hierarchy {#sec-tools-class-hierarchy}

Overview PCE's class hierarchy

The _Class Hierarchy_ browser provides a hierarchical overview of
PCE's class inheritance hierarchy.  Initially, it displays the root and
first level of sub-classes.

Classes are represented by a small box-icon, followed by their
name.  The fron-side of the box-icon indicated whether the class
is built-in (a house) or user-defined (a face).

The _Classes Hierarchy_ browser automatically displays user-defined
classes when they are created.

**User interface**:

The text-entry-field CLASS may be used to locate classes in the
hierarchy.  The argument is a pattern (see `regex->file_pattern`).
All matching classes will be added to the hierarchy and
selected.

LEFT-CLICKING a node will try to make that class the selection (i.e.  it
will show a description of the class in the _Card Viewer_.  If the `Card
Viewer' is not yet opened, DOUBLE-LEFT-CLICK opens it.

RIGHT-DOWN on a node presents a POPUP-MENU, providing access
to the ClassBrowser and source-code (if the class is user-defined).

**Tool name**: `class_hierarchy`

@see Topic Browser
@see Card Viewer
@see ClassBrowser
@see PCE Manual

## ClassBrowser {#sec-tools-classbrowser}

Examine/find class attributes

The _ClassBrowser_ finds cards that are related in some way to
the `current class`.  It can be started from the _PCE Manual_
window, or from any card that represents a PCE class by
selecting the _ClassBrowser_ option.  Some of the runtime
environment tools (Visual Hierarchy and Inspector) also allow
you to jump to the ClassBrowser.  Finally, the ClassBrowser
may be started directly using the predicate manpce/1.

**User interface**:

## Windows {#sec-tools-windows}

This tool consists of three windows.  The dialog at the left is
used to specify a query for the types of cards you are
interested in.  The window at the top indicates the current
class, the initialisation arguments and the inheritance- and
delegation relations of the class.  The browser shows all cards
that match the query.


## The dialog window {#sec-tools-the-dialog-window}

The primary goal of the dialog window is to specify a query for
finding class related cards.  As almost anything is an object,
most cards can be found this way.  Below is a specifiation of
the entry fields,

- CLASS
	May be used to switch to another class.  A partial
	class-name is completed by hitting the space-bar.

- FILTER
	Determines the class(es) of cards you are interrested
	in.  *Basic* refers to the principal and most commonly
	used items, *Advanced* to less frequently used material,
- Rare* to rarely used items, *Internal* to items that
	should not be used by normal applications, *Basic OO* to
	methods that are commondly refined in subclasses or
	methods that are used in method definitions.  *Advanced
	OO* are methods for less commonly used refinements of
	classes and *Application* refers to methods defined in
	the application language.

- DISPLAY
	Select those parts of the class definition you want
	to consider.  Values are:

		SELF		The current class itself
		SUB-CLASS	All immediate sub-classes
		VARIABLE	Associated instance variables
		RESOURCE	Associated X-resources
		SEND-METHOD	Associated send behaviour
		GET-METHOD	Associated get behaviour

- SEARCH
	Specifies a string to search for.  All candicate cards
	specified are searched for this string, and each card
	holding this string in on of the fields specified in
	'...  In:' will be put in the browser at the right.
	Searching is not case sensitive.

	# ... IN
	This specifies the textual attributes that are searched if a
	search-string is specified in the SEARCH entry-field.

	Typing RETURN is equivalent to clicking APPLY; it starts
	the actual search.

- APPLY
	This button is activated whenever one of the above describes
	specification fields is changed.  Pressing it will start the
	search and display the found cards in the browser.


## Top-right window {#sec-tools-top-right-window}

The first line indicates the class name and initialisation
arguments (see `object ->initialise`).  Double-clicking this
field shows the card belonging to the ->initialise method.

The second graphical object is a tree object that indicates the
location in inheritance tree of the current class, as well as
delegation performed by this class.   All classes in the primary
inheritance path are displayed in *bold* font.  Classes to which
delegation takes place are added in the default font.  Dotted
(on colour displays: green) lines indicate delegation relations.

Notes in this tree may be selected to indicate the scope of the
search.  All classes below selected nodes are considered when
searching for methods to display.  Double clicking selects the
class clicked upon and activates the *Apply* button.


## Bottom-left browser {#sec-tools-bottom-left-browser}

This is a standard list of cards.  LEFT-CLICK shows the card, the menu
associated with the right-button gives the usual further options.

**Tool name**: `class_browser`

@see Visual Hierarchy
@see Summary List
@see class resource
@see class class
@see class send_method
@see class get_method
@see class variable
@see topic Object -> Term
@see Card Viewer
@see Class Hierarchy
@see PCE Manual

## Errors {#sec-tools-errors}

Examine errors/warnings

The _Errors Browser_ visualises all defined error objects.  It may be
used to examine defined errors, get an extensive description of an error
or change the kind of an error.  The variable `error <-kind` describes
how the system reacts when such an error is encountered:

- ignored
	The action fails silently

- status
	The user is informed without attracting attention (see
	`object ->report`.  If there is no place to display the message
	without attracting attention it is ignored.

- inform
	The user is informed while attracting attention.  This kind of
	message is always displayed.

- warning
	The user is informed of a warning.  Attracts attention.

- error
	As a warning, but the PCE tracer is started to allow for
	debugging.

- fatal
	A message is printed, the stacks are printed (see also
	`@pce ->catch_error_signals`, `@pce ->print_c_stack`).
	PCE attempts to abort back to the host-language toplevel

It is not advised to change `fatal` messages into another kind.

**User interface**:

The user interface of this tool is similar to the other overview tools.

If a search string is specified only cards that match the regular
expression entered will be shown.

If an error is selected, the `kind` menu is activated.  It may be used
to change the `kind` of the error.  For example the error `no_behaviour`
is generated if a message is send to an object and the object has no
method implementing this message.  Setting `kind` to `ignored` will make
the message silently fail; `message` will print it, but continue
execution and `warning` will trap the tracer.  The latter allows you to
inspect PCE's goal-stack.

**Tool name**: `errors`

@see @errors

## Example Browser {#sec-tools-example-browser}

Overview of examples

This tools displays an overview of all examples.  Currently the examples
themselves are displayed using the _Card Viewer_.  A more dedicated
visualisation that allows the user to run the code is planned.

**User interface**:

Standard card summary browser.  After opening a card which is
viewed by the normal card viewer tool a section called `code` is
available.  This code may be consulted in the Prolog process
using the option _Consult_ from the popup menu in the code
section of the card.

After consulting, you may normally run the example by typing a
relevant query in the Prolog window.

Copy and Paste may be used to copy (fragments) of the examples
into your application.

**Tool name**: `examples`

@see Summary List
@see Card Viewer

## Group Overview {#sec-tools-group-overview}

Overview of Functional Groups

The _Group Overview_ tool allows the user to browse through the
defined functional groups.  A functional group is a collection
of methods and variables that perform a similar or related
action.

The window to the left displays the defined functional groups in
the same order as they appear in the card summary overviews.
This order is based on the notion of `common use`.  Typing in
this window starts incremental search.  Hitting RETURN or
double-clicking will display all related methods and variables
in the summary window at the right-side of the tool.

The window to the right is a standard card summary window and
may be used to start the card viewer on a specific card or all
cards by selecting the (top) group-header-line.

**Tool name**: `group_overview`

## Inspector {#sec-tools-inspector}

Analyse the structure of objects

The _Inspector_ allows the user to inspect runtime-objects by
showing their attributes.  This tool is first of all a debugging
tool.

There are three common ways to get a starting object.  If you
know the reference it may be typed in the _Inspect_ entry-field,
you can select an object from the screen after pressing the
pointing-hand icon and finally various other tools provide
_Inspect Object_ or similar that activate this tool, in
particular the _Visual Hierarchy_ tool.

Selecting a card will FLASH the associated graphical if this
is visible.

## An object card {#sec-tools-an-object-card}

Each object is displayed on its own card, which consists of a
two-column table.  At the top-left is the displayed object and
its class.  At the top-right are the memory-management flags
(see object <-_flags) and the reference count (see object
<-references).

Below that follows an alphabetically ordered list of
attribute-names and their current value.

**User interface**:

## Object cards {#sec-tools-object-cards}

- LEFT-CLICK on OBJECT INDICATOR in the top-left
	If the object is a graphical, send a ->flash message to
	it.  Put the reference (e.g.  `@pce`) on the clipboard.

- LEFT-CLICK on an VALUE (entry at right column)
	puts the reference or contents on the clipboard and
	shows a description in the status window.

- DOUBLE LEFT-CLICK on a value opens or exposes
	the referenced object (if any).

- MIDDLE-DRAG on CARD's BACKGROUND
	Move the card over the window.

- RIGHT-DOWN on a CARD
	Show a POPUP MENU to HIDE, EXPOSE or QUIT the card.

## Bugs {#sec-tools-bugs}

This tool sometimes appears to miss changes.

**Tool name**: `inspector`

@see Visual Hierarchy
@see display-inspect_handlers
@see topic Object -> Term
@see display->cut_buffer
@see topic Garbage collection
@see object<-references
@see topic Debugging
@see object<-_flags

## Manual Tools {#sec-tools-manual-tools}

Overview of defined manual tools

The _Manual Tools_ browser provides cards (i.e. the manual) for each of
the manual tools.   When the help-button of some tool is pressed, the
corresponding card from this list is shown in the card-viewer.

**Tool name**: `manual_tools`

@see Summary List

## Object Browser {#sec-tools-object-browser}

Overview of predefined global objects

The _Object Browser_ displays a list of documented global objects
provided by PCE.  Examples are @pce to control the environment and
@prolog to interface to Prolog.

**User interface**: Standard

**Tool name**: `global_objects`

@see Summary List
@see Card Viewer

## PCE Changes {#sec-tools-pce-changes}

Overview of changes to PCE

The _PCE Changes_ tool allows for browsing changes that have been made
to the PCE system itself.  Normally, the description of the change will
be related to the affected classes/behaviours of the PCE system.

**User interface**: The user interface is standard.

**Tool name**: `changes_browser`

**Bugs**:

A specialised visualisation is necessary that allows you to sort by name
and find changes by searching for strings.

It should also be possible to get all changes made after a certain date.

@see Summary List

## PCE Manual {#sec-tools-pce-manual}

Overall manual/environment control

You are probably viewing the first card of the PCE online documentation
system.  This card is exceptionally long as we expect you to read it all
and in the order presented.  Once you understand the principles of this
manual we hope you find it a handy tool to find your way in the
mysterious world called PCE.

The manual is organised in _Cards_.  This example contains an overview
of the manual itself.  It describes the main window and the overall
organisation of tools.  Read it carefully before you go on.  The
scrollbar allows you to scroll through the entire card.


## The manual window {#sec-tools-the-manual-window}

The window labeled _PCE Manual_ is the graphical gateway to the PCE
environment and and the PCE manual.  It contains four pulldown menus and
a field for (short) general feedback messages.

You managed to find the _HELP_ option, so probably you'll know how
to operate the menu bar.

The FILE menu provides overall operations; the BROWSERS menu provides
access to various entry-points of the reference manual material, the
TOOLS menu provides access to some tools to monitor runtime environment
of PCE.  Finally, the HISTORY menu provides access to places where you
have been in this manual.  At the bottom of this card is an overview of
the options in these menu's.

Below follows a description of the general principles and style used by the
online manual.  To get a detailed overview of the existing tools, open the
_MANUAL BROWSERS_ window from the _BROWSERS_ pulldown.


## General principles {#sec-tools-general-principles}

## Cards {#sec-tools-cards}

The manual is divided into `cards`, each describing some specific issue
of PCE. The cards are typed; each specific kind of topic is described on
a card of a specific kind.  Each card has the following general
attributes:

	Type 			A single capital indicating the type
	Name			Short meaningful name
	Last Modified	Time-stamp for last modification
	Summary			About 40 characters summary
	Description		Arbitrary length contents of the card
	See also		Related cards.

The available types and their identifiers are:

	B	Browser (manual) tool
	C	PCE Class
	E	Example
	K	Keyword entry
	M	Method of a class
	O	Global PCE object (@prolog, @pce, ...)
	P	(Prolog) predicate
	T	Topic
	V	Variable

Currently, you are reading the DESCRIPTION of a card.  In the window at
the bottom-right are some cards that are related to this card.  Note the
first letter that serves as a type indicator.


## Browsers {#sec-tools-browsers}

The entire online help system consists of three different type of tools.
First of all the main window (called _PCE manual_), which allows the
user to start a `browser`.

Each BROWSER provides access to a GROUP of CARDS, starting from
a specific ORGANISATION.  The manual is organised around a
class-oriented hierarchy (_ClassHierarchy_ and _ClassBrowser_),
a keyword-based index (_Keyword Browser_) and some special
browsers listing cards of one specific type (predicates, global
XPCE objects, examples and manual tools).

Most Browsers present a list of CARDS, displaying only the name and
possibly the summary of the card.  From there, the user may select a
card to view the contents in the CARD VIEWER.  When the card viewer
is opened, a SINGLE-LEFT-CLICK on the object will open the card.
Otherwise DOUBLE-LEFT-CLICK will start the card-viewer.

The CARD VIEWER displays the textual attributes of the card and allows
you to jump to RELATED cards.


## Selection and focus {#sec-tools-selection-and-focus}

The online manual tools use two mechanisms to communicate.  First of
all, a CARD may be SELECTED.  All other manual tools will highlight the
currently selected card.  The CARD VIEWER will show the textual
contents and relations of the selected card.  Selecting a card is done
by LEFT-CLICKING on the card, or by selecting the _SELECT_ option from
the POPUP MENU associated with most representations of a card.

The manual can be requested to FOCUS on a specific card.  The general
intention is that each tool is given the opportunity to `focus` on that
card.  In practice, the current implementation makes this feature only
handy for cards representing a CLASS: requesting it to become the focus
makes the _Class Browser_ jump to the associated class.


## Popup menus {#sec-tools-popup-menus}

From the POPUP MENU associated with a visualisation of a card, a number
of other options are available.  These are:

	SELECT			Select the card and view its contents
	FOCUS			Make card the focus (see above)
	SOURCE			Start an editor on the source code
	RELATE			Relate the card to the selection
	UNRELATE		Unrelate the card from the selection


## Menu overview {#sec-tools-menu-overview}

Menu FILE:
	ABOUT				Displays short message about PCE
	HELP				Jumps to this manual card
	========================================
	DEMO-PROGRAMS	Shows  a browser with available PCE
						demo programs
	========================================
	EDIT_MODE			Toggle edit-mode of manual
	LIST MODULES		Gives an overview of loaded modules
	LIST ALL MODULES	Gives an overview of all modules
	SAVE MANUAL		Saves modified modules to file
	========================================
	QUIT				Quit all interactive PCE tools
	QUIT PCE			Quit PCE itself

Menu BROWSERS
	MANUAL BROWSERS	Overview of available manual tools
	========================================
	CLASS HIERARCHY	Overview of the PCE class hierarchy
	CLASS BROWSER		Examine definition of a class
	GlOBAL OBJECTS		Overview of predefined objects
	ERRORS				Define errors (may be catched)
	PROLOG PREDICATES	Overview of PCE/Prolog predicates
	========================================
	KEYWORD BROWSER	Keyword-based access
	GROUP OVERVIEW	Overview of related methods
	EXAMPLES			Overview of examples

Menu TOOLS
	STATISTICS			Provides statistics on memory usage
	INSPECTOR			Allows you to inspect objects
	VISUAL HIERARCHY	Consists-of hierarchy of visual
						(graphical) objects.
	========================================
	CHECK OBJECTBASE	Runs consistency check on the PCE
						object-base (checkpce/0).

Menu HISTORY
	SELECTION			Gets back to a card where you have
						been.  The pullright menu shows the
						last 10 cards you've visited.
	FOCUS				Gets back to the last focus-point.

**Tool name**: `manual`

**Bugs**:

Various things are still incomplete or buggy to these manual tools.  A
short list:

- Many cards need to be filled
- Errors (diagnostics) need to be related to PCE error objects
- The Visual Hierarchy browser cannot be quit'ed.

@see Summary List
@see Topic Browser
@see ClassBrowser
@see Class Hierarchy
@see Card Viewer
@see topic Manual

## Predicate Browser {#sec-tools-predicate-browser}

Overview of PCE related Prolog predicates

The _Predicate Browser_ provides an overview of the Prolog predicates
related to the PCE/Prolog interface and the PCE/Prolog libraries.

**User interface**: Standard

**Tool name**: `predicate_browser`

@see Summary List
@see Card Viewer

## Search Tool {#sec-tools-search-tool}

Search for combination of words in manual

The Search Tool is located in the Browsers/Search menu of the
online manual tools.

This tool consists of a browser containing all words extracted
from the manual, a summary window with the same user-interface
as all other summary windows and a dialog containing a `Search
For' button and associated text entry field.  A search
specification can by typed in this field and all cards matching
this specification will be shown on the upper summary browser.

If the user pauses typing for one second, the tool will
evaluate the current expression and indicate the number
of matches if the expression is syntactically correct.

The expression syntax contains the following keywords and
special symbols:

- Expr1 and Expr2
	Search for cards satisfying both the left-hand and
	right-hand expression

- word Expr
	Treated as word and Expr.  Simply typing a list of
	words thus specifies cards containing all specified
	words.

- Expr1 or Expr2
	Search for cards satisfying either the left-hand or
	the right-hand expression

- not Expr
	Succeeds if Expr does not hold.  Can only be used in
	combination with `and`.

- <word>
	Succeeds if card contains the indicated word.  Only
	matches entire words.

- word
	Succeeds if card contains a word that starts with
	the indicated word.

	# ( Expr )
	Brackets may be used to specify association.  There is
	no ordering in the operators.  `not` matches the
	smallest expression.  _A and B or C_ is read as
	_A and ( B or C)_.

Below are some valid expressions:

	frame and displayed
	(file or directory) and absolute
	graphical and move and user

## Database {#sec-tools-database}

The database is maintained in the file

	pce('man/reference/index.obj')

This file is a chain_table object, mapping words in the cards on
their card identifier.  The system will prompt to create a new
index if the index cannot be found.

**Tool name**: `search`

## Statistics {#sec-tools-statistics}

Resource usage by PCE/Prolog

The _Statistics Tool_ provides an overview of the memory usage by PCE.
The values are updated every 5 seconds.

The values have the following meaning:

- Core In Use (`@pce <-core_usage`)
	The total amount of core PCE has requested itself explicitly
	via the system's malloc() function.  Note that core requested
	by libraries is not included.

- Core Wasted (`@pce <-core_wasted`)
	PCE defines it's own memory allocation schema.  This value
	indicates the total amount of memory allocated and later freed,
	but not (yet) reused.

- Objects in Use (`@pce <-objects_allocated` - `@pce <-objects_freed`)
	Total amount of `living` objects (i.e.  objects created and not
	yet freed).

**Tool name**: `statistics`

**Bugs**:

It appears this tools uses for misterious reasons a little bit of memory
each time it is updated.

@see pce<-objects_undecided
@see pce<-objects_freed
@see pce<-objects_allocated
@see pce<-core_wasted
@see pce<-core_usage
@see pce->garbage_collect
@see topic Creating Objects
@see topic Removing Objects
@see topic Garbage collection

## Summary List {#sec-tools-summary-list}

List of card summaries

Most of the manual tools provide means to find cards, giving some
starting point.  For example, the _Class Browser_ allows you to find
cards from a PCE class; the _Keyword Browser_; allows you to find cards
from some keyword.

These tools display a list of card-summaries in a browser window,
normally positioned somewhere at the right side of the tool.  This
browser window displays the cards line-by-line.  The first character
indicates the type of the card:

	B	Browser (manual) tool
	C	PCE Class
	E	Example
	K	Keyword entry
	M	Method of a class
	O	Global PCE object (@prolog, @pce, ...)
	P	(Prolog) predicate
	T	Topic
	!	Error object
	~	Description of a change to PCE

The next field provides the name of the card.  When there is enough
space, a third field is added that provides a 40 character summary
string of the contents of the card.

When the summary field is tagged with _(+)_, the description field is
not empty.  A double click will show the description field in the card
viewer.

**User interface**:

A summary browser indicates the selected card by inverting it.  It has
the follwing user interface behaviour:

A LEFT-CLICK on a line sets the global selection of the manual to this
card.  This implies the _Card Viewer_ will display the contents of the
card.  If the _Card Viewer_ is not open, DOUBLE-LEFT-CLICK will open it.

A RIGTH-DOWN on a card invokes a popup menu with the following options:

	SELECT		Equivalent to LEFT-CLICK
	FOCUS		Set global focus.  Currently only useful on
			_Class cards_ to set the current class of the
			_Class Browser_.
	=====================================================
	SOURCE		Display the source-code related to the
			card (Classes, methods, variables and
			resources).
	SPY		If the object is a method, put a Prolog
			spy-point on the implementing predicate.
	=====================================================
	TRACE		When the object is a method, put a PCE
			trace point on it.  See also Method->trace
	=====================================================
	RELATE		Relate this card to the selection
	UNRELATE	Destroy relation to the selection

@see Manual Tools
@see method-summary
@see PCE Changes
@see PCE Manual
@see Predicate Browser
@see Object Browser
@see Example Browser
@see ClassBrowser
@see Card Viewer

## Topic Browser {#sec-tools-topic-browser}

Hierarchical overview of the manual

The _Topic Browser_ displays a hierarchical overview of the manual.
Unlike the class hierarchy, which shape has a technical motivation, the
topic hierarchy is not constrained by any technical issues and contains
subjects that are not directly related to a class.

Representing a tree, the interface is similar to that of the `Class
Hierarchy' browser.  Some more options are available to allow for
editing the topic hierarchy.

The contents of a topic are shown by the _Card Viewer_, which is
automatically started when a topic is selected by a LEFT-CLICK or by the
SELECT option from the POPUP-MENU associated with each topic (under the
right mouse-button).

**User interface**:

A LEFT-CLICK on a node displayed the contents in the _Card Viewer_.  If
the _Card Viewer_ is not yet opened, DOUBLE-CLICK will do so.

Each node (topic) in the tree has a popup-menu associated with the
follwing options:

	SELECT		Make topic the selection (view contents)
	=====================================================
	EXPAND		Expand 1 level of sub-topics
	EXPAND TREE	Expand all the way down
	COLLAPSE NODE	Destroy all subnodes
	=====================================================
	RELATE		Relate topic to selection
	RENAME		Change the name to the NAME field
	=====================================================
	MOVE BELOW	Reorder: move just below selection or to be first
	MAKE DAUGHTER	Make this node a daughther of the selection
	REMOVE DAUGHTER	Remove link to selected parent
	=====================================================
	DELETE CARD	Destroy the entire topic card


A new topic card is CREATED (when the manual is in edit-mode) by:

- Selecting the super-topic
- Typing the name in the NAME field
- Typing a summary in the SUMMARY field
- Typing RETURN or Pressing CREATE

**Tool name**: `topic_browser`

@see Card Viewer
@see Class Hierarchy
@see PCE Manual

## Visual Hierarchy {#sec-tools-visual-hierarchy}

Overview consists hierarchy of visuals

The _Visual Hierarchy_ tool can be used to overview the consists_of
hierarchy of visual objects.  It is commonly used to find the
PCE reference for visual objects from the screen or to examine the
structure of a tool (in terms of the visual objects it is built from).

The tool consists of a graphical window for displaying the hierarchy and
a dialog.  The graphical window displays the hierarchy.  The root of the
hierarchy is the PCE predefined object @display_manager and is
always present.

**User interface**:

Any XPCE graphical object from the screen may be added to the
tree by positioning the pointer (mouse) on the object and
pressing META-SHIFT-CONTROL-V.  Mnemonic: _V_ for _Visual_
and all modifiers to avoid conflict with applications.

Each node of the hierarchy has a menu associated with the right
button.  This menu has the following entries:

	FLASH			Send ->flash to the represented graphical
	=============================================
	EXPAND			Expand the next level of the hierarchy
	EXPAND TREE		Expand as many levels as possible
	COLLAPSE		Remove all subtrees
	DELETE			Remove this node and anything below
	=============================================
	SOURCE			View source of class of object
	INSPECT			Start _Inspector_ on object
	TILE HIERARCHY	Frames: analyse the tile structure
	CLASS BROWSER	Start _ClassBrowser_  on class of object..
	=============================================
	FREE			Send ->free to the object
	POSTSCRIPT 		Genererate a PostScript description

The following button bindings are defined:

- LEFT
	->Flash the object and print the output of
	portray_object/1 in the dialog.  The reference of the
	object is put on the X-display clipboard for pasting in
	another application (typically the Prolog window for
	further analysis of the object).

- DOUBLE-LEFT
	Expand the next level of the hierarchy.

**Tool name**: `visual_hierarchy`

**Bugs**:

The functionality of this tool is still experimental.

The tool does not automatically update for changes in the PCE world.
Only destroyed visuals that are displayed by this tool will be removed
from it.

@see ClassBrowser
@see Inspector
@see class visual

