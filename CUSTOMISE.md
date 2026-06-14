                     A brief note on customising XPCE
                     ================================

1. Customising look-and-feel
============================

1.1.  Where do I specify look-and-feel?
=======================================

Look-and-feel and other defaults are stored as class-variable bindings
in two text files:

  * `<pcehome>/Defaults` ships with XPCE and holds the system-wide
    defaults.  The last line of this file pulls in the user file
    via `#include $PCEAPPDATA/Defaults`.
  * `$PCEAPPDATA/Defaults` is the per-user file.  On Linux it is
    `~/.config/swi-prolog/xpce/Defaults`; on macOS it lives below
    `~/Library/Application Support/swi-prolog/xpce/Defaults`; on
    Windows it is below `%APPDATA%\swi-prolog\xpce\Defaults`.

The system file is read-only after installation; everything you change
goes in the per-user file.  `<pcehome>/Defaults.user` is a commented
template that is *not* read at runtime — it is copied into your
per-user `Defaults` the first time you edit your XPCE preferences, so
you start with a worked example you can uncomment.

In Epilog the per-user file is opened (creating it from
`Defaults.user` on first use) via _Settings → Preferences → XPCE_, which
runs `prolog_edit_preferences(xpce)` from `library(swi_preferences)` —
the same hook is available to your own code if you want to drive it
programmatically.


1.2.  How do I find attributes that can be specified?
=====================================================

Each entry binds one class variable, named
`<class>.<class_variable>`.  The class browser shows the defined class
variables for a given class:

	1) Select the desired class using the `Class' item.
	2) Set the `Filter' to `all'.
	3) Set the `Display' to `Resource' only.
	4) `Apply'.
	5) Double-click on `object' in the top-right window to extend
	   the search to all classes this class inherits from.
	6) Double-click on a class variable for its description.


1.3.  Syntax for an entry
=========================

	<class>.<class_variable>:	<value>

For example, to make all circles green:

	circle.colour:		colour(green)

Multi-line values continue on the next line by ending the current line
with `\`.  Line comments start with `!`.


1.4.  Syntax for resource values
================================

The value syntax is close to Prolog with a few exceptions:

	* Creating an instance that requires no initialisation arguments
	is done using <classname>(): `chain()' creates an empty chain.
	`new(chain)' as used from Prolog will try to make an instance
	of class `new' (and thus fail).

	* Atoms starting with a Capital do not need to be quoted (but
	may be) as this syntax defines no variables.

	* '[' {<term>} ']' maps onto an XPCE chain and thus is
	equivalent to 'chain(' {<term>} ')'.

Parsing the string value into an XPCE value is done by the object
`@resource_parser`.  Use the *Global Object* browser to see its
documentation.


1.5.  Conditional values
========================

Sometimes you want a value to depend on a condition, for example on
display geometry.  Specify the value as a function object (see the
class browser for full documentation); the function is converted into
an object of the requested type by evaluating it.  Thus

	window.size:		size(@display?height / 3, \
				     @display?width / 3)

defines default windows to be one third of the display, while

	window.background:	when(@colour_display, grey95, white)

makes the window background slightly grey on colour displays and white
otherwise.  The object `@colour_display` is a predefined conditional
object.


1.6.  Constants
===============

To give various objects the same colour palette, or to define objects
you can reuse further down the file, bind the class variable

	pce.initialise

It holds an executable (code) object that is run once during start-up,
before any other class-variable conversion takes place.  The shipped
`Defaults` uses it, for example, to publish a few colour references:

    pce.initialise: \
	and(_dialog_bg @= when(@colour_display, grey80, white), \
	    _graph_bg  @= when(@colour_display, grey95, white), \
	    _win_pen   @= when(@colour_display, 0, 1))

The `@=` infix operator binds the left-hand reference name to the
right-hand object.  After this runs, `@_dialog_bg`, `@_graph_bg` and
`@_win_pen` are global references usable in later resource
specifications.


2. Prolog customisation
=======================

XPCE no longer reads its own per-user Prolog init file: the SWI-Prolog
init file (edited via _Settings → Preferences → Prolog_ in Epilog,
typically `~/.config/swi-prolog/init.pl`) is the only entry point.
