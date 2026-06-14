# Global objects {#sec-objects}

## @_not_returned {#object-_not_returned}

Internally used constant.  See `frame <-confirm`.

## @arg1 {#object-arg1}

The var objects (function objects) @arg1 ...  @arg10 are used to pass
context arguments when code objects are executed using `code ->forward`.
The example below shows this mechanism:

	?- new(@m, and(message(@prolog, write, @arg1),
	               message(@prolog, nl))).

	?- send(@m, forward, hello).
	hello

They are also used to describe the arguments to user-defined method
objects.

See also @receiver, @event, class var, class function, class method and
`send_method ->send`.

@see class ?
@see @event
@see @receiver
@see @arg10
@see @arg9
@see @arg8
@see @arg7
@see @arg6
@see @arg5
@see @arg4
@see @arg3
@see @arg2

## @arg10 {#object-arg10}

*Inherits description from*: @arg1

@see @arg1

## @arg2 {#object-arg2}

*Inherits description from*: @arg1

@see @arg1

## @arg3 {#object-arg3}

*Inherits description from*: @arg1

@see @arg1

## @arg4 {#object-arg4}

*Inherits description from*: @arg1

@see @arg1

## @arg5 {#object-arg5}

*Inherits description from*: @arg1

@see @arg1

## @arg6 {#object-arg6}

*Inherits description from*: @arg1

@see @arg1

## @arg7 {#object-arg7}

*Inherits description from*: @arg1

@see @arg1

## @arg8 {#object-arg8}

*Inherits description from*: @arg1

@see @arg1

## @arg9 {#object-arg9}

*Inherits description from*: @arg1

@see @arg1

## @black_image {#object-black_image}

Image filled with only 1's

Predefined images of `image <-kind: bitmap` of 16x16 pixels.  These
bitmaps are frequently used to fill graphicals.  See `graphical
->fill'.

@see topic Fill Patterns

## @class {#object-class}

PCE var object that points to the currently compiling class.
Pushed/popped by pce_begin_class/3 and pce_end_class/0.

Intended for directives on the class that are not converted by
the Prolog preprocessor.  For example:

	:- pce_begin_class(myclass, object).
	:- send(@class, save_style, external).

	...

	:- pce_end_class.

See also pce_compiling/1.

## @class_default {#object-class_default}

Instance of class constant used for access to instance variables
guarded by a class_variable object.   The object-allocation will
any slot for which a class-variable is defined to this slot.
First access to the slot will replace the value with the current
binding of the class-variable.  See also `object <-class_variable_value`,
`object ->obtain_class_variables`.

## @classes {#object-classes}

Hash-table of class-name --> Class

The hash_table object @classes maps class-names into class objects.
Application programs that wish to map a class-name into a class object
better use `@pce <-convert`:

	get(@pce, convert, point, class, X).
	X = @point_class

Using `@pce <-convert` will generate an exception if the class does not
exist.  See also the Prolog predicate pce_autoload/2.

## @colour_display {#object-colour_display}

Predefined executable code object that succeeds if the attached
default display @display is a colour display.  Defined as:

	new(@colour_display, @display?depth > 1).

Commonly used for conditional Default specifications:

	text_cursor.colour: when(@colour_display, red, black)

See also `@display <-visual_type`, `@display <-depth`.

## @colours {#object-colours}

A hash_table object mapping colour names into colour objects.  This
table only holds actually used colours.  A list of predefined colour
names is normally available from the file

	/usr/lib/X11/rgb.txt

The exact location of this file may be different on your X11
installation.

## @cursor_names {#object-cursor_names}

Map X-cursor names on X-cursor id's

The global and predefined sheet @cursor_names maps the names of the
X-cursors onto the entry-id in the X-cursor font.  Thus,

	get(@cursor_names, attribute_names, Chain)

returns a list of all available cursor names.

@see display-cursors
@see class cursor

## @cursors {#object-cursors}

A hash_table object that maps cursor names onto cursor objects.  Used by
`cursor <-lookup` and `cursor <-convert`.

## @default {#object-default}

Represents `default value`

The constant object @default represents defaulting.  @default is
normally used for arguments you do not wish to specify when
sending a message or creating an object.  @default is also used
as filler for a slot.  In this case it normally means `when this
slot is needed; compute a sensible default from the
environment'.

The interpretation of @default is handled by the code that handles the
arguments or attribute-value.

When too few arguments are presented to a method and the method has
defined it is willing to accept @default (indicated by putting the type
between square brackets; _[int]_ refers to an integer or @default)
for the remaining arguments, @default is passed for all remaining
arguments.  For example, the arguments to `Graphical ->set` are
_[int], [int], [int], [in]_, which makes

	send(Box, set, 40, 40)

equivalent to

	send(Box, set, 40, 40, @default, @default).

@see @nil
@see topic Types

## @display {#object-display}

Access to the X-display

The global object @display refers to the default display object.  This
display resides by default at the address specified by the environment
variable DISPLAY.

The object @display is normally used for the methods
`@display ->inform`, `@display ->confirm` and `@display <-size`.

See also class display and class display_manager.

@see class visual
@see class display

## @display_manager {#object-display_manager}

The global object @display_manager the single instance of class
display_manager.   The display manager manages the set of
available displays.   On the Win32 platforms, there is only one
display called @display.  On X11, @display refers to the default
display, but multiple displays on different computers may be
managed by XPCE.  See also `display_manager <-members`.

## @elevations {#object-elevations}

Hash_table object maintained by class elevation.  It maps
`elevation <-name`s onto elevation objects.

## @end_of_file {#object-end_of_file}

Used by class tokeniser to signal end-of-file.

## @errors {#object-errors}

The hash_table object @errors maps error-id's into error objects.  The
available errors may be examined using the _Errors_ tool from the
_Browsers_ menu

@see tool Errors

## @event {#object-event}

Currently processed event

The var object @event provides a reference to the currently executing
event.  It works similar to @arg1 ..., @receiver, etc.  The current
event is pushed by `event ->post`.

@event provides a reference to the event that caused this behaviour
to execute, regardless of where you are in your code.

Note that a window maintains a variable `window <-current_event` in
which the event currently processed by this window is stored.

@see event->post
@see @arg1
@see window-current_event
@see class event

## @event_tree {#object-event_tree}

SubsuMption hierarchy of event-types

@event_tree describes the event-type hierarchy.  This hierarchy is
used by `event ->is_a`.  This method is used by class handler to
verify the type of the event.

The demo program `events` may be used to examine the event hierarchy.

@see class handler
@see event->is_a
@see class event_tree

## @fonts {#object-fonts}

Hash-table with defined fonts

The hash_table @fonts maps font reference names (e.g. screen_roman_13)
onto font objects.  Thus, all available fonts may be listed using:

	send(@fonts, for_all, message(@pce, write_ln, @arg2)).

The demo-program _FontViewer_ may be used to examine the available
fonts.

NOTE:	The table @fonts is initially empty.  The standard fonts are
		created from display.font_families using the method `display->load_fonts`.

@see class font
@see display.font_list

## @grabbed_windows {#object-grabbed_windows}

Chain object holding windows that claimed the pointer using
`window->grab_pointer`.  New windows are added to the front of
this chain.  A call to `window->grab_pointer: @off` re-enables
the grabbing on the previous window.

## @grey12_image {#object-grey12_image}

Image filled  with 12% 1's

*Inherits description from*: @black_image

@see topic Fill Patterns

## @grey25_image {#object-grey25_image}

Image filled  with 25% 1's

*Inherits description from*: @black_image

@see topic Fill Patterns

## @grey50_image {#object-grey50_image}

Image filled  with 50% 1's

*Inherits description from*: @black_image

@see topic Fill Patterns

## @grey75_image {#object-grey75_image}

Image filled  with 75% 1's

*Inherits description from*: @black_image

@see topic Fill Patterns

## @images {#object-images}

Table of reusable images

The hash_table @images maps names of images into images.  It is used by
`Image ->initialise: name`.  If the name already exists in this table,
new/2 returns the existing image.  Otherwise a new image is created from
the named file and this image is added to @images.

@see image-name
@see image->initialise

## @key_bindings {#object-key_bindings}

Hash_table object mapping key_binding names to key_binding objects.  Any
named key_binding object is inserted in this table.  The standard
`editable` object (class text, class text_item and class editor) have
default key_binding tables with names `text`, `text_item` and `editor`.

@see key_binding<-convert

## @modifiers {#object-modifiers}

Table mapping modifiers specified as a name holding `s`, `c` and or 'm'
into modifier objects.   See `modifier <-convert`.

Do not modify modifiers in this table.

## @nil {#object-nil}

Represents `nothing`

The constant @nil is used to indicate `not-filled` or `nothing`.  It is
used very frequently in PCE.

When a new instance is created by PCE's virtual machine, all slots are
initialised to @nil, after which `object ->initialise` is invoked on the
new instance.  When an object is destroyed by PCE's virtual machine,
all slots that are filled with true objects are reset to @nil.
See `object ->unlink`.

@see @default

## @not_obtained {#object-not_obtained}

Used by class_variable objects to indicate the real value has
not yet been resolved.

## @off {#object-off}

The bool objects @on and @off represent `true` and `false`.  PCE
converts various other values into one of these objects if the requested
type is `bool`.   See `bool <-convert`.

See also class bool and class constant.

@see image-kind

## @on {#object-on}

*Inherits description from*: @off

## @open_sockets {#object-open_sockets}

This chain represents all sockets that are currently open.  It
is maintained by `socket->listen`, `socket->connect` and
`socket ->close`.

Its primary usage is to enable XPCE to do a graceful shutdown
of all active network connections at `@pce ->die`.

## @pce {#object-pce}

Access to the overall PCE environment

The object @pce is the only instance of class pce.  It defines methods
for which there is no clear receiver as well as overall access to the
PCE system.

@see class pce

## @prolog {#object-prolog}

Access to Prolog (notably callback)

The object @prolog is the only instance of class host and provides
access (calling predicates) to the hosting language.   On Lisp systems
this object is called @lisp.  See also @pce.

@see class host
@see topic Calling Prolog

## @receiver {#object-receiver}

Receiver of event/message

The var object @receiver refers to the object that is receiving
an event.  A new value of receiver is pushed if a message is
invoked from a gesture object or handler object.  In this case,
@receiver refers to the (graphical) object that received the
event.  See `handler ->event`, @event and `@event <-receiver`.

@receiver is commonly used to create reusable gestures.  For
example:

	?- new(@clicked,
		   click_gesture(left, '', single,
						 message(@receiver,
						 		 clicked))).

If this recogniser object is associated to
a graphical object, the object will receive
a ->clicked message when the user left-clicks
the object.

@see @arg1

## @reportee {#object-reportee}

The var object @reportee is managed by `visual ->report` for
avoiding cycles, as well as be able to `graphical ->alert` the
origin of the message.  It contans a chain object.  The
`chain <-head` is the original receiver of the `visual ->report`, the
other elements are visuals visited while trying to deliver the
message.

@see visual->report

## @running_processes {#object-running_processes}

Chain holding all running inferior processes

This chain is managed by `process ->open`, `process ->stopped`,
`process ->killed` and `process ->exited`.  It contains all those
processes that have pid not equal to @nil (i.e. the process objects that
have a living Unix counterpart).

This chain may be used to check whether there are any living
subprocesses associated to this PCE process.

@see process->kill

## @space_rubber {#object-space_rubber}

Rubber object normally used to render spaces.  Attributes:

	`rubber<-stretch`: 100
	`rubber<-shrink`: 1
	`rubber<-level`: 1
	`rubber<-linebreak`: allow.

Used by `tbox <-space`.

## @types {#object-types}

The hash_table object @types maps `type <-name` onto type
objects.  This method is used by `type <-lookup` and `type <-convert`.
It may also be used to enumerate the existing instances of class type.

## @white_image {#object-white_image}

Image filled  with only 0's

*Inherits description from*: @black_image

@see topic Fill Patterns

