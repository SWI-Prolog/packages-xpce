# class text_item {#class-text_item}

A text_item is a entry field for a short text.  It is normally used in
dialog windows for entering names or other short textual information.  A
text_item consists of a label, and an entry field.  The label is fixed.
The user may type in the entry field.

When the user hits RETURN, the text_item will forward the current
<-selection over the associated <-message with the following arguments:

	| @receiver | The text_item         |
	| @arg1     | The current selection |

Text_item's may used to edit/examine any PCE type that defines a
`object <-print_name` and an `object <-convert` which allows the item to
convert a typed textual representation into an object of the requested
type.   See <-type.

If is possible to generate a set of possible completions for a partially
typed text, the text_item may be programmed to perform automatic
completion.   See ->complete.

The mapping from key-strokes to edit actions is determined by the
reusable key_binding object named text_item, a reference to which
may be obtained using:

	?- new(KB, key_binding(text_item)).

See also class label and class int_item.

**Bugs**:

It is not possible to detect the user typed something before the RETURN
or ESCAPE otherwise than creating a subclass and redefining the ->event
method.

@see class key_binding
@see text->typed
@see class slider
@see class text


## Instance variables {#class-text_item-instvars}

- text_item<->advance: {next,clear,none}
    Determine the behaviour after the user hits RETURN while the text_item
    was modified.  See also ->enter. Values are:

    - next
    	Send message and goto next item in dialog

    - clear
    	Send message and clear item.  Do not move caret to next.

    - none
    	Just send message.

    **Defaults**: The initial value is `next`.

    @see device->advance

- text_item-default: any|function
    Value to which the text_item is ->restore'd.

- text_item<-editable: bool
    If @on, text may be edited by typing.  If @off, any attempt to type will
    invoke ->alert on the text_item.

    **Defaults**: @on

- text_item<->hor_stretch: 0..100
    Horizontal stretchability.  This is interpreted by `dialog ->layout`.
     The item will be extended towards the item at its right or the
     right-side of the dialog window.

- text_item<-length: int
    Length of the value-part of the text_item in <-value_font's `ex` units
    (see `font <-ex`).  See also ->value_width.

- text_item-print_name: char_array
    This slot contains the <-selection converted to text.  It is used to
    detect if the user has modified the selection.

    @see text_item<-modified

- text_item-selection: any
    Value of the selection.  Set by ->selection and <-selection if the text
    has been edited by the user.

- text_item<-show_label: bool
    Determines whether or not the label is visible.  Used seldomly.

    **Defaults**: @on

- text_item<-style: {normal,combo_box,stepper}
    Class text item can represent a <-value_set or do completion on
    the basis of their associated <-type.  In both cases,
    visualisation by means of a `combo box` provides useful
    feedback to the user as well as a commonly found user
    interface style.

    <-style combo_box is automatically selected after ->value_set or
    ->type with a suitable type.   It may be switched afterwards.

    A text item showing as a combo-box both supports completion
    and opening the combo-box.

    Style `stepper` shows a small up/down arrow at the right of the
    text-entry-field.  Pressing this field invokes ->increment and
    ->decrement, holding the mouse-button down repeats this
    message.  This style is used by class int_item.  Applications
    may use it to implement -for example- an ordinal scale.

- text_item<->type: type
    The <->type slot describes the value-type represented by the text_item.
    When a value is displayed, it is translated into a string using
    `text_item <-print_name`.  After editing, it is translated into a PCE
    object of the specified type using `Type <-check`.

    Slot <-type is set by ->default, where it is deduced from the initial
    value.  When the value is an integer, the type is `int`.  Otherwise the
    type describes the class the selection belongs to.

    If the type can generate a value_set (see `type <-value_set` and
    <->value_set, the ->style will automatically be changed to
    `combo_box`.

    EXAMPLE

    	?- new(P, picture),
    	   send(P, display, new(B, box(100,100))),
    	   send(new(D, dialog), below, P),
    	   send(D, append,
    		    text_item(size, B?size,
    					  message(B, size, @arg1))),
    	   send(P, open).

    @see type<-check
    @see text_item<-print_name

- text_item<-value_set: [chain|function]*
    Set of possible values. This slot is used by the default completion
    mechanism built in class text_item. See ->complete and <-completions.
    The interpretation of this slot is:

    - @nil
    	<-completions fails (no completion).

    - @default
    	<-completions activates `type <-value_set` on the associated
    	<-type.

    - chain object
    	<-completions answers with this chain.

    - function object
    	<-completions invokes the function and returns the result.
    	During execution of the function @arg1 is bound to
    	the `file` return of <-split_completions.  If this
    	function is invoked to generate all values for opening
    	a combo-box, @arg1 is bound to the empty name ('').
    	The function is supposed to return a chain of possible
    	values whose textual description starts with @arg1.
    	Note that ->complete will not consider elements of the
    	chain whose <-print_name does not start with
    	<-displayed_value.  Using @arg1 to select candidates is
    	only useful if this speeds up the selection process.

    @see text_item<-completions
    @see text_item->complete

- text_item<-value_text: text
    PCE text-object used to display the print-representation of the
    selection.  The text-object takes care of editing commands, etc.

- text_item-value_width: [int]
    Width of the value-part in pixels.  Note that ->right_side is
    often a comfortable for helping allignment.  See also
    <-hor_stretch.


## Send methods {#class-text_item-send}

- text_item->_wants_keyboard_focus
    Test if the text_item is prepared to get the keyboard focus.  Succeeds
    if:

    1. TextItem is displayed
    2. TextItem is active
    3. TextItem is editable

    @see device->advance

- text_item->apply: [bool]
    Executes <-message iff it is a code object, always is @on or <-modified
    is @on and <-selection succeeds.  The following arguments are forwarded:

    	| @receiver | The text_item    |
    	| @arg1     | The <-selection. |

- text_item->catch_all: selector=name, argument=unchecked ...
    Delegate to <-value_text and update the visualisation if the
    method was accepted.  This allows all text object manipulation
    methods such as `string->append` and `string->insert` to be
    applied directly on a text_item object.  See also
    `text->catch_all`.

- text_item->clear
    Equivalent to `->selection: `''.  This method is meaningless when type
    is not `name` or some other type that allows for ''.

- text_item->complete: [event_id]
    Attempt to complete the value typed so far. If this fails or there is no
    completion defined for this item. activate `text ->insert_self`.

    This method is the central method of the text_item's completion
    mechanism. This completion mechanism both deals selecting in
    hierarchical organised data (like the Unix file-system) and with
    simple lists of possible values.  The second case is generally handled
    by <->value_set.  For selecting from hierarchical organised data the
    following methods need to be redefined:

    - <-split_completion
    	Split the currently typed value in a `directory` part and a
    	`file` part.

    - <-completions
    	Return a set of possible `files` in the indicated
    	`directory` using the result of <-split_completion.

    - ->indicate_directory
    	Indicate the match of a `directory`.

    In this documentation we refer to `files` and `directories`, but in
    general this may refer to any organised data using any syntactical
    convention. The Prolog library file `file_item.pl` illustrated how the
    methods described above may be redefined to select a file from the
    Unix file-system.

    When ->complete is invoked, it attempts to complete the current entry.
    To do so, it locates the first choice-point and the set of objects that
    may be selected after this choice-point using `chain <-complete_name`.
    If the current point is a choice-point it will pop-up the browser
    object @completer with all possible completions. If the current point is
    a unique description of a completed value it will indicate such using
    the error sole_completion. Otherwise it will replace <-displayed_value
    with the text leading to the first choice-point or completion.

    @see text_item->indicate_directory
    @see @completer
    @see text_item-value_set

- text_item->complete_or_next: [event_id]
    If <-style is `combobox`, ->complete the value.  Otherwise
    use ->next to advance to the next dialog item.  See also
    `dialog->advance`.

- text_item->compute
    Updates the <-value_text, the label and value positions and the
    <-reference.

- text_item->default: any|function
    Set the variable -default to the argument and then ->restore the
    text_item.

- text_item->displayed_value: char_array
    *Inherits description from*: text_item<-displayed_value

- text_item->enter: [event_id]
    The method ->enter is by default bound the the RETURN key.  It's
    behaviour is rather complicated to get proper default action both if the
    dialog window in which the text_item resides has a `dialog
    <-default_button' and if this is not the case.

    1. Send `window ->typed` to the <-window of the text_item.  If
    	the window has an accelerator for _RET_ this will be activated.
    	If this message returns successfully the text_item considers the
    	RETURN handled.

    2. Otherwise, ->apply the text_item.  If this succeeds and
    	<-advance = `clear`, clear the text_item using ->selection ''.
    	Otherwise proceed to the next text_item using ->next.

- text_item->event: event
    Process an event:

    - When <->active equals @off: fail
    - Left button: try to get the caret (see `->caret`).
    - Middle button: paste X-cut-buffer 0

    Finally is the event is a keyboard event and @completer is working for
    this text_item `event ->post` this event to @completer.  Otherwise
    invoke `text_item ->typed`.

- text_item->execute
    Execute the text_item.  Invokes ->apply: @on.  See also
    ->enter.

- text_item->decrement
    (virtual) Decrement the selection.

- text_item->increment
    (virtual) Increment the selection.  Called when the user clicks
    the upper of the `stepper` buttons.  See also ->style: stepper.

- text_item->indicate_directory: text=string
    This method is invoked by ->complete if the expansion is unique. It
    provides a possibility to indicate that this string represents a
    `directory`. Class file_item from the PCE/Prolog library defines this
    method as:

    	indicate_directory(_FI, Dir:string) :->
    		(   send(directory(Dir), exists)
    		->  send(Dir, ensure_suffix, /)
    		;   true
    		).

    @see text_item->complete

- text_item->initialise: name=[name], default=[any|function], message=[code]*
    Create a text-entry-field from its label, initial value and message to
    execute after the value is edited by the user.  The initial value will
    be processed as ->default'

- text_item->length: int
    Length of entry field in characters.

- text_item->modified: bool
    If the argument equals @off, `->modified` runs `<-selection` to reset
    the -selection and -print_name slots.

- text_item->next
    Advance to next item in same <-device.  This invokes
    `device ->advance` using the receiver as argument.
    This method is normally bound to the TAB key.

    See also ->complete_or_next.

- text_item->paste: which=[{primary,clipboard}]
    Paste the value of the indicated X-cut buffer.  See `display
    <-cute_buffer'.  By default invoked from ->event on a middle button
    click.

- text_item->reset
    Equivalent to ->quit_completer.

- text_item->restore
    Computes the current <-default value and sets the ->selection to this
    value.

- text_item->select_completion: value_set=chain, prefix=[char_array]*, search=[char_array]*, auto_hide=[0..]
    Called by ->complete.  It's function is to allow the user to
    select a value from the set of possible values.  Prefix is the
    value entered so far,

    By default this method pops-up the browser @completer.  This may
    be redefined.

- text_item->selected_completion: item=char_array, apply=[bool]
    Called from @completer, a global browser object used to to
    display completions on a text_item or alternatives from the
    drop-down browser of a text_item.

    @completer calls this method with a single argument representing
    the selected text.

    This method appends the argument value to the prefix resulting
    from <-split_completion, sets the <-value of the text_item and
    hides @completer.

- text_item->selection: any
    Set the <-selection.  If the given value is not equal to the
    <-selection, convert it to <-type using `type <-check`.  If the value is
    still not equal to the <-selection, obtain a printable representation
    using <-print_name with the value as argument.  Finally assign
    <-selection with the (converted) value and <-print_name with
    the textual representation and update the display.

    See also <-selection and ->default.

- text_item->show_combo_box: bool
    Called internally to show/unshow the combo-box browser.  This
    browser is actuially @completer, as used by ->select_completion.

- text_item->status: {inactive,active,preview,execute,increment,decrement}
    If `active`, show the caret (`text ->show_caret`).  Otherwise the caret
    is not visible.

- text_item->typed: event|event_id
    Activates the key_binding object named `text_item` from @key_bindings.
    This key_binding object will map the typed keys onto functions
    (selectors) which will then be invoked on the text_item.

    For example, if the user types an `a` to a text_item the following
    sequence of method is traversed:

    1. ->event

    2. ->typed

    3. 'a' mapped by key_binding object to

    		   ->insert_self: @default, 97

    4. ->insert_self is delegated to <-value_text

- text_item->width: characters=int
    Equivalent to ->length.  New code should use ->length, to avoid
    the ambiguity with `graphical ->width`.

    *Inherits description from*: text_item-length, text_item->length


## Get methods {#class-text_item-get}

- text_item<-completions: char_array|tuple -> chain
    This method is normally invoked by ->complete. The argument is the
    return-value of <-split_completion.

    If the argument is a char_array the text_item completes from a plain
    list. It should return a chain of objects that match the indicated start
    of their <-print_name. The chain may contain objects whose name does
    - not* start with the indicated char_array. These will be filtered out by
    ->complete.

    If the argument is a tuple the text_item is used for a
    hierarchical structure.  This method should return a chain of
    `files` matching `tuple <-second` in the `directory` `tuple	<-first`.

    The default implementation exploits <-value_set.

    @see text_item<-split_completion
    @see text_item-value_set

- text_item<-default: -> any
    If the value of -default is a function, execute it.  Otherwise just
    return the value of this slot.

- text_item<-displayed_value: -> char_array
    Currently visible text.  Equivalent to `text<-string` of <-value_text.

- text_item<-has_completions: -> bool
    Return @on if item can generate completions.  Used by ->style:
    @default to see whether the item should appear as a normal
    entry-field or as a combo-box.

- text_item<-modified: -> bool
    Test if the user has modified the selection.  Succeeds if the current
    value of the text entry field is not equal to <-print_name.

    @see text_item-print_name

- text_item<-print_name: -> text=char_array
    Convert the current <-selection into a textual format.  It attempts the
    following conversions:

    - `object <-print_name`
    - type(char_array) <-check (calling `char_array <-convert`).
    - Internal `pretty printing` format used by the debugger.

    @see text_item-type

- text_item<-print_name_of_value: any -> char_array
    This method is called whenever the text_item wants to convert a
    value into a string.   The default implementation just calls
    `object <-print_name`.  May be redefined.  See also -print_name.

- text_item<-selection: -> any
    Recompute the <-selection from the entered value.  Performs the
    following steps:

    1. ->enter_completer

    2. If <-modified equals @on, convert the `text<-string`
    	   of the <-value_text (i.e. the displayed value) using
    	   `type<-check` to the requested type.  If this succeed
    	   assign <-selection and <-print_name.  If it fails
    	   generate a connot_convert_text error.

    3. Finally return the <-selection slot value

- text_item<-split_completion: value=char_array -> char_array|tuple
    The task of this method is to split the current <-displayed_value
    (passed as argument) into a `directory`- and a `file-part` to deal with
    hierarchically organised data.  This method should return:

    - its argument
    	If the text_item is used to select from a flat list of (named)
    	objects.   This is the default behaviour of this method.

    - A tuple object with the `directory` and `file` component
    	if the text_item is used to select from a hierarchical
    	structure.

    	Note that the return-value of this message is passed to
    	<-completions.

    @see text_item<-completions

