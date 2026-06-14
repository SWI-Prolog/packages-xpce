# class list_browser {#class-list_browser}

A list_browser object is a visualisation of a dict object.  It appears
as a scrollable list of (textual) items.  The user may examine this list
by scrolling and searching.  S/he may select one or multiple items from
the list (see <-multiple_selection).

List Browsers are commonly used for browsing or selection from a
(possibly) large list of (named) objects.  For example the manual's
card viewer uses a browser to show existing see-also relation.  The
class-browser and various of the other manual tools use them to
visualise sets of documentation cards.

List browsers are normally used through class browser which defines a
window displaying a list_browser (like class view displays an editor
object).

Various messages may be associated with list_browser objects:

	| <->select_message        | Activated on left-click   |
	| <->select_middle_message | Activated on middle-click |
	| <->open_message          | Activated on double-click |

The <->open_message is also activated when an item is selected using the
keyboard.

All these messages forward the following arguments:

	| @receiver | The browser or list_browser object |
	| @arg1     | The dict_item object selected.     |

It is possible to associate a popup with a list_browser.  See ->popup.
Finally, it is possible to program the event-handling yourself.  The
method <-dict_item may be used to translate events to dict_item objects.

## Fonts and images {#class-list_browser-fonts-and-images}

XPCE browsers support multiple fonts and images indicating the
type of the item.  Each item can only be displayed in one font.
Font and icon specification use the style object based mechanism
to specify the appearance of a dict_item object of specified
style.  For example, to make a browser for files and
directories, where the directories are printed purple and using
the `dir.bm` icon, use the following code as a starting point:

	...
	new(B, browser),
	send(B, style, directory,
		 style(icon := 'dir.bm', colour := purple)),
	send(B, style, file,
		 style(icon := 'file.bm')),
	...

	append_directory(B, Name) :-
		send(B, append,
			 dict_item(Name, style := directory)).

@see class key_binding
@see class browser
@see class dict


## Class variables {#class-list_browser-classvars}

- list_browser.search_ignore_case: bool = @on


## Instance variables {#class-list_browser-instvars}

- list_browser<->cancel_message: code*
    Send on drag-select with `up` outside browser.  Forwarding:

    	@receiver:	The list_browser object.

- list_browser<-dict: dict*
    Associated dict object (table of items).

- list_browser<-image: text_image
    Text_image object used for visualisation of the items.  Class text_image
    is also used by class editor.  There are many similarities in the
    architecture of class editor and class list_browser.

- list_browser<->key_binding: key_binding
    A key_binding object that maps typing in the list_browser object
    to actions.  The initial value is an empty table that delegates
    to the predifined table named `list_browser`.  New functions may
    be added using:

    	...
    	send(LB?key_binding, function, Name, Action)
    	...

    These functions are local to this list_browser.

- list_browser<-label_text: text*
    Text object used to display the <->label.  When @nil, no label is
    displayed (default).   The <-label_text may be accessed directly,
    but be aware that the layout of the list_browser (i.e. placement and
    sizes of the <-scroll_bar, <-image and <-label_text) will not be
    updated automatically.

- list_browser<-multiple_selection: bool
    When @on, any number of items may be in the selected state.  When @off
    (default), only zero or one item may be selected.  This flag has
    consequences for the way events are processed and the selection is
    communicated.

    @see list_browser-selection

- list_browser<->open_message: code*
    This message is send when the user double-clicks an item or selected
    an item using the keyboard (see also ->enter).  During the execution of
    this message the <-cursor is switched to the wait cursor.  The
    following arguments are forwarded:

    	| @receiver | The browser or list_browser   |
    	| @arg1     | The dict_item object selected |

    See also <->select_message

- list_browser<->popup: popup*
    There are two ways to associate a popup with a browser.  One is to use
    `list_browser ->popup`.  In this case the popup will only be displayed
    if the right-down event is detected on an item and @arg1 will be bound
    to the dict_item object on which the right-down was trapped.

    Alternatively, one may attach a popup_gesture using `Object
    ->recogniser'.  In this case the popup will behave as if it were
    attached to any graphical object.

    @see class popup

- list_browser<->select_message: code*
    If not @nil, this message is sent when the user clicks on an item with
    the left mouse-button. The following arguments are forwarded:

    	| @receiver: | The (list_)browser      |
    	| @arg1:     | The current <-selection |

    See also <->open_message and ->change_selection.

    **Defaults**: @nil (no message is sent)

    @see list_browser-select_middle_message

- list_browser-selection: chain|member:dict_item*
    The current selection of the list_browser.  If <->multiple_selection
    equals @on, this is an (empty) chain of dict_items.  If
    <->multiple_selection equals @off, this is either @nil or a dict_item.

    @see list_browser-multiple_selection
    @see list_browser<-selection

- list_browser<-size: characters=size
    Set the size of the list browser.  The <->width and <->height attributes
    of the size are interpreted in characters of the current font.

    To resize in pixel units, please see `ListBrowser ->_size` and
    `ListBrowser ->set`.

- list_browser<-start: int
    0-based index number of the first visible dict_item object.

- list_browser<-styles: sheet
    Sheet object mapping logical style-names onto style objects.


## Send methods {#class-list_browser-send}

- list_browser->_size: pixels=size
    Resizes the list_browser to the indicated *pixel* size.  Normally
    list_browser sizes are communicated in character units.  This method is
    used internally to handle resize requests of the browser window.

    @see list_browser->size

- list_browser->_wants_keyboard_focus
    Succeeds if the list_browser has a <-dict that is not empty.
    Note that typing in a list_browser searches for an item.

- list_browser->cancel_search
    Cancel the incremental search (see ->typed).  Incremental search is
    normally canceled when the pointer leaves the window and when the
    user hits ESC or Control-G.

- list_browser->change_selection: action={set,toggle,extend,clear,cancel}, context=[dict_item|chain]
    Hook in selection management.  This method is invoked from
    ->event and may be used to keep track of selection changes in
    the list_browser object.  If `action` is `set`, the selection
    will be cleared and then set to the argument dict_item object.
    Action `toggle` will toggle the status of the argument
    dict_item, while `clear` will empty the selection.

    Redefinition of this method may be used to keep track of the
    selection.    See also ->select_message and ->open_message.

- list_browser->clear
    Removes all items from the browser.  Invokes `dict ->clear` to the
    <-dict.

- list_browser->compute
    ->compute's the <-image and <-scroll_bar.  Initialises <-start_cell to
    speed up redraw.

- list_browser->enter
    Select current item as double-click.  This method is normally invoked
    through the <-key_binding when the user hits return to confirm searching
    for an item.

- list_browser->event: event
    Performs the following steps:

    1. Invoke `device ->event` and succeed if this succeeds.

    2. If the event is a keyboard event, invoke ->typed to realise
    	incremental search.

    3. If the event is an area_exit event, cancel an ongoing search.

    4. If the event is a right-button-down (ms_right_down) and there
    	is a <-popup, open the popup using @_popup_gesture.

    5. If the event is a left-or middle-click, modify the
    	<-selection using ->change_selection and, If
    	appropriate, execute one of the messages

    		| <-open_message          | double-click |
    		| <-select_message        | left-click   |
    		| <-select_middle_message | middle-click |

- list_browser->extend_prefix
    Part of incremental search.  Determines all items who's `dict_item
    <-label' start with the <-current_search and extends the search string
    to the common prefix of the matching items.  In other words, it will
    extend the search string to the next choice-point.

- list_browser->extend_prefix_or_next
    If the list_browser is searching (see ->insert_self), this
    method extends the current -search_string to the next
    choice-point (see ->extend_prefix).  Otherwise ->next
    is invoked to advance to the next item requesting the
    keyboard in the same device.

- list_browser->extend_to_current
    Extends the <-search_string to the current item.  Useful when the user
    is searching for an item that starts as the current.  See also
    ->extend_prefix.

- list_browser->geometry: x=[int], y=[int], width=[int], height=[int]
    Resize the <-image and <-scroll_bar.

- list_browser->height: int
    *Inherits description from*: list_browser<-height

- list_browser->initialise: dict=[dict], width=[int], height=[int]
    Create a list_browser for the specified dict object with specified
    width and height.  If no dict is specified a default dict is created.
    If no size is specified list_browser.size is used.  The size is
    interpreted in character units.

- list_browser->insert_self: times=[int], character=[char]
    Start/Continue incremental search.

- list_browser->label: name
    *Inherits description from*: list_browser<-label

- list_browser->next
    Move caret to next item (`device ->advance`).  See also
    `text_item ->next`.

- list_browser->previous_line: [int]
    Set selection to previous item.

- list_browser->next_line: [int]
    Move forward by arg lines.  If an incremental search is in
    progress, moves it moves the current search to the next
    line.  If the current search pattern does not match the new
    item, it is set to the entire next item.  If there is no search
    in progress, the browsercan only select a single object
    (->multiple_selection: @off) and the selection is in the
    visible part of the browser, the selected object is taken
    as the start of the search.  Otherwise, the top of the
    visible page is choosen.

    ->previous_line simply negates the argument before calling
    ->next_line.

- list_browser->normalise: member:dict_item
    Scroll window such that the indicated dict_item is visible. If this
    requires the window to be scrolled one line up or down the indicated
    item will be the first or last in the window. Otherwise the item will be
    centered in the window,

    See also ->scroll_to.

    @see list_browser->scroll_to

- list_browser->scroll_to: [int]
    Make nth-1 item first of window

    **Defaults**: By default scrolls to the last item.

    @see list_browser->normalise

- list_browser->selected: member:dict_item
    Test whether the indicated dict_item object or <-member is
    selected.

- list_browser->show_label: show=bool
    *Inherits description from*: list_browser<-show_label

- list_browser->size: size
    *Inherits description from*: list_browser-size

    @see list_browser->_size

- list_browser->style: style_name=name, style=style
    Display any item with `dict_item <->style` = name using the attributes
    from the given style object.

    Suppose one has a list_browser that visualises a lexicon and we want the
    present all items that also appear in a glossary on the same domain in
    bold-face.   This may be achieved using:

    	show_lexicon :-
    		new(B, browser('Lexicon')),
    		send(B, style, glossary, style(bold := @on)),
    		(   lexicon(Item),
    			send(B, append, new(D, dict_item(Item))),
    			(   glossary(Item)
    			->  send(D, style, glossary)
    			;   true
    			),
    			fail
    		;   true
    		),
    		send(B, open).

- list_browser->tab_stops: vector*
    Set a number of tabulator stops.  The argument is a vector with pixel
    locations of the tab stops.  See also `text_image ->tab_stops`.

    This feature may be used for multi-column tables and is especially
    useful when the items in the browser are displayed in a proportional
    font.   The following example displays a database of persons where
    each person has a unique id and a name.  The database is in the
    predicate person/2.

    	?- new(B, browser('Persons')),
    	   send(B, tab_stops, vector(200)),
    	   forall(person(Id, Name),
    			  send(B, append,
    				   dict_item(Id, string('%s\t%s',
    										Id, Name)))),
    	   send(B, open).

- list_browser->typed: event|event_id
    Activates the key_binding object <-key_binding.  <-key_binding is
    filled in ->initialise with an empty sub-keybinding of the default
    key_binding named `list_browser` from @key_bindings.

    The default key_binding object implements incremental search.  See also
    class key_binding and class editor.

- list_browser->unlink
    Unlinks the <-dict from the list_browser object and then invokes
    `device ->unlink`.

    **Bugs**:
    Implicitely created dict objects generally will not be garbage collected
    as a dict has internal references.

- list_browser->width: int
    *Inherits description from*: list_browser<-width


## Get methods {#class-list_browser-get}

- list_browser<-contains: -> chain
    Return the `dict <-members` chain of the <-dict.

- list_browser<-dict_item: event -> dict_item
    dict_item object on which event occurred.  This message may be
    used to redefine the list_browser event handling.

- list_browser<-height: -> characters=int
    Set/Query the height in lines.  See also <-width and <->size.

- list_browser<-label: -> name
    A list_browser may be assigned a label.  When a label is assigned, a
    <-label_text will be displayed on the list_browser.  See also
    <->show_label.

    Note that class browser inherits ->label from class window.

- list_browser<-view: -> int

- list_browser<-length: -> int
    The triple <-length, <-start and <-view are used by the <-scroll_bar
    and describe the following properties:

    	| <-length | Number of items in the browser |
    	| <-start  | First item on the window       |
    	| <-view   | Number of items on the window  |

- list_browser<-master: -> device
    Return the <-window is this is an instance of class browser.  See
    `visual <-master` for details.

- list_browser<-selection: -> chain|dict_item
    Get the value of the variable `selection`.  Defined as a method to
    overrule the corresponding method defined on class `device`.

    @see list_browser-selection

- list_browser<-show_label: -> bool
    If @on, the label is visible.  See also <-label_text and <->label.

- list_browser<-width: -> characters=int
    Width of the list_browser in the <-font's `font <-ex` units.

