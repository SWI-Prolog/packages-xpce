# class menu {#class-menu}

A Menu is a dialog item that allows the user the select one or more
values from a list.  In its functionality it is similar to a
list_browser object.  However, a list_browser is meant to select from a
dynamically generated list that may be large, while a menu is
meant to select from a small number of alternatives that is
generally static.

Class menu defines a large number of options that determine both the
layout of the items, the feedback provided for the selected item, and
whether or not one or more items may be selected at the same time.
For convenience, there is a method called `Menu ->kind` that allows
you to select some common configurations with a single message.


## Message forwarding {#class-menu-message-forwarding}

- * <- multiple_selection = @on

If the `toggled` menu_item has <->message not equal to @default,
this message is executed with:

	| @receiver | The menu                            |
	| @arg1     | The menu_item                       |
	| @arg2     | New value of `menu_item <-selected` |
	| @arg3     | Equivalent to @event                |

If the message of the menu_item is @default, the message associated with
the menu is executed.  The arguments are:

	| @receiver | The menu                            |
	| @arg1     | `menu_item <-value`                 |
	| @arg2     | New value of `menu_item <-selected` |
	| @arg3     | Equivalent to @event                |

- * <- multiple_selection = @off

If the `toggled` menu_item has <->message not equal to @default,
this message is executed with:

	| @receiver | The menu             |
	| @arg1     | `menu_item <-value`  |
	| @arg2     | Equivalent to @event |

If the message of the menu_item is @default, the message associated with
the menu is executed.  The arguments are:

	| @receiver | The menu             |
	| @arg1     | The <-selection      |
	| @arg2     | Equivalent to @event |


## Menu layout {#class-menu-menu-layout}

A menu consists of a label and a list of items.  The label is placed in
a box described by the area `Menu <-label_area`.  The items are placed
in a two-dimensional array, each in its own box.  These item boxes all
have the same size (`Menu <-item_size`).  The top-left-corner of the
first item is at `Menu <-item_offset`.

If `Menu <->layout` equals horizontal, the items are placed
left-to-right.  If `Menu <->layout` equals vertical, the items are placed
top-to-bottom.  The items are spread over `Menu <-columns` columns.
The following illustrates the placement of a menu with
horizontal layout, 2 columns and 5 items:

	label   item-1   item-2   item-3
	        item-4   item-5

Various parameters influence the final layout.  `Menu<->gap` defines the
horizontal and vertical space between the items.  `Menu<->pen` defines
the thickness of the lines for the boxes drawn around the items.
`Menu<->border` defines the white space between the item and the box
around it.

@see menu->initialise
@see menu_item-message
@see menu-columns
@see menu->kind
@see dialog_item-auto_value_align
@see class menu_item
@see class popup


## Instance variables {#class-menu-instvars}

- menu<-accelerator_font: font=font*
    Font in which keyboard accelerators are annotated.  When @nil,
    possible accelerators are not annotated.  Used by class popup.

- menu<-border: width=0..
    Border (in pixels) between the item and its box if `Menu <-pen` is not
    zero.  When `Menu<-pen` equals 0, border is only an additional gap.

    @see menu->border
    @see menu-gap

- menu<-columns: number=1..
    Number of columns over which the menu-items are spread.  See the class
    documentation for details.

    **Defaults**: The default is 1

    @see menu-layout
    @see class menu

- menu-default: value=any|chain|function*
    Initial value as well as the value to which the <-selection is restored
    by ->restore.   If the menu has <-multiple_selection: @off this is a
    single value, otherwise it is a chain of values.

    The default value may also be a function object.  In this case
    ->restore evaluates this function and uses the result to set the
    <-selection.

- menu<-feedback: feedback={box,image,show_selection_only}
    Style parameter that determines the visual feedback for the selection.
    The possible values are:

    - show_selection_only
    	Only applicable when `Menu <->multiple_selection` equals @off.
    	In this mode, only the selected item is displayed.  Used to
    	implement `Menu <->kind: cycle`.

    - box
    	Draw a box around the selected item(s).  The thickness of the
    	line equals the pen of the menu plus 1.

    - invert
    	Invert the selected item(s).  Used to implement `Menu ->kind:
    	choice'.  Note that inverting images gives strange results on
    	colour displays.

    - image
    	Indicate the selection by painting `Menu <-off_image` left of
    	not-selected items and `Menu <-on_image` left of selected
    	items.  Used to implement `Menu ->kind: `marked` or `toggle`.

    @see menu_item-selected
    @see menu-kind

- menu<-format: alignment={left,center,right}
    The layout of menu_items in a menu is a 1 or 2 dimensional table.  The
    menu will first determine the number and size of the rectangular cells
    in this table.  This parameter determines how the items are aligned in
    their cell.

    @see text-format

- menu<-gap: size
    Amount of white space between the items in both X- and Y-direction.
    Note that the `Menu <->border` is between the item and its surrounding
    box, while `Menu <->gap` defines the distance between the surrounding
    boxes of adjacent items.

    @see menu-border

- menu<-item_offset: offset=point
    Offset in pixels of the first item, relative to the origin of the menu.
    Normally not useful for application programmers.

    @see menu-label_area
    @see menu-item_size

- menu<-item_size: size=size
    Size of the box in which each item is formatted.  An item at location
    (X,Y) is located at x-pixel location:

    	Menu?x + Menu?item_offset?x +
    	X*(Menu?item_size?w + Menu?gap?w).

    and similar for the y-coordinate.

    @see menu-label_area
    @see menu-item_offset

- menu<-kind: kind={cycle,marked,choice,toggle,popup,cycle_popup}
    The kind attribute is a shorthand for setting various style attributes
    to some predefined value.  The variable `Menu <-kind` provides the
    argument of the latest `Menu ->kind` invocation.  Note that the menu
    may look very different due to subsequent style-parameters changed.

    @see menu->initialise
    @see menu->kind
    @see menu-feedback

- menu<-label_area: area=area*
    Pixel area relative to the origin of the menu for formatting the label.
    Only applicable if `Menu <->show_label` equals @on.

    @see menu-item_size
    @see menu-item_offset

- menu<-label_font: font*
    Font used to format the label.  Only applicable if `Menu <-show_label`
    equals @on.

- menu<-layout: orientation={horizontal,vertical}
    Determines whether subsequent items are places left-to-right
    (horizontal) or top-to-bottom (vertical).  If `Menu <-columns` is
    greater than 1 and layout is `horizontal`, the items are placed in
    horizontal rows.  If layout is `vertical`, the items are placed in
    vertical columns.

    @see menu-columns

- menu<-left_offset: offset=0..
    Left margin between the item-box and the item itself.  Used to reserve
    space for placing the selection markers if present.  Its value is 0 if
    both `Menu <-on_image` and `Menu <-off_image` equal @nil and the
    maximum width plus `Menu <-border` if one or both of the markers are
    defined.

- menu<-members: items=chain
    Chain of menu-items that are part of the menu.

    @see menu_item-menu
    @see menu->append

- menu<-multiple_selection: multiple=bool
    If @on, any item can be either selected or deselected.  It @off, exactly
    one item has status `menu_item <->selected: @on`.

    @see menu_item-selected

- menu<-off_image: image=image|{marked}*
    Image painted in front of items that have `MenuItem <->selected`: @off.
    When @nil, no image is painted before the item.

    @see menu-on_image

- menu<-on_image: image=image|{marked}*
    Image painted in front of items that have `MenuItem <->selected`: @on.
    When @nil, no image is painted before the item.

    @see menu-off_image

- menu<-popup_image: image=image*
    Image painted to the right of items that have a popup associated with
    them.  Only applicable to popup menu's with pullright submenu's in them.

- menu<-preview: item=menu_item*
    Menu_item in the preview state. Currently only used for popup-menu's.

    @see menu-preview_feedback

- menu<->preview_feedback: feedback={box,rounded_box,inverted_rounded_box,invert,colour}
    Visual feedback used to indicate the item that is in the preview state.
    Possible values:

    - box
    	Draw a box around the item (like `Menu ->feedback: box`)

    - invert
    	Invert the box in which the item is formatted.  Similar to
    	`Menu ->feedback: invert`, but the latter only inverts the
    	item.

    @see menu-preview

- menu<-right_offset: offset=0..
    Right-margin between the item and its surrounding box.  Its value is 0
    if `Menu <-popup_image` equals @nil or there is no item in the menu that
    has a <->popup.  Otherwise it is the width of `Menu <-popup_image` plus
    `Menu <-border`.

- menu<-show_label: show=bool
    If @on, the label is formatted in `Menu <-label_font` in the box
    determined by `Menu <-label_area`.  Otherwise the label is invisible.

- menu<-value_font: font=font
    Default font used to display the text of the items.  This font may be
    overruled for an individual item using `menu_item <->font`.  The latter
    is often used for font-selectors to display each item in the font that
    will be selected.

    @see menu_item-font
    @see menu_item->font

- menu<-value_width: width=0..
    Minimum width used for the value.  It is used for two purposes:

    - popup menu's
    	It is defined for popup-menu's to avoid very small windows when
    	only short options are provided.

    - Alignment of menu's
    	The automatic layout mechanism of dialog window will align the
    	values of menu's displayed below each other if their layout is
    	horizontal.

    @see dialog_item-auto_value_align


## Send methods {#class-menu-send}

- menu->_changed_item: changed=menu_item
    Internal method to deal with changed items.  When a menu-item is changed
    directly, it will send __changed_item_ to its menu.

- menu->active_all_items: active=bool
    If @on, all items will be made active; otherwise all items will be made
    inactive.

    @see menu->all_off
    @see menu_item-active
    @see menu->update

- menu->active_item: item=member:menu_item, active=bool
    (De) activate a single item indicated by `object`.  _Object_ is either a
    menu_item or the value of a menu_item.  Example:

    	?- new(M, menu(fill, marked)),
    	   send_list(M, append,
    				 [ @nil,
    				   @grey12_image,
    				   @grey25_image
    				 ]),
    	   send(M, active_item, @grey12_image, @off).

    @see menu<-active_item
    @see menu->update

- menu->all_off
    Equivalent to

    	send(Menu, active_all_items, @off).

    Available for backward compatibility.

    @see menu->all_on
    @see menu->active_all_items

- menu->all_on
    Equivalent to

    	send(Menu, active_all_items, @on).

    Available for backward compatibility.

    @see menu->all_off

- menu->append: item=menu_item
    Append a menu-item to the menu.  The appended item will always be the
    last.  Note that class menu_item defines a conversion function that
    converts popups (for pullright menu's) and values into menu_items.

    For popup objects, if the argument is the name `gap`, the last
    added item is sent `menu_item ->end_group: @on`.  This inserts a
    group-separation line in the menu.

    See also ->members, ->insert_before and send_list/3.

    @see menu->prepend
    @see menu_item<-convert
    @see menu-members

- menu->apply: always=[bool]
    If <-modified yields @on or `always` equals @on *and* <-selection
    succeeds, <-message is forwarded with the following arguments:

    	| @receiver | the menu     |
    	| @arg1     | <-selection. |

    See also `dialog ->apply`.

- menu->border: width=0..
    @see menu-border

- menu->clear
    Delete all items from the menu.

- menu->clear_selection
    Deselect all items.  See also ->selection.

    @see menu->selection

- menu->compute
    `Menu ->compute` recalculates various variables to speed-up redraw.
    These are <-label_area, <-item_offset, <-item_size, <-left_offset and
    <-right_offset.

- menu->delete: item=member:menu_item
    Delete a menu_item or the menu_item that has the same <->value as the
    argument from the menu.  Note that it is generally bad style to change
    the contents of menu's.  Normally items that should not be activated in
    some context are made inactive.

- menu->event: event
    First tries `dialog_item ->event`.  On failure it passes the event of
    @_button_gesture if <-active equals @on.   The real work is done
    by ->execute.

    @see menu<-item_from_event
    @see menu->execute

- menu->execute: event=[event]
    Execution of a menu implies setting the selection and -if there is a
    message to execute-, execute this message.  The exact behaviour
    is determined by various style parameter:

    - `Menu <-feedback` equals show_selection_only
    	Select the next item

    - `Menu <-multiple_selection` == @on
    	Toggle the <->selected attribute of the item on which the
    	event occurred.

    - `Menu <-multiple_selection` == @off
    	Make the selection of the menu the item on which the
    	event occurred.

    @see menu->event

- menu->initialise: name=[name], kind=[name], message=[code]*
    Create from its ->label, ->kind and ->message.   See class
    menu for details.

    **Defaults**:
    The default ->label is the classname of the menu (`menu`).  The default
    ->kind is determined by the resource _Menu.kind_.  The default message
    is @default.

    @see dialog_item-message
    @see menu-kind
    @see dialog_item-label
    @see class menu

- menu->insert_before: item=menu_item, before=name|menu_item
    Insert item before another one.  If `before` is a name, <-member
    is used to translate it into a menu_item.  If `before` is not in
    the menu, the new item is ->append'ed to the menu.

    See also ->append and ->prepend.

- menu->is_off: item=member:menu_item
    Find menu-item from object and test whether <->active equals @off.

    @see menu->is_on

- menu->is_on: item=member:menu_item
    Find menu-item from object and test whether <->active equals @on.

    @see menu->is_off

- menu->kind: kind={cycle,marked,choice,toggle,popup,cycle_popup}
    The `Menu ->kind` method allows for the selection of some commonly
    occurring combinations of menu parameters with a single method.  The
    available options are:

    - cycle
    	Only display the selection.  Clicking on the menu selects the
    	next right-item.  The menu has a popup-menu associated that
    	allows for an overview of all available items.  Useful for not
    	very commonly used menu's as it uses much less space on the
    	screen.

    - marked
    	Single selection menu with markers (see `Menu <-on_image` and
    	`Menu <-off_image`) to indicate the selected item.

    - toggle
    	As marked, but multiple items may be selected.

    - choice
    	As marked, but the selection is indicated by inverting the it.

    - popup
    	Used internally.  Popup menus are implemented on top of class
    	menu.

    - cycle_popup
    	Used internally for the popup of <-kind cycle menus.

    @see menu-kind
    @see class menu

- menu->member: item=member:menu_item
    Succeeds if the argument is a member menu_item.

    **Bugs**:
    Used to set the notion of <->current in older versions.  In its current
    definition it is not very useful.  Use <-member to test membership.

    @see menu<-member

- menu->members: chain
    ->clear the menu and then use ->append to add each member of the
    chain to the menu.  This implies chain may contain both menu_item objects
    and plain names.

    See also `menu_item<-convert` and send_list/3.

- menu->modified: modified=bool
    *Inherits description from*: menu<-modified

- menu->next: direction=[{down,up}]
    Selects the first item with `menu_item <-active: @on` after the
    currently selected menu_item.  When the currently selected item is the
    last or there is no currently selected item, the first active item is
    selected.

    Used internally when <-show_selection_only equals @on (<->kind:
    `cycle`).

- menu->off: item=member:menu_item
    Invokes `menu_item ->active: @off` on the indicated menu-item, making
    the indicated menu-item insensitive.

    @see menu_item-condition
    @see menu_item-active
    @see menu->on
    @see menu<-member

- menu->on: item=member:menu_item
    As `menu ->off`, but activates the menu_item.

    @see menu->off

- menu->prepend: item=menu_item
    Associate the argument menu_item as first rather than last item of the
    menu.  See also ->append and  ->insert_before.

    @see menu->append

- menu->selected: item=member:menu_item, selected=bool
    Set `menu_item <-selected` of `item`.  Equivalent to sending the
    message directly to the menu_item object, but type-conversion
    will convert a `menu_item <-value` to the corresponding
    menu_item.

- menu->selection: selection=member:menu_item|chain*
    Select the specified menu_item or chain of menu_items.  All other
    menu_items are deselected.

    If the argument is @nil, this is equivalent to ->clear_selection,
    - unless* there is a menu_item with `menu_item <-value: @nil`.
    Backward compatibility.

    **Bugs**:
    Does not check <-multiple_selection, resulting in an inconsistent menu
    when multiple object are selected in a menu with <-multiple_selection:
    @off.

    @see menu<-selection
    @see menu_item-selected
    @see menu->clear_selection

- menu->status: {inactive,active,preview,execute}
    The <-status of a menu is not used.

- menu->unlink
    Sets `menu_item <-menu` of all member items to @nil, clears ->members
    and invokes ->unlink on the super-class.

    @see dialog_item->unlink

- menu->update: context=[any]
    Update the activation of the menu_items in this menu.  For each
    menu_item that has a <->condition not equal to @nil, it invokes the
    `menu_item <->condition` attribute, providing the following context
    information:

    	| @receiver | The menu_item                   |
    	| @arg1     | The argument (i.e. the context) |

    If the message evaluates successfully, the menu_item is activated,
    otherwise it is deactivated.

    @see menu->active_item
    @see menu->active_all_items
    @see popup->update


## Get methods {#class-menu-get}

- menu<-active_item: item=member:menu_item -> active=bool
    Returns a boolean, indicating the active status of the referred to
    menu_item.

    @see menu->active_item

- menu<-contains: -> items=chain
    Returns the <-members chain of menu_item objects.

- menu<-item_from_event: event=event -> item=menu_item
    When `event` is an event that occurred on the menu, return the menu_item
    on which are it occurred.  May be used to redefine event-handling of
    menu's.

    **Diagnostics**: Fails silently when the event did not occur on a menu_item's area.

    @see menu->event

- menu<-member: value=any -> member=menu_item
    When the argument is a menu_item, return this menu_item if it is part of
    this menu.  Otherwise, find the menu_item that has <->value equal to the
    provided value and return this.

    **Diagnostics**: Fails silently on failure.

    **Bugs**:
    For backward compatibility, if there is no item with `menu_item <-value`
    that matches the specification, it will return the first item for
    which the conversion of the argument and `menu_item <-value` to a
    name yield the same name.

    @see menu->member
    @see menu->off

- menu<-modified: -> modified=bool
    Yields @on if the user has modified the menu since the last
    ->selection, ->modified: @off or ->default. See also ->apply.

    If ->modified is set to @on the <-device will be informed using
    `dialog ->modified_item`.  See `dialog_item ->modified`.

- menu<-selection: -> values=any|chain*
    When <-multiple_selection equals @off, returns `menu_item <-value` of
    the selected menu_item.  Otherwise it returns a new chain with the
    <-value for each of the selected items (the chain may be empty).

    **Diagnostics**:
    Fails if <-multiple_selection equals @off and no items are selected.
    This should only happen it there are no items in the menu.

    @see menu->selection

