# class popup {#class-popup}

A `popup` is a menu that is associated with a graphical object, but is
normally invisible.  When the user depresses a predefined button, the
popup is `pop`ed-up'.  After the user has made a choice, the popup is
removed from the display again.

Some types of graphicals have an attribute called <->popup.  For these
types the easiest way to attach a popup to them is to fill this
attribute with a popup menu.  General graphicals don't have this
feature.  For these graphicals a popup_gesture object has to be used to
connect the popup with some event.  See also `graphical ->popup` and
`graphical ->event`.

PCE encourages you to associate popup-menus with the right mouse-button
only.

By default, a popup menu inverts the current item.  This gives odd
results on colour displays.  You can change this into a box by adding
the following line to your Defaults file.

	popup.preview_feedback: box

The method ->execute is activated to execute the behaviour
associated with the selected item.  The documentation of this
method describes the available context parameters.

Forwarding rules for associated messages are described with
->execute.

**User interface**:

When (normally) the right button is depressed on an object that has a
popup associated to it, the popup will be mapped on the display such
that the first option is below the mouse.  If possible, the mouse itself
is not moved.

The user should HOLD the mouse-button depressed and DRAG to the
approprate choice to RELEASE the mouse-button there.  This will invoke
the associated command.

The user can CANCEL the operation by dragging the mouse-button outside
the popup and releasing it.

If an item has a mark _=>_ at the right side, SUB-MENU can be obtained
by dragging towards this mark.

**Bugs**:

Interface styles such as `stay-up`, OpenWindow like push-pins, etc.  are
not supported.

@see graphical->popup
@see class popup_gesture
@see class menu_item
@see class menu
@see class menu_bar
@see topic Using menu's
@see window-popup
@see menu_item-popup
@see list_browser-popup
@see dialog_item-popup


## Instance variables {#class-popup-instvars}

- popup<->default_item: {first,selection}|any*
    Determines the initial positioning of the menu.  Values:

    - first
    	Position the pointer on the first item with `menu_item<-active: @on`.

    - @nil
    	As `first`, but places the pointer a few pixels to the left of
    	the item so that it is not immediately selected.

    - selection
    	Position the pointer on the current <-selection of the popup.
    	In practice this is the last selected item.

    - Anything else
    	Position the pointer on the menu item with the specified
    	`menu_item <-value`.

    See also

- popup<-margin: margin=0..
    Margin to the left-and-right of the menu for generating the
    window size.  See also `menu <-border`.

- popup<->update_message: code*
    If not @nil, this message is invoked by `Popup ->update: context`.  This
    message is meant to generate the entries of the popup.  Activating
    individual items is normally done by the condition message associated
    with each menu_item of the popup.

    When executed, the following arguments are forwarded:

    	| @receiver | The popup itself         |
    	| @arg1     | The argument of ->update |

    **Defaults**: By default, no updating takes place

    @see menu_item-condition
    @see popup->update


## Send methods {#class-popup-send}

- popup->close
    Remove the popup from the display.  First it will ->close possible open
    pullright menus.  Then is removes the popup menu from its <-window
    and adds the window to the pool of window objects used to display
    popup menu's.

    This method is always called as a normal message and thus may be
    redefined.

- popup->drag: event, check_pullright=[bool]
    Handle a drag event.  This method is called from ->event.  It
    should not be used, nor be redefined, except for emergencies.

    This method locates the `current` item using <-item_from_event
    and activates this using ->preview.  If the drag event is on an
    item with a `menu_item <-popup` and the event is near the popup
    marker (see <-popup_image), the pullright menu is activated by
    invoking ->show_pullright_menu.

- popup->end_group: bool
    If this is a pullright menu, set `menu_item ->end_group` in the
    associated menu_item.  This will put a line below the item.

- popup->event: event
    Performs the following steps

    - If the popup has a -pullright and the event is on an item of
    	this menu, ->close the pullright menu.

    - If the popup has a -pullright *and* the event is a button-up
    	of the same button that started the popup, assign
    	<-selected_item with the pullright menu, assign -pullright with
    	@nil and ->close the pullright.

    - If there is no -pullright and the event is the button-up
    	event, assign <-selected_item with <-preview and ->close
    	the popup.

    - If the event is a button-down, set <-selected_item to @nil and
    	send <-button to the `event <-button`

    - If the event is a drag event, update ->preview.  If a
    	pullright area is entered, ->update the pullright and ->open it.

- popup->execute: context=[object]*
    This method is normally invoked from `popup_gesture ->terminate`.  The
    argument (denoted `context`) is normally the object to which the popup
    menu is associated.

    First it locates the selected menu_item object and the message to
    execute.  This message is `menu_item <-message` if this does not
    yeild @default, otherwise <-message.  The forwarded arguments are:

    - <-multiple_selection: @off using `menu_item <-message`

    		| @receiver: | The popup            |
    		| @arg1:     | The context argument |

    - <-multiple_selection: @off using `popup <-message`

    		| @receiver: | The popup            |
    		| @arg1      | `menu_item <-value`  |
    		| @arg2:     | The context argument |

    - <-multiple_selection: @on using `menu_item <-message`

    		| @receiver: | The popup              |
    		| @arg1:     | `menu_item <-selected` |
    		| @arg2:     | The context argument   |

    - <-multiple_selection: @on using `popup <-message`

    		| @receiver: | The popup              |
    		| @arg1:     | `menu_item <-value`    |
    		| @arg2:     | `menu_item <-selected` |
    		| @arg3:     | The context argument   |

    @see popup_gesture->terminate

- popup->initialise: name=[name], message=[code]*
    Create a popup menu from its name (which is only displayed if
    <-show_label equals @on) and a message.  This message will be executed
    when a menu_item object is selected that has `menu_item <-message:
    @default'.   Using a message at the popup (menu) level is commonly used
    if the menu selects from a value-set (fonts, colours, names.  ...).  If
    the menu is used to activate commands the actions are normally
    associated with the individual items.

- popup->open: on=graphical, offset=point, offset_is_pointer=[bool], warp=[bool], ensure_on_display=[bool]
    Open a popup menu on the display.  Normally invoked from a popup_gesture
    object.   The arguments are:

    - on
    	Graphical (window) for which the popup is displayed (<-context)

    - offset
    - offset_is_pointer (default `@on`)
    	Position of the top-left corner of the popup relative to the
    	<-context iff offset_is_pointer equals @off, otherwise the
    	position of the pointer.

    - warp_pointer (default `@on`)
    	If the popup is moved (see ensure_on_display), move the
    	pointer accordingly.

    - ensure_on_display (default `@on`)
    	If @on, move the popup if it would otherwise be
    	(partly) outside the display.

    @see popup_gesture->initiate

- popup->reset
    Equivalent to ->close.

- popup->show_pullright_menu: item=menu_item, event=[event], context=[any]
    Show pullright for this item.  Called from ->drag.  `item` is
    the item for which to show a pullright menu.  First updates
    the `menu_item <-popup` and -if the popup has <-members-
    opens the popup.

- popup->update: context=any
    This behaviour is normally invoked just before the popup-menu is mapped
    on the display.  This is done automatically for popups associated with
    menu_bars or managed by a popup_gesture (i.e. all normal ways to
    manage a popup).

    Update first invokes <->update_message, providing its argument as @arg1.
    This message is supposed to update the available entries of the popup;
    not the activation status of the individual entries.  Next, ->update
    invokes `Menu ->update: context`, which takes care of activating the
    appropriate entries.

    @see menu_item-condition
    @see menu->update
    @see popup-update_message
    @see popup_gesture->initiate

