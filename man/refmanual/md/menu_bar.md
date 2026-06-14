# class menu_bar {#class-menu_bar}

A menu_bar represents a series of popup objects operated as pulldown
menus.  See for example the menu_bar in the PCE manual window.  Various
techniques are commonly used to give commands through graphical
interfaces:

- A menu_bar object
	The menu_bar contains all possible actions on the application.
	The actions are grouped in pulldown menu's.  If an action needs
	an argument, this is normally the selection.

- Popup object
	(Graphical) objects in the application have a popup object
	attached to them.  This menu contains operations that can be
	performed on this object.  Normally the popup menu is associated
	with a standard (the right-) mouse-button.

- Gesture objects
	Direct manipulation of objects.  Examples are dragging an object
	onto the waste-basket to delete it, moving objects directly with
	the mouse, etc.  See class gesture.

- Keyboard accelerators
	This is normally done in combination with menu-bars.  Instead of
	choosing a menu-entry from one of the pulldown menus, a
	predefined keystroke combination may be typed.

Applications may chose for any of these techniques or a combination
thereof.   See also example _Menu Bar_.

@see topic Activating commands
@see class menu_item
@see class popup


## Class variables {#class-menu_bar-classvars}

- menu_bar.label_font: font = normal
    @see menu_bar-label_font

- menu_bar.size: size = size(80,20)
    The minimum size for a box to display a label.  See ->compute and
    <-item_size.

    @see menu_bar->compute


## Instance variables {#class-menu_bar-instvars}

- menu_bar-button: button_name*
    Name of the button (see `event <-button`) that caused the first menu to
    appear.  Used internally.

- menu_bar<-buttons: chain
    Chain holding button objects to represent each of the entries.
    When a popup object is ->append'ed, a button is added to
    this chain to visualise the popup.  The `button <-popup`
    slot refers to the popup.

    This implementation allows for easy emulation of the OpenLook
    look-and-feel style as OpenLook does not define menu-bars.

- menu_bar<-current: popup*
    Currently visible popup object or @nil if the menu_bar is inactive

- menu_bar<-format: {left,center,right}
    Describes how the labels are formatted in their boxes.  See also ->pen
    and ->label_font.

    @see menu_bar->label_font

- menu_bar<-label_font: font*
    @see menu_bar.label_font
    @see menu_bar->label_font

- menu_bar<-members: chain
    Chain of popup objects.  The `popup <-labels` are displayed
    left-to-right from from this chain.


## Send methods {#class-menu_bar-send}

- menu_bar->active_member: popup=member:popup, active=bool
    (De) activate the indicated popup object.  When inactive, the label will
    be greyed-out.   Modifies `popup <-active`.

    @see menu_bar->all_active

- menu_bar->all_active: bool
    Invokes ->active_member for all member popups.

    @see menu_bar->active_member

- menu_bar->all_off: member:popup
    @see menu_bar->all_on

- menu_bar->all_on: member:popup
    Activates `menu ->on` on the specified popup object, activating all
    menu_item objects part of it.

    @see menu_bar->all_off

- menu_bar->append: member=popup, alignment=[{left,right}], before=[name|popup]
    Attach a popup object to the menu_bar.  The menu_bar object will
    append a button object to <-buttons with the following
    attributes:

    	| `button <-popup`  | Set to the popup    |
    	| `button <-label`  | `popup <-label`     |
    	| `button <-font`   | Set to <-label_font |
    	| `button <-pen`    | Set to <-pen        |
    	| `button <-radius` | Set tp <-radius     |

    The <-label_font, <-pen and <-radius of the button are
    - not* set from the menu_bar if <-look equals
    open_look.  Because an OpenLook menu-bar really
    is a row of buttons.

    If the `alignment` argument is `right`, this button is flushed
    to the right if the menu-bar is resized to be wider than the
    space required by the buttons.  Motif and Windows 95
    look-and-feel normally flush the help menu to the right:

    	...,
    	new(MB, menu_bar),
    	send(MB, append, new(F, popup(file))),
    	send(MB, append, new(H, popup(help)), right),
    	...,

    @see menu_bar->delete
    @see menu_bar->clear

- menu_bar->assign_accelerators
    Assign accelerators for the items.

    See also `dialog->assign_accelerators` and `menu->assign_accelerators`.

- menu_bar->clear
    @see menu_bar->append

- menu_bar->compute
    Determines the size and <-item_size.  The <-item_size is the largest of
    menu_bar.size and the size needed to display the largest label.

    @see menu_bar.size

- menu_bar->delete: member:popup
    Delete the indicated popup from the menu_bar.

    @see menu_bar->append

- menu_bar->geometry: x=[int], y=[int], width=[int], height=[int]
    Changes the menu-bar geometry.  It the `width` argument is not
    @default and larger than the space required by the buttons, the
    first button with `button <-alignment: right` and all
    succeeding buttons are flushed to the right.  See also ->append.

- menu_bar->initialise: name=[name]
    Create a menu_bar from its label.  The label is not shown, but the
    menu_bar will be given this name.  This may be used to find the
    reference of the menu_bar in the dialog object (see `dialog <-member`).

    **Diagnostics**: The default name is the class name (`menu_bar`).

- menu_bar->label_font: font*
    @see menu_bar-label_font

- menu_bar->off: name|menu_item
    @see menu_bar->on

- menu_bar->on: name|menu_item
    Invokes `menu ->on` with same argument on all member popups.  Always
    succeeds.

    @see menu_bar->off

- menu_bar->show_popup: popup
    Make popup <-current and ->open it.  See also <-current and
    <-popup_from_event.  Used by ->event.


## Get methods {#class-menu_bar-get}

- menu_bar<-contains: -> any
    Returns the chain of <-members popup's

- menu_bar<-hor_stretch: -> 0..
    Horizontal stretchability.  The returned value (1) ensures that,
    if the containing device (normally a dialog object) is given a
    layout, the menu_bar is stretched to the right-edge of the
    window.  See also ->geometry.

- menu_bar<-member: name|popup -> popup
    When the argument is a popup object, return it if it is a member of this
    menu_bar.    When the argument is a name, return the first popup whose
    `popup <-label` equals the argument.

- menu_bar<-popup_from_event: event -> popup
    Find popup to open from event.   This method locates the button
    on which the event took place and returns the associated popup.
    Used by ->event.  See also ->show_popup and <-current.

