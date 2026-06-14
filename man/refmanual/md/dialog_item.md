# class dialog_item {#class-dialog_item}

Class dialog_item describes the communication between a
`dialogue window` (class dialog).  and an item in such a window.
Besides the normal communication between graphicals and windows,
this entails:

- Automatic layout of dialog windows (`dialog ->layout`).
- Forwarding changes (`dialog ->modified_item`).
- _Applying_ a dialog window (`dialog ->apply`).

Dialog Item's are normally appended to a dialog using
`dialog->append`.  In some cases there spatial relationship
is specified using ->above, ->below, ->left or ->right.

Fine tuning of layout is achieved using ->auto_align,
->alignment, ->auto_label_align, ->auto_value_align and
->reference.

Dialog items may also be displayed on normal graphical device
objects (including window objects).  With automatic layout, this
is achieved using `device ->append_dialog_item` and `device
->layout_dialog'.  Without automatic layout using `device
->display'.

@see class slider
@see dialog->layout
@see class dialog
@see class graphical
@see class event


## Class variables {#class-dialog_item-classvars}

- dialog_item.label_suffix: name = :
    Suffix added to the label of this dialog item if not already
    present.   The only sensible values are ':' or ''.   See also
    `char_array <-ensure_suffix`,  `char_array <-label_name`
    and `dialog_item ->initialise`.


## Instance variables {#class-dialog_item-instvars}

- dialog_item<-above: graphical*
    Dialog-item immediately above me.

    **Defaults**: @nil (no item is above me)

    @see dialog->layout

- dialog_item<-active: bool
    Determines whether the item is sensitive for user-events.  If @off, the
    item's image is greyed-out by `anding` it with a grey pattern.

    **Defaults**: @on (active).

- dialog_item<->alignment: {column,left,center,right}
    Determines how the dialog is fitted in the two-dimensional
    space of the dialog window by `dialog ->layout`.   The values
    are:

    - column
    	Align the item below the upper-neighbour

    - left
    	Place the item immediately right of its
    	left-neighbour.  (left means `flush-left`).

    - center
    	Place centered items adjacent and centered in the
    	available horizontal space.

    - right
    	Flush the item to the right of the available
    	horizontal space.

    For example, button objects in OpenWindow dialog windows should
    be centered.  Setting button.alignment to center will properly
    align buttons for OpenWindow look-and-feel.

- dialog_item<->auto_label_align: bool
    Align the width of the label automatically with the labels of the items
    in the same column.

    **Defaults**: @on (align labels in the same row).

    @see dialog_item-label_width
    @see dialog->layout

- dialog_item<->auto_value_align: bool
    Automatically align the width of the values.  Currently applicable to
    menu's only, where it refers to the width of the entries in the menu.

    **Defaults**: @on (do align values)

    @see menu-value_width
    @see class menu
    @see dialog->layout

- dialog_item<-below: graphical*
    Dialog item immediately below me.

    @see dialog->layout

- dialog_item<-label: char_array|image*
    Label of the item.  For most items, the label is displayed in the
    `label_font` and indicates the function of the item.

    **Defaults**:
    The provided name (first initialisation argument) is capitalised to
    provide the initial label.

    @see menu->initialise

- dialog_item<-label_format: {left,center,right}
    Determines how labels of dialog objects aligned in a vertical
    column are aligned by `dialog ->layout`.   `Dialog ->layout`
    first determines the widest label, after which it requests all
    dialog items to make their label equally wide.  This parameter
    determines how a small label is aligned in a wider box.

    See also dialog_item.label_suffix.

- dialog_item-label_width: [int]
    The width of the area in which the label is displayed.  Used to
    implement automatic alignment of labels of items displayed in a vertical
    row.  See also `dialog ->layout`.

    @see dialog_item-auto_label_align
    @see dialog->layout

- dialog_item<-left: graphical*
    Item immediately to the left.

    @see dialog->layout

- dialog_item<-look: {open_look,motif,win,gtk}
    General look-and-feel switch.  The interpretation is left to the
    subclasses.  See also `scroll_bar <->look`.

    XPCE is shipped with Defaults both to generate OpenLook
    style (Unix/X11) or Windows-95 style (Win32).

- dialog_item<->message: [code]*
    Message executed when the item is executed.  The conditions on which the
    execution takes place as well as the arguments forwarded are determined
    by the specific type of item.

    @see menu->initialise
    @see dialog_item-execute_event

- dialog_item<->popup: popup*
    Popup menu associated with the item.

    @see class popup

- dialog_item->reference: [point]
    Reference point for alignment in two-dimensional grid.  When to items
    are aligned horizontally or vertically, `dialog ->layout` aligns the
    <-x or <-y component of their reference points.

    The get-method returns the value of the variable when filled and
    fails otherwise.  This method is redefined by most dialog-item
    subclasses (`button <-reference`, `text_item <-reference`,
    etc.).

    Invoking ->reference: @default will make the item return its
    class-defined default.  Invoking this method with an instance
    of class point will explicitly set the reference-point.

- dialog_item<-right: graphical*
    Item immediately to the right.

- dialog_item<-status: {inactive,active,preview,execute}
    The status variable describes the current status with regard to
    processing user-events.  The values are:

    - inactive
    	Default initial value

    - active
    	In focus of events; the item may indicate so.

    - preview
    	Indicates releasing the button now will start execution

    - execute
    	Indicates the <->message is currently executing

    **Defaults**: inactive

    @see dialog_item-execute_event
    @see dialog_item-cancel_event
    @see dialog_item-release_event_focus
    @see dialog_item-preview_event
    @see dialog_item-obtain_event_focus


## Send methods {#class-dialog_item-send}

- dialog_item->cancel
    Cancel current gesture.  Equivalent to ->status: inactive.

- dialog_item->default: any
    Virtual method.  Defined by subclasses that may be modified.  It assigns
    the initial value as well as the value to which the item is ->restore'd.

- dialog_item->device: device*
    If the device is changed all <-above, etc.  relations are destroyed
    first.

- dialog_item->event: event
    Show <-popup when defined.

    First invoke `graphical ->event`.  If this fails and <-popup is not
    @nil, open this popup menu.  This method is redefined by most
    subclasses.

- dialog_item->initialise: name=name
    Create a dialog_item from its name.  Class dialog_item is just a
    super-class of the real dialog items and it is of very little use to
    create a instance of this super-class.	`<- label` is filled with
    `char_array <-label_name` and the label is guaranteed to end
    with Pce.DialogItem.label_suffix.

- dialog_item->modified: bool
    When @on, this method invokes ->modified_item: @receiver, @on
    on its <-device (normally `dialog->modified_item`).  When @off
    is method just succeeds.

    This method is redefined by subclasses of dialog_item that allow the
    user to modify the selection.

    See also ->restore, <-default and `dialog ->modified_item`.

- dialog_item->open
    Convenience method: if the dialog_item is not yet placed in a dialog
    window, place it in a default dialog window and open this window on
    the display, so

    	?- send(text_item(name), open).

    Suffices to create a default text_item object in a window on the
    display.

- dialog_item->reset
    Reset the <-status to inactive.  This will reset dialog items if the
    execution is aborted from the activated code.

- dialog_item->show: bool
    *Inherits description from*: dialog_item<-show

    @see graphical-displayed

- dialog_item->value_width: [int]
    *Inherits description from*: dialog_item<-value_width


## Get methods {#class-dialog_item-get}

- dialog_item<-default: -> any
    The <-default of a dialog_item defines the value to which the
    <-selection will be reset by ->restore.  The <->default mechanism is
    defined for all sub-classes of class dialog_item that may be modified by
    the user.

- dialog_item<-modified: -> bool
    Virtual method.  Subclasses that define items which may be modified by
    the user (class text_item, class menu) redefine this method to return
    @on after the user has modified the item.  See also ->apply and
    `dialog ->modified_item`.

- dialog_item<-show: -> bool
    Equivalent to <->displayed.  Retained for backward compatibility.

- dialog_item<-value_width: -> int
    Methods used to align the values of dialog_items placed above each
    other.  Implemented for class menu to align the field width when
    multiple menu's are in a vertical column,

