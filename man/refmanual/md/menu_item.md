# class menu_item {#class-menu_item}

A `menu_item` is an item of a menu.  Note that class `popup` is a
subclass of menu and therefore menu_items also describe entries of
popups.

The menu_items of a menu are organised in a chain (`menu <-members`).
The menu decides on the layout and how they are displayed.  The menu
also takes care of handling the user-interaction.

The <->value of a menu_item is used to address it in the menu.  The
<->label determines what is shown on the display, It can either
be a string or a bitmap.  The <->message what happens if the
user selects this menu_item.  The message may be @default, in which case
the message associated with the menu is used.  <->end_group
tells the menu this is the last entry of a logical group.  Used by
popup's only.  Finally, the <->condition is a code object that
determines whether or not the item should be active when in a popup.

**User interface**:

Menu_items are no graphical objects.  Their visual appearance and
user-interface is entirely determined by the menu.

**Bugs**:

The <->end_group facility is at te wrong place: it harms reusability
(which is not used very often anyway) and it makes changes to menu's
difficult.  It might be wiser to introduce a `menu_item_group` object to
bundle menu items.

@see class popup
@see class menu_bar
@see class menu


## Instance variables {#class-menu_item-instvars}

- menu_item<->accelerator: [name]*
    Used by `menu->key`,  which will make the menu behave as if
    this menu_item was selected if the argument key matches this
    slot.

- menu_item<-active: bool
    When @on, the menu_item may be selected by the user.  When  @off, the
    menu_item is insensitive to events.  Normally the menu will indicate
    this by greying the menu_item.

    **Defaults**: @on (active to events)

    @see menu->off
    @see menu_item-condition
    @see menu->active_all_items

- menu_item<-colour: [colour]
    Colour used to paint the label of this menu_item.  Normally used only
    to create colour palets.

    **Defaults**:
    @default (its default value) will paint the menu_item in the colour of
    its associated <-menu.

    @see menu_item-label
    @see menu_item-font

- menu_item<->condition: code*
    Applicable to popup-menu's only.  When not @nil, the truth value of this
    code object is tested just before the popup menu is mapped on the screen
    and the item is made <-active according the this truth value.

    Tested by `popup ->update`.

    **Defaults**: @nil (`popup->update` does not change activation status).

    @see menu->off
    @see popup->update
    @see menu_item-active
    @see popup-update_message

- menu_item<-end_group: bool
    Popup-menus only.  When @on, a line is painted between this item and the
    next.

    **Defaults**: @off (no line is painted).

- menu_item<-font: [font]
    Font used to paint the <-label.  Normally used to realise font-selection
    menus.

    **Defaults**: When @default, `menu <-value_font` of the associated <-menu is used.

    @see menu-value_font
    @see menu_item-label
    @see menu_item-colour

- menu_item<-label: [name|image]
    When a name, the menu will display this text in the <-colour and <-font
    specified with this item.  When is is an image, the menu will display
    this image in <-colour.

    The initial value is normally computed by <-default_label from the
    <-value by ->initialise.

    @see menu_item-value
    @see menu_item->initialise
    @see menu_item<-default_label
    @see menu_item-colour
    @see menu_item-font

- menu_item<-menu: menu*
    Menu I'm part of.  Used to forward changes to the associated menu which
    is responsible for the visual representation and event-processing for
    this menu_item.

    The menu maintains a list of menu_items manipulated in `menu <-members`.

    @see menu-members

- menu_item<->message: [code]*
    Message executed when the item is selected.  When @nil, no message will
    be sent.  When @default, the <-message from the associated <-menu will
    be sent.

    The arguments forwarded depend on the menu.  See description with class
    menu.

    @see class menu

- menu_item<-popup: popup*
    Popup menu's only.  When not @nil, this is the pullright menu associated
    with this item.  The <-convert method transforms popup objects in
    menu_items with this popup associated, which allows appending popup
    menu's to popup menus to create pullright menus.

    **Defaults**: @nil

    @see menu_item<-convert
    @see class popup

- menu_item<-selected: bool
    Describes whether or not the menu_item is currently selected.  Whether
    multiple menu_items may be selected at the same time is determined by
    `menu <->multiple_selection`.  The visual feedback is determined by
    `menu <->feedback`.

    @see menu->selection
    @see menu-feedback
    @see menu-multiple_selection

- menu_item<-value: any
    Value associated with this item.  Menu items have both a value and a
    label.  The latter is displayed while the value is used for internal
    communication in the program.

    This separation is useful for two purposes.  First of all menu's are
    often used to select some value.  For example a colour in a
    drawing tool.  In this case the colour object is used as a value,
    while an all-1's image with is used as label and <->colour is set to
    <->value.  When the user selects a menu-item, the colour object to be
    used may be read directly from the <->value object of the menu_item.

    Second, this mechanism may be used to implement multi-language
    interfaces.  In this case the <->value represents the internal name used
    by the program and the <->label the external name that may be in a
    different language.

    The default <->label is compute from the initial <->value using
    `menu_item <-default_label`.

    @see menu_item<-default_label
    @see menu_item-label


## Send methods {#class-menu_item-send}

- menu_item->font: [font]
    @see menu-value_font

- menu_item->initialise: value=any, message=[code]*, label=[name|image], end_group=[bool], condition=[code]*, accelerator=[name]*
    Create a menu_item from

    1. <->value (any)
    	This argument is the value associated with this item.

    2. <->message [code]*
    	Message sent when item is activated.  @default uses the
    	`menu <->message`.

    3. <->label [name|image]
    	Visual appearance: text or image.  When @default, this is
    	computed using `menu_item <-default_label` from the value.

    4. <->end_group [bool]
    	Popup menu's only.  When @on, a line is drawn between this
    	item and the next.

    5. <->condition [code]*
    	Popup menu's only.  Condition to determine <->active status.
    	When @nil (default), this is not evaluated when the popup is
    	mapped on the screen.

    @see menu_item<-convert
    @see menu_item<-default_label
    @see menu_item-label

- menu_item->off
    Equivalent to `menu_item ->active: @off`.  Backward compatibility.

    @see menu_item->on

- menu_item->on
    Equivalent to `menu_item ->active: @on`.  Backward compatibility.

    @see menu_item->off

- menu_item->popup: popup*
    Associated popup (pull-right).

- menu_item->value: value=any, label=[name|image]
    Set the -value and recompute the label similar to ->initialise.


## Get methods {#class-menu_item-get}

- menu_item<-contained_in: -> menu
    Return the menu in which this menu_item resides.

- menu_item<-convert: value=any -> menu_item
    When argument is an instance of class popup, create a menu_item from the
    `popup <-name` and relate the popup with this menu_item using
    (`menu_item ->popup` and `popup ->context`).  This conversion
    allows simple specification of pull-right menus:

    	new(P, popup(attributes)),
    	new(C, popup(colour)),
    	new(F, popup(font)),
    	send_list(C, append, [...]),	% fill sub-popups here
    	send_list(F, append, [...]),
    	send_list(P, append, [C, F]).	% associate as pullright

    When the argument is not a popup, <-convert will create a menu-item
    using the argument as a value and leaving all other ->initialise
    parameters @default.

    @see menu_item->initialise
    @see menu_item-popup
    @see menu->append

- menu_item<-default_label: value=any -> label=name|image
    Computes the default label from the value.  This method is normally
    called by `menu_item <-initialise` when the label argument is @default.

    It performs the following steps:

    - If the argument is a graphical object, create an image
    	with the size of the graphical and call `image ->draw_in` to
    	draw the graphical in the image.

    - If the argument is a name, all `char_array <-label_name`
            to map the word-separator (default '_', see `pce ->syntax`)
    	to a space and capitalise all the words.

    - If argument <-name yields a name, call
    	`char_array <-label_name` on it and return the result.

    - Call PCE's pretty-print to print the object-reference and
    	class.  The latter is an emergency to simplify debugging.

    To realise multi-language support, one might wish to redefine this
    method yielding labels in the user's preferred language from internal
    program names.

    @see char_array<-label_name
    @see menu_item->initialise
    @see menu_item-value
    @see menu_item-label

