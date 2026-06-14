# class dialog_group {#class-dialog_group}

A dialog_group object is a compound dialog item, a number of
dialog_item objects displayed on a specialised device object.

Class dialog_group specialises class device to ensure proper
handling of the layout of the controls displayed inside it as
well as the placement of the group in the dialog.  The class
is also responsible for proper handling of keyboard input,
including the <-default_button.

A dialog_group is built exactly the same as a dialog object.
Here is a small example:

	...,
	new(DG, dialog_group(location)),
	send(DG, append, text_item(address)),
	send(DG, append, text_item(city)),
	send(DG, append, text_item(country)),
	...


## Instance variables {#class-dialog_group-instvars}

- dialog_group<->alignment: {column,left,center,right}
    Alignment of the dialog group in the surrounding dialog window.
    See `dialog_item <-alignment`.

- dialog_group<->auto_align: bool
    Automatically align in dialog (@on).  See `dialog_item
    <-auto_align'.

- dialog_group<-border: [size]
    Space around the items.  The value @default uses the same value
    for the border as for the <-gap between items.  See also
    ->kind.

- dialog_group<-gap: size
    Distance between the dialog_items displayed in the group.  See
    `device ->layout_dialog` for a description of the layout
    algorithm

- dialog_group<-label: [name|image]*
    Text displayed to lable the dialog group.  If the label is the
    empty name object (''), the label is not displayed.  See also
    ->name and <-label_font.

- dialog_group<-label_font: font
    Font used for displaying the <-label.

- dialog_group<-label_format: {top,center,bottom,hot_spot}
    Alignment of the label with the line drawn around the group.
    Its value should normally be controlled by the class-variable to
    ensure consistent appearance of compound dialog groups.

- dialog_group<-radius: 0..
    Radius for the corners of the box surounding the group elements.
    Used to commit to the style.

- dialog_group<-size: [size]
    Size of the contents.  @default implies the size is computed
    from the items displayed in the group.


## Send methods {#class-dialog_group-send}

- dialog_group->append: item=graphical, relative_to_last=[{below,right,next_row}]
    Mimics the behaviour of `dialog ->append`.

- dialog_group->modified_item: item=graphical, modified=bool
    Indicates item has changed state.

- dialog_group->restore
    ->restore all items to their <-default.

- dialog_group->apply: always=[bool]
    These methods are reimplementations of the corresponding methods
    on class dialog.  See `dialog ->apply`, `dialog ->modified_item`
    and `dialog ->restore`.

- dialog_group->event: event
    Maps the obtain_keyboard_focus event to ->advance, and otherwise
    simply invokes `device ->event` to dispatch the event over the
    items.

- dialog_group->geometry: x=[int], y=[int], width=[int], height=[int]
    If either width or height is specified, a size object is
    computed and ->size is invoked.  The group is moved.

- dialog_group->initialise: name=[name], kind=[{box,group}]
    Create an empty dialog group from its name.   The name is used
    to assign the <-name slot and handed to ->name to compute the
    default <-label from it.  Using the standard configuration, the
    <-name is capitalised to produce the <-label.

    The  ->kind determines the appearance of the group.

    See also class label_box.

- dialog_group->kind: kind=[{box,group}]
    Set default appearance.  The default ->kind is `box`.  Its
    values:

    - box
    	Sets <-border to @default, <-label to the <-label_name
    	calculated from the <-name and <-pen to 1.  This results
    	in a (3d-) box with a label containing the items, which
    	is normally used to visually group items for the user.

    - group
    	Sets <-border to size(0,0), <-label to '' and <-pen to 0.
    	This mode is normally used to group dialog items for
    	technical reasons.  One such reason is the definition of
    	a single dialog item for a special type that requires
    	multiple sub-items as a subclass of class dialog_group.
    	The other is to subdivide the items in a dialog window
    	in multiple groups to define layouts that do not fit in
    	the standard matrix layout provided by dialog windows.

- dialog_group->layout_dialog
    Invokes `device ->layout_dialog` using the <-gap and <-size
    attributes.

- dialog_group->name: name
    Assigns the `graphical <-name` and determines the displayed
    <-label.  The label is computed using the <-label_name method.

- dialog_group->position: point
    Determines the top-left corner of the group.

- dialog_group->reference: point
    Overrules the definition of `device->reference` with the one of
    `graphical->reference` as changing the origin of a dialog group
    does not have any practical meaning.

- dialog_group->x: int
    Set the X-coordinate of the left-side of the group.

- dialog_group->y: int
    Set the Y-coordinate of the top-side of the group.


## Get methods {#class-dialog_group-get}

- dialog_group<-default_button: delegate=[bool] -> button
    Current Button connected to _RET_.  If delegate is @off, only
    buttons displayed on the group itself are considered.  Otherwise
    the <-device links are followed, searching for default buttons
    on containers.

- dialog_group<-label_name: name -> name
    Determine default-label from the name using the method `name
    <-label_name'.  The latter by default capitalises the name,
    replacing underscores by spaces.  It may be redefined to map
    labels to a different language.

