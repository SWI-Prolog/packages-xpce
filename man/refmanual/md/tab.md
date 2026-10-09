# class tab {#class-tab}

A tab object is a refinement of a dialog_group object, which
implies it is a device specialised for displaying dialog_item
objects.

A tab shows its name as a label above a flat rounded box with its
contents.  Tabs are designed to
cooperate in a tab_stack object, to achieve a `stack` of
sheets holding controllers, which allows the user to
switch by clicking the desired tab.

Tabs are normally used to establish settings for a system
that has many settings.  Groups of related settings will be
placed on a separate tab.

Each label is a pill: a box with a half circle at each end.  The label
of the tab on top has an edge in the accent colour.  The labels of the
hidden tabs have their background moved a little towards the text
colour and dimmed text.  This works for light and dark themes alike.
The box around the contents is drawn in the class variable
`border_colour`.

See also class tab_stack.


## Class variables {#class-tab-classvars}

- tab.indicator_colour: colour* = ui_accent
    Colour of the edge of the label of the tab on top.  @nil omits
    the edge.

- tab.indicator_width: 0.. = 2
    Width of this edge.  0 omits the edge.


## Instance variables {#class-tab-instvars}

- tab<->icon: image*
    Image shown left of the text of the label, scaled to the height
    of the text.  It sits in the half circle at the left end of the
    label, as a close button (see <-closable) sits in the one at the
    right end.  It is dimmed with the label of a hidden tab.  Default
    @nil.

- tab<-label_format: {left,center,right}
    Deternines how the label is aligned the box.

- tab<-label_offset: int
    Distance from the left-side of the object to the left-side of
    the label-box.  Normally set by `tab_stack ->layout_labels`.

- tab<-label_size: size
    Size of the label-box.  If the text does not fit the size of the
    box, it will automatically be enlarged.

- tab<-status: {on_top,hidden}
    A tab object can be in <-status: on_top, in which case it will
    display both the label and the contents.  Otherwise it will only
    display the label.


## Send methods {#class-tab-send}

- tab->event: event
    If the event occurred in the label-area of the tab, invoke
    ->label_event.  Otherwise if the tab is top-most (see
    `tab_stack->on_top` and <-status) process the event on
    the content of the tab.  See `dialog_group->event`.

- tab->flash: area=[area], time=[num]
    If the <-status is `on_top`, the drawing area of the tab is
    inverted, otherwise the tag is inverted.  See also `graphical ->flash`.

- tab->initialise: name=[name]
    Create a tab object with the given name.  A tab is a subclass
    of class dialog_group and allows for displaying dialog_item
    objects.   A tab must be added to a `stack` using the method
    `tab_stack->append` or using `tab_stack->initialise`.

- tab->label_event: event
    Process event object that occurred on label.  Default is to make
    the receiving tab the topmost (see `tab_stack->on_top`) on
    receiving an ms_left_down event.   This method may be refined
    -for example- to attach a popup object to the tab.  See also
    ->event and `graphical->event`.

- tab->x: int
    Left-side of tab.

- tab->y: int
    Top-side of tab.

- tab->position: point
    Determines the top-left corner of the tab, while the origin of
    the tab, regarded as a device object is the top-level of the
    rectangle used for displaying the contents.

## Get methods {#class-tab-get}

- tab<-label_area: area
    The box (pill) of the label, relative to the area of the tab.
    Fails if the label is not shown.

- tab<-position: -> point
    *Inherits description from*: tab->position

- tab<-x: -> int
    Determines the top-left corner of the tab, while the origin of
    the tab, regarded as a device object is the top-level of the
    rectangle used for displaying the contents.

    *Inherits description from*: tab->position

- tab<-y: -> int
    Determines the top-left corner of the tab, while the origin of
    the tab, regarded as a device object is the top-level of the
    rectangle used for displaying the contents.

    *Inherits description from*: tab->position

