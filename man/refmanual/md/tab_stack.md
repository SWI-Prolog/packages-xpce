# class tab_stack {#class-tab_stack}

A tab_stack object controls a set of tab objects, sheets holding
controllers.  Tab-stacks are normally used to make dialog
windows with large numbers of options manageble.

The code below is a partial implementation of a dialog to
specify a PC:

	new(D, dialog('PC Specifiation sheet')),
	send(D, gap, size(0,0)),
	send(D, pen, 0),
	send(D, append,
		 tab_stack(new(CPU, tab(cpu)),
			       new(Disk, tab(disk)),
				   new(Monitor, tab(monitor)),
				   ...)),
	send(CPU, append,
		 new(CPUT, menu(cpu, choice))),
	send_list(CPUT, append, [386, 486, pentium]),
	...

	send(D, open).


## Class variables {#class-tab_stack-classvars}

- tab_stack.max_label_width: 0.. = 0
    The widest a label may be.  A longer label is shortened; see
    `tab <-ellipsize_label`.  0 puts no limit on it.


## Instance variables {#class-tab_stack-instvars}

- tab_stack<->label_room: 0..*
    The width the row of labels must fit in, or @nil (default) for no
    limit.  If the labels are wider, the long ones are shortened: the
    labels narrower than their share are shown whole and the others
    share what is left.  It is not the width of the tabs: in a dialog a
    tab is as wide as its label.  Whoever gives the stack its width
    sets it, as library(tabbed_window) does, keeping room for its
    button to add a tab.


## Send methods {#class-tab_stack-send}

- tab_stack->append: tab
    Append a tab object to the right of the member tabs.  If it is
    the first tab, it will become the one ->on_top.   Tabs may be
    added both before and after displaying a tab_stack.  See
    ->layout_dialog for fixing the layout after adding to a
    displayed tab_stack.

- tab_stack->cycle: direction={next,previous}
    Put the next or previous tab on top, skipping inactive tabs and
    wrapping around at the ends.  A tab_stack does this itself on
    Control-Tab and Control-Page-Down (next) and Control-Shift-Tab
    and Control-Page-Up (previous) if no tab takes the key.

- tab_stack->erase: graphical
    Erases a tab object or graphical from the tab-stack.  Ensures
    another tab is raised if this is the <-on_top tab and recompute
    the layout of the tabs.  See also `device->erase`.

- tab_stack->initialise: member=tab ...
    Create a tab-stack holding the argument members.  The arguments
    are handed to ->append, left to right.

- tab_stack->layout_dialog: [size]
    This method is normally called by the generic dialog layout
    mechanism to fix the layout of its members.  It will invoke
    `tab->layout_dialog: size` to force all member tab objects
    to adjust themselves to the requested size.

    If size is @default, the system will invoke `tab->layout_dialog`
    to all its members and then compute the union of the member tab
    objects to determine the required size.  Next it will call this
    method again with the computed size.

- tab_stack->on_top: member:tab
    Put indicated tab on top of the others.  Normally this is called
    from the `tab->event` method after the user clicks on the label.
    of a hidden tab object.

    Note that the argument may be the name of the tab rather than
    the tab itself.


## Get methods {#class-tab_stack-get}

- tab_stack<-on_top: -> tab
    Find tab on top.  Returns the first member tab object with
    `tab <-status: on_top`.

