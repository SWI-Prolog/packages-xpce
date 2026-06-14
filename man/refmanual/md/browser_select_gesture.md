# class browser_select_gesture {#class-browser_select_gesture}

From this gesture class, normally one instance is created called
@browser_select_gesture.  This gesture is called from
`list_browser->event` to deal with modifying the selection of
the browser object.

Unless the aim is to redefine low-level look and feel of the
system, the application programmer does not have to worry about
this class.


## Instance variables {#class-browser_select_gesture-instvars}

- browser_select_gesture-scrolling: bool
    If @on, redirect events to the scroll_bar.  This is required to
    make this gesture work properly for selecting from a combo-box
    (menu of `menu<-kind: cycle` in windows and text_item with
    a defined `text_item->value_set`.


## Send methods {#class-browser_select_gesture-send}

- browser_select_gesture->drag: event
    If SHIFT is down and `list_browser<-multiple_selection` is @on,
    the selection is extended.  Otherwise the selection is modified
    to reflect the item at the pointer location.

- browser_select_gesture->initiate: event
    This method saves the selection of the list_browser object.  If
    the button is released outside the browser, the saved value is
    used to restore the selection.  See also ->terminate.

- browser_select_gesture->terminate: event
    If this terminates a normal click, `list_browser<-select_message`
    is executed.  If it terminates a double-click, `list_browser<-open_message`
    is executed.

    If the gesture is terminated outside the browser, `list_browser->change_selection:
    cancel' is ivoked using the <-saved_selection as argument.  See
    also `list_browser->cancel_message`.

