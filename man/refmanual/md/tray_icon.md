# class tray_icon {#class-tray-icon}

A tray_icon shows an icon in the system tray, also known as the
notification area, e.g., for an application that runs in the
background.  The icon can have a tooltip and a menu, which is built
from a popup.  Selecting an entry of the menu executes the popup as if
the item was selected in the popup, with the tray_icon as context:

	new(P, popup(tray)),
	send_list(P, append,
	          [ menu_item(show, message(Frame, expose)),
	            menu_item(quit, message(@prolog, halt))
	          ]),
	new(T, tray_icon(image('app.png'), 'My application', P)).

Destroying the tray_icon (`object->free`) removes the icon.  Items of
a popup with `menu->multiple_selection` @on are shown with a check
mark, inactive items are disabled, an item with a `menu_item<-popup`
becomes a submenu and `menu_item<-end_group` adds a separator.  The
menu is built when the popup is set.  After changing the popup, use
->update.

On Linux, the system tray requires the AppIndicator library
(libayatana-appindicator) and a desktop that shows such icons.

@see class popup


## Instance variables {#class-tray-icon-instvars}

- tray_icon<->image: image
    The icon.

- tray_icon<->tooltip: char_array*
    Text shown when the pointer is on the icon.

- tray_icon<->popup: popup*
    The menu.


## Send methods {#class-tray-icon-send}

- tray_icon->initialise: image=image, tooltip=[char_array]*, popup=[popup]*
    Show `image` in the tray.  Fails with an error if the system has
    no tray.

- tray_icon->update
    Rebuild the menu from <-popup after the popup changed.

- tray_icon->sdl_test_click: label=char_array
    For testing only.  Act as if the user selected the menu entry with
    this label.  Fails if there is no such entry.
