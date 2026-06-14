# class browser {#class-browser}

A browser is a window-based version of a class list_browser.  It is
implemented as a window object that displays a single list_browser
object and resizes this object if the window is resized.  A browser
delegates to its list_browser.

Many of the methods of class browser direct methods defined for both
class window and class list_browser to the list_browser.  Inheritance is
considered before delegation in PCE's message passing algorithm.

@see class list_browser
@see class dict


## Send methods {#class-browser-send}

- browser->clear
    Remove all dict_item objects from the associated dict object.

- browser->cursor: cursor*
    Defines the cursor for the item-area of the list_browser `text_image
    <->cursor'.

- browser->initialise: label=[name], size=[size], display=[display]
    Create a browser from its frame-label, size and display.  Unlink for
    the super-class window, the size is measured in character units.

- browser->normalise: member:dict_item
    Scrolls the view such that the indicated dict_item will become visible.
    This method may also be passed a `dict_item<-key` name, which will
    be translated to the corresponding <-member dict_item automatically.

- browser->popup: popup*
    *Inherits description from*: browser<-popup

- browser->scroll_to: int
    Scroll to the numbered dict_item object.  The first item is numbered 0.

- browser->selection: member:dict_item|chain*
    Set selected items.

- browser->selected: member:dict_item
    Overrule inherited window selection mechanism.  See
    `list_browser->selected` for the the definition.

## Get methods {#class-browser-get}

- browser<-contains: -> chain
    Hides the existence of the list_browser by returning
    `list_browser<-contains`.

- browser<-member: any -> dict_item
    Return `dict<-member` of the (through the list_browser) associated dict
    object rather than the named graphical.

- browser<-popup: -> popup*
    Access `list_browser <->popup` rather than `window <->popup`.

- browser<-selection: -> chain|dict_item*
    *Inherits description from*: browser->selected

- browser<-size: -> characters=size
    Return `list_browser<-size` of the associated <-list_browser.  This is
    the size of the list_browser is character units.

