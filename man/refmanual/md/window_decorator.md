# class window_decorator {#class-window_decorator}

A window_decorator object is used to decorate a window with
scroll_bar objects and/or a label.  Like frame objects,
window_decorators are normally kept invisible to the application
programmer.  Class window delegates to its associated
`window <-decoration`.

Class window_decorator may be refined to modify PCE's window
look-and-feel.


## Instance variables {#class-window_decorator-instvars}

- window_decorator<-horizontal_scrollbar: scroll_bar*
    These variables point to the associated scrollbars.  The send
    methods take a boolean argument and attach or detach a
    scrollbar.  Normally called through ->scrollbars.

- window_decorator<-label_text: text*
    *Inherits description from*: window_decorator->label

- window_decorator<-vertical_scrollbar: scroll_bar*
    *Inherits description from*: window_decorator-horizontal_scrollbar

- window_decorator<-window: window
    This variable describes the decorated window.


## Send methods {#class-window_decorator-send}

- window_decorator->displayed: bool
    Refines `graphical ->displayed`, ensuring that both the
    decorated window and the decorator have consistent
    <-displayed.

- window_decorator->geometry: x=[int], y=[int], width=[int], height=[int]
    Calls `window ->geometry`, followed by ->rearrange to update
    the layout.

- window_decorator->horizontal_scrollbar: bool
    *Inherits description from*: window_decorator-horizontal_scrollbar

- window_decorator->initialise: window=window, scrollbars=[{none,vertical,horizontal,both}], label=[char_array]
    Create a decoration for the given window.  The optional
    arguments specify the scrollbars and label of the window.

    Window decorators are normally created by PCE itself by
    invoking ->scrollbars and/or label to the window to be
    decorated.  See also `window ->catch_all`, `window ->decorate`
    and `window <-decoration`.

- window_decorator->label: format=char_array*, argument=any ...
    Define a window-level (opposed to `frame <->label`) label for
    the window.  The label is visualised by <-label_text using the
    font Pce.WindowDecorator.label_font.

    <-label returns the `text <-string` of the <-label_text or
    fails if <-label_text yields @nil.

- window_decorator->rearrange
    Rearrange <-window, <-scrollbars and <-label_text.  The default
    method places the label_text at the top-left corner and the
    scrollbars according to the scroll_bar.placement.

    This method is normally called by ->geometry and may be
    redefined to achieve different window layouts.

- window_decorator->request_geometry: x=[int], y=[int], width=[int], height=[int]
    Handle a geometry request.  It will enlarge the requested values
    by the space needed for the decorations and call ->geometry with
    the enlarged size.

- window_decorator->scrollbars: {none,horizontal,vertical,both}
    Translates the requested scrollbars in calls to
    ->horizontal_scrollbar and ->vertical_scrollbar.

- window_decorator->unlink
    Make sure the <-window is destroyed too.

- window_decorator->vertical_scrollbar: bool
    Associate or remove the vertical scroll_bar object of this
    window.

    *Inherits description from*: window_decorator-horizontal_scrollbar


## Get methods {#class-window_decorator-get}

- window_decorator<-label: -> char_array
    *Inherits description from*: window_decorator->label

- window_decorator<-scrollbars: -> {none,horizontal,vertical,both}
    *Inherits description from*: window_decorator->scrollbars

