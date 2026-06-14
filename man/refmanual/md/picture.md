# class picture {#class-picture}

A picture is a basic window for displaying arbitrary graphical objects.
A picture by default has scrollbars for horizontal and vertical
scrolling attached.  This is its only difference from a basic window.

@see class window


## Send methods {#class-picture-send}

- picture->initialise: label=[name], size=[size], display=[display]
    Create a picture window.  A picture is a window with scrollbars
    attached.  Invokes `window ->initialise`, followed by `window
    ->scrollbars: both'.

