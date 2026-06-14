# class layout_interface {#class-layout_interface}

Abstract interface between a graphical and its `layout_manager`.
A `layout_interface` holds the per-element geometry hints (row/column
position, span, stretch/shrink, alignment, …) that a layout_manager
needs to place the element.  Graphicals inside a managed device
acquire a layout_interface on demand; users normally interact with a
concrete subclass (for example a `tab_cell`) rather than constructing
a layout_interface directly.

@see class layout_manager
@see class graphical
@see class device


## Instance variables {#class-layout_interface-instvars}

- layout_interface<-layout_manager: layout_manager*
    The manager responsible for placing `<-image`, or @nil if the
    interface is not currently attached.

- layout_interface<-image: graphical
    The graphical whose layout this interface controls.


## Send methods {#class-layout_interface-send}

- layout_interface->initialise: graphical
    Bind this abstract interface to the given graphical.

- layout_interface->unlink
    Detach the interface from `<-image`, e.g. when the graphical is
    destroyed.


## Get methods {#class-layout_interface-get}

- layout_interface<-convert: graphical -> layout_interface
    Type-check helper: yield the layout_interface associated with a
    graphical, creating one on the fly when needed.
