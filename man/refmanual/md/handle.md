# class handle {#class-handle}

A handle object describes a *possible* end-point for a connection object
between two graphical objects.  A handle consists of two expression
objects in `w` and `h` of the graphical that determine the X and Y
positions of the handle.

The <-kind of handle determines which connections may attach to this
handle.  It's value should match `link <-from` or `link<-to`, depending
on which end of the connection is considered.

Finally, a handle also has a name to identify it.  Note that the total
set of handles associated with any graphical object is the merge of
`graphical <-handles` and `class <-handles` of the graphicals' class.

See also example _Linking Graphicals_, class connection, class
link, class connect_gesture and `graphical ->connect`.

**Bugs**: There are no possibilities to visualise the presence of handles.

@see graphical->handle
@see tree-parent_handle
@see graphical-handles
@see class-handles
@see class link
@see class graphical
@see class connection
@see class expression


## Instance variables {#class-handle-instvars}

- handle<->kind: name
    The connection object of a link object may attach to this handle if the
    `link<-from` or `link<-to` (depending on the side of the connection
    concerned) matches this name.

- handle<->name: name
    This slot determines the logical name of the handle.  It's value is used
    to communicate about handles.

- handle<->x_position: expression
    This slot determines the position of the handle relative to the
    graphical.  Its value is an expression expressed in `w` and `h`
    of the graphical object the handle belongs too.  Legal expressions
    are:

    - integer, number object or real object

    - a function object
    	Actually, `w` and `h` are converted to var objects with this
    	name.

    - a binary_expression object

    Below are some commonly used examples:

    	| 30   | 30 pixels                               |
    	| w/2  | half of the `graphical <-width`.        |
    	| h+10 | ten more than the `graphical <-height`. |

- handle<->y_position: expression
    *Inherits description from*: handle-x_position


## Send methods {#class-handle-send}

- handle->initialise: x=expression, y=expression, kind=[name], name=[name]
    The first two arguments are expressions representing the X- and
    Y-coordinates expressed in the `w` and `h` variable of the area they are
    attached to.  The first name is the `Handle <->kind` attribute which
    determines the category the handle belongs to.  The second name is the
    `Handle <->name` attribute that determines the logical name of the
    handle.

    **Defaults**: The default `kind` is `link`.  The  `name` defaults to the `kind`.


## Get methods {#class-handle-get}

- handle<-x: relative_to=graphical, coordinate_system_of=[device] -> int

- handle<-y: relative_to=graphical, coordinate_system_of=[device] -> int

- handle<-position: relative_to=graphical, coordinate_system_of=[device] -> point
    Compute the location of the handle relative to the indicated graphical
    and expressed in the coordinate system of the given device object.  If
    no device is specified, `graphical <-device` is used.
