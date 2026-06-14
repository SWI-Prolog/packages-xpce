# class connection {#class-connection}

Class connection is a subclass of class line.  A connection is a line
that connects two graphical objects and is automatically updated if
either of them changes.  A connection is the instantiation of a link.  The
link object describes the generic properties of the line: what the line
looks like and handle objects it can connect too.

A connection may be created between any two graphical object displayed
on the same window object.   The connected graphicals may be arbitrary
deeply nested on device objects.  The connection itself is displayed in
the deepest nested device that displays both connected graphicals.

See also class link, class handle, `graphical ->connect`, `graphical
<-connected' and class connect_gesture.

The drawing program PceDraw exploits connections.

@see graphical<-common_device
@see topic Connections
@see graphical->connect
@see graphical->handle
@see class connect_gesture
@see class line
@see class constraint
@see class link
@see class handle


## Instance variables {#class-connection-instvars}

- connection<-fixed_from: bool
    The slots <-fixed_from and <-fixed_to are used internally to represent
     that the handles the connection is related to (<-from_handle and
     <-to_handle) should not be reconsidered if the connection is updated.
     These values are normally @off and set to @on by ->initialise if the
     corresponding handle is specified.

- connection<-fixed_to: bool
    *Inherits description from*: connection-fixed_from

- connection<-from: graphical*
    Related graphical and the indicated end of the connection.  See also
    `graphical <-connections`, `graphical <-connected`, `connection
    <-opposite'.

- connection<-from_handle: name*
    Name of the handle used at the indicated end.  Depending on
    <-fixed_from/fixed_to, this handle is or is not updated if either of the
    connected graphicals is moved or changed otherwise.

- connection<-to: graphical*
    *Inherits description from*: connection-from

- connection<-to_handle: name*
    *Inherits description from*: connection-from_handle


## Send methods {#class-connection-send}

- connection->compute
    Performs the following operations:

    - if -for either graphical-, <-is_displayed yields @off,
    	undisplay the connection.

    - Recompute <-from_handle and <-to_handle if these are
    	not fixed

    - Recompute the positions of these handles and update the
    	line using ->points if necessary.

- connection->geometry: x=[int], y=[int], width=[int], height=[int]
    No-op.  The coordinates of a connection are determined by the graphical
    objects related.  See also ->points.

- connection->initialise: from=graphical, to=graphical, link=[link], handle_from=[name]*, handle_to=[name]*
    Create a connection object (automatically updating line) between
    the two graphicals.  The first becomes `connection <-from`; the
    second `connection <-to`.  _Link_ is a link object describing
    the handles and characteristics of the line (<-pen, <-arrows,
    ...).  The two name arguments define the name of the handle to
    connect to at the <-from resp.  <-to side.  When omitted and
    there are multiple matching handles PCE will connect to the
    `best-looking` one.

    Always succeeds, also if the line cannot (yet) been created because
    there is no common device to display the graphical on.

    @see graphical->connect

- connection->points: start_x=[int], start_y=[int], end_x=[int], end_y=[int]
    Define the start and end-point of the connection.  This method is called
    from ->compute.  ->points is the only method to change a connection as
    all other methods are discarded by a redefined ->geometry method.

    ->points may be used to manage a label for the connection.

- connection->update_link_attributes
    Re-read <-arrows, <-texture and <-pen from the <-link and update
    accordingly.


## Get methods {#class-connection-get}

- connection<-opposite: graphical -> graphical
    If the argument graphical is either <-from or <-to, return the other
    end.  Fail silently otherwise.

