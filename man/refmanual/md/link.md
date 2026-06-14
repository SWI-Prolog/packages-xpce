# class link {#class-link}

A link object describes the reusable part of a connection object.  A
connection is a line object between two graphicals, attached to
handle objects defined on those graphicals or on their classes.

The `reusable` link contains the `handle<-kind` of the handles
required at the start- and endpoint of the line and a prototype for the
line (with the appropriate texture, pen, arrows, etc).

See also example _Linking Graphicals_, class connection and
class handle.

@see topic Connections
@see class connection
@see class handle


## Instance variables {#class-link-instvars}

- link<->connection_class: [class]
    Class used by <-connection.  This must be a subclass of class
    connection.  If the value is @default, this slot will be set to
    class(connection) on the first call to <-connection.

- link<->from: name
    The connection instantiated from this link may connect to any handle
    that has `handle <-kind` equal to the value of this variable at the
    indicated end.

- link<->to: name
    *Inherits description from*: link-from


## Send methods {#class-link-send}

- link->initialise: handle_kind1=[name], handle_kind2=[name], line=[line], connection_class=[class]
    Define a link which may connect to a handle with `handle <-kind` is
    handle_kind1 at the `connection<-from` end and `handle <-kind` is
    handle_kind2 at the `connection<-to` end.

    The default handle_kind1 is `link`.  The default handle_kind2 is
    handle_kind1.  The default line is an instance of class line
    create with no arguments.

    The connection_class argument determines the class used by
    <-connection to instantiate the link.  The default is class
    connection.


## Get methods {#class-link-get}

- link<-connection: from=graphical, to=graphical, from_handle=[name], to_handle=[name] -> connection
    Creates an instance of the link.  An instance of a link object
    is a connection object.  This method is normally called from
    `graphical ->connect`.

    The default implementation of this method creates an instance
    of class <-connection_class using the arguments passed to
    this method.  See `connection ->initialise`.

