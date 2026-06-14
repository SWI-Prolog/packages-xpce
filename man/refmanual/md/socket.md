# class socket {#class-socket}

Class socket provides an interface to Unix sockets.  A Unix socket is a
(network) communication end-point.  Normally one process creates a
socket at a specified address (see below).  Other processes may use a
socket to ->connect to the created socket.  After the connection has
been established, a two-way text-based communication channel is
available.

Below is a first, very simple example:

	?- new(X, socket('~/xpce-socket')),
	   send(X, input_message, message(@pce, write, @arg1)),
	   send(X, listen).

In another xpce process running on the same machine we can now do

	?- new(X, socket('~/xpce-socket')),
	   send(X, connect),
	   send(X, format, 'Hello There\n'),
	   send(X, close).

Sockets addresses may be internet addresses as (allowing for
communication between machines) and unix-domain addresses, allowing for
communication between processes running on the same machine.  See also
->initialise.

We distinguish between `server` sockets and `client` sockets.  After the
connection has been established there is no difference between the two.
Below we describe the process to establish a connection:

1. Create a server socket.  The address is either a file or a
	port number.  See ->initialise.  The ->input_message is
	called whenever there is input on the socket; the
	->accept_message is called whenever a client makes an
	attempt to ->connect to this socket.

2. Invoke ->listen.  This will make the socket listen to
	requests from other processes.

3. (In the other process) Create a client socket.  The
	address is either the same file as created by the server
	or a tuple consisting of an internet node name and a
	port number.

4. Invoke ->connect.  Provided the address is right this
	will cause the server socket to start ->accept.
	->accept will <-clone the server socket and run the
	->accept_message with @receiver bound the the server
	socket and @arg1 bound to the clone.  After this, a
	communication channel exists between the cloned server
	and the client socket.  Data is sent to the socket using
	->append or ->format.  Whenever data is available,
	->input_message is called to handle this data.  Class
	process handles input from its inferior process using
	the same mechanism.

The Unix program xpce-client(l) may be used to connect to
xpce-processes.  See also telnet(1).  The sources of xpce-client may be
found in xpce/src/unx-client.c.  They may be used as a starting point
for other unix programs that wish to communicate to xpce.

See also class stream.

The library file pce_server defines pce_server/1 to create a `server`
mode.


## Instance variables {#class-socket-instvars}

- socket<->accept_message: code*
    This message is executed after a server socket has accepted a
    new connection.  It receives the following arguments:

    	| @receiver | The original socket                          |
    	| @arg1     | The <-clone'd socket for the new connection. |

    This message may be used to deny the client access by ->free'ing
    the new socket (@arg2).

    See also ->input_message.

- socket<-address: file|tuple|int*
    Address for the connection end-point.  If the socket was created
    as a server socket on inet address _0_ (this machine, port
    number 0), the ->listen call will replace the <-address with
    the actually used port number.

- socket<-input_message: code*
    Whenever there is input available, this message will be called with

    	@receiver = the socket
    	@arg1 = a new string object holding the data

    See also <-read_line.

    **Defaults**:
    The default input_message is @nil.  In this case data may only be read
    using <-read_line.

- socket<-status: {idle,listen,accepted,connected}
    Status of the associated socket.  The initial <-status of a
    socket is `idle`.  A server socket waiting for connections
    has status `listen`.  An cloned server socket handling
    a connection has status `accepted` and finally, an open
    client socket has <-status `connected`.

    See also ->listen, and ->connect.


## Send methods {#class-socket-send}

- socket->connect
    Connect with server socket.  First, the socket must be created
    with the appropriate address.  To connect a client to a
    Unix-domain socket, use:

    	new(S, socket(File)),
    	send(S, connect),
    	...

    To connect to an inet domain socket, use:

    	new(S, socket(tuple(Host, Port))),
    	send(S, connect),
    	...

    ->connect is a no-op if <-status is already `connected`.

- socket->end_of_file
    Sent by the system when end-of-file is reached on the input.
    If the socket has <-status: accepted (i.e.  it is a clone of a
    server socket handling a connection), the default is to ->free
    the socket object.

- socket->initialise: address=file|tuple|int*, domain=[{unix,inet}]
    Create a server or client socket.  The address argument is either a file
    object for Unix domain sockets or a port-number of inet-domain
    (TCP/IP) sockets.  For inet-domain server sockets the address
    argument is the port-number or 0 (zero) to select an arbitrary
    free socket.  The selected port can be requested using
    <-address.

    Unix-domain sockets appear as an entry in the filesystem and are
    only supported on Unix systems.

    After a socket has been created and the appropriate attributes (notably
    ->input_message) have been specified the socket is activated using
    ->listen for server sockets and ->connect for client sockets.

- socket->listen: accept_message=[code]*, backlog=[{1..5}], reuse=[bool]
    Prepare the socket to listen for connections.  The argument replaces the
    ->accept_message when specified.  The second argument is the `backlog`.
    It specifies how many pending requests for connections are tolerated.
    Any new connect request will be denied.  The default is 5.

    If the `reuse` argument is @on, the system tries to reuse the
    specified port, dispite it still being in use.  This option is
    needed to restart a service at the same -well known- port after
    an dirty shut-down.   If you are security minded, check the
    internet on setsockopt() for the option SO_REUSEADDR before
    activating this option.

    If an inet domain socket was created with its port specified as
    0, the system will select an available port.  The actually used
    port can be requested from the socket object using <-address
    after this method completed successfully.


## Get methods {#class-socket-get}

- socket<-peer_name: -> name|tuple
    Returns the address of the socket at the other side of the
    connection.  The socket must be connected, which implies
    it has executed a successful ->connect (client) or is a cloned
    socket from a socket in ->listen mode (server, see <-clients
    and ->accept_message).

    If the socket is in the Unix <-domain, the return value is a
    name describing the file (which will be the same as the absolute
    path-name of the file used to create the socket).

    If the socket is in the inet <-domain, the return value is a
    tuple object holding a name describing the internet address of
    the socket at the other end in decimal numeric notation and an
    integer describing the port.
