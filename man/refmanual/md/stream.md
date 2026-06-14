# class stream {#class-stream}

Class stream is a super-class for the class process and
class socket and provides methods for sending data to child
processes and over sockets as well as handling (asynchronous)
input from these objects.  The most commonly used functionality
of this class is:

- ->format:
	Format text and send it to the stream

- ->input_message:
	Handle input record

- ->record_separator:
	Separate input in records (regex object)


## Instance variables {#class-stream-instvars}

- stream-input_allocated: alien:int
    Allocated size of input_buffer.

- stream-input_p: alien:int
    Number of characters in input_buffer.

- stream-input_buffer: alien:char *
    Buffer to collect data from the stream into records.  See
    <-record_separator and <-input_message.

- stream<-input_message: code*
    Each time a complete input record (see <-record_separator) is
    available, this code is executed with the following bindings:

    	| @receiver: | the stream object                |
    	| @arg1:     | a string object holding the data |

    Note: when using a socket object, the receiver of the ->input_message
    must be @receiver as the actual connections are running on a
    <-clone'ed socket.

- stream-rdstream: alien:FILE *
    Stream used for <-read_line.

- stream-wrfd: alien:int
    File-handle to write to stream.

- stream-rdfd: alien:int
    Operating-system file-handles.  -rdstream is a buffered handle
    using with <-read_line.

- stream<-record_separator: regex|int*
    Regex that describes the record separator.  When not @nil, the
    system will collect data from the stream until a complete
    record is available before calling the <-input_message.  If the
    data arriving from the stream contains multiple records, the
    <-input_message will be called with each record separately.

    The default record separator is '\n' (newline), which implies
    that input is split into physical lines.

    If <-record_separator is an integer, the input is split into
    records of the indicated size.  This is useful for receiving
    binary data that is organised in records.

    If you just want to collect all data from the process, it is
    advised to set the <-record_separator to @nil, disabling the
    input filtering.

    The ->record_separator may be changed on an open connection.
    Changing the record separator will immediately scan the data
    in the input buffer and dispatch any complete record according
    to the new definition.  Setting the separator to @nil will run
    the <-input_message using the data in the buffer and destroy
    the buffer.

- stream-ws_ref: alien:WsRef
    Window system synchronisation handle.


## Send methods {#class-stream-send}

- stream->append: data=char_array
    Send data to stream.  The current implementation flushes the
    data automatically.  Future releases may provide input
    buffering.  See also ->newline, ->append_line and ->format.

- stream->close_output
    Close output section of stream.

- stream->close_input
    Closes the associated file-descriptor -rdfd/-wrfd and
    deallocates the input buffering.

- stream->end_of_file
    Sent by the system when end-of-file is reached on the input.
    The default implementation just succeeds.  May be redefined.

- stream->initialise: rfd=[int], wfd=[int], input_message=[code], record_separator=[regex|int]
    Creates a stream object from the following arguments:

    - rfd [int]
    	C-file-descriptor number for input.  When left @default,
    	this will be @nil, indicating input should not be
    	monitored.

    - wfd [int]
    	C-file-descriptor number for output.  When left
    	@default this will be @nil, indicating the stream cannot
    	be written to.

    - input_message [code object]
    	Code object for handling input.  See <-input_message.

    - record_separator [regex object]
    	Regular expression to split input stream into logical
    	records.  The default is '\n', splitting the input into
    	physical lines.  See <-record_separator.

    Class stream is designed to share code between class socket and
    class process.  It may be used for other interaction, but all
    such application of this class is as yet unsupported.

- stream->input: fd=[int]*
    Enable input from file-descriptor.  Assigns -rdfd and
    initialises input synchronisation with the Window System.

- stream->unlink
    Closes the stream.  See also ->close_input and ->close_output.

- stream->wait
    Dispatches input events until the input is closed after
    reading end-of-file.

- stream->write_as_file: at=[int], text=char_array
    This method allows pce_open/3 to open a socket in `append` mode,
    using Prolog's stream primitives to send data to an XPCE stream.
    For example:

    	open(Socket, append, Stream),
    	format(Socket, 'Hello World~n', []),
    	close(Stream).

    Note: streams cannot be opened in mode `write` as they lack the
    required `source_sink->truncate_as_file` method.


## Get methods {#class-stream-get}

- stream<-read_line: timeout=[real] -> string*
    Dispatches events until the next physical line of data is
    available from the stream and return the line as a string object,
    including the terminating newline character.

    This method fails if it reaches the end of the input (see also
    ->end_of_file.

    The constant @nil is returned if no full line becomes available
    within `timeout` seconds.   The granularity of timeout timing is
    system dependant.  If the `timeout` is @default, this method
    blocks forever.

    Asynchronous input handling (using ->input_message) is
    the preferred way of handling process data.

