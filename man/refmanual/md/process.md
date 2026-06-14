# class process {#class-process}

Class process provides an interface to other (Unix) processes.  This
may be used for various purposes:

- Use processes for certain operations that cannot be done
	inside PCE itself.

- Build (graphical) interfaces for terminal oriented programs.

Processes may be both `synchronous` and `asynchronous`.  In the first
case the output of the process is explicitly read from the process
object (see process <-read_line'.  In the latter case, a message object
is associated with the process that will be invoked asynchronously when
output from the process becomes available (see `->input_message`).

When PCE is killed, it will first terminate all processes.   See also
`Pce ->exit_message`.

See also class stream and class socket.

@see file->open
@see !killed_on_exit


## Instance variables {#class-process-instvars}

- process<-arguments: vector
    Vector of char_arrays, holding the arguments passed to the process.
    Set by ->initialise and ->open.  Note that the arguments are not
    interpreted by a shell.

    @see process-name

- process<-code: name|int*
    Signal name or exit status that terminated or stopped the process.  If
    it is filled with a name, this is the name of the signal.  If it is
    filled with an integer, the process exited through calling `exit()` and
    the value is the argument passed to exit.

- process<-directory: [directory]
    Working directory for the child.  When @default this is the same
    directory as PCE's working directory.

- process-environment: sheet*
    Environment (see also `pce <-environment`) for the child process.
    Either the get- or the send-method will create a sheet mapping
    environment variable named onto values and fill the slot with this
    sheet.  Send changes the sheet's content.

    If this variable is set on ->open, the child will be passed these
    environment variables.  For example, to start `tex myfile.tex` with
    TEXINPUTS set to '../TeXmacros:/usr/local/lib/tex/macros':

    	?- new(P, process(tex, 'myfile.tex')),
    	   send(P, environment, 'TEXINPUTS',
    		    '../TeXmacros:/usr/local/lib/tex/macros'),
    	   send(P, input_message,
    			message(@pce, write, @arg1)),
    	   send(P, open).

- process-input_allocated: alien:int
    Allocated size of input_buffer.

- process-input_buffer: alien:char *
    This buffer is used to collect the data read from the process before it
    is split into records (see `process <->record_separator` and `process
    <->input_message).

- process<-input_message: code*
    This code object is executed each time input is available from the
    process.  If <->record_separator equals @nil, data is passed as soon as
    it is available.  In this case it is not clear if and how long chunks of
    data are split over multiple messages.  Otherwise, the input data will
    be split according to <->record_separator and this message will be
    invoked for each complete record.

    @see process-record_separator

- process<-name: char_array
    Name of the command executed.  This command is executed using Unix
    execvp() and thus uses the executable search-path.  Note that this is
    only the plain name.  The arguments are stored in <-arguments.

    @see process->open
    @see process->initialise
    @see process-arguments

- process<-pid: int*
    When the process is running (or stopped), this variable holds the
    process number of the inferior process.

- process-rdfd: alien:int
    File-handle to read from stream.

- process<-record_separator: regex|int*
    Regular expression defining the separation between two input records.
    The record as a whole is defined to be `_.*<record_separator>_'.  When
    @nil, the input is not split into predefined units.  The latter may be
    useful when the output of a process is dumped in an editor.

    **Defaults**:
    The default is regex('\n'), which implies the input if split into
    (complete) lines.

    @see process->record_separator
    @see process-input_message

- process<-status: name
    Status of the process:

    - inactive
    	After the process object is created

    - running
    	The process has been started and not yet stopped or terminated

    - stopped
    	The process has been stopped on a signal

    - killed
    	The process has been killed on a signal

    - exited
    	The process has terminated by calling exit()

    @see process->killed
    @see process->exited

- process<->terminate_message: code*
    This message is sent when the message is either killed on a signal or
    exited by calling exit().  @receiver is bound to the process object.
    @arg1 gives the name of the signal or the exit status (integer).

    Note that input is dealt with through the event mechanism of X-windows,
    while status changes to the process are dealt with using Unix signals.
    This implies the process can signal it has terminated, while their is
    still input available from it.

    @see process->killed
    @see process->exited
    @see process->wait

- process<-tty: name*
    Name of the pseudo tty to which the process is connected.  @nil if the
    process is not running (<-pid equals @nil) or the process is not
    connected to a terminal (<->use_tty equals @off).

    @see process-use_tty

- process<-use_tty: bool
    Connect the process by means of  a pseudo-tty (@on) or two pipes (@off).
    Using a pseudo-tty is necessary for many interactive processes as they
    will not flush their output when they are not connected to a terminal.
    Processes designed to be used by other processes generally may be
    connected through pipes.

    When a process is connected by means of a tty, the tty might perform
    input and output translations for the process.  Notably
    control-characters send to the process will be translated as usual.
    Pipes to not do any translations.

    **Defaults**: @on

    @see process-tty


## Send methods {#class-process-send}

- process->close
    Close the output channel to the process.  This will normally make the
    process terminate.

- process->exited: status=int
    Process has exited with status.

- process->killed: signal=name
    Process has terminated on named signal.

- process->end_of_file
    These methods are invoked by class process on itself when the process
    has exited, was killed or end_of_file has been read from the process'
    input.  These methods may be redefined to realise application specific
    actions.  It is necessary to invoke these methods as part of the
    redefined implementation.  Arguments

    	| ->exited | exit() status of the process (0=success) |
    	| ->killed | Name of the signal. See ->kill.          |

- process->initialise: command=char_array, argument=char_array ...
    Create a process object from the command name and arguments.  The
    process is not yet started.  Processes are started using ->open.
    The first argument is used to set <-name.  A vector created from
    the remaining arguments is stored in <-arguments.

    Note that the command, nor the arguments are interpreted (like the Unix
    shell does).  The call

    	new(P, process('awk -f script.awk')).

    will search for a file names `awk -f script.awk`, which of course it
    can't find.  Instead, use

    	new(P, process(awk, '-f', 'script.awk')).

    or

    	new(P, process('/bin/sh', '-c', 'awk -f script.awk')).

    @see process->open
    @see process-name

- process->kill: signal=[1..31|name]
    Send a signal to the process (which must be running).  The argument is
    either the number of the Unix signal (see man sigvec) or the name of the
    signal (see also man sigvec).  To convert the name used in the sigvec
    manual page, delete the leading _SIG_ and convert the remainder to
    lowercase.  Examples:

    	SIGTERM		==>	term
    	SIGINT		==>	int
    	SIGHUP		==>	hup

    @see process->killed

- process->open: command=[char_array], argument=char_array ...
    Start the process.  When the first argument is given, the command name
    and argument vector will be set like ->initialise.  If the process is
    already running, nothing happens.

    When <->use_tty equals @on, a pseudo-tty is allocated, the PCE forks
    a new process, connecting standard (error) I/O to this pseudo-terminal.
    If <-environment is given, the environment of the child is manipulated
    accordingly.  If <-directory is given the child will change working
    directory.  Finally the child execvp's (see man execvp()) using the
    <-name and <-arguments from the process object.

    When <->use_tty equals @off, two pipes are created, after which PCE
    forks.  stdin of the process is associated with one of these pipes,
    stdout and stderror with the other.  The command is started the same way
    as with <->use_tty @on.

    @see process->initialise
    @see process->use_tty
    @see process-name

- process->record_separator: regex|int*
    @see process-record_separator

- process->stopped: signal=name
    Internally invoked if the process has been stopped by a signal.
    @receiver refers to the process object, @arg1 is the name of the signal
    that stopped the process.

- process->unlink
    Closes the output and input channels and ->kill's the process.

- process->use_tty: bool
    Sets the <-use_tty variable.  Checks that the process is not currently
    running.

    @see process->open

## Get methods {#class-process-get}
