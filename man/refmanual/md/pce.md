# class pce {#class-pce}

Class pce has one predefined instance, called `@pce`.  The object @pce
provides operations to control the overall environment of PCE as well as
operations that are not a clear action on some object.

@pce is also used to control the debugger/tracer; handle exceptions,
etc.

@see topic Errors
@see topic Debugging
@see topic Exceptions
@see class host


## Instance variables {#class-pce-instvars}

- pce-application_data: directory
    Directory for storing xpce-related application data.  On Unix
    systems the default is ~/.xpce.  On Windows it is
    <appdata>/xpce, where <appdata> is the directory from
    CSIDL_APPDATA.  This directory typically contains the following
    files:

    - Defaults
    	User-level  class variable values.

    - Geometry
    	Saved geometries for persistent frames (see
    	persistent_frame.pl).

    - emacs_bookmarks
    	Bookmarks created by PceEmacs

    Applications using the pce_config.pl library create additional
    files.

- pce-catch_error_signals: bool
    Determines PCE's response to (normally) fatal error situations reported
    by the operating system.  When such an error occurs, a [PCE SYSTEM
    ERROR] is generated.  The following signals are considered fatal:

    	SIGQUIT		So, ^\ can be used to generate an error
    	SIGILL		Illegal instruction
    	SIGEMT		Emulator trap
    	SIGBUS		Bus error (illegal aligned pointer)
    	SIGSEGV		Segmentation fault (out-of-range pointer)
    	SIGSYS		Bad argument to system call
    	SIGPIPE		Broken pipe
    	SIGFPE		Floating point exception

    See your Unix manual for details on these signals.

    **Defaults**:
    The initial value is @off (do not trap signals).  Some host languages
    switch this flag to @on at initialisation time.

    @see pce->crash
    @see pce-print_c_stack

- pce-catched_errors: chain
    Chain holding all errors currently catched.  Each element is either an
    error_id or a chain with error_id'.

    @see pce->catch_error
    @see class error

- pce-debugging: bool
    Variable to indicate debugging-status of PCE.  Set by `pce
    ->trace' and `method->trace`.

    **Defaults**: @off (no debugging)

    @see pce->debug

- pce-defaults: source_sink|char_array
    File/rc from which to load defaults.  See ->load_defaults, class
    class_variable and class_variable/4.

    The default value is the file("$PCEHOME/Defaults")

    A runtime application that wishes the defauts to be in the resource
    file should do:

    	resource(pce, defaults,	pce('Defaults')).

    	:- initialization
    		send(@pce, defaults, rc(pce, defaults)).

- pce-exception_handlers: sheet
    Sheet with exception handlers used by `Pce ->exception`.  Each entry of
    this sheet maps an exception-name onto a code object which is executed
    when the corresponding exception occurs.  The argument bindings for
    @arg1, ... are the second, ... argument passed to `Pce ->exception`.
    The following exceptions are currently defined:

    - undefined_class
    	An attempt is made to reference an non-existing class while
    	doing one of the following:

    - Create an object
    - Load an object from file using `File <-object`
    - Create a subclass

    	Parameters:

    		@arg1:		class name

    - undefined_error
    	Called from `error<-convert` if the error id is not in
    	@errors.  If defined, it should create an error object
    	with the requested identifier.

    	Parameters:

    		@arg1: 		identifier of missing error object.

    - undefined_assoc
    	An attempt is made to resolve a symbolic reference (i.e. @pce),
    	but the reference is unknown. 	Parameters:

    		@arg1:		reference name

    - redefined_assoc
    	An attempt is made to create an object with the same symbolic
    	reference as an already existing object.	Parameters:

    		@arg1:		The existing reference name

    - initialisation_failed
    	The ->initialisation method for some instance failed. Parameters:

    		@arg1:		The (partial) instance
    		@arg2 ...	The arguments given to ->initialise

    @see pce->exception

- pce-exit_messages: chain
    Chain of code objects.  When the PCE process is killed, each of these
    messages is executed in turn with the following argument:

    	@arg1	Exit status of the process

    Filled by `pce ->exit_message`.

    @see !killed_on_exit
    @see pce->exit_message
    @see pce->die

- pce-home: [name]
    Home directory of the pce system.  Its value is used to find

    - PostScript header file	<HOME>/postscript/pce.ps
    - Standard bitmaps		<HOME>/bitmaps
    - The sources		<HOME>/src
    - The online manual material	<HOME>/man/reference

    The Prolog interface also adds the library directory

    	<HOME>/prolog/lib

    containing the PCE libraries (manual, demo programs, pcedraw,
    utilities).

    @see pce<-environment_variable
    @see pce<-postscript_header

- pce-last_error: name*
    Last error catched by the application.  See also ->catch_error and
    ->catch_pop.

    @see pce<-os_error
    @see pce-error_object

- pce-machine: name
    Identifier name for the hardware architecture used.

    @see pce-operating_system

- pce-operating_system: name
    Identifier for the operating system running on your system.
    On Unix systems this is extracted from the autoconf standard
    system naming <os>-<vendor>-<cpu>.  On MS-Windows
    this is one of:

    	win32 	The actual version could not be retreived
    	win32s	XPCE dowsn't run on this ..
    	win95	DOS-7 based Windows (95,98,ME,...)
    	winnt	NT based Windows (NT, 2000)

    See also <-machine, <-window_system, <-window_system_revision
    and ->has_feature.

    @see pce-machine

- pce-trap_errors: bool
    This flag, which is only available in the development version,
    controls XPCE's behaviour if a runtime error is trapped in an
    application.  When @on (default), the system will normally
    enter the tracer, allowing the user to inspect the context
    of the error.  If @off, the system will use the `object->report`
    mechanism to report the error and the tracer will not be
    trapped.  The latter behaviour is the only behaviour supported
    by the runtime system.  See also ->trace.

- pce-version: name
    Current version number of XPCE.  When documenting this method:

    	'4.4.3, September 1992'

    Its format:

    - First digit (4)
    	Current major release.  Generally indicates major revisions and
    	a large number of incompatible changes: version 4 is based on
    	X-windows whereas version 3 was using SunView.  Version 4
    	adds user-defined classes, type inferencing, completely
    	reorganised graphics and many more.  Version 5 is not
    	(yet) foreseen.

    - Second digit (4)
    	Indicates minor releases with limited compatibility issues.

    - Third digit (3)
    	Patch number.  No incompatibilities except for minor one's
    	for which we do not expect any normal application to be
    	sensitive.

    These fields are followed by an indication of the time of release.

    If the <-version is requested with the argument `name`, the time
    of the release is omitted (i.e.  4.4.3).  Finally, if the
    version is requested as a number, the value 10000 * major + 100
    + minor + patch is returned.

    @see pce->banner
    @see pce->info


## Send methods {#class-pce-send}

- pce->banner
    Writes a standard banner message for PCE, indicating version, hardware,
    operating system, the hostlanguage to which PCE is connected and a
    copyright notice.

    @see pce-version
    @see host-system

- pce->bench: message=message, times=int, how={forward,execute,qad,send}
    Sends messages in a tight loop for benchmarking purposed.  First
    argument is the message that is executed repeatedly.  Second argument is
    the number of iterations.  Last argument indicates which part of the
    message invocation is placed in the loop.  The rest is moved out of the
    loop.  Its values are:

    - forward
    	Invoke the full `message ->forward` behaviour

    - execute
    	As ->forward, put pushing and popping the argument stack
    	outside the loop.

    - send
    	Invokes the full send() virtual machine operation

    - invoke
    	Move method-resolution outside the loop.  Typechecking
    	the arguments and starting the implementing code is all
    	which is left inside the loop.

    @see pce<-cpu_time

- pce->catch_error: identifier=[name|chain]
    Inform PCE's error recovery mechanism that the application takes
    care of the indicated error, one of the indicated errors or all
    errors (@default).  If such an error occurs the method normally
    fails silently, setting <-last_error.

    The method `pce ->catch_pop` restores the catched errors to the state
    before invoking ->catch_error.

    The prolog predicate pce_catch_error/2 provides an interface:

    	?- send(@pce, hello, pce).
    	[PCE error: send: No implementation for: @pce/pce ->hello
    		in: send(@pce/pce, hello, pce)]
    	PCE:  1 fail: send(@pce/pce, hello, pce) ?

    While

    	?- pce_catch_error(no_behaviour,
    					   send(@pce, hello, pce)).

    	No

    After which

    	?- get(@pce, last_error, E).    ==> E = no_behaviour

    Note that errors with <-feedback: throw can also be handled
    using Prolog exception-handling.

    @see pce->catch_pop
    @see pce->catched
    @see pce-catched_errors
    @see class error

- pce->catch_pop
    Restores the catched errors to the state before the latest `pce
    ->catch_error'.

    @see pce->catch_error
    @see class error

- pce->catched: identifier=name
    Test if the indicated error_id is currently catched.  If this method
    succeeds, raising the indicated error will make the method fail rather
    than printing an error-message.

    @see pce->catch_error
    @see class error

- pce->confirm: format=char_array, argument=any ...
    If pce is connected to an open display, invokes `display ->confirm` on
    @display.  Otherwise the text is formatted on the terminal and
    PCE requests the user to type `y` or `n`.  Used infrequently.

    @see pce->inform
    @see display->confirm

- pce->console_label
    Set the label of the console-window from which XCE was started.
    Currently only implemented for the MS-Windows version of XPCE,
    and XPCE started from a Unix xterm(1) application.  The latter
    is verified by checking the environment variable TERM for the
    value `xterm`.  See also ->show_console.

- pce->debug_subject: subject=name
    Prints messages to the terminal to debug certain aspects of the system.
    The debug_id's supported change quickly from release to release.  Below
    is a tentative list.

    	absolute_position	Translation of relative coordinates
    	allocate			memory allocation
    	compute				Graphicals ->request_compute and ->compute
    	cursor				Determining visual cursor
    	dialog				Dialog layout
    	editor				Various editor happenings
    	event				Various things with events
    	fill				Filling text (editor)
    	flash				Flashing graphicals
    	float				Floating point conversions
    	focus				Event focusing
    	fragment			Fragment handling
    	frame				Various frame events
    	gc					Incremental garbage collection
    	get_xref			Maintenance of X id database
    	image				Some image functions
    	clone				Cloning objects
    	menu				Menu repaint
    	message				Execution of message objects
    	obtain				Failing obtainers
    	path				Smoothing paths
    	popup				Popup menu's
    	post				Event posting (event ->post)
    	postscript			PostScript generation
    	process				Actions on handling subprocesses
    	redraw				Repaint management
    	save				`Object ->save_in_file`
    	scan				`char_array <-scan`
    	scroll				Scrollbar handling
    	shift				text_image shifting of lines
    	spatial				execution of spatial constraints
    	text				Updating of text_images
    	undo				Undo in text_buffers
    	xref				X window references database

    ->nodebug_subject removes an item from this list.

    User code may exploit the same mechanism and test whether
    debugging a subject is enabled using ->debugging_subject.

    @see display->open
    @see pce->nodebug_subject
    @see topic Tracing
    @see pce->debug

- pce->debugging_subject: subject=name
    Test whether the named subject is registered using	->debug_subject
    and <-debugging is @on.  This test may be used in application
    code to exploit this debugging mechanism of XPCE.

    In a runtime system this method is available, but fails always.

- pce->define_class: name=name, super=name, summary=[string], realise=code
    Declare a class without details.  The system will run `code`
    when an action is performed that requires details of the class
    (such as creating an instance).

    All, except for some vital kernel classes of XPCE itself have
    been declared this way to delay/avoid building the
    class-definitions.

    See also `class ->realise`.

- pce->die: status=[int]
    Exit from the combined PCE/{Prolog,Lisp,...} process indicating status.
    It performs the following steps:

    - Request the host language to exit.
    	This is done using the C-interface call hostAction(HOST_HALT).
    	With a completely implemented interface, this in return will
    	call PCE's cleanup functions registered with
    	hostAction(HOST_ONHALT) and terminate the process.

    - If the host refuses to kill the process, PCE itself will call
    	its registered cleanup functions and call exit() with the
    	argument status.

    With a proper implemented interface, the normal termination will execute
    the `@pce <-exit_messages`, kill possible subprocesses and terminate the
    process.  The termination process is the same regardless of whether the
    host-language terminate functions is called or `pce->die` is invoked.

    **Defaults**: The default termination status is 0 (indicating success).

    @see pce-exit_messages

- pce->exception: identifier=name, context=any ...
    Raise an exception.  The first argument is the name of the exception.
    The remaining arguments are passed as context parameters to the
    code dealing with the exception.  The exception handlers are defined
    in the sheet `Pce <-exception_handlers`.

    **Bugs**: The exception mechanism is rudimentaly.

    @see class<-convert
    @see class<-instance
    @see pce-exception_handlers

- pce->exit_message: message=code
    Prepends code object to `pce <-exit_messages`.  That is: the last exit
    message registered will be executed the first.

    @see pce-exit_messages

- pce->expose_console
    Backward compatibility equivalent of ->show_console: open.

    Expose the console window of XPCE.  Only implemented for the
    MS-Windows version.  See also ->console_label.

- pce->fail
    Fails immediately without side-effects.

    @see pce->succeed

- pce->feature: any
    *Inherits description from*: pce-features

- pce->for_name: message=code
    Executes the code object on all instances of class name.  @arg1 is
    bound the the successive name objects.

    This method is safe: the name-base may be manipulated during its
    execution.  Tbe code will not be executed for named added during the
    execution of `pce ->for_name`.

- pce->for_name_reference: message=code
    Executes code for each defined global name reference (e.g. @pce, @arg1).
    The exit status of the code is ignored.  Arguments:

    	@arg1	Name of the global object (e.g. `pce`, `arg1`).

    This method passes the reference-names rather than the objects
    themselves.  As various of them are functions, good understanding of the
    expansion-rules for functions is necessary.

    @see pce<-object_from_reference
    @see pce->rename_reference

- pce->format: format=char_array, argument=any ...
    Write formatted output to the main window.  Normally only used for
    reporting debugging information.  For example, the following query
    dumps the mapping represented by the hash_table object @classes
    to the terminal:

    	?- send(@classes, for_all,
    			message(@pce, format, '%s\t%s\n',
    					@arg1, @arg2)).

    See `string ->format` for a description of the format specification.

- pce->iconify_console
    Backward compatibility equivalent of ->show_console: iconic.

- pce->info
    @see pce-version

- pce->inform: format=char_array, argument=any ...
    If there is an open display, run `display ->inform`.  Otherwise ->format
    the information on the terminal.

    @see display->inform
    @see pce->confirm

- pce->initialise
    Called at boot time to create the one-and-only instance called @pce.
    No more instances may be created.

- pce->list_wasted_core: list_content=[bool]
    Dumps a map of wasted-core cells.  Wasted core is core allocated, freed
    and not yet reused.  With list_content @on, PCE tries to identify the
    object deleted.  This might lead to crashes.  See also <-core_wasted.

    @see pce<-core_wasted

- pce->load_defaults: source_sink
    Loads class_variable object default values from the given
    source.   Initialisation of XPCE loads <-defaults.

    A Defaults file consists of statements.  Each statement is on a
    seperate line.  A statement may be split over several lines by
    preceeding the newline with a backslash (\).  The exclamation
    mark (\!) is the line-comment character.  All input from a !
    upto the following newline is replaced by a single
    space-character.  Empty lines or lines containing only blank
    space are ignored.

    Default files may include other default files using the statement

    - include <file>

    Default statements are of the form:

    	<class>.<class-variable>: <value>

    Where <class> denotes the name of the class, or * to
    indicate the default applies for any class defining this
    class-variable.  If both forms are found, the statement with the
    explicit class-name is used.  <class-variable> denotes the
    class-variable name.  <value> is the default value of the
    class-variable.  The syntax for <value> is similar to the
    arguments of send/[2-12].  The (informal) syntax for <value> is
    below.

    	<Any>	:= <int>
    			 | <float>
    			 | <Name>
    		     | @<Name>
    			 | <Chain>
    			 | <Object>

    	<Chain> := [ <Any> {, <Any>} ]
    		     | [ {<Blank>} ]

    	<Object> := <ClassName>()
    		      | <ClassName>(<Any> {, <Any>})
    			  | <PrefixOp> <Any>
    			  | <Any> <InfixOp> <Any>
    			  | <Any> <PostfixOp>
    		      | "<String>"

    	<String> := {<CharNotDoubleQuote>|""}

    	<Name>	 := <Letter>{<Letter>|<Digit>}
    			  | '{<CharNotSingleQuote>|''}}'

- pce->multi_threading
    Enable thread-safe access to XPCE.  This method is normally sent
    from the host-language interface at initialisation time if the
    host-language wishes to access XPCE from multiple threads.  The
    host-language interface should use the functions pceMTLock() and
    pceMTUnlock() as wrappers around access to XPCE.

    This method fails with an error when invoked too late (for the
    X11 version this implies after the window system has been
    initialised).  It fails silently if this version of XPCE is not
    compiled to support multi-threading.

    XPCE multi-threading support is currently rather limited: it
    consists of one global `recursive` mutex that ensures the system
    is activated in a single-thread only.  This mutex should be
    locked by the host-language around access to the basic
    operations (send, get, new and object) and is locked by XPCE
    itself for any event processed.

    XPCE call-back occurs in the thread waiting for events.  In the
    normal XPCE/Prolog context this is the main-thread.  As the
    system is locked during event-processing, smooth interactive
    operation requires event-processing to be completed quickly.
    If an event causes a long operation to start the host-language
    should run this operation in another thread.

- pce->nodebug_subject: subject=name
    *Inherits description from*: pce->debug_subject

    @see pce->debug_subject

- pce->rename_reference: old=name, new=name
    Change the name of a global reference.  This method may be used to
    rename functions.  Its use is strongly discouraged for normal
    application programmers.  See also `object ->name_reference`.

    @see object->name_reference
    @see pce->for_name_reference

- pce->show_console
    Control visibility of the console window, if accessible.  This
    call only works in the Windows version of XPCE, and only
    if the host interface honours the hostQuery(HOST_CONSOLE)
    request.   The meaning of the argument:

    - open
    	The console is opened normally.  This is the normal
    	situation for program development.

    - iconic
    	The console is displayed as an icon.  This is useful
    	for testing applications.  If the applications behaves
    	strange, the console may be opened to check it for
    	error messages.

    - hidden
    	The console windows is removed from the display.
    	This is the normal situation for finished applications.
    	Make sure that the application terminates if the last
    	windows disappears (see library(pce_main) for some
    	standard predicates to realise the toplevel control
    	loop).

- pce->succeed
    Succeeds without side effects.  Maybe used if a code objects
    that always yields success is needed:

    	new(@succeed, message(@pce, succeed)).

    Note that the following is faster:

    	new(@succeed, new(and)).

    @see pce->fail

- pce->syntax: syntax={uppercase}, word_separator=[char]
    Some languages (notably LISP) use case-insensitive keywords, internally
    represented in upper-case.  Host-language keywords are most naturally
    mapped on PCE-names.  This method, which is normally send by the LISP
    interface right after PCE has been initialised performs the following actions:

    - Map names to uppercase
    	All predefined names of PCE that are strictly lowercase are
    	mapped onto uppercase.  This implies the lowercase equivalents
    	disappear: classes, methods, variables and all other constants
    	referred to in this manual will have uppercase names.

    - Change the word-separator.  The default word separator of
    	PCE,the '_' is replaced by the given character in all name objects.

    **Defaults**: The default word separator is '_' (unchanged).

    **Bugs**:
    To get compatible resource-handling, resources defined as type `name` will
    be mapped to uppercase (and have their word-separator changed) when
    converted.  Also, generated object-identifiers (for fonts, classes, etc.) will
    be mapped to upper-case.   The run-time generated keywords may lead to
    unexpected behaviour.

    Finally, when converting of a bitmap- or image-name to a filename, the name
    is downcased.  This makes it implossible to use automatic conversion when
    the file-name consists of uppercase characters.

    @see name->syntax

- pce->write: argument=any ...
    Write the arguments using PCE's internal pretty-printing routine.  The
    arguments are separated by spaces.  Note that output is generally not
    flushed.

    @see pce->write_ln

- pce->write_ln: argument=any ...
    Writes the arguments as `pce->write` and terminates the line with a
    newline.  Useful for dumping information to the terminal:

    	send(@images, for_all, message(@pce, write_ln, @arg1, @arg2)).

    Dumps a map of all named image objects (which is initially empty).

    @see pce->write


## Get methods {#class-pce-get}

- pce<-answer_stack_size: -> cells=int
    Statistics on the incremental garbage collection system.
    Objects created that are not related to any other object are
    pushed on the `answer` stack.  They are deleted from this stack
    iff

    - They are related to another object
    - They are `object ->lock_object`ed (or given a named
    	   reference)
    - The answer-stack is rewinded to a point before the
    	   creation of the object.

    Both event-call-back and the execution of a PCE method mark and
    rewind this stack.   Thus

    	?- new(X, point).

    Creates a point and leaves it on the answer-stack, while

    	?- [user].
    	create_point :- new(X, point).
    	^D
    	?- send(@prolog, create_point).

    Creates a point, pushes it on the answer-stack.  Then rewinds
    this stack on exit of the `@prolog ->create_point`, garbage
    collection the point object.  Thus

    	?- pceusage(send(@prolog, create_point)).
    0. 00 seconds, 0 bytes, 1 created - 1 freed = 0 objects

    NOTE: pceusage/1 should often be called twice to get the
    intended result.  The first call may create objects in the
    XPCE/Prolog cache.

- pce<-convert: object=unchecked, type=type -> converted=unchecked
    The method `pce <-convert` provides access to `type <-check`, the PCE
    type checking and conversion system.  The first argument is the object
    to be checked/converted.  The second is the type that should be met.

    Equivalent to `type <-check`, but generally more comfortable as
    this method will automatically translate a type specification
    into a type object.

    The following example converts anything convertible to an integer to
    an integer:

    	convert_to_int(Any, Int) :-
    		get(@pce, convert, Any, int, Int).

    After which

    	convert_to_int('78', X)				==> 78
    	convert_to_int(string('1992'), X)	==> 1992
    	convert_to_int(hello, X)			fails
    	convert_to_int(100, X)				==> 100

    @see type<-check
    @see type<-convert
    @see topic Conversion

- pce<-core_usage: -> bytes=int
    Number of bytes core allocated by PCE.  Notes:

    1. Memory requested by the X-window libraries or the
    	host-language is not included

    2. Memory requested by PCE is never returned to the process.

    @see pce<-core_wasted
    @see tool Statistics

- pce<-core_wasted: -> bytes=int
    Memory allocated by PCE, but currently not in use.

    @see pce->list_wasted_core
    @see pce<-core_usage
    @see tool Statistics

- pce<-cpu_time: kind=[{user,system}] -> seconds=real
    CPU time used by the process PCE belongs to.  When the argument is
    @default, this is the combined user and system time, otherwise it is the
    time specified.

    Note that the host language is normally part of the same process.

    @see pce->bench

- pce<-date: -> string
    Requests the system time using the C-library function ctime(3) and
    returns the result as a string object.  The returned string has no
    trailing newline (as its Unix counterpart).  Its format is:

    	 Sun Sep 16 01:03:52 1973

    See also class date.

    @see date<-string
    @see class date

- pce<-deferred_unalloced: -> number=int
    Number of objects that has been ->free'd by the user, but are still
    referenced.   To avoid crashes, the memory belonging to such objects
    is not freed as long as there are references to the object.

- pce<-environment_variable: name=name -> value=name
    Read named Unix environment variable which' value is returned as a name.

    If `name` is _PCEHOME_ and this is not defined as a Unix environment
    variable, the value of `@pce <-home` is returned.

    **Diagnostics**:
    If the variable is not defined `pce <-environment_variable` fails
    silently.

    @see pce-home

- pce<-fd: -> number=int
    Number of remaining unused Unix file-descriptors.  When PCE was based on
    the SunView windowing environment, each window used a file descriptor
    and an ungraceful error was generated when no more filedescriptors were
    available.

- pce<-instance: class=class, argument=unchecked ... -> created=object|function
    Create an object (as Prolog new/2 or Lisp pce-new()).  @pce <-instance
    converts the first argument into a class.  This process deals with
    handling class-names rather then class objects and invokes the
    autoloader if the class does not yet exist (see pce_autoload/2 and
    <-exception_handlers).  Next, the method `Class <-instance` is called to
    create the new instance.

    The following function creates a new point object each time it is
    evaluated:

    	?(@pce, instance, point, 4, 5)

    See also class create, `class <-instance` and @vmi_new.

    @see class<-instance

- pce<-max_integer: -> value=int
    Highest/lowest integer value that may be represented using PCE's
    data-type `int` or `number`.  Current range:

    	-2^29 ... 2^29 - 1 (-536870912 ... 536870911)

    @see class number
    @see pce<-min_integer

- pce<-min_integer: -> value=int
    *Inherits description from*: pce<-max_integer

    @see pce<-max_integer

- pce<-object_from_reference: reference=int|name -> object=unchecked
    Converts an object name or integer object-reference into an true object:

    	?- get(@pce, object_from_reference, prolog, Obj)

    	Obj = @prolog

    This is the inverse of `object <-object_reference`.  See also `object <-convert`.
    These methods are used seldomly by application programmers.

    @see object<-object_reference
    @see pce->for_name_reference

- pce<-objects_allocated: -> number=int
    Obsolete method returning the same as

    	get(class(object), no_created, @on, Count)

    See `class <-no_created` and `class<-no_freed`.

    @see pce<-objects_freed
    @see class-no_created
    @see tool Statistics

- pce<-objects_freed: -> number=int
    @see class-no_freed
    @see pce<-objects_allocated
    @see tool Statistics

- pce<-os_error: -> identifier=name
    Name indicating the last operating-system generated error.  That is, the
    text that would normally be printed by the Unix utility perror(2).  Its
    use is not encouraged.

    @see pce-last_error

- pce<-pid: -> identifier=int
    Unix process identifier for this process.  May be used to create unique
    system-wide names.  Note that file objects with a proper
    temporary name may be created using

    	?- new(TmpFile, file).

    See `file ->initialise`.

- pce<-unresolved_types: -> chain
    Scans @types for types that are of `type <-kind: class`, but have no
    initialised class associated with them.  A name that is not the name of
    an existing class will be converted into a type of kind `class`.  This
    type is only useful after the class is actually created.  This mechanism
    allows the programmer to refer to classes as a type before defining
    them.

    The most likely cause of this trouble is a misspelled type-name or a
    forgotten class definition.

- pce<-user: -> user=name
    Login name of user.  On Unix systems this uses either getlogin()
    or password information from the current UID.  On Windows it
    examines the environment variable %USER%.

    See also <-user_info.

