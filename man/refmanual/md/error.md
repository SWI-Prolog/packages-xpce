# class error {#class-error}

An error object represents describes how an error, warning or message
with specified error-id is handled.  When an error is detected PCE will
invoke `object ->error` on the associated object, which is normally the
receiver of the currently executing method.

An error object defines a <-feedback which determines whether the error
is printed in a format suitable for the PCE programmer in the
host-language window, raises a host-labnguage exception or
is reported in a format suitable for the application user using
the general `object ->report` mechanism.

An error object defines a <-kind that determines the nature and
how serious the error is considered.

The PCE online manual has an entry _Errors_ in the _Browsers_ menu that
allows the user to examine the available (reusable) error objects.

@see pce-error_object
@see pce-error_method
@see pce-error_context
@see pce-catched_errors
@see pce->catched
@see pce->catch_pop
@see pce->catch_error


## Instance variables {#class-error-instvars}

- error<->feedback: {report,throw,print,backtrace}
    Determines how the error is reported (provided it is reported; see
    `error <-kind`).   Its values are:

    - report
    	Invokes `object ->report` to report the error.  See also
    	`visual ->report`.

    - throw
            If the error is raised in a context called by the
    	host-language, and the host-language indicates it wants
    	to handle the error itself, the error is recorded in the
    	hosts goal-frame and the `object ->error` succeeds
            with failure.  When control is returned to the
    	host-language, the host-language should raise and
    	error (For example, as throw/1 in Prolog):

    		?- catch(send(@pce, hello), E, true).

    		E = error(pce(@290191/error,
    			 	      [@pce/pce, (->), hello]),
    				  send(@pce/pce, hello))

    - print
    	Prints the error in the window from which xpce was started.

    @see error-kind

- error<-format: string
    Format string used to print the message.  Used by ->display.  See
    `string ->format` for a description of the format arguments.   The first
    argument formatted is always the `generator` of the error (the
    object receiving `object ->error`).  The remaining arguments are
    the context arguments from `object ->error`.

- error<-id: name
    Unique identifier of the error. The hash_table @errors maps error
    identifiers to error objects.  See also <-convert and <-lookup.

- error<->kind: {status,inform,warning,error,fatal,ignored}
    Describes how `serious` the error is.   The error categories are related
    to the report categories defined by `object ->report` (see `visual
    ->report'), except for `fatal` and `ignored`.

    The values are:

    - status
    	Provide status information that may be ignored by the user.

    - inform
    	Provide status information that may not be ignored by the user.

    - warning
    	Report a problem that may be ignored by the user.

    - error
    	Report a problem that may not be ignored by the user.

    - fatal
    	A message is printed and the stacks are dumped.  After this, PCE
    	attempts to return to the host (Prolog, Lisp) toplevel.

    - ignored
    	The invoked action fails silently.

    @see error-feedback


## Send methods {#class-error-send}

- error->display: unchecked ...
    Present the error to the user or programmer.  This method may be
    redefined to present the error in a different way.  The default
    implementation performs the following actions:

    - if <-feedback equals `report`
    	Invoke `object ->report` using <-kind, <-format and the
    	arguments passed.  This message is sent to the object
    	that generated the error using `object ->error`.

    - if <-feedback equals `print`
    	Use '@pce ->format' to print the message.  If <-kind is
    	`error`, start the PCE tracer.

- error->initialise: name=name, format=string, kind=[{status,inform,warning,error,fatal,ignored}], feedback=[{report,throw,print,backtrace}]
    Create an error from its <-id, <-format, <-kind and <-feedback.  The
    default `kind` is warning and the default `feedback` is report.   The
    error object is locked against the garbage collector and a mapping
    from its <-id is added to the @errors database.

    The following provides a (schematic example).

    	:- new(_, error(key_clash,
    					'%N: Key %s is already in use')).

    	:- pce_begin_class(database, hash_table).

    	add(DB, Record:record) :->
    		"Add record to the database"::
    		get(Record, key, Key),
    		(   get(DB, member, Key, _)
    		->  send(DB, error, key_clash, Key),
    			fail
    		;   send(DB, append, Key, Record)
    		).

    	:- pce_end_class.

    See also `object ->report`.


## Get methods {#class-error-get}

- error<-convert: name -> error
    Convert an error-id into an error object.  It uses the hash_table
    @errors for this purpose.

- error<-lookup: name -> error
    Performs a <-lookup in the @errors database to ensure the uniqueness of
    the error identifiers.

