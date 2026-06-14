# class program_object {#class-program_object}

Class program_object is the super-class of all classes that form PCE's
"program world": classes, methods, variables, types and executable
objects representing statements (class code).

This class defines the tracing capabilities of PCE using the methods:

- ->trace
	Set a (conditional) trace-point on this object.  Entry and exit
	to this executable object will be printed.

- ->break
	Set a (conditional) break-point on this object.  The interactive
	tracer will be trapped when the object is executed.


## Instance variables {#class-program_object-instvars}

- program_object<->dflags: int
    The various debugging options are represented by an inclusive or of
    bit-masks.   The only legal user operation on these flags is:

    	get(ProgramObject, dflags, Flags),
    	<various trace/break operations>
    	send(ProgramObject, dflags, Flags).


## Send methods {#class-program_object-send}

- program_object->break: ports=[{full,enter,exit,fail}], value=[bool]
    A break-point is similar to a trace-point (see ->trace).  After
    printing the entry or completion of the execution of an object it
    prompts the user how to continue.

    At this prompt, the user may examine various stacks, set new trace or
    break-points, etc.

    @see program_object->trace

- program_object->initialise
    Initialises -dflags to represent ->trace: @default and ->break: @default
    (i.e.  inherit the value).  If the program_object is created by a
    goal running in system mode, the <-system flag is set to @on.

- program_object->initialise_new_slot: variable
    Initialises -dflags when loading program objects created before the
    introduction of this class.  See `file <-object`.

- program_object->system: bool
    When @on, the execution of the object is performed in `system-mode`,
    which implies that trace- and break-points reached during the execution
    will only be printed if `@pce <-trace` equals `always`.

- program_object->trace: ports=[{full,enter,exit,fail}], value=[bool]
    Set/clear a trace-point on the indicated object.  If an object that has
    a trace-point set is executed a message will be printed on entry and/or
    exit.

    The arguments are:

    - on/off/inherit
     	if @on, a trace-point is set on this object.  If @off, this
    	object is not traced.  If @default, the value if the trace
    	point is inherited.  For normal objects the trace-flags will be
    	inherited from the class.  For classes it will be inherited from
    	the super-class.

    - port(s)
    	The following ports or combinations of ports are defined:

    		| full  | Trace all ports (enter, exit and fail) |
    		| enter | Only trace entry                       |
    		| exit  | Only trace successful completion       |
    		| fail  | Only trace completion with failure     |

    - Condition
    	When present, this code object will be executed if the other
    	conditions are met.  If this code object returns success the
    	trace-point will be printed.   The arguments are:

    		@receiver	Receiver of the action
    		@selector	Selector of the action
    		@arg1 ...	The arguments.

    @see program_object->break


## Get methods {#class-program_object-get}

- program_object<-break: port=[{enter,exit,fail}] -> bool
    *Inherits description from*: program_object->break

- program_object<-system: -> bool
    *Inherits description from*: program_object->system

- program_object<-trace: port=[{enter,exit,fail}] -> bool
    *Inherits description from*: program_object->trace

