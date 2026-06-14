# class method {#class-method}

Methods are used by the PCE message passing algorithm to map the
combination of an object/class, the type of the message (`send` or
`get`) and the selector of message onto executable code.

Additionally, methods define the expected argument types and the
argument names of the operation.  The PCE message passing algorithm uses
this information to verify and if necessary and possible convert the
types of the arguments presented,

The implementation of a method is either a C-function pointer (used for
system-defined methods), or a PCE code object (used for user-defined
methods).  While executing the latter, the following arguments are
forwarded:

	| @receiver        | Receiver of the message  |
	| @arg1 ... arg<n> | Arguments to the message |

See also  class behaviour, `send_method ->send`, `get_method <-get`,
`class ->send_method`, `class ->get_method`, `object ->send_method` and
`object <-get_method`.

@see topic Specification
@see class send_method
@see class get_method
@see object->send_method
@see object->get_method
@see class->send_method
@see class->get_method
@see class class
@see class code
@see topic Types


## Instance variables {#class-method-instvars}

- method<-context: class|object*
    When attached to a class, `Method <-context` refers to this class.  When
    member of a method_group, attached to an interceptor or not attached at
    all, its value is @nil.

- method-function: alien:Func
    When implemented in the C-language, this slot contains a pointer to the
    implementing C-function.  When implemented with a code object, the
    value of this slot is NULL.

- method-group: [name]
    Conceptual group of the method.  Used for structuring the reference
    manual.  If the variable is @default, <-group will

    1. check for an instance variable of the same name
    2. check for a <-inherited_from method.

    and return the first non @default value encountered.

- method<->message: code|host_method*
    Implementation of the method object.  This slot cooperates with the
    -function variable:

    - @nil
    	In this case the -function is a pointer to a C-function
    	implementing the method.  This function will be passed
    	the receiver and the arguments of the method.  If the
    	last type of the method denotes multiple arguments
    	(...), the C-function will be passed an integer for the
    	number of arguments and a pointer to an array of objects
    	for the actual arguments. Internal use only.

    - host_method object
    	An opaque handle, provided by the host language, that
    	refers to a procedure in the host language.  See also
    	pce_predicate_reference/2.

    - A code object
    	If the method is a get_method object, this must be a
    	function object.

    	If the method is executed, @receiver points to the
    	receiver of the message and @arg1, ...  point to the
    	arguments provided to the message.  See also
    	->initialise.

- method<-name: name
    Selector or name of the method.

- method<->source: source_location*
    Sourcefile and linenumber where this method is defined.  The Prolog
    defined class compiler fills this argument.

    The source editor of this manual uses it to find the sources.  This
    editor uses the TAGS file in the PCE source directory to find the
    sources of PCE's built-in methods.

    **Defaults**: @nil (location of sources is unknown)

    @see class source_location

- method-summary: [string]*
    Summary description of the method.  It should be a string of about 45
    characters maximum width describing the function implemented by the
    method.

    The summary attribute is used by the summary lists of methods, etc.
    displayed by these manual tools.  It is defined for all built-in
    methods and may be defined in the body of a Prolog defined
    method.

    If the slot contains @default, and the <-context is a class
    object, the method will extract the summary from a variable with
    the same name or a method on a super-class with the same name
    (in this order).

    @see tool Summary List

- method<-types: vector
    The `Method <-types` slot describes the type-checking performed when the
    method is invoked.  it is a vector of type objects.  The elements of the
    vector denote the argument types for the corresponding argument
    positions.  When a C-function is invoked the arguments are the receiver,
    followed by the arguments that activated the method.  When a code object
    is invoked, @receiver is bound to the receiver, @arg1 is bound to the
    first argument, @arg2 to the second, etc.

    See also class type and `send_method ->send`.

    @see topic Conversion
    @see topic Checking


## Send methods {#class-method-send}

- method->initialise: name=name, types=[vector], implementation=code|host_method, summary=[string]*, source=[source_location]*, group=[name]*
    Create a new method object.  Methods come in two flavours:
    send_method objects for `procedures` and get_method objects for
    `functions`.

    Method objects may be associated to a class or to an individual
    object using the methods `class ->send_method`, `class ->get_method`,
     	`object ->send_method` or `object ->get_method`.

    The arguments to ->initialise are:

    	| <-name         | Name of the `selector` to handle       |
    	| <-types        | Vector of type objects for arguments   |
    	| implementation | <-message implementing the method      |
    	| <-summary      | Online manual summary description      |
    	| <-source       | Source-file location of the definition |
    	| <-group        | Functional group of the method         |

    Messages to classes are normally associated using the class
    definition interface implemented by pce_begin_class/[2,3],
    pce_end_class/0 and various macros defined by these
    predicates.

    The example below defines a move_gesture object that is only
    active if the window in which the graphical to be moved defines
    an attribute (get_method) <-mode that yields the value `edit`.

    	...,
    	new(G, move_gesture),
    	send(G, send_method,
    		 send_method(verify, vector(event),
    					 @arg1?window?mode == edit)),
    	...

    The alternative would be to define a new class for move gestures
    that only operate in windows that are editabe:

    	:- pce_begin_class(move_if_editable_gesture,
    					   move_gesture).

    	verify(_G, Ev:event) :->
    		"Test if the window is editable"::
    		get(Ev?window, mode, edit).

    	:- pce_end_class.

## Get methods {#class-method-get}

- method<-group: -> name
    *Inherits description from*: method-group

- method<-inherited_from: -> method
    If method is a method of a class, this method will return a method of
    the same type (send_method or get_method) of a super-class with
    the same selector.

    This method may be regarded as a redefinition (usually refinement) of
    the returned method.   Used by the reference manual to generate the
    _Refinement of:_ lines.

- method<-print_name: -> name
    Textual representation of the method of the form:

    	<class> <access><selector>

    See also `variable <-print_name`.

