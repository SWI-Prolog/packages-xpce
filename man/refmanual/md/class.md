# class class {#class-class}

Class class describes a PCE class.  All classes (including class class
itself) are an instance of this class.  The classes are organised in a
single-inheritance hierarchy.  Class object is the root of the PCE
class hierarchy.  A class can be regarded meta-data describing
(among others) the following properties of its instances:

- Instance variables (slots, attributes)
	The instance variables define the data-structure (types and
	access permissions) of the instances of the class.

- Send methods
	A collection of methods that can be activated by the `send`
	virtual machine operation.

- Get methods
	A collection of methods that can be activated by the `get`
	virtual machine operation.

- Class variables
	Class-constants that can be initialised from the
	Defaults file, see `@pce ->load_defaults`.

The user may create and modify classes to extend the PCE system.
Classes are created just like any other PCE object:

	?- new(X, class(person, object)).

Creates a class named person below class object.  Initially class
person only differs from class object by its name.

@see topic Defining Classes
@see topic Class-level Programming
@see class method
@see tool ClassBrowser
@see class variable
@see class interceptor
@see class code
@see class behaviour


## Instance variables {#class-class-instvars}

- class-changed_function: alien:SendFunc
    C-function activated if a slot of an instance is changed and `Object
    <->inspect' for this instance is @on.

    @see class-changed_messages

- class<->changed_messages: chain*
    Chain of code objects activated after a slot of an instance has changed
    that has `object ->inspect: @on`.  Each code object is activated in turn
    and the returned status of the execution is ignored.  Argument binding:

    	| @arg1 | Instance who's slot changed |
    	| @arg2 | Name of the changed slot.   |

    Changes are not notified before the initialisation of the instance is
    complete or after the destruction has started.

    This trap is used by the inspector tool.  See also ->created_message and
    ->freed_message.

    @see class-freed_messages
    @see class-created_messages
    @see object->inspect
    @see class-changed_function

- class-clone_function: alien:SendFunc
    C-function pointer.  This function is responsible for cloning alien
    data associated with an instance.

    @see topic Kloning objects

- class<->clone_style: {recursive,none,relation}
    Determines how an instance of this class is cloned.  Possible values:

    - recursive
    	Clone this instance and all its parts recursively.

    - none
    	The instance itself is returned.  Used for reusable objects.

    - nil
    	`Object <-clone` returns @nil.

    See also `object <-clone` and `variable <-clone_style`.

    @see topic Kloning objects
    @see object<-clone

- class<->created_messages: chain*
    Chain of code objects invoked when an instance of this class is created.
    After successful creation of an instance, the following arguments are
    forwarded to each code object in the chain:

    	| @arg1 | Name of the class                             |
    	| @arg2 | Reference to the new instance                 |
    	| @arg3 | Action that caused the creation of the object |

    Possible values for @arg3 are:

    	| new    | Normal creation (new/2 or `class <-instance`) |
    	| loaded | Loaded by `file <-object`                     |
    	| clone  | Cloned using `object <-clone`                 |

    May be used to keep track of the set of instances of this class
    in combination wit ->freed_message.  See also ->changed_message
    and ->record_instances.

    @see class-freed_messages
    @see class-changed_messages
    @see class<-instance

- class<-delegate: chain
    The chain <-delegate consists of instance variable objects that
    should be used for delegation.   The PCE message passing system
    will try to forward an incoming message to the values of these
    instance variables after it failed to locate an implementation at
    the object or class level.  The elements of this chain are tried in the
    order they appear in the chain.

    	| ->delegate         | Adds a variable at the end   |
    	| ->prepend_delegate | Adds a variable at the start |

    See also `class <-send_method` and `class <-get_method`.

- class<->freed_messages: chain*
    When an instance of this class is freed that has `object <->inspect:
    @on', its reference will be passed to the code objects in this chain.
    Argument binding:

    	@arg1:	Object to be freed.

    This message is sent as the first action of `object ->free`.   See also
    `->changed_message` and `->created_message`.

    @see class-created_messages
    @see class-changed_messages

- class-send_catch_all: [send_method]*
    Handle not-yet-handled send messages.

- class-get_catch_all: [get_method]*
    These variables provide fast access to the methods `object
    <->catch_all'.

    @see class-delegate_classes

- class-send_methods: chain
    Send methods not inherited.

- class-get_methods: chain
    Chain of methods of the indicated type (send/get) (re)defined for this
    class.  Inherited or delegated methods are not in this chain.   See also
    <-get_method and <-send_method, `object <-send_method` and `object
    <-get_method', for resolving methods.

    @see class-get_table

- class<-get_table: hash_table
    The -local_table maps instance_variable names onto instance variables
    and is used by `object <->slot`.  The -get_table and -send_table both
    map selectors onto implementations.  The implementation is either a
    method or a variable.  All these tables only contain the currently
    resolved methods: they are initially empty.   Used and maintained by:

    	| -local_table | <-instance_variable |
    	| -get_table   | <-get_method        |
    	| -send_table  | <-send_method       |

    @see class-get_methods

- class<->handles: chain*
    Graphical classes only.  This chain contains handles that are available
    to each instance of the class.  Note that handles can be defined
    simultaneous at the class- and the instance-level.

    Handles at the class level are normally declared using the handle/4
    construct expanded by the Prolog defined class compiler.

    @see topic Connections
    @see graphical-handles
    @see class handle

- class-in_event_area: alien:SendFunc
    C-function pointer to validate whether an event should be regarded to
    have happened `inside` a graphical object.

    @see graphical->in_event_area

- class-convert_method: [get_method]*
    Type conversion.

- class-lookup_method: [get_method]*
    Type conversion.

- class-initialise_method: [send_method]
    Direct pointers to these methods for two reasons:

    - Get the system started while there are no chains or
    	hash_tables yet.
    - Fast implementation of object creation and type conversion.

- class<-instance_size: int
    Size of an instance in bytes.  Note that this does not include the size
    of `parts` of the instance.

- class<-instance_variables: vector
    Vector object holding *all* instance variables of this class.  Initially
    a copy of the super-class.   The variable object at index <n> of this
    vector describes the <n>-th slot of the object.  See also `object <-slot`.

    The `variable <-context` argument may be used to find the class on which
    a variable is defined.  The following code returns a chain of instance
    variables *not* inherited:

    	new(Vs, chain),
    	send(Class?instance_variables, for_all,
    		 if(@arg1?context == Class,
    			message(Vs, append, @arg1)))

- class<-instances: hash_table*
    The hash_table object <-instances contains all instances of
    this class that have been created since the table was attached
    to the class.   Note that instances of sub-classes are *not* in
    this table.   The table is maintained by the new() and free()
    virtual machine operations as well as by <-clone and
    `file <-object`.

    This table is attached using ->record_instances.  If the first
    argument is @on or @default an <-instances table is attached.
    If this argument is @off a possible associated table is
    detached.  If the second (`recursive`) argument is @on
    (default), ->record_instances will be invoked with the same
    arguments on all sub-classes.  Thus

    	?- send(class(object), record_instances).

    Will record the instances of *all* classes.

    See also ->created_message and ->freed_message.

- class<-local_table: hash_table
    *Inherits description from*: class-get_table

- class<-name: name
    Name of the class.  Class names are global and must be unique.  The
    table @classes maps class-names into class objects.

    PCE built-in classes have relatively short and simple names.  It is
    adviced to prefix application classes with a common prefix to avoid
    conflicts with other packages or future PCE classes.  For example,
    all classes consituting PceDraw are called draw_<something>.

- class-no_freed: int
    Number of instances freed.

- class-no_created: int
    Number of instances created/freed.  By default it returns the
    number of created instances of the class itself.  If `subtoo`
    is @on it adds the number of instances created in all
    <-sub_classes recursively.

    To find the actual number of existing instances, subtract
    <-no_freed.

    @see pce<-objects_allocated

- class<-realised: bool
    Bool indicating whether or not the class is fully built.  A
    class that is not realised <-name, <-super_class, <-sub_classes,
    <-summary and -make_class_function are defined.  All other slots
    have undefined values.

    When information or instances of a non-realised class are
    requested, XPCE will invoke ->realise on the class to build the
    class.

    *Inherits description from*: class->realise

- class<->save_style: {normal,external,nil}
    Flag to help `object ->save_in_file`.  If this methods encounters a slot
    holding an instance of this class it will save the instance according to
    the value of <-save_style:

    	| normal   | Save the instance to the file          |
    	| external | Save the (named) reference to the file |
    	| nil      | Write @nil to the file                 |

    See also `variable <-save_style` and `file <-object`.

    @see object->save_in_file

- class<->selection_style: {none,invert,corner_handles,side_handles,corner_and_side_handles,line_handles,path_handles}*
    If class is a subclass of class graphical, the selection_style
    determines the visual feedback of graphical objects that have `graphical
    <->selected: @on'.  See also graphical.selection_style.  This
     class_variable object is redefined for each subclass of
     graphical.

- class<-send_table: hash_table
    *Inherits description from*: class-get_table

- class-solid: bool
    @see device-area_must_be_cleared

- class<->source: source_location*
    Location in the sources.

- class<-super_class: class*
    Immediate super class.

- class-sub_classes: chain*
    The variables <-sub_classes and <-super_class define the class
    hierarchy.  The class hierarchy is visualised using the `Class
    Hierarchy' tool from the online manual tools.   The <-super_class of
    class object is @nil.

    The method <-super_class_name is used to create the appropriate
    term description (see `object <-_arg`).

- class<->term_names: vector*
    Vector of selectors of get_methods that do not require arguments.  The
    elements of this vector object are used by `object <-_arg` to return the
    nth (1-based) argument of the term-description.

    If element <n> of this vector has value <selector>, the class must
    define <-<selector> and the nth (1-based) result of `object <-_arg` is
    the result of <-<selector> on the object.

    For example, class point defined <-term_functor: point and <-term_vector
    vector(x,y).  Therefore the term has the form:

    	point(`point<-x`, `point<-y`)

    @see object<-_arg
    @see topic Object -> Term

- class-neighbour_index: alien:int
    Index of neighbour in hierarchy.

- class-tree_index: alien:int
    Used for very fast implementation of `class ->is_a` and `object	->instance_of`.
    After a modification on the class hierarchy (normally a class created),
    PCE numbers the class-hierarchy top-down and left-to-right.

    The slot -tree_index is assigned the current number when this class is
    first reached, while the slot -neighbour_index is assigned the current
    number *after* numbering the subtree below this class.   Now, a class
    A `is_a` B iff

    	(A<-tree_index) >= (B<-tree_index)        AND
    	(A<-tree_index) <  (B<-neighbour_index)

- class<->un_answer: bool
    Hint for the incremental garbage collector.  By default, fresh created
    instances (either by new/2 or some get method that generates an
    instance) are stored in a special table.  As these instances are
    `answers` to methods, this table is called the `answer` table
    internally.

    If an object is made an attribute of another object, it will be removed
    from the answer table and reclaimed by the garbage collector it its
    reference count drops to zero.   Object remaining in the answer table
    are reclaimed if the `context` in which they where created is ended.
    The main event-loop starts/ends such a context and likewise do
    all method executions.

    Now consider the code below, which produces an alphabetically ordered
    list of class-names:

    	?- new(Chain, chain),
    	   send(@classes, for_all,
    			message(Chain, append, @arg1)),
    	   send(Chain, sort).

    With the above schema, Chain will be an `answer object` after creation.
    It will be made a slot of the message object, thus loosing its answer
    status.  After the `chain ->for_all`, PCE will garbage collect the
    message.  The reference count of the chain drops to zero and thus
    the chain is reclaimed too.

    This situation is common and therefore class code sets the <-un_answer
    flags to @off.  This tells `variable ->send` not to remove the chain
    from the answer table.


## Send methods {#class-class-send}

- class->changed_message: code
    *Inherits description from*: class-changed_messages

    @see object->inspect

- class->clone_style_variable: variable=name|int, style={recursive,reference,reference_chain,value,alien,nil}
    Set `variable <-clone_style` of named variable.   Equivalent to

    	get(Class, instance_variable, Name, Var),
    	send(Var, clone_style, Style)

    See also `object <-clone`.

- class->created_message: code
    *Inherits description from*: class-created_messages

    @see object->inspect

- class->delegate: variable=name|int
    *Inherits description from*: class-delegate

- class->freed_message: code
    *Inherits description from*: class-freed_messages

    @see object->inspect

- class->instance_variable: variable
    Add/redefine instance variable.

- class->send_method: send_method
    Add/redefine send method.

- class->get_method: get_method
    The methods ->get_method, ->send_method and ->instance_variable are used
    to expand or redefine the behaviour of a class.  They are normally used
    right after the class has been created.  The following restrictions
    apply:

    - Instance variables cannot be redefined or extended after
    	instances of subclasses of the class have been created.

    - The name of a instance variable may not already appear as
    	an instance variable in one of the super-classes.

    - Behaviour of each type (send/get) may be defined both using
    	variables and methods.   If a variable has `send` access there
    	should not be a send_method and if a variable has `get` access
    	there should not be a get_method with the same name.   If both
    	exist, the method is used instead of the variable.

    Note that both the Prolog and Lisp language interfaces provide an
    interface for defining methods.  See their resp. manual.

    The methods `object ->slot`, `object ->send_super`, `object
    <-get_super' and friends are designed to support the definition of
    method implementations.   See also class code.

    @see class method
    @see object<-slot
    @see object->get_method

- class->handle: handle
    *Inherits description from*: class-handles

    @see topic Connections
    @see graphical->handle
    @see topic Handles

- class->initialise: name=name, super=[class]*
    Initialise a class specified name.  The new class is (initially) a copy
    of the provided super-class.  After creation of a new class the
    following methods are commonly used to refine it:

    - ->instance_variable
    - ->delegate
    - ->send_method
    - ->get_method

- class->is_a: class
    Succeeds if the receiver is the same class of the argument or an
    (indirect) sibling of the argument.   See also `object ->instance_of`.

    @see object->same_class

- class->prepend_delegate: variable=name|int
    *Inherits description from*: class-delegate

- class->realise
    Realises a class (i.e.  fills the class definition).  Called by
    the kernel if details are requested for a class that has been
    registered using `@pce ->define_class`.   See also <-realised.

- class->record_instances: record=[bool], recursive=[bool]
    *Inherits description from*: class-instances

- class->save_style_variable: variable=name|int, style={normal,nil}
    Set the `variable <-save_style` for named variable.

## Get methods {#class-class-get}

- class<-convert: any -> class
    Converts a class-name or type object of `type <-kind: class` into a
    class object.  If a class does not exist and the argument is a name, the
    `@pce ->exception: undefined_class` with context argument the name of
    the required class is generated.  This mechanism is used by the
    autoloader (pce_autoload/2).

    @see pce->exception
    @see class<-instance

- class<-send_method: name -> behaviour

- class<-get_method: name -> behaviour
    Resolve a selector name to its implementation.  The returned value is
    either a method object or a variable.   These methods operate at the
    class level.  The methods `object <-get_method` and `object <-send_method`
    operate at the object level and take care of object-level
    defined behaviour as well as delegation.

    See also `object ->has_get_method` and `object <-get_method`.

- class<-instance: argument=unchecked ... -> object
    Create an instance of this class.  Creating an instance entails the
    following steps:

    - Memory is allocated for the instance.  All slots are
    	  filled with `variable<-alloc_value` of the
    	  corresponding variable object.

    - If it concerns a global object, a named reference is created.

    - if the class has instance variables with non-constant
    	  initial value or a function as initial value (see
    	  `variable -initial_value`), execute these funcions and
    	  assign the slot values.

    - The PCE virtual machine invokes `object ->initialise` to the
    	new object with the arguments given to this method.

    - If `object ->initialise` fails, the exception
    	 `initialise_failed` is raised using `@pce ->exception`.
    	 The context parameter is the (incomplete) object
    	 reference.

    - If ->initialise succeeds, `Class <-instance` returns the new
    	  object.

    NOTE:	`@pce <-instance: Class, any ...` is an alternative for creating
    	objects.  This gate has two advantages: type conversion allows
    	you to specify the class-name rather than the class, and -if the
    	class does not yet exist-, an exception is raised allowing the
    	class autoloading to create it.  See also class create
    	and new/2

    @see class<-convert
    @see pce->exception
    @see object->initialise
    @see variable-initial_value
    @see pce<-instance
    @see class-created_messages

- class<-instance_variable: name|int -> variable
    Resolve the variable object from the named instance variable or the
    offset (1-based) of the variable.   See also <-instance_variables,
    <-send_method or <-get_method.

- class<-lookup: name=name, super=[class] -> class
    Lookup the name of the class in the hash_table object @classes.  If
    `super` is specified, <-lookup verifies the super and generates an
    error message on a mismatch.   This method ensures that

    	?- get(class(point), instance, 4, 5, P).

    Actually returns an instance of the existing class point
    instead of returning an instance of a new class named point.

- class<-no_freed: sub_too=[bool] -> int
    How many instances of this class were freed.  If `subtoo` is
    @on the freed instances of <-sub_classes are added.  See
    also <-no_created.

- class<-super_class_name: -> name
    *Inherits description from*: class-sub_classes

