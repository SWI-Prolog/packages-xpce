# class object {#class-object}

Class object is the root of the class hierarchy.  It defines
common behaviour, mainly to do with object management:

| Creating objects       | `->initialise`                  |
| Destroying objects     | `->free`, `->done`              |
| Cloning objects        | `<-clone`                       |
| Saving objects on file | `->save_in_file`                |
| Adding attributes      | `->attribute`                   |
| Programming            | `->send_method`, `->get_method` |

Any class in PCE must be a sub-class of class object and
therefore any object listens to the methods defined on this
class.

It is allowed to create instances of class object and extend the instance
using object-level programming to realise a single object meeting some
specification.  See:

| `->attribute`   | Define object-level attributes |
| `->send_method` | Add object-level `send` method |
| `->get_method`  | Add object-level `get` method  |

@see class interceptor
@see topic Object-level Programming


## Send methods {#class-object-send}

- object->_check: recursive=[bool]
    Validate the type of the instance variables.  If the argument is @on or
    @default, validation continues recursively over instance variables that
    contain objects.  This method is protected against loops in the
    data-structures.

    Normally invoked through the Prolog predicate checkpce/0.

    **Diagnostics**: See related error objects

    @see !freed_value_value
    @see !freed_key_value
    @see !freed_element_value
    @see !freed_cell_value
    @see !freed_slot_value
    @see !bad_slot_value
    @see !creating
    @see !no_variable
    @see !checked_objects
    @see object->for_slot_reference
    @see class type

- object->_free
    *Inherits description from*: object<-_class

    @see class ?
    @see object->free

- object->_inspect: bool
    *Inherits description from*: object<-_class

    @see class ?

- object->_instance_of: class
    *Inherits description from*: object<-_class

- object->delete_hyper: hyper
    Detach a hyper from an object.

- object->attach_hyper: hyper, object
    Called from class hyper as part of `hyper ->initialise` and `hyper ->unlink`.
    These methods should _never_ be called directly by the user.  It
    is however possible to redefine these methods to intercept the
    creation and destruction of hyper-links.  A redefinition should
    _always_ call this method.

    @see ->delete_hypers.

- object->attribute: attribute|name, value=[any]
    Attach an instance variable to an object rather than to a class.  If an
    attribute with this name is already present, the value of the original
    attribute is replaced.

    An error is raised if the class already defines an instance variable
    with the same name.

    @see graphical->popup
    @see class sheet
    @see object<-all_attributes
    @see attribute<-convert
    @see class attribute
    @see object-interceptor

- object->convert_loaded_object: old_version=int, current_version=int
    Called by `file <-object` if the current save version is not the same as
    save version when the file was loaded.  The two arguments are the old
    and the new save versions.  See `pce <-save_version`.

    @see object->initialise_new_slot

- object->delete_attribute: name|attribute
    Delete (named) attribute that has previously be attached using
    ->attribute.  Fails silently if the attribute is not present.

    **Diagnostics**: Fails (silently) if the attribute is not defined.

    @see object->attribute

- object->delete_hypers: name=[name], condition=[code]
    Delete all hypers matching the name and condition.  See
    <-find_hyper for the interpretation of the name and condition
    arguments.

- object->done
    This method is used to help the PCE garbage collector and should
    normally be invoked after the program is done with the answer of a get
    operation and wants to be sure that the returned object is deleted when
    no longer referenced:

	.....
	get(Area, size, Size),
	....
	send(Size, done).

    The receiving object will be deleted from the object base as with ->free
    if the receiver has no <-references, <-lock_object is @off and
    <-protect is @off.

    In the example above ->done is equivalent to ->free as the size object
    returned by `area <-size` is created by this method (provided the ....
    computation does not lock, protect or make the size an attribute of some
    other object.  In the example below, using ->free will corrupt PCE's
    database:

	....
	get(Graphical, area, Area),
	....
	free(Area).		% ==> ERROR!!!

    The area object will be deleted and the slot `graphical <-area` will be
    pointing in the dark (checkpce/0 will find such problems).

    Using ->done is often not necessary.  After a user-event has been
    processed all unreferenced and unlocked objects will be deleted from the
    object base automatically.  Unreferenced objects created during the
    execution of a user-defined method for a PCE class are also deleted
    automatically.  ->done is useful when a lot of garbage objects are
    created in a loop.

    @see object<-lock_object

- object->equal: to=any
    Succeeds if the argument has the same reference.  Note that this method
    is redefined by various classes.  Equality may also be tested using
    class ==.  See also ->same_reference.

    **Bugs**: The notion of equality is not very clear in PCE.

    @see class \==
    @see class ==

- object->error: error=error, context=unchecked ...
    Raise an error (exception) on this object.  Error (normally specified by
    its `error <-id` and converted by `error <-convert`) is the error to be
    raised.  The remaining context arguments provide context information
    about this error.  Performs the following steps:

    - if `error <-kind` equals `ignored`, silently fail.

    - assigns `@pce <-last_error`.

    - If the error is not catched (`@pce ->catched`), it invokes
	`error ->display` on the error object.

    See also class error and ->report.

    **Diagnostics**: Diagnostics depend on the error object invoked.

    @see visual->report
    @see pce->catched
    @see error-id

- object->for_slot_reference: action=code, recursive=[bool]
    This method is intended to help writing analysis programs for the
    database.  It scans all slot-references of the receiving object.  When
    the argument is @on or @default, it will continue recursively over the
    slot-fillers.

    For each slot-reference, code is executed with the following bindings:

    | `@arg1` | The instance           |
    | `@arg2` | Type of slot reference |
    | `@arg3` | The slot               |
    | `@arg4` | The value of the slot  |

    The _Type of slot reference_ is one of:

    - slot
	For normal objects.  @arg3 is bound to the name of this variable
    - cell
	For chains.  @arg3 is bound to the 1-based index of the cell
    - element
	For vectors. @arg3 is bound to the index
    - key
	For hash_tables.  @arg3 is bound to the key; @arg4 to the
	related value.

    @see object->_check

- object->free
    Remove an object from the PCE object base.  This behaviour is the normal
    way to get rid of existing objects.  ->free performs the following steps:

    - Invoke `object ->unlink`.  This allows a class to
	unlink the object from the environment.

    - Removes all attribute references by setting all slot values
	that contain an object to @nil.

    - Removes the global name association if this existed.

    - Reclaim the memory if the object has no <-references.
	Otherwise reclaim is delayed.  See `@pce <-deferred_unalloced`.

    `Object ->unlink` is often defined on objects that need to inform
    some other object that they are destroyed.  For example, graphical
    objects inform their device to allow the device to remove the object
    from the display and update their book-keeping.

    The method `object ->unlinking` is provided as a test whether or
    not ->free is in progress on the receiver.

    See also `visual ->destroy` to destroy entire hierarchies of (visual)
    objects.

    **Diagnostics**: Fails silently if <->protect equals @on.

    **Bugs**:
    Window ->free should just delete the window from it's <-frame.   In new
    code, please delete frames using `frame ->free`.

    @see object->unlink
    @see visual->destroy
    @see topic Removing Objects
    @see object->protect
    @see object->destroy
    @see object->_free

- object->get_method: get_method|chain
    Add a get-method to this individual object.  Object-level defined
    get-methods overrule Class-defined get-methods.  An object level method
    may refer to its corresponding class level method using ->get_class.

    @see object<-get_class
    @see class method_group
    @see class method
    @see object<-slot
    @see object<-all_get_methods
    @see object->send_method
    @see class->get_method
    @see object-interceptor

- object->has_get_method: selector=name
    Test if object defines get_method.

- object->has_send_method: selector=name
    Test whether object implements the specified send/get behaviour.  This
    test is equivalent to testing whether {<-get_method, <-send_method}
    returns a value but avoids the creation of a tuple.  See also
    `class <-send_method` and `class <-get_method`.

- object->is_off: name
    Test if Obj <-name returns @off.

- object->is_on: name
    Test if Obj <-name returns @on.

- object->not_has_value: name, any
    Test if Obj <-name not-equal 2nd argument.

- object->has_value: name, any
    Execute the get operation <-Name and compare the result to the
    argument.  Succeed if they are equal (have same reference), fail
    otherwise.

    This method is intended to test on attribute values.  Similar and more
    powerful results can be obtained using class ==..  It's usage is
    to be preferred.  The following code objects and Prolog fragments all
    test whether the `point <-x` of the point object @p equals 3:

    Prolog:

	get(@p, x, 3)              % Uses Prolog unification
	send(@p, has_value, x, 3)

    Code fragments:

	message(@p, has_value, x, 3)
	@p?x == 3

    ->not_has_value is equivalent to ->has_value, but returns the inverse
    result.  ->is_on is equivalent to ->has_value: @on and ->is_off is
    equivalent to ->has_value: @off.

    @see object->is_on
    @see object->is_off
    @see class \==
    @see object->not_has_value
    @see class ==

- object->initialise
    No-operation (just succeeds).  Implemented to allow any class perform a

	send_super(Self, initialise).

    Any class should have a method ->initialise.  This method is invoked
    from PCE's virtual machine to initialise an object from the parameters
    given to the object creation operation (new/2. class create,
    `class <-instance`.

    The creation of objects is discussed in detail with `class <-instance`.

    @see class<-instance
    @see topic Creating Objects

- object->initialise_new_slot: new=variable
    Part of saved-object conversion (see `file <-object`).  Called when
    -while loading an instance from a file-, the current definition of the
    class has a new instance variable (compared to when the instance was
    saved to file using ->save_in_file).  The argument is the new variable
    object from the class.  May be used to initialise the new slot to some
    sensible value.

    @see object->convert_loaded_object

- object->inspect: bool
    If @on *and* the class of the object has one of the change trapping
    messages attached to it, changes to the object are forwarded (to the
    application) via these change messages.  If @off, these messages have no
    effect.

    @see class-changed_messages
    @see object<-inspect
    @see topic Changes
    @see class->freed_message
    @see class->created_message
    @see class->changed_message

- object->instance_of: class
    Succeeds if the object is an instance of the argument class or one of
    its subclasses.  Note that `class <-convert` converts class-names to
    class objects:

	send(Obj, instance_of, graphical)

    is the advised way to verify that _Obj_ is a graphical.

    The method `pce <-convert` may be used to combine testing with possible
    type conversion.  See also `type ->validate` and `type <-check`.  If one
    wants to test whether an object is a graphical object, a node
    object or @nil, the following two test are equivalent:

	(   send(Obj, instance_of, graphical)
	;   send(Obj, instance_of, node)
	;   Obj == @nil
	)

    or

	send(type('graphical|node*'), validate, Obj)

    @see pce<-convert

- object->lock_object: bool
    *Inherits description from*: object<-lock_object

    @see object<-lock_object
    @see object<-_flags
    @see object->protect

- object->name_reference: name*
    Give/change the global named reference associated with the
    object.   See also <-object_reference.

    **Diagnostics**:

    - Name reference <Name> already in use
        The requested new name-reference is already in use.

    **Bugs**:
    Unlike with the predicate new/2, no exception is raised if the object
    already exists.

    @see pce->rename_reference
    @see object<-name_reference
    @see topic Global references

- object->protect
    Protect an object against destruction with ->free or one of the
    other objects destruction methods.  The global PCE objects @pce,
    @prolog, etc.  are protected this way.

    There is no way to unprotect protected objects or to destroy
    them.

    @see visual->destroy
    @see object<-protect
    @see object<-_flags
    @see topic Removing Objects
    @see object->lock_object
    @see object->free
    @see object->destroy

- object->report: kind={status,inform,progress,done,warning,error,fatal}, format=[char_array], argument=any ...
    Report a message related to the specified object. This method locates
    the visual object from which the user invoked the current command using
    `@event <-receiver` and invokes `visual ->report` on this object.

    If @event is unbound the error is caused by a query from the
    host-language interaction window and PCE thus prints the message
    using `@pce ->format`.

    Some classes redefine this method because they *know* they are related
    to some visual object.   See also <-report_to.

    @see visual->report

- object->same_class: object
    Test if two objects belong to the same class.  The following two
    executable objects are the same:

	Obj1?class == Obj2?class.

    and

	message(Obj1, same_class, Obj2)

    @see object<-class
    @see class->is_a

- object->same_reference: to=any
    Test if i'm the same object as the argument.  This method
    verifies both objects have the same identity.  The method
    `object ->equal` performs the same test, but is refined by
    various classes to test for `same properties`.  See -for
    example- `point ->equal`.

- object->save_in_file: file
    Save an object and all objects that can be reached from it to a file.
    This method takes care of cyclic structures.  The save-format is binary.
    The object may be reloaded in this or in another PCE process using
    `file <-object`.  When the object is reloaded in this PCE process, the
    combined functionality is similar to `object <-clone`.

    When the data, manipulated by the application is stored in PCE, the
    normal way is to attach all this information to a single object
    (normally a hash_table, sheet or chain) and save this object to a file.
    All attached objects (i.e.  all data needed by the application to save
    its `persistent` data) are then saved in the file.

    PCE's save and load functionality has a limited way to deal with
    versions.  The conversion activities are described with `file <-object`.

    **Diagnostics**:

    - Do not know how to save <object>
        Object cannot be saved because it contains alien references and
        no defined behaviour to take care of them.  This is the case
        with most objects that refer directly to the X-window system
        (e.g. windows, cursors, etc.).

    Diagnostics related to opening, writing and closing a file are
    applicable as well.

    **Bugs**:

    - User-defined classes currently cannot define private save and
      load behaviour.

    @see !cannot_save_object
    @see file->check_object
    @see object<-clone
    @see file<-object
    @see class-save_style
    @see topic Saving to File

- object->send_class: selector=name, argument=unchecked ...
    Invoke send behaviour of the class, bypassing object-level send_method
    objects attached to this object.  See  <-get_class for details..

    @see object->send_method

- object->send_hyper: hyper_name=[name], selector=name, argument=unchecked ...
    Perform a broadcast send-operation to all (named) <-hypered objects.
    Similar to <-get_hyper, but does not stop if the method is received
    successfully.   Succeeds if there was at least one hypered object
    accepting the message, fails otherwise.

- object->send_method: send_method|chain
    Add a send-method to this individual object.  Object-level defined
    send-methods overrule Class-defined send-methods.  An object level
    method may refer to its corresponding class level method using
    ->send_class.

    @see object->send_class
    @see class method_group
    @see class method
    @see object<-all_send_methods
    @see object->slot
    @see class->send_method
    @see object->get_method
    @see object-interceptor

- object->send_sub: selector=name, argument=unchecked ...
    Send to @receiver, using behaviour at the level of the class @receiver
    is an instance of.  See also <-get_sub.

- object->send_super: selector=name, argument=unchecked ...
    Invoke behaviour of the super-class.  This behaviour is intended to be
    used only for using behaviour of the super-class when defining new
    classes.

    @see object<-get_super
    @see topic Methods
    @see topic Class-level Programming

- object->send_super_vector: unchecked ...
    Combines ->send_super and ->send_vector, creating an argument vector
    from the leading arguments and vector and invoking behaviour defined at
    the super-class.   See ->send_super and ->send_vector for details.

    @see object->send_super
    @see object->send_vector

- object->send_vector: unchecked ...
    `Object ->send_vector` supports the implementation of methods with
    variable argument list.  Its format is:

	unchecked..., Vector, [shift]

    First, a list is created containing the following arguments:

    - The objects from `unchecked...`
    - The elements of the vector, with the first [shift] elements
	  skipped.

    Next, the first element of this list is used as the selector and the
    remaining arguments as the arguments to the message. Examples:

    - send(@prolog, send_vector, vector(write_ln, 'Hello World')).
    - send(@prolog, send_vector, write_ln, vector('Hello World')).

    **Defaults**: Shift defaults to 0.

    **Diagnostics**: Fails silently if the selector cannot be created,

    @see !bad_vector_usage
    @see code->forward_vector
    @see object->send_super_vector
    @see object<-get_vector
    @see topic Delegation

- object->slot: name|int, unchecked
    Set the value of a (class-defined) slot without activating the
    corresponding method.

    The slot specification (first argument) is either the slot-name, or the
    offset in the object (1-based).  The latter possibility is for very
    specific (internal) use.

    This method should only be used from a send-method that has the same
    name (selector) as the slot and serves as a `wrapper` around the slot to
    deal with side-effects:

	:- pce_begin_class(person, object).

	variable(date_of_birth,	date, get, "When person was born").
	...

	date_of_birth(Person, Date:date) :->
		(   send(Date, after, new(date))
		->  send(Person, report, error,
			     'Cannot be born in the future!'),
			fail
		;   send(Person, slot, date_of_birth, Date)
		).

    **Bugs**:
    This message should also apply to object-level defined attributes (see
    `object ->attribute`.

    @see class variable
    @see object<-slot
    @see class->send_method
    @see object->send_method

- object->unlink
    The method ->unlink is called by `object ->free`.  Its task is to unlink
    the instance from its environment.  The ->unlink method may not be
    called directly by the user.  The user should provide an unlink method
    when writing a user-defined class that requires specialised unlink
    behaviour.

    Some examples:

    - Graphicals notify their device when they are destroyed, so the
	device can delete them from `device <-graphicals` and update
	the display.

    - A window will destroy the associated Xt-widget realising the
	window in X.

    - A char_array will unalloc the associated C-data-structure
	representing the text.

    NOTE: Unlink should *never* be called directly by the user.  Any
    user-defined ->unlink method should invoke the method of the
    super-class (see `object ->send_super`).

    See also ->free, ->done, `visual ->destroy` and ->unlinking

    @see object->free

- object->unlinking
    Succeeds if the object is in currently being ->unlink'ed.
    Intended as a test in ->unlink methods to avoid unnecessary
    computation.


## Get methods {#class-object-get}

- object<-_arg: int -> unchecked
    The nth argument (1-based) of the term description.  Unless overruled at
    the object level, this is equivalent to:

	get(Obj, ?(Obj?class?term_names, element, N), Arg).

    This behaviour is intended to remain reserved for system usage.  Do not
    rely on its definition.

    @see chain<-_arg
    @see object<-functor
    @see object<-_arity
    @see class-term_names
    @see class ?
    @see topic Object -> Term

- object<-_arity: -> int
    Number or arguments to the term description.  Unless overruled, this is
    the size of the term_names vector of the associated class.

    @see chain<-_arity
    @see object<-functor
    @see object<-_arg
    @see class ?
    @see topic Object -> Term

- object<-_class_name: -> name

- object<-_flags: -> name

- object<-_inspect: -> bool

- object<-_man_id: -> name

- object<-_references: -> int

- object<-_slot: name|int -> unchecked

- object<-_class: -> class
    These methods starting with an '_' (underscore) are defined both on
    class function and class object.  Their behaviour is equivalent to the
    counterpart with the leading underscore deleted for normal objects.
    As these methods are explicitly defined on class function however,
    invoking one of these methods to a function will cause the function
    itself to handle the message instead of the evaluation.  Example:

	?- new(O, @display?size),
	   get(O, '_class_name', C1),
	   get(O, class_name, C2).

	C1 = ?
	C2 = size

    **Bugs**:
    It is probably more elegant to introduce a separate send- and get-
    operation that does not evaluate the receiver.

    @see object<-class
    @see class ?

- object<-all_constraints: create=[bool] -> chain

- object<-all_get_methods: create=[bool] -> chain

- object<-all_hypers: create=[bool] -> chain

- object<-all_send_methods: create=[bool] -> chain

- object<-all_attributes: create=[bool] -> chain
    Return a chain holding all the object-level extensions to the object of
    the given type.   If `create` equals @on, this call will always succeed,
    possibly returning an empty chain.  Otherwise the call might fail.

    The following extensions are defined:

    | `->attribute`   | Object-level instance variable              |
    | `constraint`    | Binary constraint between two objects       |
    | `hyper`         | Binary relation (two-way attribute)         |
    | `->recogniser`  | Handler for event objects (class graphical) |
    | `->send_method` | Object-level method (send)                  |
    | `->get_method`  | Object-level method (get)                   |

    See also class constraint and class hyper.

    @see object->attribute
    @see object-interceptor

- object<-attribute: name -> unchecked
    Get the value of an attribute associated to the object using
    `object ->attribute`.  The attribute may also me requested using
    a plain get operation:

	?- new(@o, object),
	   send(@o, attribute, gnus, 4).

	?- get(@o, attribute, gnus, G).
	?- get(@o, gnus, G).

    The two are equivalent, except that the latter gives an error if no such
    attribute is defined.

- object<-class: -> class
    Instance of class class to which this object belongs.  The normal way to
    test that an object is of some particular class is either ->instance_of
    or <-class_name.  In most situations the first is to be preferred.

    @see object<-class_name
    @see object<-_class
    @see object->same_class

- object<-class_name: -> name
    Name of the class the object belongs to.  Shorthand for:

	get(Object?class, name, ClassName).

    @see object<-class
    @see object<-_class_name

- object<-class_variable_value: name -> any
    Get value of associated class_variable object.  See also
    class_variable/4.  Note that, in the absence of a method or
    variable with the name name, `Object <-Name` is equivalent.

- object<-clone: -> object
    Object <-clone creates a clone of an object.  The semantics of cloning
    objects in general are difficult to define.  Which related objects are
    to be cloned and which are to be shared between the clone and the
    original object?

    The approach taken by PCE is not particularly neat.  We do not recommend
    heavy use of this facility.

    Object are cloned `recursively`.  This implies all objects that
    can be reached from this object are cloned as well.  The
    algorithm deals correctly with cyclic structures.

    While executing a recursive clone, two flags may influence the
    result:

    | `variable <-clone_style` | Defines the type of relation |
    | `class <-clone_style`    | Defines the type of object   |

    Please consult the documentation of these for details.

    **Bugs**:
    The semantics are ill defined.  Be *very* carefull with `plain` cloning
    and study this description carefully before going ahead.  When in doubt,
    use the `inspector` tool from the main menu to find out about the
    result.

    @see class-clone_style
    @see object->save_in_file

- object<-convert: int|char_array -> object
    The method <-convert is called by `type <-translate`, part of the
    generic type-checking and type conversion system.  It is not a normal
    method not the *receiver*, but the *result* is an instance of the class
    on which the method is defined.  The receiver is normally the class.

    The task of the <-convert method is to translate the argument into an
    instance of the requested type.

    For class object itself no translation is necessary as anything in PCE
    already is an instance of class object.  This method will therefore
    never be called directly by the PCE type conversion system.  This method
    translates a text (char_array object) of the form `@<name>` into an
    object if <name> is a valid object reference.  This method is useful
    for defining other conversion functions.

- object<-create_context: condition=[code] -> object
    If the receiver is executing ->initialise, scan the goal stack
    searching for a receiver higher in the goal stack for which
    condition succeeds.  Condition is executed with the following
    arguments:

    | `@receiver` | receiving object                         |
    | `@arg1`     | receiver of message of parent goal       |
    | `@arg2`     | implementation of message of parent goal |

    This method is intended to find the context in which an object
    (often a graphical) is created, allowing it to query the context
    for additional information.  See also `visual <-contained_in`.


- object<-find_all_send_methods: condition=[code] -> chain
    Compute a chain with all behaviour objects that may be used to invoke
    send behaviour on this object.  It will analyse:

    - Attributes
    - Class' variables and send methods
    - Delegation

    Overruled behaviour is automatically deleted from the chain.  The
    additional conditions is executed binding @arg1 to the implementation
    object.

- object<-find_hyper: hyper_name=[name], test=[code] -> hyper
    Find hyper-object from specifications.  `hyper_name` is the name
    of the hyper, seen from the receiver of this message.  If hyper_name
    is @default, all hypers are considered.  `test` is a code
    object.  If it is not @default, it is executed using the
    following arguments:

    | `@arg1` | Receiver of this message          |
    | `@arg2` | Hyper object                      |
    | `@arg3` | Object at other end of the hyper. |

    See also <-hypered.

- object<-functor: -> name
    Functor of the term description of the object.  Unless overruled, this
    is equivalent to <-class_name.

    @see topic Object -> Term
    @see class-term_functor
    @see object<-_arity
    @see object<-_arg

- object<-get_class: selector=name, argument=unchecked ... -> unchecked
    Invoke get_method on object, bypassing possible object-level
    get_methods associated with ->get_method.  This method serves a similar
    purpose as ->get_super when using class-level programming.

    @see object->get_method

- object<-get_hyper: hyper_name=[name], selector=name, argument=unchecked ... -> unchecked
    Perform a `broadcast` get operation on objects <-hypered to this object.
    If a hyper_name is given only hypers with this name are considered.
    Otherwise all hypers are considered.  As soon as one of the hypered
    objects returns a value, this method returns with this value.  Otherwise
    the method fails.  Example:

	?- new(@o, object),
	   new(_, hyper(@o, bitmap('pce.bm'), bitmap)),
	   new(_, hyper(@o, text(hello), text)).

	?- pce_catch_error(no_behaviour,
					   get(@o, get_hyper,
						   @default, string, S).
	S = hello

	?- pce_catch_error(no_behaviour,
					   get(@o, get_hyper,
						   @default, pixel, 3, 5, P).
	P = @off

    The first query is answered by the text object, while the latter is
    answered by the bitmap object (delegated to the `bitmap <-image`).  Note
    the use of pce_catch_error/2 to avoid an error while broadcasting to
    objects that do not understand the method.

- object<-send_method: name -> tuple

- object<-get_method: name -> tuple
    This method locates the implementation for the specified selector for
    the specified (send/get) type of operation.  It will try (in this
    order):

    - attribute objects (see `object ->attribute`).
    - `Class <-send_method`
    - Delegation.  See `class <-delegate`

    The `tuple <-first` contains the receiver, which is either the object
    itself or the object the message will be delegated to.  The
    `tuple <-second` contains the behaviour object that implements
    the method.

    Examples:

	?- new(@p, point),
	   get(@p, send_method, mirror, tuple(Object, Impl)).

	Object = @p/point
	Impl = @632241/send_method

	?- new(@v, view),
	   get(@v, send_method, append, tuple(Object, Impl)).

	Object = @833889/editor
	Impl = @749374/send_method

    The second example indicates that, when a message ->append is sent to
    @v it will be delegated to the editor @833889.

    See also ->has_get_method, ->has_send_method and <-all_send_methods.

- object<-get_sub: selector=name, argument=unchecked ... -> unchecked
    Invoke message on @receiver, using the definition from the object's own
    class instead of the current class.  See `class ->get_method` for
    further details.

    **Bugs**:
    Actually, a normal get operation executed in a method already performs a
    <-get_sub.  Reserved fro future extension.

- object<-get_super: selector=name, argument=unchecked ... -> unchecked
    Invoke behaviour of the super-class.  This behaviour is intended to be
    used only for using behaviour of the superclass when defining new
    classes.  This method is only valid in the context of class-defined
    methods.  The receiver should be the same object as the receiver of the
    executing method (i.e. <-get_super is always to `it self`).

    See also <-slot.

    @see topic Methods
    @see topic Class-level Programming
    @see object->send_super

- object<-get_vector: unchecked ... -> unchecked
    Invokes a get-method on the object itself.  The first element of the
    vector is used as a selector, the subsequent ones are used as arguments
    to the behaviour.  E.g.

	get(@prolog, get_vector, vector(plus, 1, 1), X).

    Is a very complicated way to add 1 and 1.

    This behaviour is intended to deal with delegating messages trapped via
    the ->get_catch_all behaviour.

    @see object->send_vector

- object<-hypered: hyper_name=[name], test=[code] -> object
    Find a hyper-related object.  Name is the name of the hyper (seen from
    the side of the receiver).  Test is an optionally additional test.  If
    present, this test is executed using the following arguments:

    | `@arg1` | This object                            |
    | `@arg2` | The hyper object                       |
    | `@arg3` | The object at the other end of the hyper |

    The first matching object is returned.  See also <-all_hypers.

- object<-inspect: -> bool
    *Inherits description from*: object->inspect

    @see object->inspect
    @see object<-_inspect

- object<-lock_object: -> bool
    When @on, PCE's garbage collector will not remove this object, even if
    it has no references.  Globally named objects are by default locked, as
    are most reusable objects for which the class maintains a table:
    types, names, modifiers, etc.

    A locked object is destroyed by ->free.  To protect an object against
    ->free, use ->protect.

    When @off, the lock is removed from the object.  If the object
    has no references, it will be ->free'd.  If this is not
    desirable, use <-unlock.

    @see object->protect
    @see object->done
    @see object->lock_object

- object<-object_reference: -> name|int
    Reference name of the object (see ->name_reference) or integer
    reference for objects that have no named reference.  See also
    `@pce <-object_from_reference`.

    @see pce<-object_from_reference

- object<-print_name: -> text=char_array
    The method <-print_name is intended to convert an object into a textual
    representation.   It serves two purposes:

    - Implement the %N special sequence in `string ->format`
    - Realise `text_item <-print_name_of_value`.

    This method is redefined by various subclasses.  `Object <-print_name`
    itself performs the following steps:

    - If the object accepts <-name and the return value can be
	converted into a char_array object, return this converted value.

    - Otherwise return a new string holding the internal debugger
	representation of objects.

- object<-protect: -> bool
    *Inherits description from*: object->protect

    @see object->protect

- object<-references: -> int
    PCE uses reference counts for the incremental garbage collector.  The
    number of references is the number of slot relations towards this
    object.

    Note that PCE does not know which objects have references to an object;
    slot-relations are uni-directional.

    @see tool Inspector
    @see topic Garbage collection
    @see object<-_references

- object<-report_to: -> object
    Object for ->report.  The default for non-visual objects
    is the receiver of the current event (@event).   See also
    `visual <-report_to`.

- object<-self: -> object
    Returns the object itself.  Hardly ever necessary within the
    architecture of PCE.

- object<-slot: name|int -> unchecked
    Get the value of the named slot, bypassing protection.  When given an
    integer, get the value of the nth-slot (counting from 1).

    This method is used in the implementation of get_method objects if the
    get method wants to access a slot with the same name without interaction
    from the method.  See also `class ->get_method`.

    @see class variable
    @see class->get_method
    @see object->get_method
    @see object->slot
    @see object<-_slot

- object<-storage_reference: -> any
    This method is used by `object ->save_in_file` to handle shared
    object resources (colours, fonts, etc.).  If this method is
    executed successfully, ->save_in_file stores the classname of
    the the object and the returned object.

    `file <-object` loads the classname, converts the classname to
    a type object, loads the returned `reference object` and
    attempts `type <-check` to produce an object of the requested
    type.

    See `colour <-storage_reference` for an example.

- object<-unlock: -> unchecked
    Unlock object and return <-self.  If, after unlocking, the
    object has no references, it will be pushed on the `anser
    stack', making it visible to the incremental garbage collector.

    In combination with ->lock_object, this method may be used
    to disconnect an object from its environment, handing it back
    to the incremental garbage collector.  An example can be found
    in `emacs_window<-prompt`, a part of PceEmacs.

    Assume we have a chain object holding a point and we want to
    delete the object from the chain without deleting the point.
    Direct usage of `chain->delete` may destroy the point.  The
    following code avoids this:

	send(Point, lock_object, @on),
	send(Chain, delete, Point),
	get(Point, unlock, _).

