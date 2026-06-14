# class variable {#class-variable}

A variable object describes of an instance-variable (slot, attribute) of
an object.  Variables are managed by classes and used (among other
things) to access slots in objects.

A variable has a <-name, which must be unique for this class and all
super-classes of this class.  A variable has a <-type, which defines the
type of value that can be stored in the corresponding slot.  Finally, a
variable defines <-access: the behaviour that is associated with it.

@see class->instance_variable
@see class type
@see topic Specification
@see object<-slot
@see object->slot
@see tool ClassBrowser


## Instance variables {#class-variable-instvars}

- variable<-access: {none,send,get,both}
    The <-access attribute of a variable defines the side-effect-free
    methods that are associated:

    	| none | No methods, only access via `object <->slot`   |
    	| get  | Get access, only write through `object ->slot` |
    	| send | Send access, only read through `object <-slot` |
    	| both | Both read and write access                     |

    If a variable requires side-effects for writing (the most common case),
    it should be declared with <-access: read and a send_method with the
    same name should be defined on the class.  This method may write
    the variable using `object ->slot`.

- variable-group: [name]
    Conceptual group of variable.  Used by the online manual and to
    structure the reference manual.

- variable<-offset: int
    Offset in instance structure.  See also `slot <-object` and `class
    <-instance_variables'.

- variable<->summary: string*
    Summary documentation.  Used by the reference manual (online and paper).

- variable<-type: type
    Type of object that should be used to fill the instance variable.
    Checked when an attempt is made to write the variable.  The method
    `object ->_check` may be used to validate the object base.  Part of
    its job is checking the consistency of the values of instance variables.

    See also ->send, `type <-check` and `type <-convert`.


## Send methods {#class-variable-send}

- variable->clone_style: {recursive,reference,reference_chain,value,alien,nil}
    Describes the semantics with respect to cloning instance (`object <-clone`)
    of the instance-variable relation:

    - recursive
    	When cloning recursively, proceed by cloning the value of
    	this variable recursively too (unless overruled by `class <-clone_style`
    	of the value in the slot).

    - reference
    	Copy the object reference (e.i. do not clone the value of the
    	slot).  If however, the value is cloned as an other part of the
    	cloning process, make the new field point to the copy.

    - reference_chain
    	The variable contains a chain with references to other
    	objects.  In the clone, a new chain will be installed,
    	holding all cloned objects.  Used by
    	`text_buffer <-editors`.

    - value
    	Just copy the value.

    - alien
    	The slot contains alien data.  32 bits copy is made for it
    	without interpreting the data.  Many classes define
    	`class -clone_function` for properly cloning alien data.

    - nil
    	Replace the value in the copy with @nil.  If the value gets
    	cloned as an other part of the cloning process, make the new
    	field point to the copy.  This style is used by
    	`graphical <-device`.  If a graphical is cloned it will
    	be disconnected from it's device.  If a device is cloned
    	however, all graphicals part of it will be cloned and
    	displayed on the clone.

- variable->send_access
    Test if variable has write access.

- variable->get_access
    Test if the variable has the indicated access.  Exploits <-access.

- variable->init_function: any*
    Function to initialise the variable.  See ->initial_value and
    ->alloc_value for details.

- variable->initial_value: any|function
    Initial value for this variable.  This is either a plain value
    or a function object.  It is normally passed as the 7-th
    argument of ->initialise.

    Please note that using a normal object (e.g. size(20,5)) causes
    the same size instances to be shared by all individuals of this
    class.  In most cases one should use create(size, 20,5) to force
    the creation of a new instance.  See also class create.  The
    Prolog class compiler supports this through the following
    construct:

    	variable(size, size:=new(size(20,5)), get).

    If the argument is a constant, name or int, the ->alloc_value
    is set.  Otherwise special precautions are required in
    `class <-instance` and the ->init_function is set.

- variable->initialise: name=name, type=[type], access=[{none,send,get,both}], summary=[string]*, group=[name], initial_value=[any|function]
    Initialise an variable.  A variable object normally represents an
    instance variable of a class (see `class ->instance_variable`.
    The arguments are:

    - name (<-name)
    	Name of the instance variable.  Instance variable names are
    	local to a class (though instance variables of super-classes are
    	inherited.

    - type (<-type)
    	The type of object that may be used to fill the instance
    	variable (see `object <->slot`).

    - access (<-access)
    	Defines implicit side-effect free methods associated with this
    	variable.  See ->access.

    - summary (<-summary)
    	Summary string used by the online documentation

    - group (<-group)
    	Name of the functional group this variable belongs too.

- variable->save_style: {normal,nil}
    Defines how the value of this instance variable is saved by `object
    ->save_in_file':

    	| normal | Save the value according to `class <-save_style` |
    	| nil    | Save the value as if it was @nil                 |

    See also `class <-save_style`, `object ->save_in_file` and `file
    <-object'.

- variable->send: receiver=object, value=unchecked
    *Inherits description from*: variable<-get

## Get methods {#class-variable-get}

- variable<-alloc_value: -> unchecked
    Value used to allocate the object.  This normally @nil for XPCE
    slots and NULL for alien slots.  It is set to @default for XPCE
    slots that have a type that accepts @default, but does not
    accept @nil.  E.g.  slots of type [int] are initialised to
    @default.  See also ->initial_value.

- variable<-argument_type: index=[int] -> type
    Type of the nth (1-based) argument.  If index is 1, returns <-type,
    otherwise fails.  For compatibility to `method <-argument_type`.

- variable<-clone_style: -> name
    *Inherits description from*: variable->clone_style

- variable<-get: object -> unchecked
    Execute the variable as a behaviour.  <-get requires the argument vector
    to be empty, ->send requires it to have exactly one element.  ->send
    `type <-check`es the argument.

    See also class behaviour, `object <-get_method` and `object
    <-send_method'.

- variable<-print_name: -> name
    Returns a name object of the form <class> <access><selector>.
    For example:

    	point <->x

- variable<-save_style: -> {normal,nil}
    Save style for this slot.  This value is used by `object
            ->save_in_file' to determine whether or not the value of this
            slot should be saved in the file.  The default value is
            `normal`, which implies the value is saved depending on
            `class <-save_style`.  If this attribute is `nil`, the value is
            saved as if the slot contains the value @nil.

