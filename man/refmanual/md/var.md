# class var {#class-var}

Class var describes a global variable.  It is a subclass of class
function, which implies that a variable is expanded to its value for all
methods that do not explicitly accept type function or var.

PCE global variables have global existence, but may have local binding.
The most well known var objects are @receiver, @event and  @arg1 ...
@arg10.

A variable has a <-_type to define legal values, a <-_name to identify
them (named var objects are used by class handle and class spatial) and
a <-_value.   Note that these names start with an underscore.  Thus,

	get(Var, '_name', Name)

yields the name of the var object, while

	get(Var, name, Name)

yields the name of the <-_value of the var object.

Values are assigned using ->assign.  Class assign defines an executable
object to assign variables.

## Using var objects explicitly {#class-var-using-var-objects-explicitly}

There are some common ways to exploit var objects in applications:

- As global changeable constants
	For example, consider an application that wants to log progress
	and events of its computation.  It might be desirable to send
	the log info sometimes to a file, sometimes to a view object or
	to the main window.   In this case one could declare a global
	var object @log_output and rebind it if the destination of the
	output has to be (temporary) changed.

- As context parameters
	A good example is @event, which provides access to the
	currently executing events.

- As local variables in code fragments
	In the two applications mentioned above, the var object with
	normally have a global (named) object reference.  When used
	as a local variable, the object reference is normally
	anonymous.  In the following example, _Chain_ is a chain
	of chains of graphical objects.  The task is to display
	the graphicals of each chain on a separate device and
	display all these devices on a (common) device.

		new(Dev, device),
		send(Chain, for_all,
			 and(assign(new(SubDev, var),
						?(@pce, instance, device)),
				 message(@arg1, for_all,
						 message(SubDev, display, @arg1)),
				 message(Dev, display, SubDev)))

@see class assign


## Instance variables {#class-var-instvars}

- var<-_global_value: any
    Initial and global value of the var object.  The value is restored to
    this value on recovery from a fatal error.

- var<-_name: name*
    Name of the var object.  All named variables are stored in the
    hash_table @variables.  <-convert uses this to convert names to var
    objects.

- var<->_type: type
    Allowed type for the current <-_value.  Actually not verified in the
    current implementation.

    **Defaults**: unchecked

- var-_value
    Current value of the variable.


## Send methods {#class-var-send}

- var->assign: value=any, scope=[{local,outer,global}]
    Assign a <-_value to the variable.  The scope of this assignment is
    determined by the scope argument:

    - local [default]
    	Only assign in the current `frame`.  A variable frame is pushed
    	by `code ->forward`, `send_method ->send` and `get_method <-get`
    	if the implementation of the method is a code object
    	(user-defined class).

    - outer
    	Assign in this *and* the environment above the current one.

    - global
    	Assign in all environments as well as to the <-_global_value.

    **Defaults**: The default scope is local.

- var->initialise: type=[type], name=[name], value=[any]
    Create a var object that may contain objects of the specified type.  A
    var object may have an optional name and initial value.

    **Defaults**:
    	# type:	unchecked
    - name:	@nil (unnamed, not stored in @variables)
    - value:	@default


## Get methods {#class-var-get}

- var<-convert: name -> var
    Converts name to var from @variables.

    In older versions of PCE @arg1, etc.  where special objects.  The `x`,
    `y`, etc.  variables appearing in formulas for class handle and class
    spatial where other special features.  In the current implementation
    class var takes care of all these cases.  To keep the `name-as-variable`
    as used in formulas compatible we had to introduce the name conversion
    mechanism.

