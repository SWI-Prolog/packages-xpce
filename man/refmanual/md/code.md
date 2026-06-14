# class code {#class-code}

Executable code in PCE is represented by PCE objects.  This approach
allows us to define new executable code for the PCE system from the host
language (Prolog) without extending the interface between PCE and
Prolog.

The super-class `code` is a super-class of everything that is executable
in PCE.  Code objects are normally attached to method objects, or to
objects to define the behaviour for handling some events.  For example,
a button object has a code object attached to it that determines the
behaviour of the button when it is depressed.  Finally, code objects are
used as conditions to the iteration and finding methods	`chain->for_all`,
`chain <-find_all`, etc.   Examples:

Attaching a code object as an action to an interface object:

	?- new(D, dialog('Hello World')),
	   send(D, append,
		    button(quit, message(D, destroy))),
	   send(D, open).

Using a code object to sort a chain of objects by name:

	send(Chain, sort,
		 ?(@arg1?name, compare, @arg2?name)).

Using a code object to define a method:

	send(class(person), send_method,
		 send_method(print, vector('on=object',
					 message(@arg1, format,
							 'Name:\t%s\nAge:\t%s\n',
							 @receiver?name,
							 @receiver?age))))

This example defines a method on a hypothetical class person called
`print` that formats the name and age of the person to a specified
object.  Note that XPCE/Prolog as well as XPCE/Lisp define an interface
which allows for defining methods in a programmer-friendly fashion.
See pce_begin_class/3.

Code objects are normally activated from the object they are attached
to.  There are two ways to activate a code object.  One that pushes
an variable frame (->forward) and one that uses the current variable
frame ->execute.

Code objects normally return either success or failure, with the
exception of the subclasses of class function, which -like get()-
returns a true value.

@see class function
@see topic Parameters
@see class method


## Send methods {#class-code-send}

- code->_execute
    This method is redefined by the various subclasses of class code and
    does the actual execution of the code object.

    **Bugs**:
    Its implementation *must* be in the C-language, which makes it
    impossible for the application programmer to create new code classes.

- code->debug_class: {user,service}
    If `service`, execution happens in `service` mode, hiding it
    from the XPCE and Prolog debuggers.  This mechanism is
    used by the Visual Hierarchy and Inspector tools to hide
    change-tracking from the debugger.

- code->execute
    Execute the code object.  This method is redefined by all `procedure`
    type code object (returning success or failure).  The method at this
    level invokes <-execute if the code object is an instance of a subclass
    of class function and returns success if the function executes
    successfully.  This makes functions available as procedures too.

    @see class-execute_function

- code->forward: any ...
    This is the common way to activate code objects.  It binds the
    predefined var objects @arg1 ...  @arg10 to the arguments provided to
    this message and then invokes ->execute on itself.  Example:

    	new(@m, message(@pce, write_ln, @arg1, @arg2)),
    	send(@m, forward, hello, world).

    will print

    	hello world

    ->forward pushes a `variable-binding` frame, thus limiting the scope of
    local assignments to var objects (see class assign and `var ->assign`.

    @see code->forward_vector
    @see code<-forward

- code->forward_vars: assign ...
    Makes the assignments from the given list and then ->execute's the
    code object.  The scope of the assignments is limited to the execution
    of the code object.  Suppose @client is defined as a global var object
    reflecting a notion of `current client`.  The following example executes
    the code object while binding @client to the value of the Prolog
    variable Client:

    	send(Code, forward_vars, assign(@client, Client)).

    This interface should be considered experimental and equivalent to the
    following (recall that `code ->forward` limits the scope of local
    assignments to var objects):

    	send(and(assign(@client, Client), Code), forward)

    See also class assign and class var.

- code->forward_vector: any ...
    The actual argument syntax is:

    	code ->forward_vector: any ..., vector, [int]

    Bind @arg1 ... from the concatenation of the leading arguments and the
    vector with the first [int] elements skipped and then invokes ->execute.
    See `object ->send_vector`

    @see code->forward
    @see object->send_vector

