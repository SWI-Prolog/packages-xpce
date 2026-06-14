# class send_method {#class-send_method}

A send_method object defines the mapping from a selector to an
implementation for the `send` virtual machine operation.  Send_method
objects may be found using `object <-send_method` and executed using
->send.

See also class method and class behaviour.

@see class type
@see host->call
@see class method
@see tool ClassBrowser
@see object<-all_send_methods


## Send methods {#class-send_method-send}

- send_method->send: receiver=object, argument=unchecked ...
    Invoke a send-method on receiver.  The remaining arguments provide the
    method arguments.  They will be bound to the format arguments of the
    method according to the following rules:

    1. Assign all unbound (e.g. no instances of class :=) by
    	position.

    2. If the last argument allows for multiple values, pack all
    	remaining unbound arguments is a code_vector object and pass
    	assign them to this last formal argument.

    3. If there are bound arguments, process them left-to-right,
    	assigning them to there corresponding named format argument.

    	If the named formal argument allows for multiple values, append
    	the value of the binding to the vector of values.

    	If the last argument allows for multiple values and
    	there are remaining unbound arguments append them to the vector
    	of values.

    4. Assign all unassigned arguments to @default.

    Finally all arguments are type-checked (see `type->validate` and
    `type<-translate`. Examples:

    - Type vector: x=[int], y=[int], width=[int], height=[int]

    		send(<Obj>, <selector>, 1, 2, 3)  -->
    			x = 1, y = 2, width = 3, height = @default

    		send(<Obj>, <selector>, 1, height := 4) -->
    			x = 1, y = width = @default, height = 4

    - Type vector: name=name, options=:= ...

    		send(<Obj>, <selector>, foo,
    			 size := size(4, 6),
    			 font := font(helvetica, bold, 14)) -->
    			name = foo,
    			options = code_vector(size := size(4, 6),
    			font = font(helvetica,bold,14))

    		send(<Obj>, <selector>,
    			 format := center, name = foo) -->
    			name = foo,
    			options = code_vector(format := center)

    The second example illustrates how named arguments may be used in
    combination with multiple valued arguments to achieve Lisp-like keyword
    arguments.

    @see class :=

