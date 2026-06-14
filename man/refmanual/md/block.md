# class block {#class-block}

A block object defines a sequence of code objects with a formal
parameter list.

On ->forward, the var objects in <-parameters will be bound to
the forwarded arguments (exceeding arguments are bound to
@arg1 ...) and the <-members statements of the block are
executed just as with the super-class and.

Example:

	?- send(block(new(X, var),
				  message(@pce, write_ln, X)),
			forward,
			'Hello World').

	Hello World.

@see class ?


## Instance variables {#class-block-instvars}

- block<->parameters: code_vector*
    Vector with formal parameters.   Assigned (see `var->assign`) by
    ->forward.


## Send methods {#class-block-send}

- block->forward: any ...
    When a block object is forwarded, it will bind the arguments to
    the var objects in <-parameters by position.  Possible exceeding
    arguments are bound to @arg1 ...  for compatibility reasons.
    Next the block is ->execute'd just like the and object.

    Example:

    	?- send(chain(chain(a,b), chain(c,d)), for_all,
    			block(new(Sub, var),
    				  message(Sub, for_all,
    		 		  message(@pce, write_ln, Sub, @arg1)))).

