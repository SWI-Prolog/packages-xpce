# class quote_function {#class-quote_function}

A quote_function object may be used to avoid evaluation of a function
object, notably to pass function objects over methods that would cause
the function to be evaluated.   Two mechanisms make quote_function
objects almost transparent to functions objects:

- Class quote_function delegates to its <-function.
- `function <-convert` translates a quote_function into the
	<-function.

In the following example, Chain is a chain of chains.  The task is to
sort the elements of the sub-chains by name.

	send(Chain, for_all,
		 message(@arg1, sort,
				 quote_function(?(@arg1?name, compare,
								  @arg2?name))))

Without the quote_function, the function will be evaluated by the message
object, while evaluation by `chain ->sort` is the intended behaviour.


## Instance variables {#class-quote_function-instvars}

- quote_function<->function: function
    The function object quoted.  Methods are delegated to this object.


## Send methods {#class-quote_function-send}

- quote_function->initialise: function=function
    Create a quote_function from the function quoted.

