# class message {#class-message}

A message object is a template for the execution of a *send* virtual
machine operation: messages represent behaviour to be invoked.

Messages are the most commonly used code objects.  The code
below creates a small dialog window with buttons to quit the window
and to start PceDraw (the XPCE drawing demo):

	?- new(D, dialog('Start PceDraw')),
	   send(D, append,
		    button(start_draw,
				   message(@prolog, pcedraw))),
	   send(D, append,
			button(quit,
				   message(D, destroy))),
	   send(D, open).

It illustrates how a message can be used to activate a host language
function (in this case a Prolog predicate) as well as how a message
can be used to start an internal XPCE operation.   The following example
shows how a message may be used to change the `graphical ->pen` of
all graphical objects on a device object.  It exploits `chain
->for_all'.

	send(Device?graphicals, for_all,
		 message(@arg1, pen, 3)).


## Instance variables {#class-message-instvars}

- message<-arguments: code_vector|any|function*
    Vector of arguments to pass to the send-operation.  When @nil, no
    arguments are passed.  It is a code_vector object because it should
    allow the elements to be function objects.   See also ->initialise and
    <->argument.

- message<-context: any*
    The context slot is reserved for cooperation with `function-based`
    modularization.
    For Prolog, it contains the module from which the object was
    created.  If the object is `executed`, this context argument
    will be used to resolve the proper function.  In Prolog:

	?- new(X, foo:message(@prolog, hello)),
	   send(X, execute).

    will call the predicate `hello` in the module `foo`.  See also
    '?<-_context' and `rc<-context`.


## Send methods {#class-message-send}

- message->_execute
    Start the XPCE virtual machine operation

	send(<-receiver, <-selector, <-arguments ...)

    Any function object appearing in the <-receiver, <-selector or
    <-arguments are first evaluated.

- message->argument: index=int, value=any|function
    Set the nth (1-based) argument of the send-operation.  When necessary,
    the <-arguments code_vector object is created or enlarged.

- message->initialise: receiver=object|function, selector=name|function, argument=any|function ...
    Create a send-template from its receiver, selector and argument list.
    Note that all arguments may be function objects.  If the message is
    executed it will first evaluate all functions.    Example:

	?- new(D, dialog),
	   send(D, append,
		    button(show_display_size,
				   message(@display, inform,
						   'The display is %dx%d pixels',
						   @display?size?width,
						   @display?size?height))),
	   send(D, open).

    See also class ?.


## Get methods {#class-message-get}

- message<-argument: index=int -> value=any|function
    *Inherits description from*: message->argument

