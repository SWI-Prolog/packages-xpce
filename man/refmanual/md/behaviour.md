# class behaviour {#class-behaviour}

Behaviour is an super-class of class method and class variable, the
classes than can define behaviour at the class level.   Class behaviour
itself has few properties, but sharing these simplifies and speeds up
message passing.

The sub-classes class send_method, class get_method and class variable
all define the methods ->send and <-get which executes them in a send-
or get operation.  Sending a message to an object is thus realised
using:

	send(Object, Selector, Arg ...) :-
		get(Object, send_method, Selector,
			tuple(Receiver, Behaviour)),
		send(Behaviour, send, Receiver, Args ...)

(Pseudo code; Prolog cannot handle Arg ...)

@see class interceptor
@see class object
@see class class


## Instance variables {#class-behaviour-instvars}

- behaviour<-context: class|object*
    Definition context of this method.  If the behaviour is associated with
    a class, this is always the class on which the behaviour was initially
    defined:

    	?- get(class(box), send_method, y, YMethod),
    	   get(YMethod, context, C).

    	C = @graphical_class

    See also `method <-inherited_from`.

- behaviour<-name: name
    Selector of this behaviour.  This is the name of the send- or get-behaviour
    handled by this method object.

