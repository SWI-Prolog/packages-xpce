# class obtain {#class-obtain}

Class ? is (pronounced as _obtainer_) is a template for the virtual machine
instruction _get_ in the same fashion as a message object is a template to
the _send_ operation.   Obtainers are the most commonly used function
objects.

Obtainers add a very powerful mechanism to PCE.  Understanding them is
critical for writing readable and fast PCE programs.  Please study the
examples.   In the example below, an obtainer is used to combine
dialog_item selections to create a single call to the application:

	?- new(D, dialog),
	   send(D, append, new(label)),		% for feedback
	   send(D, append, new(Name, text_item(name, ''))),
	   send(D, append, new(Age,  int_item(age))),
	   send(Age, hor_stretch, 100),		% stretch to right of D
	   send(D, append,
		button(create, message(@prolog, create_person,
				       Name?selection,
				       Age?selection))),
	   send(D, open).

Of course, the application must define a predicate `create_person/2`.

@see @arg1
@see topic Checking
@see class get_method
@see class block
@see topic Obtainers
@see object<-_slot
@see object<-_references
@see object<-_inspect
@see object<-_flags
@see object<-_class_name
@see object<-_class
@see object<-_arity
@see object<-_arg
@see object->_inspect
@see object->_free


## Instance variables {#class-obtain-instvars}

- obtain-_context
    Contains the Prolog module if the obtainer refers to a Prolog
    predicate.  Here is an example of an obtainer using Prolog:

       ?- send(@pce, writeln,
	       ?(@prolog,current_prolog_flag,version)).
       100109

## Send methods {#class-obtain-send}

- obtain->initialise: receiver=object|function, selector=name|function, argument=any|function ...
    Create a get-template from its receiver, selector and argument list.
    Note that all arguments may be function objects.  When the obtainer
    is evaluated it first evaluates all functions.  Example:

	?- new(B, box(100,100)),
	   send(@pce, writeln, B?center?x).
	50

    See also class message.

## Get methods {#class-obtain-get}

- obtain<-_execute
    Evaluate the obtainer.  Function objects in the <-receiver, <-selector
    or <-arguments are evaluated first, after which the XPCE virtual
    machine operation

	get(<-receiver, <-selector, <-arguments ..., Value)

    is performed and the resulting Value is the result of the obtainer.

