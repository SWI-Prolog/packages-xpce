# class create {#class-create}

Class create defines a function that creates new instances like
the interface predicate new/2.  The first argument is either the
name of the class to create instances from or a class object.
The remaining arguments are packed into the code_vector object
<-arguments.

Instances may also be created using class ?:

	?- new(CreatePoint, ?(@pce, instance, point)).
	?- new(CreatePoint, class(point)? instance).

Class create is more comfortable to write, is faster and does
not require the class to exist when the create instance is
created.

See <-_execute for a description of the creation process.  See
also `@pce <-instance`, `class <-instance`, @vmi_new and new/2.


## Instance variables {#class-create-instvars}

- create<->argument: code_vector*
    Arguments used to create instance.  @nil is interpreted as an
    empty argument vector.

- create<->class: name|class
    Class (name) to create instance of.  If it is a name, it will be
    dereferenced to a class object on the first <-_execute call.


## Send methods {#class-create-send}

- create->argument: index=int, value=any|function
    Access the nth (1-based) argument in the argument vector used to
    create new instances.

- create->initialise: class=name|class, argument=any|function ...
    The first argument is either the name of a class or a class
    object.  The remaining arguments are packed in the code_vector
    object <-arguments.   See <-_execute for the evaluation of
    create objects.


## Get methods {#class-create-get}

- create<-_execute: -> unchecked
    Create an instance of <-class using <-arguments.  If <-class is
    a name, it is converted into a class object and the slot is
    replaced with the class object to speed up subsequent execution
    of this create object.

    Function objects in <-arguments are expanded (as with message
    objects and ? objects) and finally the VMI new() is activated to
    create the new instance.

- create<-argument: index=int -> value=any|function
    *Inherits description from*: create->argument

