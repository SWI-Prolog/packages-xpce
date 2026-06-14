# class obtain {#class-obtain}

Class ? is (pronounced as `obtainer`) is a template for the virtual machine
instruction get in the same fashion as a message object is a template to
the send operation.   Obtainers are the most commonly used function
objects.

Obtainers add a very powerful mechanism to PCE.  Understanding them is
critical for writing readable and fast PCE programs.  Please study the
examples.   In the example below, an obtainer is used to combine
dialog_item selections to create a single call to the application:

	?- new(D, dialog),
	   send(D, append, new(label)),		% for feedback
	   send(D, append, new(Name, text_item(name, ''))),
	   send(D, append, new(Age, text_item(age, ''))),
	   send(D, append,
			button(create, message(@prolog, create_person,
								   Name?selection,
								   Age?selection))),
	   send(D, open).

Of course, the application must define the predicate create_person.

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
    *Inherits description from*: message-context

