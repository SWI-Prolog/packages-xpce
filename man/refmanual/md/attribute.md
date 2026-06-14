# class attribute {#class-attribute}

An attribute object is a mapping from a <-name to an <-value.
Attributes are used by class sheet to form a list of attribute-value
pairs and by class object to associate `object ->attribute`s to objects
(object-level programming).

An attribute is a subclass of class program_object, allowing the
programmer to spy attribute assignment and attribute fetch operations.
See `program_object ->trace` and friends.

@see object<-all_attributes
@see object->attribute
@see class interceptor
@see class sheet


## Instance variables {#class-attribute-instvars}

- attribute<->name: any
    Name of the attribute.  When used as attribute to an object, this may be
    any name that is not already the name of an instance variable defined at
    the class level for this object.  It is advised to use a name that is
    not defined as a get- or send-method at the class level.  For a sheet any
    name is valid, but it is advised not to use a name that conflicts with a
    get- or send-method defined at class sheet.  See also `sheet ->value`.

- attribute<->value: any
    The value of an attribute has type `any`.  This implies that any PCE
    object may be used to fill the attribute, but an attempt to fill it with
    a function object such as @receiver, @arg1, ...  or an obtainer will
    evaluate the special object or obtainer:

    	send(Attribute, value, @arg1)

    Will fill _Attribute_ with the value @arg1 refers to.


## Send methods {#class-attribute-send}

- attribute->initialise: name=any, value=any
    Create an attribute object from its <-name and <-value.  Attributes are
    normally used only in the context of sheet objects or `object
    ->attribute'.

- attribute->send: context=object, value=any
    Set the value of the attribute (takes 1 argument).  Compatibility to
    `send_method ->send`.


## Get methods {#class-attribute-get}

- attribute<-argument_type: index=[int] -> type
    Type of the 1-based argument.  For compatibility with methods and
    variables.   Returns type(any) if index equals 1 or @default.

- attribute<-convert: any -> attribute
    Convert a name into an attribute with this name and <-value @nil.  Used
    to associate attributes without giving them a value:

    	send(Object, attribute, age).
    	send(Sheet, append, age).

    @see object->attribute

- attribute<-get: object -> any
    Fetch the value of the attribute.  No arguments may be supplied.
    Compatibility with `get_method <-get`.

