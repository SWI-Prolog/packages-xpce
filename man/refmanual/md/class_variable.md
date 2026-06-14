# class class_variable {#class-class_variable}

Class-variables represents constants for the class.  The are
commonly used to represent default values for the visual
appearance of graphical classes: thickness, fonts, colours,
etc.

Class variables are normally declared using class_variable/4.

If a class defines both a variable object and a class_variable
object, the value of the class-variable is used as default
(initial) value for the instance variable.

Defaults for class-variables may be defined externally using
the Defaults file, which resides in the XPCE home directory.

See also:

- `object<-class_variable_value`
- `@pce->load_defaults`
- The UserGuide


## Instance variables {#class-class_variable-instvars}

- class_variable<-default: any
    Program default value.  This may be any object that satisfies
    <-type.

- class_variable<-type: type
    Type of the class-variable.  A textual value from the Defaults
    file will be converted using this type.  Default is the type of
    the corresponding instance-variable.

- class_variable-value: any
    Current value.  Send-access is guarded by ->value to verify the
    type, while get-access is handled by <-value to check the
    Defaults file, or use <-default.  The initial value is the
    constant @not_obtained.


## Send methods {#class-class_variable-send}

- class_variable->initialise: class=class, name=name, default=any, type=[type], summary=[string]*
    Define a class-variable for the given class.  _Name_ is the name
    of the class-variable.  If this name matches the name of an
    instance variable (see class variable), the class-variable will
    be used as a default for the instance variable.  _Default_
    defines the value if no value is specified in the Defaults file
    (see `@pce ->load_defaults`).  _Type_ declares the type.  If the
    class-variable acts as a default-variable for an
    instance-variable, using @default will copy the type of the
    instance-variable.   _Summary_ is used for documentation.  When
    absent, the documentation from the corresponding instance
    variable is used, or the documentation from a class-variable
    defined in one of the super-classes.

    Class-variables are normally defined using class_variable/4 in
    the definition of a user-defined class.

- class_variable->value: any
    Set value of the class-variable.  Should be used very carefully,
    as instances are not notified, and may not be aware of the
    possibility that the value of a class-variable can change.


## Get methods {#class-class_variable-get}

- class_variable<-convert_string: textual=char_array -> any|function
    Called to convert the textual value found in the defaults file.
    It calls the @object_parser to translate the textual
    representation into an object, after which <-type is used to
    verify the type.

- class_variable<-get: object -> unchecked
    Invoke (read) class-variable.  Required to make class-variables
    available as normal object-behaviour.

- class_variable<-print_name: -> name
    Yields <class>.<name>.  Used to print messages.

- class_variable<-string_value: -> char_array
    Returns the textual value from the Defaults file.  See also
    `@pce ->load_defaults`.

- class_variable<-value: -> any
    Compute and return the value.  Normally, the value is used as
    default for an instance-variable, or fetched using
    `object<-class_variable_value`.

