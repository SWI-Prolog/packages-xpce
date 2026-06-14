# class get_method {#class-get_method}

A get_method object defines the mapping from a selector to an
implementation for the `get` virtual machine operation.  For details see
class method and class behaviour.

@see class type
@see host<-call
@see class method
@see tool ClassBrowser
@see class ?


## Instance variables {#class-get_method-instvars}

- get_method<-return_type: type
    Describes the type of the value returned by this method.  It is first of
    all intended for documentation and consistency checking purposes.

    @see !converted_return_value
    @see !bad_return_value


## Send methods {#class-get_method-send}

- get_method->initialise: name=name, return=[type], types=[vector], implementation=function|host_method, summary=[string]*, source=[source_location]*, group=[name]*
    The ->initialise of a get_method takes one additional argument
    with respect to `method ->initialise`: the return_type.  If the
    actually returned value does not satisfy this type, type
    conversion is attempted.  If the conversion succeeds, the error
    converted_return_value is raised and the converted value is
    returned.  If the conversion is not successful, the error
    bad_return_value is raised and the method fails.


## Get methods {#class-get_method-get}

- get_method<-get: receiver=object, argument=unchecked ... -> value=unchecked
    Invoke get-behaviour on object.  Similar to `send_method ->send`.  After
    the implementation returns a result, this result is passed to the
    <-return_type for validation.

