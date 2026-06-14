# class binding {#class-binding}

Name/Value pair used to pass named arguments to methods.  A binding is
written as `Name := Value` and is recognised by `send_method ->send`
when it scans an argument list, allowing the caller to skip optional
arguments by name.  See `send_method ->send` for the full argument
binding rules.

The following example creates a `menu_item` named `help` whose
`end_group` slot is set via a binding rather than by positional
arguments:

    ?- new(X, menu_item(help, end_group := @on)).

@see send_method->send


## Instance variables {#class-binding-instvars}

- binding<->name: name
    Name of the formal argument or slot to bind.

- binding<->value: any|function
    Value to bind it to.  If a function is supplied it is evaluated
    before assignment.


## Send methods {#class-binding-send}

- binding->initialise: name=name, value=any|function
    Create a binding from a name and a value.  Normally written using
    the `:=` syntactic shorthand rather than constructed explicitly.
