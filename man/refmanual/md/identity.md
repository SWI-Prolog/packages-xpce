# class identity {#class-identity}

An `identity` expresses that _Attribute A of object O1_ equals
_Attribute B of object O2_.  It is the most basic relation object.


## Instance variables {#class-identity-instvars}

- identity<->from: selector1=name
    Attribute at the specified side of the constraint object.  For normal
    bi-directional relations, both constrained objects should define both
    send- and get-behaviour with the specified name.

    If a constraint object is used as a propagator (see `constraint
    ->initialise'), get-behaviour on one side and send-behaviour on the
    other suffices.

- identity<->to: selector2=name
    *Inherits description from*: identity-from


## Send methods {#class-identity-send}

- identity->backwards: from=object, to=object
    Performs a get-operation on `to` using the selector <-to, followed
    by a send-operation on `from` using the send <-from.

- identity->create: from=object*, to=object*
    Update after instantiation.

- identity->forwards: from=object, to=object
    Performs a get-operation on `from` using the selector <-from, followed
    by a send-operation on `to` using the send <-to.

- identity->initialise: selector1=name, selector2=[name]
    Create a relation defining

    	``Attribute selector1 of `constraint <-from` equals
    	Attribute selector2 of `constraint <-to`''

    If the second selector is not specified is defaults to the first.

