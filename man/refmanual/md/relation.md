# class relation {#class-relation}

A `relation` describes the reusable part of a constraint object.  Class
relation may be subclassed to define new relations.  Subclasses normally
redefine ->create, ->forward and/or ->backwards.

The current system contains two types of relations, two of which
are strictly between graphicals and one general.  They are:

- class spatial
	Spatial relation between two graphicals

- class identity
	Identity of attributes.

New relations may be defined as user-defined subclasses
of this class.  A relation should define the methods:

- ->create
	Called when the relation is established

- ->forwards
	Called if the `constraint <-from` side changes.

- ->backwards
	Called if the `constraint <-to` side changes.

@see class constraint


## Send methods {#class-relation-send}

- relation->backwards: from=object, to=object
    Called after `to` has changed.  Its task is to update `from`
    accordingly.

- relation->create: from=object, to=object
    Invokes ->forwards to itself.  This method is activated when the
    constraint object this relation is referred from is initialised.

- relation->forwards: from=object, to=object
    Called after `from` has changed.  Its task is to update `to`
    accordingly.  If ->create has not been redefined, this method is also
    called when the constraint object is established.

