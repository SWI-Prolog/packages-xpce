# class event_tree {#class-event_tree}

Class event-tree defines the inheritance hierarchy of event-types.  Each
node of this tree represents a specific event.  The event-tree must be
capable to do one operation very quick: telling from to event-type names
whether the one is below the other in the tree.  For this purpose the
event-tree contains a has-table to find a node from an event-type.

Do not use this class directly.  It's definition is subject to change.
Notably multiple inheritance is studied as a possible extension.

This class has one global predefined object associated to it:
@event_tree.  This object can be used to examine the inheritance
hierarchy of events.

See also `event ->is_a`, class event_node and the _Events_ demo program.

@see event->is_a
@see class event
@see class event_node
@see object->recogniser


## Instance variables {#class-event_tree-instvars}

- event_tree-table: hash_table
    Hash_table object mapping `event<-id`s into event_node objects.


## Send methods {#class-event_tree-send}

- event_tree->root: event_node
    @see !already_has_root

