# class event_node {#class-event_node}

Node in the type inheritance hierarchy.  Do not use this class for other
purposes as it's definition is subject to change.

@see class event_tree
@see object->recogniser


## Instance variables {#class-event_node-instvars}

- event_node<-parent: event_node|event_tree
    Parent of the node.  The root has the event_tree object as a parent.

- event_node<-sons: chain*
    Chain of event_node objects that represent the sons.  Ordering is
    not defined.

- event_node<-value: event_id
    `event <-id` associated with this node.


## Send methods {#class-event_node-send}

- event_node->initialise: value=name, parent=[name|event_node]*
    Create an event_node object below `parent`.   By creating a new
    event_node the set of valid names for the type `event_id` is
    extended and event objects of this new type can be created.

    The system defines a node named `user` for user-defined
    event-types.

    See also @event_tree, `event->initialise` and `event->is_a`.

- event_node->son: event_node
    @see !already_has_parent

