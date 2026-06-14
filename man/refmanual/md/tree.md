# class tree {#class-tree}

Class `tree` is a subclass of class `device` which displays its
graphical objects formatted as a hierarchy.  Class `tree` fits well in
PCE's target domain: graphical interfaces for structured representations
(models).

The nodes of a PCE tree may be arbitrary graphical objects (yes, even
trees, editors, etc.).  Trees allow for multiple inheritance, but the
automatically generated layout is often not optimal when multiple
inheritance is used intensively.  The automatically generated layouts
for hierarchies are top-to-bottom, left-to-right (default) or `indented
list'.

Class tree itself is responsible for displaying and formatting the nodes
of the hierarchy.  The hierarchy is defined by `node` objects.  A node
object refers to a graphical (using `node <->image`), its parent node(s)
and its son nodes.

Event-handling for trees may be done in two ways.  The first is to
define event-handling for each of the individual graphicals representing
the nodes.  The second is to associate recognisers with the tree object
itself.  Class `tree` defines various different recognisers, activation
of which depends on the status of the node.  See `tree ->event`.

@see graphical->layout
@see class node


## Instance variables {#class-tree-instvars}

- tree<-auto_layout: bool
    When @on (default), the tree will recompute its layout on any change to
    any of its nodes or change to the tree itself.  When @off the tree
    won't automatically recompute the layout.

    Sometimes, it is desirable to allow the user to move the nodes of a
    tree by hand.  With <-auto_layout: @on this is not possible as the
    tree will immediately move the node back to the original position.

    See also ->compute and ->layout.

- tree<-collapsed_handlers: chain
    @see tree->event

- tree<-direction: {horizontal,vertical,list}
    Defines the layout:

    - horizontal
    	Root at the left, the sons are displayed left-to-right.

    - vertical
    	The tree is displayed `top-to-bottom`.

    - list
    	The tree is displayed as an indented list.  The line
    	properties of the <-link are used to determine the
    	thickness, texture and colour of the lines.  Each node
    	will, if it has sons, draw a line from (x + <-level_gap/2, bottom side)
    	down to halfway the last son.  If to node is not the
    	root, it will draw a line from halfway its height
    	towards the line from its parent.  See also `node <->collapsed`.

    Note: the default tree is displayed left-to-right rather than
    top-to-bottom as the nodes are often much wider than high.  Using
    left-to-right layout allows for displaying much larger hierarchies.

    When a large number of items is to be displayed as an indented list,
    also consider class list_browser.  The indentation may be realised using
    `list_browser ->tab_stops`.

- tree<-display_root: node*
    Root of the visible part of the tree.  This variable is managed using
    ->zoom and ->unzoom.

    @see tree-root
    @see tree->zoom

- tree<-leaf_handlers: chain
    @see tree->event

- tree<-level_gap: int
    Distance between a node and its parents or sons.

- tree<-link: link
    Link used to create connections between parent- and son-nodes.  Its
    attributes may be changed to attach arrows or modify one of the line
    attributes.  For example,

    	send(Tree?link, arrows, first).

    will attach an arrow pointing to the root to every relation created
    after this call has been made.

    @see tree-link_gap
    @see tree-parent_handle
    @see tree-son_handle

- tree<-link_gap: int
    Distance between the image and the line.  Used to define the X-
    and Y-positions of the <-son_handle and <-parent_handle.

    @see tree-parent_handle
    @see tree-son_handle
    @see tree-link

- tree<-neighbour_gap: int
    Distance between two nodes that are at the same level (= distance from
    the root).  The default (0) satisfies when the nodes are
    represented by text object.  For other graphicals a small
    positive value gives better results.

    @see tree->level_gap
    @see tree->neighbour_gap

- tree<-node_handlers: chain
    @see tree->event

- tree<-parent_handle: handle
    This handle object is attached to each graphical representing a node.
    It is the connection-point for a line linking this graphical to its
    parent.

    This handle is automatically updated by `tree ->direction`..

    @see tree-link_gap
    @see tree-link
    @see tree->direction
    @see tree-son_handle
    @see class handle

- tree<-root: node*
    Root node of the tree.  Note that the root of the visual part of the
    tree is represented by <-display_root.  Class tree delegates messages
    not understood to its <-root node when present.

    If the argument is *nil, all nodes are removed from the tree.
    If the argument is a new root object the behaviour depends
    on the `relink` argument.  With `relink` = @off (default), the
    tree is first cleared using ->root: @nil, after which the new
    root is attached.  With `relink` = @on the old root is added
    as the only `node->son` of the new root.

    See also ->zoom and ->unzoom.

    @see tree-display_root

- tree<-root_handlers: chain
    @see tree->event

- tree<-son_handle: handle
    Similar to `tree <-parent_handle`, but is the connection-point for the
    connection towards son-nodes.

    @see tree-link_gap
    @see tree-link
    @see tree-parent_handle


## Send methods {#class-tree-send}

- tree->collapsed_handler: recogniser
    Attach a recogniser for all nodes that have `node <->collapsed: @on`.
    See `tree ->event` for the interpretation.

    @see node-collapsed
    @see tree->event

- tree->compute
    Whenever something changes to the tree that might affect its layout,
    this is notified using `graphical ->request_compute`.  `tree ->compute`
    recomputes the layout of the tree by invoking ->layout.

- tree->direction: {horizontal,vertical,list}
    @see tree-parent_handle

- tree->event: event
    `Tree ->event` allows for centralised event-processing for all nodes
    displayed on the tree.  It performs the following steps.

    1. Invoke `device ->event`.  When successful, succeed.
    2. Invoke `node->event` on the node below the pointer.

    - * Finding the Context Arguments

    The most reliable way to find the various context objects are:

    	| @event?receiver        | Graphical receiving the event  |
    	| @event?receiver?node   | Node object for this graphical |
    	| @event?receiver?device | The tree object                |

    @see tree->root_handler
    @see tree->node_handler
    @see tree->leaf_handler
    @see tree->collapsed_handler
    @see graphical<-node
    @see tree-root_handlers
    @see tree-node_handlers
    @see tree-leaf_handlers
    @see tree-collapsed_handlers

- tree->for_some: code
    Run code on all nodes.

- tree->for_all: code
    Invokes the message to the <-root of the tree, iterating over all nodes
    of the tree.  See also `node ->for_all`, `node ->for_some` and
    <-contains.

    @see node->for_all
    @see tree->for_some

- tree->initialise: root=[node]*
    Create a tree object and assign it a root.  When the root is @nil
    (default), the tree must be assigned a root using `tree ->root` before
    any other nodes may be attached.

    Note that class node defines a convert method for graphicals that creates
    a node with the specified graphical as `node <-image`.

    @see tree->root

- tree->layout
    Recompute the layout of the tree.  This method is normally called from
    ->compute and sends the following three messages to <-display_root:

    	| `node ->compute_level`  | Update the `node<-level` slots |
    	| `node ->compute_size`   | Compute the sizes of all nodes |
    	| `node ->compute_layout` | Place the nodes                |

- tree->leaf_handler: recogniser
    Attach a recogniser for all nodes that have no sons (i.e. are leaves).
    See `tree ->event` for the interpretation.

    @see tree->event

- tree->level_gap: int
    @see tree-neighbour_gap

- tree->neighbour_gap: int
    @see tree-neighbour_gap

- tree->node_handler: recogniser
    Attach a recogniser for all nodes. See `tree ->event` for the
    interpretation.

    @see tree->event

- tree->root: root=node*, relink=[bool]
    @see !already_has_root
    @see tree->initialise

- tree->root_handler: recogniser
    Attach a recogniser for the `tree <-display_root` node.  See `tree
    ->event' for the interpretation.

    @see tree->event

- tree->unzoom
    Equivalent to:

    	send(Tree, zoom, Tree?root).

    To unzoom only one level, use

    	send(Tree, zoom, Tree?display_root?parents?head).

    Note that this method is ambiguous when the display_root has multiple
    parents.

    @see node->unzoom
    @see tree->zoom

- tree->zoom: node
    _Zoom_ in for a particular subtree.  The visualisation of the tree will
    behave as if the argument-node of this method is the root.  Sets the
    variable `tree <-display_root`.

    @see node->zoom
    @see node-displayed
    @see node-collapsed
    @see tree->unzoom
    @see tree-display_root


## Get methods {#class-tree-get}

- tree<-contains: -> chain
    New chain holding all node objects in the tree.

    Its device functionality would be to return the chain of graphicals,
    holding only the graphicals for the displayed nodes and connections.

    The contains-hierarchy for trees is defined as follows:

    	tree
    	|  node
    	|   |  image

    Image (being a graphical) may contain any number of sub-visuals.

    @see graphical<-contained_in
    @see node<-contains
    @see node<-contained_in

