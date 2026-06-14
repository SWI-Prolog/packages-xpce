# class node {#class-node}

A `node` represents a node in a tree object.  A node is a kind of
`wrapper` around an arbitrary graphical representing the image of the
node.

A node maintains it's relations to the tree, it's parent node(s) and
it's child nodes.

Class node defines the methods to make a tree editable.  See ->delete,
->son, ->delete_tree, etc.  The HierarchyEditor demo may be used as
a starting point for manipulating hierarchies.

@see graphical<-node
@see class tree


## Instance variables {#class-node-instvars}

- node<-collapsed: bool*
    When @on, the image related to this node is displayed, but all subnodes
    will be made invisible.  `Node ->collapsed` and `tree ->zoom` make it
    possible to visualise only a small part of a large tree.  A tree might
    contain multiple `collapsed` nodes.

    If the tree is displayed using `tree ->direction: list`, an
    image is displayed at the left-hand of the line connecting the
    node to its parent.  This image depends on the value of
    <-collapsed:

    - @on
    	Show a [+] sign (Pce.Tree.collapsedImage).  If the user
    	lef-clicks on this sign, a message ->collapsed: @off is
    	sent to the node.

    - @off
    	Show a [-] sign (Pce.Tree.expandedImage).  If the user
    	lef-clicks on this sign, a message ->collapsed: @on is
    	sent to the node.

    - @nil
    	Shows nothing, no events are handled for
    	collapse/expand.

    See library(pce_toc) for an example and a starting point the
    visualisation of a hierarchy.  See also `tree ->direction`.

    @see tree->zoom
    @see node-displayed
    @see tree->collapsed_handler

- node<-displayed: [bool]
    When @off, the node is invisible either because it is above `tree
    <-display_root' or below a node that has `node <-collapsed: @on`.

    @see tree->zoom
    @see node-collapsed

- node<-image: graphical
    Graphical object that represents the node.  This may be any graphical
    object.  Note that class graphical defines some methods that take care
    of the fact that they are part of a tree.  See `graphical<-node`
    and `graphical<-contained_in`.

    @see graphical<-convert
    @see graphical<-node

- node<-level: int
    Integer representing the distance from the `tree<-display_root` node.
    The node `tree<-display_root` has level 0.  This variable is updated by
    `tree ->compute`, which scans the tree depth-first.

- node-my_size: int
    Height (or width if `tree <-direction` equals vertical) or the graphical
    image represented.  Part of the layout system.

    @see node-sons_size

- node<-parents: chain
    Chain of parent nodes of this node.  For the `tree <-root` node this is
    the empty chain.  Nodes may have multiple parents, but many methods of
    the classes tree and node are ill-defined for this case.

- node<-sons: chain
    Chain holding all immediate sub-nodes.  The nodes are displayed
    left-to-right or top-to-bottom in the order they appear in this chain.
    This order may be changed using `node ->move_after`.

    @see node->son

- node-sons_size: int
    Combined height (width) of the entire subtree starting at this node.
    Part of the automatic layout mechanism.

    @see node-my_size


## Send methods {#class-node-send}

- node->delete
    Deletes node from the tree.  Each of the sons of this node will be
    connected to each of the parents of this node.

    The root of the tree may be deleted with this method.  If this node has
    no sons, `tree <-root` will be set to @nil.  Otherwise the first son of
    this node will be made the new root of the tree.

    When the `tree <-display_root` is deleted, it is set to the first parent
    of this node.

    **Bugs**: Deleting the root of a tree that has multiple sons is not well defined.

    @see node->unlink
    @see node->delete_tree

- node->delete_tree
    Deletes the entire subtree starting at this node.  Node that if any
    subnode of this node is still connected to the root through another
    chain of parents it will *not* be deleted from the tree.  See
    also ->unlink

    @see node->delete

- node->event: event
    `Node->event` is a little different from the other refinements
    of `graphical->event` as class node is not a subclass of
    graphical.  It is invoked from `tree->event` and allows for
    adding behaviour to nodes regardless of the underlying
    graphicals.

    The default method uses `central` handler objects from the
    associated <-tree:

    	a) If it is collapsed, run <-collapsed_handlers
    	b) If it is a leaf, run <-leaf_handlers
    	c) If it is the display_root, run <-root_handlers
    	d) else, run <-node_handlers

    In each of the cases a) to d), `run` means invoke the following
    message for each of the members of the handler-chain:

    	`event ->post: graphical, recogniser`

    If you wish to refine this method, please be aware that the
    normal schema described with `graphical->event` does not
    work for nodes.  Here is a skeleton implementation:

    	:- pce_global(@my_node_recogniser,
    				  make_my_node_recogniser).

    	make_my_node_recogniser(G) :-
    		...

    	event(Node, Ev:event) :->
    		(   send_super(Node, event, Ev)
    		->  true
    		;   send(Ev, post, Node?image,
    				 @my_node_recogniser)
    		).

- node->for_all: code
    Invokes itself recursively on all `node<-sons` of this
    node and finally runs `code` on the receiving node.

    Note that multiple-inheritance will cause some nodes to be visited
    more than once.  Argument binding:

    	@arg1:	Node object.

    See also <-find.

    NOTE: Upto version 5.0.9, the execution order was different:
    code was first executed on the node and then on the <-sons.
    The current order allows for deleting nodes.

    **Diagnostics**:
    Terminates immediately with failure if code could not be executed fro
    some node

    **Bugs**:
    `Node ->for_all` is not safe when the executed code manipulates the
    subtree below this node.

    @see node<-find
    @see node->for_some
    @see tree->for_all

- node->for_some: code
    Equivalent to `node ->for_all`, but ignores the exit-status of the
    executed code.  Succeeds always.

    @see node->for_all

- node->initialise: image=graphical
    Create a node from an arbitrary graphical object.  Nodes may be created
    from any graphical object, including devices, dialog_items and even
    trees.   A typical way to build a tree is the following:

    	?- new(T, tree(new(Root, node(text(anything))))),
    	   send(Root, son, new(S0, node(text(living_thing)))),
    	   ...

    @see node<-convert

- node->is_parent: node
    Succeeds if the argument is an (indirect) parent of this node.  Fails
    silently otherwise.

    @see node->is_son

- node->is_son: node
    Succeeds if argument node is an (indirect) son of this node.  Fails
    silently otherwise.

    @see node->is_parent

- node->move: node
    Make the argument node a direct son of this node.

    Succeeds without doing anything if the argument is already a son.  Fails
    silently if one of the following is the case:

    1. The argument node is in another tree
    2. This node is not part of a tree
    3. The argument node is this node
    4. The argument node is a parent of this node

    @see node->son

- node->move_after: [node]*
    If node and the argument are immediate sons of the same parent,
    the receiver is placed immediately after the argument node.
    If the argument is @default, the receiver is placed at the
    end chain of sons (making it the lowest or rightmost node).
    If the argument is @nil, the receiver is places at the start
    of the chain of sons.

    **Bugs**: Not well defined when the receiver node has multiple parents.

    @see chain->move_after

- node->son: son=node, before=[node]*
    Makes the argument node a son of this node.  This is the most
    common way to build a hierarchy.  If before is @nil or @default,
    the child is added at the end of the <-sons chain.  Otherwise it
    is added just in front of the before argument.

    **Diagnostics**:
    # <Node> already in a tree
    	Argument node is member of another tree.

    - <Node> should be in a tree
    	The receiver node is not in a tree.

    @see node-sons
    @see node->move

- node->sort_sons: code
    Sort the chain of <-nodes, just like `chain->sort`, and update
    the hierarchy layout afterwards.

- node->swap: node
    Swap the images of the specified nodes.

    **Bugs**:
    This method manipulates the images rather than the nodes as done by most
    of the other methods manipulating the layout of the hierarchy.

    @see node->swap_tree

- node->swap_tree: node
    Swap the subtrees formed by both nodes.  It performs the following
    steps:

    1. Identify common parents and swap the positions in the
    	parent <-sons chain.

    2. Swaps the <-parents chain between both nodes

    3. Destroy and recreate the necessary connections.

    **Diagnostics**:
    Fails silently if:

    1. The nodes are not in the same tree or not in a tree at all
    2. One node is a parent of the other

    @see node->swap

- node->unlink
    Equivalent to `node->delete`: removes node from the tree binding all
    sons to all parents of this node.  To remove an entire subtree,
    see ->delete_tree.

    @see node->delete

- node->unrelate: node
    Delete the direct relation between this node and the argument.  Removes
    all nodes that have no relation to the root of the tree anymore.

    **Diagnostics**: Fails if node is not a direct parent or son of the argument.

- node->unzoom
    Invokes `tree ->unzoom`, unzooming the tree to its root.

    @see node->zoom
    @see tree->unzoom

- node->zoom
    Invokes `tree ->zoom`, giving this node as the argument.  This will make
    node the root of the visible part of the hierarchy.

    @see node->unzoom
    @see tree->zoom


## Get methods {#class-node-get}

- node<-contained_in: -> tree
    Return <-tree.  Fails if the node is not attached to a tee
    object.  See `visual<-contained_in` for a general description
    of this method.

    @see tree<-contains
    @see node<-contains

- node<-contains: -> chain
    @see tree<-contains
    @see node<-contained_in

- node<-convert: graphical -> node
    Create node for graphical object.  This method may be used to expand the
    tree by adding graphical objects rather than nodes.  Note however that
    the actually stored object will be the node:

    	?- new(@t, tree(text(animal))),
    	?- get(@t, root, Root)

    	Root = @345263
    	?- get(@345263, class_name, Class)

    	Class = node.

    @see node->initialise

- node<-find: code -> node
    Traverses the tree in a depth-first fashion until code can be executed
    successfully on a node and then returns this node.  See also
    ->for_all.

    **Diagnostics**: Fails if code fails for all nodes.

    @see node<-find_node
    @see node->for_all

- node<-find_node: graphical -> node
    Find node (either myself or a node in the subtree of which this node is
    the root) that has `node <-image` equal to the specified graphical.

    @see graphical<-node
    @see node<-find

