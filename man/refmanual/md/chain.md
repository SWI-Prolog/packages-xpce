# class chain {#class-chain}

A chain is a linear single-linked list of objects.  Chains form the
common way to represent collections of objects in PCE.  Chains are also
used to represent multiple-valued slots.  Examples are `device
<-graphicals', `class <-sub_classes`, etc.

PCE chains form the natural counterpart of Prolog lists.  For this
reason the PCE/Prolog interface defines predicates to transform Prolog
lists to PCE chains and visa-versa.  Note however that in many cases it
is not necessary to convert a PCE chain to a Prolog list; PCE defines
many operations on chains.  Before you consider converting a PCE chain
to a Prolog list, first check whether it is not easier to do the job
inside PCE.  Notably look at ->member, ->empty,. ->for_all, ->for_some,
<-find, <-find_all and <-map.

@see class vector
@see class table
@see class hash_table


## Instance variables {#class-chain-instvars}

- chain-current: alien:Cell
    Alien reference to `current cell` of the chain.  The notion of `current`
    was introduced a long time ago in PCE, while methods only had one
    argument.  Methods needing a second often used `current` for this
    purpose.

    Now that PCE methods may have any number of arguments there is little
    use for `current`.  It is advised not to use `current`.

    @see topic Current

- chain-head: alien:Cell
    First cell of the chain.  When the chain is empty, its value is @nil.

- chain<-size: int
    Number of elements in the chain.  Maintained by the various methods
    manipulating chains.

- chain-tail: alien:Cell
    Last cell of the chain.  When the chain is empty, the slot has
    value is @nil and the method fails.


## Send methods {#class-chain-send}

- chain->_append: value=any|function
    Append that allows for appending function objects (@arg1, @event,
    @receiver, class ?) to a chain without evaluating them.

    @see chain->append

- chain->add: value=any
    ->append object to chain if it is not already a member of it.

    @see chain->member
    @see chain->append

- chain->after: first=any, second=any
    Succeeds if the first occurrence of the first argument is after the
    first occurrence of the second argument.  Example:

    	?- new(@ch, chain(aap, noot, mies)).
    	?- send(@ch, after, noot, aap)		--> yes
    	?- send(@ch, after, aap, mies)		--> no

    Fails if one of the arguments is not in the chain.

    @see chain->before

- chain->append: value=any
    Append argument at the end of the chain.  Although chains are
    represented as single linked lists, append is efficient because chains
    maintain a pointer to the last cell.

    Note that chains may contain duplicates.

    @see chain->add
    @see chain->_append

- chain->before: first=any, second=any
    Succeeds if the first occurrence of the first argument is before the
    first occurrence of the second argument.  See `chain ->after`.

    @see chain->move_before
    @see chain->after

- chain->cell_value: cell_reference=int, value=any
    Set the value of specified cell.  The first argument is the reference
    returned by `chain <-head_cell` or `chain <-next_cell`.

    One should try to avoid using this function.

    @see chain<-cell_value
    @see chain<-head_cell
    @see chain<-next_cell

- chain->clear
    Remove all elements from the chain.  Note that this implies that
    elements that have no further references are destroyed.  The other
    elements are not affected.

- chain->current: value=any*
    Replace the value of the `current` cell with the argument.

    @see topic Current

- chain->current_no: index=int
    Make the nth (1-based) cell current.  When the argument is 0, there is
    no current cell.

    **Diagnostics**:
    Fails without modifiying the current element if the index is out of
    range.

    @see topic Current
    @see chain<-nth1

- chain->delete: value=any
    Delete the first occurrence of `object` from the chain.  Fails if there
    is no such object.  Note that objects are compared by their object
    identity; not by their contents.  Hence, the following fails because the
    second point is a different instance.

    	?- new(@ch, chain(point(6,6))).
    	Yes
    	?- send(@ch, delete, point(6,6)).
    	No

    See also ->delete_all and ->subtract.

    @see chain->delete_all

- chain->delete_all: value=any
    Delete all elements of the chain that are equal to the argument.  Always
    succeeds.  See also ->delete.

    @see chain->delete

- chain->delete_current
    Delete the current cell.

    @see topic Current

- chain->delete_head
    Delete the first element of the chain.  See also <-head and
    <-delete_head.

    **Diagnostics**: Fails if the chain is empty.

- chain->delete_tail
    Delete the last element of the chain.  Note that chains are single
    linked lists and thus deleting the last implies traversing the entire
    list.

    **Diagnostics**: Fails if the chain is empty.

- chain->empty
    Succeeds if the chain has no elements.  Intended as a test method.

- chain->equal: chain
    Succeeds if both chains have the same elements.  For convenience
    reasons, the type is defined the include @default and @nil and thus this
    method fails silently when a chain is compared to one of these constants
    and fails with a type-error if the argument is neither one of these
    constants, nor an instance of class chain or a subclass thereof.

- chain->find: test=code
    Successively executes code on all elements of the chain.  Succeeds,
    setting `<-current` if code is executed with success.  Fails otherwise.
    Arguments:

    	| @arg1 | The (current) element    |
    	| @arg2 | 1-based index of element |

    New code should consider using <-find, which avoids
    the usage of <-current.

    @see chain->for_all
    @see chain<-find

- chain->for_all: action=code, safe=[bool]
    This method invokes its argument code object on all elements of the
    chain.  During the execution the following argument is bound:

    	| @arg1 | Current element.               |
    	| @arg2 | Index of the element (1-based) |

    The iteration stops with failure if the execution of `code` fails for
    some element of the chain.

    In the example below we print the elements of a chain on the terminal:

    	?- new(@ch, chain(hello, world)).
    	?- send(@ch, for_all, message(@pce, write_ln, @arg1)).
    	hello
    	world

    If the `safe` argument is @on (default), the elements of the
    chain are first copied to a local array and existence of the
    objects in the elements is tested prior to executing the code
    object.  This implies that the chain may be modified by the
    execution of the method and new elements will not be seen.  If
    this argument is @off this method iterates over the chain
    itself.  In this case it is *not* allowed to modify the chain.

    **Defaults**: By default this operation is caried-out `safe` (= @on).

    @see chain<-find_all
    @see chain<-find
    @see chain->find
    @see chain->for_some
    @see hash_table->for_all
    @see device->for_all

- chain->for_some: action=code, safe=[bool]
    Similar to `chain ->for_all`, but continues the iteration even if code
    fails to execute for some element.  Always succeeds.  Forwarded
    arguments:

    	@arg1		element
    	@arg2		1-based index of element

    @see chain->for_all

- chain->initialise: member=any ...
    Create a chain from an initial list of elements.

- chain->insert: value=any
    Insert argument before <-current.  Use of <-current is
    discouraged and new code must consider using ->insert_before.

    @see chain->move_before
    @see topic Current

- chain->insert_after: value=any, after=any*
    @see chain->move_before

- chain->insert_before: value=any, before=any
    Insert first before second object.  If the second object is not
    a <-member of the chain, value is ->append'ed.

    See also ->insert_after.

- chain->member: value=any
    @see chain->add

- chain->move_after: value=any, after=[any]
    Move 1st object just after second.  If the second object
    is omitted, the `value` is made the <-head of the chain.

    See also ->move_before.

    @see chain->move_before
    @see node->move_after

- chain->move_before: value=any, before=any
    Move the first member of the chain to be just before the second.
    Fails if either of the elements is not a ->member or they are
    equal.  To move an element to the start of the chain, you may
    use either of:

    	?- ignore(send(Ch, move_before, Element, Ch?head)).
    	?- send(Ch, move_after, Element).

    See also ->move_after, ->before and ->after.

    **Diagnostics**: Fails silently if either argument is not a ->member of the chain.

    @see chain->insert
    @see chain->before
    @see chain->insert_after
    @see chain->move_after

- chain->sort: compare=[code|function], unique=[bool]
    Sort the elements in the chain.  Two elements are compared by examining
    the return status of the code object.  Both @arg1 and @arg2 are bound to
    some element of the chain.  Execution of the code is supposed to succeed
    if @arg1 must be before @arg2 in the sorted chain.  If `code` is
    a function object, the return status is interpreted as follows:

    - integer smaller then 0 or the atom `smaller`
    	   @arg1 must be before @arg2

    - 0 or the atom `equal`
    	   Both elements are equal (neither is deleted, but their order
    	   is undefined).

    - anything else
    	   @arg1 must be after @arg2

    Common code objects to use are the arithmetic comparison objects and
    messages invoking a string comparison method.  Examples:

    Sort a chain of integers, lowest first:

    	send(Chain, sort, @arg1 < @arg2).

    Sort a chain of char_array objects, alphabetically lowest first:

    	send(Chain, sort, ?(@arg1, compare, @arg2)).

    If `compare` code is omitted, the contents of the chain is
    sorted on alphabetically on their <-print_name.

    If `unique` equals @on, duplicates (i.e.  pairs of objects for
    which executing the comparison yields `equal`) will be removed.
    Default is not to remove duplicates.  See also `chain ->unique`.

    NOTE:	When sorting with a non-function, the result of comparing
    		two element is a boolean and cannot express `equal`.
    		->sort yields an undefined result if the chain contains
    		two or more element for which `code ->forward: A, B` *and*
    		`code ->forward: B, A` yield the same result.  This undefined
    		behaviour can include a fatal error.

    @see char_array<-compare
    @see char_array->smaller
    @see char_array->larger
    @see class binary_condition
    @see vector->sort

- chain->subtract: chain
    Delete all elements in the argument from the receiving chain.
    See also ->delete_all.

- chain->unique
    Remove all duplicates from chain.  Objects are compared by
    reference.  See also `chain ->sort` and `chain ->add`.

- chain->unlink
    First invokes ->clear to remove the cells.


## Get methods {#class-chain-get}

- chain<-_arg: index=int -> any
    @see object<-_arg

- chain<-_arity: -> int
    @see object<-_arity

- chain<-cell_value: cell_reference=int -> any
    Value associated with a cell.  Use with care.

    @see chain<-next_cell
    @see chain<-head_cell
    @see chain->cell_value

- chain<-complete_name: prefix=char_array, extract_name=[function]*, ignore_case=[bool] -> tuple
    Support the implementation of automatic completion as found with class
    text_item.  The chain is a chain of elements from which one has to be
    chosen by means of completion.  _Prefix_ is the string typed so far.
    _Extract_name_ is a function to extract the name from the each element.
    Its values are:

    	| @nil     | Use default conversion to text.          |
    	| @default | Use `object <-print_name`.               |
    	| Function | Use this function, @arg1 is the element. |

    The return value is a tuple object.  The `tuple <-first` contains a new
    chain holding all elements of the receiving string whose name start with
    the given `prefix`.  The `tuple <-second` contains a new string object
    holding the largest common prefix of all matches.  If no element of the
    chain matches prefix, this method fails.  Example:

    	?- get(chain(gnu, foo, gnats, bar), complete_name, 'g',
    		   tuple(Matches, string(Common))),
    	   object(Matches, Chain).

    	Common = gn
    	Chain = chain(gnu, gnats)
    	Matches = @854357/chain

    See also `text_item ->complete`.

- chain<-convert: vector -> chain
    Convert a vector object (array) into a linked list (chain object).
    Because a Prolog list is translated into a code_vector object,
    any method requiring a chain argument can be passed a Prolog
    list.  For example:

    	send(new(B, browser), open),
    	send(B, members, [gnu, gnat, gnome]).

- chain<-copy: -> chain
    Creates a copy of the chain holding the same elements in the same order.

    When the receiver is a subclass of chain, the resulting chain will be an
    instance of the same (sub)class: an instance of this subclass is created
    with no arguments, after which the append behaviour of class chain is
    used to append the elements.

    **Bugs**: Creating copies of subclasses is ill-defined.

- chain<-current: -> any
    Get the value of the `current` cell.  Use of the `current` field
    of chains is discouraged as it makes the code non-reentrant.

    @see topic Current

- chain<-current_no: -> int
    Get index of the current cell.  Fails if the chain has no
    current cell.  See also ->current

    @see topic Current
    @see chain<-index

- chain<-find: test=code -> any
    Successively executes code on the elements.  Returns the first element
    for which code succeeds or fails.  Arguments:

    	| @arg1 | Element tested           |
    	| @arg2 | 1-based index of element |

    @see chain->for_all
    @see chain<-find_all
    @see chain->find
    @see hash_table<-find_key

- chain<-find_all: test=code -> chain
    Returns a new chain holding all elements for which the execution of code
    succeeds.  Arguments:

    	| @arg1 | Element tested           |
    	| @arg2 | 1-based index of element |

    The following example finds all inverted graphicals on the graphical
    device _Dev_:

    	?- get(Dev?graphicals, find_all,
    		   @arg1?inverted == @on, Inverted).

    See also `chain ->for_all`.

    @see chain->for_all
    @see chain<-find

- chain<-head_cell: -> int
    @see chain<-cell_value
    @see chain->cell_value

- chain<-index: value=any -> index=int
    @see chain<-current_no

- chain<-map: function -> chain
    Executes obtainer or block for each element of the chain.  Returns a new
    chain holding the resulting values of executing the code.  All elements
    for which the execution fails are omitted.  Argument forwarding:

    	| @arg1 | Element tested               |
    	| @arg2 | 1-based index of the element |

    The example below returns a chain with names holding the names of all
    graphical objects on the graphical device.

    	graphical_names(Device, Names)  :-
    		get(Device, graphicals, Chain),
    		get(Chain, map, @arg1?name, Names).

- chain<-next: [any] -> any
    Returns the element right after the given argument.  If the
    argument is not in the chain or the argument is the last
    member of the chain, this method fails.   If the argument
    occurs multiple times in the chain, the next of the first
    appearance is returned.

    For backward compatibility only, if no argument is provided,
    <-current is returned and <-current is moved to the next
    element.

    See also <-previous, <-nth1, <-head and <-tail.

    @see topic Current

- chain<-next_cell: cell_reference=int -> cell_reference=int
    @see chain<-cell_value
    @see chain->cell_value

- chain<-nth1: index=int -> value=any
    @see chain->current_no

- chain<-previous: [any] -> any
    Returns the element right before the given argument.  If the
    argument is not in the chain or the argument is the first
    member of the chain, this method fails.   If the argument
    occurs multiple times in the chain, the element before the
    first appearance is returned.  See also <-next, <-nth1, <-head
    and <-tail.

- chain<-sub: start=int, end=[int] -> chain
    Get new sub-chain from 0-based start and end.  End defaults to
    <-size.  The object at <-end is not included.  Example:

    	?- get_object(chain(0,1,2,3), sub, 1, 2, X).

    	X = chain(1)

    See also `string<-sub`.

