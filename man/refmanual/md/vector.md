# class vector {#class-vector}

A `vector` is a one-dimensional array.  Vectors act as dynamic arrays:
values can be stored at any index.  The initial arguments are stored
from index 1.

The amount of memory used to store a vector is determined by the
difference between the lowest index and the highest index.  In many
applications, sparse arrays are better realised using class hash_table.

@see class chain
@see class table


## Instance variables {#class-vector-instvars}

- vector-elements: alien:Any *
    Alien array of elements.  Note that this may be moved if number of
    elements of the vector is changed.

- vector<-offset: int
    Offset of the vector relative to a 1-based vector.  E.g. if offset
    equals 2, the first element is at index 3.

- vector<-size: 0..
    Total number of elements in the vector.


## Send methods {#class-vector-send}

- vector->clear
    Delete all elements, as well as the allocated array.  After this
    operation, the vector has the same status as a vector just
    created from an empty argument list.

- vector->element: index=int, value=any
    Set element N (1-based) to the specified value.  N is outside the array,
    the array is automatically enlarged.  The `skipped` elements are filled
    with @default.

- vector->fill: value=any, from=[int], to=[int]
    Fill range of elements with the specified value.  `from` and `to`
    default to <-low_index resp.  <-high_index.  This is a common way to
    create a vector with a specific range.

- vector->for_all: code=code, from=[int], to=[int]
    Run's code on all elements of the vector.  Fails if code fails for some
    element.  Succeeds after all elements have been processed.  Arguments:

    	| @arg1 | The current element   |
    	| @arg2 | Index of this element |

    Its behaviour is undefined if the size of the vector is changed by code.

    Using the `from` and `to` arguments, the range as well as the
    order of visited elements can be reduced.  Using only `from`,
    the vector is enumerated starting at the given index..  Using
    only `to`, the vector is enumerated from <-low_index to the
    given index.  Using both `from` and `to`, the vector is
    enumerated in the given range.  If `to` < `from` the enumeration
    is executed backwards.

    See also ->for_some, <-find and <-find_all.

- vector->for_some: code=code, from=[int], to=[int]
    Run's code on all elements of the vector.  Ignores the exit status of
    code and always succeeds. Arguments:

    	| @arg1 | The current element   |
    	| @arg2 | Index of this element |

    Its behaviour is undefined if the size of the vector is changed by code.

    Using `from` and `to`, the range and order can be defined.  See
    ->for_all for details.

- vector->initialise: element=any ...
    Create a vector from the initial arguments.  The first argument is
    placed at index 1, the second at 2, etc.

    **Bugs**:
    It would be useful to be able to create a vector with a specified low
    and high index.  To create a vector [5...20] do:

    	new(V, vector),
    	send(V, fill, @nil, 5, 20).

- vector->shift: places=int
    Shift the elements by <n> places.  The `freed` places are filled with
    @nil:

    	?- new(V, vector(gnu, gnats)),
    	   send(V, shift, 1),
    	   object(V, Term).

    	Term = vector(@nil, gnu)

- vector->sort: compare=code, from=[int], to=[int]
    Sort the elements in the vector.  Argument passing is equivalent to
    `chain ->sort`.  _From_ and _To_ may be used to sort only part
    of the vector.  _From_ defaults to <-low_index, and _To_ to
    <-high_index.  If _To_ <= _From_, ->sort succeeds without
    any side-effects.

    @see chain->sort

- vector->unlink
    Unreferences the -elements.  All elements that have no further
    references will be destroyed by PCE's incremental garbage collector.


## Get methods {#class-vector-get}

- vector<-_arg: int -> any
    Get the n-th argument of the term description.  Argument 1 is the
    argument at the lowest defined index.

- vector<-_arity: -> int
    Get the number of arguments of the term-description.  This is the total
    number of elements in the vector.

- vector<-copy: -> vector
    Creates a copy of the vector with the same elements and <->offset.  The
    copy also is of the same class as the original, which implies that the
    ->initialise method of this subclass must accept the same arguments as
    class vector.

- vector<-element: index=int -> any
    Get the nth (1-based) element of the vector.  Fails silently when
    outside the defined range.

- vector<-find: code=code, from=[int], to=[int] -> unchecked
    Run code on each element of the vector until it succeeds.  Return the
    element for which it succeeded.  Arguments:

    	| @arg1 | The current element  |
    	| @arg2 | Index of the element |

    Using `from` and `to`, the range and order can be defined.  See
    ->for_all for details.  For example, to find the last element
    satisfying `code`, use:

    	find_last(Vector, Code, Element) :<-
    		get(Vector, high_index, End),
    		get(Vector, low_index, Start),
    		get(Vector, find, Code, End, Start, Element).

    @see vector<-index
    @see vector<-rindex

- vector<-find_all: code=code, from=[int], to=[int] -> unchecked
    Return a new chain holding all elements for which code could be
    successfully executed.  Arguments:

    	| @arg1 | The current element |
    	| @arg2 | The index           |

    Using `from` and `to`, the range and order can be defined.  See
    ->for_all for details.

- vector<-high_index: -> int
    The highest index for which <-element succeeds.  This is <-offset +
    <-size.

    @see vector<-low_index

- vector<-index: any -> int
    Searches the vector from <-low_index, returning the first index that
    holds the argument object.

    See also `chain->member` and <-rindex.

    @see vector<-find
    @see vector<-rindex

- vector<-low_index: -> int
    Lowest index for which <-element succeeds.  This is <-offset + 1.

    @see vector<-high_index

- vector<-rindex: any -> int
    Scan the vector starting at <-high_index working downwards and return
    the highest index that contains the argument object.  See also <-index.

    @see vector<-find
    @see vector<-index

