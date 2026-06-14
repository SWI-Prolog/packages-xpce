# class hash_table {#class-hash_table}

The PCE class hash_table implements a fast associative table.
Hash-tables are commonly used to map symbols on values.  For example,
PCE itself uses the hash_table @classes to map class names onto class
objects.

PCE hash_tables are implemented as `closed` hash_tables.  The number of
`buckets` in enlarged automatically as the number of symbols in the
table grows.

Note that hash_tables implement the standard set operations ->for_all,
->for_some, <-find_key and <-find_value.

See also class chain_table (mapping a key onto multiple values),
class sheet and class table (multi-column indexed table).

@see class sheet
@see class table


## Instance variables {#class-hash_table-instvars}

- hash_table-buckets: alien:int
    Number of buckets in the table.  Automatically doubled if the table is
    filled over 2/3-th.

    NOTE:	The number of buckets is a power of 2.  Hash-table literature
    	claims prime numbers to provide a better distribution.  Timing
    	on the SUN-SPARC station suggests this does ot compensate
    	for the difference between a division and a masking operation.

- hash_table-refer: {none,name,value,both}
    Hash_tables are used internally by PCE in algorithms where the table
    should not give a reference count to the objects in it.  Internal use
    only.

- hash_table<-size: int
    Number of symbols in the table.

    @see hash_table->empty

- hash_table-symbols: alien:Symbol
    Alien pointer to the symbols.


## Send methods {#class-hash_table-send}

- hash_table->append: key=any, value=any
    Append a new association to the table.  The first argument is the `key`;
    the second is the associated value.  If the key was already in the
    table, associated value is changed.

- hash_table->buckets: int
    Set the number of buckets in the table.  This method might be useful
    for two purposes:

    - When the number of elements in known in advance, setting
    	   the number of buckets avoids rehashing.

    - The number of buckets is not minimised if members are
    	  deleted or the table is cleared.  The ->buckets method
    	  may be used to make the table smaller.

    The next legal value (large enough and a power of 2) is used.

- hash_table->clear
    Delete all entries from the table.  After ->clear, <-size equals 0.  The
    number of buckets is *not* changed.

- hash_table->delete: key=any
    Delete association with given key.  If there is no such association,
    ->delete fails.

- hash_table->empty
    Test if <-size equals 0.

    @see hash_table-size

- hash_table->for_all: action=code, safe=[bool]
    Run code on all elements of the table.  If code fails for some symbol,
    this method immediately exists with failure.  Arguments:

    	| @arg1: | Key of the symbol   |
    	| @arg2: | Value of the symbol |

    Note that the order in which the symbols are presented to the code is
    not specified.

    @see chain->for_all
    @see hash_table->for_some

- hash_table->for_some: action=code, safe=[bool]
    Run code on all elements of the table.  The return status of code is
    ignored.

    	| @arg1: | Key of the symbol   |
    	| @arg2: | Value of the symbol |

    Note that the order in which the symbols are presented to the code is
    not specified.

    @see hash_table->for_all

- hash_table->initialise: buckets=[int]
    Create a hash_table.  The argument is the number of buckets.  Note that
    this will be automatically increased if necessary.  Normally it is not
    specified.

    **Defaults**: 8

- hash_table->unlink
    Clears the table and deallocates the alien symbol array.


## Get methods {#class-hash_table-get}

- hash_table<-buckets: -> buckets=int
    Value of the alien variable -buckets, indicating the current number of
    buckets in the table.  A power of 2.

- hash_table<-find_key: test=code -> key=any
    Run code on all elements of the table until it succeeds.  Then return
    the key of the symbol for which code succeeded.  Arguments:

    	| @arg1: | Key of the symbol   |
    	| @arg2: | Value of the symbol |

    Fails if there is no symbol for which code succeeds.  Note that the
    order in which the symbols are presented to the keys is not specified.
    See also <-find_value.

    @see chain<-find
    @see hash_table<-find_value

- hash_table<-find_value: test=code -> value=any
    Run code on all elements of the table until it succeeds.  Then return
    the value of the symbol for which code succeeded.  Arguments:

    	| @arg1: | Key of the symbol   |
    	| @arg2: | Value of the symbol |

    Fails if there is no symbol for which code succeeds.  Note that the
    order in which the symbols are presented to the keys is not specified.

    @see hash_table<-find_key

- hash_table<-member: key=any -> value=any
    Get value associated with some key.  This method fails if there is no
    value associated.

