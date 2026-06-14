# class chain_table {#class-chain_table}

A chain table is similar to a hash-table, but allows multiple values to
be associated with the same key.  Each symbol-value is a chain holding
all objects associated with the symbol-key.

NOTE:	In earlier versions, class hash_table supported the option
	`unique`.  The interface to non-unique hash-tables however
	was clumsy, while the performance of normal hash-tables
	was harmed by this option.  Chain_tables are a much cleaner
	solution to the same problem.


## Send methods {#class-chain_table-send}

- chain_table->add: key=any, value=any
    Add association to table if this association is not already part
    of the table.   See also ->append.

- chain_table->append: key=any, value=any
    Append association to the table.  If this is a new key, associate a new
    chain holding the value with this key.  Otherwise append the value to
    the chain associated to key.  See also ->add.

- chain_table->delete: key=any, value=[any]
    Delete all associations with the given key.  If `value` is specified,
    the first occurrence is deleted from the associated chain.  If the
    chain then becomes empty the entire association is deleted.

    Fails is key is not in the table, or a value is specified and not in the
    table.

- chain_table->prepend: key=any, value=any
    As ->append but, if there is already a chain associated to `key`, the
    value is added to the start of the chain rather than to the end.

