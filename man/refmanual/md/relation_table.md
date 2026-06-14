# class relation_table {#class-relation_table}

Class table defines a multi-column table with various indexing
mechanisms.  Each row of the table is formed by a vector object.
The rows are not ordered and rows (vectors) may be selected
quickly using index tables.   Each column may be declared in
three modes:

	| unique-key | Indexed and no duplicates         |
	| key        | Indexed, possible duplicates      |
	| otherwise  | Not indexed (may have duplicates) |

The following example defines a  _1 to n_ mapping
between `persistent` objects (called `p`) and their visualisations
(called `v`):

	?- new(T, table(vector(p, v),
					vector(unique, key)).

@see class hash_table
@see class vector


## Instance variables {#class-relation_table-instvars}

- relation_table<-keys: vector
    Vector that describes the column properties.  Each element of this vector
    is one of the names `key`, `unique` or `data`:

    - key
    	The column is used to find association vectors (rows), but may
    	have multiple rows with the same value for this column.  The
    	table created a chain_table object for fast access to all rows
    	with some specified key value.

    - unique
    	Each row in the table has a different value in this column and
    	this column is used for indexing.  The table creates a
    	hash_table object for fast access.

    - data
    	This column just holds associated data.  No (fast) lookup is
    	provided.

- relation_table<-names: vector
    Names of the columns.   Documentation purpose and used by <-vectors.

- relation_table<-tables: vector
    The hash_table objects or chain_table objects for fast access on the
    specified column.  See <-keys.


## Send methods {#class-relation_table-send}

- relation_table->append: association=vector
    Add a row to the table.  The argument should be a vector object with the
    first element at index 1 and the same number of elements as the width of
    the table.  The vector will be recorded in the <-tables as defines by
    <-keys.

- relation_table->delete: association=vector
    Delete the argument row.  Succeeds always, even if the vector is not a
    row of the table.

- relation_table->initialise: names=vector, keys=vector
    Create a multi-column table.  The first vector defines the names of the
    columns.  The second defines the lookup capabilities for the columns.
    Both vectors should start at index 1 and have the same number of
    elements.  See also <-keys.


## Get methods {#class-relation_table-get}

- relation_table<-match: pattern=vector -> associations=chain
    Return a new chain with rows (vector objects) matching the argument
    vector.  A row is supposed to match iff

    1. The vectors have the same range of elements

    2. All elements are pairwise equal or the argument vector has
    	@default for each index they are not pairwise equal.

    This method exploits the available indexing <-tables.  See also
    <-vectors.

- relation_table<-members: -> chain
    New chain holding all rows (vector objects).  Note that the order of
    these rows is not defined.

- relation_table<-vectors: column=name, value=any -> chain|vector
    If `name` is the name of an indexed column, return the row (vector
    object) with the specified `value` in the given column.  If the columns
    is `unique` a single vector will be returned.  Otherwise a chain with
    matching vectors will be returned.  Fails silently if no match can be
    found.

