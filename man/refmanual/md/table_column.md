# class table_column {#class-table_column}

Subclass of class table_slice, representing a column of a table
object.  Unlike class table_row, a column does not actually hold
table_cell objects.  It is used to store default alignment
information on the column, address the column, find cells in the
column (which actually uses the `table <-row` information to
find the requested cell) and compute the layout of the column.

Probably the only interesting user-methods are ->halign, to et
default alignment for this column and `table ->delete: TableColumn`
to delete an entire column from a table object.


## Instance variables {#class-table_column-instvars}

- table_column<-alignment: {left,right,center,reference,stretch}
    Default alignment of cells.  Refinement of `table_slice <-alignment`,
    deleting top/bottom from the value-set.  <->halign provides the
    access to this method.


## Send methods {#class-table_column-send}

- table_column->compute
    Compute dimensions of the column.  Invoked by `table->compute`
    to set <-width and <-reference, after which `table->compute`
    assigns the column a <-position.

- table_column->halign: {left,right,center,reference,stretch}
    *Inherits description from*: table_column-alignment

- table_column->initialise: [{left,right,center,reference,stretch}]
    Initialises an empty table_column object from its alignment.
    Normally, a table_column is created using `table <-column: index`.

- table_column->unlink
    Ensures the table_column object is nicely removed from the
    <-table.


## Get methods {#class-table_column-get}

- table_column<-cell: int -> table_cell
    Cell at indicated row.  Actually fetches the `table <-row` at
    the given index and using `table_row <-cell` to find the cell.

- table_column<-halign: -> {left,right,center,reference,stretch}
    *Inherits description from*: table_column-alignment

