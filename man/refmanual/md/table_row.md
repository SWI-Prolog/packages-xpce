# class table_row {#class-table_row}

Subclass of class table_slice, representing a row of a table
object.  A table object stores the table_cell objects in
instances of this class, which in turn are stored in the `table <-rows`
vector.  The table_row objects are primarily used for ->valign
and `table ->delete`.


## Instance variables {#class-table_row-instvars}

- table_row<-alignment: {top,bottom,center,reference,stretch}
    See `table_column <-alignment`.


## Send methods {#class-table_row-send}

- table_row->compute
    Compute dimensions of the row.  See `table_column->compute` for
    details.

- table_row->initialise: [{top,bottom,center,reference,stretch}]
    Create a table_row object from its alignment.  Normally, these
    objects are created by `table <-row`.

- table_row->unlink
    Properly unregister the row from the <-table.

- table_row->valign: {top,bottom,center,reference,stretch}
    *Inherits description from*: table_row-alignment


## Get methods {#class-table_row-get}

- table_row<-cell: int|name -> table_cell
    Cell at indicated column.  Fails if the row has no cell at the
    indicated location.  See also `table <-cell` and `table_column<-cell`.

- table_row<-valign: -> {top,bottom,center,reference,stretch}
    *Inherits description from*: table_row-alignment

