# class table_slice {#class-table_slice}

Class table_slice is a super-class of class table_row and class
table_column for implementation purposes.  The user shall
normally never create instances of this class directly.


## Instance variables {#class-table_slice-instvars}

- table_slice<-alignment: {top,bottom,left,right,center,reference,stretch}
    Default alignment of cells.  This variable is refined by
    `table_column <->halign` and `table_row <->valign`, which should
    be used by the application programmer for specifying row/column
    alignment.

- table_slice<-displayed: bool
    Determines whether the row/column is actually displayed in the
    table.  The default is @on.  The cells of a not-displayed row
    are considered to determine the column-widths, but the row
    is not displayed.  Same holds for not-displayed columns.

    This feature is suitable for quickly inserting and deleting
    rows/columns, for example to realise scrolling of not-to-big
    tables while maintaining title rows, collapsing/expanding groups
    of rows/columns, etc.

    BUGS: Hiding rows/columns holding spanned cells may yield
    unexpected results.

- table_slice<-end_group: bool
    If the associated <-table has `table->rules: groups`, a line
    will be painted below (table_row object) or right (table_column
    object) of the slice.

- table_slice<-index: int
    The column-index for a table_column object, and the row-index
    for a table_row object.  Assigned and maintained by the <-table.

- table_slice<->name: name*
    Name of the column/row.  Used with `table <-row` and
    `table<-column` to get access to a row/column by name.  Default
    is @nil, implying the row/column cannot be accessed by its name.

- table_slice<-position: int
    Offset of this row/column to the origin of the device.
    Maintained by `table->compute`.  See also <-width,
    <-reference and <-index.

- table_slice<-reference: int
    If the slice holds cells aligned by there reference (see
    `table_cell <-halign`), this value is the location of the
    reference point relative to the top/left of the row/column.

    See also <-width.  Set by the `table_row ->compute` or
    `table_column->compute`.

- table_slice<-rubber: rubber*
    Rubber object that describes the minimum and maximum dimension
    as well as the stretch- and shrinkability of the slice.  If
    @nil, the natural width of a column is the maximum of the widths
    of the contained cells and the stretch and shrinkability are
    both 100.

    See also `table->width`.

- table_slice<-selected: bool
    If @on, all cells in the row/column are selected.  For example,
    to select a named column in a table, do:

    	...,
    	get(Table, column, Name, Col),
    	send(Col, selected, @on),
    	...

    To deselect all columns in a table, use

    	...,
    	send(Table?columns, for_all,
    		message(@arg1, selected, @off)),
    	...

    See also `table_cell ->selected`.

- table_slice<-table: table*
    Table this row/column is part of.

- table_slice<-width: 0..
    Total width of the row/column, set by `table_row->compute` or
    `table_column->compute`.  See also <-reference.


## Send methods {#class-table_slice-send}

- table_slice->initialise
    Initialises an empty table_slice object.

