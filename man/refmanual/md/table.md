# class table {#class-table}

Class table is a subclass of class layout_manager, dealing with
tabular layout of graphical objects.  The functionality of this
class is modelled after the HTML-3 definition of tables.

A number of classes are involved in the definition of a table:

- Class table
	The layout manager that takes care of the overall
	control of the table.

- Class device (or window)
	A table is a layout_manager, and requires a graphical
	device to manage.  The first step in creating a table is
	to assign a table object to a device object using the
	method `device ->layout_manager`.

- Class table_slice, class table_row and class table_column
	The classes table_row and table_column are used by class
	table to build the grid as well as to store common
	row/column properties.  They are normally created and
	managed by the table object.  The user may wish to get
	a handle to them for deleting a row or column, or
	enumerating the cells in a row or column.

- Class table_cell
	This is a subclass of class layout_interface, and deals
	with the interface between the layout_manager (the
	table) and the managed graphicals.  See also
	`graphical <-layout_interface`.


## Instance variables {#class-table-instvars}

- table<-area: area
    Occupied area on the <-device.  Includes the <-frame and
    <-rules.  Updated by ->compute.

- table<-cell_padding: size
    Space around contents of the cell.  The send-method ->cell_padding
    also accepts an integer, translating this into a size with equal
    width and height.  See also ->cell_spacing, ->frame, ->border.

- table<-cell_spacing: size
    Space between the cells.  Note that by setting this to minus the
    <-border, the borders of adjecent cells will overlap, resulting
    in single lines between the cells.  See also ->cell_padding,
    ->rules, ->frame and ->border.

- table-changed: bool
    A change has been made affecting the layout, so ->compute will
    recompute the layout of the table.

- table<-columns: vector
    Return the vector holdin all table_column objects.  May be used
    to enumerate the defined columns.

    Please note that this vector always exist, but may not contain
    the actual columns.  Appended cells are stored in the <-rows,
    and the column information is updated by ->compute.

- table<->current: point
    Current ->append location.  Used to fill a table left-to-right,
    row-by-row.  See ->append and ->next_row.  May also
    be set explicitely, for example to modify the contents of a
    row with a sequence of ->append messages.

- table<-frame: {void,above,below,hsides,vsides,box}
    Parts of the frame painted.  The definition is according to the
    HTML 3 proposal:

    - void
    	Don't draw the frame

    - above
    	Just draw a single line above the table

    - below
    	Just draw a single line below the table

    - hsides
    	above and below combined

    - vsides
    	Draw vertical lines left and right of the table

    - box
    	Draw a box around the table.

    See also ->rules, controlling the lines around the individual
    cells.

- table<-rows: vector
    Return the vector object holding the table_row objects.  May be
    used to enumerate the rows.  Do not modify the contents of this
    vector directly.  See also <-row and <-columns.

- table<-rules: {none,groups,rows,cols,all}
    Rules painted around cells:

    - none
    	Do not paint any lines around the cells

    - groups
    	Draw a line below/right of each <-column or <-row
    	that has `table_slice ->end_group: @on`.

    - rows
    	Draw a line between each row.

    - cols
    	Draw a line between each column

    - all
    	Put each cell in a box.

    Note that ->frame controlls the lines around the entire table
    object.


## Send methods {#class-table-send}

- table->append: cell=table_cell, x=[int], y=[int]
    Append a cell at the given location.  A default x or y is
    read from the <-current attribute.  Please note that
    `table_cell <-convert` wraps graphical objects into a table
    cell, so graphicals can be appended directly to the table.

    After the ->append, <-current is incremented to the next
    column on this row.  If there is no next column, it will be
    created using <-column.  Use ->next_row to force the
    insertion point to the first column on the next row.

    If the next column on this row points into a `row-spanned`
    cell, the insertion point will be incremented to point to the
    first free column after this row-spanned cell.

- table->border: 0..
    Thickness of border-lines.  Which lines are painted is
    controlled by ->rules and ->frame.

- table->compute
    Compute column and row dimensions, places the graphicals in the
    cells and updates the <-area.  Invoked by `device ->compute` of
    the <-device I'm associated with.

- table->compute_bounding_box
    Compute the bounding box.  Called by the <-device to set the
    bounding box of the <-device (to the <-area of the table
    object).

- table->delete: what=table_cell|table_row|table_column, keep=[bool]
    Delete a cell, row or column.  These objects are fetched using
    <-cell, <-row or <-column.  Deleting rows or columns holding
    spanned cells will deal `naturally` with these cells: There
    location and spanning will be updated such that they span the
    same set of rows/columns.

    If `keep` is @on, the content of the row, column or cell are
    kept.  Otherwise the content is `graphical ->destroy`ed.

    If a column is deleted, all cells that will be deleted from the
    table are attached to the table_column object.  Together with
    `table ->insert_column`, this allows for relocating columns if
    a table without loosing the table-properties.  Note that the
    column should be ->lock_object'ed to avoid destruction
    by the incremental garbage collector.  The following code
    moves column 4 to be the first column:

    	...
    	get(Table, column, 4, Col),
    	send(Col, lock_object, @on),
        send(Tbale, delete, Col, @on),
    	send(Table, insert_column, 1, Col),
    	send(Col, lock_object, @off),
    	...

    See also `table->insert_column`, `table->insert_row`,
    `table->append`, `table_cell ->row_span` and
    `table_cell	->col_span`.

- table->delete_rows: from=[int], to=[int], keep=[bool]
    Delete a rows from the table.  From defaults to the first row
    and to to the last one.  If `keep` is @on the graphicals are
    not deleted from the device.  By default they are deleted
    using `visual ->destroy`.

- table->initialise
    Creates an empty table.  Normally, a table is immediately
    associated to a device object:

    	...
    	new(D, device),
    	send(D, layout_manager, new(T, table)),
    	...

    Next, columns may be specified using <-column, and/or the
    table may be filled using ->append.  Note that explicit
    definition of columns is not required.

- table->insert_column: at=int, new=table_column
    Insert new column at the specified index.  All columns at or
    to the right of this column will be moved 1 position to the
    right.  If the column is inserted in a spanned cell, the
    spanning of this cell is increased by one.  See also
    ->insert_row and ->delete.

- table->insert_row: at=int, row=[table_row]
    Insert new row at the specified index.  Rows with this or
    greater index will be moved down.  Cells spanning this
    row will have their `table_cell <-row_span` incremented.
    See also ->insert_column and ->delete.

- table->next_row: end_group=[bool]
    Modifies <-current to point to the first column of the next row.
    Used together with ->append to fill the table left-to-right,
    row-by-row.

- table->place_cells
    Called by ->compute to deal with the actual placement of
    the graphicals in the cells.

- table->selection: table_cell|chain*
    Specify the selected cell or cells.  There is yet no possibility
    to select a row or column as a unit.

    Please note that selecting a table_cell object is different from
    selecting the graphical image of the cell in the associated
    <-device.

    See also <-selection.

- table->sort_rows: compare=code, from=[int], to=[int]
    Sort rows in indicated range.  Default is to sort all rows in
    the table object.  This ranged may be narrowed using the
    optional _From_ and _To_ arguments.  See `vector->sort`
    for details.

    In the current implementation, none of the sorted rows is
    allowed to contain cells with row-spanning (see
    `table_cell->row_span`) larger than 1.   If such cells
    are encountered, the error spanned_row is raised.

- table->stretched_column: column=table_column, width=int
    [Virtual] A column has been stretched to this width.  This
    method may be defined to adjust the content of the affected
    cells.

    See also ->stretched_row.

- table->stretched_row: column=table_row, height=int
    [Virtual] A row has been stretched to this width.  See
    ->stretched_column for details.

- table->width: [0..]
    Total width of the table.  If @default, the width is computed
    from the desired widths of the columns.  If specified, the
    columns are distributed over the given width.

    See also `table_slice->rubber`.


## Get methods {#class-table-get}

- table<-cell: x=int|name, y=int|name -> table_cell
    Fetch the table_cell object at the given column/row coordinate.
    Cordinates are 1-based.  Fails silently if the table has no cell
    at the given coordinate.   See also <-row, <-column, ->append.

- table<-cell_from_position: at=point|event, allow_border=[bool] -> table_cell|point
    Translate coordinate to cell or point.  If the table contains a
    cell at the given location, this cell will be returned.
    Otherwise, if the location is within the column/row range of the
    table, a point will be returned containing the coordinates
    of the (empty) cell.  This may be used to create a cell at the
    indicated location.   For example:

    	start_typing(Table, Ev:event) :->
    		get(Table, cell_from_position, Ev, CellOrPos),
    		(   send(CellOrPos, instance_of, table_cell)
    		->  send(CellOrPos?image, keyboard_focus, @on)
    		;   object(CellOrPos, point(X, Y)),
    			send(Table, append,
    			     new(T, editable_text('')), X, Y),
    			send(Table, compute), % force display now
    			send(T, keyboard_focus, @on)
    		).

- table<-cells_in_region: area -> chain
    Find all cells in a row/column region.  Handy for operations on
    a rectangular subpart of the table.  See also <-cell, <-row or
    <-column.

- table<-column: x=int|name, create=[bool] -> table_column
    Fetch the table_column object at the given 1-based column
    index or with the given name. If _Create_ is @on, a new column
    is created and attached to the table if the specified n-th
    column is not yet defined.

    Normally used to specify the column attributes:

    	...,
    	get(Table, column, 1, @on, Col),
    	send(Col, halign, center),
    	...

    Access to the table_column object is can also used with
    `table ->delete` to delete a column from the table.

    See also <-row, `table_slice->name`.

- table<-row: y=int|name, create=[bool] -> table_row
    Fetch the table_row object at the given 1-based row
    index or with the given name.  If _Create_ is @on, a new row is
    created and attached to the table if the specified n-th row is
    not yet defined.

    Normally used to specify the row attributes:

    	...,
    	get(Table, row 1, @on, Col),
    	send(Col, valign, center),
    	...

    Access to the table_row object is can also used with
    `table ->delete` to delete a row from the table.

    See also <-column.

- table<-selection: -> chain
    Chain holding selected cells.  See also ->selection.

