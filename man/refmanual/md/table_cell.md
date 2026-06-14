# class table_cell {#class-table_cell}

Cell in a table.  It plays a similar role to class node,
controlling graphical images in a hierarchical layout.

Normally, a table_cell object is created as a side effect
of `table ->append`, appending a graphical to a table.

The table_cell object stores layout information on this
particular cell (spanning, alignment, etc.), as well as
information required by the table object for fast manipulation
of the cells.  Important attributes:

	| ->cell_padding | Space around the <-image          |
	| ->col_span     | # columns spanned                 |
	| ->row_span     | # rows spanned                    |
	| ->halign       | Horizontal alignment of the image |
	| ->valign       | Vertical alignment of the image   |
	| ->selected     | Select the cell.                  |
	| <-column       | 1-based column index (read)       |
	| <-row          | 1-based row index (read)          |


## Instance variables {#class-table_cell-instvars}

- table_cell<-cell_padding: [size]
    Size around contents of the cell.  If @default, this is read
    from the <-table.

- table_cell<-col_span: 1..
    Number of columns spanned.  The default is 1.  Extending the
    ->col_span will destroy cells in the spanned area, while
    contracting the span will created `holes` in the table.  See
    also ->row_span.

- table_cell<-column: int*
    X-location in table environment.  Updated by the table object.
    @nil if the cell is not part of a table.  See also `table <-column`

- table_cell-halign: [{left,center,right,reference,stretch}]
    Horizontal alignment of <-image in cell.  Values:

    - @default
    	Use value from the table_column object.

    - left
    	Left align the area of the <-image in the box.

    - center
    	Center the area of the <-image in the box

    - right
    	Right align the <-image in the box

    - reference
    	Fetch the `graphical <-reference` of all cells in
    	this column having this alignment, and place the
    	graphicals such that the references are horizontally
    	aligned.  For example, one could define a subclass
    	of class text that returns the X-location of a decimal
    	dot as the X-value of the reference, for proper
    	alignment of a column holding numerical data.

    - stretch
    	The image will be made as big as the cell.

    See also `table_column ->halign` and ->valign.

- table_cell<-note_mark: image*
    If present, this image is painted in the top-right corner of the
    table_cell object.  It is intended for marking cells, for
    example for indicating the presence of an annotation to
    this cell.

    The image is part of the background of the cell.  See
    also ->background.

- table_cell<-row: int*
    Y-location in table environment.  Updated by the table object.
    @nil if the cell is not part of a table.  See also `table <-row`.

- table_cell<-row_span: 1..
    Number of rows spanned.  Symetric to ->col_span.

- table_cell<-selected: bool
    Is cell selected?  The selection is used by `table->_redraw_area` to
    draw a box around the selected cells.  See also `table <->selection`.

- table_cell-valign: [{top,center,bottom,reference,stretch}]
    Vertical alignment of <-image in cell.  Symetric to ->halign.


## Send methods {#class-table_cell-send}

- table_cell->background: [colour|pixmap]
    Backround colour for the cell.  In addition to a colour or
    image, backgrounds can also be modified using the ->note_mark.

- table_cell->initialise: graphical
    Create a default table_cell object from the graphical object.
    The graphical becomes the <-image of the cell.  Normally,
    table_cell objects are created implicitely by <-convert.

- table_cell->unlink
    Informs the <-table and removes the <-image from its device.


## Get methods {#class-table_cell-get}

- table_cell<-area: -> area
    Area of the cell (including `table <-cell_padding`) in the
    coordinate system of the device to which the table object is
    connected.  See also `table<-cell_from_position`,
    `graphical<-area` and `graphical->in_event_area`.

- table_cell<-convert: graphical -> table_cell
    Converts a graphical object into a table_cell by creating a
    default table_cell and associating the graphical as the <-image.

    This method ensures graphicals can be appended directly to
    a table.

- table_cell<-table: -> table
    Same as <-layout_manager, but fails of the cell is not part of a
    table, instead of returning @nil.

