# class resize_table_slice_gesture {#class-resize_table_slice_gesture}

Gesture that lets the user resize the columns or rows of a `table`
by dragging the line between two cells.  The gesture is attached to
the device of a table-laid-out `picture`; depending on `<-mode` it
resizes columns or rows.

The press on a row/column boundary becomes a drag that sends
`table->user_resize_slice: Slice, Size` to the table, where `Slice`
is the `table_row` or `table_column` being resized and `Size` is the
distance between the current event and the slice's
`<-position`.

A typical setup that allows the user to resize columns by dragging
their boundaries:

    new(P, picture('My Table')),
    send(P, layout_manager, new(_T, table)),
    send(P, recogniser, resize_table_slice_gesture(column)),

@see class table
@see class table_row
@see class table_column


## Instance variables {#class-resize_table_slice_gesture-instvars}

- resize_table_slice_gesture<->mode: {column,row}
    Whether the gesture resizes columns or rows.

- resize_table_slice_gesture<-row: int*
    Index of the row currently being resized, or @nil.

- resize_table_slice_gesture<-column: int*
    Index of the column currently being resized, or @nil.

- resize_table_slice_gesture<->min_size: size*
    Minimum size of the row/column; the drag is clipped so that the
    slice never falls below this.


## Send methods {#class-resize_table_slice_gesture-send}

- resize_table_slice_gesture->initialise: mode={column,row}, button=[button_name], modifier=[modifier]
    Create the gesture in column- or row-resize mode, bound to the
    given button and modifier.

- resize_table_slice_gesture->verify: event
    Succeed when the device has a `table` layout manager and the
    pointer is near a row/column boundary.

- resize_table_slice_gesture->initiate: event
    Set the resize cursor and warp the pointer to the boundary.

- resize_table_slice_gesture->drag: event
    Send `table->user_resize_slice` with the new size.

- resize_table_slice_gesture->terminate: event
    Equivalent to `->drag`.


## Class variables {#class-resize_table_slice_gesture-classvars}

- button: middle
    Button on which the gesture is active.

- margin_fraction: 4
    Pointer must be within 1/<value> of a slice's size from the
    boundary to activate.

- margin_width: 15
    Absolute pixel cap on the boundary "grab zone".

- min_size: size(10,10)
    Lower bound when dragging.
