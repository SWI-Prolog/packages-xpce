# class format {#class-format}

A format object can be used to turn a device object into a table
of graphicals.  Two modes are defined.  In `column` mode, the
graphicals are aligned in a two-dimensional grid that is
(depending on the orientation) `n` columns width or high.  In
`non-column` mode, if aligns the graphicals in rows, very
similar as words are placed in a paragraph of text with no
aligned right margin.

A format object is just a data-object.  It is interpreted by attaching
it to a graphical device using `device ->format`.

Examples:

- format(horizontal, 300, @off)
		aaa bbb ccccccc ddd eeee
		ff ggggggg hhhhhh

- format(horizontal, 2, @on)
		aaa      bbbbbbbbb
		ccccccc  dddd
		eeee

- format(vertical, 2, @on)
		aaaaa   cccc   eeeee
		bbbbb   dddd

**Bugs**:

The place of a graphical in the grid/row is determined by its
position in the <-graphicals chain of the figure.  This makes it very
difficult to modify the contents a formatted figure and get the
graphicals at the right position in the grid.

There are no provisions for `headers`; similar to the `multicolumn`
mechanism of many text-formatters.

There is not way to draw lines between rows/columns.

They are unnecessary complicated and very confusing.

So, in short, a better alternative will be implemented some day.  This
probably will in the form of one or more subclasses of class figure.
The functionalily provided by the <->format machanism will remain
however.

@see device->format
@see device-format
@see device-bad_format


## Instance variables {#class-format-instvars}

- format<->adjustment: vector*
    Applicable if <-columns equals @on.  When present it should be a vector
    with the same number of elements as <-width.  Each element is one of the
    names `left`, `center` or `right` and determines the placement of the
    graphical objects in their cell.  If <-direction = vertical `left` and
    `right` should actually be read as `top` and `bottom`

    The following creates a three column table with the first column aligned
    to the left, the middle centered and the last to the right:

    	?- new(D, device),
    	   new(F, format(horizontal, 3, @on)),
    	   send(F, adjustment, vector(left, center, right)),
    	   send(D, format, F),

    	%  fill the table.
    	   send_list(D, display,
    				 [ '1.1', '1.2', '1.3',
    				   '2.1'
    				 ]).

    See also ->column_sep and ->row_sep.

- format<->column_sep: int
    Distance between columns.  If <-columns = @off this is the distance
    between `words of the paragraph`.  The default is 10 pixels.
    See also ->row_sep.

- format<->columns: bool
    If @on, the graphical objects of the device are placed in a two
    dimensional grid.  When @off the graphicals are places like words in a
    paragraph of text.

- format<->direction: {horizontal,vertical}
    When horizontal and <-columns equals @off the `words` are printed as
    the english do. `vertical` prints as the chinese do.

    If <-columns = @on, `horizontal` produces vertical columns.

- format<->row_sep: int
    Distance between rows.  With <-direction = vertical this is the distance
    between columns.  The default is 10 pixels.

    See also <->column_sep and <->adjustment.


## Send methods {#class-format-send}

- format->initialise: orientation=[{horizontal,vertical}], width=[1..], columns=[bool]
    Create a format object.  A format object may be attached to a
    graphical device object using `device->format` to achieve simple
    table layout.  Arguments:

    - <-direction
    	If `horizontal`, the table consists of vertical columns
    	and `width` describes the horizontal width or number
    	of columns

    - <-width
    	If `columns` is @on, the number of columns to create.
    	Otherwise the with in pixels in which the formatting
    	needs to be done.

    - <-columns
    	If @on, make a table, otherwise format as words are
    	formatted in a paragraph.

    The <-adjustment variable describes the adjustment of the
    elements in their column.

