# class parbox {#class-parbox}

Class parbox is the central class of the `document rendering`
primitives of XPCE.  These classes are used to visualise a
mixture of text and graphics.  They realise the common document
rendering primitives such as paragraphs, embedded graphics,
list-making environments, tables, etc.    Below is a brief
summary of these classes:

- class parbox
	Subclass of class device, rendering text as paragraphs,
	mixed with graphics.

- class hbox
	_Horizontal box_: a basic element of a parbox object.
	It is a superclass of class tbox and class grbox.  The
	class is used for layout (paragraph separators, spaces,
	etc.).

- class tbox
	Subclass of class hbox, rendering some text (represented
	using a string object or a name object) with a style
	object (defining font, colour, etc.).

- class grbox
	Wraps any XPCE graphical object into an hbox object.
	Graphicals may be defined as `floating objects`: placed
	at the left or right margin with the text floating
	around the graphical.

- class lbox
	Subclass of device providing a `list`.  A list is a set
	of list items, each of which consists of a label and a
	body.  Class lbox defines the layout of these items.
	It can be used to create bullet-lists, definition lists,
	etc.

- class table
	This layout manager can be used to define tabular
	layout.

Two classes support the above:

- class rubber
	Defines stretchability and ability to break lines for
	hbox objects.

- class style
	Defines font and other visual attributes for text in
	tbox objects.


## Instance variables {#class-parbox-instvars}

- parbox<-alignment: {left,right,center,justify}
    Alignment of text in box.  After ->compute has filled a line,
    this attribute defines the placements of the hbox objects in the
    physical line.   The meaning of `left`, `right` and `center` are
    obvious.  The value `justtify` implies a straight right and left
    margin.

- parbox<-auto_crop: bool
    If @on, the width of the parbox as a graphical object is made to
    fit exactly the content.  If @off (default), the width is the
    minimum of the <-line_width and the width of the content.

    Auto cropping is used mostly for defining table-cells, where the
    natural width of the cell is (initially) determined by the
    content.

    If ->geometry is invoked with an explicit `width`, <-auto_crop
    is automatically set to @off, trying to fullfil the requested
    width as closely as possible.


## Send methods {#class-parbox-send}

- parbox->alignment: {left,right,center,justify}
    Alignment of text in box.  After ->compute has filled a line,
    this attribute defines the placements of the hbox objects in the
    physical line.   The meaning of `left`, `right` and `center` are
    obvious.  The value `justtify` implies a straight right and left
    margin.

    *Inherits description from*: parbox-alignment

- parbox->append: hbox
    Append a hbox.  See also ->cdata.

- parbox->auto_crop: bool
    *Inherits description from*: parbox-auto_crop

- parbox->cdata: cdata=string, style=[style], space=[hbox], ignore_blanks=[{none,leading,trailing,both}]
    Append a text by breaking it into words and appending a sequence
    of tbox objects and hbox objects representing spaces.  The name
    of this method is inspired by SGML, where CDATA is used to
    declare `character data`.

    If `ignore_blanks` is leading, first all leading blank characters are skipped.
    Words encountered in the string are added as instances of the
    class tbox using the specified style.  Sequences of spaces, tabs
    and newlines are mapped to a single hbox object.  If the `space`
    argument is provided, this is used for representing these
    spaces, otherwise a default stretchable hbox object is created
    with the dimensions of a space in the current font.

    See also ->append, `tbox->initialise` and class hbox.

- parbox->geometry: x=[int], y=[int], width=[int], height=[int]
    Modify the size and location of the parbox object.  The <-height
    of a parbox is always defined by the <-line_width and the
    content.  If `width` is specified, the parbox changes its
    <-line_width trying to fullfil the requested width.  It will
    then adjust the <-width and <-height accordingly.

    Specifying an explicit <-width sets <-auto_crop to @off.

- parbox->initialise: width=[int], alignment=[{left,center,right,justify}], content=hbox ...
    Create a new parbox object from its <-line_width and
    <-alignment.

- parbox->line_width: int
    Maximum width of a textline in the parbox object.  This method
    is automatically invoked of the ->geometry of the parbox is
    changed.

    If the ->line_width is changed, the parbox will sent the message
    `graphical ->container_size_changed: @default, Width` to each
    embedded graphical object.  This mechanism allows for defining
    the width of a graphical in terms of the width of the paragraph.


## Get methods {#class-parbox-get}

- parbox<-find: code -> tuple
    Recursively traverse the parbox object and parboxes displayed on
    them (possibly embedded in one or more device layers), applying
    Considition to each hbox object encountered using the following
    parameters:

    	| @receiver: | Considered parbox           |
    	| @arg1      | Considered hbox             |
    	| @arg2      | Index of @arg2 in @receiver |

    On success, a tuple object containing the parbox and index of
    the hbox  are returned.

    Typically, this method is used to locate `anchors`.  Class hbox
    is therefore subclassed and decorated with identifying
    information for the anchor.  <-find is then used to locate an
    instance with the desired property of the anchor class.

    See also <-box_area.

- parbox<-minimum_width: -> int
    Return `hbox<-width` of largest hbox object in paragraph.  It is
    not desirable to make the paragraph smaller (see ->line_width)
    than this size.

    Introduced to support the compuation of column widths in table
    objects whose cells are filled with parbox objects.

