# class rubber {#class-rubber}

A rubber object is associated to `hbox <-rubber` to declare the
stretchability of the box as well as its possibility to break
lines.  See class parbox for an overview of the document
rendering primitives.

Attributes:

- <-level
	After collecting the hbox objects for a line, all boxes
	that share the highest <-level are used to distribute
	the remaining horizontal space on the line.  Values
	used are:

		1	spaces
		2	alignment
		3	stretching lines, etc.

- <-shrink
	Difficulty to get smaller.  0 implies the box does not
	want to get smaller (default).  Higher values represent
	more ease to get smaller.

- <-stretch
	Difficulty to get wider.  0 impliesthe box does not want
	to get wider (default).  Higher values represent more
	ease to get wider.

- <-linebreak
	Possibility to break a line

		| @nil  | Cannot start a new line |
		| allow | May start a new line    |
		| force | Must start a new line   |

- <-natural
	Defined natural size.  If @default, the size
	is determined from the content.

- <-minimum
	Minimum size.  Try as hard as possible to avoid
	getting smaller.

- <-maximum
	Maximum size.  Try as hard as possible to avoid
	getting larger.

See also @space_rubber.


## Send methods {#class-rubber-send}

- rubber->initialise: level=[1..], stretch=[0..], shrink=[0..], linebreak=[{allow,force}]*
    Create a rubber object initialising ->level, ->stretch, ->shrink
    and ->linebreak.   Other useful properties are:

    	| ->maximum: | Maximum size of object   |
    	| ->minimum: | Minimum size of object   |
    	| ->natural: | Preferred size of object |

