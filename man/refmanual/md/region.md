# class region {#class-region}

A region object defines a sub-area of an area, expressed using function
objects to denote the X, Y, W and H of the sub-area in terms of the
dimensions of the main area.

Regions are used by handler objects to denote the handler should only
respond when an event object occurred in some sub-area of a graphical.
As handler objects have become old-fashioned since the introduction of
class gesture this class too is seldomly used.

For example, the region

	region(x, y, w/10, h/10)

Denotes a region with the same origin as the area it is related to,
extending to 10% in both directions.

@see class expression
@see class area
@see class handler


## Instance variables {#class-region-instvars}

- region<->height: expression
    Expresses the named dimension of the region in terms of the dimensions
    of the area (see ->inside).   The expression is passed the variables
    `x`, `y`, `w` and `h` denoting the dimensions of the area.  See also
    class binary_expression.

- region<->width: expression
    *Inherits description from*: region-height

- region<->x: expression
    *Inherits description from*: region-height

- region<->y: expression
    *Inherits description from*: region-height


## Get methods {#class-region-get}

- region<-area_width: area -> int

- region<-area_x: area -> int

- region<-area_y: area -> int

- region<-area_height: area -> int
    Dimension of the described region for the specified area.  Example:

    	?- get(region(x+w/2, y+h/2, w/2, h/2), area_height,
    		   area(100, 100, 200, 200), H),

    	H = 100
