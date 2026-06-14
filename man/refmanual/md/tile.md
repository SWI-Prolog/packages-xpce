# class tile {#class-tile}

A tile is an object that manages the area of a rectangular object, such
as a graphical or window.  Tiles are used by class frame to manage the
layout of the windows in the frame.

Normal users hardly ever need to be aware of the existence of
tile objects.  Tiles are managed by and manages window objects
and frame objects.  The only relevant messages that are directly
delegated to from class window are ->hor_stretch, ->hor_shrink,
->ver_stretch and ->ver_shrink that determine how easily the
managed window object can grow bigger or smaller.

Tiles are organised in a consists-of hierarchy.  The leafs of this tree
are connected to the actually managed objects (for frames, these are the
windows).  The root of the tree is associated with the total area over
which the objects must be distributed (the frame).

A tile describes the *ideal size* as well as the merits to be
larger (stretch) or get smaller (shrink).  A tile negotiates
with its super-tile in the consists-of hierarchy to get a size
that is a close as possible to its ideal size, but it is always
the super-tile that decides on the size.

Leaf-tiles communicate with the actual object by means of the methods

	| <-width, <-height | To set the initial ideal size |
	| ->set: X, Y, W, H | To resize/move the object     |

The non-leaf tiles are either horizontal or vertical.  Horizontal tiles
manage a set of sub-tiles that are next to one-another.  Vertical tiles
manage a set of sub-tiles that are above one-another.


## Defining a tile hierarchy {#class-tile-defining-a-tile-hierarchy}

A tile-hierarchy is build inwards-out.  First of all, all objects that
need to be tiled are given a tile (the leaf-tiles).  Next, the
deepest-nested adjacent tiles that must have the same width or
height are connected to one another, etc.  This is done using
the ->left, ->above, etc.  behaviour.  The creation messages are
always sent to the leaf-tiles, who create and maintain tiles
higher in the hierarchy whenever needed.

@see frame<-tile
@see topic Window Layout
@see class window
@see class frame


## Instance variables {#class-tile-instvars}

- tile<->border: int
    Distance between the object tiled and its reserved area.

    @see window->border

- tile->can_resize: [bool]
    This variable is used by class frame to deal with user-initiated
    resize of the subwindow layout.  Initially it is default, When
    requested and it is default <-can_resize sets it to @on if
    this tile is stretchable in the direction of its <-super and
    there is at least one stretchable tile below or right of this
    tile.

    See also ->hor_stretch, ->hor_shrink, ->ver_stretch and
    ->ver_shrink.

- tile<->hor_shrink: int
    Encouragement to decrease the width of this tile. When multiple tiles
    are related left-to-right, the system will compute the sum of the
    <-ideal_width's of the stacked tiles.  If this sum is equal to the
    desired total width the stretch- and shrink-parameters are ignored.  If
    the sum is smaller than the desired width <-hor_stretch is considered.
    Finally, if the sum is larger than the desired total width the system
    first attempts to shrink all windows that have <-hor_shrink > 0
    proportional to their <-hor_shrink value.  If this does not make the
    total width small enough, all windows with <-hor_shrink == 0 are made
    equally less wide.

    **Defaults**: The default value is 100.

    @see tile-ideal_width
    @see tile-hor_stretch

- tile<->hor_stretch: int
    @see tile-hor_shrink

- tile<->ideal_width: int
    @see tile-hor_shrink

- tile<-members: chain*
    If a tile is not directly managing an object (`leaf-tile`) it is
    managing a horizontal or vertical stack of subtiles. These subtiles are
    in the chain <-members.  Each of the subtiles' <-super points to this
    tile object.   The variable <-orientation is set to either horizontal or
    vertical.

    A tile that manages a number of sub-tiles is normally created by its
    sub-tiles:

    	?- new(X, tile(box(100, 100))),
    	   new(Y, tile(box(200, 100))),
    	   send(X, right, Y).

    Will create two tiles and a third, which is a tile with <-orientation:
    horizontal and <-members the X and Y tiles.

    @see tile-super

- tile<-object: object*
    _Leaf-tiles_ directly manage the area of an object.  This may be any
    object provided it implements <-width, <-height and ->set.

- tile<-orientation: {none,horizontal,vertical}
    Direction of adjacent sub-tiles.  If `none`, the tile is a
    leaf tile controlling an <-object.  If `horizontal`, the
    <-members are places left-to-right.  If `vertical` they
    are placed top-to-bottom.

- tile<-super: tile*
    @see tile-members


## Send methods {#class-tile-send}

- tile->above: object, delegate=[bool]
    Place the argument tile above me.

- tile->enforce: [bool]
    If the tile is not yet <-enforced, it will force its subtitles to fit
    the computed <-ideal_width and <-ideal_height of the tile using
    ->layout.

    If the argument is @off, the <-enforced argument will be set to
    @off on this tile and all subtiles, after which the ideal sizes
    are recomputed.  See also `frame->fit`.

    @see frame->fit

- tile->initialise: object=object*, width=[int], height=[int]
    Create a tile for the specified object.  If width and height are given,
    these values will be used as <-ideal_width and <-ideal_height.
    Otherwise these values will be extracted from the object using
    `graphical <-width` and `graphical <-height`.

- tile->layout: x=[int], y=[int], width=[int], height=[int]
    Compute subtile layout and adjust objects.  For tiles with
    <-orientation horizontal or vertical this implies setting the
    <-area of the tile and distributing the <-members over the
    given area.  For tiles with <-orientation none (leaf-tiles)
    this implies setting <-area and sending ->geometry to
    the controlled <-object.

    If the tile has an <-adjuster, call ->update_adjuster_position
    to update the location of the small buttons to control the
    distribution of stretchable subwindows.

- tile->set: x=[int], y=[int], width=[int], height=[int]
    Principal manipulation method for the area occupied by a tile.  Its
    behaviour depends on the place of the tile in the hierarchy as well as
    whether or not the tile is already <-enforced.

    If the tile is not yet <-enforced, the width and height components are
    used to set <-ideal_width and <-ideal_heigth and the ideal sizes are
    propagated over the tile hierarchy using ->compute.

    If the tile is already enforced, it will set the ideal size of
    this tile.  Next, it will change the stretch/shrink properties
    of all members of the <-super tile, such that all members
    before and upto (above, left of) this tile have fixed size and
    all members after (below, right of) can be resized.  Used by
    `frame ->event` to realise resizable subwindows.

## Get methods {#class-tile-get}

- tile<-root: -> tile
    @see frame<-tile

- tile<-sub_tile_to_resize: point -> tile
    Normally invoked on the <-root of a tile hierarchy.  If `point`
    is on the border between any two tiled object in the tile
    hierarchy, return the tile above or left of the given position.

    Used by `frame ->event` to implement resizing of subwindows.

