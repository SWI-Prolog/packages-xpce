# class resize_gesture {#class-resize_gesture}

A `resize_gesture` allows for resizing objects by dragging the corners
or sides (for constrained resize) of the objects.  While dragging, the
direction/constraints are indicated by showing an appropriate cursor.

The resize_gesture only responds to events the happen close the the
edges/corners of the graphical.  The class-variables
resize_gesture.margin_fraction and resize_gesture.margin_width
determine these areas: the gesture responds if the down event
occurs between the edge and 1/margin_fraction of the size or
margin_width (whoever is the least).

**Bugs**: Does not deal properly with negative areas.

@see class resize_outline_gesture


## Instance variables {#class-resize_gesture-instvars}

- resize_gesture<->h_mode: {left,keep,right}
    How the graphical is resized horizontally.  Values are:

    	| left  | Drag the left edge  |
    	| keep  | Change neither edge |
    	| right | Drag the right edge |

    This value is set by ->verify and used by ->initiate and ->drag.

    @see resize_gesture->verify

- resize_gesture<->max_size: size*
    Size that defines the maximum size the graphical may have.  If it
    depends on the circumstances, it may be set in ->verify or ->initiate.

    If @nil, there is no maximum size.

    **Defaults**: @nil

- resize_gesture<->min_size: size*
    Size that defines the minimal size the graphical should have.  If it
    depends on the circumstances, it may be set in ->verify or ->initiate.

    If @nil, there is no minimum size.

    **Defaults**: Resource defined.

    @see resize_gesture->verify

- resize_gesture<->v_mode: {top,keep,bottom}
    How the graphical is resized vertically.  Values are:

    	| top    | Drag the top edge    |
    	| keep   | Change neither edge  |
    	| bottom | Drag the bottom edge |

    This value is set by ->verify and used by ->initiate and ->drag.

    @see resize_gesture->verify


## Send methods {#class-resize_gesture-send}

- resize_gesture->drag: event
    Resize the graphical according the <-v_mode and the <-h_mode determined
    upon starting the gesture.

- resize_gesture->initiate: event
    Set the cursor to indicate which edges are going to be changed and moves
    the pointer to be precisely on those sides.

- resize_gesture->terminate: event
    Equivalent to ->drag.

- resize_gesture->verify: event
    The ->verify method determines the <->v_mode and <->h_mode attributes.
    The gesture changes those sides where the caret is closer that the
    maximum of

    	{Width/Height} / margin_factor AND margin_width

    Both resize_gesture.margin_factor and resize_gesture.margin_width
    are Defauls.  ->verify succeeds if at least one of <->v_mode and
    <->h_mode is not set to `keep`.

    @see resize_gesture-min_size
    @see resize_gesture-v_mode
    @see resize_gesture-h_mode

