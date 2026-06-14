# class dict_item {#class-dict_item}

A dict_item object is an entry in a dictionary (dict object).
It consists of four fields: a <-key, used for indexing purposes;
a <-label to be displayed when the dict_item is visualised in a
browser object and and an <-object, which can be anything that
is to be associated with the dict_item and finally a <-style to
indicate the `category` the object belongs too.  Each category
may be displayed using another font, colour or attributes.  See
`list_browser->style`.

The <-label defaults to the <-key, the object and style default to
@nil.

@see class dict


## Instance variables {#class-dict_item-instvars}

- dict_item<-dict: dict*
    Dict object I'm part of.  When @nil, I'm not part of a dict.  Otherwise
    I'm member of the chain `dict <-members`.

- dict_item<-index: int
    Index-number of the dict_item in the dict.  The first item has number 0.
    Used to speed up changes forwarding and repaint.

- dict_item<-key: any
    The key value of this dict_item.  This should be a unique value with
    respect to the other members of the dict object holding this dict_item.
    See also <-label,  `dict ->append` and `dict <-member`.

    If the registered key is an instance of class char_array or one
    of its subclasses, the key is automatically converted to a name
    object.

    @see dict<-member
    @see dict_item<-label
    @see dict->append

- dict_item-label: [char_array]
    When @default, the displayed label is the <-print_name of the
    <-key.  Otherwise the value of this variable is displayed.  See <-label.

    @see dict_item-key
    @see dict_item<-label

- dict_item<->object: any
    This value may contain any user-defined information to be associated
    with this item of the dictionary.  It is commonly used for two purposes:

    - Store additional info
    	When a dict (normally though a browser object) is used to
    	browse and select some information, <-object is often used
    	to store additional information.  For example, the summary
    	browsers of this online manual store the documentation card
    	represented by an entry in the <-object slot.

    - Store a frame
    	Dict objects where originally intended to store and
    	visualise structured databases.  The object was
    	generally filled with a sheet object to store attributes
    	of this entry.  For example, a dict object may be used
    	to store a database of clients, The <-key would be the
    	client's name or identifier.  The <-object a sheet or
    	instance of a user-defined class that contains the
    	client's phone, address and other info.

    Dict items delegate to their <-object.

- dict_item<-style: [name]
    Logical name of the category the item belongs too.  The sheet
    `list_browser <-styles` maps these category names onto style objects
    describing the font, colour and attributes for the item.


## Get methods {#class-dict_item-get}

- dict_item<-contained_in: -> dict
    Returns the <-dict of the item.

- dict_item<-convert: any -> dict_item
    Convert a name to a dict_item object with default <-label and <-object.

- dict_item<-image: -> list_browser
    Internal method that allows popup menu's to operate on dict_item's.  See
    `list_browser ->popup`.

- dict_item<-label: -> char_array

- dict_item<-position: -> point
    Return a new point object describing the top-left corner of the
    area used by this dict_item in the coordinate system of the
    list_browser object it is displayed on.

    Fails silently if the dict_item is currently not displayed or
    outside the visible part of the list_browser.

