# class text_image {#class-text_image}

Class text_image takes care of mapping a one-dimensional array of
(character,font,colour,attribute) quadruples onto a two-dimensional text
image as we normally like to see text displayed.  This class is used
both by class editor and class list_browser to manage the text-part.

Class text_image uses dedicated interface techniques to their associated
editor or list_browser and cannot be used outside this context.  This
prohibits the use of text_image objects outside the context of editor
or list_browser by the application programmer.

Most behaviour of class text_image is just for communication with the
associated editor or list_browser.  Of interesting to the user may be:

	| <->tab_distance | Distance between (regular) tab-stops |
	| <->tab_stops    | Vector of tab-stops                  |
	| <->wrap         | How long lines are handled           |

**Bugs**: We should realise boxed text-style and horizontal scrolling.

@see editor-image
@see class font


## Instance variables {#class-text_image-instvars}

- text_image-change_start: alien:int
    Start of changes (character index).

- text_image-inserted: alien:int
    How much text was inserted/deleted.

- text_image-change_end: alien:int
    These variables are used by ->compute to determine the range of the text
    that should be updated and the area of the <-image that is to be
    repainted.

- text_image-scan: alien:ScanFunction
    C-Function to scan for a syntactical category.

- text_image-seek: alien:SeekFunction
    C-Function to seek to a position.

- text_image-fetch: alien:FetchFunction
    C-function pointers obtained from the <-text to perform the following
    low-level action:

    	| -fetch | Read next character with attributes |
    	| -seek  | Set index for next -fetch operation |
    	| -scan  | Scan for a syntactical category     |

- text_image-map: alien:TextScreen
    Array of line descriptions.  Each line describes a screen-line:
    character index for the start, displayed characters, fonts,
    colours and attributes.  This map is updated by ->compute.
    See also ->dump_map.

- text_image-start: int
    _Start_ is normally the start of a line of the underlying text (e.i.
    the first character after a newline character).  It will be made the
    <-start of the text_image.  _SkipLines_ is the number of screen lines
    to omit and may be used to start the screen in the middle of a wrapped
    long line.  The default `skip_lines` is 0.

    <-start returns the character index for the first character of the nth
    (1-based) screenline.  By default this is the first visible character.

- text_image<-tab_stops: vector*
    Vector of tab-stops in pixels.  When defined, this overrules
    <->tab_distance.

- text_image<-text: object
    Holder of the text.  The text should implement:

    	| <-_seek_function  | get -seek function  |
    	| <-_scan_function  | get -scan function  |
    	| <-_fetch_function | get -fetch function |

    Besides this, it should notify changes to the text_image object
    using:

    	| ->_changed_region | Indicate text in region has changed |
    	| ->_insert         | Indicate insert/delete              |

    This functionality is currently reserved for the C-programmer.


## Send methods {#class-text_image-send}

- text_image->center: index=int, line=[int]
    Scroll the text such that the character at the given index is on the
    given physical line (1-based) of the window.  When `line` is omitted,
    index will be placed around the center of the image.

- text_image->compute
    Updates the -map and informs the redrawing system of the part of
    the image that needs to be updated.

- text_image->dump_map
    Dump map of the screen.   Used for system maintenance only.  See
    also -map and ->compute.

- text_image->wrap: {none,character,word}
    Wrap mode for long lines.  This has several implications on the
    behaviour of editor.  Please check `editor->wrap` for details.


## Get methods {#class-text_image-get}

- text_image<-character_position: int -> point
    X-offset, Y-BaseLine of character at the given index.

- text_image<-index: event -> int
    Translate an event into the index of a character.  This method is used
    by class editor and class list_browser to implement pointer-associated
    event-handling.  It may be used when connecting specialised
    event-handling to an editor or list-browser.

