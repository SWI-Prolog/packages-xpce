# class style {#class-style}

A style object describes the visual appearance of a fragment object in
an editor object or dict_item objects in a list_browser object: font,
highlighting, underlining, etc.  Style objects are associated with
editors using the `editor ->style: name, style` method.  Fragments that
have `fragment <->style` equal to name will be displayed according to
the associated style.

The online manual uses various styles.  E.g.

	| title  | Section titles in helvetica bold font       |
	| code   | code fragments in a fixed-width screen font |
	| active | attribute-bold for links to definitions.    |

An elaborate example of using editors, fragments and styles is
the XPCE help browser, whose sources are in the XPCE library
file pce_helper.pl.  A simple example is in the example
_Multiple Fonts_.

**Bugs**:

When a style is changed, the corresponding text is not immediately
updated.  It is adviced to use styles in a read-only fashion.

@see class text_margin
@see editor->style
@see editor-selected_fragment_style
@see class editor
@see class fragment


## Instance variables {#class-style-instvars}

- style-attributes: alien:long
    Or'ed bits for ->underline, ->highlight, ->grey, ->closed and ->bold.

- style<->background: [colour|pixmap|elevation]
    Background for the characters.  If it is an instance of class
    elevation, the characters are placed in an elevated box.
    See also ->colour.

- style<->colour: [colour]
    Colour of the characters.  See also ->background.

- style<->font: [font]
    Font used to display the characters of the fragment.  When two fragments
    overlap and they do not agree on the font to be used, the font of the
    smallest fragment is used.

    **Defaults**: @default (meaning use `editor <->font`).

- style<->icon: image*
    When a style object is associated with an editor that has a text_margin
    object attached to it, the start of fragments related to this style
    object is marked in the margin using this image object.  See class
    text_margin and `editor ->margin_width`.

    @see class text_margin
    @see editor->margin_width

- style<->left_margin: int
    Left- and right-margins.  Used by class text_image (underlying
    class editor and class view) to control indentation and
    wrapping.

    In combination with `text_image ->wrap: word`, the margins may
    be used to realise quotations and intended lists in WYSIWYG
    like editors.

    Class text_image fetches the new margins whenever it starts a
    new screen line.

- style<->right_margin: int
    *Inherits description from*: style-left_margin

- style<->underline: [bool|texture_name|colour]
    Underline text.  If @default, underlining of overlapping
    fragments is respected.  If @off there is no underlining.  If
    @on, the text is underlined using the same colour as the text.
    If an explicit colour is used the line is painted in this
    colour.


## Send methods {#class-style-send}

- style->bold: bool
    *Inherits description from*: style<-bold

- style->grey: bool
    *Inherits description from*: style<-bold

- style->hidden: bool
    If a style object has ->hidden: @on, the text will be made
    invisible.  Currently this attribute is ignored if the style is
    used for a dict_item object.

    May be used to implement `outline` editors or to hide control
    sequences.

    See also class fragment and class editor.

- style->highlight: bool
    *Inherits description from*: style<-bold

- style->initialise: icon=[image]*, font=[font], colour=[colour], highlight=[bool], underline=[bool|texture_name|colour], bold=[bool], grey=[bool], background=[colour|pixmap|elevation], hidden=[bool], left_margin=[int], right_margin=[int], strikethrough=[bool|texture_name|colour]
    Create a style object from its (margin-)image, font and text-attributes.
    For example:

    	send(Editor, style, section_header,
    	     style(font := huge)).

    The arguments are:

    	| ->icon         | Image shown in text_margin object |
    	| ->font         | Font used to display characters   |
    	| ->colour       | Colour for characters             |
    	| ->highlight    | Swap colour and background        |
    	| ->underline    | Underline text                    |
    	| ->bold         | Print bold                        |
    	| ->grey         | Print grey using colour<-reduce   |
    	| ->background   | Colour/elevation for background   |
    	| ->hidden       | If true, text is invisible        |
    	| ->left_margin  | Left margin (pixels)              |
    	| ->right_margin | Right margin (pixels from right)  |

    NOTE: this method defines a large number of arguments.  It is
    advised to use the `keyword := value` construct for specifying
    the arguments.    The following two terms define a `bold` style:

    	style(bold := @on)
    	style(@default, @default, @default, @default, @default, @on)

    See also class := and `send_method ->send`.

- style->underline
    **Bugs**:
    Underlining is done at a fixed distance of the base-line with a fixed
    line thickness (1).  These parameters ought to be read from the (X-)font
    attributes.

    *Inherits description from*: style<-bold


## Get methods {#class-style-get}

- style<-grey: -> bool

- style<-highlight: -> bool

- style<-underline: -> [bool|texture_name|colour]

- style<-bold: -> bool
    These methods set/query the appropriate field from the style -attributes
    slot.   The attributes are:

    	| ->bold      | bold-face using double-strike (see also ->font) |
    	| ->underline | underlined text.                                |
    	| ->highlight | inverted background and foreground.             |
    	| ->grey      | greyed-out by and'ing with @grey50_image.       |

    **Bugs**:
    <->closed is not implemented.  Future versions might use this to `hide`
    the text of a fragment temporary.

- style<-hidden: -> bool
    *Inherits description from*: style->hidden
