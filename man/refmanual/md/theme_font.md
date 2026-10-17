# class theme_font {#class-theme_font}

A `theme_font` is a font whose name describes its role rather than its
appearance.  The font aliases, such as `normal`, `bold` and `tt`,
are theme fonts.  A theme font is derived from another font and copies
its family, style, weight and points.  Unlike other fonts, a theme
font is not shared with the fonts that have the same properties:
`normal` and `font(sans, normal, 12)` are different objects.  Changing
the font a theme font is derived from changes the theme font in
place, so everything that uses it follows:

	?- send(new(P, picture), open),
	   send(P, display, text('Hello world', left, normal)).
	?- get(@pce, convert, normal, font, F),
	   send(F, derived_from, font(serif, italic, 20)),
	   send(@display_manager, fonts_changed).

Theme fonts are created from the class variables `font.system_fonts`
and `font.user_fonts` when the first font is used.  Changing these
class variables and sending `display_manager ->fonts_changed` updates
the theme fonts.  Theme fonts are in the hash table `@font_aliases`,
which maps their name to the theme font, and in the chain
`@theme_fonts`.  They are protected, so they are never garbage
collected.

A theme font may be derived from another theme font.  It then takes
the font the other theme font is derived from at that moment: it
does not follow later changes to the other theme font.  Methods that
create a new font from a theme font, such as `font<-rescale`, create
an ordinary font.

The class variable editor saves a theme font by its name.  Its font
items have a button to select a theme font.

@see class font
@see class theme_colour
@see display_manager->fonts_changed


## Instance variables {#class-theme_font-instvars}

- theme_font<-name: name
    Name of the font, e.g., `normal`.  This is the key in
    `@font_aliases`.

- theme_font<-derived_from: font
    The font this font copies its family, style, weight and points
    from.  This is never a theme font.

- theme_font<->summary: string*
    Description of the role of this font.


## Send methods {#class-theme_font-send}

- theme_font->initialise: name=name, derived_from=font, summary=[string]*
    Create a theme font with the given name, derived from the given
    font.  The font is added to `@font_aliases` and `@theme_fonts`.  If
    a theme font with this name exists, `<-lookup` returns it after
    setting `->derived_from`.

- theme_font->derived_from: font
    Change the font this font is derived from.  If it differs, the
    theme font copies its family, style, weight and points and drops
    its window system font and metrics, so they are computed again on
    the next use.  This does not update the windows: use
    `display_manager ->fonts_changed` after changing theme fonts.

- theme_font->unlink
    Remove the font from `@theme_fonts` and `@font_aliases`.


## Get methods {#class-theme_font-get}

- theme_font<-lookup: name=name, derived_from=font, summary=[string]* -> theme_font
    If a theme font with this name exists, update it using
    `->derived_from` and return it.  This makes creating a theme font
    again a way to change it.
