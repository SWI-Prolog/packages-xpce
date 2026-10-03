# class theme_colour {#class-theme_colour}

A `theme_colour` is a colour whose name describes its role rather
than its value, for example `ui_window_background` or
`syntax_comment`.  It is derived from another colour, which is the
name of another theme colour or a system colour (`sys_*`), or a colour
object.  The xpce user interface and the development tools refer to
theme colours, either directly or by name, so they follow the selected
theme when its colours change:

	?- new(_, theme_colour(my_highlight, khaki1)),
	   send(new(P, picture), open),
	   send(P, display, new(B, box(100, 100))),
	   send(B, fill, my_highlight).
	?- new(_, theme_colour(my_highlight, khaki4)),
	   send(@display_manager, colours_changed).

The RGB value is computed when it is needed, by following
`<-derived_from` through other theme colours until an ordinary colour
is found.  Changing what any theme colour is derived from, or reloading the system
colours, resets the RGB value of _all_ theme colours, so each is
resolved again on its next use.  Theme colours may therefore refer to
each other in any order and a theme colour may be created before the
colour it refers to.  A cycle or an unknown colour name prints a
warning and resolves to grey50.

Theme colours are locked and never garbage collected.  They are never
returned when looking up a colour from its RGB values (see
`colour<-lookup`), because their value changes.  All theme colours are
in the chain `@theme_colours`.

xpce defines a number of theme colours itself, the `ui_*` colours for
the basic user interface elements and the `ansi_*` colours of the
terminal.  They are created when the class is initialised.  Their
values in the default `light` theme are in the hash table
`@theme_colour_defaults`.  Themes are managed by the Prolog library
`library(pce_theme)`.  See the section _Themes_ of the XPCE User
Guide.

@see class colour
@see display_manager->colours_changed
@see display_manager->system_colours_changed


## Instance variables {#class-theme_colour-instvars}

- theme_colour<-derived_from: name|colour
    The colour this colour is derived from.  This is not called
    `value` because `colour<-value` is the _value_ of the HSV model.  Either a colour name,
    which may be the name of another theme colour, or a colour
    object.  The name `#RRGGBB` specifies an RGB value.


## Send methods {#class-theme_colour-send}

- theme_colour->initialise: name=name, derived_from=name|colour
    Create a theme colour with the given name, derived from the given
    colour.  The colour is added to `@colours` and `@theme_colours` and
    locked.  Its `<-rgba` is @default until it is resolved.  If a theme
    colour with this name exists, `<-lookup` returns it after setting
    `->derived_from`.

- theme_colour->derived_from: name|colour
    Change the colour this colour is derived from.  If it differs, the
    RGB value of all theme colours is reset, so they are resolved again
    on their next use.  This does not redraw anything: use `display_manager
    ->colours_changed` after changing theme colours.

- theme_colour->unlink
    Remove the colour from `@theme_colours` and `@colours`.


## Get methods {#class-theme_colour-get}

- theme_colour<-lookup: name=name, derived_from=name|colour -> theme_colour
    If a theme colour with this name exists, update it using
    `->derived_from` and return it.  This makes creating a theme
    colour again a way to change it.
