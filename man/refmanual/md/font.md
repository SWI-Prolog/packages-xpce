# class font {#class-font}

A `font` describes the appearance of a run of text.  An xpce font has
four characteristics:

- `<-family` — the typeface family (e.g. `sans`, `mono`, `serif`,
  `times`, …).
- `<-style` — the role within that family: `normal`, `italic`,
  `oblique` or `bold`.
- `<-points` — the nominal type size in points; @default lets xpce
  fall back to the current Pango default.
- `<-weight` — a separate axis on top of `<-style`.  Either a name
  in `{thin, ultralight, light, semilight, book, normal, medium,
  bold, ultrabold, heavy, ultraheavy}` or a numeric weight in
  `100..1000`.  Setting `<-style` to `bold` is equivalent to also
  selecting `weight = bold`; the `<-weight` axis lets you pick
  intermediate values that `<-style` alone cannot express.

xpce uses **Pango** for everything below the xpce API: family
resolution, glyph layout, hinting and rendering.  Application code
normally refers to a font by its *alias* name held in the
`@font_aliases` hash table — for instance:

	send(Editor, font, normal).

The aliases are seeded from the `font.system_fonts` and (when
present) `font.user_fonts` class variables.  Any alias these leave
undefined is added from the built-in value of `font.system_fonts`, so
a `Defaults` file that overrules `font.system_fonts` only needs to
mention the aliases it wants to change.  See `<-convert` for the
accepted name forms.

An alias is a `theme_font`: a font that takes its properties from the
font it is derived from.  Unlike other fonts, it is not shared with
the fonts that have the same properties, so `normal` and
`font(sans, normal, 12)` are different objects.  Changing the font of
an alias changes the alias in place, so everything that uses the alias
follows.

The xpce `<-family` slot is translated to a Pango family by looking
the name up in the `@font_families` hash table (seeded from
`font.pango_families`).  That table maps the generic family names
`mono`, `sans` and `serif` to a sensible platform-specific Pango
family.  The old names `screen`, `helvetica` and `times` use the
mapping of `mono`, `sans` and `serif` unless the table maps them
explicitly.  Any other name not in the table is passed to Pango
literally.  A Pango family name may itself be a comma-separated list
of alternatives that Pango tries in order; `<-pango_property` reports
which family was actually selected.  Characters that none of these
families provide are taken from the fallback fonts of the system, so
the mapping does not limit the characters that can be shown.

Fonts are reusable: constructing one with the same family, style,
points and weight returns the existing instance (see `<-lookup`).
Font objects are auto-bound to a global reference of the form
`@<family>_<style>_<points>` for convenient debugging access.

A sheet describing every Pango font on the host is available as

	get(@font_class, font_families, Sheet).

The method accepts two optional arguments: `get(@font_class,
font_families, Monospace, Covers, Sheet)`.  If Monospace is `@on`,
only monospaced families are returned, if `@off`, only the others.
If Covers is a char_array, only families whose font provides all its
characters (without using a fallback font) are returned.  For example,
`Covers = 'Aé'` removes symbol fonts and fonts for other scripts.
Checking Covers requires loading each font and is relatively slow.

xpce text manipulation handles both fixed-width and proportional
fonts.  Many classes ship class variables that pick reasonable
default fonts for their role.

@see text<-font
@see class fragment
@see class editor
@see class text_image


## Class variables {#class-font-classvars}

- font.scale: num = 1.0
    Make all text larger or smaller.  The size of every font is
    multiplied by this factor, so a value of 1.2 makes all text 20%
    larger, whatever font it uses.  This is the simplest way to adapt
    the user interface to the resolution of your screen and your
    eyes.  Use `font.system_fonts` to change the size of specific
    kinds of text.

    For example, to make all text 25% larger, add this line to your
    `Defaults` file:

    	font.scale: 1.25

    The scale also applies to fonts that are not named by an alias.
    After changing it at runtime, send `display_manager
    ->fonts_changed` to apply it to the existing fonts and windows.
    The preferences editor (_Settings/Edit GUI preferences_ in the IDE)
    does this for you.

- font.system_fonts: chain
    Define the font aliases.  The user interface does not name fonts
    by their family, style and size, but by an _alias_ that describes
    their role: `normal` for most text, `bold` for labels, `fixed` for
    the text of editors and terminals, etc.  This setting defines the
    font of each alias as a list of `Alias := Font`.  The
    built-in value is

    	[ normal    := font(sans, normal, 12),
    	  bold      := font(sans, bold,   12),
    	  italic    := font(sans, italic, 12),
    	  small     := font(sans, normal, 10),
    	  large     := font(sans, normal, 14),
    	  boldlarge := font(sans, bold,   14),
    	  huge      := font(sans, normal, 18),
    	  boldhuge  := font(sans, bold,   18),
    	  fixed     := font(mono, normal, 14),
    	  tt        := font(mono, normal, 14),
    	  boldtt    := font(mono, bold,   14),
    	  itt       := font(mono, italic, 14),
    	  bitt      := font(mono, italic, 14, bold)
    	]

    A font is font(Family, Style, Points) or font(Family, Style,
    Points, Weight).  The families `sans`, `serif` and `mono` are
    _generic_: `font.pango_families` decides which installed font they
    use.  Style is one of `normal`, `bold`, `italic` or `oblique`.  An
    alias that you leave out keeps its built-in font, so you only need
    to list the aliases you want to change.  For example, to use a
    larger font for editors and terminals:

    	font.system_fonts: [ fixed := font(mono, normal, 16), \
    	                     tt    := font(mono, normal, 16)  \
    	                   ]

    Use `font.scale` to make all text larger or smaller.  Note that
    changing an alias changes all text that uses it, while a setting
    such as `editor.font` changes only the font of one kind of
    object.  The value may also define new aliases, which
    applications can use as fonts.

    The Text tab of the preferences editor, which is
    _Settings/Edit GUI preferences_ in the IDE, shows each alias with
    its family, style and size, followed by a sample text in this
    font.  Changes apply immediately.  When saved, the `Defaults` file
    holds all aliases.  If _Same size for related fonts_ is on, which
    is the default, changing the size of an alias also changes the
    size of the aliases that have the same family and size and only
    differ by style or weight.  For example, changing `tt` to 16
    points also changes `boldtt`, `itt` and `bitt`.

    @see class theme_font

- font.pango_families: chain
    Define the installed font used for the generic families `mono`,
    `sans` and `serif`.  xpce fonts name a generic family rather than
    an installed font, such that applications look reasonable on any
    system.  This setting maps each generic family to a family
    known to Pango, the library xpce uses to render text.  The value is
    a list of `Generic := Family`, where Family is the name of an
    installed font family or a comma-separated list of families that
    are tried in order.  For example, to use _Fira Code_ for all
    monospaced text if it is installed:

    	font.pango_families: [ mono := 'Fira Code,monospace' ]

    A family that you leave out keeps its built-in value, which
    depends on the operating system.  The legacy names `screen`,
    `helvetica` and `times` follow `mono`, `sans` and `serif`.
    Characters that the selected family does not provide, such as
    emoji or other scripts, are taken from other installed fonts.

    The Text tab of the preferences editor, which is
    _Settings/Edit GUI preferences_ in the IDE, offers the installed
    families, each shown in its own font.  After changing it at runtime, send
    `display_manager ->fonts_changed`; the preferences editor does this
    for you.

- font.no_font: font = normal
    Substitute used when an explicitly-requested font cannot be
    resolved.


## Instance variables {#class-font-instvars}

- font<-family: name
    Typeface family — e.g. `sans`, `mono`, `serif`, or any Pango
    family name accepted by the host.

- font<-style: {normal,italic,oblique,bold}
    Style within the family.

- font<-weight: {thin,ultralight,light,semilight,book,normal,medium,bold,ultrabold,heavy,ultraheavy}|100..1000
    Numeric or named weight; orthogonal to `<-style`.

- font-points: [num]
    Nominal point size.  @default means "use Pango's default size";
    the getter `<-points` returns the slot or `<-height` when
    @default.

- font-ascent: [num]
- font-descent: [num]
    Caches for the metrics returned by `<-ascent` / `<-descent`.

- font-ex: num*
    Pixel height of the letter `x` in this font, cached on first
    access.  Used by classes that size by character count rather
    than pixels (see `editor`, `text_item`).  See also `<-width`.

- font-avg_char_width: num*
    Average glyph advance, cached on first access; the basis of
    `<-width` when no explicit string is given.

- font-fixed_width: [bool]
    Cache for `<-fixed_width`.  @off means proportional, @on means
    every glyph has the same advance.  Computed once by comparing
    the advances of `i` and `w`.


## Send methods {#class-font-send}

- font->initialise: family=name, style=name, points=[int], weight=[{thin,ultralight,light,semilight,book,normal,medium,bold,ultrabold,heavy,ultraheavy}|100..1000]
    Create a font from its family, style, point size and weight.

    Fonts are reusable: constructing a font with the same
    family/style/points/weight returns the existing instance from
    `@fonts` (see `<-lookup`).  Resolution against Pango is lazy —
    the underlying Pango font is only built when xpce first needs
    metrics or wants to render text.

    The new font is auto-bound to the global reference
    `@<family>_<style>_<points>` for convenience.

    New code is normally clearer when it picks fonts by alias name
    (the type-checker calls `<-convert`) than by spelling out the
    constructor.

- font->unlink
    Release the Pango resources backing this font.  Called
    automatically on destruction.

- font->member: char=char, family=[bool]
    Succeeds if the indicated character is defined in this font.
    With `family = @on` (the default), the search also walks the
    fallback families that Pango would substitute for missing
    glyphs.


## Get methods {#class-font-get}

- font<-points: -> num
    Specified `<-points`, or `<-height` when `<-points` is @default.

- font<-ascent: -> num
- font<-descent: -> num
    Pixels above / below the baseline.

- font<-height: -> num
    Cap height of the tallest character; equal to
    `<-ascent + <-descent`.

- font<-ex: -> num
    Pixel height of the letter `x`.

- font<-avg_char_width: -> num
    Average glyph advance in pixels.

- font<-size: -> size
    `size(width('x'), <-height)` — convenient when laying out by
    cell size.

- font<-width: [char_array] -> num
    Pixel width of the argument string when rendered in this font
    (defaults to `"x"`).

    Counts ink extents, so a glyph with a wide left side-bearing
    contributes its visual width.  `<-advance` reports the cursor
    movement instead (the sum of glyph advances), which is what
    layout code usually wants.

    @see font<-advance
    @see text->clip

- font<-advance: char_array -> num
    Cursor advancement when the string is rendered: the sum of the
    glyph advances along the baseline.  Use this rather than
    `<-width` to position the next glyph after a string.

- font<-rescale: num -> font
    Return a font with the same family, style and weight but
    `<-points` multiplied by the argument:

	?- get(font(sans, normal, 10), rescale, 1.3, F).
	F = @sans_normal_13.

- font<-fixed_width: -> bool
    @on when every glyph has the same advance, @off when the font
    is proportional.  Determined by comparing the advances of `i`
    and `w`.

- font<-default_character: -> char
    Character that Pango falls back to when asked to render a
    glyph the font does not contain (typically `?` or a tofu box).

- font<-domain: family=[bool] -> tuple
    Range of code points the font can render, returned as
    `tuple(Lo, Hi)`.  With `family = @on` (default) the range
    includes characters covered by Pango's family fallbacks; with
    `family = @off` only the chosen face is considered.

- font<-pango_property: int|name -> {description,family,style,weight,size}
    Properties of the resolved Pango font.  The reported family
    may differ from the `<-family` slot, because `<-family` is
    first mapped through `@font_families` and the result is then
    passed to Pango's own font matcher.  The reported size is
    scaled by `font.scale`.

- font<-lookup: family=name, style=name, points=[int], weight=[{thin,ultralight,light,semilight,book,normal,medium,bold,ultrabold,heavy,ultraheavy}|100..1000] -> font
    Find an existing font in `@fonts` matching the arguments, or
    fail.  Mirrors `->initialise`; used by the type-checker to
    deduplicate constructions.

- font<-convert: name -> font
    Type-checker hook.  Accepts:

    - the legacy reference syntax `@<family>_<style>_<points>`
      (kept for compatibility with code that wrote fonts out by
      reference);
    - any alias name registered in `@font_aliases` (the
      recommended form for new code).

    Anything not matched here fails; callers that want to build
    fonts from family/style/points/weight should construct the
    `font(...)` term explicitly.
