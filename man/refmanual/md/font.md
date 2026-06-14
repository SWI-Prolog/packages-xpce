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
present) `font.user_fonts` class variables.  See `<-convert` for the
accepted name forms.

The xpce `<-family` slot is translated to a Pango family by looking
the name up in the `@font_families` hash table (seeded from
`font.pango_families`).  That table maps the generic family names
`mono`, `sans` and `serif` to a sensible platform-specific Pango
family; any name not in the table is passed to Pango literally.  A
Pango family name may itself be a comma-separated list of
alternatives that Pango tries in order; `<-pango_property` reports
which family was actually selected.

Fonts are reusable: constructing one with the same family, style,
points and weight returns the existing instance (see `<-lookup`).
Font objects are auto-bound to a global reference of the form
`@<family>_<style>_<points>` for convenient debugging access.

A sheet describing every Pango font on the host is available as

	get(class(font), font_families, Sheet).

xpce text manipulation handles both fixed-width and proportional
fonts.  Many classes ship class variables that pick reasonable
default fonts for their role.

@see text<-font
@see class fragment
@see class editor
@see class text_image


## Class variables {#class-font-classvars}

- font.scale: real = 1.0
    Multiplication factor applied to every font's `<-points` before
    Pango is asked to render.  Useful for HiDPI tweaks.

- font.system_fonts: chain
    Seed for `@font_aliases`: pairs of alias name → font object
    (`normal := font(sans, normal, 12)`, `tt := font(mono, normal,
    12)`, ...).  The default list defines `normal`, `bold`,
    `italic`, `small`, `large`, `boldlarge`, `huge`, `boldhuge`,
    `fixed`, `tt` and `boldtt`.

- font.pango_families: chain
    Seed for `@font_families`: pairs that map an xpce generic
    family name (`mono`, `sans`, `serif`, plus legacy `helvetica`,
    `times`, `screen`) to a Pango family name (or a
    comma-separated list of fallbacks).

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

    	?- get(font(sans, roman, 10), rescale, 1.3, F).
    	F = @sans_roman_13.

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

- font<-lookup: family=name, style=name, points=[int], weight=[name|int] -> font
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
