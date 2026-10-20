# class markup_text {#class-markup_text}

A markup_text displays text formatted using
[Pango markup](https://docs.gtk.org/Pango/pango_markup.html).  The
markup can mix fonts, sizes, styles and colours within one text, for
example:

	new(T, markup_text("Hello <b>bold</b> and <span foreground='red'>red</span>")),
	send(Picture, display, T, point(10, 10)).

Unlike class text, a markup_text cannot be edited and has no caret or
selection.  Its layout is done by Pango, which also handles wrapping
lines at a given <-width, aligning, justifying and ellipsizing them.
The size of a markup_text is computed from its content and layout.
Text that has no colour in the markup is drawn using
`graphical<-colour`.  To rotate or scale a markup_text, display it on
a figure with a `figure<-transform`.

The most common tags are `<b>`, `<i>`, `<u>`, `<s>`, `<tt>`, `<big>`,
`<small>`, `<sub>` and `<sup>`.  The `<span>` tag accepts attributes
such as `font`, `size`, `weight`, `style`, `foreground`, `background`,
`underline` and `strikethrough`.  The characters `<`, `>` and `&` must
be written as `&lt;`, `&gt;` and `&amp;`.

Besides the colours Pango knows, colour attributes such as `foreground`
and `background` accept the name of any existing xpce colour.  Notably,
this allows for theme colours, for example
`<span foreground='ui_link'>`, which follow a change of theme.
Likewise, `font_family` and `face` accept xpce's font families such as
`mono`, and `<tt>` uses the family `mono`, the font xpce uses for
fixed-width text, rather than Pango's `Monospace`, on the faint theme
colour `ui_code_background`.

## Instance variables {#class-markup_text-instvars}

- markup_text<-markup: char_array
    The text with Pango markup.  Setting it to invalid markup raises a
    syntax error and leaves the markup unchanged.

- markup_text<-font: font
    Font for text that has no font in the markup.  Sizes such as `big`
    are relative to this font.

    **Defaults**: the class variable `font` (normal).

- markup_text<-width: int*
    If not @nil, lines are wrapped at this width (at word boundaries
    if possible) and the width of the graphical is at least this.
    ->geometry sets this value if a width is given.

    **Defaults**: @nil (lines are only broken at newlines)

- markup_text<-alignment: {left,center,right}
    Alignment of the lines within <-width or, if there is no width,
    within the longest line.

    **Defaults**: left

- markup_text<-justify: bool
    If @on, stretch wrapped lines to fill <-width.

    **Defaults**: @off

- markup_text<-ellipsize: {none,start,middle,end}
    If not `none` and a <-width is given, lines are not wrapped.
    Instead, text that does not fit is replaced by `...` at the
    indicated place.

    **Defaults**: none

- markup_text<-line_spacing: num
    Distance between the baselines as a factor of the font's line
    height.  0 uses the font's.

    **Defaults**: 0

## Send methods {#class-markup_text-send}

- markup_text->initialise: markup=char_array, width=[int]*, font=[font]
    Create from markup and optionally a wrap width and font.  Fails with
    a syntax error if the markup is not valid.

- markup_text->geometry: x=[int], y=[int], width=[int], height=[int]
    Move the text.  If a width is given, set <-width.  The height is
    determined by the content.

## Get methods {#class-markup_text-get}

- markup_text<-text: -> string
    The text without the markup.
