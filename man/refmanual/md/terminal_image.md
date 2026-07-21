# class terminal_image {#class-terminal_image}

Graphical that renders text in a terminal-style buffer with ANSI cell
colouring, a hardware-style cursor, scrollback and selection.  Used
to embed a terminal view (REPL output, command transcripts, child
process I/O, …) inside an xpce frame.  Subclass of `graphical`.

A terminal_image keeps a ring buffer of rendered cells (size
`<-save_lines`), exposes the visible region as a scrollable area
with an associated `<-scroll_bar`, and emits high-level events for
keystrokes and hovered hyperlinks.

@see class graphical
@see class text_image
@see class text_buffer


## Instance variables {#class-terminal_image-instvars}

- terminal_image<->bindings: key_binding
    Key bindings active in the image.

- terminal_image<-font: font
    Default (regular-weight) font used to draw text.

- terminal_image<-bold_font: font*
    Optional bold font; bold text falls back to `<-font` if @nil.

- terminal_image<->background: [colour]
    Background colour of the buffer.

- terminal_image<->selection_style: [style]
    `style` applied to selected cells.

- terminal_image<->nfd_style: style*
    Style applied to NFD grapheme clusters (e.g. accented composed
    characters), or @nil to disable highlighting.

- terminal_image<->link_style: style*
    Style applied to detected hyperlinks.

- terminal_image<->link_armed_style: style*
    Style applied to the hyperlink under the mouse pointer.

- terminal_image<->ansi_colours: vector*
    Vector of 16 `colour` objects: the 8 base ANSI colours followed
    by their bright variants.

- terminal_image<-armed_link: bool
    `@on` when the pointer is over a hyperlink.

- terminal_image<->link_message: code*
    Optional code invoked when a hyperlink is activated.

- terminal_image<->scroll_bar: scroll_bar*
    Associated scroll_bar (driven by `->bubble_scroll_bar`).

- terminal_image<->save_lines: int
    Number of past lines retained in the scroll-back buffer.

- terminal_image<->syntax: syntax_table
    Syntax table used for word boundaries in selection.


## Send methods {#class-terminal_image-send}

- terminal_image->initialise: width=int, height=int
    Create a terminal_image of the given column/row size.

- terminal_image->geometry: x=[int], y=[int], width=[int], height=[int]
    Move/resize the image.

- terminal_image->font: font=font, bold=[font]
    Change the regular and bold fonts.

- terminal_image->compute
    Recompute the rendered image.

- terminal_image->scroll_vertical: direction={forwards,backwards,goto}, unit={file,page,line}, amount=int
    Scroll request from the associated scroll_bar (also bound to
    keyboard navigation).

- terminal_image->event: event
    Top-level event dispatcher.

- terminal_image->typed: event
    Process a single keystroke.

- terminal_image->insert: text=char_array
    Insert text at the caret position.

- terminal_image->send: text=char_array
    Send text to the connected process.

- terminal_image->copy: which=[{primary,clipboard}]
    Copy the selected text to the primary selection or clipboard.

- terminal_image->paste: which=[{primary,clipboard}]
    Paste the contents of the primary selection or clipboard.

- terminal_image->select_all
    Select the entire buffer (including scroll-back).

- terminal_image->has_selection
    Succeeds if a non-empty selection exists.

- terminal_image->interrupt
    Virtual hook called on Ctrl-C; subclasses send a signal to the
    connected process.

- terminal_image->copy_or_interrupt
    Copy if there is selected text, otherwise call `->interrupt`.

- terminal_image->window_label: char_array
    Set the enclosing frame's label, e.g. from an OSC 0 sequence.


## Get methods {#class-terminal_image-get}

- terminal_image<-pty_name: -> name*
    Path of the pseudo-terminal device, or `@nil` when not connected
    to one.

- terminal_image<-displayed_cursor: -> cursor
    Cursor reflecting whether the pointer is on text or over a link.

- terminal_image<-selected: -> string
    New string with the contents of the selection.

- terminal_image<-cursor_position: -> point
    Logical cursor position as `point(col, row)`.

- terminal_image<-row: int -> string
    Text contents of a visible row, 0-based from the top.

- terminal_image<-link: point|event -> name
    Hyperlink URL at the given position or under an event.

- terminal_image<-cwidth: code=int -> int
    Number of columns the code point `code` occupies when drawn in
    `<-font`: 0 for combining marks, 2 for wide characters and 1 for
    the rest.  This is the classification the renderer itself uses, so
    clients that must predict our layout can share it rather than keep
    a second copy that drifts.  In particular it accounts for symbol
    and emoji code points that the static Unicode tables call width 1
    while an emoji-presenting font draws them twice as wide.  Fails
    while the terminal has no cell metrics yet.


## Class variables {#class-terminal_image-classvars}

- font, bold_font: default to `tt` and `boldtt`.
- background, colour: default to `white` and `black`.
- selection_style: yellow background (X) or system selection style.
- link_style, link_armed_style: blue, dotted/solid underline.
- save_lines: 1000 by default.
- auto_copy: copy selected text to clipboard automatically (default
  `@on` on macOS, `@off` elsewhere).
- ansi_colours: 16-element vector with the standard ANSI palette.
