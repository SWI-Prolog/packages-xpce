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

- terminal_image<->isearch_style: style*
    `style` applied to the hit of an incremental search, in place of
    `<-selection_style`, so that what a search found can be told from
    what the user picked with the mouse.

- terminal_image<->isearch_other_style: style*
    `style` applied to the other matches of a running incremental
    search that are on the screen, or `@nil` to leave them alone.  Only
    the visible ones: they are worked out afresh every time the window
    is painted, so scrolling brings the ones it reaches into view
    without the search doing anything about it.

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

- terminal_image<-focus_function: name*
    Method that is sent every keystroke while it succeeds, ahead of
    everything `->typed` normally does.  `@nil` unless an incremental
    search is running.

- terminal_image<-search_string: string*
    What the incremental search is looking for, or `@nil`.

- terminal_image<-search_direction: {forward,backward}
    Direction the incremental search is going.

- terminal_image<->exact_case: bool
    Whether the incremental search is case sensitive.

- terminal_image<->search_word: bool
    Whether the incremental search matches whole words only, in the
    sense of `<-syntax`.

    Both outlive the search that used them, and setting either while
    one is running looks again at once, so the hit, the tally and what
    is painted all follow.


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
    Process a single keystroke.  This takes these steps:

    - If a focus function is active -- an incremental search -- the key
      goes to it, and we are done if it succeeds.  This comes first:
      the search would otherwise lose its keys to the accelerators
      below, or to a process running on the terminal.
    - If the event has the `s` (super, Apple ⌘) or the event has both
      shift and control modifiers active _and_ the key is handled as
      an accelerator, we are done.
    - If there is no attached foreground process running _and_
      the key is handled as an accelerator, we are done.
    - Else, encode the event using the terminal escape sequences and
      send the resulting bytes to the client.

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
    Interrupt the process running in the terminal by handing the tty
    its interrupt character.  Fails if `<-foreground_process` reports
    none, leaving the interrupt to a subclass whose client is not a
    process of its own.

- terminal_image->copy_or_interrupt
    Copy if there is selected text, otherwise call `->interrupt`.

- terminal_image->selection: from=[int], to=[int]
    Make [from, to) the selection.  Both `@default` clears it, as does
    an empty region; a single `@default` reaches to that end of the
    buffer.  Indices out of range are clamped, and endpoints that
    arrive the wrong way round are swapped.

- terminal_image->scroll_to: index=int
    Scroll the line holding `index` into view, moving as little as
    possible and doing nothing while the line is already on the screen.
    Fails on the alternate screen and on an index out of range.

- terminal_image->isearch_forward
- terminal_image->isearch_backward
    Start an Emacs style incremental search, towards the end of the
    buffer or towards its start.  `\C-\S-f` is bound to
    `->isearch_backward`, as a terminal's history lies behind the
    caret.  While the search runs it has every key (see
    `<-focus_function`):

    The hit is painted in `<-isearch_style` and the other matches on
    the screen in `<-isearch_other_style`.  What it reports as it goes
    -- through `->report`, so where that lands is the window's business
    -- says which of the matches it is on and how many there are, as
    `(3/4)`.  Those are counted over the whole buffer, and from its
    start whichever way the search is going, so a search backwards
    begins at the last of them and counts down.  Every place the string
    occurs counts, overlapping ones included: a repeat steps a single
    character, so those are places the search can get to.

    | `^S`, `^R`         | The next hit, forwards resp. backwards |
    | `Backspace`        | Drop a character and search again |
    | `^W`               | Take the word behind the hit into the search string, along with whatever separates the two, so that pressing it again walks on word by word.  Not across a line: a search string with a line break in it matches almost nothing |
    | `M-c`              | Turn `<->exact_case` on or off |
    | `M-w`              | Turn `<->search_word` on or off |
    | `^G`               | Give back the view and the selection the search started from |
    | `Escape`, `Return` | Leave the search with the hit selected |
    | Any other key      | Leaves the search, and then means what it usually means |

    Two things differ from `editor->isearch_forward`.  `Escape` and
    `Return` are swallowed rather than passed on: an unhandled key here
    reaches the process on the terminal, and leaving a search is no
    reason to submit a line to a shell.  `^C` is the exception that
    proves it -- it ends the search and then interrupts, or a search
    started over a running program would trap the interrupt.

    Running out of hits only says so; the attempt after that starts
    over at the far end of the buffer.  A search refuses to start on
    the alternate screen, whose lines are not in the buffer, and an
    application that claims the screen ends one that is running.

- terminal_image->window_label: char_array
    Set the enclosing frame's label, e.g. from an OSC 0 sequence.


## Get methods {#class-terminal_image-get}

- terminal_image<-pty_name: -> name*
    Path of the pseudo-terminal device, or `@nil` when not connected
    to one.

- terminal_image<-foreground_process: -> int
    Process group of another session that owns the pty, i.e. of the
    process running in the terminal.  Fails if there is none, which is
    the case while the terminal is driven by a thread of this process.
    While it succeeds the control keys are passed to that process
    rather than looked up in `<-bindings`.  Unix only.

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

- terminal_image<-find: from=int, for=string, times=[int], return=[{start,end}], exact_case=[bool], word=[bool] -> int
    Search the buffer, as `text_buffer<-find` does and with the same
    defaults: case sensitive, ignoring word boundaries, and -- note --
    reporting the *end* of a match going forwards and its start going
    back.  A negative `times` searches backwards.  `from` outside the
    buffer is clamped to the nearest end.  Fails if there is no match.

    Unlike `text_buffer<-find`, a repeat starts one character past the
    match it found, so `times` greater than one reaches the hits after
    the first.

- terminal_image<-length: -> int
    Number of characters in the buffer.

- terminal_image<-contents: from=[int], size=[int] -> string
    Text of the buffer from `from`.

- terminal_image<-cell_style: column=int, row=int -> style
    The style painted over the cell at `column` of the visible `row`:
    the selection, the hit of an incremental search, one of its other
    matches, or the hyperlink under it.  Fails when the cell is drawn
    from its own attributes, which is to say from the colours and the
    bold or underline the client asked for.

    This goes through the very computation the painter uses, so it is
    not a second opinion about what should be drawn.  It says nothing
    about what has actually been drawn yet: a cell whose line has not
    been repainted since still shows what it showed before.

- terminal_image<-cwidth: code=int -> int
    Number of columns the code point `code` occupies when drawn in
    `<-font`: 0 for combining marks, 2 for wide characters and 1 for
    the rest.  This is the classification the renderer itself uses, so
    clients that must predict our layout can share it rather than keep
    a second copy that drifts.  In particular it accounts for symbol
    and emoji code points that the static Unicode tables call width 1
    while an emoji-presenting font draws them twice as wide.  Fails
    while the terminal has no cell metrics yet.


## Character indices {#class-terminal_image-indices}

`<-find`, `<-length`, `<-contents`, `->selection` and `->scroll_to`
address the buffer as a flat sequence of characters.  Index 0 is the
first character of the oldest line still kept.  A wide character counts
once, and the cell holding the right half of it not at all; a combining
mark counts on its own.  Lines are separated by a single newline, except
that a wrapped line and its continuation are not separated at all --
`<-selected`, which hands text to another program, uses `\r\n` instead.

**Indices are relative to the oldest line still kept and shift whenever
output pushes lines out of the scroll-back.**  One is only good until the
client writes again.  These methods do not see the alternate screen: the
lines an application replaced left the buffer, and while it is up they
see the scroll-back that came before it.


## Class variables {#class-terminal_image-classvars}

- font, bold_font: default to `tt` and `boldtt`.
- background, colour: default to `white` and `black`.
- selection_style: yellow background (X) or system selection style.
- isearch_style: green background.
- isearch_other_style: pale turquoise background.
- exact_case, search_word: both `@off`.
- exact_case: `@off`; the incremental search ignores case.
- link_style, link_armed_style: blue, dotted/solid underline.
- save_lines: 1000 by default.
- auto_copy: copy selected text to clipboard automatically (default
  `@on` on macOS, `@off` elsewhere).
- ansi_colours: 16-element vector with the standard ANSI palette.
