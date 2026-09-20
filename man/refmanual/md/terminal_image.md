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

- terminal_image<-italic_font: font*
    Optional slanted font (SGR 3); italic text falls back to `<-font` if
    @nil.

- terminal_image<-bold_italic_font: font*
    Optional bold slanted font (SGR 1 and 3 together).  If @nil, bold
    italic text falls back to `<-italic_font`, then `<-bold_font`, then
    `<-font`.

- terminal_image<->background: [colour]
    Background colour of the buffer.

- terminal_image<->selection_style: [style]
    `style` applied to selected cells.

- terminal_image<->isearch_style: style*
    `style` applied to the hit of an incremental search, in place of
    `<-selection_style`, so that what a search found can be told from
    what the user picked with the mouse.

- terminal_image<->isearch_other_style: style*
    `style` applied to the matches on the screen other than the one the
    user is on, or `@nil` to leave them alone.  It serves an incremental
    search and a selection alike: what a search is looking for while one
    runs, and otherwise the selected text (see `<-selection_string`).
    Only the visible ones: they are worked out afresh every time the
    window is painted, so scrolling brings the ones it reaches into view
    without the search or the selection doing anything about it.

- terminal_image<->nfd_style: style*
    Style applied to NFD grapheme clusters (e.g. accented composed
    characters), or @nil to disable highlighting.

- terminal_image<->link_style: style*
    Style applied to detected hyperlinks.

- terminal_image<->link_armed_style: style*
    Style applied to the hyperlink under the mouse pointer.

- terminal_image<->fold_style: style*
    Style of the fold marker and the tally beside a folded command.
    `@nil` takes both away, and with them the click that toggles a fold.

- terminal_image<->ansi_colours: vector*
    Vector of 16 `colour` objects: the 8 base ANSI colours followed
    by their bright variants.

- terminal_image<-armed_link: bool
    `@on` when the pointer is over a hyperlink.

- terminal_image<-armed_fold: bool
    `@on` when the pointer is over the marker of a fold, i.e. in the
    left margin beside a command whose output can be hidden.

- terminal_image<-blocks: chain
    The commands the client has run, as `terminal_block` objects, oldest
    first.  Empty unless the client marks its prompts; see class
    `terminal_block`.

- terminal_image<->link_message: code*
    Optional code invoked when a hyperlink is activated.  A link is
    followed by Control-clicking it, or, on MacOS, by Command-clicking
    it: Control-click asks for the popup menu there.  See `event
    ->is_popup`.

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

- terminal_image<-selection_string: string*
    The selected text, when it is a selection whose other occurrences
    are highlighted, and `@nil` when it is not.  See `->selection`; it
    is also what `->isearch_selection_forward` searches for.

- terminal_image<-match_word: bool
    Whether matching asks for whole words *now*: `<->search_word`, or a
    selection made by double clicking a word.  A search takes this from
    the selection it was seeded with; one started with `\C-\S-f` has no
    selection to take it from.  This, not `<->search_word`, is what the
    window should show as the state of a `Word` box, so that the box says
    what the tally beside it was counted with.

- terminal_image<-search_direction: {forward,backward}
    Direction the incremental search is going.

- terminal_image<->exact_case: bool
    Whether the incremental search is case sensitive.

- terminal_image<->search_word: bool
    Whether matching asks for whole words only, in the sense of
    `<-syntax`.

    Both decide what counts as a match for a selection as well as for a
    search.  Both outlive the search that used them, and setting either
    looks again at once, so the hit or the selection, the tally and what
    is painted all follow.

    A selection can ask for whole words on its own account -- see
    `<-match_word` -- so turning `->search_word` off also takes that back:
    the setting means what it says.


## Send methods {#class-terminal_image-send}

- terminal_image->initialise: width=int, height=int
    Create a terminal_image of the given column/row size.

- terminal_image->geometry: x=[int], y=[int], width=[int], height=[int]
    Move/resize the image.

- terminal_image->font: font=font, bold=[font], italic=[font], bold_italic=[font]
    Change the regular font and its variants.  A variant left at
    @default is derived from `font`, keeping its family and size.  A
    variant that does not advance by the same amount as `font` is
    rejected with a message and set to @nil: the cell grid is measured
    from the regular font alone, so a variant of another pitch would not
    line up with it.

- terminal_image->compute
    Recompute the rendered image.

- terminal_image->scroll_vertical: direction={forwards,backwards,goto}, unit={file,page,line}, amount=int
    Scroll request from the associated scroll_bar (also bound to
    keyboard navigation).  Stops where the last line of the buffer is on
    the bottom row: there is nothing under it to show, so going further
    would only walk the text off the top.

- terminal_image->event: event
    Top-level event dispatcher.

- terminal_image->input_focus: bool
    The terminal gained (@on) or lost (@off) the keyboard focus.  This
    is called from `->event` on the `activate_keyboard_focus` and
    `deactivate_keyboard_focus` events.  The default implementation
    enables or disables the platform text input (the input method),
    stops an incremental search -- it lives off the keyboard --, updates
    the caret and, if the client asked to be told about the focus
    (`CSI ? 1004 h`), sends it `CSI I` or `CSI O`.

    A subclass may refine this to react on the terminal gaining or
    losing the focus.  A refinement should call the super method to keep
    the above in sync.

    Note that this is not `graphical->keyboard_focus`, which works the
    other way around: it asks the window to give the focus to this
    object.

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

- terminal_image->prompt_mark: kind={prompt,input,output,end}, continuation=[bool]
    Called on OSC 133, the semantic prompt marks: `A`, `B`, `C` and `D`
    decoded to where a prompt starts, where the line the user edits
    starts, where that line was entered and its output begins, and where
    the output ends.  `continuation` is the `k=s` of a secondary prompt,
    i.e. the client is collecting another line of an input it has not
    finished.  The default implementation is what builds `<-blocks`.

    A subclass may refine this to act on what its client is doing; class
    `prolog_terminal` does, to fold the command before the one being
    entered.  A refinement **must not write to the terminal or destroy
    it**: the escape sequence it arrived in is still being parsed, and
    output of its own would move the very lines and blocks that parse is
    holding.

- terminal_image->fold_all
- terminal_image->unfold_all
    Hide the output of every command that has finished, or show it all
    again.  See `terminal_block->fold`.

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

    Selecting text says what to look for as well as what to copy.  The
    other places the selected text occurs on the screen are painted in
    `<-isearch_other_style`, the way the other matches of an incremental
    search are, so that picking out a variable or an atom finds the rest
    of them without typing it again; `<->exact_case` and `<->search_word`
    decide what counts as one.  What it reports -- through `->report`,
    like a search -- says which of the matches the selection is and how
    many there are, as `Selection: X (2/5)`, counted over the whole
    buffer while what is painted is the page.

    Double clicking picks a word, and that says to look for the word
    rather than for the letters it happens to be made of: matching is
    whole-word for such a selection whatever `<->search_word` stands at
    (see `<-match_word`), so a double click on `Bar` passes over `Barn`
    while dragging over the same three characters does not.  It also
    makes a one-character word worth looking for.

    Not every selection is looked for, and `<-selection_string` says
    which are.  A selection that occurs nowhere else says nothing and is
    painted as a plain selection.  Neither is one that is blank, that
    runs over a line break, that is longer than 100 characters, or that
    is a single character while `<-match_word` is `@off` -- each of those
    would light up most of the screen.  Nor is anything looked for
    on the alternate screen, whose lines are not in the buffer.

    What is looked for is the text under the selection as it is now: a
    client that repaints or erases the screen changes what a selection
    holds without moving it, and the matches follow.

- terminal_image->isearch_selection_forward
- terminal_image->isearch_selection_backward
    Start an incremental search for what the selection holds, from where
    the selection is, and step to the next resp. previous match at once
    -- so the matches that are already lit up (see `->selection`) are the
    ones this walks.  From there it is the ordinary search below, `^S`
    and `^R` included.  `\C-s` and `\C-r` are bound to these.

    Fails when there is no such selection, and a binding that fails
    hands the key on: without one, `^S` and `^R` mean to whatever is
    reading from the terminal what they always meant, which for the line
    editor is its own history search.

    `^G` gives back the view *and* the selection the search started from,
    so a search one did not mean to start costs nothing.

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
    the screen in `<-isearch_other_style`.  A search owns that feedback
    while it runs, so the selection's own matches (see `->selection`)
    are not shown as well; leaving a search with the hit selected hands
    them straight over.  What it reports as it goes
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
    | `M-w`              | Turn whole-word matching on or off, as `<-match_word` reports it |
    | `^G`               | Give back the view and the selection the search started from |
    | `Escape`, `Return` | Leave the search with the hit selected |
    | Any other key      | Leaves the search, and then means what it usually means |

    A search that started from a selection
    (`->isearch_selection_backward`) differs in one way: `^G` puts that
    selection back rather than leaving none.

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

- terminal_image<-block: id=int -> terminal_block
    The block with this `<-id`, or fail if it has scrolled out of the
    buffer.

- terminal_image<-block_at: at=int|point|event -> terminal_block
    The block holding a character index, or the one at a position given
    as pixels or taken from an event.  Fails outside every block.

- terminal_image<-fold_at: at=point|event -> terminal_block
    The block whose fold marker is at this position, i.e. fails unless
    the position is in the left margin beside a command whose output can
    be hidden.  What tells a click or a popup on the marker from one on
    the text.

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
client writes again; `terminal_block<-id` is the handle that outlives
them.  These methods do not see the alternate screen: the
lines an application replaced left the buffer, and while it is up they
see the scroll-back that came before it.


## Folding {#class-terminal_image-folding}

The output of a command that has finished can be taken off the display
with `terminal_block->fold`, leaving the command that produced it with a
triangle in the left margin and a tally of what is hidden.  A click on
the triangle toggles it; unlike a hyperlink it needs no modifier,
because the marker belongs to the terminal rather than to the client.

Folding is a property of the view and not of the text.  `<-contents`,
`<-find`, `<-selected` and `->select_all` keep seeing what a fold hides;
only what is painted, the rows a click maps to and the scrollbar leave
it out.  `->scroll_to` opens a fold it lands in, as does an incremental
search whose hit is inside one.

Closing a fold takes rows out of the window, so a window that was showing
the end of the buffer is pulled back to go on showing it.  One the user
has scrolled away from stays where they put it.

`<-fold_at` says whether a position is on a marker, which is what lets a
popup on the marker be about the one command it stands beside rather than
about the terminal.  Class `prolog_terminal` uses it for `->block_popup`.

## Class variables {#class-terminal_image-classvars}

- font, bold_font, italic_font, bold_italic_font: default to `tt`,
  `boldtt`, `itt` and `bitt`.
- background, colour: default to `white` and `black`.
- selection_style: yellow background (X) or system selection style.
- isearch_style: green background.
- isearch_other_style: pale turquoise background.
- exact_case, search_word: both `@off`, so matching ignores case and
  does not ask for word boundaries.
- link_style, link_armed_style: blue, dotted/solid underline.
- fold_style: grey50; `@nil` takes the fold markers away.
- save_lines: 1000 by default.
- auto_copy: copy selected text to clipboard automatically (default
  `@on` on macOS, `@off` elsewhere).
- ansi_colours: 16-element vector with the standard ANSI palette.
