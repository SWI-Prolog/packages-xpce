# class fragment {#class-fragment}

A fragment object describes part of a text_buffer object.  Fragments are
maintained by the text_buffer they are associated with and move along
with edit operations, so they keep pointing to the same text.

Fragments have been implemented to allow for annotation of textual
information.  Besides defining a range of characters, fragments define
their visual appearance when displayed by an editor.  The location of
fragments in a text can be visualised by a text_margin object
associated with an editor object.  This visualisation is done using
icons (image objects).

Fragments may be used to realise multiple fonts, styles and colours in
editor objects.  Another use for fragments is to make fragments of the
text `active`, e.g.  allow the user to follow a link from the specified
fragment (hyper-text).  Finally, fragments may be used as text-markers.

The following fragment turns the selection of an editor into
bold font:

	demo :-
		send(new(V, view), open),
		send(V, style, bold, style(bold := @on)),
		send(V, key_binding, '\C-b',
			 message(@prolog, make_bold, V?editor)).

	make_bold(E) :-
		get(E, selection_start, Start),
		get(E, selection_end, End),
		Len is End - Start,
		new(_, fragment(E, Start, Len, bold)).

@see editor-selected_fragment
@see editor-margin
@see class style
@see editor-selected_fragment_style
@see editor-styles
@see editor->selected_fragment
@see text_buffer<-find_all_fragments
@see class font
@see class editor
@see class text_buffer
@see class text_margin


## Instance variables {#class-fragment-instvars}

- fragment-previous: fragment*
    Previous in double-linked chain.

- fragment-next: fragment*
    These variables together with `text_buffer <-first_fragment` and
    `text_buffer <-last_fragment` define the double linked list of fragments
    that is sorted on <-start.

    These methods are normally not used to analyse the available fragments.
    See `text_buffer <-find_all_fragments` and `text_buffer<-find_fragment`
    for locating fragments.

- fragment<-style: [name]
    Logical name of the fragment.  This logical name expresses the semantics
    of the text inside it (title, example, ...).  Using `editor ->style` the
    user specifies how fragments of this style are visualised.

    @see text_margin-styles


## Send methods {#class-fragment-send}

- fragment->end: int
    *Inherits description from*: fragment<-end

- fragment->does_include: what={start,end}
    Test whether start or end is included.

- fragment->include: what=[{start,end,both}], include=[bool]
    Define whether start and end are included.  If text insertion
    takes place exactly on the corresponding end of the fragment,
    the fragment is expanded if include equals @on or left untouched
    if include equals @off.

    _What_ defaults to `both` (include both start and end).
    `include` defaults to @on.  Initially neither of the ends is
    included.  See also ->insert.

- fragment->initialise: text=text_buffer, start=int, length=int, style=[name]
    Create a fragment on the specified text_buffer object.  The fragment
    starts are <-start and is <-length (may be 0) characters long.
    <-style is a logical indicating the category of the fragment
    (e.g.  title, example, glossary, refers_to, ...).  The visual
    implication of a style-name is determined using `editor ->style`
    and/or `list_browser ->style`.

- fragment->insert: [int], char_array
    Insert text in the fragment.  The fragment actually invokes `text_buffer
    ->insert' with the properly translated arguments.  If the index is
    @default, the text is appended to the fragment.   For example, the
    PceEmacs shell mode uses a fragment to insert the output of the
    sub-process.  See xpce/prolog/lib/emacs/shell_mode.pl.

- fragment->overlap: int|fragment|point
    Succeed if the fragment overlaps the specified index, overlaps with the
    specified fragment or with a range starting at `point <-x` to
    `point<-y`.  Often used in combination with `text_buffer<-find` to
    find a fragment overlapping the caret.

- fragment->start: int, move_end=[bool]
    Start index (0-based).  If `move_end` equals @off, the end-point
    is not moved.  Otherwise the end-point is moved by the same
    amount (<-length is not changed).

- fragment->unlink
    Unlinks the fragment from the <-text_buffer and inform the
    possible related editors of the change.


## Get methods {#class-fragment-get}

- fragment<-end: -> int
    The <-end is defined as <-start + <-length.  Modifying the <-end
    modifies the <-length of the fragment.

- fragment<-previous: condition=[code] -> fragment

- fragment<-next: condition=[code] -> fragment
    Return the next/previous fragment of the double linked fragment
    list for which code succeeds.  If code is omitted, these methods
    return the next/previous fragment of the list.  While executing
    the condition, @arg1 is bound the candidate fragment.  Execution
    may *not* alter the fragment list of the associated
    <-text_buffer.

    If no fragment can be found in the indicated direction, these
    methods fail.
