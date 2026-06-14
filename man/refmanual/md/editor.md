# class editor {#class-editor}

Class editor describes a graphical object that can be used to edit
long textual descriptions.   PCE editors have similar design and
user-interface as GNU-Emacs.   New functionality may added by
means of class key_binding.

WHY A PCE EDITOR?

Many applications manipulate large text fields.  For many applications,
editing text is one of the most important activities.  Unfortunately,
there is not much of a standard as far as editors is concerned.  For
this reason, the best option would be to allow the user to run an editor
of his/her choice.  Unfortunately no good interfacing protocol
is available that allows us to manage the text in the editor
from inside PCE and read a considerable amount of status
information from the editor.

Therefore, PCE defines it's own editor.  The basic user interface of the
editor is based on the widely accepted GNU-EMACS editor and can be
extended and modified to improve compatibility to other editors.


## Organisation {#class-editor-organisation}

Class editor is a subclass of `device` because it is implemented as a
compound graphical object, consisting of the following parts.

	| text_image object  | Graphical that displays the text itself  |
	| scroll_bar object  | Allows you to scroll the text vertically |
	| text_cursor object | Graphical that displays the caret        |
	| text_margin object | Optional annotation margin               |
	| text_buffer object | Storage and low-level manipulation       |

## Vocabulary {#class-editor-vocabulary}

The editor uses the following vocabulary:

- <-caret
	Current position of the `insertion-point`, indicated using a
	text_cursor object.  For proportional fonts, this is a
	triangle with a vertical line-segment.  For fixed fonts,
	this is a static block.

- <-mark
	Unvisible point that, together with the <-caret,
	describes the REGION.  Many operations work on a REGION
	of the entire text (deleting, formatting, etc.).

- region
	All characters between the <-caret and the <-mark.

- argument
	Many interactive operations accept a numeric argument.
	The numeric argument is set using one of the sequences

		\C-u digit ...
		M-<digit> ...

	and reset after the completion of the next interactive command.
	The interpretation is command dependent, but most commands
	interpret the argument as `do this command so many times`.


## Compatibility {#class-editor-compatibility}

The UI of PCE editors is based on GNU-EMACS.  Many of the basic function
of GNU-EMACS are available.  Like GNU-EMACS, the PCE editors can be
programmed.  Unlike GNU-EMACS however, this is not done by programming
EMACS-LISP, but by adding methods to the editor (or class editor) and
linking these methods to key-strokes.

As the basic functionality is rich and can be expanded/redefined, it
should not be very difficult to transform the PCE-editor into an editor
that is largely compatible to most available editors.


## Mouse and keyboard {#class-editor-mouse-and-keyboard}

PCE-editors can be operated both mouse-driven and keyboard-driven,
although the latter is the principal way of manipulating a text.

@see class key_binding
@see class view
@see class text
@see class text_buffer
@see class style
@see class fragment
@see class font
@see tool Card Viewer


## Class variables {#class-editor-classvars}

- editor.exact_case: bool = @off
    @see editor-exact_case

- editor.indent_increment: int = 2
    @see editor-indent_increment


## Instance variables {#class-editor-instvars}

- editor<->auto_newline: bool
    If @on, a newline is appended automatically after each ->append.
    Backward compatibility only.

    @see editor->append

- editor<->bindings: key_binding
    Key_binding object used to map typing onto methods defined on the
    editor.  It is filled with an empty key_binding object that has
    `key_binding <-defaults`  bound to the built-in key_binding object
    named `editor`.  This implies that bindings created using ->key_binding
    are local to the editor.

    @see editor->typed

- editor<-caret: int
    The <-caret is the current `insertion` point.  In ranges from 0 up to and
    including <-size.  Zero implies `insert at the beginning`; <-size
    implies `append at the end`.

    The method ->caret is invoked explicitly from all internal
    methods that manipulate the caret and this method may thus
    be redefined to keep track of the users caret-movements.

    The <-mark is a mark that moves with inserts and, together with
    the <-caret, defines the region.  The region can be in various
    modes as described by <-mark_status.

    To get the caret from an event, see `text_image<-index`.  The
    location of the caret is visualised by a text_cursor object that
    can be requested using <-text_cursor.

    @see editor-mark

- editor-dabbrev_origin: int*
    Caret index for the beginning of the word on which dynamic expansion was
    started.

    @see editor->dabbrev_expand

- editor-dabbrev_pos: int*
    Caret index at which the dynamic expansion was initiated.  The string
    expanded ranges from `dabbrev_origin` to `dabbrev_pos`.

    @see editor->dabbrev_expand

- editor-dabbrev_reject: chain*
    Chain of names of expansion alternatives already rejected by the user.

    @see editor->dabbrev_expand

- editor-dabbrev_target: name*
    Name, representing the start of the word to be expanded.

    @see editor->dabbrev_expand

- editor<->editable: bool
    Boolean that indicates whether or not the text can be edited.  If @off, a warning
    is generated whenever the user attempts to invoke an interactive command that
    would change the contents of the associated text_buffer.

    See also ->show_caret for hiding the caret on read-only editors.

            BUG: In the original design, methods were fixed and methods
    either implemented behaviour intended for user interaction or
    methods intended for program interaction.  The ->editable flag
    only affected interactive methods.  Some of this distinction
    still remains.  It is adviced to set editable to @on before
    modifying a read-only editor object from the program to avoid
    confusion.

    @see text_buffer-modified

- editor<->error_message: code*
    When an error is trapped in a method, it is reported to the editor using
    ->report.   When defined, ->report will execute this message with
    arguments given below:

    	@receiver:	The editor (or view)
    	@arg1		idem
    	@arg2		The type (status/warning/inform/error)
    	@arg3		String representing the message itself

    See `visual ->report` for a general discussion on the reporting
    system.

    **Defaults**: @nil

    @see display->inform
    @see graphical->alert

- editor<->exact_case: bool
    Determines whether or not search and replace is case-sensitive.
    ->switch_case_mode sets <-exact case according to the argument:

    	| @default | toggle <-exact_case |
    	| > 0      | ->exact_case: @on   |
    	| <= 0     | <-exact_case: @off  |

    **Defaults**: @off (Resource determined).

    @see editor->isearch_forward
    @see editor->isearch_backward
    @see editor->switch_case_mode
    @see editor.exact_case

- editor<-file: source_sink*
    File that is associated with the editor.

    @see editor->save_buffer
    @see editor->save
    @see editor->load

- editor<->fill_mode: bool
    If @on, the current paragraph will be automatically reformated if the
    current line exceeds the <->right_margin while inserting text.
    ->auto_fill_mode may be used to set <-fill_mode interactively:

    	| @default: | toggle <-fill_mode |
    	| > 0       | ->fill_mode: @on   |
    	| <= 0      | ->fill_mode: @off  |

    @see editor->insert_self_fill
    @see editor->insert_self

- editor<->focus_function: name*
    The <->focus_function variable is used to process commands that start a
    kind of `sub-mode`, such as entering a numeric argument, doing
    incremental search, doing a dabbrev expansion, etc.

    An interactive behaviour that uses such a sub-loop sets this variable to
    the selector that handles the loop.  As a consequence, ->typed will
    invoke this method on each subsequent character typed until

    1. The method fails.  In this case the character is handled just
    	    as if no focus_function was defined.

    2. The method resets <->focus_function to @nil.

    If the method invoked via <->focus_function succeeds, ->typed assumes
    the character is handled.

    By convention, the method that is invoked by the focus function has the
    same name than the method that set the focus preceded by an underscore:

    	quoted_insert ->
    		_quoted_insert

- editor<-font: font
    Font used to display the contents of the associated text_buffer.  Class
    editor can handle both proportional and non-proportional fonts.

- editor<-image: text_image
    Text_image object used for actually displaying the text.  Direct access
    to this object may be necessary to exploit the following methods:

    	| `text_image ->tab_stops` | Set vector of tab-stops |
    	| `text_image ->wrap`      | Control line-wrapping   |

    Direct access may also be used to attach event-handling to the editor.
    For example, if a popup is attached to the editor as a whole it might
    contradict with the <-scroll_bar.  Associating the popup with the
    <-image ensures it only operates in the text area.

    @see class text_image

- editor<->indent_increment: int
    Number of characters with which the indentation is
    incremented/decremented by the indentation-manipulation commands.

    This group of commands is intended for editing source-code.

    **Defaults**: 4 (Resource defined)

    @see editor->undent_selection
    @see editor->undent_region
    @see editor->undent_line
    @see editor->indent_selection
    @see editor->indent_region
    @see editor->indent_line
    @see editor.indent_increment

- editor-internal_mark: alien:int
    Additional (floating) mark, used by some internal functions.

- editor<-label_text: text*
    Text object that displays the label.  Initially @nil.  A label
    is associated using ->label.  See also <->show_label.

- editor<->left_margin: int
    Margins used for filling text.  See ->fill, ->justify_paragraph,
    ->fill_mode, etc.  The method ->set_fill_column may be used to set both
    methods interactively (Dope compatibility):

    	| @default | Report current margins                   |
    	| > 0      | Set ->right_margin                       |
    	| < 0      | Set ->left_margin to the absolute value. |

    **Defaults**: 0

    @see editor->append
    @see editor->fill_selection
    @see editor->fill_region
    @see editor->fill_paragraph
    @see editor-right_margin
    @see editor->set_fill_column

- editor<-margin: text_margin*
    Margin with icons that indicate the position of fragments.  See
    `fragment <-style`, and ->style.  When the ->margin_width is set to a
    value > 0 and there is no <-margin, a margin is created.

    **Defaults**: @nil (no margin)

    @see class fragment
    @see editor->margin_width
    @see class text_margin

- editor<-mark: int
    *Inherits description from*: editor-caret

    @see editor->set_mark
    @see editor->point_to_mark
    @see editor->exchange_point_and_mark
    @see editor-caret

- editor<-mark_ring: vector
    Ring of old marks.  If a new mark is set using ->mark, the old
    mark is pushed into the ring.  The maximum size of the ring is
    hard-coded to 16.   The command ->set_mark with a prefix
    argument cycles throught the saved marks.

- editor<-mark_status: {active,inactive,highlight}
    Status of the mark/region.  Defined values are:

    - active
    	The region (selection) is highlighted using <-selection_style.
    	Region commands are effective if the mark is in this state.

    - inactive
    	The mark is there, but the region is not visible and
    	region commands are not executed.  The current region
    	can be made active using ->exchange_point_and_mark
    	(C-x C-x).  The regions is made inactive with the
    	command ->keyboard_quit (C-g).

    - highlight
    	The region is highlighted, but not available for
    	region-commands.  This mode is used for highting parts
    	of the text, such as using the comment ->select_line.

    See also ->caret and  ->selection.

- editor<->modified_message: code*
    When the value of the <->modified attribute of the associated
    text_buffer changes and this message is not @nil, this message is
    executed with the following arguments:

    	| @receiver | Editor or view           |
    	| @arg1     | New value of <->modified |

    @see text_buffer-modified

- editor<->right_margin: int
    **Defaults**: 72 (Resource determined).

    *Inherits description from*: editor-left_margin

    @see editor->fill_selection
    @see editor->fill_region
    @see editor->fill_paragraph
    @see editor->set_fill_column
    @see editor->insert_self_fill
    @see editor-left_margin

- editor<-scroll_bar: scroll_bar
    Scroll_bar object attached to the editor.  In the current implementation
    it is not possible to remove the scrollbar or place it at the right-side
    of the editor.  However, setting the `scroll_bar ->width` to 0 before
    the editor is displayed effectively removes it.

    @see class scroll_bar

- editor-search_base: int
    Character index from which the last search was started.

    @see editor->isearch_forward

- editor-search_direction: {forward,backward}
    Current direction of the search.  Set by ->isearch_forward and
    ->isearch_backward to resp. `forward` and `backward`.

    @see editor->isearch_backward
    @see editor->isearch_forward

- editor-search_origin: int
    Place where the search was initiated.  Set by ->isearch_forward and
    ->isearch_backward.  Used to return when the search is canceled.

    @see editor->isearch_forward

- editor<->search_string: string*
    Current target string for incremental search.

    @see editor->isearch_forward

- editor<-selected_fragment: fragment*
    Currently selected fragment.  The visual appearance of this fragment is
    determined by <-selected_fragment_style.  There is no default
    user-interface for selecting fragments.  `text_buffer <-find_fragment`
    and `text_margin <-fragment` may be useful methods for defining a
    user-interface.

    @see editor-selected_fragment_style
    @see class fragment

- editor<-selected_fragment_style: style
    @see class style
    @see editor-selected_fragment
    @see text_margin-styles
    @see class fragment

- editor<-size: size
    Size management of class editor has been redefined to communicate in
    character units rather then in pixel units.  Width is expressed in
    `font <-ex` of the current <-font., height in `font <-height`.

    The methods ->_size, ->area and ->set may still be used to handle the
    pixel size directly.

- editor<-styles: sheet
    Mapping from style names as stored in `fragment <-style` onto style
    objects that determine the actual appearance of a fragment.  This
    separates semantical information represented by the fragment from
    visualisation as achieved in the editor.  Setting the style to
    @nil deletes the visual appearance of fragments with this name.

    The following skeleton illustrates this.  _E_ is an editor.  The final code
    fragment creates a fragment to turn the current selection into a title.

    	:- pce_global(@title_style,
    				  new(style(font :=
    							font(helvetica, bold, 16)))).

    		...
    		send(E, style, title, @title_style),
    		...

    		...
    		get(E, selection, point(Start, End)),
    		Length is End - Start,
    		new(F, fragment(E, Start, Length, title)),
    		...

    @see text_margin-styles
    @see class fragment

- editor<-tab_distance: characters=int
    Distance between tab-stops in `font <-ex` units of the current <-font.  Mainly
    used for handling source-code.  When ->tab_stops is defined the value of
    this variable is ignored.

- editor<-text_buffer: text_buffer
    The <-text_buffer stores the text.  Class editor delegates messages not
    understood to the <-text_buffer.  A text_buffer can have multiple
    editors operating on it simultaneously.  The text_buffer object
    associated to an editor may be changed anytime.

- editor<-text_cursor: text_cursor
    Visualisation of the caret.  It may be displayed in various styles.  See
    class text_cursor.  The default caret is a static inverting block for
    fixed-width fonts and a line with triangle for proportional fonts.


## Send methods {#class-editor-send}

- editor->_dabbrev_expand: event_id
    @see editor->dabbrev_expand

- editor->align: column=int, index=[int]
    Insert/delete spaces and tab characters such that the character
    at `index` [<-caret] will have <-indentation `column`.   When
    layout characters are deleted, at least a single space remains.

    Useful for realising automatic indentation and alignment of comments in
    syntax driven editors.

- editor->align_line: column=[int]
    Align the line holding the caret such that the first non-layout
    character is at the indicated column.   When columns is
    omitted, <-left_margin is used.  See also ->align, align_region
    and ->indent_line.

- editor->align_region: column=[int]
    Apply ->align_line to all lines in the current region (mark, caret).

- editor->append: text=char_array
    Append the specified text.  If <-left_margin is not zero, ->align the
    last line to <-left_margin.  Then append the text.  If <-auto_newline
    is @on, append a newline too.  Set the ->caret at the end of the
    buffer.

    ->print ->insert's the text at the <-caret, followed by a newline
    character if <-auto_newline equals @on.

    **Bugs**: Messy.  New code should use the ->align and ->format methods.

    @see editor-left_margin
    @see editor-auto_newline

- editor->print: text=string
    Insert text at caret (auto_newline).

- editor->appendf: format=char_array, argument=any ...
    Formatted append (see ->format and `string ->format` (argument
    specification).

- editor->auto_fill: from=[int], skip=[regex]
    Fill after ->insert_self_fill detected a long line.  The default
    implementation fills from the start of the current line to the
    end of the paragraph, using the left-margin from the current
    indentation.

    This method is intended for redefinition in language mode
    to perform more mode-dependent filling such as breaking
    long comments, log argument lists, etc.

- editor->auto_fill_mode: [int]
    *Inherits description from*: editor-fill_mode

- editor->backward_paragraph: [int]
    Move paragraphs backward.

- editor->backward_sentence: [int]
    Move sentences backward.

- editor->backward_term: [int]
    Move Prolog terms backward.

- editor->backward_word: [int]
    Move words backward.

- editor->backward_char: [int]
    Move the caret the `times` the indicated unit backwards.  The definition
    of words, sentences and paragraphs depends on the syntax_table object
    associate to the <-text_buffer.   See also `text_buffer <-scan`.

- editor->delete_char: [int]
    Delete characters forward.

- editor->backward_delete_char: [int]
    Delete characters from the caret.  By default deletes a single
    character.

- editor->kill_line: [int]
    Kill lines forward.

- editor->kill_paragraph: [int]
    Kill paragraphs.

- editor->kill_sentence: [int]
    Kill sentences forward.

- editor->kill_term: [int]
    Kill Prolog terms.

- editor->kill_word: [int]
    Kill words forward.

- editor->backward_kill_word: [int]
    Kill the specified (default 1) number of units in the specified
    direction.  See ->kill and ->yank for details on killing and
    yanking (cut/copy) text.

- editor->beginning_of_line: [int]
    Move lines backward, default is to the start of the current line.

- editor->downcase_previous_word: [int]
    Lower-case n words before caret.

- editor->upcase_previous_word: [int]
    Uppercase n words before caret.

- editor->capitalise_previous_word: [int]
    Similar to the ->capitalise_word/->upcase_word and ->downcase_word
    group, but affects the word *before* rather than after the <-caret.
    Saves keystrokes when used to correct typos while entering text.

- editor->downcase_word: [int]
    Loser-case n words after caret.

- editor->upcase_word: [int]
    Uppercase n words after caret.

- editor->capitalise_word: [int]
    Change the case of the word, starting at the first letter scanning
    forwards from the <-caret up to the end of this word.  ->capitalise_word
    maps the first letter to uppercase and the remainder to lowercase.

- editor->clear
    Remove all text and fragments using `text_buffer ->clear`.  Deletes the
    selection and sets the <-caret to 0.

- editor->column: column=int
    *Inherits description from*: editor<-column

- editor->compute
    Invoke `text_image ->compute`.  Next verifies that the <-caret is in the
    window and moves the window if necessary.  Finally update the
    <-text_cursor, <-margin and <-scroll_bar.

- editor->cua_key_as_prefix: event|event_id
    Test whether to use the keys \C-x or \C-c as Emacs prefix keys
    or CUA-mode Cut/Copy commands.  The rules for this are based
    on cua.el, a GNU-Emacs mode merging CUA and Emacs by
    Kim F. Storm (storm@cua.dk):

    - If there is an active selection (see <-mark_status),
    	   map them to copy or cut.
    - If the shift-key is depressed, map them to their Emacs
    	  prefix behaviour.
    - If another key is depressed within 0.25 seconds, use
    	  map them to their Emacs prefix behaviour.

    According to Kim this results in pretty normal behaviour for
    both Emacs and CUA users.  CUA users expect the bevahiour
    on the selection only.  Emacs users have some trouble with
    \C-c or \C-x commands that act on the region, but luckily
    these conflicts are acceptable.

    The underlying behaviour is implemented by `key_binding->typed`.

- editor->dabbrev_expand
    Expand the word before the caret.  This is achieved by scanning the
    current buffer backwards for words that start the same way as the
    partial current word.   Successive execution of this method will try
    alternative completions.

    @see editor-dabbrev_target
    @see editor-dabbrev_reject
    @see editor-dabbrev_pos
    @see editor-dabbrev_origin
    @see editor->_dabbrev_expand

- editor->delete: from=int, to=int
    Each of the commands ->delete, ->grab and ->kill operate on characters
    in the range [from, to) (i.e.  `to` minus `from` characters).  If `to` <
    `from`, the arguments are first swapped.   The commands may or may not
    delete the range and may or may not add the deleted text to the
    kill-buffer @text_kill_ring:

    	| ->delete | Just deletes the text                              |
    	| ->kill   | ->delete and add the deleted text to the kill ring |
    	| ->grab   | Add the text to the kill-ring without deleting it. |

    See also @text_kill_ring and ->yank.

- editor->delete_blank_lines
    GNU-Emacs compatible command.
    See also ->delete_horizontal_space and ->just_one_space.

- editor->grab: from=int, to=int
    Add text to the kill-buffer.

- editor->kill: from=int, to=int
    Delete text and add it to the kill-buffer.

- editor->delete_horizontal_space: [int]
    Delete all white space (*no* newlines) around caret and reinsert the
    specified number of spaces.   Default is to insert 0 spaces.  See also
    ->just_one_space.

- editor->capitalise_region
    Capitalise words in region.

- editor->downcase_region
    Map all characters in the region [<-mark ... <-caret) to the specified
    case.  ->capitalise_region will map any first letter of a word to
    uppercase and any other letter to lowercase.

    @see editor->upcase_region

- editor->electric_caret: index=int, seconds=[real]
    Temporary display the caret at the indicated position. The caret is
    restored to the insertion-point after the indicated time (in seconds) or
    after the user has typed in the editor.

    This method may be used to indicate a related position. For example if
    the user types a closing-bracket this method may be used to indicate the
    matching opening bracket.

    **Defaults**: The default time to restore the caret is 0.5 seconds.

    @see editor->show_caret_at

- editor->event: event
    Process an event:

    1. Succeed if `device ->event` succeeds.  This allows for
    	redefining event-handling on any of the parts of the editor.

    2. If the event is a keyboard event, invoke ->typed.

    3. If the event is an area event, update the
    	`text_cursor->active` status.

    4. If the recogniser object @editor_recogniser exists,
    	invoke `recogniser ->event` to this object and return
    	the result thereof.

- editor->exchange_point_and_mark
    Exchange the location of <-mark with <-caret.  Note that commands
    operating on the region (the text between the two) do not care which of
    the two has the lowest index.

    @see editor-mark

- editor->fill: from=int, to=int, left_margin=[int], right_margin=[int], justify=[bool]
    Fill paragraphs in the range [from, to).  The specified margins default
    to <-left_margin and <-right_margin.  If `justify` equals @on, ->fill
    inserts spaces such that the right_margin will be straight.

    **Bugs**: Only works properly with fixed-width fonts,

- editor->fill_region
    Fill paragraphs in region.

- editor->fill_selection
    Compat: ->fill_region.

- editor->fill_paragraph: justify=[int]
    Frontend methods to ->fill the specified text.  The margins default to
    <-left_margin and <-right_margin.

    @see editor-right_margin
    @see editor-left_margin

- editor->find_cut_buffer: [int]
    Find the value of the indicated `display <-cut_buffer` as a plain
    string.  Respects <-exact_case.   If the string is found it is selected,
    otherwise a warning is ->report'ed.

- editor->format: format=char_array, argument=any ...
    Inserts formatted text at the <-caret location.  See `string ->format`
    for a description on the format specification.  See also ->appendf.

- editor->forward_paragraph: [int]
    Move paragraphs forward.

- editor->forward_sentence: [int]
    Move sentences forward.

- editor->forward_term: [int]
    Move Prolog terms forward.

- editor->forward_word: [int]
    Move words forward.

- editor->forward_char: [int]
    Move the caret the `times` the indicated unit forwards.  The
    definition of words, sentences and paragraphs depends on the
    syntax_table object associate to the <-text_buffer.  See also
    `text_buffer <-scan`.

- editor->geometry: x=[int], y=[int], width=[int], height=[int]
    Handle geometry requests to the editor.  Properly resizes and positions
    the <-scroll_bar, <-image and <-margin.   The minimum size is defined
    to be 50x20 pixels.

- editor->gosmacs_transpose
    Transpose characters compatible to the GosMacs editor: transposes the
    character before the caret with the one before that one.   The advantage
    lies in correcting character transposition typos.  Using
    ->gosmacs_transpose this requires a single keystroke.  Using
    ->transpose_chars this requires 3 keystrokes.

- editor->height: int
    *Inherits description from*: editor-size

- editor->hover_fragment_icon: fragment=fragment*, icon_area=[area]
    Virtual method.  Called from the text_margin object when the
    mouse enters and exits the area of an icon.  On enter the
    fragment object and the area where the fragment is painted
    relative to the margin is passed.  On exit, the fragment is @nil
    and the argument @default.

- editor->indent_region: [int]
    Indent lines in region by <-indent_increment.

- editor->indent_selection: [int]
    Compat: ->indent_region.

- editor->undent_line: [int]
    Unindent line by <-indent_increment.

- editor->undent_region: [int]
    Unindent lines in region by <-indent_increment.

- editor->undent_selection: [int]
    Compat: ->undent_region.

- editor->indent_line: [int]
    Increase (undent: decrease) the indentation of the current
    line by `arg` times <-indent_increment.  This group of methods
    is compatible to the Dope editor and allows for manipulating the
    indentation when editing source-code.  See also ->align_line.

    @see editor-indent_increment

- editor->initialise: text=[text_buffer], width=[int], height=[int], margin=[int]
    Create an editor object from the specified arguments.  If no text_buffer
    is specified, a default empty text_buffer object is used.  The
    width and height arguments are interpreted in terms of character
    units.

    The margin specifies the with of the text_margin object attached to the
    right side used for annotations.  The default is 0, attaching no
    annotation margin.  See also ->margin.

    Class view encapsulates an editor object in a window.

- editor->insert: text=char_array
    Insert the given text at the caret and move the caret to point just after the
    inserted text.

- editor->insert_cut_buffer: [int]
    Insert the text from the `display <-cut_buffer`.   Move caret to the end.

- editor->insert_self_fill: times=[int], character=[char]
    Insert char n times; adjust margins.

- editor->insert_self: times=[int], character=[char]
    Insert the argument character `times` times.

    The ->insert method calls ->insert_self_fill if <-auto_fill_mode
    equals @on.

    The ->insert_self_fill calls ->auto_fill if -after the insertion-,
    the text extends the <-right_margin.   Normally this fills the
    remainder of the paragraph.  By redefining ->auto_fill, more
    specialised handling of long lines can be implemented.

    @see editor-fill_mode

- editor->isearch_backward
    Start incremental search backward.

- editor->isearch_forward
    Start an *incremental* search.  The editor will search for the typed
    plain text (respecting <-exact_case) and indicate the first `hit` using
    the selection on each subsequent letter typed by the user.  While
    searching, the following keys have special meaning:

    	keyboard_quit			Abort and revert to where search started
    	isearch_backward
    	isearch_forward			Revert direction or search for next
    	backward_delete_char	Remove the last character from the target
    	ESCAPE					Terminate search
    	\C-w					Extend target to current word

    @see editor->isearch_backward
    @see editor-search_string
    @see editor-search_origin
    @see editor-search_direction
    @see editor-search_base
    @see editor-exact_case

- editor->just_one_space
    Replace all white-space (tab, space, *not* newline) surrounding the
    caret by a single space.

- editor->justify_region
    Justify region.

- editor->justify_paragraph
    Front-end to ->fill the specified unit of text.  The `justify` argument
    of ->fill is set to @on, creating a straight right margin.

- editor->key_binding: key=name, action=name|code
    Add a new binding to <-bindings using `key_binding ->function` to this
    object.  The key_binding remains local to the receiving editor.  The
    method <-key_binding requests the current binding (local or global)
    using `key_binding <-function`.

- editor->kill_or_grab_region: [int]
    Without an argument, ->kill the current region.  Otherwise
    ->grab the region.

- editor->label: name
    Associate a label with the editor and set the string of the
    label.  The label is displayed by <-label_text.  The font of
    the label is dictated by editor.label_font. See also <->show_label.

- editor->line_number: line=int
    Position the caret at the start of nth (1-based) line.

- editor->line_to_top_of_window: [int]
    Scroll such that the current line is the nth (1 based, default = 1) line
    of the window.  See also ->normalise.

- editor->load: file=source_sink
    Load the contents of the specified file object.  Performs the
    following steps:

    	->clear						Clear the editor
    	`text_buffer ->insert_file`	Insert the file contents
    	<-file						Assign the file variable
    	->caret: 0					Put the caret `home`
    	`text_buffer ->modified: @off`	Clear modified
    	`text_buffer ->reset_undo`	Clear undo info
    	->editable: Bool

    If `file->access: write` succeeds, ->editable: @on is
    invoked on the editor, otherwise ->editable: @off.

    @see editor-file

- editor->lost_text_buffer
    Invoked by the associated <-text_buffer if it is being freed.
    By default, this method will simply send ->free to the editor,
    or its associated view object.

    This method may be redefined, but should either free the editor,
    or associate another (new) text_buffer object with the editor.

    See also `text_buffer ->unlink`.

- editor->margin_width: pixels=int
    *Inherits description from*: editor-margin, editor<-margin_width

    @see style-icon
    @see editor-margin
    @see class text_margin

- editor->mark: mark=[int], status=[{active,inactive,highlight}]
    Set mark and region-status.  The region (selection) is defined
    by the characters in the range [<-mark ...  <-caret) or [<-caret
    ..  <-mark) if the caret is before the mark.  The old position
    of the mark is pushed in <-mark_ring.  If the mark is
    `active`, it can be used for region-commands.

    See also ->set_mark, <-mark_ring and ->selection.

- editor->newline: [int]
    Insert `times` newline characters.  ->newline inserts before the caret,
    ->open_line *after* the caret, so the caret remains at the same line.

- editor->open_line: [int]
    Insert newlines after caret.

- editor->newline_and_indent: [int]
    Start a new line and copy the indentation from the previous line.  The
    argument defines the number of lines inserted.

- editor->next_line: lines=[int], column=[int]
    Move lines downward; place caret at column.  If columns is omitted,
    <-column is used.   Normally the column is maintained by class
    key_binding.

    @see editor-save_column

- editor->normalise: from=[int], to=[int]
    Try to make the range [from,to) visible.  If this range is too big to
    fit on a screenful, the line on which `from` resides will be the first
    line displayed.  See also ->line_to_top_of_window and
    ->recenter.

    **Defaults**: Both arguments default to `Editor <-caret`

    @see editor->selection

- editor->point_to_bottom_of_window: [int]
    Scroll caret to bottom of window.

- editor->point_to_bottom_of_file: [int]
    Place the caret at the nth-line from the end of the file/window.
    Without an argument, the caret is placed at the end of the file/window.

- editor->point_to_mark
    Move <-caret to the <-mark.  In most cases ->exchange_point_and_mark is
    to be preferred.  See also ->set_mark.

    @see editor-mark

- editor->point_to_top_of_window: [int]
    Move to 1st character of window.

- editor->point_to_top_of_file: [int]
    Move caret to the nth-line from the top of the file/window.  Default is
    the start of the file/window.  See also ->line_number.

- editor->previous_line: lines=[int], column=[int]
    @see editor-save_column

- editor->recenter: [int]
    Same as ->line_to_top_of_window, but centers the current line on the
    window rather than making it the first line.

- editor->report: kind={status,inform,progress,done,warning,error,fatal}, format=[char_array], argument=any ...
    If <-error_message is not @nil, execute it using the following
    arguments:

    	| @receiver | The <-master of the editor |
    	| @arg1     | The editor                 |
    	| @arg2     | the `kind`                 |
    	| @arg3     | The message                |

    See `visual ->report` for a discussion on the reporting mechanism.

- editor->save: file=[file]
    Save to <-file or named file.  If no file is specified and <-file is
    @nil, this method fails.  If the file to save in exists, a backup is
    created using `file ->backup`.  Next, the contents of the editor is
    saved into the named file.  If a file was specified, <-file will be
    filled with the argument file.

    @see editor-file

- editor->save_buffer: always=[int]
    Save the editor to <-file if it is <-modified or the argument is not
    @default.  If the operation succeeds, the <-modified flag is cleared.
    Diagnostics are reported using ->report.

    @see editor-file

- editor->scroll_one_line_down: [int]
    Scroll lines (1 line) downward.

- editor->scroll_one_line_up: [int]
    Scroll lines (1 line) upward.

- editor->scroll_up: [int]
    Scroll lines (1 screen) upward.

- editor->scroll_down: [int]
    Scroll forwards or backwards by the specified number of lines.  For the
    `one_line` versions of this method, the default is to scroll a single
    line.  For ->scroll_up/->scroll_down the default is a screenful minus
    a line.

- editor->scroll_to: index=[int], screenline=[int]
    Set start of screenline-th line of window to index (character
    based).  Index should be the start of a physical line, though
    this is not actively enforced.  If screenline is @default, the
    given position is centered in the window.  The first screenline
    is line 1.

    See `text_buffer <-scan`.

- editor->scroll_vertical: direction={forwards,backwards,goto}, unit={file,page,line}, amount=int
    Handle requests from the <-scroll_bar.  Normally there is no
    reason to deal with this method yourself.  We document it here
    to explain the three scrolling models used by the editor.

    If the `text_buffer<-size` (i.e.  the number of characters held
    by the editor) is large, It determines the position and size of
    the scrollbar based on character offsets.

    If the size gets `medium`, it counts physical lines, i.e. the
    number of newline characters.

    If the size gets small, it computes the line-layout, including
    line-heigth using the current <-wrap setting.  This mode
    provides relyable precise scrolling, but quickly gets too
    costly for real-time usage.

    *Inherits description from*: editor<-length

- editor->select_line: line=[int], newline=[bool]
    Select the line with given number.  Numbering lines starts at 1.

- editor->selected_fragment: fragment*
    @see class fragment

- editor->selected_fragment_style: style
    Style for the currently <-selected_fragment.

    See also ->margin_width, ->selection_style and class style.

- editor->selection: mark=[int], caret=[int], status=[{active,inactive,highlight}]
    Make characters in the range [from,to) the selection and invoke
    `Editor ->normalise` on the same range to make the selection or
    as much as possible thereof visible.  If `to` is equal to
    `from`, no selection is visible and the editor is not
    normalised.

    The selection is determined by <-mark and <-caret.  It can be in
    three modes as described with <->mark_status.

    See also <-selected, <-selection, <-selection_start and <-selection_end.

    **Defaults**:
    The first argument (`from`) defaults to the current start of the
    selection; the second (`to`) to the current end of the selection.

    @see editor->normalise

- editor->selection_extend: to=int
    Extend the selection to include the specified index.  This method is
    used when defining the selection with the pointer.  This method respects
    -selection_unit.

- editor->set_fill_column: [int]
    *Inherits description from*: editor-left_margin

    @see editor-right_margin
    @see editor-left_margin

- editor->set_mark: [int]
    Interactive command to set <-mark at <-caret, starting an active
    selection, also called region.  With an argument this command
    cycles through the list marks in the <-mark_ring.

    Programmatic access to the mark is provided by ->mark.

    @see editor-mark

- editor->show_caret: show=bool
    Controls the visibility of the caret.  In addition, if the caret
    is not visible, the cursor up/down keys scroll the editor rather
    than moving the (invisible) caret.  See also ->editable.

- editor->show_caret_at: index=[int]
    Display the caret at the indicated location without moving the
    insertion-point. The indicated position must be inside the visible range
    of the editor (see <-start and <-view). This method is used by
    ->electric_caret..

    @see editor->electric_caret

- editor->show_label: show=bool
    Show/unshow the label.  If the editor has no <-label_text, one
    is created.  Otherwise the <->displayed attribute of the
    label_text is used to control the visibility of the label.  The
    method ->geometry is invoked to realise the proper layout
    of the components of the editor object.

- editor->show_matching_bracket: index=[int]
    Indicate the bracket matching the bracket at index (default <-caret)
    using ->electric_caret.

- editor->size: size
    *Inherits description from*: editor-size

- editor->sort: from=[int], to=[int]
    *Inherits description from*: text_buffer->sort

- editor->style: fragment=name, style=style*
    *Inherits description from*: editor-styles

    @see class style

- editor->switch_case_mode: [int]
    *Inherits description from*: editor-exact_case

    @see editor-exact_case

- editor->tab_stops: vector*
    Set/query tab-stops at the indicated (pixel) positions.  This method is
    commonly used to display tabular information.

    @see editor<-tab_stops

- editor->transpose_lines
    Transpose line with line above.

- editor->transpose_terms
    Transpose Prolog terms around point.

- editor->transpose_word
    Transpose words around caret.

- editor->transpose_chars
    Transpose the unit before the caret with the one after the caret.  The
    exact definition of the unit depends on the syntax_table object
    associated to the <-text_buffer

- editor->typed: event|event_id
    Normally called from ->event to handle typing.  Handles <-focus_function
    and/or invokes `key_binding ->typed` on <-bindings.

    @see editor-bindings

- editor->undo
    Undo the last interactively given command.  Marking the undo-log is
    achieved by ->event.

- editor->upcase_region
    @see editor->downcase_region

- editor->width: int
    *Inherits description from*: editor-size

- editor->wrap: {none,character,word}
    *Inherits description from*: editor<-wrap

- editor->yank: [int]
    Insert the text from the nth (default 0) `vector <-element` of
    @text_kill_ring.  Text is inserted in the kill-ring using ->kill or
    ->grab.


## Get methods {#class-editor-get}

- editor<-column: index=[int] -> column=0..
    Determine the column for `index`.  Interprets <-tab_stops.  When set,
    the <-caret is moved as close as possible to the indicated column.  See
    also ->align.

- editor<-contains: -> visual
    Fails (hiding the internal structure of the editor for the visual
    hierarchy).

- editor<-convert: view -> editor
    Convert a view object into its `view <-editor`.  Improves the
    transparency between class editor and class view.

- editor<-dabbrev_candidates: mode={user0,user1,user2,user3}, target=char_array -> chain*
    Helper called by ->dabbrev_expand.  The mode argument describes
    the stage of the expansion:

      - user0 is called first.
                - Next, standard dynamic abbreviation is used, first looking
        towards the start of the buffer and then to the end.
      - Then, user1, user2 and user3 are tried.

    This method should return a list if candidates, each starting
    with `target`.

- editor<-height: -> character=int
    *Inherits description from*: editor-size

- editor<-indentation: index=[int], skip=[regex] -> column=int
    Column for first non-blank character of line holding index.  The default
    index is <-caret.  If regex is specified, `regex ->match` is executed
    from the start of the line and the column for the end of the match is
    returned.  If the regex does not match at all, 0 is returned.

    This method is commonly used in combination with ->align for language
    dependent automatic indentation and alignment.

- editor<-key_binding: key=name|event_id -> action=name|code
    *Inherits description from*: editor->key_binding

- editor<-start: [int] -> int

- editor<-view: -> int

- editor<-length: -> int
    The methods <-length, <-start and <-view are used by the related
    scroll_bar object to determine the visible part of the text.  The method
    ->scroll_vertical is called from the <-scroll_bar.

- editor<-line: index=[int] -> line=string
    New string with text of the line holding index.  When omitted, the line
    holding the <-caret is returned.

- editor<-line_number: index=[int] -> line=int
    1-based line-number of `index`.  If index is omitted, <-caret
    is used as default.  The line-numbers returned refers to the
    physical line number.  In other words, the returned number is
    the number of LF (Line-Feed) characters from the start of
    the buffer to `index` plus one.

    This numbering is conventionally used by programming
    languages.

- editor<-margin_width: -> pixels=int
    *Inherits description from*: editor-margin

- editor<-master: -> editor|view
    Return the view object if <-device yields an instance of class view.
    Otherwise return the editor itself.   Used when the editor executes
    a code object.   See <-error_message and <-modified_message.

    See also `visual <-master`.

- editor<-read_line: -> line=string
    As <-line, moves caret to next line.  Fails after reading the last line
    of text.  When reading the whole buffer in this fashion, don't
    forget to set ->caret: 0 before starting. See also `file <-read_line`.

- editor<-selection: -> point
    Returns a new point object whose <-x is the start of the selection and
    <-y is the end of the selection (not including: 50,51
    represents a selection holding one characters).  The <-x is
    guaranteed to be smaller than <-y.

    The selection is determined by the <-mark and <-caret.

- editor<-selection_end: -> int
    Index for end of selection.  This message is a backward
    compatible method for <-caret.  See also <-mark and
    <-selection.

- editor<-selection_start: -> int
    Index for start of selection.  This is a backward compatibility
    method for <-mark.  See also <-caret and <-selection.

    *Inherits description from*: editor-selection_end

- editor<-size: -> characters=size
    *Inherits description from*: editor-size

- editor<-tab_stops: -> vector*
    *Inherits description from*: editor->tab_stops

    @see editor->tab_stops

- editor<-width: -> characters=int
    *Inherits description from*: editor-size

- editor<-wrap: -> {none,character,word}
    How to display handle long lines.  The default is `character`,
    breaking long-lines at the character level.  If `none` is used,
    text exceeding the left-margin is simply discarded.  Finally,
    `word` makes the editor break lines at the start of a word.
    See also `text_image->wrap`.

    ->wrap chooses between the two views on a text-file realised
    by class editor.  The default mode is intended for editing
    source code or other traditional line-based text documents,
    generally using a fixed-width font object.  Using `->wrap:
    word', the editor treats the file as a sequence of paragraphs
    separated by newlines.  The latter is the common view for
    most modern GUIs for textareas.

    In addition to wrapping, a number of commands are influenced
    by <-wrap to use screen-lines rather then physical lines if
    <-wrap = word.  This is only the case if the method is *not*
    passed a numerical argument.

    - ->end_of_line, ->beginning_of_line
    - ->kill_line
    - ->cursor_up, ->cursor_down
    - ->scroll_vertical (handling the scroll_bar object).

