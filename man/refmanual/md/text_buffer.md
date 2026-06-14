# class text_buffer {#class-text_buffer}

A text_buffer forms the storage component of an editor object.  It
represents a text, possible with fragment objects defined on it.

Class text_buffer defines the primitive operations that can be done on
text.  The more user-oriented behaviour of an editor is defined with
class editor.  Among the operations are inserting text, deleting text,
scanning for units such as words, lines and paragraphs, saving and
loading, etc.

A text_buffer maintains a changes record that supports the
implementation of ->undo.

A text_buffer can simultaneously be visualised by any number of editor
objects.

@see class regex
@see class editor
@see topic Text
@see class fragment


## Instance variables {#class-text_buffer-instvars}

- text_buffer<-editors: chain
    Chain of editors associated to this text_buffer object.
    Whenever the text_buffer is <-modified, the editor objects in
    <-editors will be informed.

    This variable is managed by ->attach and ->detach, called from
    `editor ->initialise`, `editor ->unlink` and `editor	->text_buffer`.
    The methods ->attach and ->detach may be redefined to monitor
    attachments/detachments of editors.  The redefinition must use
    ->send_super and may not fail.

- text_buffer<-first_fragment: fragment*
    First/last fragment in the double-linked list of fragments.  Fragments
    in this list are kept ordered on `fragment <-start`.  See also
    `fragment <-next` and `fragment <-previous`.

- text_buffer<-generation: 0..
    Integer that is incremented each time the text_buffer object has
    been changed.   It is used by PceEmacs colourisation support
    to check whether the text-buffer has changed since the last
    time it was coloured.  We cannot use the <->modified flag
    for that purpose as this reflects whether the content is
    different from the file or not.

    Please note that this counter is only incremented; ->undo
    actions increment it too.

- text_buffer<-last_fragment: fragment*
    *Inherits description from*: text_buffer-first_fragment

- text_buffer<-modified: bool
    Indicate that the text_buffer has been modified.  When the contents of
    this variable has changed, each associated editor will forwards its
    `editor <-modified_message` with the following arguments:

	| @receiver | `editor <-master`        |
	| @arg1     | New value of <-modified. |

    See also <-generation.

    @see editor-modified_message
    @see editor-editable

- text_buffer-size: alien:int
    Number of characters in the text_buffer.  A character is defined as a
    Unicode code point. The method <-length is available to avoid a
    conflict with `editor <->size`.

- text_buffer<->syntax: syntax_table
    Description of the syntax.  This table describes character categories,
    brackets, quotes, comment, sentences and paragraphs.  See class
    syntax_table and notably `syntax_table->syntax`  for details.

    Initially this slot is filled with the built-in syntax_table object
    named `default`.

    The text_buffer's <-syntax table is used by many methods on
    class text_buffer and class editor to recognise words, matching
    braces, comment, etc.  Syntax tables are used extensively by
    PceEmacs.

    See also <-scan, ->in_comment, <-skip_comment, and
    ->for_all_comments

- text_buffer<-undo_buffer_size: bytes=int
    Size of the undo-buffer in characters.  Larger values allow for more
    undo at the cost of more memory.  The value 0 disables recording
    of undo information.


## Send methods {#class-text_buffer-send}

- text_buffer->append: text=char_array, times=[int]
    Append the indicated text n times at the end of the buffer.  See also
    ->insert and ->format.

- text_buffer->attach: editor
    *Inherits description from*: text_buffer-editors

- text_buffer->capitalise: from=int, size=int
    Change the case in the specified region.  Each start of a word
    is changed to uppercase, the remainder of the word to lowercase.

- text_buffer->check_point_undo
    Create a `no-change` checkpoint in the undo buffer.  If ->undo reverts
    back to this point, <-modified will be reset to @off.

- text_buffer->contents: char_array
    Replace the <-contents of the text_buffer object.  In addition,
    it resets the ->undo system and deletes all associated
    fragment objects.

    See also ->insert_file.

- text_buffer->delete: at=int, characters=[int]
    Delete characters forwards from the given (0-based) index. The second
    argument is the number of characters to be deleted.

    **Defaults**:

    By default, 1 character is deleted.

- text_buffer->detach: editor
    *Inherits description from*: text_buffer-editors

- text_buffer->for_all_fragments: code
    Iterate code over all fragments.  The code object is allowed to
    create and/or destroy fragments associated with this text_buffer
    object.  See also <-find_fragment and <-find_all_fragments.
    Arguments:

	@arg1	The fragment object

- text_buffer->format: format=char_array, argument=any ...
    ->append formatted text.  See `string ->format` for the
    definition of the format string and its arguments.

- text_buffer->in_string: index=int, start=[int]
    Test if first index is in string constant.

- text_buffer->in_comment: index=int, start=[int]
    Succeed if the indicated index is in a comment/string statement of the
    target language.  By default it starts scanning from the start of the
    buffer.  If `start` is specified it will start scanning from this index,
    assuming that the given index is not in a string or comment.  See
    `syntax_table ->syntax` for defining the comment syntax.

- text_buffer->initialise: contents=[char_array]
    Create a text_buffer object.  The argument defines the initial contents.

- text_buffer->insert_file: at=int, data=source_sink, times=[int]
    Insert the given file at the indicated position.  _Times_ is the number
    of times the file is inserted and defaults to 1.  This method
    sets the `file<->newline_mode` if it is set to detect at entry
    to simplify Windows/Unix interaction.  This is exploited by
    PceEmacs.

    See also ->save, ->contents and class editor.

- text_buffer->iso_latin_1
    Succeed if the text_buffer object uses 8-bit ISO Latin-1
    encoding or after successful conversion from wide-character
    encoding to ISO Latin-1.  It can be used to see whether the
    memory footprint can be lowered or to test whether it is
    possible to save the text using ISO Latin-1 encoding.

    See also `file->encoding`.

- text_buffer->mark_undo
    Set a mark on the undo buffer if there is no mark at the `head` of the
    undo buffer.  A subsequent undo will ->undo all changes back to this
    mark.  Normally set by `editor ->event`.

- text_buffer->report: kind={status,inform,progress,done,warning,error,fatal}, format=[char_array], argument=any ...
    Report to all associated <-editors.  See `editor ->report` and `visual
    ->report'.

- text_buffer->reset_undo
    Clear the undo-buffer.  Actually also deallocates the buffer.  On the
    first modification a new buffer will be created.

- text_buffer->save: in=file, from=[int], size=[int]
    Save (part-of) the text_buffer's content in the given file.  The default
    `from` is 0 and the `size` will save to the end of the buffer.  If both
    `from` and `size` are omitted, <-modified will be set to @off.   See also
    `editor ->save` and `editor ->save_buffer`.

- text_buffer->sort: from=[int], to=[int]
    Sort the lines between the indicated character indices alphabetically.

    **Bugs**:
    Not very flexible and doubtfful, whether this is the right level for
    implementing this method.

- text_buffer->undo
    Undo all changes to the text_buffer back to the last ->undo_mark.  If
    this is a ->reset_undo checkpoint, reset ->modified to @off.  Changes
    are stored in a buffer of size <-undo_buffer_size.  Changes that exhaust
    the size of this buffer are lost.

- text_buffer->unlink
    Send `editor ->lost_text_buffer` to all related <-editors and
    returns all alien resources to the system.


## Get methods {#class-text_buffer-get}

- text_buffer<-sub: from=[int], to=[int] -> string

- text_buffer<-contents: from=[int], size=[int] -> string
    New string holding the text in the specified interval.  <-contents and
    <-sub only differ in the interface.  <-sub is compatible to `string <-sub`
    and the preferred interface.

- text_buffer<-convert: editor -> text_buffer
    Translate an editor in its corresponding text_buffer.  Notably useful
    when dealing with regular expressions (class regex) and class fragment
    which both define various methods that accept a text_buffer argument.

- text_buffer<-find: from=int, for=string, times=[int], return=[{start,end}], exact_case=[bool], word=[bool] -> index=int
    Search for a plain string.  The arguments are in the table below.
    Values between parenthesis are the defaults.

	| from:            | Start the search from this index                |
	| for:             | Search for this string                          |
	| times (1):       | Return the nth occurrence                       |
	| return (start):  | `start`: first character, `end`: last character |
	| exactcase (@on): | @off: case does not matter                      |
	| wordmode (@off): | @on: find whole words only                      |

    Fails silently if the requested string cannot be found. See also class regex.

- text_buffer<-find_all_fragments: test=[code] -> matching=chain
    Return a new chain holding all associated fragments for which code could
    be executed successfully.  @arg1 refers to the current fragment.  See
    also `chain <-find_all`.

    This method is commonly used to find fragments with a certain attribute
    overlapping the caret position:

	...
	get(TextBuffer, find_all_fragments,
	    and(message(@arg1, overlap, Caret),
			@arg1?style == active),
		Fragments),
	...

    See also ->for_all_fragments, <-find_fragment.

    @see class fragment

- text_buffer<-find_fragment: test=code -> fragment
    Find first fragment object in the text_buffer object that
    accepts code.  See also <-find_all_fragments,
    ->for_all_fragments.

- text_buffer<-length: -> int
    Number of characters in the text_buffer.  A character is defined as a
    Unicode code point.  Reads the value of `text_buffer-size`.

- text_buffer<-matching_bracket: from=int, bracket=[char] -> index=int
    Find index of a matching bracket.  _From_ is the index on which to start
    the scan.  Assume `from` holds a <-character `bracket`.  If no bracket
    is specified, the <-character at from is used.   Specifying the bracket
    is useful to find the start of the bracketed environment point is in.

	start_of_compound_statement(TB, Here, Start) :-
		get(TB, matching_bracket, Here, '}', Start).

    If this method traps -while scanning-, a bracket mismatch (e.g. (hello})
    it will generate a mismatched_bracket error.  See class error and the
    predicate pce_catch_error/2.

    See class syntax_table for defining brackets.

    This method deals with string and comment.  See ->in_string,
    ->in_comment and <-scan.

- text_buffer<-matching_quote: from=int, direction={forward,backward} -> index=int
    Find quote matching the quote at `from`, scanning in the indicated
    direction.  Class syntax_table defines quotes and escape sequences for
    quotes.

- text_buffer<-scan: from=int, unit={character,word,line,sentence,paragraph,term}, times=[int], return=[{start,end}] -> index=int
    Scan over a number of textual units.  This function is used by class
    editor to determine the range of operations like `editor ->forward_word`,
    `editor ->transpose_terms`, etc.   Arguments:

	| from   | Index to start scanning                 |
	| unit   | Unit to scan for                        |
	| times  | Number of units to skip (<0: backwards) |
	| return | start: start of unit, end: end of unit  |

    For example,

	get(TextBuffer, scan, 200, word, 2, start, Index)

    will scan two words from index 200 and return the start of the 2nd word.

    The definitions of the units is determined by the associated <-syntax.

    See also <-matching_bracket.

- text_buffer<-scan_syntax: from=[int], to=[int] -> tuple
    Find syntactical state of position using the defined string and
    comment syntax from the associated <-syntax object.  Scanning
    starts at `from`, which is assumed to be outside comment and
    strings.  The return-value is a tuple object, whose first part
    is a name indicating the syntactical category at `to` and whose
    second part denotes the start-position of this syntactical
    object.

    Returned syntactical categories:

    - code
	_To_ is outside strings or comment

    - comment
	_To_ is inside a comment

    - string
	_To_ is inside a quoted string.

    See also class syntax_table, <-skip_comment, <-skip_layout,
    <-matching_bracket.

- text_buffer<-size: -> int
    *Inherits description from*: text_buffer-size

- text_buffer<-skip_comment: from=int, to=[int], skip_layout=[bool] -> index=int
    Assume `from` points to a layout character or the start of a comment.
    Return the first location not in a comment and not whitespace.  If
    `skip_layout` is @off, just comment will be skipped.

    See also ->in_comment and <-skip_layout and class syntax_table.

- text_buffer<-skip_layout: from=int, direction=[{forward,backward}], skip_newline=[bool] -> index=int
    Scan the contents of the text_buffer object in the indicating
    direction, returning the first non-blank character.  If skip_newline
    is @off, newlines will not be skipped.

    See also <-skip_comment, <-scan, <-find, `regex->search`.
