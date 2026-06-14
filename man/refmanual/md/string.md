# class string {#class-string}

A string object is a sequence of ASCII characters.  Strings do not pose
any limits on their size.  They can contain any ASCII value, which the
exception of the EOS character (_\0_).

PCE various ways to represent text.  See class char_array, class
name and class text_buffer.

@see class text
@see topic Text


## Send methods {#class-string-send}

- string->character: at=int, char=char
    Change character at 0-based index. See also ->insert_character.

- string->delete: from=int, length=[int]
    Delete `length` characters starting at `from`.  If `length` is omitted it
    deletes all characters starting at `from` to the end of the string.

- string->ensure_nl: [char_array]
    If the last character of the string is not a newline (ASCII 10) a
    newline is appended.  If additional text is provides this is appended
    too.

- string->ensure_suffix: text=char_array, ignore_case=[bool]
    If string does not have ->suffix, ->append the suffix.  Notably useful
    for handling file suffixes.

- string->format: format=[char_array], argument=any ...
    Replace the <-value of the string object using formatted output.
    Formatting is based on the C-language printf() standard library
    function.  Argument objects are converted to the type requested
    by the format statement.

    If you have an implementation of the C-language on your
    computer, you may wish to check the documentation of
    printf (Unix: man printf).

    Various other classes implement the ->format method.  See also
    `editor ->format`, `text_buffer->format`, `pce->format`.

    Prolog formatting may be used as an alternative by exploiting
    pce_open/3:

    	...,
    	pce_open(View, append, Stream),
    	format(Stream, 'Hello ~d-th ~w~n', [100, world]),
    	close(Stream),
    	...

    The `format` argument to ->format is a string with two types of
    control-arguments.  The % sign introduces a conversion
    statement, while the \ character introduces a special character.
    The % sign is optionally followed by a numeric argument,
    controlling the output.

- string->initialise: format=[char_array], argument=any ...
    Create a string object from a format specification and
    formatting arguments.  See also ->format.  To create a string
    holding a literal copy of another char_array object, enforce
    type-conversion, or use:

    	new(S, string('%s', Original))

- string->insert: at=[int], text=char_array
    Insert text at the given 0-based index.  If index is @default this is
    equivalent to ->append.  If index is 0 this is equivalent to ->prepend.

- string->insert_character: char=char, at=[0..], times=[0..]
    Insert the given character `times` times at the given location.
    If `at` is @default, the character(s) is/are appended at the
    end of the string.

    If a string is to be created from a list of character codes, it
    is advised to create a string of the necessary length first
    using this method.  The following illustrates this making a
    string with all characters between 0 and 255:

    	new(S, string),
    	send(S, insert_character, 0, 0, 256),
    	forall(between(0, 255, I),
    		   send(S, character, I, I)).

    See also `string ->character`.

- string->newline: times=[0..]
    Append `times` newlines (default 1).  This is equivalent to:

    	send(String, append, string('\n')).

- string->strip: [{leading,trailing}]
    Strip all layout characters (see `pce <-syntax`) from the start and/or
    and of the string.  When the argument is `leading`, only the layout
    characters at the start are removed.  When `trailing`, the layout
    characters at the end are removed.  Otherwise (@default) they are
    removed from both ends.

    This method is regarded obsolete.  New code should use
    `char_array <-strip`.

    @see class regex

- string->translate: from=char, into=char*
    Map all occurrences of `from` into `to`.  If `to` is @nil, delete all
    `from` characters.  The following translates a string holding a
    list of words line-by-line into a string where the words are
    separated by spaces:

    	send(String, translate, '\n', ' ').

- string->truncate: int
    Truncate the string to the specified number of characters.  The
    following two calls are equivalent:

    	send(String, truncate, 4)
    	send(String, delete, 4)

- string->untabify: tabs=[int|vector]
    Replaces all tab characters (ASCII 9) by spaces, preserving the layout.
    The argument specifies the positions of the tab-stops.  If this is an
    integer it specifies infinitely tab-stops at the specified intervals.
    If it is a vector it should be a vector of integers.  If `tabs` is
    omitted _8_ is used as a default value.

    The main application area is handling of plain (Unix) ASCII text.


## Get methods {#class-string-get}

- string<-copy: -> string
    Returns a new instance of the same class with the same text.  Equivalent
    to:

    	get(S?class, instance, '%s', S).

    See also `char_array <-copy`.

- string<-modify: char_array -> string
    Create a new string from the argument text.  Defined as:

    	modify(S, To:char_array, Result:string) :<-
    		get(S, class, Class),
    		get(Class, instance, '%s', To, Result).

    See `char_array <-modify` for details.

