# class char_array {#class-char_array}

Class char_array is the superclass of both class name (for unique
textual constants) and class string for editable text.  Class char_array
defines common behaviour and avoids type-conversions if the receiving
method is just interested in the text represented.  It is not common for the
user to create instances of the class directly.

See also class text_buffer for handling large amounts of text, and class
regex for searching in text.

@see class regex


## Instance variables {#class-char_array-instvars}

- char_array-text: alien:wint_t *
    C char * pointer to a '\0' terminated array of characters.


## Send methods {#class-char_array-send}

- char_array->equal: text=char_array, ignore_case=[bool]
    Succeeds if both char_arrays represent the same text.  If
    ignore_case is @on the comparison is executed case-insensitive.

    See also ->prefix and ->suffix.

- char_array->initialise: text=char_array
    Create from the argument text.   Internally this loop is broken by creating
    char_array objects from host-language textual constants:

    	?- new(X, char_array(hello)).

    Creates a char_array object from a Prolog atom.

- char_array->larger: than=char_array
    @see chain->sort

- char_array->prefix: text=char_array, ignore_case=[bool]
    Test if receiver starts with the given argument string.  See
    also ->suffix,  ->equal and class regex.

- char_array->smaller: than=char_array
    @see chain->sort

- char_array->sub: text=char_array, ignore_case=[bool]
    Test if argument is a substring, If the boolean is @off, the test is
    ignores possible case differences.  See also `regex ->search`.

- char_array->suffix: text=char_array, ignore_case=[bool]
    Test if receiver ends with the given string.  See also ->prefix,
    `<-ensure_suffix` and <-delete_suffix.


## Get methods {#class-char_array-get}

- char_array<-append: char_array ... -> char_array
    New char array representing the concatenation of the receiver and the
    argument.

- char_array<-base64_encode: -> char_array

- char_array<-base64_decode: -> char_array
    The methods <-base64_decode and <-base64_encode realise base64
    encoding and decoding conform rfc-2045.  This encoding
    technique is commonly used for transferring 8-bit data over
    7-bit or otherwise restricted channals.

    Encoded data contains the characters 0-9, a-z, A-Z, + and / and
    end in 0, 1 or 2 _=_ characters.  The length of an encode string
    is always a multiple of 4.  <-base64_decode fails silently of
    the input could not be translated due to a format error.

    Base64 encoding is used in transferring E-mail, in the HTTP
    protocol and many others.  See also `date <-rfc_string` and
    the class httpd from the library(http/httpd).

- char_array<-capitalise: -> char_array
    Capitalised version of name.  The first character is mapped to
    its uppercase equivalent.  All other letters are mapped to
    their lowercase equivalent.  The letter after a word_separator
    character (default ('_')) is mapped to uppercase, while the
    separator is deleted.  Examples:

    	name			'Name'
    	label_name		'LabelName'
    	'label name'	'Label name'

    See also <-label_name.

- char_array<-character: int -> char
    Returns the ASCII value of the nth (0-based) character of the string.
    Fails silently if the index is out of range.

- char_array<-compare: text=char_array, ignore_case=[bool] -> {smaller,equal,larger}
    Alphabetically compares the receiver with the argument and returns:

    - smaller
    	if the receiver is alphabetically before the argument

    - equal
    	If both represented texts are identical

    - larger
    	if the receiver is alphabetically after the argument

    Intended to be used together with `chain ->sort` and `vector ->sort`.

    @see chain->sort

- char_array<-convert: any -> char_array
    Converts the following:

    	| subclasses of char_array | The text                            |
    	| int, number              | Decimal representation of the value |
    	| real                     | _%f_ representation of the value    |

- char_array<-copy: -> char_array
    Returns an instance of the same class holding the same text.
    Equivalent to:

    	get(CA?class, instance, CA).

    A subclass for which this behaviour does not produce the desired result
    should redefine this method.  See for example `string <-copy`.

- char_array<-delete_prefix: prefix=char_array -> char_array
    If the receiver starts with the argument (see also ->prefix),
    return a new char_array object holding the remaining text.
    See also <-delete_suffix and ->prefix.

- char_array<-delete_suffix: suffix=char_array -> char_array
    If the receiving char_array ends with suffix, a copy is returned with
    this suffix deleted.  Otherwise this call fails.  See also ->suffix,
    <-delete_prefix, <-ensure_suffix and `string ->ensure_suffix`.
    Example:

    	?- get('hello.pl', delete_suffix, '.pl', V).

    	V = hello

- char_array<-downcase: -> char_array
    Map all uppercase letters to lowercase

- char_array<-ensure_suffix: suffix=char_array -> char_array
    If the receiving char_array object already has suffix, just
    return it, otherwise return a copy with the suffix <-append'ed.
    Examples:

    	?- get(hello, ensure_suffix, '.pl', X).
    	?- get('hello.pl', ensure_suffix, '.pl', X).

    both yield `hello.pl`.

- char_array<-rindex: of=char, from=[int] -> int

- char_array<-index: of=char, from=[int] -> int
    Return the index of the given character.  When `from` is given, the
    search (forwards for <-index, backwards for <-rindex) starts at the
    given index.  Fails silently if the character cannot be located.

- char_array<-label_name: -> char_array
    This method is used by `menu_item <-default_label` to compute a nice
    looking label from a nice looking identifier in the host language.  It
    performs the following transformations:

    1. Any word-separator character (default '_', for LISP often
    	 '-' (see `pce ->syntax`) is transformed into a space.

    2. The first character is capitalised.

    This method is called as an explicit method on class name by all
    built-in dialog-related classes to translate identifier names
    into labels.  Below is a skeleton for dealing with labels in
    international applications.

    	:- pce_extend_class(name).

    	label_name(Name, Label:name) :<-
    		(   translate(Name, Label)
    		->  true
    		;   get_class(Name, char_array,
    			label_name, Label)
    		).

    	:- pce_end_class.

    @see menu_item<-default_label

- char_array<-modify: char_array -> char_array
    Called internally by the various get methods that return a modified copy
    of the char_array (<-append, etc.).   The version defined for class
    char_array is defined as:

    	modify(CA, To:char_array, Value:char_array) :<-
    		get(CA, class, Class),
    		get(Class, instance, To, Value).

    Redefinition may be necessary for subclasses of class char_array.
    Examples are the PCE/Lisp predefined class lisp_symbol.

- char_array<-read_as_file: from=int, size=int -> char_array
    Allow read data from object using pce_open/3.  This allows for
    opening atoms and XPCE string objects as input-stream:

    	open('hello(world).\n', read, Stream),
    	read(Stream, X),
    	close(Stream)

    --> X = hello(World)

- char_array<-scan: format=char_array -> vector
    Parse text according to format.  Converted values are returned in the
    result vector object.  This method provides an interface to the
    C-library function sscanf(3).  For details consult your C-library
    documentation.  On most Unix systems this is done using the command
    `man sscanf`.

    Below is a description of the format string as taken from the SunOs
    manual.

    The control string usually contains conversion specifications, which are
    used to direct interpretation of input sequences.  The control string
    may contain:

    - White-space characters (SPACE, TAB, or NEWLINE) which, except
    	in two cases described below, cause input to be read up to the
    	next non-white-space character.

    - An ordinary character (not _%_), which must match the next
    	character of the input stream.

    - Conversion specifications, consisting of the character _%_ or
    	the character sequence %digit$, an optional assignment
    	suppressing character _*_, an optional numerical maximum field
    	width, an optional l (ell) or h indicating the size of the
    	receiving variable, and a conversion code.

    Conversion specifications are introduced by the character % or the
    character sequence %digit$.  A conversion specification directs the
    conversion of the next input field; the result is placed in the variable
    pointed to by the corresponding argument, unless assignment suppression
    was indicated by _*_.  The suppression of assignment provides a way of
    describing an input field which is to be skipped.  An input field is
    defined as a string of non-space characters; it extends to the next
    inappropriate character or until the field width, if specified, is
    exhausted.  For all descriptors except `_[_' and ``c`', white space
    leading an input field is ignored.

    The conversion character indicates the interpretation of the input
    field; the corresponding pointer argument must usually be of a
    restricted type.  For a suppressed field, no pointer argument is given.
    The following conversion characters are legal:

    	# %
    	A single % is expected in the input at this point; no assignment
    	is done.

    The various integer formats (the result is always returned as a PCE
    int).

    - d
    	A decimal integer is expected; the corresponding argument should
    	be an integer pointer.

    - u
    	An unsigned decimal integer is expected; the corresponding
    	argument should be an unsigned integer pointer.

    - o
    	An octal integer is expected; the corresponding argument should
    	be an integer pointer.

    - x
    	A hexadecimal integer is expected; the corresponding argument
    	should be an integer pointer.

    - i
    	An integer is expected; the corresponding argument should be an
    	integer pointer.  It will store the value of the next input item
    	interpreted according to C conventions: a leading `_0_' implies
    	octal; a leading `_0x_' implies hexadecimal; otherwise, decimal.

    - n
    	Stores in an integer argument the total number of characters
    	(including white space) that have been scanned so far since the
    	function call.  No input is consumed.

    The various floating point formats.  The result is passed as an instance
    of class real.

    - e,f,g
    	A floating point number is expected; the next field is converted
    	accordingly and stored through the corresponding argument, which
    	should be a pointer to a float.  The input format for floating
    	point numbers is as described for string_to_decimal(3), with
    	fortran_conventions zero.

    A character.  The result is returned as the ASCII (PCE int).

    - c
    	A character is expected; the corresponding argument should be a
    	character pointer.  The normal skip over white space is
    	suppressed in this case; to read the next non-space character,
    	use %1s.  If a field width is given, the corresponding argument
    	should refer to a character array, and the indicated number of
    	characters is read.

    The string formats.  The result is a PCE string object.

    - s
    	A character string is expected; the corresponding argument should
    	be a character pointer pointing to an array of characters large
    	enough to accept the string and a terminating \0, which will be
    	added automatically.  The input field is terminated by a white
    	space character.

    	# [
    	Indicates string data; the normal skip over leading white space is
    	suppressed.  The left bracket is followed by a set of
    	characters, which we will call the scanset, and a right bracket;
    	the input field is the maximal sequence of input characters
    	consisting entirely of characters in the scanset.  The
    	circumflex (^), when it appears as the first character in the
    	scanset, serves as a complement operator and redefines the
    	scanset as the set of all characters not contained in the
    	remainder of the scanset string.  There are some conventions
    	used in the construction of the scanset.  A range of characters
    	may be represented by the construct first-last, thus
    	[0123456789] may be expressed [0-9].  Using this convention,
    	first must be lexically less than or equal to last, or else
    	the dash will stand for itself.  The dash will also stand for
    	itself whenever it is the first or the last character in the
    	scanset.  To include the right square bracket as an element of
    	the scanset, it must appear as the first character (possibly
    	pre- ceded by a circumflex) of the scanset, and in this case it
    	will not be syntactically interpreted as the closing bracket.
    	The corresponding argument must point to a character array large
    	enough to hold the data field and the terminating \0, which will
    	be added automatically.  At least one character must match for
    	this conversion to be considered successful.

    See also class regex.

- char_array<-split: separator=[char_array] -> chain
    Implementation of the popular split-string operatoon.  If
    `separator` is given, it is used to separate parts of the
    text.  If omitted, a non-empty sequence of layout characters
    is used.  Examples:

    	?- get_object('foo+bar', split, +, X).
    	X = chain(foo, bar).

    	?- get_object('foo bar', split, X).
    	X = chain(foo, bar).

    Please note that -like the other get-methods on class
    char_array- the returned fragments are of the same class as the
    receiver.  In other words, breaking a name object yields a chain
    of names and breaking a string object yields a chain of string
    objects.

    See also `regex->for_all`, which may be used to realise more
    complicated breaking of text.

- char_array<-strip: [{canonicalise,leading,trailing,both}] -> char_array
    Canonicalise leading, trailing and internal blanks.  Depending
    on the argument, the behaviour is defined as:

    - canonicalise (default)
    	Delete all blanks from the start and end of the text and
    	map all sequences of internal layout characters to a
    	single space character.

    - leading
    	Only remove leading layout characters

    - trailing
    	Only remove trailing layout characters

    - both
    	Remove both leading and trailing layout characters.
    	This is the same as the obsolete method `string->strip`.

    `Char_array<-strip` is commonly used together with class
    text_item to canonicalise identifiers entered by the user.  See
    also <-upcase, <-downcase and `string->translate`.

- char_array<-sub: start=int, end=[int] -> char_array
    Return a copy containing the characters [start ...  end).  If end is
    omitted, the end of the char_array is used.  Both `start` end
    `end` are 0-based indices.

- char_array<-value: -> name
    Return the text of the char_array as a name object.  On most
    host-languages this name will be mapped on the language's
    identifier type (atom for Prolog, keyword for Lisp):

    	?- get(string("hello"), value, V).

    	   V = hello

