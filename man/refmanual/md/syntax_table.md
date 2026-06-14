# class syntax_table {#class-syntax_table}

Class syntax_table describes syntactical properties of (programming)
languages.  It is used by class editor and class text_buffer to
realise syntax driven editors.  Class tokeniser used syntax tables to
determine tokens and skip comment.

Syntax-tables are named reusable objects (like -for example- colours or
fonts).  The default syntax table is named `default`.

The code below defines a syntax table for PCE/Prolog programs:

	?- new(X, syntax_table(prolog)),
	   send(X, syntax, '"',  string_quote, '"'),
	   send(X, syntax, '''', string_quote, ''''),
	   send(X, syntax, @,    symbol),

	   send(X, syntax,     '%',  comment_start),
	   send(X, add_syntax, '\n', comment_end),
	   send(X, add_syntax, '/',  comment_start, '*'),
	   send(X, add_syntax, '*',  comment_end, '/').

After this definition, the following call associates this syntax table
to an editor (view):

	?- new(X, view),
	   send(X, syntax, prolog).


## Instance variables {#class-syntax_table-instvars}

- syntax_table<-name: name*
    Name of the syntax_table.  Each table has a unique name.  The global
    hash_table object @syntax_tables maps table names into tables.  See
    also <-lookup and <-convert.

- syntax_table<->paragraph_end: regex
    Regular expression (regex object) that describes the end of a
    paragraph.  This regex is actiavted using `regex->match` for
    each start of  a physical line and should succeed if the lines
    separates two paragraphs.  The default is '\s *$', implying a
    line containing only layout characters.

    See also ->sentence_end. and `text_buffer <-scan`.

    **Defaults**: The default regex describes an empty line: regex('\s *$').

    @see syntax_table-sentence_end

- syntax_table<->sentence_end: regex
    Regular expression (regex object) that describes the end of a sentence.
    Used by `text_buffer <-scan`, `editor ->forward_sentence` `editor
    ->backward_sentence' and `editor ->fill`.

    @see syntax_table-paragraph_end

- syntax_table<-size: int
    Size of the table.  Currently XPCE uses 8-bit characters and this value
    is thus fixed to 256.

- syntax_table-table: alien:ushort *
    Vector of bitmasks.  Used by ->syntax, ->has_syntax, etc.  See also
    <-context.


## Send methods {#class-syntax_table-send}

- syntax_table->add_syntax: character=char, category=syntax_name, context=[char]
    Performs a bitwise `or` on the table entry, placing the character in
    multiple categories.  If a context is specified it replaces the defined
    context.  See also ->syntax.

    **Bugs**:
    A character may be in any number of syntactical categories but only has
    one context character defined.

    @see syntax_table->syntax

- syntax_table->context: character=char, context=char
    Set the context character for the specified (1st) character.  Normally
    the context is specified along with the syntax category using ->syntax
    or ->add_syntax.

- syntax_table->copy: syntax_table
    Copy the contents of the argument syntax table.  See also ->initialise.

- syntax_table->has_syntax: character=char, category=syntax_name
    Test that character has syntax `syntax_name`.

- syntax_table->initialise: name=[name]*, prototype=[syntax_table]
    Create a named or anonymous (name = @nil) syntax table.  Named syntax
    tables are registered in @syntax_tables for later reuse.  If a
    syntax_table is specified its contents is copied in the new table.

    Anonymous tables are not stored in @syntax_tables.

- syntax_table->syntax: character=char, category=syntax_name, context=[char]
    Modify entry for specified character.  See `type <-kind` for a
    description on type char.  Type syntax_name is a `name_of` type that
    allows for the following syntactical categories.

    	Category            Context Default table
    	=========================================
    	uppercase_letter	--	    A-Z
    	lowercase_letter	--	    a-z
    	digit				--		0-9
    	word_separator		--		_
    	symbol				--
    	open_bracket		close	([{
    	close_bracket		open	}])
    	end_of_line			--		\C-j
    	white_space			--		\C-a - \C-i, \C-k - SPC, DEL - M-DEL
    	string_quote		escape	"'
    	punctuation			--		!#$%&*+,-./:;<=>?\^|~
    	end_of_string		--		\C-@
    	comment_start		end
    	comment_end			start
    	letter				--		uppercase_letter|lowercase_letter
    	word				--		letter|digit|word_separator|symbol
    	layout				--		end_of_line|white_space

    Notes:

    - string_quote
    	Context is the character to escape the quote in a string.  For
    	most languages this is '\' (backslash), for Prolog this is the
    	quote itself.

    - comment_start, comment_end
    	If there is no context this specifies a single-character
    	comment.  Otherwise the the context is the second character of
    	the comment start/end sequence.  More than 2 character comment
    	delimiters are not supported.  See also `text_buffer ->for_all_comments`.

    ->syntax makes the table forget anything defined for this character.
    ->add_syntax may be used to specify a character into multiple
    syntactical categories.

    See also `text_buffer ->syntax` and class editor.

    @see syntax_table->add_syntax


## Get methods {#class-syntax_table-get}

- syntax_table<-comment_end: [1..2] -> name
    Return 1 or 2 character long comment end sequence as a name.  With the
    argument omitted, the 1 character long comment end sequence is returned.

    **Diagnostics**: Fails silently if no such comment sequence is defined.

- syntax_table<-comment_start: [1..2] -> name
    Return 1 or 2 character long comment start sequence as a name.  With
    the argument omitted, the 1 character long comment start sequence is
    returned.

    **Diagnostics**: Fails silently if no such comment sequence is defined.

- syntax_table<-context: character=char -> context=char
    Some syntactical categories have a context character associated
    (see ->syntax).  The context characters are stored in this
    C-array.

- syntax_table<-convert: name -> syntax_table
    First performs a lookup in @syntax_tables.  When successful, this table
    is returned.  Otherwise it returns a new (default) table.  See also
    ->initialise.

- syntax_table<-lookup: name -> syntax_table
    Performs a lookup in @syntax_tables.

- syntax_table<-syntax: character=char -> category=name|chain
    Return the syntactical category of the character.  If the character is
    in multiple categories a chain with category names is returned.    See
    also <-context.

