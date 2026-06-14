# class regex {#class-regex}

Class regex implements `regular-expression` handling.  A regular
expression is a specification of a string with various wildcard
mechanisms.  It is compiled to a finite state machine which is
executed to make the match.  XPCE's regular expression use Henry
Spencer's regex library, modified by the Tcl project to handle
UNICODE and modified by us to deal with implementation
details of class text_buffer and class char_array.  Regular
expressions can be used to search in one of the following
objects:

- class text_buffer (underlying class editor)
- class char_array (class string and class name)
- class fragment (sub-search in a text_buffer object)

The match is controlled by the following variables:

- <-ignore_case
	If @on, the match is done ignoring case.  The regex
	library uses the UNICODE locale for case matching
	Default is to match case-senstive.

- <-syntax
	One of basic, extended or advanced.  The default is
	advanced.  See ->pattern for details.

- <-pattern
	Represents the actual pattern.  The syntax of regular
	expressions is described with <->pattern.


## Example {#class-regex-example}

The following code searched for method references in the PCE
documentation style.

	find_method_ref(TextBuffer, Access, Method) :-
		new(R, regex('(->|<-|<->)(\w+)')),
		send(R, search, TextBuffer),
		get(R, register_value,
		    1, TextBuffer, name, Arrow),
		get(R, register_value,
		    2, TextBuffer, name, Method),
		map_arrow(Arrow, Access).

	map_arrow(->, send).
	map_arrow(<-, get).
	map_arrow(<->, both).


## Reusability note {#class-regex-reusability-note}

A regex object holds information about the last match.  This information
will be overwritten on a new ->match or ->search.

@see string->strip
@see class text_buffer
@see class char_array


## Instance variables {#class-regex-instvars}

- regex-compiled: alien:regex_t*
    Internal representation (library format) of the compiled expression.

- regex<-pattern: char_array
    NOTE: This material is copied from the Tcl manual (man
    re_syntax).

    A regular expression describes strings of characters.  It's a
    pattern that matches certain strings and doesn't match others.

    DIFFERENT FLAVORS OF REs

    Regular expressions (‘‘RE''s), as defined by POSIX, come in two
    flavors: extended REs (‘‘EREs'') and basic REs (‘‘BREs'').  EREs
    are roughly those of the traditional egrep, while BREs are
    roughly those of the traditional ed.  This implementation adds a
    third flavor, advanced REs (‘‘AREs''), basically EREs with some
    significant extensions.

    This manual page primarily describes AREs.  BREs mostly exist
    for backward compatibility in some old programs; they will be
    discussed at the end.  POSIX EREs are almost an exact subset of
    AREs.  Features of AREs that are not present in EREs will be
    indicated.

    REGULAR EXPRESSION SYNTAX

    Tcl regular expressions are implemented using the package
    written by Henry Spencer, based on the 1003.2 spec and some (not
    quite all) of the Perl5 extensions (thanks, Henry!).  Much of
    the description of regular expressions below is copied
    verbatim from his manual entry.

    An ARE is one or more branches, separated by ‘|', matching
    anything that matches any of the branches.

    A branch is zero or more constraints or quantified atoms,
    concatenated.  It matches a match for the first, followed by a
    match for the second, etc; an empty branch matches the empty
    string.

    A quantified atom is an atom possibly followed by a single
    quantifier.  Without a quantifier, it matches a match for the
    atom.  The quantifiers, and what a so-quantified atom matches,
    are:

             *     a sequence of 0 or more matches of the atom

             +     a sequence of 1 or more matches of the atom

             ?     a sequence of 0 or 1 matches of the atom

             {m}   a sequence of exactly m matches of the atom

             {m,}  a sequence of m or more matches of the atom

             {m,n} a  sequence  of  m through n (inclusive) matches of the atom; m may
                   not exceed n

             *?  +?  ??  {m}?  {m,}?  {m,n}?
                   non-greedy quantifiers, which match  the  same  possibilities,  but
                   prefer  the  smallest  number  rather  than  the  largest number of
                   matches (see MATCHING)

    The forms using { a nd } are known as bounds.  The numbers m and
    n are unsigned decimal integers with permissible values from 0
    to 255 inclusive.
    An atom is one of:

    (re)  (where re is any regular expression) matches a match for  re,  with
                   the match noted for possible reporting

             (?:re)
                   as  previous,  but  does  no  reporting (a ‘‘non-capturing'' set of
                   parentheses)

             ()    matches an empty string, noted for possible reporting

             (?:)  matches an empty string, without reporting

             [chars]
                   a bracket expression, matching any one of the  chars  (see  BRACKET
                   EXPRESSIONS for more detail)

              .    matches any single character

             \k    (where  k  is  a non-alphanumeric character) matches that character
                   taken as an ordinary character, e.g. \\ matches a backslash charac‐
                   ter

             \c    where c is alphanumeric (possibly followed by other characters), an
                   escape (AREs only), see ESCAPES below

             {     when followed by a character other than a digit, matches the  left-
                   brace  character ‘{'; when followed by a digit, it is the beginning
                   of a bound (see above)

             x     where x is a single character with no other  significance,  matches
                   that character.

    A constraint ma tches an empty string when specific conditions
    are met.  A constraint may not be followed by a quantifier.  The
    simple constraints are as follows; some more constraints are
    described later, under ESCAPES.

             ^       matches at the beginning of a line

             $       matches at the end of a line

             (?=re)  positive lookahead (AREs only), matches at any point where a sub‐
                     string matching re begins

             (?!re)  negative lookahead (AREs only), matches at  any  point  where  no
                     substring matching re begins

     	The lookahead constraints may not contain back references (see
    later), and all parentheses within them are considered
    non-capturing.

    An RE may not end with ‘\'.

    BRACKET EXPRESSIONS

    A bracket expression is a list of characters enclosed in ‘[]'.
    It normally matches any single character from the list (but see
    below).  If the list begins with ‘^', it matches any single
    character (but see below) not from the rest of the list.

    If two characters in the list are separated by ‘-', this is
    shorthand for the full range of characters between those two
    (inclusive) in the collating sequence, e.g.  [0-9] in ASCII
    matches any decimal digit.  Two ranges may not share an
    endpoint, so e.g.  a-c-e is illegal.  Ranges are very collat‐
    ing-sequence-dependent, and portable programs should avoid
    relying on them.

    To include a literal ] or - in the list, the simplest method is
    to enclose it in [.  and .] to make it a collating element (see
    below).  Alternatively, make it the first character (following
    a possible ‘^'), or (AREs only) precede it with ‘\'.
    Alternatively, for ‘-', make it the last character, or the
    second endpoint of a range.  To use a literal - as the first
    endpoint of a range, make it a collating element or (AREs only)
    precede it with ‘\'.  With the exception of these, some
    combinations using [ (see next paragraphs), and escapes, all
    other special characters lose their special significance within
    a bracket expression.

    Within a bracket expression, a collating element (a character, a
    multi- character sequence that collates as if it were a single
    character, or a collating-sequence name for either) enclosed in
    [.  and .] stands for the sequence of characters of that
    collating element.  The sequence is a single element of the
    bracket expression's list.  A bracket expression in a locale
    that has multi-character collating elements can thus match more
    than one character.  So (insidiously), a bracket expression that
    starts with ^ can │ match multi-character collating elements
    even if none of them appear in the │ bracket expression!  (Note:
    Tcl currently has no multi-character collating │ elements.  This
    information is only for illustration.) │

    For example, assume the collating sequence includes a ch
    multi-character │ collating element.  Then the RE [[.ch.]]*c
    (zero or more ch's followed by │ c) matches the first five
    characters of ‘chchcc'.  Also, the RE [^c]b │ matches all of
    ‘chb' (because [^c] matches the multi-character ch).

    Within a bracket expression, a collating element enclosed in [=
    and =] is an equivalence class, standing for the sequences of
    characters of all col‐ lating elements equivalent to that one,
    including itself.  (If there are no other equivalent collating
    elements, the treatment is as if the enclosing delimiters were
    ‘[.' and ‘.]'.) For example, if o and ^ are the members of an
    equivalence class, then ‘[[=o=]]', ‘[[=^=]]', and ‘[o^]' are all
    synony‐ mous.  An equivalence class may not be an endpoint of a
    range.  (Note: Tcl │ currently implements only the Unicode
    locale.  It doesn't define any equivalence classes.  The
    examples above are just illustrations.)

    Within a bracket expression, the name of a character class
    enclosed in [: and :] stands for the list of all characters (not
    all collating elements!) belonging to that class.  Standard
    character classes are:

                  | alpha  | A letter.                                            |
                  | upper  | An upper-case letter.                                |
                  | lower  | A lower-case letter.                                 |
                  | digit  | A decimal digit.                                     |
                  | xdigit | A hexadecimal digit.                                 |
                  | alnum  | An alphanumeric (letter or digit).                   |
                  | print  | An alphanumeric (same as alnum).                     |
                  | blank  | A space or tab character.                            |
                  | space  | A character producing white space in displayed text. |
                  | punct  | A punctuation character.                             |
                  | graph  | A character with a visible representation.           |
                  | cntrl  | A control character.                                 |

    A locale may provide others.  (Note that the current Tcl
    implementation has │ only one locale: the Unicode locale.) A
    character class may not be used as an endpoint of a range.

    There are two special cases of bracket expressions: the bracket
    expressions [[:<:]] and [[:>:]] are constraints, matching empty
    strings at the beginning and end of a word respectively.  A
    word is defined as a sequence of word characters that is neither
    preceded nor followed by word characters.  A word character is
    an alnum character or an underscore (_).  These special bracket
    expressions are deprecated; users of AREs should use constraint
    escapes instead (see below).

    ESCAPES

    Escapes (AREs only), which begin with a \ followed by an
    alphanumeric char‐ acter, come in several varieties: character
    entry, class shorthands, constraint escapes, and back
    references.  A \ followed by an alphanumeric character but not
    constituting a valid escape is illegal in AREs.  In EREs, there
    are no escapes: outside a bracket expression, a \ followed by an
    alphanumeric character merely stands for that character as an
    ordinary character, and inside a bracket expression, \ is an
    ordinary character.  (The latter is the one actual
    incompatibility between EREs and AREs.)

    Character-entry escapes (AREs only) exist to make it easier to
    specify non- printing and otherwise inconvenient characters in
    REs:

             \a   alert (bell) character, as in C

             \b   backspace, as in C

             \B   synonym for \ to help reduce backslash doubling in some applications
                  where there are multiple levels of backslash processing

             \cX  (where  X is any character) the character whose low-order 5 bits are
                  the same as those of X, and whose other bits are all zero

             \e   the character whose collating-sequence name  is  ‘ESC',  or  failing
                  that, the character with octal value 033

             \f   formfeed, as in C

             \n   newline, as in C

             \r   carriage return, as in C

             \t   horizontal tab, as in C

             \uwxyz
                  (where  wxyz is exactly four hexadecimal digits) the Unicode charac‐
                  ter U+wxyz in the local byte ordering

             \Ustuvwxyz
                  (where stuvwxyz is exactly eight hexadecimal digits) reserved for  a
                  somewhat-hypothetical Unicode extension to 32 bits

    \v   vertical tab, as in C are all available.

             \xhhh
                  (where  hhh  is  any  sequence  of hexadecimal digits) the character
                  whose hexadecimal value is 0xhhh (a single character no  matter  how
                  many hexadecimal digits are used).

             \0   the character whose value is 0

             \xy  (where  xy  is exactly two octal digits, and is not a back reference
                  (see below)) the character whose octal value is 0xy

             \xyz (where xyz is exactly three octal digits, and is not a  back  refer‐
                  ence (see below)) the character whose octal value is 0xyz

    Hexadecimal digits are ‘0'-‘9', ‘a'-‘f', and ‘A'-‘F'.  Octal
    digits are ‘0'-‘7'.

    The character-entry escapes are always taken as ordinary
    characters.  For example, \135 is ] in ASCII, but \135 does not
    terminate a bracket expres‐ sion.  Beware, however, that some
    applications (e.g., C compilers) inter‐ pret such sequences
    themselves before the regular-expression package gets to see
    them, which may require doubling (quadrupling, etc.) the ‘\'.

    Class-shorthand escapes (AREs only) provide shorthands for
    certain com‐ monly-used character classes:

             \d        [[:digit:]]

             \s        [[:space:]]

             \w        [[:alnum:]_] (note underscore)

             \D        [^[:digit:]]

             \S        [^[:space:]]

             \W        [^[:alnum:]_] (note underscore)

    Within bracket expressions, ‘\d', ‘\s', and ‘\w' lose their
    outer brackets, and ‘\D', ‘\S', and ‘\W' are illegal.  (So, for
    example, [a-c\d] is equivalent to [a-c[:digit:]].  Also,
    [a-c\D], which is equivalent to [a- │ c^[:digit:]], is illegal.)

    A constraint escape (AREs only) is a constraint, matching the
    empty string if specific conditions are met, written as an
    escape:

             \A    matches  only  at the beginning of the string (see MATCHING, below,
                   for how this differs from ‘^')

             \m    matches only at the beginning of a word

             \M    matches only at the end of a word

             \y    matches only at the beginning or end of a word

             \Y    matches only at a point that is not the beginning or end of a word

             \Z    matches only at the end of the string (see MATCHING, below, for how
                   this differs from ‘$')

             \m    (where m is a nonzero digit) a back reference, see below

             \mnn  (where  m  is  a nonzero digit, and nn is some more digits, and the
                   decimal value mnn is not greater than the number of closing captur‐
                   ing parentheses seen so far) a back reference, see below

    A word is defined as in the specification of [[:<:]] and [[:>:]]
    above.  Constraint escapes are illegal within bracket
    expressions.

    A back reference (AREs only) matches the same string matched by
    the parenthesized subexpression specified by the number, so
    that (e.g.) ([bc])\1 matches bb or cc but not ‘bc'.  The
    subexpression must entirely precede the back reference in the
    RE.  Subexpressions are numbered in the order of their leading
    parentheses.  Non-capturing parentheses do not define subex‐
    pressions.

    There is an inherent historical ambiguity between octal
    character-entry escapes and back references, which is resolved
    by heuristics, as hinted at above.  A leading zero always
    indicates an octal escape.  A single non-zero digit, not
    followed by another digit, is always taken as a back reference.
    A multi-digit sequence not starting with a zero is taken as a
    back reference if it comes after a suitable subexpression
    (i.e.  the number is in the legal range for a back reference),
    and otherwise is taken as octal.

    METASYNTAX

     	In addition to the main syntax described above, there are some
    special forms and miscellaneous syntactic facilities available.

    Normally the flavor of RE being used is specified by
    application-dependent means.  However, this can be overridden by
    a director.  If an RE of any flavor begins with ‘***:', the rest
    of the RE is an ARE.  If an RE of any flavor begins with ‘***=',
    the rest of the RE is taken to be a literal string, with all
    characters considered ordinary characters.

    An ARE may begin with embedded options: a sequence (?xyz) (where
    xyz is one or more alphabetic characters) specifies options
    affecting the rest of the RE.  These supplement, and can
    override, any options specified by the application.  The
    available option letters are:

             b  rest of RE is a BRE

             c  case-sensitive matching (usual default)

             e  rest of RE is an ERE

             i  case-insensitive matching (see MATCHING, below)

             m  historical synonym for n

             n  newline-sensitive matching (see MATCHING, below)

             p  partial newline-sensitive matching (see MATCHING, below)

             q  rest of RE is a literal (‘‘quoted'') string, all ordinary characters

             s  non-newline-sensitive matching (usual default)

             t  tight syntax (usual default; see below)

             w  inverse  partial newline-sensitive (‘‘weird'') matching (see MATCHING,
                below)

             x  expanded syntax (see below)

    Embedded options take effect at the ) terminating the sequence.
    They are available only at the start of an ARE, and may not be
    used later within it.

    Embedded options take effect at the ) terminating the sequence.
    They are available only at the start of an ARE, and may not be
    used later within it.

    In addition to the usual (tight) RE syntax, in which all
    characters are significant, there is an expanded syntax,
    available in all flavors of RE with the -expanded switch, or in
    AREs with the embedded x option.  In the expanded syntax,
    white-space characters are ignored and all characters between a
    - and the following newline (or the end of the RE) are ignored,
    permitting paragraphing and commenting a complex RE.  There are
    three exceptions to that basic rule:

             a white-space character or ‘#' preceded by ‘\' is retained

             white space or ‘#' within a bracket expression is retained

             white space and comments are illegal within multi-character symbols  like
             the ARE ‘(?:' or the BRE ‘\('

    Expanded-syntax white-space characters are blank, tab, newline,
    and any │ character that belongs to the space character class.

    Finally, in an ARE, outside bracket expressions, the sequence
    ‘(?#ttt)' (where ttt is any text not containing a ‘)') is a
    comment, completely ignored.  Again, this is not allowed between
    the characters of multi-char‐ acter symbols like ‘(?:'.  Such
    comments are more a historical artifact than a useful facility,
    and their use is deprecated; use the expanded syn‐ tax instead.

    None of these metasyntax extensions is available if the
    application (or an initial ***= director) has specified that the
    user's input be treated as a literal string rather than as an
    RE.

    MATCHING

    In the event that an RE could match more than one substring of a
    given string, the RE matches the one starting earliest in the
    string.  If the RE could match more than one substring starting
    at that point, its choice is determined by its preference:
    either the longest substring, or the short‐ est.

    Most atoms, and all constraints, have no preference.  A
    parenthesized RE has the same preference (possibly none) as the
    RE.  A quantified atom with quantifier {m} or {m}?  has the same
    preference (possibly none) as the atom itself.  A quantified
    atom with other normal quantifiers (including {m,n} with m equal
    to n) prefers longest match.  A quantified atom with other
    non-greedy quantifiers (including {m,n}?  with m equal to n)
    prefers short‐ est match.  A branch has the same preference as
    the first quantified atom in it which has a preference.  An RE
    consisting of two or more branches connected by the | operator
    prefers longest match.

    Subject to the constraints imposed by the rules for matching the
    whole RE, subexpressions also match the longest or shortest
    possible substrings, based on their preferences, with
    subexpressions starting earlier in the RE taking priority over
    ones starting later.  Note that outer subexpressions thus take
    priority over their component subexpressions.

    Note that the quantifiers {1,1} and {1,1}?  can be used to force
    longest and shortest preference, respectively, on a
    subexpression or a whole RE.

     	Match lengths are measured in characters, not collating
    elements.  An empty string is considered longer than no match at
    all.  For example, bb* matches the three middle characters of
    ‘abbbc', (week|wee)(night|knights) matches all ten characters of
    ‘weeknights', when (.*).* is matched against abc the
    parenthesized subexpression matches all three characters, and
    when (a*)* is matched against bc both the whole RE and the
    parenthesized subexpression match an empty string.

    If case-independent matching is specified, the effect is much as
    if all case distinctions had vanished from the alphabet.  When
    an alphabetic that exists in multiple cases appears as an
    ordinary character outside a bracket expression, it is
    effectively transformed into a bracket expression con‐ taining
    both cases, so that x becomes ‘[xX]'.  When it appears inside a
    bracket expression, all case counterparts of it are added to the
    bracket expression, so that [x] becomes [xX] and [^x] becomes
    ‘[^xX]'.

    If newline-sensitive matching is specified, .  and bracket
    expressions using ^ will never match the newline character (so
    that matches will never cross newlines unless the RE explicitly
    arranges it) and ^ and $ will match the empty string after and
    before a newline respectively, in addition to matching at
    beginning and end of string respectively.  ARE \A and \Z con‐
    tinue to match beginning or end of string only.

     	If partial newline-sensitive matching is specified, this affects
    .  and bracket expressions as with newline-sensitive matching,
    but not ^ and ‘$'.

    If inverse partial newline-sensitive matching is specified, this
    affects ^ and $ as with newline-sensitive matching, but not .
    and bracket expressions.  This isn't very useful but is
    provided for symmetry.

    LIMITS AND COMPATIBILITY

    No particular limit is imposed on the length of REs.  Programs
    intended to be highly portable should not employ REs longer than
    256 bytes, as a POSIX- compliant implementation can refuse to
    accept such REs.

     	The only feature of AREs that is actually incompatible with
    POSIX EREs is that \ does not lose its special significance
    inside bracket expressions.  All other ARE features use syntax
    which is illegal or has undefined or unspecified effects in
    POSIX EREs; the *** syntax of directors likewise is outside the
    POSIX syntax for both BREs and EREs.

    Many of the ARE extensions are borrowed from Perl, but some have
    been changed to clean them up, and a few Perl extensions are not
    present.  Incompatibilities of note include ‘\b', ‘\B', the lack
    of special treatment for a trailing newline, the addition of
    complemented bracket expressions to the things affected by
    newline-sensitive matching, the restrictions on parentheses and
    back references in lookahead constraints, and the
    longest/shortest-match (rather than first-match) matching
    semantics.

    The matching rules for REs containing both normal and non-greedy
    quantifiers have changed since early beta-test versions of this
    package.  (The new rules are much simpler and cleaner, but don't
    work as hard at guessing the user's real intentions.)

    Henry Spencer's original 1986 regexp package, still in
    widespread use (e.g., in pre-8.1 releases of Tcl), implemented
    an early version of today's EREs.  There are four
    incompatibilities between regexp's near-EREs (‘RREs' for short)
    and AREs.  In roughly increasing order of significance:

    - In AREs, \ followed by an alphanumeric character is
    	either an escape or an error, while in RREs, it was just
    	another way of writing the alphanumeric.  This should
    	not be a problem because there was no reason to write
    	such a sequence in RREs.

    	* { followed by a digit in an ARE is the beginning of a
    	bound, while in RREs, { was always an ordinary
    	character.  Such sequences should be rare, and will
    	often result in an error because following charac‐ ters
    	will not look like a valid bound.

    - In AREs, \ remains a special character within ‘[]', so
    	a literal \ within [] must be written ‘\\'.  \\ also
    	gives a literal \ within [] in RREs, but only truly
    	paranoid programmers routinely doubled the backslash.

    - AREs report the longest/shortest match for the RE,
    	rather than the first found in a specified search order.
    	This may affect some RREs which were written in the
    	expectation that the first match would be reported.
    	(The careful crafting of RREs to optimize the search
    	order for fast matching is obsolete (AREs examine all
    	possible matches in parallel, and their performance is
    	largely insensitive to their complexity) but cases where
    	the search order was exploited to deliberately find a
    	match which was not the longest/shortest will need
    	rewriting.)

    BASIC REGULAR EXPRESSIONS

    BREs differ from EREs in several respects.  ‘|', ‘+', and ?  are
    ordinary characters and there is no equivalent for their
    functionality.  The delimiters for bounds are \{ and ‘\}',
    with { and } by themselves ordinary characters.  The
    parentheses for nested subexpressions are \( and ‘\)', with (
    and ) by themselves ordinary characters.  ^ is an ordinary
    character except at the beginning of the RE or the beginning of
    a parenthesized subexpression, $ is an ordinary character
    except at the end of the RE or the end of a parenthesized
    subexpression, and * is an ordinary character if it appears at
    the beginning of the RE or the beginning of a parenthesized
    subexpression (after a possible leading ‘^').  Finally,
    single-digit back references are available, and \< and \> are
    synonyms for [[:<:]] and [[:>:]] respec‐ tively; no other
    escapes are available.

- regex<-syntax: {basic,extended,advanced}
    Regex syntax used.  See <-pattern for a description of the
    pattern and how it is affected by this variable.  See also
    ->ignore_case.


## Send methods {#class-regex-send}

- regex->compile: optimise=[bool]
    Compile `<->pattern` to a compiled representation in _-compiled_.
    Normally invoked automatically by ->search when needed.

    If the argument is @on, the pattern is compiled `optimised`,
    increasing compilation time, but reducing search time.

- regex->file_pattern: file_pattern=char_array
    If `file_pattern` represents a common (Unix csh) file-pattern,
    translate it into a regular expression representing the same
    pattern and associate this using ->pattern.

    The following constructs are mapped:

    	?		.
    	*		.*
    	[...]	[...]
    	{a,b}	\(a\|b\)

    The pattern is closed with a '$' sign.

- regex->for_all: in=char_array|text_buffer|fragment, action=code, from=[int], to=[int]
    Run code on each match of this regular expression in the specified text.
    Forwards the following arguments:

    	| @arg1 | The regex object       |
    	| @arg2 | The object searched in |

    After each successful ->search, the argument code object is executed.
    The next ->search is started at <-register_end.  If the regex
    matched an empty string and the string is still empty after
    executing the code object, the search is restarted at
    <-register_end + 1 to avoid a loop.

    When `from` and `to` are specified, the for_all is ran only in
    the specified range.

    The code executed may invoke ->register_value, ->replace,
    etc.  to modify the text.  The following code replaces all `foo` in
    `bar` in string _S_:

    	?- send(regex(foo), for_all, S,
    			message(@arg1, replace, @arg2, bar)).

    See also `char_array<-split`.

- regex->ignore_case: bool
    Ignore case differences during ->search when @on.

- regex->initialise: pattern=[char_array], case_sensitive=[bool], syntax=[{basic,extended,advanced}]
    Create a regular expression from a pattern string.  The regular
    expression will be ->compile'd on the first call to ->match or
    ->search.   After creation the pattern may be changed using
    ->pattern.

    If the `case_sensitive` argument is @off, searching and matching
    is caried out ignoring case.  See also ->ignore_case.

- regex->match: in=char_array|text_buffer|fragment, start=[int], end=[int]
    *Inherits description from*: regex<-match

- regex->pattern: char_array
    *Inherits description from*: regex-pattern

- regex->replace: in=object, value=char_array
    Replace last match which char_array.

- regex->register_value: in=object, value=char_array, register=[0..9]
    Replace the indicated register in the object.  All register indices
    stored in the regular expression are updated accordingly.

    ->replace is equivalent to ->register_value: Object, Value, but in
    addition to this method expands the special construct '\digit' with
    the text of the corresponding register value.

- regex->search: in=char_array|text_buffer|fragment, start=[int], end=[int]
    Search for the expression in a char_array (name or string) or a
    text_buffer.  The two integers specify the range to search in as [start,
    end).  This method succeeds if a match is found and fails otherwise.
    After a successful search, <-register_start, <-register_end and
    <-register_value may be used to get information on the location of the
    match.

    If `end` is smaller than `start`, the search is executed backwards.
    The <-search variant returns the <-register_start.

    See also ->pattern, ->match and <-match.

    @see regex<-register_start
    @see regex<-register_value
    @see regex<-register_end


## Get methods {#class-regex-get}

- regex<-match: in=char_array|text_buffer|fragment, start=[int], end=[int] -> length=int
    Match the regular expression's <-pattern with the given object.  The
    match starts at `start` (default 0) and should not pass `end` (default
    end of the object matched).    The <-match variant returns the number of
    characters matched.  For both methods, <-register_start,
    <-register_value, etc. may be used afterwards.

    See also ->search and ->match.

- regex<-quote: pattern=char_array -> quoted=string
    Given some string that may contain regex special characters, return a
    string in which all these characters are properly escaped.  May be used
    to build a pattern from a plain text.   Example:

    	?- get(regex(''), quote, '^hello*', S),
    	   get(S, value, T).

    	S = @27463123
    	T = '\^hello\*'

- regex<-register_end: register=[0..9] -> index=int
    0-based index of the end of the match or the end of some register.  _0_
    refers to the entire match; the numbers 1 to 9 to register values.

    @see regex->search

- regex<-register_start: register=[0..9] -> index=int
    0-based index of the start of the match or the end of some register.
    _0_ refers to the entire match; the numbers 1 to 9 to register values.

    @see regex->search

- regex<-register_value: in=object, register=[0..9], type=[type] -> register=any
    New string holding the text of the entire match or one of the registers
    1 to 9.

    @see regex->search

- regex<-registers: -> registers=int
    Number of registers in the pattern.

- regex<-search: in=char_array|text_buffer|fragment, start=[int], end=[int] -> start=int
    Search for the expression in a char_array (name or string) or a
    text_buffer.  The two integers specify the range to search in as [start,
    end).  This method succeeds if a match is found and fails otherwise.
    After a successful search, <-register_start, <-register_end and
    <-register_value may be used to get information on the location of the
    match.

    If `end` is smaller than `start`, the search is executed backwards.
    The <-search variant returns the <-register_start.

    *Inherits description from*: regex->search

