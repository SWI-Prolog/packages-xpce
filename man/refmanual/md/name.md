# class name {#class-name}

Class name defines atomic text in PCE.  That is, each instance of this
class represents a unique text or, in other words, two name objects
represent the same text if-and-only-if they have the same object
reference (hence are the same objects).

Names, like atoms in Prolog and keywords in Lisp, are used as
identifiers: class names, method names, etc.  The `uniqueness` feature
guarantees fast mapping from names to the named objects (see class
hash_table).

PCE does not garbage collect names.  Names should thus only be used
for constants and not for handling text that is continuously modified.
Class string is designed for handling temporary and changing textual
information.


## Send methods {#class-name-send}

- name->_value: char_array
    Modify the contents of the name, preserving its identity.  This method
    supports ->syntax and should only be used at boot time to solve
    specific host-language binding problems.

- name->equal: name
    Succeeds if the argument name is the same name as the receiver.
    See notes on uniqueness with class name and ->initialise.

- name->initialise: text=char_array
    Create a name from the argument text.  Note that this method will only
    be called by the PCE object creation system if <-lookup fails (i.e. if
    this is a new name).   This method copies the text and then invokes
    ->register on itself to insert itself into the symbol table.

- name->register
    *Inherits description from*: name<-lookup

- name->syntax: {uppercase}, char
    If the word is strictly lowercase, all characters are mapped to
    uppercase and the nametable is updated accordingly.

    The second argument is the word-separator character.  All occurrences of
    the current word-separator will be mapped on this character.

    This method is exclusively to support `pce ->syntax`.

    @see pce->syntax


## Get methods {#class-name-get}

- name<-_bench: int -> int
    Calls the internal function to look-up the text of itself <n> times and
    returns the number of failing comparisons in the symbol-table.

- name<-_buckets: -> int
    Returns the number of buckets in the name-table.  Should return the
    same value for each (defined) name.

- name<-copy: -> name
    Equivalent to <-self.

- name<-lookup: char_array -> name
    The methods ->register and <-lookup form a pair that guarantee the
    uniqueness of name objects.  If a string (char_array) is to be converted
    into a name object, the PCE object creation system will first call
    <-lookup, which attempts a lookup in the name-table.   On failure, the
    object creation system will call ->initialise to create a name.  ->initialise
    calls ->register which registers the new name in the name-table.

- name<-modify: char_array -> name
    Returns a name object from the given argument text.  See `char_array
    <-modify' for details.

