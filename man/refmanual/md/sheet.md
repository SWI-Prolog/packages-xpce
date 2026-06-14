# class sheet {#class-sheet}

A sheet object is a list of name/value pairs.  Sheets are similar to
records in Pascal, structures in C or frames in knowledge representation
formalisms.  Unlike records and structures however, sheets can be given
new attributes  and attributes can be deleted dynamically..

Sheets are generally used to create a single handle to a number of
objects.  In previous versions of PCE they where also commonly used to
represent application data.  The current version allows for user-defined
classes, which form a much more efficient implementation of name/value
pairs than sheets.

Note also that adding an attribute to an object using `Object
->attribute' is often a good alternative for using sheets.  Finally, if
the collection of attribute is large and lookup is more often than
changing the data, class hash_table should be considered.

@see class hash_table
@see object->attribute
@see topic Object-level Programming
@see topic Class-level Programming


## Instance variables {#class-sheet-instvars}

- sheet<-members: chain
    Chain object holding the attributes of the sheet.  It is allowed to
    analyse and modify this chain directly.


## Send methods {#class-sheet-send}

- sheet->append: attribute
    Append an attribute.  If the sheet already has an attribute with the
    same name, the value of the existing attribute is replaced by the value
    of the argument attribute.   See also ->value.

    Note that `attribute <-convert` convert an attribute name into an
    attribute object with value @nil.

    @see sheet->value

- sheet->catch_all: key=name, value=any
    The methods <->catch_all are called by PCE's message-passing system if
    there is no other definition for a method.  Class sheet uses these
    methods to store and fetch attributes:

    	?- new(@s, sheet(name, age, address)),
    	   send(@s, name, gnu).

    	?- get(@s, name, X).

    	   X = gnu

    This approach has, besides its definite charm, one drawbacks: Attributes
    whose name collide with a method name cannot be accessed this way.
    Code that handles unknown attribute names should use <->value to
    avoid this problem.

- sheet->delete: name=any
    Delete attribute with the given name.  Fails silently if no such
    attribute exists.

- sheet->for_some: action=code
    Run code on all attributes.

- sheet->for_all: action=code
    Run code on all <-members.  Arguments:

    	@arg1	attribute object

- sheet->initialise: member=attribute ...
    Create a sheet from a list of attributes:

    	?- new(Sheet, sheet(attribute(name, gnu),
    						attribute(age, 100))).

- sheet->value: key=any, value=any
    Associate `name` with `object`.  If the sheet already has an attribute
    with this name, the value of the attribute is changed.  Otherwise a new
    attribute with the given name and value is appended to the sheet.

    @see sheet->send_catch_all
    @see sheet->append


## Get methods {#class-sheet-get}

- sheet<-_arg: int -> attribute
    Nth-1 attribute object.  Used for conversion to a host-language term.

- sheet<-catch_all: key=name -> value=any
    *Inherits description from*: sheet->catch_all

