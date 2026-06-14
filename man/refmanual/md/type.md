# class type {#class-type}

A type object defines which data entities are accepted as filler for
an instance variable or arguments to a method.  Type objects are part
of the definition of both methods and variables.

Types are normally specified using a name. The <-convert method
translates type-names into (reusable) type objects.  See <-convert
for the type syntax.

If a type is used to specify the arguments to a method, it can also
specify the name of the argument.  See <-argument_name.  Argument names
are used for documentation purposes as well as to allow for the
specification of arguments by name rather than by position.  See class
:= for details.

**Bugs**:

Currently, the semantics of a type is determined by the <->kind
attribute and no new kinds may be introduced.

Type objects have no protection against changes to them, but they may
not be changed, except as part of their definition process.

@see object->_check
@see class variable
@see class send_method
@see class get_method


## Instance variables {#class-type-instvars}

- type<-argument_name: name*
    Name of the argument specified.  If @nil, no argument name is
    associated.   Types used to specify an argument name have
    <-kind: alias.  <-context contains the real type specification.

- type<-context: any
    The context field is provided with the C-functions <-translate_function
    and <-validate_function.  The contents of this field is determined by
    these functions, and therefore by the <->kind attribute.

    @see type-kind

- type<-fullname: name
    The name used to define this type.  See <-convert.

- type<-kind: name
    The <-kind attribute determines the -validate_function and -translate_function
    pointers and thus interpretation of the context attribute.  Its values are (between
    brackets the the syntax construct; see also <-convert).

    Objects (instances of classes):

    - class [<name of the class>]
    		Context:		An instance of class class.
    		Validate:		Accepts instances of this class or a super-class.
    		Convert:		Activates the <-convert method of the class.

    - class_object [object]
    		Context:		@object_class.
    		Validate:		Any objects, except instances of class function.
    		Convert:		See `Object <-convert`.

    Special types:

    - unchecked [unchecked]
    		Validate:		Accepts anything.

    - any [any]
    		Validate:		'object|int'  (i.e. no function objects).
    		Translate:	Evaluates functions.

    Numeric values:

    - int [int]
    		Convert:		Name, string, number, real.

    - char [char]
    		Validate:		Integer 0..255.
    		Convert:		char_array with the following syntax.

    - one character
    			* \n, \t, \r, \f, \b with their usual meaning
    			* ^<char> for control characters
    			* \C-<char> for control characters
    - M- followed by any of above to form meta characters.

    - int_range [<low>..<high>]
    		Context:		tuple(low, high).
    		Validate:		Checks range (including low and high).
    		Translate:	Translates to `int` and validates range,

    - real_range [<low>..<high>]
    		Context:		tuple(low, high).
    		Validate:		Checks range (including low and high).
    		Translate:	Translates to `real` and validates range.

    Events:

    - event_id [event_id]
    		Validate:		char or node in @event_tree.
    		Convert:		`char` or event-name (see @event_tree).

    _Set_ types:

    - value
    		Context:		value accepted.
    		Validate:		only the equivalent of the context.

    - name_of [{<name1>,<name2>, ...}]
    		Context:		Chain with accepted names.
    		Validate:		Accepts members of the chain.
    		Translate:	To a name, then checks for membership.

    - member [member:<type name>]
    		Context:		Requested type.
    		Validate:		Validate requested type.
    		Translate:	Invoke `Context <-member: value`.

    - value_set [no syntax]
    		Context:		Chain of element or function
    		Validate:		Expand functions

    	Because <-context does not allow for a function, functions should
    	be passed embedded in a quote_function object.  The function
    - must* evaluate to a chain.

    Type disjunction:

    - compound [<type name 1>|<type name 2>|...]
    		Validate:		Just validates the super-types.
    		Translate:	According to the super-types.

    Type-name alias and argument names:

    - alias
    		Context:		aliases type.
    		Validate:		as <-context.
    		Translate:	as <-context.

    Internal use:

    - alien [alien:<name of C-type]
    		Context:		Name of C-type.
    		Validate:		Accepts anything.

    @see type-validate_function
    @see type-translate_function
    @see type-context

- type<-supers: chain*
    Chain of type objects that act as alternatives.  Used by ->validate and
    <-translate after its own validation or translation fails.

    @see type<-check

- type-translate_function: alien:Func
    @see type-kind
    @see type<-translate

- type-validate_function: alien:SendFunc
    @see type-kind

- type<-vector: bool
    Indicates the type should be interpreted as a `varargs` type by a
    method.  This implies any number of datums of this type is allowed.


## Send methods {#class-type-send}

- type->initialise: name=name, kind=[name], context=[any], supers=[chain*]
    Create a new type object.  Name is the name of the type and
    should not already exist (see also <-lookup).  After creation,
    the type is ->protect'ed and a mapping from the type <-fullname
    to the type object is added to the hash_table object @types.
    The arguments are:

    - name
    	<-fullname of the new type.  This name will be added to the
    	type name-table and must be unique.

    - <-kind
    	Determines the validate and translate behaviour (the
    	interpretation of the context).

    - <-context
    	The context value.  See ->kind for details

    - <-supers
    	Alternative types of datums accepted

    Types are normally created by converting names (see `Type <-convert`) or
    <-copy to create synonym names for a complex type.

    @see type<-convert
    @see type<-copy

- type->validate: unchecked, [object]*
    Validate that the argument can be accepted as satisfying this type.
    First activates the <-validate_function.  When this fails, it
    requests the super-types to validate the argument.

    @see type<-check
    @see type<-translate


## Get methods {#class-type-get}

- type<-check: value=unchecked, context=[object]* -> unchecked
    The <-check method is the normal way to activate a type.  The first
    argument is the object to be checked.  The second is the receiver that
    requests the conversion.  The latter is currently used only for the
    <->kind member.

    Types are normally referred to by their <-name rather than by their
    reference.   For this reason type checking and conversion is also
    accessible through `@pce <-convert`.  The following two lines are
    identical:

    	?- get(@pce, convert, '34', int, Int).
    	?- get(type(int), check, '34', Int).

    First, ->validate is activated.  If this fails, <-translate is activated
    and the result thereof is returned.

    @see class function
    @see pce<-convert
    @see text_item-type
    @see type->validate
    @see type<-translate
    @see type-supers

- type<-convert: name -> type
    Convert a name into a type.  This is the normal way to define or use a type.
    The conversion syntax is (informally) defined below:

    	<Type>      ::= <KindType>
    			      | <SingleType> '...'		% Methods: Varargs

    	<KindType>  ::= 'alien:'<C-type>        % Alien (C) types
    				  | 'member:'<Single>		% Members

    	<Single>    ::= <PrimType>
    				  | <Single>'|'<Single>		% Disjunction
    				  | '['Single']'			% May be @default
    				  | <Single>'*'				% May be @nil

    	<PrimType>  ::= <PredefinedType>        % any defined type
    		  		  | {<Name>',' ...}			% name_of
    				  | <int>'..'<int>			% integer range
    				  | <real>'..'<real>		% real range
    				  | <ClassName>

    Below are some examples:

    	point			Instance of class point or a subclass
    	int		    	integer
    	[name]			A name or @default
    	graphical*		A graphical object or @nil
    	int|point		integer or point
    	{left,right}	either the name 'left' or 'right'
    0. .100			integer in the range 0 ... 100 (including)
    	name|function	Accepts a name or a function object.

    @see !bad_type_syntax
    @see pce<-convert
    @see type->initialise

- type<-copy: name -> type
    Create a copy of a type with a new name.  This may be used to create
    synonym types or extend types in ways not supported by the default
    conversion from a name to a type.  Example:

    	?- get(@pce, convert, '{left,center,right}', type, X),
    	?- get(X, copy, format_name, _).

    Creates a type `format_name` that accepts the names 'left', 'center' or
    'right'.

    @see type->initialise

- type<-lookup: name -> type
    Lookup type in type-database @types.

- type<-translate: value=unchecked, context=[object]* -> unchecked
    Convert the first argument into a datum that satisfies the type.  The
    second argument is optional context information.  Both variables and
    methods pass the receiver through this argument.  The pseudo code
    of this method is:

    	translate(Type type, Any value, Object context)
    	{ Activate translate_function.
    	  IF successful, return the translated value;
    	  ELSE try super-types until one succeeds and return the value
    	}

    NOTE:	Normally not activated directly, but through `type <-check`
    		because the argument may not be of the requested type.

    @see type->validate
    @see type<-check
    @see type-translate_function

- type<-value_set: [object]* -> chain
    Chain with values that satisfy the type object. This method is exploited
    by `text_item <-completions` to perform automatic completion.

    Currently this method takes care of:

    - <-kind = name_of types ({name1,name2})
    - <-kind = int_range if the range entails at most 10 integers
    - <-kind = value
    - <-supers

