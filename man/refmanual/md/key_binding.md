# class key_binding {#class-key_binding}

Class key_binding is a subclass of class recogniser. It accepts keyboard
events and maps them onto messages.

Class key_binding deals with

- Parsing multiple key functions (see <-prefix)
- Parsing universal argument (compatible to GNU-EMACS)
	   (see <-argument).
- Handling column-saving for next_line and previous_line
	   functions.  (see <-saved_column).

Key_binding objects are used by various classes that are sensitive to
keyboard events:  editor, text, text_item and list_browser.

## Example {#class-key_binding-example}

The following example shows how a key_binding object may be used to
make a graphical object editable using the mouse:

	 ?- send(new(P, picture('Graphical editor')), open),
		send(P, display, new(B, box(100,100))),
		send(B, recogniser, new(K, key_binding(@nil, argument))),
		send(K, function, 'TAB',
			 message(@receiver, inverted,
					 @receiver?inverted?negate)),
		send(K, function, '\C-d', free),
		send(K, function, '\C-p', pen).

Typing TAB (= control-I), the box is inverted; typing control-d will
delete the graphical; typing META-3 control-p will make the pen
(thickness) of the line 3. See <-argument on how to specify the
universal numeric argument.

## Organisation of the class {#class-key_binding-organisation-of-the-class}

How a key_binding parses a keyboard event is described with ->typed;
The specification of symbolic key-names is described with ->function;
Resolution of a binding from a symbolic key is described with<-function;
Predefined binding tables are described with ->initialise.  See also
@key_bindings.

## Library keybinding {#class-key_binding-library-keybinding}

The PCE/Prolog library file keybinding.pl defines the predicate
show_key_bindings/1, which dumps the key_binding of objects or plain
tables.

@see class list_browser
@see class text_item
@see class text
@see class editor


## Instance variables {#class-key_binding-instvars}

- key_binding<-argument: [int]
    Current setting of the (integer) argument.  The following functions set
    this argument:

    - digit_argument
    	If \e0 .. \e9 and \e- are bound to the function digit_argument
    	the universal argument may be composed by typing its
    	value with the META-key held down.

    - universal_argument
    	A value may be composed by typing the key bound to
    	the function `universal_argument`, followed by the digits
    	to construct its value.

    The predefined key_binding name `argument` binds \e0 .. \e9 and \e- to
    `digit_argument` and \C-u to `universal_argument`. Using this table, the
    following sequences are examples for entering a universal_argument:

    	| Sequence | Value |
    	| \e1\e4   | 14    |
    	| \C-u56   | 56    |

- key_binding<-bindings: sheet
    Sheet mapping key-names to functions. The key-name is a symbolic
    representation of the key-sequence mapped.  These names are
    described with the method ->function. The function is either a name, in
    which case the key-binding will invoke the named method on the
    event-receiver, or a code object.

    @see key_binding->function

- key_binding<->default_function: name|code*
    Name or code object which is used when neither the <-bindings nor one of
    the <-defaults key_bindings define a binding for the typed key.

    A common way to use this facility is to declare <->default_function to
     `alert`. Any unknown keys will then be mapped onto the method
     `graphical ->alert`.

- key_binding<-name: name*
    Name of this binding table. Any named binding-table is stored in the
    global object @key_bindings for reuse.

- key_binding<-prefix: name
    Currently parsed prefix for multi-key functions. Its initial value is
    the empty name (''). If a typed key is bound to the function `prefix`
    this variable is set to this key.

    The predefined table `emacs_special` binds \e, \C-c and \C-x to
    the function prefix.

- key_binding<-saved_column: int*
    This variable is used to store the current column for multiple next_line
    and previous_line functions.  Its value is managed by ->event.

- key_binding-status: {universal_argument,quoted_insert}*
    Internal flag used to parse universal arguments.


## Send methods {#class-key_binding-send}

- key_binding->apply_preferences
    This method is called by the system on predefined named
    key-binding tables after the default initialisation.  The system
    implementation calls ->bind_resources, which applies
    modifications from class-variables as defined in the
    XPCE Defaults file.

- key_binding->bind_resources: [name]
    Apply preferences from class-variables.  If `name` is not
    specified the <-name of the binding table is used.  For
    example, to bind ^S to `save` in editor objects use the
    following line in ~/.xpce/Defaults:

    	key_binding.editor = \
    		[ '\\C-s' = save \
    		]

- key_binding->event: event
    Refinement of the `recogniser->event` method.

    If the event is a subtype of event-type `keyboard` this method invokes
     ->typed: `event <-id`, `event <-receiver`.

    @see key_binding->typed

- key_binding->execute: receiver=object, selector=name, arguments=any ...
    Called by ->fill_arguments_and_execute.  It simply invokes the
    method `selector` on `receiver` with the given arguments.  This
    method may be redefined to take action before or after invoking
    a method on the receiver:

    	execute(KB, Rec:object, Sel:name, Args:any ...) :->
    		<before-refinement>
    		send(KB, send_super_vector, execute, Rec, Sel, Args),
    		<after-refinement>.

    See also `object ->send_super_vector`.

- key_binding->fill_arguments_and_execute: id=event_id, receiver=object, selector=name, arguments=any ...
    This method is called from ->typed.  The key_binding object
    should send a message to `receiver` named `selector`.
    `arguments` is a partial argument list.  This method collects
    possible additional arguments:

    If an argument of the send-method does not accept @default,
    - and* the <-receiver defines the method <-interactive_argument,
    this method will be invoked using the following arguments:

    	| 1-st: | Behaviour object implementing the method |
    	| 2-nd: | Ordinal number of the argument required  |

    This method should return a value for the argument or 	fail.

    If no argument can be found, the error no_argument is raised.

    When all arguments are gathered, ->execute is activated.

- key_binding->function: key=name|event_id, action=name|code
    Append a mapping. The first argument is the symbolic name of the key
    sequence to be mapped.  The second is either the selector of the method
    to be invoked or a code object to be executed.

    When an event_id is passed it will be converted into the corresponding
    symbolic key-name.   The symbolic key-names for the characters
    0. .127 (ASCII value) are.

    	| Symbolic name | ASCII   | Description                   |
    	| \C-@ ... \C-h | 0..8    | Control-characters            |
    	| TAB           | 9       | The tab-key or control-I      |
    	| LFD           | 10      | The linefeed-key or control-J |
    	| \C-k .. \C-l  | 11..12  | Control-characters            |
    	| RET           | 13      | The return-key or control-M   |
    	| \C-n ... \C-z | 14..26  | Control-characters            |
    	| \e            | 27      | The escape-key or control-[   |
    	| \C-\ ... \C-_ | 28..31  | Control-characters            |
    	| SPC           | 32      | The space-bar                 |
    	| @ ... ~       | 33..126 | Normal printable characters   |
    	| DEL           | 127     | The delete-key                |

    The meta-characters (ASCII 128..255) are mapped onto \e, followed by the
    0. .127 symbolic name.

    @see key_binding-bindings

- key_binding->initialise: name=[name]*, super=key_binding ...
    Create a key_binding object from its name and default tables (see
    <-defaults).   PCE predefines some names.  If a table with a predefined
    name is created the table will be initialised according to this name.
    Tables that do not have a predefined name are initially empty.

    The predefined table-name are:

    - insert
    	Assigns the function `insert_self` to all printable characters
    	and the TAB, LFD and RET keys.

    - argument
    	Assigns `digit_argument` to \e0 ... \e9 and \e-; assigns
    	`universal_argument` to \C-u.

    - emacs_special
    	Assigns `prefix` to \e, \C-c and \C-x; `keyboard_quit to \C-g

    - emacs_page
    	Basic emacs paging commands: \C-v; \ev; \C-l

    - emacs_edit_basics
    	Simple emacs editing commands

    - emacs_basics
    	Simple emacs commands combined.

    - emacs_view_basics
    	Simple emacs commands excluding editing commands

    - editor
    - text
    - text_item
    - text_item_view
    - list_browser
    	Default binding table for the corresponding classes.

- key_binding->reset: [graphical]*
    Reset <-receiver to the argument graphical; <-prefix to '' and
    <-argument to @default.

    **Defaults**: When no graphical is specified, <-receiver is set to @nil.

- key_binding->typed: id=event|event_id, for=[object]
    Process the event(-id) of a keyboard event object arrived on the
    given graphical.  It performs the following steps:

    1. If `graphical` is specified and not equal to <-receiver,
    	  invoke ->reset and set <-receiver to `graphical`.

    2. Construct the `key` by appending the symbolic name of
    	the typed character to the current <-prefix.  Determine
    	the binding of this key using <-function.

    3. Handle the predefined bindings:

    - prefix
    		Assign <-prefix with the current key.
    - keyboard_quit
    		Invoke ->reset to clear <-prefix and <-argument
    - next_line, previous_line
    		If <-saved_column equals @default, set it to the current
    		column by invoking <-column on the receiver.
    - digit_argument
    		If the key is \e0 ... \e9 or \e-, set the <-argument.
    - universal_argument
    		Start specification of <-argument. First time: set
    		<-argument to 4; afterwards: multiply <-argument by
    4. Typing digits will set the argument to the typed
    		value.

    4. If the binding is a name, resolve the method to be invoked
    	using `object <-send_method`.  Next specify the arguments
    	according to their argument-type:

    - event_id
    		Fill the argument with argument event_id
    - char
    		Fill the argument with the event_id if this is the id
    		of an ASCII event
    - int
    		Fill the argument with <-argument when specified
    - default
    		Fill the argument with @default.

    	Finally, ->typed invokes ->fill_arguments_and_execute to
    	fill additional arguments, which in turn calls ->execute
    	to send a message to the <-receiver.

    	If the key-binding is a code object, ->forward this code object
    	with the following arguments:

    		| @receiver | <-receiver            |
    		| @arg1     | <-argument            |
    		| @arg2     | the argument event-id |

    Finally on behalf of uniting Emacs and Windows users with the
    editor, if a key is bound to the function prefix_or_copy or
    prefix_or_cut, the system invokes `editor->cua_key_as_prefix`
    on the receiver.  If this method succeeds the key acts as prefix
    and otherwise it is mapped to the functions cut or copy.

    **Defaults**: If no graphical is specified, <-receiver left unmodified.

    @see key_binding<-function
    @see key_binding->event


## Get methods {#class-key_binding-get}

- key_binding<-convert: name -> key_binding
    Convert the name of a named (reusable) key_binding object into the
    corresponding object. Named key_binding objects are stored in the table
    @key_bindings.

    If the name cannot be found in @key_bindings but it is the name of a
    predefined table (see <-initialise), <-convert will create a table and
    initialise it.

    @see @key_bindings

- key_binding<-function: event|name|event_id -> name|code
    Find the function to which the specified key is bound. This function is
    used by ->typed.  It performs the following steps:

    1. Lookup the key in the table <-bindings.  Return the result
    	     if found.

    2. Lookup in the <-bindings of the <-defaults tables
    	    (recursively).  If found somewhere return the result.

    3. Return <-default_function when not @nil.

    4. Search for <-default_function in the <-defaults tables.

    5. Fail silently if all of the above fails.

    @see key_binding->typed

- key_binding<-lookup: name, key_binding ... -> key_binding
    Lookup existing table in @key_bindings.

