# class constant {#class-constant}

Instances of class constant represent unique values.  This uniqueness is
represented and guaranteed by the object-reference.

Class constant defines the instance variables <-name and <-summary for
documentation purposes.

The system defines the following constants:

	| @nil     | _Nothing, not-filled_ |
	| @default | _Use default value_   |

The user may create constants for a specific application.  Constants may
not be deleted as they are automatically protected using
->protect.   See also class bool.

@see class bool


## Instance variables {#class-constant-instvars}

- constant<->name: name
    Name of the constant.  Note that <-print_name (used by class text_item)
    returns <-name by default.

- constant<->summary: string*
    Short description for documentation purposes.  Similar to `class
    <-summary', etc.


## Send methods {#class-constant-send}

- constant->initialise: name=name, summary=string
    Create a constant from its name and summary.  The corresponding slots
    are filled and the created instance is ->protect'ed against the garbage
    collector.  It is very uncommon for application programmers to define
    new constants.

