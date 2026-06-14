# class host_data {#class-host_data}

Class host_data deals with representing native host-data in XPCE
objects.  A host_data object contains a void * handle that can
be used by the interface to the host-language to pack native
data in an XPCE object.

Objects of class host_data are only acceptable to types that
explicitely include this type.  I.e. the generic types `any` and
`object` do not accept instances of this class.  This is
primarily for backward compatibility reasons: a Prolog term
should be translated as quickly as possible into an XPCE object
to avoid multiple places where this translation happens, and
thus unexpected and unintended creation of multiple different objects
from the same Prolog term.

Except for class host, no methods in XPCE accept objects of
class host_data.  Usage of this class and its associated type
therefore only makes sense in the context of classes defined
in the host-language.

See class prolog_term and ``Programming in XPCE/Prolog.


## Send methods {#class-host_data-send}

- host_data->initialise: alien:void *
    Instances of class host_data are created using special interface
    code between the host and XPCE.  Explicit creation of instances
    is not allowed.   The following example stores host-data in an
    instance-variable:

    	:- pce_begin_class(term, object,
    					   "Represent a prolog term").

    	variable(value, prolog, both, "represented value").

    	initialise(T, Term:prolog) :->
    		send_super(T, initialise),
    		send(T, value, Term).

    	:- pce_end_class.

