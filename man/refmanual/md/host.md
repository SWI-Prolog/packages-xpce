# class host {#class-host}

Class host normally has only one instance representing the programming
environment XPCE is connected to.   When connected to Prolog, this
object is called @prolog and when connected to Lisp it is called @lisp.
Other and multiple languages are possible too, but the interfaces have
not yet been defined.  As the reference manual is geared towards prolog,
we will call the instance of class host @prolog.

The object @prolog/@lisp makes the Prolog/Lisp environment available
from XPCE code objects..  It allows PCE to pass control to a Prolog
predicate by mapping a send operation onto a call to a Prolog predicate
with the same name as the selector of the message.

Thus,

	?- send(@prolog, format, 'Hello World~n').

Invokes `format(`Hello World~n')'  and thus prints _Hello World_.

Similar, the *get* operation asks Prolog to compute a value.
Suppose the predicate plus(+A, +B, -C) exists that adds two
numbers.  Then

	?- get(@prolog, plus, 1, 2, X).

responds with _3_ as expected.  Sending messages to @prolog is the
common way to link UI objects, existing in PCE to the application,
running in Prolog:

	new(B, button(help, message(@prolog, give_help)))

will invoke the Prolog predicate `give_help/0` if the user presses the
_Help_ button.

The communication between PCE and the host language may be synchronous
or asynchronous.  See ->call_back and <-messages.

@see class pce


## Instance variables {#class-host-instvars}

- host<->call_back: bool
    When @on, a message sent to the host object (@prolog) will immediately
    be executed.  Otherwise it will be stored in the chain <-messages for
    later retrieval by the host language.

    Using call_back is the default.  Synchronous interfacing (no call_back)
    harms the functionality as PCE code and host-language code will not be
    intercallable.  Sometimes necessary when PCE is connected to a host
    language that is not capable of handling callback.

    **Defaults**: @on (use call_back based interface).

    @see host-messages

- host<->language: {prolog,lisp,c}
    Language this host is talking too.  Currently documentation purposes
    only.

- host<-messages: chain*
    Queue of message for the hostlanguage.  When a message reaches the host
    object (@prolog) and <-call_back is @off, a message object is created
    from the receiver and arguments and this is appended to this chain for
    later retrieval by <-message.

    @see host<-message
    @see host-call_back

- host<->system: name
    Identifier name for the host language implementation PCE is connected
    too.  Used by `pce ->banner` that prints the banner message when PCE is
    started.  May be useful to solve incompatibility problems between, for
    example, two different Prolog versions.

    All supported languages should be assigned a unique identifier name.

    @see pce->banner


## Send methods {#class-host-send}

- host->call: name|host_data, unchecked ...
    `Host <->call: FunctorName, ...Args...` is used to implement PCE
    send/get methods in the host language.

    It attempts to convert the first argument into a name.  This argument is
    used to find the function in the host language.  For all other
    arguments, the special arguments @receiver, @arg1, ... @arg10 are
    expanded to their current bindings.  No further typechecking is done as
    this is already done by the send_method that normally invokes this
    method.

    **Diagnostics**:
    Fails silently if first argument cannot be converted into a name or the
    invoked host-language function fails.

    @see host<-call
    @see class send_method
    @see host->send_catch_all

- host->catch_all: name, any|host_data ...
    Call function in the host-language.  As requested by the argument types,
    functions will be evaluated by this method.  The method <->catch_all is
    invoked by the message passing system if there is no method defined,
    thus

    	?- message(@prolog, my_nice_little_predicate).

    calls my_nice_little_predicate/0, but

    	?- message(@prolog, protect).

    Invokes `object ->protect`.  See also <->call.

- host->initialise: name=name
    Create an interface to the host language.  This is allowed, but very
    uncommon.  It might be useful to use mixed synchronous and asynchronous
    interfaces or multiple message channels.


## Get methods {#class-host-get}

- host<-call: name, unchecked ... -> unchecked
    *Inherits description from*: host->call

    @see host->call
    @see class get_method
    @see host<-get_catch_all

- host<-catch_all: name, any|host_data ... -> any
    *Inherits description from*: host->catch_all

- host<-message: -> message
    Return the head of the message chain when available.  Otherwise process
    user-events using `display ->dispatch` until there is a message in
    <-messages and then return the head.

    @see host-messages

