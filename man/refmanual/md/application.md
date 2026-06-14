# class application {#class-application}

An application object defines a collection of cooperating frame
objects.  Applications are new as of XPCE version 4.9, and
therefore do not yet have a stable definition.  It is however
assumed that the current implementation will only be enhanced
with new, compatible, features.

Applications are designed to deal with the following issues:

- Find frames belonging to the application
	Using the <-member method, frames that constitute an
	application can find each other.  See also
	`frame<->name` and `graphical<-application`.

- Define the scope of `modal` frames
	Frames may be defined modal over the other frames in
	the application.  See `frame ->modal`.

- Define general application properties
	XPCE itself uses applications of <-kind: service to
	realise the programming environment, and avoid the
	environment from tracing and debugging itself.

Normally, applications are created explicitely, often as global
named objects.  Frames are attached to it using the application
initialisation argument application:

	:- pce_global(@my_application, make_my_application).

	make_my_application(App) :-
		new(App, application(mine)),
		...

		...
		new(F, frame('My title',
					 application := @my_application)),
		...


## Instance variables {#class-application-instvars}

- application<->kind: {user,service}
    If `user`, (default), the methods and predicates called as a
    result of event-handling by windows and frames of the
    application can be debugged.  If `service`, all Prolog and
    XPCE activity will be run in `system` mode to avoid debugging
    the XPCE environments tools, while trying to debug the user
    application.

    Timer objects are not covered by this flag as they do not belong
    to an application.  They provide the `timer->service` method to
    turn them into a service.

- application<-members: chain
    Chain holding member frames.  This chain is maintained by
    ->delete and ->append.

- application-modal: chain
    Frame that currently acts as modal window for the application.
    This implies that no window in another frame object can receive
    events.  See also `frame->modal`.

- application<->name: name
    Identifier name of the application. Currently not used.


## Send methods {#class-application-send}

- application->append: frame
    Add frame to the application.  See also `frame ->application`,
    and the `application` argument of `frame ->initialise`.

- application->delete: frame
    Remove frame from the application.  Associating a frame with
    another application object will automatically delete it from the
    application it was a <-member of.  This method may be redefined
    to keep track of the applications, but *must* call the
    super-implementation.

- application->initialise: name
    Create an application with the given name.  The application is
    added to the chain object @applications.

- application->reset
    Implementation of `visual <-reset`.  Removes any possible
    <-modal frame object.

- application->unlink
    Destroying an application will invoke `frame ->destroy` on all
    <-members.


## Get methods {#class-application-get}

- application<-contains: -> chain
    Implementation of `visual <-contains`, returning <-members.

- application<-member: name -> frame
    Find a member frame object with given `frame <-name`.

