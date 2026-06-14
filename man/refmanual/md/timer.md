# class timer {#class-timer}

A timer is an object that -when the system is dispatching messages-
generates an event at regular intervals.  Timers are used to implement
simulation, blinking (caret) or to wake-up the user under some
(time-based) conditions.

The interval of a timer is specified as a real-number.  The actual
accuracy depends on the timing capabilities offered by the
X-implementation on which PCE has been build.

When executing the message, no arguments are forwarded.  @receiver is
bound to the timer itself.  See ->execute.

NOTE: When multiple objects are `animated` at a similar speed
performance will be much better if a single timer is used.  The code
below provides a skeleton:

	:- pce_global(@stepping, chain).

	start_stepping :-
		send(new(@stepper,
				 timer(1, message(@stepper, for_all,
						 		  step))),
		     start).

	step(Obj) :-
		send(@stepping, append, Obj).

@see class date


## Instance variables {#class-timer-instvars}

- timer<-interval: real
    Interval in seconds.  Note that

    - The timer is only executing if PCE is in its idle loop
    	  (see `display ->dispatch`).

    - Granularity depends on the underlying implementation.
    	  For the X11 version, consult the manual page for
    	  select().

    The interval may be changed when the timer is running.  In this case
    it will first fire <-interval seconds from the time is was changed.

- timer<->message: code*
    Code executed each time.  See ->execute for details.

- timer<->service: bool
    If @on, execution cannot be debugged.  See also `application ->kind`.

- timer<-status: {idle,repeat,once}
    Status of the timer.  Its values:

    	| idle   | Timer is not running                  |
    	| once   | After ->execute'ing once it will stop |
    	| repeat | Timer will repeat ->execute'ing       |


## Send methods {#class-timer-send}

- timer->delay
    Delay for <-interval.  XPCE processes events while execution is
    suspended.

- timer->execute
    Execute the timers <-message with the following arguments:

    	@receiver		The timer itself.

- timer->initialise: interval=real, message=[code]*
    Create a timer object from its interval and the message to execute.
    After creation, the timer is idle.  Use ->start or ->status to start the
    timer.

- timer->start: how=[{repeat,once}]
    Equivalent to ->status: [repeat].

- timer->stop
    Equivalent to ->status: idle.

- timer->running: running=bool
    Start/stop the timer in repeat mode.

    	| @on, ->start | is equivalent to ->status: repeat |
    	| @off, ->stop | is equivalent to ->status: idle.  |
