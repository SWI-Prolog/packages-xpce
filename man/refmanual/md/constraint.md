# class constraint {#class-constraint}

A constraint is a relation between two objects that must be maintained.
A constraint is defined by a relation object.  Class relation is a
super-class for all predefined relations.

Constraints deal properly with cycles.  Contradictory constraints lead
to unpredictable results.

Constraints are commonly used to maintain relations between graphicals.
They can also be used to maintain a relation between dialog objects and
`model` objects.  The example below illustrates both usages.

	:- pce_global(@center, new(identity(center))).

	constraint_demo :-
		new(P, picture('Constraint Demo')),
		send(new(D, dialog), below, P),

		send(P, display, new(B1, box(100,100))),
		send(P, display, new(B2, box(50,50))),
		send_list([B1, B2], recogniser,
			  handler_group(new(resize_gesture),
					new(move_gesture))),
		new(_, constraint(B1, B2, @center)),

		send(D, append, new(S1, slider(center_x, 0, 500, 200))),
		send(D, append, new(S2, slider(center_y, 0, 500, 200))),
		send_list([S1, S2], drag, @on),
		send(D, append, button(quit, message(D, destroy))),

		new(_, constraint(S1, B1,
						  identity(selection, center_x))),
		new(_, constraint(S2, B2,
						  identity(selection, center_y))),

		send(D, open).

**Bugs**:

Currently, the constraint mechanism implies that everytime a
send-message is accepted by a constrainted object, the associated
relation is evaluated.  This is unnecessary expensive and limits the
application area of constraints to exist only between objects that do
not receive time-critical messages.

It is not possible to create a relation with ITSELF.  The following does
- not* work:
	make_square(S, W) :-
		new(S, box(W,W)),
		new(_, constraint(S, S, indentity(width, height))).

@see class connection
@see class hyper
@see class interceptor
@see class relation
@see object<-all_constraints


## Instance variables {#class-constraint-instvars}

- constraint<-from: object
    Object at the specified side of the constraint.  Once the constraint is
    established it makes no difference which object is at which side.  The
    constraint is established by making <-to consistent with <-from.

    The constraint may be found though `object <-all_constraints` from
    either constrained object.

- constraint-locked: {none,forwards,front,backwards,back}
    This variable serves two purposes: avoid loops and implement constraints
    that only propagate in one direction.  Its values are:

    	| forwards  | Only propagate <-from to <-to  |
    	| backwards | Only propagate <-to to <-from  |
    	| none      | Propagate both ways            |
    	| front     | <-from has changed. Avoid loop |
    	| back      | <-to has changed. Avoid loop   |

- constraint<-relation: relation
    The relation that maintains the constraint.  This object will be send
    the following messages:

    	| `relation ->create: From, To`    | Upon establishing. |
    	| `relation ->forwards: From, To`  | <-from has changed |
    	| `relation ->backwards: From, To` | <-to has changed   |

- constraint<-to: object
    *Inherits description from*: constraint-from


## Send methods {#class-constraint-send}

- constraint->initialise: from=object, to=object, relation=relation, propagate=[{forwards,backwards}]
    Create a constraint object from both objects to be constrained (<-from
    and <-to), the <-relation describing the constraint and optionally
    whether the constraint should propagate changes in both or only one
    direction.  By default a constraint propagates changes either way.  With
    propagate equals `forwards` only changes to <-from are propagated to
    <-to and with propagate equals `backwards` the other way around.

