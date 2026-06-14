# class binary_condition {#class-binary_condition}

Class binary_condition is a super class for the various arithmetic
condition statements:

	| <  | Test <-left to be less than <-right                |
	| =< | Test <-left to be less than or equal to <-right    |
	| =  | Test <-left to be equal to <-right                 |
	| >= | Test <-left to be greater than or equal to <-right |
	| >  | Test <-left to be greater than <-right             |

These code objects are almost exclusively used in conditions for if
objects and friends.   When executed, the subclasses of binary_condition
evaluates <-left and <-right to a numeric value and compares the two
according to the class.

The type-check of a binary_condition is `expression`.   Type expression
is a disjunction of the following types:

	| int      | XPCE integers                                 |
	| number   | number object                                 |
	| real     | real object                                   |
	| function | function object (including @arg1 and class ?) |

The following example defines that the pointer should be in the positive
part of the coordinate system of the receiving graphical:

	and(@event?x > 0, @event?y > 0).

@see chain->sort


## Instance variables {#class-binary_condition-instvars}

- binary_condition<->left: expression
    Both operants of the condition.

- binary_condition<->right: expression
    *Inherits description from*: binary_condition-left


## Send methods {#class-binary_condition-send}

- binary_condition->initialise: left=expression, right=expression
    Create the condition from two expressions.  There is no use in creating
    instances of this class as there is no execution defined for it.

