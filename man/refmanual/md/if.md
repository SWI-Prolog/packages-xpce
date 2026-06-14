# class if {#class-if}

An if object is a conditional statement.  When executed, it will
execute the <-condition.  On success, it will execute the <-then branch,
otherwise it will execute the <-else branch.  An empty <-then or <-else
(@nil) simply succeeds.

A special trick is to use `if(<Statement>)`.  This will succeed
regardless of the success or failure of <Statement>.

The following example exploits the if object to remove all graphicals
with `area <-measure: 0` from _Device_:

	send(Device?graphicals, for_all,
		 if(@arg1?area?measure == 0,
		    message(@arg1, free)))

Class when implements the function equivalent of class if.

@see class when


## Instance variables {#class-if-instvars}

- if<->condition: code
    Condition to be tested.  Note that, if this is a function object, it
    tests whether the function returns a value.  See also `code ->execute`.

- if<->else: code*
    Branch of the conditional statement.  <-then is executed on successful
    execution of <-condition, <-else otherwise.  If the statement is @nil,
    it executes successfully without side-effects.  If the statement is a
    function, it will be executed as defined by `code ->execute`.

- if<->then: code*
    *Inherits description from*: if-else

