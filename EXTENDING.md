# Extending XPCE

You may want to add new classes to the  base system. If the class can be
expressed in terms of existing classes   or  the combination of existing
classes with features provided by the  host-language (Prolog, Lisp), the
best way to define the  new  functionality   is  to  use  the XPCE class
definition protocol defined for your host-language.

If you want to add new features  that   need  to be very fast or require
access to the operating system at a level   not  provided by XPCE or the
host-language, you must define the new class in  C. To be able to define
a new class (or method) in C, you  need the full source-tree of XPCE. It
is important to note that we DO   NOT  GUARANTEE THE INTERFACE TO REMAIN
STABLE. If you have additions, the best  is to write them in cooperation
with us, so we can add it to the base-system and maintain it.

## Steps to add a new C-class

  1) Determine a name for the class.  The class should be defined
     in a file with the same name, or a descriptive name if the
     classname consists of symbols rather than letters and digits.

     Determine the category the new file should be in and copy a
     simple class into the new file.  I often take adt/attribute.c.
     Let's assume our new class is called `rpc` (Remote Procedure
     Call).  Then this becomes:

     ```
     cp adt/attribute.c unx/rpc.c
     ```

     New files need the BSD-2 license header from `.fileheader`.

  2) Edit `unx/rpc.c`.  First change all `attribute` in the various
     capitalisations into `rpc` with the corresponding capitalisation.

     a) Define the class structure.  If the class is a
        subclass of class object, this becomes:

        ```
	NewClass(rpc)
          Name	function;	/* Function to be called */
          ...
	End;
	```

        If the class is not a subclass of object, the
        super-class should define a macro ``ABSTRACT_<classname>``
        to inherit the super attributes, and the definition
        will look like this

        ```
        NewClass(my_window)
	  ABSTRACT_WINDOW
	  Any		my_slot;
        End;
	```

        Types of attributes are XPCE types.  See h/types.h for
	type-names defined.

        You may add private C handles, but remember that each
	slot should have the size of a pointer (64 bits).

        The `NewClass()` definition may be in a file in `../h/`
	or be in the sourcefile of the class.  The first is
	only required you want to have direct access to the
	structure attributes from other classes.  The header
	for the `unx` directory is `h/unix.h`.

     b) Define the pointer type, which is a capitalised
        version of the structure name.  The definition may
	be in `.../h/types.h` or in the sourcefile itself.

     c) Define a global variable of type Class with name
        Class<CapitalisedClassName>.  The definition again may
	be in `.../h/types.h` or the local file.

     d) Update the class declaration tables at the end of the
        file.  `var_rpc[]` must have one `IV()` or `SV()` entry
	for each slot.  `SV()` adds a C function that is called
	to set the slot (use `IV_STORE` in the access flags).
	Non-XPCE slots should be tagged alien:<Ctype> for
	their type; `assign()` does not apply to them.  Methods
	are declared using `SM()` in `send_rpc[]` and `GM()` in
	`get_rpc[]`, using `T_<name>[]` arrays for the argument
	types if there is more than one argument.  Class variables
	go in `rc_rpc[]` (or `#define rc_rpc NULL`).

     e) Update `rpc_termnames[]` and the `ClassDecl()` (the
        number is the length of `rpc_termnames`), as well as
	the `initialiseRpc()` function.  `makeClassRpc()` just
	calls `declareClass()`.

     f) Define the real methods.  Send methods return `status`
        using `succeed` or `fail`.  Get methods return their
	result using `answer(x)` or `fail`.  See the various
	examples for details.

  3) Make the class known to the system:

     a) Add a declaration to ker/declarations.c:
        `{ NAME_rpc, NAME_object, makeClassRpc, &ClassRpc, "summary" }`
     b) Add the file(s) to `cmake/XPCESources.cmake`
     c) Add the makeClassRpc() prototype and other non-static
        functions to the local proto.h file.

  4) Make atoms known to the system.  `NAME_<atom>` constants are
     collected from the sources by `find_names`.  This only runs
     if `src/namedb.txt` changes, so bump its version number
     whenever you use a new `NAME_<atom>`.  The list of scanned
     files is determined when CMake is configured, so a new file
     requires re-running CMake (which happens automatically after
     editing `cmake/XPCESources.cmake`).

  5) Run `ninja` in the build directory.  Check the class
     definition with the online manual.


## Tips and Hints for writing C-classes

1. First just write the data definition and go through the steps above.
   verify the data-definition is ok (using the online manual), create
   an instance and run checkpce/0 to see if XPCE thinks the instance is
   fine.

2. Consider prototyping (part of) the class using Prolog-defined methods
   (pce_extend_class/pce_end_class).  The development cycle is much shorter!

3. Integers (`Int`) are currently the same as `Num`, which is a double
   that lost one mantissa bit for the _tag_.  `Int` is merely a `Num`
   that happens to be integer, just as in JavaScript.  To turn a C int
   into its XPCE equivalent, use `toInt(x)`.  The macro `valInt(x)`
   performs the reverse operation. Note that `valInt(valInt(x))`
   returns an undefined result. So does `toInt(toInt(x))`.  For
   C doubles, use `toNum(x)` and `valNum(x)`.

   Note that many graphical classes still use `Int`.  Most of the underlying
   Cairo drawing takes doubles, so use `Num` and `valNum(x)` instead for new
   code.

4. Assignment to an instance slot may **never** be done using the C structure
   assignment ``ptr->slot = value``.  Instead, one should use the `assign()`
   macro as below.  The `assign()` macro maintains reference counts for the
   garbage collector and allows for tracing the slot.

   ```
   assign(ptr, slot, value)
   ```

5. checkpce/0 and running the system in debugging mode will help you
   locating trouble.  The latter is achieved using the query below,
   which enables additional consistency checks and allows for tracing
   slot assignments.

   ```
   ?- debugpce.
   ```

6. If you need debug statements, the following macro helps:

   ```
   DEBUG(NAME_<topic>, <C-code>)
   ```

   you can activate the C-code there using

   ```
   ?- debugpce(<topic>).
   ```

   The macros pp(x) (pretty-print) prints any XPCE data structure in a human
   readable format.  An example could be:

   ```
   DEBUG(NAME_rpc, Cprintf("calling to %s\n", pp(dest)));
   ```

   where `dest` is a local variable that should hold XPCE data.  `pp(x)` is
   very careful and generally prints non-XPCE values hexadecimal, but
   occasionally crashes when passed non-XPCE values.

7. Use the standard C `assert()` macro.  The XPCE definition is changed a bit
   such that a failing assertion will generate a fatal error calling
   `sysPce()`.  Put a `gdb` breakpoint on this function if failing asserts
   need to be analysed using `gdb`.  Assertions are removed if compiled
   with `-DNOASSERT`.

8. Make the ``->unlink`` method fool proof: do not expect the instance to be
   in a consistent state and be prepared to be called more than once.

