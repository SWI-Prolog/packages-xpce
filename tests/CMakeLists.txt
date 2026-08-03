################
# Additional xpce tests (plunit-based, standalone modules)

test_libs(number_conversion)
test_libs(object_reference)

set(XPCE_EXTRA_TESTS xpce:number_conversion
                     xpce:object_reference)

# The terminal suite is registered by hand rather than with
# test_libs(): it must run without --on-error=status.  Tearing a
# terminal down closes the master end of its pty, and the terminal's
# own Prolog thread reports the resulting read error on its way out.
# That is not a test failure, and it comes from a thread no test can
# reach.  A failing test still fails the run: run_tests/1 fails, so
# the -g goal does.
#
# It runs twice.  Once against the epilog terminal, and once against a
# child process on that terminal's pty -- the only way to reach
# libedit's ordinary termcap paths, as the epilog terminal always runs
# it with the EPILOG flag.  The child is placed on the terminal by
# shell/1, which needs a pty, so that run is Unix only.

add_test(NAME "xpce:terminal"
	 COMMAND ${PROG_SWIPL} -f none --no-packs
		 -s ${CMAKE_CURRENT_SOURCE_DIR}/test_terminal.pl
		 -g "test_terminal"
		 -t halt)
list(APPEND XPCE_EXTRA_TESTS xpce:terminal)

if(NOT WIN32)
  add_test(NAME "xpce:terminal_child"
	   COMMAND ${PROG_SWIPL} -f none --no-packs
		   -s ${CMAKE_CURRENT_SOURCE_DIR}/test_terminal.pl
		   -g "test_terminal(child(xterm))"
		   -t halt)
  list(APPEND XPCE_EXTRA_TESTS xpce:terminal_child)
endif()

set_tests_properties(${XPCE_EXTRA_TESTS}
                     PROPERTIES ENVIRONMENT "${XPCE_TEST_ENV}")
