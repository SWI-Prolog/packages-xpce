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

  # Once more against swipl-winconsole.ti, which is the fake termcap
  # swipl.exe links on a Windows console (win_ncurses.c) written out as
  # a terminal description.  It puts libedit through the same decisions
  # the Windows console does -- am without xenl, and an insert that
  # opens one cell per column because there is no IC and no insert mode
  # -- so this is where a Windows console bug can be caught without a
  # Windows console.  Both bugs found so far were of that kind.
  add_test(NAME "xpce:terminal_child_winconsole"
	   COMMAND ${PROG_SWIPL} -f none --no-packs
		   -s ${CMAKE_CURRENT_SOURCE_DIR}/test_terminal.pl
		   -g "test_terminal(child(winconsole))"
		   -t halt)
  list(APPEND XPCE_EXTRA_TESTS xpce:terminal_child_winconsole)

  # And once with a description that has ich1, which sends libedit down
  # its insert-mode path (\e[4h) rather than its insert-character one.
  add_test(NAME "xpce:terminal_child_insert_mode"
	   COMMAND ${PROG_SWIPL} -f none --no-packs
		   -s ${CMAKE_CURRENT_SOURCE_DIR}/test_terminal.pl
		   -g "test_terminal(child(linux))"
		   -t halt)
  list(APPEND XPCE_EXTRA_TESTS xpce:terminal_child_insert_mode)
endif()

# On Windows the same suite can be aimed at a real console:
# library(win_console) allocates one, puts swipl.exe on it and reads
# conhost's own screen buffer back.  That is the only run of the four
# that shows what a user would see rather than what a terminal of ours
# makes of it.
#
# It needs the foreign library from packages/libedit.  The program under
# test draws on a screen buffer of its own, so this can equally be run
# by hand from a toplevel on the console you are sitting on.
#
# This test does not run under Wine.

if(WIN32 AND NOT CMAKE_CROSSCOMPILING_EMULATOR)
  add_test(NAME "xpce:terminal_console"
	   COMMAND ${PROG_SWIPL}
		   -f none --no-packs
		   -s ${CMAKE_CURRENT_SOURCE_DIR}/test_terminal.pl
		   -g "test_terminal(console)"
		   -t halt)
  list(APPEND XPCE_EXTRA_TESTS xpce:terminal_console)
endif()

set_tests_properties(${XPCE_EXTRA_TESTS}
                     PROPERTIES ENVIRONMENT "${XPCE_TEST_ENV}")
