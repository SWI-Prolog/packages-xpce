################
# Additional xpce tests (plunit-based, standalone modules)

test_libs(number_conversion)
test_libs(object_reference)

set(XPCE_EXTRA_TESTS xpce:number_conversion
                     xpce:object_reference)

# The terminal suite drives a terminal and reads the screen back, so it
# spends its time waiting for the terminal rather than computing: about a
# minute against the four backends, where the rest of the xpce tests
# together take five seconds.  It is off unless asked for.
#
# Do ask for it when touching the terminal, libedit or shell/1: it is
# what catches the bugs those have, and several of them were found by no
# other means.

option(XPCE_TERMINAL_TESTS
       "Run the terminal test suites (slow: drives a terminal in real time)"
       OFF)

if(XPCE_TERMINAL_TESTS)

# The units are read from the suite itself, so that adding one there is
# enough to have the build run it.

file(STRINGS ${CMAKE_CURRENT_SOURCE_DIR}/test_terminal.pl terminal_unit_facts
     REGEX "^terminal_test_unit\\(")
set(TERMINAL_UNITS)
foreach(fact ${terminal_unit_facts})
  string(REGEX REPLACE "^terminal_test_unit\\(([a-z_]+)\\)\\." "\\1"
         unit "${fact}")
  list(APPEND TERMINAL_UNITS ${unit})
endforeach()

# A process per unit.  The units are independent of one another but xpce
# is not: it has a single thread that may touch an object, so they cannot
# share one process, and running them in threads corrupts the object
# store.  Separate processes also let ctest run them in parallel, which
# is the whole of the difference between a minute and a few seconds.
#
# Registered by hand rather than with test_libs(): the suite must run
# without --on-error=status.  Tearing a terminal down closes the master
# end of its pty, and the terminal's own Prolog thread reports the
# resulting read error on its way out.  That is not a test failure, and
# it comes from a thread no test can reach.  A failing test still fails
# the run: run_tests/1 fails, so the -g goal does.

function(add_terminal_tests prefix backend)
  set(added)
  foreach(unit ${TERMINAL_UNITS})
    add_test(NAME "${prefix}.${unit}"
             COMMAND ${PROG_SWIPL} -f none --no-packs
                     -s ${CMAKE_CURRENT_SOURCE_DIR}/test_terminal.pl
                     -g "test_terminal(${backend},${unit})"
                     -t halt)
    list(APPEND added "${prefix}.${unit}")
  endforeach()
  set(XPCE_EXTRA_TESTS ${XPCE_EXTRA_TESTS} ${added} PARENT_SCOPE)
endfunction()

add_terminal_tests("xpce:terminal" "epilog")

# And against a child process on the terminal's pty, which is the only
# way to reach libedit's ordinary termcap paths: the epilog terminal
# always runs it with the EPILOG flag.  The child is placed on the
# terminal by shell/1, which needs a pty, so these are Unix only.
#
# Once with swipl-winconsole.ti, the fake termcap swipl.exe links on a
# Windows console (win_ncurses.c) written out as a terminal description.
# It puts libedit through the same decisions the Windows console does --
# am without xenl, and an insert that opens one cell per column because
# there is no IC and no insert mode -- so a Windows console bug can be
# caught without a Windows console.  Both found so far were of that kind.
# And once with a description that has ich1, which sends libedit down its
# insert-mode path (\e[4h) rather than its insert-character one.

if(NOT WIN32)
  add_terminal_tests("xpce:terminal_child"              "child(xterm)")
  add_terminal_tests("xpce:terminal_child_winconsole"   "child(winconsole)")
  add_terminal_tests("xpce:terminal_child_insert_mode"  "child(linux)")
endif()

# On Windows the same suite can be aimed at a real console:
# library(win_console) allocates one, puts swipl.exe on it and reads
# conhost's own screen buffer back.  That is the only one of these that
# shows what a user would see rather than what a terminal of ours makes
# of it.  It needs library(win_console) from packages/libedit, which is
# built only with LIBEDIT_DEBUG.  It does not run under Wine.

if(WIN32 AND LIBEDIT_DEBUG AND NOT CMAKE_CROSSCOMPILING_EMULATOR)
  add_terminal_tests("xpce:terminal_console" "console")
endif()

endif(XPCE_TERMINAL_TESTS)

set_tests_properties(${XPCE_EXTRA_TESTS}
                     PROPERTIES ENVIRONMENT "${XPCE_TEST_ENV}")
