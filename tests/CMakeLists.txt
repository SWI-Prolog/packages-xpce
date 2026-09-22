################
# Additional xpce tests (plunit-based, standalone modules)

test_libs(colour               OPTIONS ${XPCE_TEST_OPTIONS})
test_libs(text_image           OPTIONS ${XPCE_TEST_OPTIONS})
test_libs(editor_placeholder   OPTIONS ${XPCE_TEST_OPTIONS})
test_libs(text_item            OPTIONS ${XPCE_TEST_OPTIONS})
test_libs(tab_frame            OPTIONS ${XPCE_TEST_OPTIONS})
test_libs(fixed_graphicals     OPTIONS ${XPCE_TEST_OPTIONS})
test_libs(font_alias           OPTIONS ${XPCE_TEST_OPTIONS})
test_libs(pane_frame           OPTIONS ${XPCE_TEST_OPTIONS})
test_libs(pane_layouts         OPTIONS ${XPCE_TEST_OPTIONS})
test_libs(barchart             OPTIONS ${XPCE_TEST_OPTIONS})
test_libs(bookmark_store       OPTIONS ${XPCE_TEST_OPTIONS})
test_libs(bookmarks            OPTIONS ${XPCE_TEST_OPTIONS})
test_libs(class_template       OPTIONS ${XPCE_TEST_OPTIONS})
test_libs(emacs_split          OPTIONS ${XPCE_TEST_OPTIONS})
test_libs(event_clone          OPTIONS ${XPCE_TEST_OPTIONS})
test_libs(prolog_mode          OPTIONS ${XPCE_TEST_OPTIONS})
if(EPILOG)
test_libs(epilog_tabs          OPTIONS ${XPCE_TEST_OPTIONS})
test_libs(mixed_panes          OPTIONS ${XPCE_TEST_OPTIONS})
test_libs(undefined            OPTIONS ${XPCE_TEST_OPTIONS})
test_libs(tool_panes           OPTIONS ${XPCE_TEST_OPTIONS})
endif()
test_libs(no_display           OPTIONS ${XPCE_TEST_OPTIONS})
test_libs(number_conversion    OPTIONS ${XPCE_TEST_OPTIONS})
test_libs(object_reference     OPTIONS ${XPCE_TEST_OPTIONS})
test_libs(popup_event          OPTIONS ${XPCE_TEST_OPTIONS})
test_libs(scrollbar            OPTIONS ${XPCE_TEST_OPTIONS})
test_libs(terminal_osc         OPTIONS ${XPCE_TEST_OPTIONS})
test_libs(terminal_client      OPTIONS ${XPCE_TEST_OPTIONS})
test_libs(terminal_bce         OPTIONS ${XPCE_TEST_OPTIONS})
test_libs(terminal_margin      OPTIONS ${XPCE_TEST_OPTIONS})
test_libs(terminal_palette     OPTIONS ${XPCE_TEST_OPTIONS})
test_libs(terminal_glyphs      OPTIONS ${XPCE_TEST_OPTIONS})
test_libs(terminal_width       OPTIONS ${XPCE_TEST_OPTIONS})
test_libs(terminal_rewrap      OPTIONS ${XPCE_TEST_OPTIONS})
test_libs(redraw_reentrance    OPTIONS ${XPCE_TEST_OPTIONS})

set(XPCE_EXTRA_TESTS xpce:colour
                     xpce:text_image
                     xpce:editor_placeholder
                     xpce:text_item
                     xpce:tab_frame
                     xpce:fixed_graphicals
                     xpce:font_alias
                     xpce:pane_frame
                     xpce:pane_layouts
                     xpce:barchart
                     xpce:bookmark_store
                     xpce:bookmarks
                     xpce:class_template
                     xpce:emacs_split
                     xpce:event_clone
                     xpce:prolog_mode
                     xpce:no_display
                     xpce:number_conversion
                     xpce:object_reference
                     xpce:popup_event
                     xpce:scrollbar
                     xpce:terminal_osc
                     xpce:terminal_client
                     xpce:terminal_bce
                     xpce:terminal_margin
                     xpce:terminal_palette
                     xpce:terminal_glyphs
                     xpce:terminal_width
                     xpce:terminal_rewrap
                     xpce:redraw_reentrance)

if(EPILOG)
  list(APPEND XPCE_EXTRA_TESTS xpce:epilog_tabs xpce:mixed_panes
                               xpce:undefined xpce:tool_panes)
endif()

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
                     ${XPCE_TEST_OPTIONS}
                     -s ${CMAKE_CURRENT_SOURCE_DIR}/test_terminal.pl
                     -g "test_terminal(${backend},${unit})"
                     -t halt)
    list(APPEND added "${prefix}.${unit}")
  endforeach()
  set(XPCE_EXTRA_TESTS ${XPCE_EXTRA_TESTS} ${added} PARENT_SCOPE)
endfunction()

add_terminal_tests("xpce:terminal" "epilog")

# And once with the resize signal held back.  A window resize raises
# SIGWINCH on the client thread, but that is not what libedit may rely
# on: a Windows console raises nothing at all, and a signal can be lost
# -- one that arrives while libedit is repainting for the previous one
# used to be cleared along with it.  What is left is the size on the pty,
# which read_char() polls before it hands a key to libedit.  Without the
# poll the three resize tests that press a key fail, which is what this
# used to do a run in five, from a lost signal, with nothing to say why.

add_test(NAME "xpce:terminal_no_sigwinch.terminal_resize"
         COMMAND ${PROG_SWIPL} -f none --no-packs
                 ${XPCE_TEST_OPTIONS}
                 -s ${CMAKE_CURRENT_SOURCE_DIR}/test_terminal.pl
                 -g "test_terminal(epilog,terminal_resize)"
                 -t halt)
set_tests_properties("xpce:terminal_no_sigwinch.terminal_resize"
                     PROPERTIES ENVIRONMENT
                     "${XPCE_TEST_ENV};EPILOG_NO_SIGWINCH=1")

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
