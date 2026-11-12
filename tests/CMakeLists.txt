################
# Additional xpce tests (plunit-based, standalone modules)

# Every test needs the headless display and the Defaults isolation set up
# in ../CMakeLists.txt.  xpce_test() registers a test and records it in
# XPCE_EXTRA_TESTS, which gets that environment at the end of this file,
# so that a new test cannot be added without it.

set(XPCE_EXTRA_TESTS)
macro(xpce_test name)
  test_libs(${name} OPTIONS ${XPCE_TEST_OPTIONS})
  list(APPEND XPCE_EXTRA_TESTS xpce:${name})
endmacro()

xpce_test(colour)
xpce_test(theme)
xpce_test(text_image)
xpce_test(editor_placeholder)
xpce_test(editor_isearch)
xpce_test(text_item)
xpce_test(filter_item)
xpce_test(tab_frame)
xpce_test(fixed_graphicals)
xpce_test(graphics_state)
xpce_test(toc)
xpce_test(type_syntax)
xpce_test(pending_exception)
xpce_test(goal_backtrace)
xpce_test(font_alias)
xpce_test(font)
xpce_test(visual)
xpce_test(preferences)
xpce_test(class_variable)
xpce_test(class_variable_editor)
xpce_test(theme_editor)
xpce_test(theme_file)
xpce_test(doc_render)
xpce_test(slice)
xpce_test(terminal_graphical)
xpce_test(man_search)
xpce_test(bool_item)
xpce_test(chain_list)
xpce_test(initialise_free)
xpce_test(cursor)
xpce_test(cursor_item)
xpce_test(help_message)
xpce_test(menu)
xpce_test(accelerators)
xpce_test(password_item)
xpce_test(keyboard_focus)
xpce_test(native_keys)
xpce_test(button)
xpce_test(dialog_group)
xpce_test(focus_border)
xpce_test(scroll_bar)
xpce_test(list_browser)
xpce_test(shadow)
xpce_test(cairo_render)
xpce_test(path_segments)
xpce_test(markup_text)
xpce_test(socket)
xpce_test(emacs_chrome_server)
xpce_test(epilog_links)
xpce_test(terminal_limits)
xpce_test(pce_format)
xpce_test(canvas)
xpce_test(graphics_demos)
xpce_test(frame_position)
xpce_test(slider)
xpce_test(text_cursor)
xpce_test(text_style)
xpce_test(style_item)
xpce_test(pane_frame)
xpce_test(pane_layouts)
xpce_test(barchart)
xpce_test(hierarchy_chart)
xpce_test(plot_axis)
xpce_test(memory_viewer)
xpce_test(idg_viewer)
xpce_test(xdot)
xpce_test(sdg_viewer)
xpce_test(bookmark_store)
xpce_test(bookmarks)
xpce_test(key_binding_browser)
xpce_test(class_template)
xpce_test(class_doc)
xpce_test(emacs_split)
xpce_test(event_clone)
xpce_test(subpixel_events)
xpce_test(input_device)
xpce_test(smooth_scroll)
xpce_test(sdl_input)
xpce_test(gamepad)
xpce_test(sound)
xpce_test(pce_open)
xpce_test(file_item)
xpce_test(event_viewer)
xpce_test(animation)
xpce_test(image_save)
xpce_test(emacs_init)
xpce_test(predicate_info)
xpce_test(emacs_lsp)
xpce_test(lsp_offset)
if(MULTI_THREADED)		# library(json_rpc_client) needs threads
  xpce_test(emacs_lsp_editor)
endif()
xpce_test(syntax_table)
xpce_test(emacs_c_mode)
xpce_test(camera)
xpce_test(sdl_texture)

# The video tests wait for playback in real time: some 40 seconds in
# one process.  A process per unit, read from test_video.pl, lets ctest
# run them in parallel.

file(STRINGS ${CMAKE_CURRENT_SOURCE_DIR}/test_video.pl video_unit_facts
     REGEX "^video_test_unit\\(")
foreach(fact ${video_unit_facts})
  string(REGEX REPLACE "^video_test_unit\\(([a-z_]+)\\)\\." "\\1"
         unit "${fact}")
  add_test(NAME "xpce:video.${unit}"
           COMMAND ${PROG_SWIPL} -f none --no-packs --on-error=status
                   ${XPCE_TEST_OPTIONS}
                   -s ${CMAKE_CURRENT_SOURCE_DIR}/test_video.pl
                   -g "test_video(${unit})"
                   -t halt)
  list(APPEND XPCE_VIDEO_TESTS "xpce:video.${unit}")
endforeach()

# test_libs() also records the test for an installed tree, which runs
# test_video/0 as a whole.  The ctest it adds would repeat the units.

if(INSTALL_TESTS)
  test_libs(video OPTIONS ${XPCE_TEST_OPTIONS})
  set_tests_properties(xpce:video PROPERTIES DISABLED TRUE)
endif()

xpce_test(clipboard_media)
xpce_test(display_info)
xpce_test(tray_icon)
xpce_test(prolog_mode)
if(EPILOG)
xpce_test(epilog_tabs)
xpce_test(mixed_panes)
xpce_test(undefined)
xpce_test(tool_panes)
endif()
xpce_test(no_display)
xpce_test(display_function)
xpce_test(display_removed)
xpce_test(number_conversion)
xpce_test(pce_date)
xpce_test(regex)
xpce_test(refman_signatures)
xpce_test(emacs_grep)
xpce_test(object_reference)
xpce_test(popup_event)
xpce_test(scrollbar)
xpce_test(terminal_osc)
xpce_test(terminal_client)
xpce_test(terminal_bce)
xpce_test(terminal_margin)
xpce_test(terminal_palette)
xpce_test(terminal_glyphs)
xpce_test(terminal_width)
xpce_test(terminal_rewrap)
xpce_test(terminal_rows)
xpce_test(redraw_reentrance)
xpce_test(profiler)

list(APPEND XPCE_EXTRA_TESTS ${XPCE_VIDEO_TESTS})

# Gamepad input is only delivered while one of our windows has the
# keyboard focus.  Where the tests use a real display, as on Windows and
# Wine, the windows of tests running in parallel take it away.

set_tests_properties(xpce:gamepad PROPERTIES RUN_SERIAL TRUE)

# process_encoding checks the bytes on the wire using od and iconv

if(NOT WIN32)
  xpce_test(process_exit)
  find_program(PROG_OD od)
  find_program(PROG_ICONV iconv)
  if(PROG_OD AND PROG_ICONV)
    xpce_test(process_encoding)
  endif()
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
