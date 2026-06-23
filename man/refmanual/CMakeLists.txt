# xpce reference manual built from per-class .md sources.
#
# Pipeline:
#   1. xpce-refman-tex  .md  -> .tex  via gen_refman.pl
#                                     (uses pldoc_xpce LaTeX backend)
#   1b.                  predicates.plx -> predicates.tex via doc2tex.pl
#   2. xpce-refman-html .tex -> .html via ltx2htm
#   3. xpce-refman-pdf  .tex -> .pdf  via runtex --pdf  (lualatex)
#
# The .md files are authoritative and live in this directory under
# md/. The .doc / export_md.pl exporter that originally produced them
# was retired in Phase 8. Outputs live in ${CMAKE_CURRENT_BINARY_DIR};
# nothing is written back into the source tree.
#
# Top-level target: xpce-refman  builds html (and pdf if PDF docs are
# enabled).
#
# Test: xpce-refman-check  re-parses every .md through PlDoc as a
# regression guard. Wired into ctest via the standard "test_libs"
# machinery so it runs under `ninja test`.

set(REFMAN_SRC_DIR ${CMAKE_CURRENT_SOURCE_DIR})
set(REFMAN_BIN_DIR ${CMAKE_CURRENT_BINARY_DIR})
set(REFMAN_MD_DIR  ${CMAKE_CURRENT_SOURCE_DIR}/md)
set(REFMAN_TEX_DIR ${REFMAN_BIN_DIR}/tex)
set(REFMAN_HTML_DIR ${REFMAN_BIN_DIR}/html)

set(REFMAN_GENERATOR ${CMAKE_CURRENT_SOURCE_DIR}/gen_refman.pl)
set(REFMAN_DRIVER    ${CMAKE_CURRENT_SOURCE_DIR}/xpcerefman.plx)
set(REFMAN_PLDOC_XPCE ${CMAKE_CURRENT_SOURCE_DIR}/../../prolog/lib/pldoc_xpce.pl)
set(REFMAN_PREDS_PLX ${CMAKE_CURRENT_SOURCE_DIR}/predicates.plx)
set(REFMAN_DOC2TEX   ${CMAKE_SOURCE_DIR}/man/doc2tex.pl)

# Stamp gates Stage 1 .md -> .tex without enumerating the 192 .md /
# .tex files in CMake. The .md inputs are picked up via a
# CONFIGURE_DEPENDS glob so editing a .md (or dropping a new one in)
# re-fires the generator on the next ninja run.
set(REFMAN_TEX_STAMP ${REFMAN_BIN_DIR}/tex.stamp)
file(GLOB REFMAN_MD_FILES CONFIGURE_DEPENDS ${REFMAN_MD_DIR}/*.md)

# Stage 1: .md -> .tex
add_custom_command(
    OUTPUT  ${REFMAN_TEX_STAMP}
    COMMAND ${CMAKE_COMMAND} -E make_directory ${REFMAN_TEX_DIR}
    COMMAND ${PROG_SWIPL} --home=${SWIPL_BUILD_HOME} -f none --no-packs
	    -DSDL_VIDEODRIVER=dummy
            ${REFMAN_GENERATOR} -- ${REFMAN_MD_DIR} ${REFMAN_TEX_DIR}
    COMMAND ${CMAKE_COMMAND} -E copy
            ${REFMAN_DRIVER} ${REFMAN_TEX_DIR}/xpcerefman.tex
    COMMAND ${CMAKE_COMMAND} -E touch ${REFMAN_TEX_STAMP}
    DEPENDS core prolog_home plugin_pl2xpce
            ${REFMAN_GENERATOR}
            ${REFMAN_DRIVER}
            ${REFMAN_PLDOC_XPCE}
            ${REFMAN_MD_FILES}
    VERBATIM
    COMMENT "-- Generating refman .tex from .md")

# Stage 1b: predicates.plx -> predicates.tex via the shared doc2tex.pl
# preprocessor (handles Name/Arity -> \predref, code blocks, etc.). The
# --xpce flag turns on the extra recogniser for Class->sel / Class<-sel
# / Class<->sel / Class.sel patterns.
add_custom_command(
    OUTPUT  ${REFMAN_TEX_DIR}/predicates.tex
    COMMAND ${CMAKE_COMMAND} -E make_directory ${REFMAN_TEX_DIR}
    COMMAND ${PROG_SWIPL} --home=${SWIPL_BUILD_HOME} -f none --no-packs
            ${REFMAN_DOC2TEX} --
            --xpce ${REFMAN_PREDS_PLX} ${REFMAN_TEX_DIR}/predicates.tex
    DEPENDS core prolog_home
            ${REFMAN_PREDS_PLX}
            ${REFMAN_DOC2TEX}
    VERBATIM
    COMMENT "-- doc2tex: predicates.plx -> predicates.tex")

# Stage 2: .tex -> .html via ltx2htm
add_custom_command(
    OUTPUT  ${REFMAN_TEX_DIR}/XPCERefMan/index.html
    COMMAND ${PROG_SWIPL} --home=${SWIPL_BUILD_HOME} -f none
            -s ${SWIPL_BUILD_LIBRARY}/ext/ltx2htm/latex2html.pl
            -t halt
            -- --quiet xpcerefman.tex
    WORKING_DIRECTORY ${REFMAN_TEX_DIR}
    DEPENDS core prolog_home latex2html
            ${REFMAN_TEX_STAMP}
            ${REFMAN_TEX_DIR}/predicates.tex
    VERBATIM
    COMMENT "-- Rendering refman HTML via ltx2htm")

# Stage 3b: copy generated HTML into ${SWIPL_BUILD_HOME}/xpce/man/refmanual/
# so save_man_index/0 (see packages/pldoc/man_index.pl, file_search_path
# swi_man_xpce -> swi('xpce/man/refmanual')) picks the entries up and
# merges them into manindex.db.
set(REFMAN_HOME_DIR ${SWIPL_BUILD_HOME}/xpce/man/refmanual)
set(REFMAN_HOME_STAMP ${REFMAN_BIN_DIR}/home.stamp)
add_custom_command(
    OUTPUT  ${REFMAN_HOME_STAMP}
    COMMAND ${CMAKE_COMMAND} -E rm -rf ${REFMAN_HOME_DIR}
    COMMAND ${CMAKE_COMMAND} -E copy_directory
            ${REFMAN_TEX_DIR}/XPCERefMan ${REFMAN_HOME_DIR}
    COMMAND ${CMAKE_COMMAND} -E touch ${REFMAN_HOME_STAMP}
    DEPENDS ${REFMAN_TEX_DIR}/XPCERefMan/index.html
    COMMENT "-- Staging xpce refman HTML for manindex.db")

add_custom_target(xpce-refman-tex  DEPENDS ${REFMAN_TEX_STAMP}
                                          ${REFMAN_TEX_DIR}/predicates.tex)
add_custom_target(xpce-refman-html DEPENDS ${REFMAN_HOME_STAMP})

# Install the staged HTML under the xpce home tree (matches the
# swi_man_xpce -> swi('xpce/man/refmanual') file_search_path the
# manual indexer uses at runtime).
if(INSTALL_DOCUMENTATION)
  install(DIRECTORY ${REFMAN_HOME_DIR}
          DESTINATION ${SWIPL_INSTALL_PREFIX}/xpce/man
          COMPONENT Documentation
          PATTERN "*.tex" EXCLUDE)
endif()

# Stage 4 (optional): .tex -> .pdf via lualatex
if(BUILD_PDF_DOCUMENTATION)
  set(RUNTEX_BIN ${CMAKE_SOURCE_DIR}/man/runtex)
  add_custom_command(
      OUTPUT  ${REFMAN_TEX_DIR}/xpcerefman.pdf
      COMMAND env "TEXINPUTS=${CMAKE_SOURCE_DIR}/packages/xpce/TeX:${CMAKE_SOURCE_DIR}/packages/pldoc:${CMAKE_SOURCE_DIR}/man/lib:"
              ${RUNTEX_BIN} --pdf xpcerefman
      WORKING_DIRECTORY ${REFMAN_TEX_DIR}
      DEPENDS ${REFMAN_TEX_STAMP}
      COMMENT "-- Building refman PDF via lualatex")
  add_custom_target(xpce-refman-pdf
                    DEPENDS ${REFMAN_TEX_DIR}/xpcerefman.pdf)
endif()

# Aggregate target
add_custom_target(xpce-refman)
add_dependencies(xpce-refman xpce-refman-html)
if(BUILD_PDF_DOCUMENTATION)
  add_dependencies(xpce-refman xpce-refman-pdf)
endif()

# Wire HTML build into the top-level doc.html aggregate so
# save_man_index/0 sees the xpce <dt class="pubdef"> entries when
# building ${SWIPL_BUILD_HOME}/doc/manindex.db.
if(TARGET doc.html)
  add_dependencies(doc.html xpce-refman-html)
endif()

# Regression test: parse every .md through PlDoc.
add_test(
    NAME    xpce:refman_check
    COMMAND ${PROG_SWIPL} -f none --no-packs
            ${REFMAN_GENERATOR} -- --check ${REFMAN_MD_DIR})
