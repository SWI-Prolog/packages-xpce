cmake_minimum_required(VERSION 2.8.12)
project(swipl-xpce)

include("../cmake/PrologPackage.cmake")

find_package(X11)
find_package(JPEG)

if(X11_FOUND AND JPEG_FOUND)

include(XPCESources)
find_package(Freetype)
find_package(FontConfig)

set(X11_LIBRARIES
    ${X11_Xpm_LIB}
    ${X11_Xinerama_LIB}
    ${X11_Xext_LIB}
    ${X11_Xt_LIB}
    ${X11_X11_LIB}
    ${X11_SM_LIB}
    ${X11_ICE_LIB}
    ${X11_Xft_LIB}
    ${FREETYPE_LIBRARIES}
    ${FONTCONFIG_LIBRARIES})

include(XPCEConfig)

configure_file(config.h.cmake config.h)

################
# Build pl2xpce shared object

# -DSWI is required for the interface only
if(UNIX)
  add_compile_options(-DHAVE_CONFIG_H -DSWI)
endif()

add_custom_command(
    OUTPUT ${CMAKE_CURRENT_SOURCE_DIR}/src/h/names.ih
	   ${CMAKE_CURRENT_SOURCE_DIR}/src/h/names.ic
    COMMAND find_names h/names.ic h/names.ih -- h/*.h ???/*.c
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/src
    COMMENT "Collecting NAME_ declarations")

add_custom_target(
    xpce_names
    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/src/h/names.ih)

add_executable(find_names src/find_names.c)

swipl_plugin(
    pl2xpce
    C_SOURCES ${XPCE_SOURCES} ${HOST_INTERFACE_SOURCES}
    C_LIBS ${X11_LIBRARIES} ${JPEG_LIBRARIES} ${JPEG_LIBRARIES}
    PL_LIBS)
add_dependencies(plugin_pl2xpce xpce_names)

target_include_directories(
    plugin_pl2xpce BEFORE PRIVATE
    ${CMAKE_CURRENT_BINARY_DIR}
    ${CMAKE_CURRENT_SOURCE_DIR}/src
    ${JPEG_INCLUDE_DIRS}
    ${X11_INCLUDE_DIR}
    ${FREETYPE_INCLUDE_DIRS}
    ${FONTCONFIG_INCLUDE_DIR})

################
# Install

# Install the data files
foreach(d ${XPCE_DATA_DIRS})
    string(REGEX REPLACE "/" "_" filevar ${d})
    prepend(files ${d} ${XPCE_DATA_${filevar}})
    install(FILES ${files}
	    DESTINATION ${SWIPL_INSTALL_PREFIX}/xpce/${d})
endforeach()

install(FILES Defaults Defaults.user
	DESTINATION ${SWIPL_INSTALL_PREFIX}/xpce)

# Install the link file
install(FILES swipl/swipl-rc
	DESTINATION ${SWIPL_INSTALL_PREFIX}
	RENAME swipl.rc)

################
# Create indexes
# This is currently done using install(CODE ...).  The advantage is that we pick only
# the installed libraries.  Some care must be taken that the generated files are
# processed correctly.

set(XPCE_PROLOG_LIB_DIR ${SWIPL_INSTALL_PREFIX}/xpce/prolog/lib)
set(XPCE_MANINDEX ${SWIPL_INSTALL_PREFIX}/xpce/man/reference/index.obj)

install(CODE "message(\"-- Creating indexes\")")

install(CODE "message(\"  -- Building INDEX.pl\")")
install(CODE "EXECUTE_PROCESS(COMMAND swipl -f none -g \"make_library_index('${XPCE_PROLOG_LIB_DIR}')\" -t halt)")

install(CODE "message(\"  -- Building CLASSINDEX.pl\")")
install(CODE "EXECUTE_PROCESS(COMMAND swipl -f none -s ${XPCE_PROLOG_LIB_DIR}/pce_class_index.pl -g \"pce_make_library_index('${XPCE_PROLOG_LIB_DIR}')\" -t halt)")

install(CODE "message(\"  -- Building ${XPCE_MANINDEX}\")")
install(CODE "EXECUTE_PROCESS(COMMAND swipl -f none -s ${XPCE_PROLOG_LIB_DIR}/man/man_index.pl -g \"pce_make_manual_index('${XPCE_MANINDEX}')\" -t halt)")

install(CODE "message(\"-- Indexes created\")")


endif(X11_FOUND AND JPEG_FOUND)
