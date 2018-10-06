cmake_minimum_required(VERSION 2.8.12)
project(swipl-xpce)

include("../cmake/PrologPackage.cmake")

if(WIN32)
  find_package(LibXpm)
  set(HAVE_GUI 1)
  set(WIN32_GRAPHICS 1)
  set(FOR_MSW 1)
  set(O_XOS 1)
  set(O_DOSFILENAMES 1)
  set(O_HASSHARES 1)
  set(HAVE_WINSOCK 1)
  set(GUI_LIBRARIES
      gdi32.lib
      comdlg32.lib
      ole32.lib
      shell32.lib
      ws2_32.lib
      ${XPM_LIBRARY})
  set(GUI_INCLUDE_DIRS)
  set(XPCE_LINK_RC swipl-win)
else()
  find_package(X11)
  if(X11_FOUND)
    set(HAVE_GUI 1)
    find_package(Freetype)
    find_package(FontConfig)

    set(GUI_LIBRARIES
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

    set(GUI_INCLUDE_DIRS
	${X11_INCLUDE_DIR}
	${FREETYPE_INCLUDE_DIRS}
	${FONTCONFIG_INCLUDE_DIR})
    set(XPCE_LINK_RC swipl)
  endif()
endif()
find_package(JPEG)
set(GUI_INCLUDE_DIRS ${GUI_INCLUDE_DIRS} ${JPEG_INCLUDE_DIR})

if(HAVE_GUI AND JPEG_FOUND)

include(QLF)
include(XPCESources)

include(XPCEConfig)
set(PCE_MACHINE ${SWIPL_ARCH})
set(PCE_OS ${CMAKE_HOST_SYSTEM_NAME})
set(PCE_VERSION 6.6.6)

configure_file(config.h.cmake config.h)

################
# Build pl2xpce shared object

# -DSWI is required for the interface only
if(UNIX)
  add_compile_options(-DHAVE_CONFIG_H -DSWI)
endif()

add_custom_command(
    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/h/names.ih
	   ${CMAKE_CURRENT_BINARY_DIR}/h/names.ic
    COMMAND mkdir -p ${CMAKE_CURRENT_BINARY_DIR}/h
    COMMAND find_names ${CMAKE_CURRENT_BINARY_DIR}/h/names.ic
		       ${CMAKE_CURRENT_BINARY_DIR}/h/names.ih
		       -- h/*.h ???/*.c
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/src
    COMMENT "Collecting NAME_ declarations")

add_custom_target(
    xpce_names
    DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/h/names.ih)

add_executable(find_names src/find_names.c)

swipl_plugin(
    pl2xpce
    C_SOURCES ${XPCE_SOURCES} ${HOST_INTERFACE_SOURCES}
    C_LIBS ${GUI_LIBRARIES} ${JPEG_LIBRARIES}
    PL_LIBS)
add_dependencies(plugin_pl2xpce xpce_names)

target_include_directories(
    plugin_pl2xpce BEFORE PRIVATE
    ${CMAKE_CURRENT_BINARY_DIR}
    ${CMAKE_CURRENT_SOURCE_DIR}/src
    ${GUI_INCLUDE_DIRS})

install_dll(${JPEG_LIBRARIES})

################
# Create indexes

set(XPCE_BUILD_HOME      ${SWIPL_BUILD_HOME}/xpce)
set(XPCE_BUILD_LIBRARY   ${XPCE_BUILD_HOME}/prolog/lib)
set(XPCE_PREDICATE_INDEX ${XPCE_BUILD_LIBRARY}/INDEX.pl)
set(XPCE_CLASS_INDEX     ${XPCE_BUILD_LIBRARY}/CLASSINDEX.pl)
set(XPCE_MANINDEX        ${XPCE_BUILD_HOME}/man/reference/index.obj)

set(SWIPL_COMMAND_DEPENDS plugin_pl2xpce)
set(SWIPL_QLF_BASE        ${XPCE_BUILD_LIBRARY})

add_swipl_target(
    xpce-predicate-index
    OUTPUT  ${XPCE_PREDICATE_INDEX}
    COMMAND "make_library_index('${XPCE_BUILD_LIBRARY}')"
    QUIET
    COMMENT "-- Building xpce predicate index")
add_swipl_target(
    xpce-class-index
    OUTPUT  ${XPCE_CLASS_INDEX}
    COMMAND "pce_make_library_index('${XPCE_BUILD_LIBRARY}')"
    DEPENDS xpce-predicate-index
    COMMENT "-- Building xpce class index")
add_swipl_target(
    xpce-manual-index
    OUTPUT ${XPCE_MANINDEX}
    COMMAND "pce_make_manual_index('${XPCE_MANINDEX}')"
    SCRIPT  "${XPCE_BUILD_LIBRARY}/man/man_index.pl"
    DEPENDS xpce-predicate-index
    COMMENT "-- Building manual index")

set(SWIPL_COMMAND_DEPENDS ${SWIPL_COMMAND_DEPENDS}
    xpce-predicate-index xpce-class-index)

add_qcompile_target(pce               SOURCES ${XPCE_QLF_pce})
add_qcompile_target(emacs/emacs	      SOURCES ${XPCE_QLF_emacs}
				      DEPENDS qlf-pce)
add_qcompile_target(trace/trace       SOURCES ${XPCE_QLF_trace}
				      DEPENDS qlf-emacs-emacs)
add_qcompile_target(emacs/prolog_mode DEPENDS qlf-emacs-emacs)

################
# Install

# Install the data files
foreach(d ${XPCE_DATA_DIRS})
    string(REGEX REPLACE "/" "_" filevar ${d})
    prepend(files ${d} ${XPCE_DATA_${filevar}})
    install_src(pkg_xpce_${filevar}
		FILES ${files}
		DESTINATION ${SWIPL_INSTALL_PREFIX}/xpce/${d})
endforeach()

install_src(pkg_xpce_defaults
	    FILES Defaults Defaults.user
	    DESTINATION ${SWIPL_INSTALL_PREFIX}/xpce)

# Install the link file
foreach(rc ${XPCE_LINK_RC})
  install(FILES swipl/swipl-rc
	  DESTINATION ${SWIPL_INSTALL_PREFIX}
	  RENAME ${rc}.rc)
endforeach()

endif(HAVE_GUI AND JPEG_FOUND)
