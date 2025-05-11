cmake_minimum_required(VERSION 3.10)
project(swipl-xpce)

include("../cmake/PrologPackage.cmake")

option(RAYLIB "Build with raylib backend instead of X11/MSW" OFF)
option(SDL    "Build with SDL backend instead of X11/MSW" OFF)

if(SDL)				# SDL3 + Cairo
  find_package(SDL3 REQUIRED CONFIG REQUIRED COMPONENTS SDL3-shared)
  find_package(SDL3_image REQUIRED CONFIG)
  find_package(PkgConfig REQUIRED)
  pkg_check_modules(CAIRO REQUIRED cairo)
  set(HAVE_GUI 1)
  set(SDL_GRAPHICS 1)
  set(GUI_LIBRARIES
    SDL3::SDL3
    SDL3_image::SDL3_image
    ${CAIRO_LIBRARIES})
elseif(RAYLIB)			# Raylib
  find_package(raylib REQUIRED)
  set(GUI_INCLUDE_DIRS ${raylib_INCLUDE_DIRS})
  set(HAVE_GUI 1)
  set(RAY_GRAPHICS 1)
elseif(WIN32)				# Windows
  add_subdirectory(deps/xpm)
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
      libxpm)
  set(XPCE_LINK_RC swipl-win)
  find_package(JPEG)
  set(GUI_INCLUDE_DIRS
      ${JPEG_INCLUDE_DIR})
else()					# X11
  find_package(X11)
  if(X11_FOUND AND X11_Xpm_LIB)
    find_package(Freetype)
    find_package(FontConfig)
    find_package(JPEG)

    if(FREETYPE_FOUND AND FONTCONFIG_FOUND AND JPEG_FOUND)
      set(HAVE_GUI 1)
      set(X11_GRAPHICS 1)
      set(GUI_LIBRARIES
	  ${X11_Xpm_LIB}
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
	  ${FONTCONFIG_INCLUDE_DIR}
	  ${JPEG_INCLUDE_DIR})
      set(XPCE_LINK_RC swipl)
      has_package(swipl-win HAVE_SWIPL_WIN)
      if(HAVE_SWIPL_WIN)
	list(APPEND XPCE_LINK_RC swipl-win)
	if(BUILD_MACOS_BUNDLE)
	  list(APPEND XPCE_LINK_RC SWI-Prolog)
	endif()
      endif()
    else()
      message("Dropped xpce")
      if(NOT JPEG_FOUND)
        message("  Could not find JPEG")
      endif()
      if(NOT FONTCONFIG_FOUND)
        message("  Could not find FontConfig")
      endif()
      if(NOT FREETYPE_FOUND)
        message("  Could not find Freetype")
      endif()
    endif()
  endif()
endif()				# RAY/MSW/X11

if(HAVE_GUI)
include(QLF)
include(XPCESources)

include(XPCEConfig)
if(HAVE_X11_EXTENSIONS_XINERAMA_H)
  set(GUI_LIBRARIES ${X11_Xinerama_LIB} ${GUI_LIBRARIES})
endif()
if(X11_Xrandr_FOUND)
  set(GUI_LIBRARIES ${X11_Xrandr_LIB} ${GUI_LIBRARIES})
endif()
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

set(cdirs adt ari box evt fmt gnu gra img itf ker men msg msw
    prg rel rgx txt unx win x11 sdl)
file(GLOB files RELATIVE "${CMAKE_CURRENT_SOURCE_DIR}/src"
     "${CMAKE_CURRENT_SOURCE_DIR}/src/h/*.h")
foreach(d ${cdirs})
  file(GLOB tmp RELATIVE "${CMAKE_CURRENT_SOURCE_DIR}/src"
       "${CMAKE_CURRENT_SOURCE_DIR}/src/${d}/*.c")
  list(APPEND files ${tmp})
endforeach()

add_custom_command(
    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/h/names.ih
	   ${CMAKE_CURRENT_BINARY_DIR}/h/names.ic
    COMMAND ${CMAKE_COMMAND} -E make_directory ${CMAKE_CURRENT_BINARY_DIR}/h
    COMMAND find_names ${CMAKE_CURRENT_BINARY_DIR}/h/names.ic
                       ${CMAKE_CURRENT_BINARY_DIR}/h/names.ih
                       -- ${files}
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/src
    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/src/namedb.txt
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
set_property(TARGET plugin_pl2xpce PROPERTY
	     C_VISIBILITY_PRESET hidden)

target_include_directories(
    plugin_pl2xpce BEFORE PRIVATE
    ${CMAKE_CURRENT_BINARY_DIR}
    ${CMAKE_CURRENT_SOURCE_DIR}/src
    ${GUI_INCLUDE_DIRS})

install_dll(${JPEG_LIBRARIES})

################
# Tests

test_libs(xpce)

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
    DEPENDS library_index
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
    QUIET
    OUTPUT ${XPCE_MANINDEX}
    COMMAND "pce_make_manual_index('${XPCE_MANINDEX}')"
    SCRIPT  "${XPCE_BUILD_LIBRARY}/man/man_index.pl"
    DEPENDS xpce-predicate-index
    COMMENT "-- Building manual index")

set(SWIPL_COMMAND_DEPENDS ${SWIPL_COMMAND_DEPENDS}
    xpce-predicate-index xpce-class-index)

if(INSTALL_QLF)
  add_dependencies(library_qlf xpce-predicate-index xpce-class-index)
endif()

add_qcompile_target(pce               SOURCES ${XPCE_QLF_pce})
add_qcompile_target(emacs/emacs	      SOURCES ${XPCE_QLF_emacs}
				      DEPENDS qlf-pce)
add_qcompile_target(trace/trace       SOURCES ${XPCE_QLF_trace}
				      EXPECTDEPS "library(debug)"
				      DEPENDS qlf-emacs-emacs)
add_qcompile_target(emacs/prolog_mode DEPENDS qlf-emacs-emacs
				      EXPECTDEPS "library(debug)")

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
install_src(pkg_xpce_rc
	    FILES swipl/swipl-rc
	    DESTINATION ${SWIPL_INSTALL_PREFIX}/xpce/prolog)

# Install the link file
foreach(rc ${XPCE_LINK_RC})
  install(FILES swipl/swipl-rc
	  DESTINATION ${SWIPL_INSTALL_PREFIX}
	  RENAME ${rc}.rc)
endforeach()

else(HAVE_GUI)
add_custom_target(pl2xpce COMMENT "dummy target to avoid breaking build without GUI")
endif(HAVE_GUI)
