cmake_minimum_required(VERSION 2.8.12)
project(swipl-xpce)

include("../cmake/PrologPackage.cmake")

find_package(X11)
find_package(JPEG)

if(X11_FOUND AND JPEG_FOUND)

include(XPCESources)
include(XPCEConfig)

set(X11_LIBRARIES
    ${X11_Xpm_LIB}
    ${X11_Xinerama_LIB}
    ${X11_Xext_LIB}
    ${X11_Xt_LIB}
    ${X11_X11_LIB}
    ${X11_SM_LIB}
    ${X11_ICE_LIB}
    ${X11_Xft_LIB})

configure_file(config.h.cmake config.h)

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
    xpce
    C_SOURCES ${XPCE_SOURCES} ${HOST_INTERFACE_SOURCES}
    C_LIBS ${X11_LIBRARIES} ${JPEG_LIBRARIES} ${JPEG_LIBRARIES}
    PL_LIBS)
add_dependencies(plugin_xpce xpce_names)

target_include_directories(
    plugin_xpce BEFORE PRIVATE
    ${CMAKE_CURRENT_BINARY_DIR}
    ${CMAKE_CURRENT_SOURCE_DIR}/src
    ${JPEG_INCLUDE_DIRS})

endif(X11_FOUND AND JPEG_FOUND)
