# Find XMP for Windows

find_library(XPM_LIBRARY
	     NAMES Xpm)

include(FindPackageHandleStandardArgs)

find_package_handle_standard_args(
    LibXpm
    REQUIRED_VARS XPM_LIBRARY)

mark_as_advanced(XPM_LIBRARY)

