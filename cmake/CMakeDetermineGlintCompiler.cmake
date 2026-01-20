# FOR CMAKE v3.27
#
# This file is meant to set CMAKE_Glint_COMPILER to the Glint compiler on
# the current system. It should also set CMAKE_Glint_COMPILER_ENV_VAR to
# the name of the environment variable where the Glint compiler may be
# expected.
#

# FIXME: Do we need this?
# include(${CMAKE_ROOT}/Modules/CMakeDetermineCompiler.cmake)

# Set CMAKE_Glint_COMPILER to the path to LCC (the glint compiler).
# First, Attempt to use local, system-specific compiler preferences for
# this language. This allows vendors to make things "just work".
include(Platform/${CMAKE_SYSTEM_NAME}-Determine-Glint OPTIONAL)
include(Platform/${CMAKE_SYSTEM_NAME}-Glint OPTIONAL)

# Second, if the vendor didn't supply Glint, attempt to first use the
# value of the environment variable 'GLINTC', if set. If not set, use
# `find_program` (hoping it's in PATH).
if(NOT CMAKE_Glint_COMPILER)
  if (DEFINED ENV{GLINTC})
    set (CMAKE_Glint_COMPILER $ENV{GLINTC})
  else()
    find_program(
      CMAKE_Glint_COMPILER
      lcc
      DOC "Lensor Compiler Collection"
    )
  endif()
endif()

# This is the environment variable that the glint compiler should be
# found under.
set(CMAKE_Glint_COMPILER_ENV_VAR "GLINTC")

configure_file(
  ${CMAKE_CURRENT_LIST_DIR}/CMakeGlintCompiler.cmake.in
  ${CMAKE_PLATFORM_INFO_DIR}/CMakeGlintCompiler.cmake
  @ONLY
)
