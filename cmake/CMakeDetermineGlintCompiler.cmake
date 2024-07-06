# FOR CMAKE v3.27

# FIXME: Do we need this?
# include(${CMAKE_ROOT}/Modules/CMakeDetermineCompiler.cmake)

# Local system-specific compiler preferences for this language.
include(Platform/${CMAKE_SYSTEM_NAME}-Determine-Glint OPTIONAL)
include(Platform/${CMAKE_SYSTEM_NAME}-Glint OPTIONAL)

if(NOT CMAKE_Glint_COMPILER)
  find_program(
    CMAKE_Glint_COMPILER
    lcc
    DOC "Lensor Compiler Collection"
  )
endif()

configure_file(
  ${CMAKE_CURRENT_LIST_DIR}/CMakeGlintCompiler.cmake.in
  ${CMAKE_PLATFORM_INFO_DIR}/CMakeGlintCompiler.cmake
  @ONLY
)
set(CMAKE_Glint_COMPILER_ENV_VAR "GLINTC")
