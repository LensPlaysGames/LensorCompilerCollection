# FOR CMAKE v3.27

# Local system-specific compiler preferences for this language.
include(Platform/${CMAKE_SYSTEM_NAME}-Determine-Laye OPTIONAL)
include(Platform/${CMAKE_SYSTEM_NAME}-Laye OPTIONAL)

if(NOT CMAKE_Laye_COMPILER)
  find_program(
    CMAKE_Laye_COMPILER
    lcc
    DOC "Lensor Compiler Collection"
  )
endif()

configure_file(
  ${CMAKE_CURRENT_LIST_DIR}/CMakeLayeCompiler.cmake.in
  ${CMAKE_PLATFORM_INFO_DIR}/CMakeLayeCompiler.cmake
  @ONLY
)
set(CMAKE_Laye_COMPILER_ENV_VAR "LAYEC")
