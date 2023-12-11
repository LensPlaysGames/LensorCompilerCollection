# FOR CMAKE v3.27

# FIXME: Do we need this?
# include(${CMAKE_ROOT}/Modules/CMakeDetermineCompiler.cmake)

# Local system-specific compiler preferences for this language.
include(Platform/${CMAKE_SYSTEM_NAME}-Determine-Int OPTIONAL)
include(Platform/${CMAKE_SYSTEM_NAME}-Int OPTIONAL)

if(NOT CMAKE_Int_COMPILER)
  find_program(
    CMAKE_Int_COMPILER
    lcc
    DOC "Lensor Compiler Collection"
  )
endif()

configure_file(
  ${CMAKE_CURRENT_LIST_DIR}/CMakeIntCompiler.cmake.in
  ${CMAKE_PLATFORM_INFO_DIR}/CMakeIntCompiler.cmake
  @ONLY
)
set(CMAKE_Int_COMPILER_ENV_VAR "INTC")
