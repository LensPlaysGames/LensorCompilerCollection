cmake_minimum_required(VERSION 3.19)
project(Intercept C CXX)

# Use `ccache` if it is installed in system's PATH.
find_program(CCACHE_PROGRAM ccache)
if (CCACHE_PROGRAM)
  set_property(GLOBAL PROPERTY RULE_LAUNCH_COMPILE "${CCACHE_PROGRAM}")
endif()

# Export compilation database in JSON format.
set(CMAKE_EXPORT_COMPILE_COMMANDS on)
set(CMAKE_C_STANDARD 11)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Allow arbitrary characters in test names.
cmake_policy(SET CMP0110 NEW)

# User Option(s)
set(
  NATIVE_OPT TRUE
  CACHE BOOL
  "Whether or not to optimise for the system being compiled on. This should be true *unless* you are making distributed binaries."
)

set(
  BUILD_NATIVE_TESTING TRUE
  CACHE BOOL
  "Whether or not to include native tests in ctest"
)

set(
  BUILD_OPT_TESTING FALSE
  CACHE BOOL
  "Whether or not to include optimisation tests in ctest"
)

set(
  BUILD_OBJ_TESTING FALSE
  CACHE BOOL
  "Whether or not to include object file target tests in ctest (default just includes assembly)"
)

set(
  BUILD_LLVM_TESTING FALSE
  CACHE BOOL
  "Whether or not to include LLVM target tests in ctest (default just includes assembly)"
)

add_executable(
  intc
  src/ast.c
  src/codegen.c
  src/error.c
  src/main.c
  #src/ir_parser.c
  src/parser.c
  src/typechecker.c
  src/utils.c
  src/platform.c
  src/module.c
  src/ir/dom.c
  src/codegen/generic_object.c
  src/codegen/instruction_selection.c
  src/ir/ir.c
  src/codegen/register_allocation.c
  src/codegen/opt/opt.c
  src/ir/inline.c
  src/codegen/machine_ir.c
  #src/codegen/ir/ir.c
  src/codegen/llvm/llvm_target.c
  src/codegen/x86_64/arch_x86_64.c
  src/codegen/x86_64/arch_x86_64_common.c
  src/codegen/x86_64/arch_x86_64_isel.c
  src/codegen/x86_64/arch_x86_64_tgt_assembly.c
  src/codegen/x86_64/arch_x86_64_tgt_generic_object.c
)
target_include_directories(
  intc
  PUBLIC src/
)

## Hard-code ISel table locations for now so it works on Windows in VS.
target_compile_definitions(intc PRIVATE
    ISEL_TABLE_LOCATION_X86_64="${PROJECT_SOURCE_DIR}/src/codegen/x86_64/arch_x86_64.isel"
)

## Build the format check plugin *now*, as CMake will complain otherwise when we
## try to add it to the command-line for the Intercept compiler below.
if (CMAKE_C_COMPILER_ID STREQUAL "Clang")
  find_package(LLVM CONFIG)
  if (DEFINED LLVM_PACKAGE_VERSION AND LLVM_VERSION_MAJOR GREATER_EQUAL 15)
    execute_process(
      COMMAND "${CMAKE_COMMAND}"
        -S .
        -B out
        "-DLLVM_DIR=${LLVM_DIR}"
        "-DCMAKE_CXX_COMPILER=${CMAKE_CXX_COMPILER}"
        "-DCMAKE_C_COMPILER=${CMAKE_C_COMPILER}"
        "-DCMAKE_GENERATOR=${CMAKE_GENERATOR}"
      WORKING_DIRECTORY "${PROJECT_SOURCE_DIR}/fmt-check-plugin"
      ECHO_OUTPUT_VARIABLE
      ECHO_ERROR_VARIABLE
      COMMAND_ERROR_IS_FATAL ANY
    )

    execute_process(
      COMMAND "${CMAKE_COMMAND}" --build out --target fmt-check-plugin
      WORKING_DIRECTORY "${PROJECT_SOURCE_DIR}/fmt-check-plugin"
      ECHO_OUTPUT_VARIABLE
      ECHO_ERROR_VARIABLE
      COMMAND_ERROR_IS_FATAL ANY
    )

    # Run the format checking plugin.
    message(STATUS "Done compiling format check plugin.")
    target_compile_options(intc PRIVATE "-fplugin=${PROJECT_SOURCE_DIR}/fmt-check-plugin/out/fmt-check-plugin")
  endif()
endif()

# Do not link with libm (math) when target is windows executable.
if (NOT WIN32)
  target_link_libraries(intc PRIVATE m)
endif()

## Debug/Release flags.
if (NOT MSVC)
  target_compile_options(intc PRIVATE
    $<$<CONFIG:DEBUG>:-O0 -g3 -ggdb3>
    $<$<CONFIG:RELEASE>:-O3>
  )
  target_link_options(intc PRIVATE
    $<$<CONFIG:DEBUG>:-O0 -g3 -ggdb3>
    $<$<CONFIG:RELEASE>:-O3>
  )

  # Keep internal symbol names in debug mode if possible. See below for clang.
  if (CMAKE_C_COMPILER_ID STREQUAL "GCC")
    target_compile_options(intc PRIVATE $<$<CONFIG:DEBUG>:-rdynamic>)
  endif()

  # -march=native only makes sense when compiling for the compiled system.
  if (NATIVE_OPT)
    target_compile_options(intc PRIVATE
      $<$<CONFIG:RELEASE>:-march=native>
    )
  endif()

else()
  target_compile_options(intc PRIVATE
    $<$<CONFIG:DEBUG>:/Od>
    $<$<CONFIG:RELEASE>:/O2>
  )
endif()

# When compiling with clang-cl, MSVC is also defined, so we put this here instead.
if (CMAKE_C_COMPILER_ID STREQUAL "Clang")
  target_link_options(intc PRIVATE $<$<CONFIG:DEBUG>:-Wl,-export-dynamic>)
endif()

# Enable asan if requested and possible.
if (NOT WIN32)
  if (ENABLE_ASAN)
    target_compile_options(intc PRIVATE -fsanitize=address)
    target_compile_definitions(intc PRIVATE ENABLE_ASAN=1)
    target_link_options(intc PRIVATE -fsanitize=address)

    # Make address sanitiser ignore memory leaks. This is useful if there are
    # more severe problems (e.g. use-after-free bugs) that need to be fixed.
    if (ASAN_IGNORE_LEAKS)
      target_compile_definitions(intc PRIVATE ASAN_IGNORE_LEAKS=1)
    endif ()
  endif()
endif ()

# Compile options.
if (NOT MSVC)
  target_compile_options(
    intc
    PRIVATE
    -Wall -Wextra
    -Wshadow -Wconversion

    -Werror=return-type -Werror=implicit-function-declaration
    -Werror=implicit-int -Werror=pointer-arith
    -Werror=incompatible-pointer-types

    -Wno-unused-parameter -Wno-unused-function
    -Wno-format-zero-length -Wno-comment
    -Wno-cast-function-type
  )
  # Use color codes in the output of the compiler for readability.
  # FIXME: Make this a generator expression
  if (CMAKE_C_COMPILER_ID STREQUAL "Clang")
    target_compile_options(intc PRIVATE -fcolor-diagnostics)
  else()
    target_compile_options(intc PRIVATE -fdiagnostics-color=always)
  endif()
else()
  target_compile_options(
    intc
    PRIVATE
    # Enable "all" warnings.
    /W4

    # Source character set is UTF-8
    /utf-8

    # Allow unnamed structs/unions.
    /wd4201

    # Don't warn about unused functions.
    /wd4505
  )

  # clang-cl
  if (CMAKE_C_COMPILER_ID STREQUAL "Clang")
    target_compile_options(
      intc
      PRIVATE
      -Wno-unused-function -Wno-unused-parameter
      -Wno-format-zero-length
    )
  endif()
endif()

# On Windows, don't suggest the _s nonsense functions.
if (WIN32)
  target_compile_definitions(intc PRIVATE
    _CRT_SECURE_NO_WARNINGS
    _CRT_SECURE_NO_WARNINGS_GLOBALS
    _CRT_NONSTDC_NO_WARNINGS
  )
endif()

find_program(A68G_PROGRAM a68g NO_CACHE)

if (A68G_PROGRAM STREQUAL "A68G_PROGRAM-NOTFOUND")
  message(NOTICE "

[41m
[m
[31;5mERROR: MISSING PROGRAM!  Could not find `a68g` Algol 68 Genie Interpreter, test target has not been generated. See README in `tst` subdirectory.
[41m
[m

")
else()
  add_custom_target(
    algol-suite
    COMMAND a68g main.a68
    WORKING_DIRECTORY ${CMAKE_CURRENT_LIST_DIR}/tst
    COMMENT "Running tests"
    VERBATIM
    USES_TERMINAL
  )
endif()

include(CTest)
include(FetchContent)
if (BUILD_TESTING)
  message(STATUS "Downloading dependencies for test runner. This may take a while.")
  FetchContent_Declare(fmtlib
    GIT_REPOSITORY https://github.com/fmtlib/fmt.git
    GIT_TAG 10.0.0
    SOURCE_DIR "${CMAKE_CURRENT_BINARY_DIR}/libs/fmt"
  )

  FetchContent_MakeAvailable(fmtlib)

  add_executable(test_instantiator tst/main.cpp)
  target_link_libraries(test_instantiator PRIVATE fmt)
  target_include_directories(test_instantiator SYSTEM PRIVATE
    "${clopts_SOURCE_DIR}/include"
    "${fmtlib_SOURCE_DIR}/include"
  )
  target_compile_options(test_instantiator PUBLIC -fconcepts)

  ## User needs to specify a linker.
  if (NOT DEFINED INTERCEPT_TEST_LINKER)
    message (FATAL_ERROR "Please specify a linker for tests using -DINTERCEPT_TEST_LINKER=/path/to/linker. The path must be an absolute path and point to a version of gcc or clang (not clang-cl!)")
  endif ()
  message(STATUS "Using linker ${INTERCEPT_TEST_LINKER} for tests.")

  file(GLOB tests ${CMAKE_CURRENT_LIST_DIR}/tst/tests/*.int)
  foreach (test ${tests})
    if (BUILD_NATIVE_TESTING)
      add_test(
        NAME ASM_${test}
        COMMAND $<TARGET_FILE:test_instantiator>
        asm
        "${test}"
        $<TARGET_FILE:intc>
        "${INTERCEPT_TEST_LINKER}"
        WORKING_DIRECTORY ${CMAKE_CURRENT_LIST_DIR}
        COMMAND_EXPAND_LISTS
      )
      set_tests_properties(ASM_${test} PROPERTIES TIMEOUT 10)
    endif()
    if (BUILD_OBJ_TESTING)
      if (WIN32)
        add_test(
          NAME OBJ_${test}
          COMMAND $<TARGET_FILE:test_instantiator>
          coff_object
          "${test}"
          $<TARGET_FILE:intc>
          "${INTERCEPT_TEST_LINKER}"
          WORKING_DIRECTORY ${CMAKE_CURRENT_LIST_DIR}
          COMMAND_EXPAND_LISTS
        )
        set_tests_properties(OBJ_${test} PROPERTIES TIMEOUT 10)
      else()
        add_test(
          NAME OBJ_${test}
          COMMAND $<TARGET_FILE:test_instantiator>
          elf_object
          "${test}"
          $<TARGET_FILE:intc>
          "${INTERCEPT_TEST_LINKER}"
          WORKING_DIRECTORY ${CMAKE_CURRENT_LIST_DIR}
          COMMAND_EXPAND_LISTS
        )
        set_tests_properties(OBJ_${test} PROPERTIES TIMEOUT 10)
      endif()
    endif()
    if (BUILD_OPT_TESTING)
      add_test(
        NAME OPT_${test}
        COMMAND $<TARGET_FILE:test_instantiator>
        asm
        "${test}"
        $<TARGET_FILE:intc>
        "${INTERCEPT_TEST_LINKER}"
        -O
        WORKING_DIRECTORY ${CMAKE_CURRENT_LIST_DIR}
        COMMAND_EXPAND_LISTS
      )
      set_tests_properties(OPT_${test} PROPERTIES TIMEOUT 10)
    endif()
    if (BUILD_LLVM_TESTING)
      add_test(
        NAME LLVM_${test}
        COMMAND $<TARGET_FILE:test_instantiator>
        llvm
        "${test}"
        $<TARGET_FILE:intc>
        "${INTERCEPT_TEST_LINKER}"
        WORKING_DIRECTORY ${CMAKE_CURRENT_LIST_DIR}
        COMMAND_EXPAND_LISTS
      )

      set_tests_properties(LLVM_${test} PROPERTIES TIMEOUT 10)
    endif()
  endforeach()
endif()
