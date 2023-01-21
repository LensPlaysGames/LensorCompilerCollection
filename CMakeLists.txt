cmake_minimum_required(VERSION 3.14)

# Use `ccache` if it is installed in system's PATH.
find_program(CCACHE_PROGRAM ccache)
if(CCACHE_PROGRAM)
  set_property(GLOBAL PROPERTY RULE_LAUNCH_COMPILE "${CCACHE_PROGRAM}")
endif()

# Export compilation database in JSON format.
set(CMAKE_EXPORT_COMPILE_COMMANDS on)
set(CMAKE_C_STANDARD 11)

project(FUNCompiler C CXX)

## Clang plugin to typecheck format strings.
if (CMAKE_C_COMPILER_ID STREQUAL "Clang")
  find_package(LLVM CONFIG)
  if (DEFINED LLVM_PACKAGE_VERSION)
    message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")

    ## Add the plugin as a target.
    set (CMAKE_CXX_STANDARD 20)
    add_library(fmt-check-plugin SHARED fmt-check-plugin/fmt-check-plugin.cc)

    ## LLVM include dirs and definitions.
    target_include_directories(fmt-check-plugin PRIVATE ${LLVM_INCLUDE_DIRS})
    separate_arguments(LLVM_DEFINITIONS_LIST NATIVE_COMMAND ${LLVM_DEFINITIONS})
    target_compile_definitions(fmt-check-plugin PRIVATE ${LLVM_DEFINITIONS_LIST})

    ## LLVM libs.
    llvm_map_components_to_libnames(llvm_libs all)
    target_link_libraries(fmt-check-plugin PRIVATE ${llvm_libs})

    ## Disable this since is a bit annoying.
    add_compile_options(-Wno-unused-command-line-argument)
  endif()
endif()

# User Option(s)
set(
  NATIVE_OPT TRUE
  CACHE BOOL
  "Whether or not to optimise for the system being compiled on. This should be true *unless* you are making distributed binaries."
)

add_executable(
  func
  src/ast.c
  src/codegen.c
  src/error.c
  src/main.c
  src/ir_parser.c
  src/parser.c
  src/typechecker.c
  src/opt.c
  src/utils.c
  src/platform.c
  src/codegen/dom.c
  src/codegen/intermediate_representation.c
  src/codegen/register_allocation.c
  src/codegen/ir/ir.c
  src/codegen/x86_64/arch_x86_64.c
)
target_include_directories(
  func
  PUBLIC src/
)

# Do not link with libm (math) when target is windows executable.
if (NOT WIN32)
  target_link_libraries(func PRIVATE m)
endif()

# Run the format checking plugin if possible.
if (CMAKE_C_COMPILER_ID STREQUAL "Clang" AND DEFINED LLVM_PACKAGE_VERSION)
  add_dependencies(func fmt-check-plugin)
  if (EXISTS "./libfmt-check-plugin.so")
    target_compile_options(func PRIVATE "-fplugin=./libfmt-check-plugin.so")
  endif()
endif()

## Debug/Release flags.
if (NOT MSVC)
  target_compile_options(func PRIVATE
    $<$<CONFIG:DEBUG>:-O0 -g3 -ggdb3>
    $<$<CONFIG:RELEASE>:-O3>
  )
  target_link_options(func PRIVATE
    $<$<CONFIG:DEBUG>:-O0 -g3 -ggdb3>
    $<$<CONFIG:RELEASE>:-O3>
  )

  # Keep internal symbol names in debug mode if possible. See below for clang.
  if (CMAKE_C_COMPILER_ID STREQUAL "GCC")
    target_compile_options(func PRIVATE $<$<CONFIG:DEBUG>:-rdynamic>)
  endif()

  # -march=native only makes sense when compiling for the compiled system.
  if (NATIVE_OPT)
    target_compile_options(func PRIVATE
      $<$<CONFIG:RELEASE>:-march=native>
    )
  endif()

else()
  target_compile_options(func PRIVATE
    $<$<CONFIG:DEBUG>:/Od>
    $<$<CONFIG:RELEASE>:/O2>
  )
endif()

# When compiling with clang-cl, MSVC is also defined, so we put this here instead.
if (CMAKE_C_COMPILER_ID STREQUAL "Clang")
  target_compile_options(func PRIVATE $<$<CONFIG:DEBUG>:-Wl,-export-dynamic>)
endif()

# Enable asan if requested and possible.
if (DEFINED ENABLE_ASAN AND NOT WIN32)
  target_compile_options(func PRIVATE -fsanitize=address)
  target_link_options(func PRIVATE -fsanitize=address)
endif()

# Compile options.
if (NOT MSVC)
  target_compile_options(
    func
    PRIVATE
    -Wall -Wextra
    -Wshadow -Wconversion
    -Werror=return-type -Werror=implicit-function-declaration
    -Werror=implicit-int -Werror=pointer-arith

    -Wno-unused-parameter -Wno-unused-function
    -Wno-format-zero-length -Wno-comment

    -fdiagnostics-color=always
  )
else()
  target_compile_options(
    func
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
      func
      PRIVATE
      -Wno-unused-function -Wno-unused-parameter
      -Wno-format-zero-length
    )
  endif()
endif()

# On Windows, don't suggest the _s nonsense functions.
if (WIN32)
  target_compile_definitions(func PRIVATE
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
    test
    COMMAND a68g main.a68
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/tst
    COMMENT "Running tests"
    VERBATIM USES_TERMINAL
  )
endif()
