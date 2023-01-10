cmake_minimum_required(VERSION 3.14)

# Use `ccache` if it is installed in system's PATH.
find_program(CCACHE_PROGRAM ccache)
if(CCACHE_PROGRAM)
  set_property(GLOBAL PROPERTY RULE_LAUNCH_COMPILE "${CCACHE_PROGRAM}")
endif()

# Export compilation database in JSON format.
set(CMAKE_EXPORT_COMPILE_COMMANDS on)
set(CMAKE_C_STANDARD 11)

project(FUNCompiler C)

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

if (DEFINED ENABLE_ASAN AND NOT WIN32)
  target_compile_options(func PRIVATE -fsanitize=address)
  target_link_options(func PRIVATE -fsanitize=address)
endif()

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

    -fdiagnostics-color
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
endif()

# If user is targeting Windows.
if (WIN32)
  target_compile_definitions(
    func
    PRIVATE
    # Don't suggest the _s nonsense functions.
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
