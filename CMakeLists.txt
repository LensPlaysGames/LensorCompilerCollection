cmake_minimum_required(VERSION 3.20)
project(LCC VERSION 4.20.0 LANGUAGES C CXX)

# User Option(s)
set(
  NATIVE_OPT TRUE
  CACHE BOOL
  "Whether or not to optimise for the system being compiled on. This
  should be true *unless* you are making distributed binaries."
)

set(
  USE_LTO TRUE
  CACHE BOOL
  "Whether or not to utilise link time optimisation (LTO).
  You should probably enable this for most release builds, as it
  increases code performance while decreasing code size (generally).
  However, you should probably disable this for your development build
  tree, as it may add a lot of extra time to each build invocation."
)

set(
  USE_COLOUR TRUE
  CACHE BOOL
  "Whether or not to tell the compiler to use color for emitting
  diagnostics (i.e. via '-fdiagnostics-color=always')."
)

set(
  LCC_USE_DEBUG_ITERATORS FALSE
  CACHE BOOL
  "Whether or not to use checked container iterators.
  NOTE: Debug iterators are always in use in non-release builds (NDEBUG
  not defined)."
)

# ============================================================================
#  Global CMake Variables
# ============================================================================
# Export compilation database in JSON format.
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_C_STANDARD 11)
set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# ============================================================================
#  Other Global Settings
# ============================================================================
# Use `ccache` if it is installed in system's PATH.
find_program(CCACHE_PROGRAM ccache)
if (CCACHE_PROGRAM)
  set_property(GLOBAL PROPERTY RULE_LAUNCH_COMPILE "${CCACHE_PROGRAM}")
endif()

# Turn on diagnostics colours.
if (USE_COLOUR)
  if (CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
    add_compile_options(-fdiagnostics-color=always)
  elseif (CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
    add_compile_options(-fcolor-diagnostics)
  endif()
endif()

# Use mold as the default linker, if it exists.
if (CMAKE_CXX_COMPILER_ID STREQUAL "GNU" OR CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
  find_program(MOLD_LINKER "mold")
  if (MOLD_LINKER)
    add_link_options(-fuse-ld=mold)
  endif()
endif()

if (USE_LTO)
  # Do link time optimisation
  if (CMAKE_CXX_COMPILER_ID STREQUAL "GNU" OR CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
    add_compile_options(-flto)
    add_link_options(-flto)
  endif()
  if (MSVC)
    add_compile_options(/GL)
    add_link_options(/LTCG)
  endif()
endif()

# Output HTML as well as JS if compiling with emscripten (for wasm)
if (EMSCRIPTEN)
  # Disable "native" optimisation.
  # The web isn't native anywhere we are building.
  set(NATIVE_OPT FALSE)
  # Ask Emscripten to please generate HTML.
  set(CMAKE_EXECUTABLE_SUFFIX ".html")
  # Reduce binary size: Don't include setjmp/longjmp handling.
  add_compile_options(-sSUPPORT_LONGJMP=0)
  # NOTE: EVAL_CTORS doesn't work well
  # Reduce binary size: Don't include assertion handling.
  add_link_options(-sASSERTIONS=0)
  # Reduce binary size: Use Emscripten's smaller malloc.
  add_link_options(-sMALLOC="emmalloc")
  # Use the closure javascript compiler.
  add_link_options(--closure 1)
  # Custom HTML template file.
  add_link_options(--shell-file ${CMAKE_CURRENT_LIST_DIR}/res/lcc.in.html)
  # Increase the stack size; some languages use a recursive descent parsing
  # algorithm that means the stack isn't always necessarily small.
  add_link_options(-sSTACK_SIZE=262144)
  # Do not automatically run LCC when the script is loaded; without
  # arguments, it's not all that useful, anyway.
  add_link_options(-sINVOKE_RUN=0)
  # Exports used from JavaScript.
  add_link_options("-sEXPORTED_RUNTIME_METHODS=[\"callMain\", \"FS\"]")
  # Create an async factory function to run the generated module, vs doing
  # things in the global scope.
  add_link_options(-sEXPORT_NAME="LCC" -sMODULARIZE=1 -sEXPORT_ES6=1)
  # Do exit/cleanup things.
  add_link_options(-sEXIT_RUNTIME=1)
endif()

# ============================================================================
#  Compiler options.
# ============================================================================
add_library(options INTERFACE)

# Flags for Clang and GCC.
if (CMAKE_CXX_COMPILER_ID STREQUAL "GNU" OR CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
  target_compile_options(options INTERFACE
    # Warnings.
    -Wall -Wextra     # Enable ‘all’ warnings.
    -Wundef           # Invalid #undef or undefined macro in #if.
    -Wcast-align      # Casting that changes alignment.
    -Wconversion      # Implicit conversions.
    -Wsign-conversion # Implicit sign conversions.
    -Werror=shadow    # No shadowing!
    -Wformat=2        # Stricter format checking.

    # Disabled warnings.
    -Wno-unused-function
    -Wno-unused-local-typedefs
    -Wno-unused-parameter


    # NULL Errors.
    -Werror=nonnull # Passing NULL to nonnull parameter.

    # Memory Errors.
    -Werror=address              # Suspicious use of addresses.
    -Werror=init-self            # Initialization of a variable with itself.
    -Werror=uninitialized

    # Return type.
    -Werror=return-type

    # C/C++.
    -Werror=implicit-fallthrough
    -Werror=missing-include-dirs # User-specified include dir does not exist.
    -Werror=pointer-arith        # Disallow void* and function pointer arithmetic.
    -Werror=string-compare       # Nonsensical string comparisons.
    -Werror=switch               # Missing switch cases.
    # -Werror=switch-enum          # Switch on enum (even if there is a default case).
    -Werror=write-strings        # Strings in C should be const char*.

    # C++.
    -Werror=missing-field-initializers
    -Werror=non-virtual-dtor
    -Werror=pessimizing-move
  )

  # -march=native only makes sense when compiling for the compiled system.
  if (NATIVE_OPT)
    target_compile_options(options INTERFACE
      $<$<CONFIG:RELEASE>:-march=native>
    )
  endif()
endif()

# Additional flags for GCC.
if (CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
  target_compile_options(options INTERFACE
    -Wlogical-op      # Duplicate or unintended logical operators.
    -Werror=invalid-memory-model # For atomics.
    -Werror=maybe-uninitialized
    -Werror=missing-requires
    -Werror=return-local-addr
  )
endif()

# Additional flags for Clang.
if (CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
  target_compile_options(options INTERFACE
    -Werror=dangling
    -Werror=return-stack-address
  )
endif()

# Flags for MSVC/clang-cl.
if (MSVC)
  target_compile_options(options INTERFACE
    # Enable ‘all’ warnings.
    /W4

    # Source character set is UTF-8
    /utf-8

    # Allow unnamed structs/unions.
    /wd4201

    # Don't warn about unused functions.
    /wd4505
  )
endif()

# On Windows, don’t suggest the _s nonsense functions.
if (WIN32)
  target_compile_definitions(options INTERFACE
    _CRT_SECURE_NO_WARNINGS
    _CRT_SECURE_NO_WARNINGS_GLOBALS
    _CRT_NONSTDC_NO_WARNINGS
  )
endif()

# Debug/Release flags.
if (NOT MSVC)
  if (NOT CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
    target_compile_options(options INTERFACE $<$<CONFIG:DEBUG>:-rdynamic>)
  else()
    target_compile_options(options INTERFACE -Wno-unused-private-field)
  endif()

  target_compile_options(options INTERFACE
    $<$<CONFIG:DEBUG>:-O0 -g3 -ggdb3>
    $<$<CONFIG:RELEASE>:-O3>
  )
  target_link_options(options INTERFACE
    $<$<CONFIG:DEBUG>:-O0 -g3 -ggdb3 -rdynamic>
    $<$<CONFIG:RELEASE>:-O3>
  )

  if (NATIVE_OPT)
    target_compile_options(options INTERFACE
      $<$<CONFIG:RELEASE>:-march=native>
    )
    target_link_options(options INTERFACE
      $<$<CONFIG:RELEASE>:-march=native>
    )
  endif()
else()
  target_compile_options(options INTERFACE
    $<$<CONFIG:DEBUG>:/Od>
    $<$<CONFIG:RELEASE>:/O2>
  )
endif()

# Enable AddressSanitizer if requested
if (ENABLE_ASAN)
  target_compile_options(options INTERFACE -fsanitize=address)
  target_link_options(options INTERFACE -fsanitize=address)
  target_compile_definitions(options INTERFACE ENABLE_ASAN=1)
endif()

# ============================================================================
#  Dependencies
# ============================================================================
# Find libfmt.
include(FetchContent)
set(FETCHCONTENT_QUIET OFF)
set(FETCHCONTENT_UPDATES_DISCONNECTED ON)
message(STATUS "Checking dependencies ...")
# NOTE: If you don't like cloning every time you configure a new build
# tree, you can specify `fmtlib_SOURCE_DIR` as pointing to the cloned
# repo.
FetchContent_Declare(
  fmtlib
  GIT_REPOSITORY https://github.com/fmtlib/fmt.git
  GIT_TAG 12.1.0
  SOURCE_DIR "${CMAKE_CURRENT_BINARY_DIR}/libs/fmt"
)
FetchContent_MakeAvailable(fmtlib)
target_include_directories(options INTERFACE ${fmtlib_SOURCE_DIR}/include)

# Link against libfmt.
target_link_libraries(options INTERFACE fmt)

# Add ‘include’ as an include dir.
target_include_directories(options INTERFACE inc)

# Do not link with libm (math) when target is windows executable.
if (NOT WIN32)
  target_link_libraries(options INTERFACE m)
endif()

# ============================================================================
#  Executables and libraries.
# ============================================================================

# NOTE: If you go to use a glob, and are trying to complain that it's too
# hard to list files individually, or it's too hard to alphabetise the
# lists, well, the tree command does both of those things for you by
# default, and I don't take kindly to those not willing to put in the
# effort ơ_ơ.

# Generic object library (convertible to ELF and COFF, currently)
add_library(
  object STATIC
  inc/object/coff.h
  inc/object/elf.h
  inc/object/elf.hh
  inc/object/generic.hh
  lib/object/elf.cc
  lib/object/generic.cc
  lib/object/generic_to_coff.cc
  lib/object/generic_to_elf.cc
)
target_include_directories(object PUBLIC inc)
target_link_libraries(object PRIVATE options)

# Add the main lcc library.
add_library(
  liblcc STATIC
  inc/lcc/calling_convention.hh
  inc/lcc/calling_conventions/ms_x64.hh
  inc/lcc/calling_conventions/sysv_x86_64.hh
  inc/lcc/codegen/gnu_as_att_assembly.hh
  inc/lcc/codegen/isel.hh
  inc/lcc/codegen/mir.hh
  inc/lcc/codegen/mir_utils.hh
  inc/lcc/codegen/register_allocation.hh
  inc/lcc/codegen/x86_64/assembly.hh
  inc/lcc/codegen/x86_64/isel_patterns.hh
  inc/lcc/codegen/x86_64/object.hh
  inc/lcc/codegen/x86_64/x86_64.hh
  inc/lcc/context.hh
  inc/lcc/core.hh
  inc/lcc/diags.hh
  inc/lcc/file.hh
  inc/lcc/format.hh
  inc/lcc/forward.hh
  inc/lcc/ir/core.hh
  inc/lcc/ir/domtree.hh
  inc/lcc/ir/module.hh
  inc/lcc/ir/type.hh
  inc/lcc/lcc-c.h
  inc/lcc/location.hh
  inc/lcc/opt/opt.hh
  inc/lcc/syntax/lexer.hh
  inc/lcc/syntax/token.hh
  inc/lcc/target.hh
  inc/lcc/typedefs.hh
  inc/lcc/utils.hh
  inc/lcc/utils/aint.hh
  inc/lcc/utils/ast_printer.hh
  inc/lcc/utils/dependency_graph.hh
  inc/lcc/utils/fractionals.hh
  inc/lcc/utils/generator.hh
  inc/lcc/utils/ir_printer.hh
  inc/lcc/utils/iterator.hh
  inc/lcc/utils/macros.hh
  inc/lcc/utils/platform.hh
  inc/lcc/utils/result.hh
  inc/lcc/utils/rtti.hh
  inc/lcc/utils/string_distance.hh
  inc/lcc/utils/twocolumnlayouthelper.hh
  lib/lcc/calling_convention.cc
  lib/lcc/calling_conventions/ms_x64.cc
  lib/lcc/calling_conventions/sysv_x86_64.cc
  lib/lcc/codegen/isel.cc
  lib/lcc/codegen/mir.cc
  lib/lcc/codegen/register_allocation.cc
  lib/lcc/codegen/x86_64/assembly.cc
  lib/lcc/codegen/x86_64/object.cc
  lib/lcc/codegen/x86_64/x86_64.cc
  lib/lcc/context.cc
  lib/lcc/diags.cc
  lib/lcc/file.cc
  lib/lcc/init.cc
  lib/lcc/ir/core.cc
  lib/lcc/ir/domtree.cc
  lib/lcc/ir/llvm.cc
  lib/lcc/ir/module.cc
  lib/lcc/ir/module_mir.cc
  lib/lcc/ir/module_wat.cc
  lib/lcc/ir/parser.cc
  lib/lcc/lcc-c.cc
  lib/lcc/location.cc
  lib/lcc/opt/opt.cc
  lib/lcc/platform.cc
  lib/lcc/utils.cc
  lib/lcc/utils/fractionals.cc
  lib/lcc/utils/string_distance.cc
)
target_include_directories(liblcc PUBLIC inc)
set_target_properties(liblcc PROPERTIES OUTPUT_NAME lcc)

if(LCC_USE_DEBUG_ITERATORS)
  target_compile_definitions(liblcc PRIVATE LCC_DEBUG_ITERATORS)
endif()

# Link main lcc library with generic object library.
target_link_libraries(liblcc PUBLIC object)

# Apply options to main lcc library
target_link_libraries(liblcc PRIVATE options)

# Glint language library
add_library(
  glint STATIC
  inc/glint/ast.hh
  inc/glint/ast_eval.hh
  inc/glint/driver.hh
  inc/glint/error_ids.hh
  inc/glint/eval.hh
  inc/glint/ir_gen.hh
  inc/glint/lexer.hh
  inc/glint/parser.hh
  inc/glint/sema.hh
  lib/glint/ast.cc
  lib/glint/ast_deserialise_expr.cc
  lib/glint/ast_deserialise_type.cc
  lib/glint/ast_eval.cc
  lib/glint/ast_module.cc
  lib/glint/ast_serialise_expr.cc
  lib/glint/ast_serialise_type.cc
  lib/glint/convert.cc
  lib/glint/driver.cc
  lib/glint/eval.cc
  lib/glint/init.cc
  lib/glint/ir_gen.cc
  lib/glint/lexer.cc
  lib/glint/parser.cc
  lib/glint/sema.cc
  lib/glint/sema_type.cc
)
target_include_directories(glint PUBLIC inc)
target_link_libraries(glint PRIVATE options)
target_link_libraries(glint PRIVATE object)
target_link_libraries(glint PRIVATE liblcc)

# Add the driver.
add_executable(
  lcc
  src/cli.cc
  src/lcc.cc
  src/sarif.cc
)
target_include_directories(lcc PUBLIC src)

# Apply options to driver.
target_link_libraries(lcc PRIVATE options liblcc)

# Make project version accessible from source code
configure_file(
  src/version.hh.in
  ${CMAKE_CURRENT_BINARY_DIR}/version.hh
  @ONLY
)
target_include_directories(lcc PUBLIC ${CMAKE_CURRENT_BINARY_DIR})

# Link lcc driver with main lcc library.
target_link_libraries(lcc PRIVATE liblcc)

# Link lcc driver with language libraries.
target_link_libraries(lcc PUBLIC glint)

# Copy lcc.html, lcc.js, and lcc.wasm to `docs/` when emscripten.
if (EMSCRIPTEN)
  # Add file-level dependency on HTML template, so if it changes, we
  # actually use the updated template.
  set_target_properties(
    lcc
    PROPERTIES
    LINK_DEPENDS ${CMAKE_CURRENT_LIST_DIR}/res/lcc.in.html
  )
  # Copy lcc.svg to local build (so that a local web server will work).
  add_custom_command(
    TARGET lcc
    POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy
    ${CMAKE_CURRENT_LIST_DIR}/res/lcc.svg
    ${CMAKE_CURRENT_BINARY_DIR}/lcc.svg
    COMMENT "Copying resources for local build..."
  )
  # For release builds, copy generated binaries and resources to LCC
  # Playground hosted on Github Pages.
  if (CMAKE_BUILD_TYPE STREQUAL "Release" OR CMAKE_BUILD_TYPE STREQUAL "MinSizeRel")
    add_custom_command(
      TARGET lcc
      POST_BUILD
      COMMAND ${CMAKE_COMMAND} -E copy
      ${CMAKE_CURRENT_BINARY_DIR}/lcc.html
      ${CMAKE_CURRENT_LIST_DIR}/docs/lcc.html
      COMMAND ${CMAKE_COMMAND} -E copy
      ${CMAKE_CURRENT_BINARY_DIR}/lcc.js
      ${CMAKE_CURRENT_LIST_DIR}/docs/lcc.js
      COMMAND ${CMAKE_COMMAND} -E copy
      ${CMAKE_CURRENT_BINARY_DIR}/lcc.wasm
      ${CMAKE_CURRENT_LIST_DIR}/docs/lcc.wasm
      COMMAND ${CMAKE_COMMAND} -E copy
      ${CMAKE_CURRENT_LIST_DIR}/res/lcc.svg
      ${CMAKE_CURRENT_LIST_DIR}/docs/lcc.svg
      COMMENT "Copying generated files for gh-pages..."
    )
  endif()
endif()

if (BUILD_TESTING)
  message(
    FATAL_ERROR
    "CTest Testing has not yet been re-implemented
    see ${CMAKE_CURRENT_LIST_DIR}/tst/README.org"
  )
endif()
