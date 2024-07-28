cmake_minimum_required(VERSION 3.20)
project(LCC VERSION 4.20.0 LANGUAGES C CXX)

# User Option(s)
set(
  NATIVE_OPT TRUE
  CACHE BOOL
  "Whether or not to optimise for the system being compiled on. This
  should be true *unless* you are making distributed binaries."
)

# ============================================================================
#  Global CMake Variables
# ============================================================================
set(CMAKE_EXPORT_COMPILE_COMMANDS ON) # Export compilation database in JSON format.
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
if (CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
    add_compile_options(-fdiagnostics-color=always)
elseif (CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
    add_compile_options(-fcolor-diagnostics)
endif()

# Use mold as the default linker, if it exists.
if (CMAKE_CXX_COMPILER_ID STREQUAL "GNU" OR CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
    find_program(MOLD_LINKER "mold")
    if (MOLD_LINKER)
        add_link_options(-fuse-ld=mold)
    endif()
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
        $<$<CONFIG:RELEASE>:-O3 -march=native>
    )

    target_link_options(options INTERFACE
        $<$<CONFIG:DEBUG>:-O0 -g3 -ggdb3 -rdynamic>
        $<$<CONFIG:RELEASE>:-O3 -march=native>
    )
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
    GIT_TAG 10.2.1
    SOURCE_DIR "${CMAKE_CURRENT_BINARY_DIR}/libs/fmt"
)
FetchContent_MakeAvailable(fmtlib)
target_include_directories(options INTERFACE ${fmtlib_SOURCE_DIR}/include)

# Link against libfmt.
target_link_libraries(options INTERFACE fmt)

# Add ‘include’ as an include dir.
target_include_directories(options INTERFACE include)

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

# Glint language library
add_library(
  glint STATIC
  include/glint/ast.hh
  include/glint/eval.hh
  include/glint/ir_gen.hh
  include/glint/lexer.hh
  include/glint/parser.hh
  include/glint/sema.hh
  lib/glint/ast.cc
  lib/glint/ast_module.cc
  lib/glint/eval.cc
  lib/glint/init.cc
  lib/glint/ir_gen.cc
  lib/glint/lexer.cc
  lib/glint/parser.cc
  lib/glint/sema.cc
)
target_include_directories(glint PUBLIC include)
target_link_libraries(glint PRIVATE options)
target_link_libraries(glint PRIVATE object)

# Generic object library (convertible to ELF and COFF, currently)
add_library(
  object STATIC
  include/object/elf.h
  include/object/elf.hh
  include/object/generic.hh
  lib/object/elf.cc
  lib/object/generic.cc
)
target_include_directories(object PUBLIC include)
target_link_libraries(object PRIVATE options)

# Add the main lcc library.
add_library(
  liblcc STATIC
  include/lcc/calling_conventions/sysv_x86_64.hh
  include/lcc/codegen/gnu_as_att_assembly.hh
  include/lcc/codegen/isel.hh
  include/lcc/codegen/mir.hh
  include/lcc/codegen/mir_utils.hh
  include/lcc/codegen/register_allocation.hh
  include/lcc/codegen/x86_64/assembly.hh
  include/lcc/codegen/x86_64/isel_patterns.hh
  include/lcc/codegen/x86_64/object.hh
  include/lcc/codegen/x86_64/x86_64.hh
  include/lcc/context.hh
  include/lcc/core.hh
  include/lcc/diags.hh
  include/lcc/file.hh
  include/lcc/format.hh
  include/lcc/forward.hh
  include/lcc/ir/domtree.hh
  include/lcc/ir/ir.hh
  include/lcc/ir/module.hh
  include/lcc/ir/type.hh
  include/lcc/lcc-c.h
  include/lcc/location.hh
  include/lcc/opt/opt.hh
  include/lcc/syntax/lexer.hh
  include/lcc/syntax/token.hh
  include/lcc/target.hh
  include/lcc/utils.hh
  include/lcc/utils/aint.hh
  include/lcc/utils/ast_printer.hh
  include/lcc/utils/dependency_graph.hh
  include/lcc/utils/generator.hh
  include/lcc/utils/ir_printer.hh
  include/lcc/utils/iterator.hh
  include/lcc/utils/macros.hh
  include/lcc/utils/platform.hh
  include/lcc/utils/result.hh
  include/lcc/utils/rtti.hh
  include/lcc/utils/twocolumnlayouthelper.hh
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
  lib/lcc/ir/domtree.cc
  lib/lcc/ir/ir.cc
  lib/lcc/ir/llvm.cc
  lib/lcc/ir/module.cc
  lib/lcc/ir/module_mir.cc
  lib/lcc/ir/parser.cc
  lib/lcc/lcc-c.cc
  lib/lcc/location.cc
  lib/lcc/opt/opt.cc
  lib/lcc/platform.cc
  lib/lcc/utils.cc
)
target_include_directories(liblcc PUBLIC include)
set_target_properties(liblcc PROPERTIES OUTPUT_NAME lcc)

# Link main lcc library with generic object library.
target_link_libraries(liblcc PUBLIC object)

# Add the driver.
add_executable(
  lcc
  src/cli.cc
  src/lcc.cc
)
target_include_directories(lcc PUBLIC src)

# Link lcc driver with language libraries.
target_link_libraries(lcc PUBLIC glint)

# Apply options.
target_link_libraries(liblcc PRIVATE options)
target_link_libraries(lcc PRIVATE options liblcc)

if (BUILD_TESTING)
  # TODO
  message(FATAL_ERROR "Testing has not yet been re-implemented")
endif()
