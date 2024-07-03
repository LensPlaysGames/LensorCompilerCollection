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

# Enable asan if requested and possible.
if (NOT WIN32)
    if (ENABLE_ASAN)
        target_compile_options(options INTERFACE -fsanitize=address)
        target_compile_definitions(options INTERFACE ENABLE_ASAN=1)
        target_link_options(options INTERFACE -fsanitize=address)

        # Make address sanitiser ignore memory leaks. This is useful if there are
        # more severe problems (e.g. use-after-free bugs) that need to be fixed.
        if (ASAN_IGNORE_LEAKS)
            target_compile_definitions(options INTERFACE ASAN_IGNORE_LEAKS=1)
        endif ()
    endif()
endif ()

# ============================================================================
#  libfmt and include dirs.
# ============================================================================
# Find libfmt.
include(FetchContent)
set(FETCHCONTENT_QUIET OFF)
set(FETCHCONTENT_UPDATES_DISCONNECTED ON)
message(STATUS "Checking dependencies ...")
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

# Precompile utils.hh.
target_precompile_headers(options INTERFACE "${PROJECT_SOURCE_DIR}/include/lcc/utils.hh")

# ============================================================================
#  Executables and libraries.
# ============================================================================
# ‘include’ and ‘lib’ are library sources.
file(GLOB_RECURSE library-sources
    "${PROJECT_SOURCE_DIR}/lib/*.cc"
    "${PROJECT_SOURCE_DIR}/include/*.hh"
)

# Add the main lcc library
add_library(liblcc STATIC ${library-sources})
set_target_properties(liblcc PROPERTIES OUTPUT_NAME lcc)

# Add the driver.
add_executable(lcc "${PROJECT_SOURCE_DIR}/src/lcc.cc")

# Set public include dir for lcc.
target_include_directories(liblcc PUBLIC "${PROJECT_SOURCE_DIR}/include")

# Apply our options.
target_link_libraries(liblcc PRIVATE options)
target_link_libraries(lcc PRIVATE options liblcc)

if (BUILD_TESTING)
  # TODO
  message(FATAL_ERROR "Testing has not yet been re-implemented")
endif()
