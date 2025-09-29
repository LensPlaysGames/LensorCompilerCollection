set(CMAKE_EXPORT_COMPILE_COMMANDS ON) # Export compilation database in JSON format.
set(CMAKE_C_STANDARD 11)
set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

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

cmake_minimum_required(VERSION 3.24)
project(glinttools LANGUAGES CXX)

# ================================================================
#  Compiler Options
# ================================================================
add_library(glinttools_options INTERFACE)

# Flags for Clang and GCC.
if (CMAKE_CXX_COMPILER_ID STREQUAL "GNU" OR CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
    target_compile_options(glinttools_options INTERFACE
        # Warnings.
        -Wall -Wextra     # Enable ‘all’ warnings.
        -Wundef           # Invalid #undef or undefined macro in #if.
        -Wcast-align      # Casting that changes alignment.
        -Wconversion      # Implicit conversions.
        -Wsign-conversion # Implicit sign conversions.
        # -Werror=shadow    # We don't want shadowing, but, pybind11 does it like ALL the time
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
        target_compile_options(glinttools_options INTERFACE
            $<$<CONFIG:RELEASE>:-march=native>
        )
    endif()
endif()

# Additional flags for GCC.
if (CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
    target_compile_options(glinttools_options INTERFACE
        -Wlogical-op      # Duplicate or unintended logical operators.
        -Werror=invalid-memory-model # For atomics.
        -Werror=maybe-uninitialized
        -Werror=missing-requires
        -Werror=return-local-addr
    )
endif()

# Additional flags for Clang.
if (CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
    target_compile_options(glinttools_options INTERFACE
        -Werror=dangling
        -Werror=return-stack-address
    )
endif()

# Flags for MSVC/clang-cl.
if (MSVC)
    target_compile_options(glinttools_options INTERFACE
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
    target_compile_definitions(glinttools_options INTERFACE
        _CRT_SECURE_NO_WARNINGS
        _CRT_SECURE_NO_WARNINGS_GLOBALS
        _CRT_NONSTDC_NO_WARNINGS
    )
endif()

# Debug/Release flags.
if (NOT MSVC)
    if (NOT CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
        target_compile_options(glinttools_options INTERFACE $<$<CONFIG:DEBUG>:-rdynamic>)
    else()
        target_compile_options(glinttools_options INTERFACE -Wno-unused-private-field)
    endif()

    target_compile_options(glinttools_options INTERFACE
        $<$<CONFIG:DEBUG>:-O0 -g3 -ggdb3>
        $<$<CONFIG:RELEASE>:-O3 -march=native>
    )

    target_link_options(glinttools_options INTERFACE
        $<$<CONFIG:DEBUG>:-O0 -g3 -ggdb3 -rdynamic>
        $<$<CONFIG:RELEASE>:-O3 -march=native>
    )
else()
    target_compile_options(glinttools_options INTERFACE
        $<$<CONFIG:DEBUG>:/Od>
        $<$<CONFIG:RELEASE>:/O2>
    )
endif()

# Enable AddressSanitizer if requested
if (ENABLE_ASAN)
  target_compile_options(glinttools_options INTERFACE -fsanitize=address)
  target_link_options(glinttools_options INTERFACE -fsanitize=address)
  target_compile_definitions(glinttools_options INTERFACE ENABLE_ASAN=1)
endif()

# ================================================================
#  Dependencies
# ================================================================

# Find pybind11.
include(FetchContent)
set(FETCHCONTENT_QUIET OFF)
set(FETCHCONTENT_UPDATES_DISCONNECTED ON)
message(STATUS "Checking dependencies ...")
# NOTE: If you don't like cloning every time you configure a new build
# tree, you can specify `pybind11_SOURCE_DIR` as pointing to the cloned
# repo.
FetchContent_Declare(
    pybind11
    GIT_REPOSITORY https://github.com/pybind/pybind11.git
    GIT_TAG v3.0.1
    SOURCE_DIR "${CMAKE_CURRENT_BINARY_DIR}/libs/pybind11"
)
FetchContent_MakeAvailable(pybind11)

target_include_directories(glinttools_options INTERFACE ${pybind11_SOURCE_DIR}/include)

# Link against libfmt.
target_link_libraries(glinttools_options INTERFACE fmt)

# Add ‘include’ as an include dir.
# target_include_directories(glinttools_options INTERFACE include)

# Do not link with libm (math) when target is windows executable.
if (NOT WIN32)
  target_link_libraries(glinttools_options INTERFACE m)
endif()

# set_property(TARGET glinttools PROPERTY CMAKE_POSITION_INDEPENDENT_CODE ON)
set(CMAKE_POSITION_INDEPENDENT_CODE ON)

add_subdirectory("./../../../" "${CMAKE_CURRENT_BINARY_DIR}/lcc" EXCLUDE_FROM_ALL)

find_package(Python COMPONENTS Interpreter Development REQUIRED)

pybind11_add_module(glinttools SHARED src/main.cpp)
target_include_directories(glinttools PUBLIC ${Python_INCLUDE_DIRS})
target_link_libraries(glinttools PUBLIC ${PYTHON_LIBRARIES} glinttools_options liblcc glint)
