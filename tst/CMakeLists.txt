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
project(lcctest)

# ============================================================================
#  Executables and libraries.
# ============================================================================

add_subdirectory("./../" "lcc" EXCLUDE_FROM_ALL)

# LangTest
add_subdirectory("./../lib/glint/tests/" "langtest_glint")

# CodeTest
add_subdirectory("./../lib/lcc/tests/x86_64/" "codetest_x86_64")

add_custom_target(
  runtest
  COMMAND
  emacs -Q
  --batch
  --chdir "${CMAKE_CURRENT_LIST_DIR}/../runtest/"
  --eval "(setq lcc-path \"$<TARGET_FILE:lcc>\")"
  --load ./runtest.el
  VERBATIM
  USES_TERMINAL
)

add_custom_target(
  langtest_glint_command
  COMMAND
  glinttests
  DEPENDS
  glinttests
  WORKING_DIRECTORY
  "${CMAKE_CURRENT_LIST_DIR}/../lib/glint/tests/"
  USES_TERMINAL
)

add_custom_target(
  codetest_x86_64_command
  COMMAND
  codetest_x86_64
  DEPENDS
  codetest_x86_64
  WORKING_DIRECTORY
  "${CMAKE_CURRENT_LIST_DIR}/../lib/lcc/tests/x86_64/"
  USES_TERMINAL
)

add_custom_target(
  test
  DEPENDS
  langtest_glint_command
  codetest_x86_64_command
  runtest
)

