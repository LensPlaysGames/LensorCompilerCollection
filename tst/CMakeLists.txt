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
if (
    CMAKE_CXX_COMPILER_ID STREQUAL "GNU"
    OR CMAKE_CXX_COMPILER_ID STREQUAL "Clang"
  )
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

# We don't want to build an LCC for no reason, unless it is used by a
# testing facility. We don't need the LCC binary, basically.
add_subdirectory("./../" "lcc" EXCLUDE_FROM_ALL)

# LangTest
add_subdirectory("./langtest/glint/" "langtest_glint")

# CodeTest
add_subdirectory("./codetest/x86_64/" "codetest_x86_64")

add_custom_target(
  runtest
  COMMAND
  emacs -Q
  --batch
  --chdir "${CMAKE_CURRENT_LIST_DIR}/runtest/"
  --eval "(setf lcc-path \"$<TARGET_FILE:lcc>\")"
  --load ./runtest.el
  DEPENDS
  lcc
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
  "${CMAKE_CURRENT_LIST_DIR}/langtest/glint/"
  USES_TERMINAL
)

add_custom_target(
  codetest_x86_64_command
  COMMAND
  codetest_x86_64
  DEPENDS
  codetest_x86_64
  WORKING_DIRECTORY
  "${CMAKE_CURRENT_LIST_DIR}/codetest/x86_64/"
  USES_TERMINAL
)

# langtest target
# |-- Glint LangTest Implementation
# `-- ...
add_custom_target(
  langtest
  DEPENDS
  langtest_glint_command
)

# codetest target
# |-- x86_64
# `-- ...
add_custom_target(
  codetest
  DEPENDS
  codetest_x86_64_command
)

add_custom_target(
  test
  DEPENDS
  langtest
  codetest
  runtest
)

