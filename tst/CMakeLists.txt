# User Options
set(
  VERIFY_COMPILERS ON
  CACHE BOOL
  "Whether or not to build full GCC and full LLVM builds.
  This helps verify that LCC code is standards-compliant and portable."
)

# Global Configuration

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

cmake_minimum_required(VERSION 3.24)
project(lcctest)

# ============================================================================
#  Executables and libraries.
# ============================================================================

# We don't want to build an LCC for no reason, unless it is used by a
# testing facility. We don't need the LCC binary, basically.
add_subdirectory("./../" "lcc" EXCLUDE_FROM_ALL)

if (USE_LTO)
  # Do link time optimisation
  # NOTE: We must ensure the linker matches the compiler toolchain for LTO
  # to work properly.
  if (CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
    add_compile_options(-flto)
    add_link_options(-flto -fuse-ld=bfd)
  elseif (CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
    add_compile_options(-flto)
    add_link_options(-flto -fuse-ld=lld)
  elseif (MSVC)
    add_compile_options(/GL)
    add_link_options(/LTCG)
  endif()
endif()

add_subdirectory("./vew/" "vew" EXCLUDE_FROM_ALL)

# LangTest
add_subdirectory("./langtest/glint/" "langtest_glint")
add_subdirectory("./langtest/c/" "langtest_c")

# CodeTest
add_subdirectory("./codetest/x86_64/" "codetest_x86_64")

# IRTest
add_subdirectory("./irtest/" "irtest")

# RunTest
add_custom_target(
  runtest
  COMMAND
  emacs -Q
  --batch
  --chdir "${CMAKE_CURRENT_LIST_DIR}/runtest/"
  --eval "(setf lcc-path \"$<TARGET_FILE:lcc>\")"
  --load ./runtest.el
  -f run-test--main
  DEPENDS
  lcc
  BYPRODUCTS
  ${CMAKE_CURRENT_LIST_DIR}/runtest/runtest.sarif
  VERBATIM
  USES_TERMINAL
)

# IRTest
add_custom_target(
  irtest
  COMMAND
  irtest_target
  DEPENDS
  irtest_target
  BYPRODUCTS
  ${CMAKE_CURRENT_LIST_DIR}/irtest/irtest.sarif
  WORKING_DIRECTORY
  "${CMAKE_CURRENT_LIST_DIR}/irtest/"
  USES_TERMINAL
)

add_custom_target(
  langtest_glint_command
  COMMAND
  glinttests
  DEPENDS
  glinttests
  BYPRODUCTS
  ${CMAKE_CURRENT_LIST_DIR}/langtest/glint/langtest.sarif
  WORKING_DIRECTORY
  "${CMAKE_CURRENT_LIST_DIR}/langtest/glint/"
  USES_TERMINAL
)
add_custom_target(
  langtest_c_command
  COMMAND
  lccctests
  DEPENDS
  lccctests
  BYPRODUCTS
  ${CMAKE_CURRENT_LIST_DIR}/langtest/c/langtest.sarif
  WORKING_DIRECTORY
  "${CMAKE_CURRENT_LIST_DIR}/langtest/c/"
  USES_TERMINAL
)

add_custom_target(
  codetest_x86_64_command
  COMMAND
  codetest_x86_64
  DEPENDS
  codetest_x86_64
  BYPRODUCTS
  ${CMAKE_CURRENT_LIST_DIR}/codetest/x86_64/codetest.sarif
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
  langtest_c_command
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
  irtest
)

add_custom_target(
  view_command
  COMMAND
  vew
  ./codetest/x86_64/codetest.sarif
  ./irtest/irtest.sarif
  ./langtest/glint/langtest.sarif
  ./langtest/c/langtest.sarif
  ./runtest/runtest.sarif
  DEPENDS
  vew
  WORKING_DIRECTORY
  ${CMAKE_CURRENT_LIST_DIR}
  VERBATIM
  USES_TERMINAL
)

add_custom_target(
  test_and_view_command
  COMMAND
  vew
  ./codetest/x86_64/codetest.sarif
  ./irtest/irtest.sarif
  ./langtest/glint/langtest.sarif
  ./langtest/c/langtest.sarif
  ./runtest/runtest.sarif
  DEPENDS
  vew
  ./codetest/x86_64/codetest.sarif
  ./irtest/irtest.sarif
  ./langtest/glint/langtest.sarif
  ./langtest/c/langtest.sarif
  ./runtest/runtest.sarif
  test
  WORKING_DIRECTORY
  ${CMAKE_CURRENT_LIST_DIR}
  VERBATIM
  USES_TERMINAL
)

add_custom_target(
  view
  DEPENDS
  view_command
)

add_custom_target(
  test_and_view
  DEPENDS
  test_and_view_command
)

if (VERIFY_COMPILERS)
  include(ExternalProject)

  set(
    COMMON_CONFIG
    -DUSE_LTO=OFF
    -G ${CMAKE_GENERATOR} # pass through generator
  )

  add_custom_target(verify_compilers)

  find_program(GCC_PROGRAM gcc)
  find_program(GXX_PROGRAM g++)
  if (GCC_PROGRAM AND GXX_PROGRAM)
    ExternalProject_Add(
      gnu_build
      SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/..
      BINARY_DIR ${CMAKE_CURRENT_BINARY_DIR}/thegnubuild
      DOWNLOAD_COMMAND ""
      UPDATE_COMMAND ""
      INSTALL_COMMAND ""
      CMAKE_ARGS
      -DCMAKE_C_COMPILER=gcc
      -DCMAKE_CXX_COMPILER=g++
      ${COMMON_CONFIG}
    )
    add_dependencies(verify_compilers gnu_build)
    set_property(DIRECTORY APPEND PROPERTY
      ADDITIONAL_CLEAN_FILES "${CMAKE_CURRENT_BINARY_DIR}/thegnubuild"
    )
  else()
    message(STATUS "VERIFY_COMPILERS: gcc compiler verification is not available")
  endif()

  find_program(CLANG_PROGRAM clang)
  find_program(CLANGXX_PROGRAM clang++)
  if (CLANG_PROGRAM AND CLANGXX_PROGRAM)
    ExternalProject_Add(
      llvm_build
      SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/..
      BINARY_DIR ${CMAKE_CURRENT_BINARY_DIR}/thellvmbuild
      DOWNLOAD_COMMAND ""
      UPDATE_COMMAND ""
      INSTALL_COMMAND ""
      CMAKE_ARGS
      -DCMAKE_C_COMPILER=clang
      -DCMAKE_CXX_COMPILER=clang++
      -DCMAKE_CXX_FLAGS="-stdlib=libc++"
      ${COMMON_CONFIG}
    )
    add_dependencies(verify_compilers llvm_build)
    set_property(DIRECTORY APPEND PROPERTY
      ADDITIONAL_CLEAN_FILES "${CMAKE_CURRENT_BINARY_DIR}/thellvmbuild"
    )
    # LLVM Compiler, GNU Standard Library
    ExternalProject_Add(
      llvmgnu_build
      SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/..
      BINARY_DIR ${CMAKE_CURRENT_BINARY_DIR}/thellvmgnubuild
      DOWNLOAD_COMMAND ""
      UPDATE_COMMAND ""
      INSTALL_COMMAND ""
      CMAKE_ARGS
      -DCMAKE_C_COMPILER=clang
      -DCMAKE_CXX_COMPILER=clang++
      -DCMAKE_CXX_FLAGS="-stdlib=libstdc++"
      ${COMMON_CONFIG}
    )
    add_dependencies(verify_compilers llvmgnu_build)
    set_property(DIRECTORY APPEND PROPERTY
      ADDITIONAL_CLEAN_FILES "${CMAKE_CURRENT_BINARY_DIR}/thellvmgnubuild"
    )
  else()
    message(STATUS "VERIFY_COMPILERS: clang compiler verification is not available")
  endif()
  add_dependencies(test verify_compilers)

  find_program(MINGW_CXX NAMES ${CMAKE_SYSTEM_PROCESSOR}-w64-mingw32-g++)
  if (MINGW_CXX)
    ExternalProject_Add(
      mingw_build
      SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/..
      BINARY_DIR ${CMAKE_CURRENT_BINARY_DIR}/themingwbuild
      DOWNLOAD_COMMAND ""
      UPDATE_COMMAND ""
      INSTALL_COMMAND ""
      CMAKE_ARGS
      -DCMAKE_C_COMPILER=${CMAKE_SYSTEM_PROCESSOR}-w64-mingw32-gcc
      -DCMAKE_CXX_COMPILER=${CMAKE_SYSTEM_PROCESSOR}-w64-mingw32-g++
      ${COMMON_CONFIG}
    )
    add_dependencies(verify_compilers mingw_build)
    set_property(DIRECTORY APPEND PROPERTY
      ADDITIONAL_CLEAN_FILES "${CMAKE_CURRENT_BINARY_DIR}/themingwbuild"
    )
  else()
    message(STATUS "VERIFY_COMPILERS: mingw compiler verificaiton is not available")
  endif()

  find_program(UCRT_CXX NAMES ${CMAKE_SYSTEM_PROCESSOR}-w64-mingw32ucrt-g++)
  if (UCRT_CXX)
    ExternalProject_Add(
      ucrt_build
      SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/..
      BINARY_DIR ${CMAKE_CURRENT_BINARY_DIR}/theucrtbuild
      DOWNLOAD_COMMAND ""
      UPDATE_COMMAND ""
      INSTALL_COMMAND ""
      CMAKE_ARGS
      -DCMAKE_C_COMPILER=${CMAKE_SYSTEM_PROCESSOR}-w64-mingw32ucrt-gcc
      -DCMAKE_CXX_COMPILER=${CMAKE_SYSTEM_PROCESSOR}-w64-mingw32ucrt-g++
      ${COMMON_CONFIG}
    )
    add_dependencies(verify_compilers ucrt_build)
    set_property(DIRECTORY APPEND PROPERTY
      ADDITIONAL_CLEAN_FILES "${CMAKE_CURRENT_BINARY_DIR}/theucrtbuild"
    )
  else()
    message(STATUS "VERIFY_COMPILERS: ucrt compiler verificaiton is not available")
  endif()
endif()
