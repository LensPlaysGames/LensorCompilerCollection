cmake_minimum_required(VERSION 3.14)

# Use `ccache` if it is installed in system's PATH.
find_program(CCACHE_PROGRAM ccache)
if(CCACHE_PROGRAM)
  set_property(GLOBAL PROPERTY RULE_LAUNCH_COMPILE "${CCACHE_PROGRAM}")
endif()

# Export compilation database in JSON format.
set(CMAKE_EXPORT_COMPILE_COMMANDS on)

project(FUNCompiler C)

add_executable(
  func
  src/codegen.c
  src/error.c
  src/environment.c
  src/file_io.c
  src/main.c
  src/parser.c
  src/typechecker.c
)
target_include_directories(
  func
  PUBLIC src/
)
