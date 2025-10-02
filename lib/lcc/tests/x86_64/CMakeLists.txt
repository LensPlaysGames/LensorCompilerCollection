set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

cmake_minimum_required(VERSION 3.14)
project(codetest_x86_64)

add_subdirectory("../../../../" "lcc")
set_target_properties(
  lcc
  PROPERTIES
  EXCLUDE_FROM_ALL ON
)

add_executable(codetest_x86_64 main.cpp)
target_link_libraries(codetest_x86_64 options)
target_link_libraries(codetest_x86_64 liblcc)
