set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

cmake_minimum_required(VERSION 3.14)
project(glinttests)

add_subdirectory("../../../" "lcc")
set_target_properties(
  lcc
  PROPERTIES
  EXCLUDE_FROM_ALL ON
)

add_executable(glinttests main.cpp)
target_link_libraries(glinttests options)
target_link_libraries(glinttests liblcc)
target_link_libraries(glinttests glint)
