set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

cmake_minimum_required(VERSION 3.14)
project(lccirtest_project)

if (NOT TARGET lcc)
  add_subdirectory("../../" "lcc" EXCLUDE_FROM_ALL)
endif()

add_executable(irtest_target main.cpp)
target_link_libraries(irtest_target options)
target_link_libraries(irtest_target liblcc)
