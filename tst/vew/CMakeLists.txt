set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

cmake_minimum_required(VERSION 3.14)
project(vew_project)

if (NOT TARGET lcc)
  add_subdirectory("../../" "lcc" EXCLUDE_FROM_ALL)
endif()

add_executable(vew main.cpp)
target_link_libraries(vew fmt)
target_link_libraries(vew options)
