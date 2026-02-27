set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

cmake_minimum_required(VERSION 3.14)
project(glinttests)

if (NOT TARGET lcc)
  add_subdirectory("../../../" "lcc" EXCLUDE_FROM_ALL)
endif()

add_executable(glinttests main.cpp)
target_link_libraries(glinttests options)
target_link_libraries(glinttests liblcc)
target_link_libraries(glinttests glint)

target_compile_definitions(
  glint PUBLIC LCC_LANGTEST
)
