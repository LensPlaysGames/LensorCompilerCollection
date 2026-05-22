set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

cmake_minimum_required(VERSION 3.14)
project(lccctests)

if (NOT TARGET lcc)
  add_subdirectory("../../../" "lcc" EXCLUDE_FROM_ALL)
endif()

add_executable(lccctests main.cpp)
target_link_libraries(lccctests options)
target_link_libraries(lccctests liblcc)
target_link_libraries(lccctests liblccjson)
target_link_libraries(lccctests languagec)

target_compile_definitions(
  languagec PUBLIC LCC_LANGTEST
)
