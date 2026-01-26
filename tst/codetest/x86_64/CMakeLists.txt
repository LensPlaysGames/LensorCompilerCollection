set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

cmake_minimum_required(VERSION 3.14)
project(codetest_x86_64)

if (NOT TARGET lcc)
  add_subdirectory("../../../../" "lcc" EXCLUDE_FROM_ALL)
endif()

add_executable(codetest_x86_64 main.cpp)
target_link_libraries(codetest_x86_64 options)
target_link_libraries(codetest_x86_64 liblcc)
