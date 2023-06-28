cmake_minimum_required(VERSION 3.14)

project(Intercept-Format-Check-Plugin C CXX)

find_package(LLVM CONFIG)
message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")

## Add the plugin as a target.
set (CMAKE_CXX_STANDARD 20)
add_library(fmt-check-plugin SHARED "${PROJECT_SOURCE_DIR}/fmt-check-plugin.cc")

## LLVM include dirs and definitions.
target_include_directories(fmt-check-plugin PRIVATE ${LLVM_INCLUDE_DIRS})
separate_arguments(LLVM_DEFINITIONS_LIST NATIVE_COMMAND ${LLVM_DEFINITIONS})
target_compile_definitions(fmt-check-plugin PRIVATE ${LLVM_DEFINITIONS_LIST})

## LLVM libs.
llvm_map_components_to_libnames(llvm_libs all)
target_link_libraries(fmt-check-plugin PRIVATE ${llvm_libs})

## These are platform-specific, so yeet them.
set_target_properties(fmt-check-plugin PROPERTIES PREFIX "")
set_target_properties(fmt-check-plugin PROPERTIES SUFFIX "")

## Disable this since is a bit annoying.
add_compile_options(-Wno-unused-command-line-argument)

## Compiling this plugin takes a bit.
message(STATUS "Compiling format check plugin. This may take a while...")