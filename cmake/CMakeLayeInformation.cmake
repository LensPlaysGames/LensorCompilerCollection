set(CMAKE_Laye_OUTPUT_EXTENSION .s)
# TODO When Lens_r writes an assembler again, do this thing.
# if(UNIX)
#   set(CMAKE_Laye_OUTPUT_EXTENSION .o)
# else()
#   set(CMAKE_Laye_OUTPUT_EXTENSION .obj)
# endif()

set(CMAKE_INCLUDE_FLAG_Laye "-I ")

# set(CMAKE_Laye_CREATE_SHARED_LIBRARY)
# set(CMAKE_Laye_CREATE_SHARED_MODULE)
# set(CMAKE_Laye_CREATE_STATIC_LIBRARY)

if(NOT CMAKE_Laye_COMPILE_OBJECT)
  set(
    CMAKE_Laye_COMPILE_OBJECT
    "<CMAKE_Laye_COMPILER> <DEFINES> <INCLUDES> <FLAGS> -f asm -o <OBJECT> <SOURCE>"
  )
endif()

if(NOT CMAKE_Laye_LINK_EXECUTABLE)
  set(
    CMAKE_Laye_LINK_EXECUTABLE
    "<CMAKE_C_COMPILER> <FLAGS> <CMAKE_C_LINK_FLAGS> <LINK_FLAGS> <OBJECTS> -o <TARGET> <LINK_LIBRARIES>"
  )
endif()

set(CMAKE_Laye_INFORMATION_LOADED 1)
