set(CMAKE_Int_OUTPUT_EXTENSION .s)
# TODO When I write an assembler again, do this thing.
# if(UNIX)
#   set(CMAKE_Int_OUTPUT_EXTENSION .o)
# else()
#   set(CMAKE_Int_OUTPUT_EXTENSION .obj)
# endif()

set(CMAKE_INCLUDE_FLAG_Int "-I ")

# variables supplied by the generator at use time for rules
# <TARGET>
# <TARGET_BASE> the target without the suffix
# <OBJECTS>
# <OBJECT>
# <LINK_LIBRARIES>
# <FLAGS>
# <LINK_FLAGS>

# set(CMAKE_Int_CREATE_SHARED_LIBRARY)
# set(CMAKE_Int_CREATE_SHARED_MODULE)

if(NOT CMAKE_Int_COMPILE_OBJECT)
  set(
    CMAKE_Int_COMPILE_OBJECT
    "<CMAKE_Int_COMPILER> <DEFINES> <INCLUDES> <FLAGS> -f asm -o <OBJECT> <SOURCE>"
  )
endif()

if(NOT CMAKE_Int_CREATE_STATIC_LIBRARY)
  set(
    CMAKE_Int_CREATE_STATIC_LIBRARY
    "<CMAKE_C_COMPILER> <FLAGS> <CMAKE_C_LINK_FLAGS> <LINK_FLAGS> -c <OBJECTS> -o <TARGET> <LINK_LIBRARIES>"
  )
endif()

if(NOT CMAKE_Int_LINK_EXECUTABLE)
  set(
    CMAKE_Int_LINK_EXECUTABLE
    "<CMAKE_C_COMPILER> <FLAGS> <CMAKE_C_LINK_FLAGS> <LINK_FLAGS> <OBJECTS> -o <TARGET> <LINK_LIBRARIES>"
  )
endif()

set(CMAKE_Int_INFORMATION_LOADED 1)
