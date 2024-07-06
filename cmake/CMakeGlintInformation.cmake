set(CMAKE_Glint_OUTPUT_EXTENSION .s)
# TODO When I write an assembler again, do this thing.
# if(UNIX)
#   set(CMAKE_Glint_OUTPUT_EXTENSION .o)
# else()
#   set(CMAKE_Glint_OUTPUT_EXTENSION .obj)
# endif()

set(CMAKE_INCLUDE_FLAG_Glint "-I ")

# variables supplied by the generator at use time for rules
# <TARGET>
# <TARGET_BASE> the target without the suffix
# <OBJECTS>
# <OBJECT>
# <LINK_LIBRARIES>
# <FLAGS>
# <LINK_FLAGS>

# set(CMAKE_Glint_CREATE_SHARED_LIBRARY)
# set(CMAKE_Glint_CREATE_SHARED_MODULE)

if(NOT CMAKE_Glint_COMPILE_OBJECT)
  set(
    CMAKE_Glint_COMPILE_OBJECT
    "<CMAKE_Glint_COMPILER> <DEFINES> <INCLUDES> <FLAGS> -f asm -o <OBJECT> <SOURCE>"
  )
endif()

if(NOT CMAKE_Glint_CREATE_STATIC_LIBRARY)
  set(
    CMAKE_Glint_CREATE_STATIC_LIBRARY
    "<CMAKE_C_COMPILER> <FLAGS> <CMAKE_C_LINK_FLAGS> <LINK_FLAGS> -c <OBJECTS> -o <TARGET> <LINK_LIBRARIES>"
  )
  set(CMAKE_Glint_ARCHIVE_CREATE "<CMAKE_AR> crs <TARGET> <OBJECTS>")
  set(CMAKE_Glint_ARCHIVE_FINISH "")
endif()

if(NOT CMAKE_Glint_LINK_EXECUTABLE)
  set(
    CMAKE_Glint_LINK_EXECUTABLE
    "<CMAKE_C_COMPILER> <FLAGS> <CMAKE_C_LINK_FLAGS> <LINK_FLAGS> <OBJECTS> -o <TARGET> <LINK_LIBRARIES>"
  )
endif()

set(CMAKE_Glint_INFORMATION_LOADED 1)
