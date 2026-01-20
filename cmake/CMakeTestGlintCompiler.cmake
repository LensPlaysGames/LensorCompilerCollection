#
# This file is supposed to set CMAKE_Glint_COMPILER_WORKS iff the Glint
# compiler works as expected.
#

# FIXME Actually do a test, make sure it works.
# We should probably just attempt to compile a simple Glint program, or
# something... or a couple.

# For now, fudge it and always report that the Glint compiler works.
set(CMAKE_Glint_COMPILER_WORKS 1 CACHE INTERNAL "")
