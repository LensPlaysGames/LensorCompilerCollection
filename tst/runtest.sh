#!/usr/bin/sh
# Run this script with the LCC repository root as current working
# directory.
(cmake -B dbg -DCMAKE_BUILD_TYPE=Debug && cmake --build dbg --parallel 12 && emacs -Q --chdir ./runtest --script ./runtest.el)
