(cmake -B dbg -DCMAKE_BUILD_TYPE=Debug && cmake --build dbg --parallel 12 && emacs -Q --chdir ./runtest --script ./runtest.el)
