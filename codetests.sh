(cd ./lib/lcc/tests/x86_64 && cmake -B bld -DCMAKE_BUILD_TYPE=Debug && cmake --build bld --parallel 12 && ./bld/codetest_x86_64)
