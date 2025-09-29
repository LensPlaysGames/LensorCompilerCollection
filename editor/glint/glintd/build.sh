cmake -B bld -DCMAKE_BUILD_TYPE=Debug
cmake --build bld --parallel 12
cp ./bld/glinttools.cpython-313-x86_64-linux-gnu.so ./
