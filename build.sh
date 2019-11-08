#!/bin/sh

JKC99_DIR=`dirname "$(readlink -f "$0")"`

mkdir build 2>/dev/null
mkdir bin 2>/dev/null

cd build

CLANG_FLAGS="-g -std=c99 -pedantic -Wall -Wextra -Werror -Wno-missing-field-initializers -Wno-unknown-pragmas -D_XOPEN_SOURCE=500"

clang $CLANG_FLAGS $JKC99_DIR/src/jkc99.c -ldl -o jkc99
for f in $JKC99_DIR/src/modules/*.c 
do 
    clang $CLANG_FLAGS -shared -fPIC $f -o `basename $f .c`.so
done

mv -u jkc99 ../bin 2>/dev/null
mv -u *.so ../bin 2>/dev/null

cd ..
