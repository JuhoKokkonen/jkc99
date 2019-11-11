#!/bin/sh

JKC99_RUN_UNIT_TESTS=1

JKC99_DIR=`dirname "$(readlink -f "$0")"`

cd $JKC99_DIR

mkdir build 2>/dev/null
mkdir bin 2>/dev/null

cd build

CLANG_FLAGS="-g -std=c99 -pedantic -Wall -Wextra -Werror -Wno-missing-field-initializers -Wno-unknown-pragmas -D_XOPEN_SOURCE=500"

clang $CLANG_FLAGS -Wl,--export-dynamic $JKC99_DIR/src/jkc99.c -ldl -o jkc99
if [ $? -ne 0 ]; then
    exit 1
fi
for f in $JKC99_DIR/src/modules/*.c 
do 
    clang $CLANG_FLAGS -shared -fPIC $f -o `basename $f .c`.so
    if [ $? -ne 0 ]; then
        exit 1
    fi
done

mv -u jkc99 ../bin 2>/dev/null
mv -u *.so ../bin 2>/dev/null

if [ $JKC99_RUN_UNIT_TESTS -eq 1 ]; then
    clang $CLANG_FLAGS -DJKC99_TEST_BUILD -E $JKC99_DIR/src/jkc99.c -o jkc99.i
    if [ $? -ne 0 ]; then
        exit 1
    fi
    $JKC99_DIR/bin/jkc99 jkc99.i test print > jkc99_test.i
    if [ $? -ne 0 ]; then
        exit 1
    fi
    clang $CLANG_FLAGS -DJKC99_TEST_BUILD -Wl,--export-dynamic jkc99_test.i -ldl -o jkc99_test
    if [ $? -ne 0 ]; then
        exit 1
    fi
    ./jkc99_test
    if [ $? -ne 0 ]; then
        exit 1
    fi
fi
