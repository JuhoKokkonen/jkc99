#!/bin/sh

TEST_DIR=`dirname "$(readlink -f "$0")"`
JKC99_DIR=$TEST_DIR/..
OUTPUT_DIR=$TEST_DIR/introspect_output

CLANG_FLAGS="-g -std=c99 -pedantic -Wall -Wextra -Werror -Wno-missing-field-initializers -Wno-unknown-pragmas -Wno-unused-parameter -D_XOPEN_SOURCE=500"

mkdir $OUTPUT_DIR 2>/dev/null
rm $OUTPUT_DIR/* 2>/dev/null

cd $OUTPUT_DIR

clang $CLANG_FLAGS -E $TEST_DIR/introspect_test.c -o introspect_test.i
if [ $? -ne 0 ]; then
    exit 1
fi
$JKC99_DIR/bin/jkc99 introspect_test.i introspect print > introspect_test_out.i
if [ $? -ne 0 ]; then
    exit 1
fi
clang $CLANG_FLAGS introspect_test_out.i -o introspect_test
if [ $? -ne 0 ]; then
    exit 1
fi
./introspect_test

cd ..
