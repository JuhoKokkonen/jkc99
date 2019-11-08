#!/bin/sh

TEST_DIR=`dirname "$(readlink -f "$0")"`

CLANG_FLAGS="-g -std=c99 -pedantic -Wall -Wextra -Werror -Wno-missing-field-initializers -Wno-unknown-pragmas -Wno-unused-parameter -D_XOPEN_SOURCE=500"

rm introspect_test.i
rm introspect_test_out.i
rm introspect_test

clang $CLANG_FLAGS -E $TEST_DIR/introspect_test.c -o introspect_test.i
../bin/jkc99 introspect_test.i introspect > introspect_test_out.i
clang $CLANG_FLAGS $TEST_DIR/introspect_test_out.i -o introspect_test
./introspect_test
