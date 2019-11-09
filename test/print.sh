#!/bin/sh
TEST_DIR=`dirname "$(readlink -f "$0")"`
JKC99_DIR=$TEST_DIR/..
OUTPUT_DIR=$TEST_DIR/print_output

CLANG_FLAGS="-g -std=c99 -pedantic -Wall -Wextra -Werror -Wno-missing-field-initializers -Wno-unknown-pragmas -D_XOPEN_SOURCE=500"
CLANG_ASM_FLAGS="-g -S -O2 -std=c99 -pedantic -Wall -Wextra -Werror -Wno-missing-field-initializers -Wno-unknown-pragmas -D_XOPEN_SOURCE=500"
CLANG_EXE_FLAGS="-O2 -std=c99 -pedantic -Wall -Wextra -Werror -Wno-missing-field-initializers -Wno-unknown-pragmas -D_XOPEN_SOURCE=500"

mkdir $OUTPUT_DIR 2>/dev/null
rm $OUTPUT_DIR/* 2>/dev/null

cd $OUTPUT_DIR

clang $CLANG_FLAGS -E $JKC99_DIR/src/jkc99.c -o jkc99.i
clang $CLANG_ASM_FLAGS jkc99.i -o jkc99.s
clang $CLANG_EXE_FLAGS jkc99.i -ldl -o jkc99_ref

cp jkc99.i jkc99_ref.i
mv jkc99.s jkc99_ref.s

$JKC99_DIR/bin/jkc99 jkc99.i print > jkc99p.i
if [ $? -ne 0 ]; then
    exit 1
fi
rm jkc99.i
mv jkc99p.i jkc99.i
clang $CLANG_ASM_FLAGS jkc99.i -o jkc99.s
if [ $? -ne 0 ]; then
    exit 1
fi
clang $CLANG_EXE_FLAGS jkc99.i -ldl -o jkc99_print
if [ $? -ne 0 ]; then
    exit 1
fi

mv jkc99.i jkc99_print.i
mv jkc99.s jkc99_print.s

cmp -s jkc99_ref jkc99_print
if [ $? = 0 ]
then
    echo "Executables are identical"
else
    echo "Executables are NOT identical"
fi
