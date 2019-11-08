#!/bin/sh
TEST_DIR=`dirname "$(readlink -f "$0")"`
CLANG_FLAGS="-g -std=c99 -pedantic -Wall -Wextra -Werror -Wno-missing-field-initializers -Wno-unknown-pragmas -D_XOPEN_SOURCE=500"
CLANG_ASM_FLAGS="-g -S -O2 -std=c99 -pedantic -Wall -Wextra -Werror -Wno-missing-field-initializers -Wno-unknown-pragmas -D_XOPEN_SOURCE=500"
CLANG_EXE_FLAGS="-O2 -std=c99 -pedantic -Wall -Wextra -Werror -Wno-missing-field-initializers -Wno-unknown-pragmas -D_XOPEN_SOURCE=500"

rm *.i
rm *.s

clang $CLANG_FLAGS -E $TEST_DIR/../src/jkc99.c -o jkc99.i
clang $CLANG_ASM_FLAGS $TEST_DIR/jkc99.i -o jkc99.s
clang $CLANG_EXE_FLAGS $TEST_DIR/jkc99.i -ldl -o jkc99

cp jkc99.i jkc99_ref.i
mv jkc99.s jkc99_ref.s
mv jkc99 jkc99_ref

../bin/jkc99 jkc99.i print > jkc99p.i
rm jkc99.i
mv jkc99p.i jkc99.i
clang $CLANG_ASM_FLAGS $TEST_DIR/jkc99.i -o jkc99.s
clang $CLANG_EXE_FLAGS $TEST_DIR/jkc99.i -ldl -o jkc99

mv jkc99.i jkc99_print.i
mv jkc99.s jkc99_print.s
mv jkc99 jkc99_print

cmp -s jkc99_ref jkc99_print
if [ $? = 0 ]
then
    echo "Executables are identical"
else
    echo "Executables are NOT identical"
fi
