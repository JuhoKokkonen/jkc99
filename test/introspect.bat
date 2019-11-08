@echo off

set JKC99Dir=%~dp0\..
set TestDir=%JKC99Dir%\test
set OutputDir=%TestDir%\introspect_output

set CLANG_FLAGS=-g -gcodeview -fdiagnostics-absolute-paths -std=c99 -pedantic -Wall -Wextra -Werror -Wno-unused-variable -Wno-missing-field-initializers -Wno-unknown-pragmas -Wno-unused-parameter -D_CRT_SECURE_NO_WARNINGS

mkdir %OutputDir% > NUL 2> NUL
del %OutputDir%\* /Q > NUL 2> NUL

pushd %OutputDir%

clang %CLANG_FLAGS% -E %TestDir%\introspect_test.c -o introspect_test.i
IF errorlevel 1 GOTO End
%JKC99Dir%\bin\jkc99.exe introspect_test.i introspect print > introspect.i
IF errorlevel 1 GOTO End
clang %CLANG_FLAGS% introspect.i -o introspect_test.exe
IF errorlevel 1 GOTO End
introspect_test.exe

:End

popd
