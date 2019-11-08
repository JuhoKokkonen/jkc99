@echo off

set JKC99Dir=%~dp0\..
set OutputDir=%JKC99Dir%\test\test_output

set CLANG_FLAGS=-O2 -std=c99 -pedantic -Wall -Wextra -Werror -Wno-missing-field-initializers -Wno-unknown-pragmas -D_CRT_SECURE_NO_WARNINGS

mkdir %OutputDir% > NUL 2> NUL
del %OutputDir%\* /Q > NUL 2> NUL

pushd %JKC99Dir%
clang %CLANG_FLAGS% -E %JKC99Dir%\src\jkc99.c -o %OutputDir%\jkc99.i
IF errorlevel 1 GOTO End
popd

pushd %OutputDir%

clang %CLANG_FLAGS% -S jkc99.i -o jkc99.s
IF errorlevel 1 GOTO End
clang %CLANG_FLAGS% jkc99.i -o jkc99.exe
IF errorlevel 1 GOTO End

copy jkc99.i jkc99_ref.i
move jkc99.s jkc99_ref.s
move jkc99.exe jkc99_ref.exe

%JKC99Dir%\bin\jkc99.exe jkc99.i print > jkc99p.i
IF errorlevel 1 GOTO End
del jkc99.i
move jkc99p.i jkc99.i
clang %CLANG_FLAGS% -S jkc99.i -o jkc99.s
IF errorlevel 1 GOTO End
clang %CLANG_FLAGS% jkc99.i -o jkc99.exe
IF errorlevel 1 GOTO End

move jkc99.i jkc99_print.i
move jkc99.s jkc99_print.s
move jkc99.exe jkc99_print.exe

fc jkc99_ref.s jkc99_print.s
fc /B jkc99_ref.exe jkc99_print.exe

:End

popd
