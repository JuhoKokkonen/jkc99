@echo off

set JKC99RunUnitTests=0

set JKC99Dir=%~dp0

set WindowsFlags=-DUNICODE -D_UNICODE -DWIN32_LEAN_AND_MEAN -D_CRT_SECURE_NO_WARNINGS 
set ClangFlagsCommon=-std=c99 -pedantic -Wall -Wextra -Werror -Wno-missing-field-initializers
set ClangFlags=-g -gcodeview -fdiagnostics-absolute-paths %ClangFlagsCommon%
REM set ClangFlags=-O2 %ClangFlagsCommon%

pushd %JKC99Dir%

mkdir build > NUL 2> NUL
mkdir bin > NUL 2> NUL

pushd build

del *.lib > NUL 2> NUL
del *.exp > NUL 2> NUL
del *.ilk > NUL 2> NUL
del *.obj > NUL 2> NUL
del *.pdb > NUL 2> NUL

clang %ClangFlags% %WindowsFlags% %JKC99Dir%\src\jkc99.c -o jkc99.exe
IF errorlevel 1 GOTO End
for %%M in (%JKC99Dir%\src\modules\*.c) do (
    clang %ClangFlags% %WindowsFlags% -shared -ljkc99 %%M -o %%~nM.dll
    IF errorlevel 1 GOTO End
)

move jkc99.exe ..\bin > NUL 2> NUL
move *.dll ..\bin > NUL 2> NUL

IF %JKC99RunUnitTests% EQU 1 (
    clang %ClangFlags% %WindowsFlags% -DJKC99_TEST_BUILD -E %JKC99Dir%\src\jkc99.c -o jkc99.i
    IF errorlevel 1 GOTO End
    %JKC99Dir%\bin\jkc99 jkc99.i test print > jkc99_test.i
    IF errorlevel 1 GOTO End
    clang %ClangFlags% %WindowsFlags% -DJKC99_TEST_BUILD jkc99_test.i -o jkc99_test.exe
    IF errorlevel 1 GOTO End
    jkc99_test.exe
    IF errorlevel 1 GOTO End
)

:End

popd
popd
