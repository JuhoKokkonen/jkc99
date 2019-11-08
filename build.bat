@echo off

set JKC99Dir=%~dp0

set WindowsFlags=-DUNICODE -D_UNICODE -DWIN32_LEAN_AND_MEAN -D_CRT_SECURE_NO_WARNINGS 
set ClangFlagsCommon=-O2 -std=c99 -pedantic -Wall -Wextra -Werror -Wno-missing-field-initializers
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
)

move jkc99.exe ..\bin > NUL 2> NUL
move *.dll ..\bin > NUL 2> NUL

:End

popd
popd
