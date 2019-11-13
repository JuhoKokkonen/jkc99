# jkc99
jkc9 is a parser for the C programming language, specifically standard compliant C99.
The program can be used for example to perform static code analysis or to add programming features that standard C is lacking.
This is a personal project and as such is not ready for any sort of production use.
Lexer is currently based on modified version of [stb_c_lexer.h](https://github.com/nothings/stb/blob/master/stb_c_lexer.h) by Sean Barrett.
Otherwise original code by the author.

## Building
Currently there are simple build scripts included, *build.bat* for Windows, *build.sh* for Linux. Clang is assumed to be installed.

## Usage
`jkc99 [options] <target> <<module-name> [module-options]>>...`
Where target is a file containing preprocessed C source code (jkc99 does not currently include a preprocessor).
Target is followed by a list of modules and their corresponding options.
These modules (.dll/.so) are loaded dynamically and hook into the main executable by installing callback to various parts of processing.

For example to add type introspection features to `main.c` the compilation process would be along these lines:
```
clang -E main.c -o main.i
jkc99 main.i introspect print > main_with_type_introspection.i
clang main_with_type_introspection.i -o main
```

## Modules
### Introspect
This module can be used to add run time type introspection capabilities as well as a table of global symbols to any project.
These features can be quite useful when building debug features or internal tools.
### Test
This module can be used to make writing unit tests a bit more convenient. 
It provides macros for defining unit test conditions and functions which can be placed anywhere in the project.
Test module will automatically collect all of the defined unit test functions and they can then be performed with a single `jkc99_test_run()` function call.
### Print
This module simply prints the current state of the AST.
Typically this is used as the last module after other modules have added their modifications to the input.

## Current state
JKC99 is still very much a work in progress.
The biggest limiting factor currently is error handling which in many parts will simply assert and crash out on invalid input.
There are also some features of standard C99 that are not fully supported.
Naturally compiler specific features also need work, currently jkc99 is only being tested using clang.
Some of the todo items:
- Extensive error handling
- Support for universal character names
- Proper support for bitfields
- Expression evaluation needs a lot of work (e.g. for determining sizes of arrays)
- Discriminating functions based on calling convention
- Optimising in general. Currently pretty much everything is done with linear lookups etc. Lots of low-hanging fruit for hash tables and such.
- C preprocessor
As this is primarily a project for personal use, the development tends to follow whatever I happen to run into at any given moment.
