/* TODO These are some of the bigger items
 *  - Proper error messages universally. At the moment we basically just assert and/or chash out
 *  - Compile time expression evaluation needs a lot of work
 *  - Support multiple compilation units properly
 *  - Support for bitfields
 *  - Support for Complex types
 *  - Optimise, at the very least use hash table in place of linear lookups in certain places
 *  - Discriminate function types on calling convention
 *  - Support stateful pragmas such as pack or attribute push/pop for types
 */

#ifndef JKC99_H
#define JKC99_H


#include <stdarg.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <limits.h>
#include <float.h>

#include "lexer.c"

#if 0
#include <assert.h>
#define jkc99_assert assert
#else
#define jkc99_assert(cond) ((cond) ? 0 : ((*(volatile int*)(0)) = 123), 0)
#endif

#include "arena.h"

#define jkc99_array_count(arr) (sizeof((arr)) / sizeof((arr)[0]))

#ifdef __clang__
#define UNUSED __attribute__((__unused__))
#define JKC99_COMPILER_ID 0
#define HAS___ATTRIBUTE__ 1
#define HAS___EXTENSION__ 1
#define HAS_GCC_ASM 1
#define HAS___DECLSPEC 1
#else 
#define UNUSED
#define JKC99_COMPILER_ID -1
#define HAS___ATTRIBUTE__ 0
#define HAS___EXTENSION__ 0
#define HAS_GCC_ASM 0
#define HAS___DECLSPEC 0
#error "Unsupported compiler"
#endif

#if defined(_WIN32)
#define EXPORT __declspec(dllexport)
#define JKC99_API __declspec(dllexport)
#elif defined(__linux__)
#define EXPORT extern
#define JKC99_API extern
#else
#error "Unsupported operating system"
#endif

#define STRING_STORAGE_SIZE (64*1024)

#if 1
#if __STDC_VERSION__ >= 199001L && defined(__STDC_IEC_559_COMPLEX__) && __STDC_IEC_559_COMPLEX__ == 1
#define JKC99_HAS_COMPLEX 1

#if 1

#include <complex.h>
typedef float _Complex          Float_Complex;
typedef double _Complex         Double_Complex;
typedef long double _Complex    LongDouble_Complex;


#define float_complex_to_bool(c)        (c)
#define double_complex_to_bool(c)       (c)
#define long_double_complex_to_bool(c)  (c)

#define float_complex_add(l,r)          ((l)+(r))
#define double_complex_add(l,r)         ((l)+(r))
#define long_double_complex_add(l,r)    ((l)+(r))

#define float_complex_sub(l,r)          ((l)-(r))
#define double_complex_sub(l,r)         ((l)-(r))
#define long_double_complex_sub(l,r)    ((l)-(r))

#define float_complex_mul(l,r)          ((l)*(r))
#define double_complex_mul(l,r)         ((l)*(r))
#define long_double_complex_mul(l,r)    ((l)*(r))

#define float_complex_div(l,r)          ((l)/(r))
#define double_complex_div(l,r)         ((l)/(r))
#define long_double_complex_div(l,r)    ((l)/(r))

#define float_complex_eq(l,r)           ((l)==(r))
#define double_complex_eq(l,r)          ((l)==(r))
#define long_double_complex_eq(l,r)     ((l)==(r))

#define float_complex_ne(l,r)           ((l)!=(r))
#define double_complex_ne(l,r)          ((l)!=(r))
#define long_double_complex_ne(l,r)     ((l)!=(r))
#endif

#else

typedef struct Float_Complex {
    float re;
    float im;
} Float_Complex;

typedef struct Double_Complex {
    double re;
    double im;
} Double_Complex;

typedef struct LongDouble_Complex {
    long double re;
    long double im;
} LongDouble_Complex;

#define creal(c)                        ((c).re)
#define cimag(c)                        ((c).im)

#define float_complex_to_bool(c)        (creal(c) || cimag(c))
#define double_complex_to_bool(c)       (creal(c) || cimag(c))
#define long_double_complex_to_bool(c)  (creal(c) || cimag(c))

#if 0
#define JK_COMPLEX_ADD(ftype) \
    static inline ftype##_complex ftype##_complex_add(ftype##_complex l, ftype##_complex r) { \
        ftype##_complex res; \
        res.re = creal(l) + creal(r); \
        res.im = cimag(l) + cimag(r); \
        return res; \
    }
#define JK_COMPLEX_SUB(ftype) \
    static inline ftype##_complex ftype##_complex_sub(ftype##_complex l, ftype##_complex r) { \
        ftype##_complex res; \
        res.re = creal(l) - creal(r); \
        res.im = cimag(l) - cimag(r); \
        return res; \
    }
#define JK_COMPLEX_MUL(ftype) \
    static inline ftype##_complex ftype##_complex_mul(ftype##_complex l, ftype##_complex r) { \
        ftype##_complex res; \
        res.re = (creal(l)*creal(r)) - (cimag(l)*cimag(r)); \
        res.im = (creal(l)*cimag(r)) + (cimag(l)*creal(r)); \
        return res; \
    }
#define JK_COMPLEX_DIV(ftype) \
    static inline ftype##_complex ftype##_complex_div(ftype##_complex l, ftype##_complex r) { \
        ftype##_complex res; \
        res.re = (creal(l)*creal(r)) + (cimag(l)*cimag(r)); \
        res.im = (creal(l)*cimag(r)) - (cimag(l)*creal(r)); \
        r.re = 1.0f / ((creal(l)*creal(l)) + (cimag(l)*cimag(l))); \
        res.re *= r.re; \
        res.im *= r.im; \
        return res; \
    }
#define JK_COMPLEX_EQ(ftype) \
    static inline _Bool ftype##_complex_eq(ftype##_complex l, ftype##_complex r) { \
        return ((creal(l) == creal(r)) && (cimag(l) == creal(r))); \
    }
#define JK_COMPLEX_NE(ftype) \
    static inline _Bool ftype##_complex_ne(ftype##_complex l, ftype##_complex r) { \
        return !ftype##_complex_eq(l,r); \
    }

JK_COMPLEX_ADD(float);
JK_COMPLEX_ADD(double);
JK_COMPLEX_ADD(long_double);

JK_COMPLEX_SUB(float);
JK_COMPLEX_SUB(double);
JK_COMPLEX_SUB(long_double);

JK_COMPLEX_MUL(float);
JK_COMPLEX_MUL(double);
JK_COMPLEX_MUL(long_double);

JK_COMPLEX_DIV(float);
JK_COMPLEX_DIV(double);
JK_COMPLEX_DIV(long_double);

JK_COMPLEX_EQ(float);
JK_COMPLEX_EQ(double);
JK_COMPLEX_EQ(long_double);

JK_COMPLEX_NE(float);
JK_COMPLEX_NE(double);
JK_COMPLEX_NE(long_double);

#endif

#endif
#endif


typedef struct StringIntern {
    const char      *str;
    size_t          len;
} StringIntern;

typedef struct Strings {
    Arena           arena;
    StringIntern    *strings;
} Strings;

typedef enum JKC99Keywords {
#define X(keyword) kw_##keyword,
#include "keywords.h"
    kw_count
} JKC99Keywords;
const char *kw[kw_count];

typedef struct Source {
    const char      *file;
    unsigned int    line;
    unsigned int    column;
    const char      *from;
    int             lineMarkerFlags;
} Source;

typedef struct PreprocessorDirectiveLineMarker {
    int                         flags;
    unsigned int                line;
    const char                  *file;
} PreprocessorDirectiveLineMarker;

typedef struct PreprocessorDirectiveLine {
    unsigned int                line;
    const char                  *file;
} PreprocessorDirectiveLine;

typedef enum PreprocessorDirectiveKind {
    kPreprocessorDirectiveNone,
    kPreprocessorDirectiveLine,
    kPreprocessorDirectiveGCCLineMarker,
    kPreprocessorDirectivePragma
} PreprocessorDirectiveKind;

typedef struct PreprocessorDirective PreprocessorDirective;

#define AST_COMMON \
    size_t                  directiveCount; \
    PreprocessorDirective   *directives;    \
    Source                  src;

typedef struct ASTCommon {
    AST_COMMON
} ASTCommon;

struct PreprocessorDirective {
    AST_COMMON
    PreprocessorDirectiveKind   kind;
    union {
        PreprocessorDirectiveLineMarker lineMarker;
        PreprocessorDirectiveLine       line;
        const char                      *pragma;
    } u;
};

typedef struct Attribute {
    AST_COMMON
    const char          *strBegin;
    const char          *strEnd;
} Attribute;

typedef struct Stmt Stmt;
typedef struct Expr Expr;
typedef struct Declaration Declaration;
typedef struct FunctionDefinition FunctionDefinition;

typedef enum StmtKind {
    kStmtNone,
    kStmtLabeled,
    kStmtCompound,
    kStmtExpression,
    kStmtSelection,
    kStmtIteration,
    kStmtJump,
#if HAS_GCC_ASM
    kStmtAsm
#endif
} StmtKind;

typedef enum StmtLabeledKind {
    kStmtLabeledNone,
    kStmtLabeledLabel,
    kStmtLabeledCase,
    kStmtLabeledDefault
} StmtLabeledKind;

typedef struct StmtLabeled {
    StmtLabeledKind     kind;
    Stmt                *stmt;
    union {
        struct {
            const char      *label;
            size_t          attributeCount;
            Attribute       *attributes;
        } label;
        Expr            *caseExpr;
    } u;
} StmtLabeled;

typedef struct BlockItem {
    Declaration         *decl;
    Stmt                *stmt;
} BlockItem;

typedef struct StmtCompound {
    size_t              count;
    BlockItem           *items;
} StmtCompound;

typedef struct StmtExpr {
    Expr                *expr;
    size_t              attributeCount;
    Attribute           *attributes;
} StmtExpr;

typedef enum StmtSelectionKind {
    kStmtSelectionNone,
    kStmtSelectionIf,
    kStmtSelectionSwitch
} StmtSelectionKind;

typedef struct StmtSelection {
    StmtSelectionKind   kind;
    Expr                *expr;
    Stmt                *stmt;
    Stmt                *elseStmt;
} StmtSelection;

typedef enum StmtIterationKind {
    kStmtIterationNone,
    kStmtIterationWhile,
    kStmtIterationDoWhile,
    kStmtIterationFor
} StmtIterationKind;

typedef struct StmtIteration {
    StmtIterationKind   kind;
    Expr                *condExpr;
    Stmt                *stmt;
    Expr                *initExpr;
    Declaration         *initDecl;
    Expr                *iterExpr;
} StmtIteration;

typedef enum StmtJumpKind {
    kStmtJumpNone,
    kStmtJumpGoto,
    kStmtJumpContinue,
    kStmtJumpBreak,
    kStmtJumpReturn
} StmtJumpKind;

typedef struct StmtJump {
    StmtJumpKind        kind;
    union {
        const char      *identifier;
        Expr            *returnExpr;
    } u;
} StmtJump;

#if HAS_GCC_ASM
typedef enum StmtAsmKind {
    kStmtAsmBasic,
    kStmtAsmExtended
} StmtAsmKind;

typedef enum StmtAsmQualifier {
    kStmtAsmQualifierVolatile   = 1 << 0,
    kStmtAsmQualifierInline     = 1 << 1,
    kStmtAsmQualifierGoto       = 1 << 2
} StmtAsmQualifier;

typedef struct StmtAsmOutputOperand {
    const char          *symbolicName;
    const char          *constraints;
    Expr                *expr;
} StmtAsmOutputOperand, StmtAsmInputOperand;

typedef const char *StmtAsmClobber;

typedef struct StmtAsm {
    StmtAsmKind             kind;
    int                     qualifiers;
    const char              *instructions;
    size_t                  outputOperandCount;
    StmtAsmOutputOperand    *outputOperands;
    size_t                  inputOperandCount;
    StmtAsmInputOperand     *inputOperands;
    size_t                  clobberCount;
    StmtAsmClobber          *clobbers;
    size_t                  gotoLabelCount;
    const char              **gotoLabels;
} StmtAsm;
#endif

struct Stmt {
    AST_COMMON
    StmtKind            kind;
    union {
        StmtLabeled     l;
        StmtCompound    c;
        StmtExpr        e;
        StmtSelection   s;
        StmtIteration   i;
        StmtJump        j;
#if HAS_GCC_ASM
        StmtAsm         a;
#endif
    } u;
};

typedef enum DirectAbstractDeclaratorKind {
    kDirectAbstractDeclaratorNone,
    kDirectAbstractDeclaratorDeclarator,
    kDirectAbstractDeclaratorArray,
    kDirectAbstractDeclaratorFunc
} DirectAbstractDeclaratorKind;

typedef enum CallingConvention {
    kCallingConventionNone,
    kCallingConventionCdecl,
    kCallingConventionClrcall,
    kCallingConventionStdcall,
    kCallingConventionFastcall,
    kCallingConventionThiscall,
    kCallingConventionVectorcall
} CallingConvention;

typedef enum TypeQualifierKind {
    kTypeQualifierNone          = 0,
    kTypeQualifierConst         = 1 << 0,
    kTypeQualifierRestrict      = 1 << 1,
    kTypeQualifierVolatile      = 1 << 2,
    kTypeQualifier__ptr32       = 1 << 3,
    kTypeQualifier__ptr64       = 1 << 4,
    kTypeQualifier__unaligned   = 1 << 5,
    kTypeQualifier__sptr        = 1 << 6, /* TODO */
    kTypeQualifier__uptr        = 1 << 7  /* TODO */
} TypeQualifierKind;

enum {
    bTypeQualifierFormatQ       = 1 << 0,
    bTypeQualifierFormat__Q     = 1 << 1,
    bTypeQualifierFormat__Q__   = 1 << 2
};

typedef struct TypeQualifier {
    AST_COMMON
    short                   kind;
    short                   format;
} TypeQualifier;

typedef struct Pointer {
    size_t                      qualifierCount;
    TypeQualifier               *qualifiers;
} Pointer;

typedef struct DirectAbstractDeclarator {
    AST_COMMON
    DirectAbstractDeclaratorKind    kind;
    union {
        struct AbstractDeclarator           *declarator;
        struct {
            struct DirectAbstractDeclarator *direct;
            size_t                          attributeCount;
            Attribute                       *attributes;
            size_t                          qualifierCount;
            TypeQualifier                   *qualifiers;
            bool                            isStatic;
            bool                            isVLA;
            Expr                            *expr;
        } array;
        struct {
            struct DirectAbstractDeclarator *direct;
            CallingConvention               callingConvention;
            bool                            hasEllipsis;
            bool                            hasParameterList;
            size_t                          count;
            union {
                struct ParameterDeclaration **parameters;
                const char                  **identifiers;
            } u;
        } func;
    } u;
} DirectAbstractDeclarator;

typedef struct AbstractDeclarator {
    AST_COMMON
    size_t                      pointerCount;
    Pointer                     *pointers;
    DirectAbstractDeclarator    *direct;
    size_t                      beforeAttributeCount;
    Attribute                   *beforeAttributes;
    size_t                      afterAttributeCount;
    Attribute                   *afterAttributes;
} AbstractDeclarator;

typedef enum StorageClassKind {
    kStorageClassNone           = 0,
    kStorageClassTypedef,
    kStorageClassExtern,
    kStorageClassStatic, 
    kStorageClassAuto,
    kStorageClassRegister
} StorageClassKind;

typedef struct StorageClass {
    AST_COMMON
    StorageClassKind        kind;
} StorageClass;

typedef enum TypeSpecifierKind {
    kTypeSpecifierNone          = 0,
    kTypeSpecifierBuiltin       = 1 << 0,
    kTypeSpecifierVoid          = 1 << 1,
    kTypeSpecifierChar          = 1 << 2,
    kTypeSpecifierShort         = 1 << 3,
    kTypeSpecifierInt           = 1 << 4,
    kTypeSpecifierLong          = 1 << 5,
    kTypeSpecifierFloat         = 1 << 6,
    kTypeSpecifierDouble        = 1 << 7,
    kTypeSpecifierSigned        = 1 << 8,
    kTypeSpecifierUnsigned      = 1 << 9,
    kTypeSpecifier_Bool         = 1 << 10,
    kTypeSpecifier_Complex      = 1 << 11,
    kTypeSpecifierStruct        = 1 << 12,
    kTypeSpecifierUnion         = 1 << 13,
    kTypeSpecifierEnum          = 1 << 14,
    kTypeSpecifierTypedefName   = 1 << 15,
    kTypeSpecifier__int8        = 1 << 16,//TODO Get rid of this and use builtin instead
    kTypeSpecifier__int16       = 1 << 17,//TODO Get rid of this and use builtin instead
    kTypeSpecifier__int32       = 1 << 18,//TODO Get rid of this and use builtin instead
    kTypeSpecifier__int64       = 1 << 19 //TODO Get rid of this and use builtin instead
} TypeSpecifierKind;

typedef struct StructDeclaration StructDeclaration;

typedef enum DeclspecModifierKind {
    kDeclspecModifierNone,

    kDeclspecModifier_align,
    kDeclspecModifier_allocate,
    kDeclspecModifier_allocator,
    kDeclspecModifier_appdomain,
    kDeclspecModifier_code_seg,
    kDeclspecModifier_deprecated,
    kDeclspecModifier_dllimport,
    kDeclspecModifier_dllexport,
    kDeclspecModifier_jitintrisic,
    kDeclspecModifier_naked,
    kDeclspecModifier_noalias,
    kDeclspecModifier_noinline,
    kDeclspecModifier_noreturn,
    kDeclspecModifier_nothrow,
    kDeclspecModifier_novtable,
    kDeclspecModifier_process,
    kDeclspecModifier_property,
    kDeclspecModifier_restrict,
    kDeclspecModifier_safebuffers,
    kDeclspecModifier_selectany,
    kDeclspecModifier_spectre,
    kDeclspecModifier_thread,
    kDeclspecModifier_uuid
} DeclspecModifierKind;

typedef struct DeclspecModifier {
    DeclspecModifierKind    kind;
    union {
        unsigned int        align;
        const char          *segname;
        const char          *deprecated;
        struct {
            const char      *get;
            const char      *put;
        } property;
        const char          *uuid;
    } u;
} DeclspecModifier;

typedef struct Declspec {
    AST_COMMON
    size_t              modifierCount;
    DeclspecModifier    *modifiers;
} Declspec;

typedef struct StructOrUnionSpecifier {
    const char                  *identifier;
    bool                        definedHere;
    size_t                      memberDeclarationCount;
    struct StructDeclaration    **memberDeclarations;
    size_t                      attributeCount;
    Attribute                   *attributes;
    size_t                      declspecCount;
    Declspec                    *declspecs;
} StructOrUnionSpecifier;

#if 0
typedef size_t TypeHandle;
#define JKC99_INVALID_TYPE_HANDLE 0
#define JKC99_TYPE_HANDLE_INVALID(handle) (handle) == 0
#define JKC99_TYPE_HANDLE_NOT_INVALID(handle) (handle) != 0
#define JKC99_TYPE_HANDLE_EQ(h1,h2) (h1) == (h2)
#define JKC99_TYPE_HANDLE_NEQ(h1,h2) (h1) != (h2)
#define JKC99_TYPE_HANDLE_TO_INDEX(handle) (handle)
#define JKC99_INDEX_TO_TYPE_HANDLE(i) (i)
#else
typedef struct TypeHandle {
    size_t          index;
} TypeHandle;
#define JKC99_INVALID_TYPE_HANDLE (TypeHandle){.index=0}
#define JKC99_TYPE_HANDLE_INVALID(handle) (handle).index == 0
#define JKC99_TYPE_HANDLE_NOT_INVALID(handle) (handle).index != 0
#define JKC99_TYPE_HANDLE_EQ(h1,h2) (h1).index == (h2).index
#define JKC99_TYPE_HANDLE_NEQ(h1,h2) (h1).index != (h2).index
#define JKC99_TYPE_HANDLE_TO_INDEX(handle) (handle).index
#define JKC99_INDEX_TO_TYPE_HANDLE(i) (TypeHandle){.index=(i)}
#endif

typedef enum ExprResultKind {
    kExprResultNotResolved,
    kExprResultError,
    kExprResultUnableToResolveAtCompileTime,
    kExprResultSuccess
} ExprResultKind;

#define EXPR_RESULT_SUCCESS(res)    (res)->kind == kExprResultSuccess
#define EXPR_RESULT_ERROR(res)      (res)->kind == kExprResultError
#define EXPR_RESULT_UNABLE(res)     (res)->kind == kExprResultUnableToResolveAtCompileTime

#define EXPR_RESULT_SET_SUCCESS(res)    (res)->kind = kExprResultSuccess
#define EXPR_RESULT_SET_ERROR(res)      (res)->kind = kExprResultError
#define EXPR_RESULT_SET_UNABLE(res)     (res)->kind = kExprResultUnableToResolveAtCompileTime

typedef struct ExprResult {
    ExprResultKind      kind;
    TypeHandle          type;
    union {
        _Bool                   _Bool_val;
        char                    char_val;
        signed char             signed_char_val;
        signed short            signed_short_val;
        signed int              signed_int_val;
        signed long             signed_long_val;
        signed long long        signed_long_long_val;
        unsigned char           unsigned_char_val;
        unsigned short          unsigned_short_val;
        unsigned int            unsigned_int_val;
        unsigned long           unsigned_long_val;
        unsigned long long      unsigned_long_long_val;
        float                   float_val;
        double                  double_val;
        long double             long_double_val;
        Float_Complex            float_complex_val;
        Double_Complex          double_complex_val;
        LongDouble_Complex      long_double_complex_val;
        intptr_t                intptr_val;
        uintptr_t               uintptr_val;
        size_t                  size_t_val;
        void                    *ptr;
        Expr                    *expr;
    } u;
} ExprResult;

typedef struct Enumerator {
    AST_COMMON
    const char                  *identifier;
    Expr                        *expr;
    ExprResult                  exprResult;
    int                         exprOffset;
} Enumerator;

typedef struct EnumSpecifier {
    const char                  *identifier;
    bool                        definedHere;
    size_t                      enumeratorCount;
    Enumerator                  *enumerators;
    size_t                      attributeCount;
    Attribute                   *attributes;
    size_t                      declspecCount;
    Declspec                    *declspecs;
} EnumSpecifier;

typedef struct TypeSpecifier {
    AST_COMMON
    TypeSpecifierKind           kind;
    union {
        StructOrUnionSpecifier  su;
        EnumSpecifier           e;
        const char              *t;
    } u;
} TypeSpecifier;


typedef enum FunctionSpecifierKind {
    kFunctionSpecifierNone      = 0,
    kFunctionSpecifierInline    = 1 << 0
} FunctionSpecifierKind;

typedef enum FunctionSpecifierFormat {
    kFunctionSpecifierFormatF           = 1 << 0,
    kFunctionSpecifierFormat__F         = 1 << 1,
    kFunctionSpecifierFormat__F__       = 1 << 2,
    kFunctionSpecifierFormat__forceF    = 1 << 3
} FunctionSpecifierFormat;

typedef struct FunctionSpecifier {
    AST_COMMON
    short               kind;
    short               format;
} FunctionSpecifier;

typedef enum DeclarationSpecifierKind {
    kDeclarationSpecifierNone = 0,

    kDeclarationSpecifierStorageClassSpecifier,
    kDeclarationSpecifierTypeSpecifier,
    kDeclarationSpecifierTypeQualifier,
    kDeclarationSpecifierFunctionSpecifier,

    kDeclarationSpecifierDeclspec,
    kDeclarationSpecifierAttribute,
    kDeclarationSpecifierCallingConvention,
    kDeclarationSpecifierExtension

} DeclarationSpecifierKind;

typedef struct DeclarationSpecifier {
    DeclarationSpecifierKind    kind;
    union {
        StorageClass            storageClass;
        TypeSpecifier           typeSpecifier;
        TypeQualifier           typeQualifier;
        FunctionSpecifier       functionSpecifier;
#if HAS___DECLSPEC
        Declspec                declspec;
#endif
#if HAS___ATTRIBUTE__
        Attribute               attribute;
#endif
        CallingConvention       callingConvention;
    } u;
} DeclarationSpecifier;

typedef struct ParameterDeclaration ParameterDeclaration;

typedef enum DirectDeclaratorKind {
    kDirectDeclaratorNone,
    kDirectDeclaratorIdentifier,
    kDirectDeclaratorDeclarator,
    kDirectDeclaratorArray,
    kDirectDeclaratorFunc
} DirectDeclaratorKind;

typedef struct DirectDeclarator {
    AST_COMMON
    DirectDeclaratorKind    kind;
    size_t                  typeIndex;
    union {
        struct {
            const char              *str;
            size_t                  symIndex;
        } identifier;
        struct Declarator           *declarator;
        struct {
            struct DirectDeclarator *direct;
            size_t                  attributeCount;
            Attribute               *attributes;
            size_t                  qualifierCount;
            TypeQualifier           *qualifiers;
            bool                    isStatic;
            bool                    isVLA;
            Expr                    *expr;
        } array;
        struct {
            struct DirectDeclarator *direct;
            CallingConvention       callingConvention;
            bool                    hasEllipsis;
            bool                    hasParameterList;
            size_t                  count;
            union {
                ParameterDeclaration    **parameters;
                const char              **identifiers;
            } u;
            Stmt                    *body;
        } func;
    } u;
} DirectDeclarator;

typedef struct Declarator {
    AST_COMMON
    size_t              pointerCount;
    Pointer             *pointers;
    DirectDeclarator    *direct;
    size_t              beforeAttributeCount;
    Attribute           *beforeAttributes;
    size_t              afterAttributeCount;
    Attribute           *afterAttributes;
} Declarator;

typedef struct StructDeclarator {
    Declarator          *declarator;
    Expr                *bitCountExpr;
} StructDeclarator;

typedef enum InitializerKind {
    kInitializerNone,
    kInitializerExpr,
    kInitializerList
} InitializerKind;

typedef struct Designation {
    size_t              count;
    struct Designator   *list;
} Designation;

typedef struct InitializerList {
    AST_COMMON
    size_t              count;
    struct Designation  *designations;
    struct Initializer  *initializers;
} InitializerList;

typedef struct Initializer {
    InitializerKind         kind;
    union {
        Expr                *expr;
        struct InitializerList  list;
    } u;
} Initializer;

typedef enum DesignatorKind {
    kDesignatorNone,
    kDesignatorIndex,
    kDesignatorField
} DesignatorKind;

typedef struct Designator {
    AST_COMMON
    DesignatorKind      kind;
    union {
        Expr            *indexExpr;
        const char      *field;
    } u;
} Designator;

typedef struct InitDeclarator {
    Declarator                  *declarator;
    Initializer                 initializer;
} InitDeclarator;

#define DECLARATION_COMMON \
    AST_COMMON \
    TypeHandle                  type; \
    size_t                      specifierCount; \
    DeclarationSpecifier        *specifiers; \
    bool                        hasExtension; \
    Stmt                        *asmStmt;

typedef struct DeclarationCommon {
    DECLARATION_COMMON
} DeclarationCommon;

struct ParameterDeclaration {
    DECLARATION_COMMON
    StorageClassKind            storageClass;
    FunctionSpecifier           functionSpecifier;
    Declarator                  *declarator;
    AbstractDeclarator          *abstractDeclarator;
};

struct Declaration {
    DECLARATION_COMMON
    StorageClassKind            storageClass;
    FunctionSpecifier           functionSpecifier;
    size_t                      initDeclaratorCount;
    InitDeclarator              *initDeclarators;
};

struct StructDeclaration {
    DECLARATION_COMMON
    size_t                      declaratorCount;
    StructDeclarator            *declarators;
};

struct FunctionDefinition {
    DECLARATION_COMMON
    Declarator                  *declarator;
    size_t                      declarationCount;
    Declaration                 **declarations;
    Stmt                        *body;
    const char                  *name;
};

typedef struct ExternalDeclaration {
    FunctionDefinition          *func;
    Declaration                 *decl;
} ExternalDeclaration;

typedef struct TypeName {
    AST_COMMON
    size_t                      attributeCount;
    Attribute                   *attributes;
    TypeHandle                  type;
    size_t                      specifierCount;
    DeclarationSpecifier        *specifiers;
    AbstractDeclarator          *abstractDeclarator;
} TypeName;

//NOTE Please be careful when editing this enum. Pay attention to first/last tokens, used in eval_is_* functions etc.
typedef enum TypeKind {
    kTypeUnknown,
    kTypeBuiltin,

    kTypeVoid,

    kTypeFirstIntegerType,
    kTypeChar = kTypeFirstIntegerType,

    kTypeFirstSignedIntegerType,
    kTypeFirstStandardSignedIntegerType = kTypeFirstSignedIntegerType,
    kTypeSignedChar = kTypeFirstStandardSignedIntegerType,
    kTypeSignedShort,
    kTypeSignedInt,
    kTypeSignedLong,
    kTypeSignedLongLong,
    kTypeLastStandardSignedIntegerType = kTypeSignedLongLong,
    kTypeSignedExtended,
    kTypeLastSignedInteger = kTypeSignedExtended,

    kTypeFirstUnsignedIntegerType,
    kTypeFirstStandardUnsignedIntegerType,
    kType_Bool = kTypeFirstStandardUnsignedIntegerType,
    kTypeUnsignedChar,
    kTypeUnsignedShort,
    kTypeUnsignedInt,
    kTypeUnsignedLong,
    kTypeUnsignedLongLong,
    kTypeLastStandardUnsignedIntegerType = kTypeUnsignedLongLong,
    kTypeUnsignedExtended,
    kTypeLastUnsignedIntegerType = kTypeUnsignedExtended,

    kTypeEnum,
    kTypeLastIntegerType = kTypeEnum,

    kTypeFirstFloatingType,
    kTypeFirstRealFloatingType = kTypeFirstFloatingType,
    kTypeFloat = kTypeFirstRealFloatingType,
    kTypeDouble,
    kTypeLongDouble,
    kTypeLastRealFloatingType = kTypeLongDouble,
    kTypeFirstComplexType,
    kTypeFloat_Complex = kTypeFirstComplexType,
    kTypeDouble_Complex,
    kTypeLongDouble_Complex,
    kTypeLastComplexType = kTypeLongDouble_Complex,
    kTypeLastFloatingType = kTypeLastComplexType,

    kTypeQualified,

    kTypeArray,
    kTypePointer,
    kTypeBitField,
    kTypeStruct,
    kTypeUnion,
    kTypeFunc,

    kTypeVector
} TypeKind;


typedef struct StructUnionMember {
    const char                  *identifier;
    TypeHandle                  type;
} StructUnionMember;

typedef struct JKC99Type {
    TypeKind                    kind;
    size_t                      userCount;
    size_t                      *users;
    union {
        struct {
            const char              *name;
        } builtin;
        struct {
            int                     qualifiers;
            TypeHandle              base;
        } q;
        struct {
            const char              *name;
            size_t                  size;
            signed long long        min;
            unsigned long long      max;
            TypeHandle              correspondingType;
            int                     rank;
        } i;
        struct {
            const char              *name;
            size_t                  size;
            long double             min;
            long double             max;
            TypeHandle              correspondingType;
        } flt;
        struct {
            const char              *name;
            size_t                  size;
            TypeHandle              correspondingType;
        } c;
        struct {
            Expr                    *bitCountExpr;
            int                     bitCount;
            TypeHandle              base;
        } b;
        struct {
            TypeHandle              base;
            bool                    isVLA;
            bool                    isStatic;
            Expr                    *countExpr;
            size_t                  count;
            struct Initializer      *init;
        } a;
        struct {
            TypeHandle              base;
        } p;
        struct {
            const char              *tag;
            size_t                  scope;
            size_t                  memberCount;
            StructUnionMember       *members;
            struct StructOrUnionSpecifier *specifier;
        } su;
        struct {
            const char              *tag;
            size_t                  scope;
            size_t                  enumeratorCount;
            Enumerator              *enumerators;
            struct EnumSpecifier    *specifier;
        } e;
        struct {
            TypeHandle              returnType;
            size_t                  paramCount;
            TypeHandle              *paramTypes;
            bool                    hasEllipsis;
        } f;
        struct {
            TypeHandle              base;
            int                     bytes;
        } v;
    } u;
} JKC99Type;


typedef enum ExprKind {
    kExprPrimary,
    kExprPostfix,
    kExprUnary,
    kExprCast,
    kExprMultiplicative,
    kExprAdditive,
    kExprShift,
    kExprRelational,
    kExprEquality,
    kExprAnd,
    kExprExclusiveOr,
    kExprInclusiveOr,
    kExprLogicalAnd,
    kExprLogicalOr,
    kExprConditional,
    kExprAssignment,
    kExprComma

} ExprKind;

typedef enum ExprPrimaryKind {
    kExprPrimaryIdentifier,
    kExprPrimaryConstant,
    kExprPrimaryStringLiteral,
    kExprPrimaryExpr
} ExprPrimaryKind;

typedef enum ConstantKind {
    kConstantInteger,
    kConstantFloat,
    kConstantEnumeration,
    kConstantCharacter
} ConstantKind;

typedef enum ConstantRepresentation {
    kConstantDecimal,
    kConstantHex,
    kConstantOctal,
    kConstantDecimalWithExponent
} ConstantRepresentation;

typedef struct Constant {
    ConstantKind            kind;
    const char              *str;
    ConstantRepresentation  representation;
    char                    suffix[3];
    union {
        unsigned long long  intVal;
        double              floatVal;
        char                charVal;
    } u;
} Constant;

typedef struct ExprPrimary {
    ExprPrimaryKind kind;
    union {
        const char      *identifier;
        Constant        constant;
        const char      *str;
        Expr            *expr;
    } u;
} ExprPrimary;

typedef enum ExprPostfixKind {
    kExprPostfixIndex,
    kExprPostfixCall,
    kExprPostfixDot,
    kExprPostfixArrow,
    kExprPostfixIncrement,
    kExprPostfixDecrement,
    kExprPostfixCompound,
#ifdef __clang__
    kExprPostfixVaArg
#endif
} ExprPostfixKind;


typedef struct ExprPostfix {
    ExprPostfixKind kind;
    struct Expr     *expr;
    union {
        Expr        *indexExpr;
        Expr        **callArgs;
        const char  *dot;
        const char  *arrow;
        struct {
            TypeName        *typeName;
            InitializerList initializerList;
        } compound;
        struct {
            Expr            *list;
            TypeName        *typeName;
        } va_arg;
    } u;
} ExprPostfix;

typedef enum ExprUnaryKind {
    kExprUnaryIncrement,
    kExprUnaryDecrement,
    kExprUnaryAddressOf,
    kExprUnaryDeref,
    kExprUnaryPlus,
    kExprUnaryMinus,
    kExprUnaryBitwiseNeg,
    kExprUnaryLogicalNot,
    kExprUnarySizeofExpr,
    kExprUnarySizeofType,
    kExprUnaryOffsetof
} ExprUnaryKind;

typedef struct ExprUnary {
    ExprUnaryKind   kind;
    union {
        Expr        *expr;
        TypeName    *typeName;
        struct {
            TypeName    *typeName;
            const char  *field;
        } offsetof;
    } u;
} ExprUnary;

typedef struct ExprCast {
    TypeName        *typeName;
    Expr            *expr;
} ExprCast;

typedef enum ExprMultiplicativeKind {
    kExprMultiplicativeMul,
    kExprMultiplicativeDiv,
    kExprMultiplicativeMod
} ExprMultiplicativeKind;

typedef struct ExprMultiplicative {
    ExprMultiplicativeKind  kind;
    Expr                    *left;
    Expr                    *right;
} ExprMultiplicative;

typedef enum ExprAdditiveKind {
    kExprAdditiveAdd,
    kExprAdditiveSub
} ExprAdditiveKind;

typedef struct ExprAdditive {
    ExprAdditiveKind        kind;
    Expr                    *left;
    Expr                    *right;
} ExprAdditive;

typedef enum ExprShiftKind {
    kExprShiftLeft,
    kExprShiftRight
} ExprShiftKind;

typedef struct ExprShift {
    ExprShiftKind           kind;
    Expr                    *left;
    Expr                    *right;
} ExprShift;

typedef enum ExprRelationalKind {
    kExprRelationalLT,
    kExprRelationalGT,
    kExprRelationalLTE,
    kExprRelationalGTE
} ExprRelationalKind;

typedef struct ExprRelational {
    ExprRelationalKind      kind;
    Expr                    *left;
    Expr                    *right;
} ExprRelational;

typedef enum ExprEqualityKind {
    kExprEqualityEQ,
    kExprEqualityNE
} ExprEqualityKind;

typedef struct ExprEquality {
    ExprEqualityKind        kind;
    Expr                    *left;
    Expr                    *right;
} ExprEquality;

typedef struct ExprAnd {
    Expr                    *left;
    Expr                    *right;
} ExprAnd;

typedef struct ExprExclusiveOr {
    Expr                    *left;
    Expr                    *right;
} ExprExclusiveOr;

typedef struct ExprInclusiveOr {
    Expr                    *left;
    Expr                    *right;
} ExprInclusiveOr;

typedef struct ExprLogicalAnd {
    Expr                    *left;
    Expr                    *right;
} ExprLogicalAnd;

typedef struct ExprLogicalOr {
    Expr                    *left;
    Expr                    *right;
} ExprLogicalOr;

typedef struct ExprConditional {
    Expr                    *cond;
    Expr                    *left;
    Expr                    *right;
} ExprConditional;

typedef enum ExprAssignmentKind {
    kExprAssignmentBasic,
    kExprAssignmentMul,
    kExprAssignmentDiv,
    kExprAssignmentMod,
    kExprAssignmentAdd,
    kExprAssignmentSub,
    kExprAssignmentShiftLeft,
    kExprAssignmentShiftRight,
    kExprAssignmentAnd,
    kExprAssignmentXor,
    kExprAssignmentOr
} ExprAssignmentKind;

typedef struct ExprAssignment {
    ExprAssignmentKind      kind;
    Expr                    *left;
    Expr                    *right;
} ExprAssignment;

typedef struct ExprComma {
    Expr                    *left;
    Expr                    *right;
} ExprComma;


struct Expr {
    AST_COMMON
    ExprKind        kind;
    union {
        ExprPrimary         primary;
        ExprPostfix         postfix;
        ExprUnary           unary;
        ExprCast            cast;
        ExprMultiplicative  multiplicative;
        ExprAdditive        additive;
        ExprShift           shift;
        ExprRelational      relational;
        ExprEquality        equality;
        ExprAnd             and;
        ExprExclusiveOr     exclusiveOr;
        ExprInclusiveOr     inclusiveOr;
        ExprLogicalAnd      logicalAnd;
        ExprLogicalOr       logicalOr;
        ExprConditional     conditional;
        ExprAssignment      assignment;
        ExprComma           comma;
    } u;
};

typedef enum SymbolKind {
    kSymbolNone,
    kSymbolType,
    kSymbolFunction,
    kSymbolVariable,
    kSymbolEnumerator
} SymbolKind;

typedef struct SymbolDeclaration {
    Declaration                 *declaration;
    size_t                      declaratorIndex;
} SymbolDeclaration;

typedef struct Symbol {
    SymbolKind                  kind;
    const char                  *identifier;
    TypeHandle                  type;
    SymbolDeclaration           *declarations;
    union {
        SymbolDeclaration           definition;
        FunctionDefinition          *functionDefinition;
        size_t                      enumeratorIndex;
    } u;
} Symbol;

/* NOTE Never remove these. kHookCount is used for versioning between modules and host applications! */
typedef enum HookKind {
    kHookNewExternalDeclaration,
    kHookNewFunctionDefinition,
    kHookNewTypeStruct,
    kHookNewTypeUnion,
    kHookNewSymbol,
    kHookNewEnumSpecifier,
    kHookParseExprUnaryNone,
    kHookParseComplete,

    kHookCount
} HookKind;

typedef stb_lexer           *ParseBuffer;
typedef struct ParseContext ParseContext;

#define JKC99_NEW_BUFFER(name) ParseBuffer name(ParseContext *ctx, const char *filename, size_t bufSize, const char *buffer)
typedef JKC99_NEW_BUFFER(jkc99_new_buffer_func);

#define JKC99_SET_BUFFER(name) void name(ParseContext *ctx, ParseBuffer lexer)
typedef JKC99_SET_BUFFER(jkc99_set_buffer_func);

#define JKC99_GET_BUFFER(name) ParseBuffer name(ParseContext *ctx)
typedef JKC99_GET_BUFFER(jkc99_get_buffer_func);

#define JKC99_PARSE(name) void name(ParseContext *ctx)
typedef JKC99_PARSE(jkc99_parse_func);

#define JKC99_HOOK(name) bool name(ParseContext *ctx, UNUSED void *modulePtr, UNUSED void *ptr)
typedef JKC99_HOOK(jkc99_hook_func);

#define JKC99_INSTALL_HOOK(name) void name(ParseContext *ctx, HookKind kind, jkc99_hook_func *func, void *ptr)
typedef JKC99_INSTALL_HOOK(jkc99_install_hook_func);

#define JKC99_READ_ENTIRE_FILE(name) const char *name(const char *filename, size_t *len)
typedef JKC99_READ_ENTIRE_FILE(jkc99_read_entire_file_func);

#define JKC99_WRITE_BUFFER_TO_FILE(name) int name(const char *buf, size_t len, const char *filename)
typedef JKC99_WRITE_BUFFER_TO_FILE(jkc99_write_buffer_to_file_func);

typedef struct Hook {
    jkc99_hook_func                 *func;
    void                            *ptr;
} Hook;


typedef struct JKC99Version {
    size_t          size;
    size_t          keywordCount;
    const char      **keywords;
    int             compiler;
    size_t          hookCount;
} JKC99Version;

#define JKC99_GET_VERSION(name) JKC99Version name(void)
typedef JKC99_GET_VERSION(jkc99_get_version_func);

#define JKC99_MODULE_INIT(name) bool name(ParseContext *ctx)
typedef JKC99_MODULE_INIT(jkc99_module_init_func);

#define OPTION_FUNC(name) bool option_##name(UNUSED ParseContext *ctx, UNUSED const char *value)
typedef OPTION_FUNC(func);

typedef struct Option {
    const char      letter;
    const char      *name;
    size_t          nameLen;
    const char      *helpString;
    option_func     *func;
} Option;

#define OPTION(l,n,h) { .letter=l, .name=#n, .nameLen=(sizeof(#n)-1), .helpString=h, .func=option_##n }

#define JKC99_MODULE_GET_OPTIONS(name) Option *name(size_t *count)
typedef JKC99_MODULE_GET_OPTIONS(jkc99_module_get_options_func);

typedef struct JKC99Module {
    const char                      *name;
    size_t                          optionCount;
    Option                          *options;
    jkc99_get_version_func          *get_version;
    jkc99_module_init_func          *init;
    jkc99_module_get_options_func   *get_options;
} JKC99Module;

JKC99_API void jkc99_lexer_next(ParseContext *ctx);
JKC99_API void jkc99_lexer_source(ParseContext *ctx, Source *src);
JKC99_API void jkc99_lexer_rollback(ParseContext *ctx, Source *src);
JKC99_API bool jkc99_lexer_is(ParseContext *ctx, long type);
JKC99_API bool jkc99_lexer_is_id(ParseContext *ctx, const char *id);
JKC99_API bool jkc99_lexer_match(ParseContext *ctx, long type);
JKC99_API bool jkc99_lexer_match_id(ParseContext *ctx, const char *id);
JKC99_API bool jkc99_lexer_require(ParseContext *ctx, long type);
JKC99_API bool jkc99_lexer_require_id(ParseContext *ctx, const char *id);
#define jkc99_lexer_is_kw(ctx,keyword) jkc99_lexer_is_id(ctx,kw[kw_##keyword])
#define jkc99_lexer_match_kw(ctx,keyword) jkc99_lexer_match_id(ctx,kw[kw_##keyword])
#define jkc99_lexer_require_kw(ctx,keyword) jkc99_lexer_require_id(ctx,kw[kw_##keyword])

JKC99_API void     jkc99_scope_push(ParseContext *ctx);
JKC99_API void     jkc99_scope_pop(ParseContext *ctx);

JKC99_API void     jkc99_sym_push(ParseContext *ctx, Symbol *sym);
JKC99_API Symbol  *jkc99_sym_get(ParseContext *ctx, const char *identifier);
JKC99_API Symbol  *jkc99_sym_get_current_scope(ParseContext *ctx, const char *identifier);
JKC99_API Symbol  *jkc99_sym_get_global_index(ParseContext *ctx, size_t index);
JKC99_API size_t   jkc99_sym_count_global(ParseContext *ctx);
JKC99_API size_t   jkc99_sym_count_current_scope(ParseContext *ctx);

JKC99_API TypeHandle   jkc99_type_void(ParseContext *ctx);
JKC99_API TypeHandle   jkc99_type_char(ParseContext *ctx);
JKC99_API TypeHandle   jkc99_type_signed_char(ParseContext *ctx);
JKC99_API TypeHandle   jkc99_type_signed_short(ParseContext *ctx);
JKC99_API TypeHandle   jkc99_type_signed_int(ParseContext *ctx);
JKC99_API TypeHandle   jkc99_type_signed_long(ParseContext *ctx);
JKC99_API TypeHandle   jkc99_type_signed_long_long(ParseContext *ctx);
#define jkc99_type_short        jkc99_type_signed_short
#define jkc99_type_int          jkc99_type_signed_int
#define jkc99_type_long         jkc99_type_signed_long
#define jkc99_type_long_long    jkc99_type_signed_long_long
JKC99_API TypeHandle   jkc99_type__Bool(ParseContext *ctx);
JKC99_API TypeHandle   jkc99_type_unsigned_char(ParseContext *ctx);
JKC99_API TypeHandle   jkc99_type_unsigned_short(ParseContext *ctx);
JKC99_API TypeHandle   jkc99_type_unsigned_int(ParseContext *ctx);
JKC99_API TypeHandle   jkc99_type_unsigned_long(ParseContext *ctx);
JKC99_API TypeHandle   jkc99_type_unsigned_long_long(ParseContext *ctx);
JKC99_API TypeHandle   jkc99_type_float(ParseContext *ctx);
JKC99_API TypeHandle   jkc99_type_double(ParseContext *ctx);
JKC99_API TypeHandle   jkc99_type_long_double(ParseContext *ctx);
JKC99_API TypeHandle   jkc99_type_float__Complex(ParseContext *ctx);
JKC99_API TypeHandle   jkc99_type_double__Complex(ParseContext *ctx);
JKC99_API TypeHandle   jkc99_type_long_double__Complex(ParseContext *ctx);
JKC99_API TypeHandle   jkc99_type_size_t(ParseContext *ctx);
#ifdef __clang__
JKC99_API TypeHandle   jkc99_type___builtin_va_list(ParseContext *ctx);
#endif
JKC99_API TypeHandle   jkc99_type_struct(ParseContext *ctx, const char *tag);
JKC99_API TypeHandle   jkc99_type_union(ParseContext *ctx, const char *tag);
JKC99_API TypeHandle   jkc99_type_enum(ParseContext *ctx, const char *tag);
JKC99_API TypeHandle   jkc99_type_qualified(ParseContext *ctx, TypeHandle base, int qualifiers);
JKC99_API TypeHandle   jkc99_type_ptr(ParseContext *ctx, TypeHandle base);
JKC99_API TypeHandle   jkc99_type_array(ParseContext *ctx, TypeHandle base, Expr *countExpr, struct Initializer *init);
JKC99_API TypeHandle   jkc99_type_array_vla(ParseContext *ctx, TypeHandle base, Expr *countExpr);
JKC99_API TypeHandle   jkc99_type_func(ParseContext *ctx, TypeHandle returnType, size_t paramCount, TypeHandle *paramTypes, bool hasEllipsis);
JKC99_API TypeHandle   jkc99_type_vector(ParseContext *ctx, TypeHandle base, int bytes);
JKC99_API TypeHandle   jkc99_type_bitfield(ParseContext *ctx, TypeHandle base, Expr *bitCountExpr);

JKC99_API size_t       jkc99_type_count(ParseContext *ctx);
JKC99_API TypeHandle   jkc99_type_first(ParseContext *ctx);
JKC99_API TypeHandle   jkc99_type_next(ParseContext *ctx, TypeHandle ref, size_t count);

JKC99_API void         jkc99_type_add_user(ParseContext *ctx, TypeHandle handle, size_t index);
JKC99_API void         jkc99_type_get(ParseContext *ctx, TypeHandle handle, JKC99Type *type);
JKC99_API void         jkc99_type_set(ParseContext *ctx, TypeHandle handle, JKC99Type *type);

JKC99_API _Bool        jkc99_type_is_signed(ParseContext *ctx, TypeHandle type);

JKC99_API void                  *jkc99_declaration_alloc(ParseContext *ctx, Source *src, size_t size, size_t specifierCount, DeclarationSpecifier *specifiers);
JKC99_API Declaration           *jkc99_declaration(ParseContext *ctx, Source *src, size_t specCount, DeclarationSpecifier *specs, size_t declaratorCount, InitDeclarator *declarators);
JKC99_API StructDeclaration     *jkc99_declaration_struct(ParseContext *ctx, Source *src, size_t specifierCount, DeclarationSpecifier *specifiers, size_t declaratorCount, StructDeclarator *declarators);
JKC99_API ParameterDeclaration  *jkc99_declaration_parameter(ParseContext *ctx, Source *src, size_t specifierCount, DeclarationSpecifier *specs, Declarator *declarator, AbstractDeclarator *abstractDeclarator);
JKC99_API FunctionDefinition    *jkc99_function_definition(ParseContext *ctx, Source *src, size_t specifierCount, DeclarationSpecifier *specifiers, Declarator *declarator, size_t declarationCount, Declaration **declarations, Stmt *stmt, const char *name, TypeHandle type);

JKC99_API void                  jkc99_declaration_specifiers_get_type_storage_class_and_function_specifier(ParseContext *ctx, size_t specifierCount, DeclarationSpecifier *specifiers, TypeHandle *handle, StorageClassKind *sclass, FunctionSpecifier *fspec);

JKC99_API Declarator        *jkc99_declarator(ParseContext *ctx, Source *src, size_t beforeAttrCount, Attribute *beforeAttrs, size_t afterAttrCount, Attribute *afterAttrs, size_t ptrCount, Pointer *pointers, DirectDeclarator *dd);
JKC99_API DirectDeclarator  *jkc99_direct_declarator_alloc(ParseContext *ctx, Source *src, DirectDeclaratorKind kind);
JKC99_API DirectDeclarator  *jkc99_direct_declarator_identifier(ParseContext *ctx, Source *src, const char *identifier);
JKC99_API DirectDeclarator  *jkc99_direct_declarator_declarator(ParseContext *ctx, Source *src, Declarator *declarator);
JKC99_API DirectDeclarator  *jkc99_direct_declarator_array_vla(ParseContext *ctx, Source *src, size_t attributeCount, Attribute *attributes, DirectDeclarator *direct, size_t qualifierCount, TypeQualifier *qualifiers);
JKC99_API DirectDeclarator  *jkc99_direct_declarator_array(ParseContext *ctx, Source *src, size_t attributeCount, Attribute *attributes, DirectDeclarator *direct, size_t qualifierCount, TypeQualifier *qualifiers, bool isStatic, Expr *expr);
JKC99_API DirectDeclarator  *jkc99_direct_declarator_func_params(ParseContext *ctx, Source *src, DirectDeclarator *direct, CallingConvention callingConvention, size_t paramCount, ParameterDeclaration **params, bool hasEllipsis);
JKC99_API DirectDeclarator  *jkc99_direct_declarator_func_identifiers(ParseContext *ctx, Source *src, DirectDeclarator *direct, CallingConvention callingConvention, size_t idCount, const char **identifiers, bool hasEllipsis);

JKC99_API TypeHandle        jkc99_declarator_get_type_and_name(ParseContext *ctx, Declarator *dd, TypeHandle type, const char **name, Initializer *initializer);
JKC99_API TypeHandle        jkc99_direct_declarator_get_type_and_name(ParseContext *ctx, DirectDeclarator *dd, TypeHandle typeHandle, const char **name, Initializer *initializer);

JKC99_API AbstractDeclarator        *jkc99_abstract_declarator(ParseContext *ctx, Source *src, size_t beforeAttrCount, Attribute *beforeAttrs, size_t afterAttrCount, Attribute *afterAttrs, size_t ptrCount, Pointer *pointers, DirectAbstractDeclarator *dd);
JKC99_API DirectAbstractDeclarator  *jkc99_direct_abstract_declarator_alloc(ParseContext *ctx, Source *src, DirectAbstractDeclaratorKind kind);
JKC99_API DirectAbstractDeclarator  *jkc99_direct_abstract_declarator_declarator(ParseContext *ctx, Source *src, AbstractDeclarator *declarator);
JKC99_API DirectAbstractDeclarator  *jkc99_direct_abstract_declarator_array_vla(ParseContext *ctx, Source *src, DirectAbstractDeclarator *direct);
JKC99_API DirectAbstractDeclarator  *jkc99_direct_abstract_declarator_array(ParseContext *ctx, Source *src, size_t attributeCount, Attribute *attributes, DirectAbstractDeclarator *direct, size_t qualifierCount, TypeQualifier *qualifiers, bool isStatic, Expr *expr);
JKC99_API DirectAbstractDeclarator  *jkc99_direct_abstract_declarator_func_params(ParseContext *ctx, Source *src, DirectAbstractDeclarator *direct, CallingConvention callingConvention, size_t paramCount, ParameterDeclaration **params, bool hasEllipsis);

JKC99_API TypeHandle                jkc99_abstract_declarator_get_type(ParseContext *ctx, AbstractDeclarator *d, TypeHandle type);
JKC99_API TypeHandle                jkc99_direct_abstract_declarator_get_type(ParseContext *ctx, DirectAbstractDeclarator *dd, TypeHandle typeHandle);


JKC99_API Stmt *jkc99_stmt_alloc(ParseContext *ctx, Source *src, StmtKind kind);
JKC99_API Stmt *jkc99_stmt_label(ParseContext *ctx, Source *src, size_t attrCount, Attribute *attrs, const char *label, Stmt *stmt);
JKC99_API Stmt *jkc99_stmt_case(ParseContext *ctx, Source *src, Expr *expr, Stmt *stmt);
JKC99_API Stmt *jkc99_stmt_default(ParseContext *ctx, Source *src, Stmt *stmt);
JKC99_API Stmt *jkc99_stmt_compound(ParseContext *ctx, Source *src, size_t count, BlockItem *items);
JKC99_API Stmt *jkc99_stmt_compound_push(ParseContext *ctx, Stmt *compound, BlockItem blockItem);
JKC99_API Stmt *jkc99_stmt_expr(ParseContext *ctx, Source *src, size_t attributeCount, Attribute *attributes, Expr *expr);
JKC99_API Stmt *jkc99_stmt_if(ParseContext *ctx, Source *src, Expr *expr, Stmt *stmt, Stmt *elseStmt);
JKC99_API Stmt *jkc99_stmt_switch(ParseContext *ctx, Source *src, Expr *expr, Stmt *stmt);
JKC99_API Stmt *jkc99_stmt_while(ParseContext *ctx, Source *src, Expr *expr, Stmt *stmt);
JKC99_API Stmt *jkc99_stmt_do_while(ParseContext *ctx, Source *src, Expr *expr, Stmt *stmt);
JKC99_API Stmt *jkc99_stmt_for(ParseContext *ctx, Source *src, Declaration *initDecl, Expr *initExpr, Expr *condExpr, Expr *iterExpr, Stmt *stmt);
JKC99_API Stmt *jkc99_stmt_goto(ParseContext *ctx, Source *src, const char *identifier);
JKC99_API Stmt *jkc99_stmt_continue(ParseContext *ctx, Source *src);
JKC99_API Stmt *jkc99_stmt_break(ParseContext *ctx, Source *src);
JKC99_API Stmt *jkc99_stmt_return(ParseContext *ctx, Source *src, Expr *expr);
#if HAS_GCC_ASM
JKC99_API Stmt *jkc99_stmt_asm_basic(ParseContext *ctx, Source *src, int qualifiers, const char *instructions);
JKC99_API Stmt *jkc99_stmt_asm_extended(ParseContext *ctx, Source *src, int qualifiers, const char *instructions, StmtAsmOutputOperand *outputOperands, StmtAsmInputOperand *inputOperands, StmtAsmClobber *clobbers, const char **gotoLabels);
#endif

JKC99_API Expr *jkc99_expr_expr(ParseContext *ctx, Source *src, Expr *expr);
JKC99_API Expr *jkc99_expr_string(ParseContext *ctx, Source *src, const char *str);
JKC99_API Expr *jkc99_expr_int(ParseContext *ctx, Source *src, const char *str, unsigned long long intlit, ConstantRepresentation repr, size_t suffixLen, const char *suffix);
JKC99_API Expr *jkc99_expr_float(ParseContext *ctx, Source *src, const char *str, double floatlit, ConstantRepresentation repr, size_t suffixLen, const char *suffix);
JKC99_API Expr *jkc99_expr_char(ParseContext *ctx, Source *src, char charlit);
JKC99_API Expr *jkc99_expr_identifier(ParseContext *ctx, Source *src, const char *identifier);
JKC99_API Expr *jkc99_expr_compound(ParseContext *ctx, Source *src, TypeName *typeName, InitializerList initializerList);
JKC99_API Expr *jkc99_expr_index(ParseContext *ctx, Source *src, Expr *expr, Expr *indexExpr);
#ifdef __clang__
JKC99_API Expr *jkc99_expr_va_arg(ParseContext *ctx, Source *src, Expr *expr, Expr *list, TypeName *typeName);
#endif
JKC99_API Expr *jkc99_expr_call(ParseContext *ctx, Source *src, Expr *expr, Expr **args);
JKC99_API Expr *jkc99_expr_dot(ParseContext *ctx, Source *src, Expr *expr, const char *identifier);
JKC99_API Expr *jkc99_expr_arrow(ParseContext *ctx, Source *src, Expr *expr, const char *identifier);
JKC99_API Expr *jkc99_expr_increment(ParseContext *ctx, Source *src, Expr *expr);
JKC99_API Expr *jkc99_expr_decrement(ParseContext *ctx, Source *src, Expr *expr);
JKC99_API Expr *jkc99_expr_unary(ParseContext *ctx, Source *src, ExprUnaryKind kind, Expr *expr);
JKC99_API Expr *jkc99_expr_sizeof_type(ParseContext *ctx, Source *src, TypeName *typeName);
JKC99_API Expr *jkc99_expr_offsetof(ParseContext *ctx, Source *src, TypeName *typeName, const char *field);
JKC99_API Expr *jkc99_expr_cast(ParseContext *ctx, Source *src, TypeName *typeName, Expr *expr);
JKC99_API Expr *jkc99_expr_multiplicative(ParseContext *ctx, Source *src, ExprMultiplicativeKind kind, Expr *left, Expr *right);
JKC99_API Expr *jkc99_expr_additive(ParseContext *ctx, Source *src, ExprAdditiveKind kind, Expr *left, Expr *right);
JKC99_API Expr *jkc99_expr_shift(ParseContext *ctx, Source *src, ExprShiftKind kind, Expr *left, Expr *right);
JKC99_API Expr *jkc99_expr_relational(ParseContext *ctx, Source *src, ExprRelationalKind kind, Expr *left, Expr *right);
JKC99_API Expr *jkc99_expr_equality(ParseContext *ctx, Source *src, ExprEqualityKind kind, Expr *left, Expr *right);
JKC99_API Expr *jkc99_expr_and(ParseContext *ctx, Source *src, Expr *left, Expr *right);
JKC99_API Expr *jkc99_expr_exclusive_or(ParseContext *ctx, Source *src, Expr *left, Expr *right);
JKC99_API Expr *jkc99_expr_inclusive_or(ParseContext *ctx, Source *src, Expr *left, Expr *right);
JKC99_API Expr *jkc99_expr_logical_and(ParseContext *ctx, Source *src, Expr *left, Expr *right);
JKC99_API Expr *jkc99_expr_logical_or(ParseContext *ctx, Source *src, Expr *left, Expr *right);
JKC99_API Expr *jkc99_expr_conditional(ParseContext *ctx, Source *src, Expr *cond, Expr *left, Expr *right);
JKC99_API Expr *jkc99_expr_assign(ParseContext *ctx, Source *src, ExprAssignmentKind kind, Expr *left, Expr *right);
JKC99_API Expr *jkc99_expr_comma(ParseContext *ctx, Source *src, Expr *left, Expr *right);

JKC99_API const char *jkc99_parse_identifier(ParseContext *ctx);
JKC99_API const char **jkc99_parse_identifier_list(ParseContext *ctx, size_t *count, bool *hasEllipsis);
JKC99_API const char *jkc99_parse_string(ParseContext *ctx);

JKC99_API ExternalDeclaration jkc99_parse_external_declaration(ParseContext *ctx);
JKC99_API Declaration *jkc99_parse_declaration(ParseContext *ctx);

JKC99_API Stmt *jkc99_parse_stmt(ParseContext *ctx);
JKC99_API Stmt *jkc99_parse_stmt_compound(ParseContext *ctx);

JKC99_API Expr *jkc99_parse_expr_primary(ParseContext *ctx);
JKC99_API Expr *jkc99_parse_expr_postfix(ParseContext *ctx);
JKC99_API Expr *jkc99_parse_expr_unary(ParseContext *ctx);
JKC99_API Expr *jkc99_parse_expr_cast(ParseContext *ctx);
JKC99_API Expr *jkc99_parse_expr_multiplicative(ParseContext *ctx);
JKC99_API Expr *jkc99_parse_expr_additive(ParseContext *ctx);
JKC99_API Expr *jkc99_parse_expr_shift(ParseContext *ctx);
JKC99_API Expr *jkc99_parse_expr_relational(ParseContext *ctx);
JKC99_API Expr *jkc99_parse_expr_equality(ParseContext *ctx);
JKC99_API Expr *jkc99_parse_expr_and(ParseContext *ctx);
JKC99_API Expr *jkc99_parse_expr_exclusive_or(ParseContext *ctx);
JKC99_API Expr *jkc99_parse_expr_inclusive_or(ParseContext *ctx);
JKC99_API Expr *jkc99_parse_expr_logical_and(ParseContext *ctx);
JKC99_API Expr *jkc99_parse_expr_logical_or(ParseContext *ctx);
JKC99_API Expr *jkc99_parse_expr_conditional(ParseContext *ctx);
JKC99_API Expr *jkc99_parse_expr_constant(ParseContext *ctx);
JKC99_API Expr *jkc99_parse_expr_assignment(ParseContext *ctx);
JKC99_API Expr *jkc99_parse_expr(ParseContext *ctx);

JKC99_API TypeName *jkc99_parse_type_name(ParseContext *ctx);

JKC99_API DeclarationSpecifier  *jkc99_parse_specifier_qualifier_list(ParseContext *ctx, size_t *count);
JKC99_API TypeQualifier         *jkc99_parse_type_qualifier_list(ParseContext *ctx, size_t *count);
JKC99_API CallingConvention     jkc99_parse_calling_convention(ParseContext *ctx);
JKC99_API Pointer               *jkc99_parse_pointer(ParseContext *ctx, Pointer *list);
JKC99_API AbstractDeclarator    *jkc99_parse_abstract_declarator(ParseContext *ctx);
JKC99_API _Bool                 jkc99_parse_parameter_list(ParseContext *ctx, size_t *count, ParameterDeclaration ***list, bool *hasEllipsis);

JKC99_API Declarator        *jkc99_parse_declarator(ParseContext *ctx);
JKC99_API DirectDeclarator  *jkc99_parse_direct_declarator(ParseContext *ctx);

JKC99_API InitDeclarator    *jkc99_parse_init_declarator_list(ParseContext *ctx, size_t *count);

JKC99_API InitializerList   jkc99_parse_initializer_list(ParseContext *ctx);
JKC99_API Initializer       jkc99_parse_initializer(ParseContext *ctx);

JKC99_API Enumerator        *jkc99_parse_enumerator_list(ParseContext *ctx, size_t *count);

JKC99_API bool      jkc99_parse_attribute(ParseContext *ctx, Attribute *attr);
JKC99_API Attribute *jkc99_parse_attribute_list(ParseContext *ctx, size_t *count);

JKC99_API ExprResult jkc99_eval_expr(ParseContext *ctx, TypeHandle expected, Expr *expr);

JKC99_API const char *jkc99_str_intern_range(ParseContext *ctx, const char *str, size_t len);
JKC99_API const char *jkc99_str_intern(ParseContext *ctx, const char *str);

JKC99_API void jkc99_log_error(ParseContext *ctx, const char *format, ...);
JKC99_API void jkc99_log_error_src(ParseContext *ctx, Source *src, const char *format, ...);
JKC99_API void jkc99_log_error_unexpected(ParseContext *ctx);

JKC99_API ParseBuffer   jkc99_buffer_new(ParseContext *ctx, const char *filename, size_t bufSize, const char *buffer);
JKC99_API void          jkc99_buffer_set(ParseContext *ctx, ParseBuffer lexer);
JKC99_API ParseBuffer   jkc99_buffer_get(ParseContext *ctx);
JKC99_API const char    *jkc99_buffer_file(ParseContext *ctx, ParseBuffer buf);
JKC99_API void          jkc99_buffer_parse(ParseContext *ctx, ParseBuffer buf);

JKC99_API size_t                jkc99_ext_decl_count(ParseContext *ctx);
JKC99_API ExternalDeclaration   *jkc99_ext_decl_first(ParseContext *ctx);
JKC99_API ExternalDeclaration   *jkc99_ext_decl_next(ParseContext *ctx, ExternalDeclaration *ref, size_t count);

JKC99_API void jkc99_parse(ParseContext *ctx);

JKC99_API JKC99_INSTALL_HOOK(jkc99_install_hook);

JKC99_API JKC99_READ_ENTIRE_FILE(jkc99_os_read_entire_file);
JKC99_API JKC99_WRITE_BUFFER_TO_FILE(jkc99_os_write_buffer_to_file);



static const JKC99Version gVersion = {
    .size = sizeof(struct JKC99Version),
    .keywordCount = jkc99_array_count(kw),
    .keywords = kw,
    .compiler = JKC99_COMPILER_ID,
    .hookCount = kHookCount
};

EXPORT JKC99_GET_VERSION(jkc99_get_version) {
    return gVersion;
}

#endif /* JKC99_H */
