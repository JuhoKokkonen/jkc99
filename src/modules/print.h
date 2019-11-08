#include "../jkc99.h"
#include "../dynamic_array.h"

static int gPrintIndent = 0;
static bool gPrintDirectives = true;
static bool gPrintMinify = false;
static Source gPrintSource = {0};

#ifndef PRINT_INDENT 
#define PRINT_INDENT 4
#endif

#if defined(JK_DYNAMIC_ARRAY_H) && !defined(JK_DYNAMIC_ARRAY_NO_PRINTF)
static char **gPrintBuffer = NULL;
#define printf_(...) (gPrintBuffer ? da_printf(*gPrintBuffer, __VA_ARGS__) : printf(__VA_ARGS__))
#else
#define printf_ printf
#endif

const char *gSpaces = "                                                                                                                                                                                                                                                                                                                                                                                                                    ";

#define print_indent() (gPrintMinify ? 0 : printf_("%.*s", gPrintIndent*PRINT_INDENT, gSpaces))
#define print_indent_inc() gPrintIndent++
#define print_indent_dec() gPrintIndent--

//#define NEWLINE_DEFAULT() printf_("\n"); gPrintSource.line++; print_indent()
static inline void print_newline(void *ptr) {
    ASTCommon *node = ptr;
    if(node && 
        node->src.file == gPrintSource.file && 
        node->src.line == gPrintSource.line) {
        printf_(" ");
    } else {
        if(gPrintMinify) {
            printf_(" ");
        } else {
            printf_("\n");
            gPrintSource.line++; 
            print_indent();
        }
    }
}

static void print_directives(void *ptr);

static void print_declspec(Declspec *ds);
static void print_preprocessor_directive(PreprocessorDirective *dir);
static void print_declaration(Declaration *decl);
static void print_struct_declaration(StructDeclaration *decl);
static void print_declaration_specifier(DeclarationSpecifier *spec);
static void print_declarator(Declarator *d);
static void print_abstract_declarator(AbstractDeclarator *d);
static void print_initializer(Initializer *init);
static void print_initializer_list(InitializerList *list);
static void print_expr(Expr *expr);
static void print_stmt(Stmt *stmt);

const char *gLineMarkerFlagString[] = {
    [0] = "",
    [bLineMarkerNewFile] = " 1",
    [bLineMarkerNewFile | bLineMarkerReturnToFile] = " 1 2",
    [bLineMarkerNewFile | bLineMarkerReturnToFile | bLineMarkerSystemHeader] = " 1 2 3",
    [bLineMarkerNewFile | bLineMarkerReturnToFile | bLineMarkerSystemHeader | bLineMarkerExternC] = " 1 2 3 4",
    [bLineMarkerNewFile | bLineMarkerSystemHeader] = " 1 3",
    [bLineMarkerNewFile | bLineMarkerSystemHeader | bLineMarkerExternC] = " 1 3 4",
    [bLineMarkerNewFile | bLineMarkerReturnToFile | bLineMarkerExternC] = " 1 2 4",
    [bLineMarkerNewFile | bLineMarkerExternC] = " 1 4",
    [bLineMarkerReturnToFile] = " 2",
    [bLineMarkerReturnToFile | bLineMarkerSystemHeader] = " 2 3",
    [bLineMarkerReturnToFile | bLineMarkerSystemHeader | bLineMarkerExternC] = " 2 3 4",
    [bLineMarkerReturnToFile | bLineMarkerExternC] = " 2 4",
    [bLineMarkerSystemHeader] = " 3",
    [bLineMarkerSystemHeader | bLineMarkerExternC] = " 3 4",
    [bLineMarkerExternC] = " 4",
};

static void print_directives(void *ptr) {
    ASTCommon *n = ptr;

    if(!gPrintDirectives) {
        return;
    }

    for(size_t i = 0; i < n->directiveCount; ++i) {
        print_preprocessor_directive(n->directives + i);
    }
    if(n->directiveCount) {
        printf_("\n");
        print_indent();
    }

    if(!n->src.file || !n->src.line) {
        return;
    }

    if(n->src.file != gPrintSource.file || n->src.line != gPrintSource.line) {
        if(n->src.file == gPrintSource.file && n->src.line > gPrintSource.line && n->src.line <= (gPrintSource.line+10)) {
            while(gPrintSource.line < n->src.line) {
                print_newline(NULL);
            }
        } else {
            gPrintSource.file = n->src.file;
            gPrintSource.line = n->src.line;
            gPrintSource.lineMarkerFlags = n->src.lineMarkerFlags;
            jkc99_assert(gLineMarkerFlagString[gPrintSource.lineMarkerFlags & (~bLineMarkerReturnToFile)]);
            printf_("\n# %u \"%s\"%s\n",
                    gPrintSource.line,
                    gPrintSource.file,
                    gLineMarkerFlagString[gPrintSource.lineMarkerFlags & (~(bLineMarkerNewFile | bLineMarkerReturnToFile))]);
            print_indent();
        }
    } else {
#if 0
        if(n->src.column > gPrintSource.column) {
            int count = (n->src.column - gPrintSource.column);
            printf_("%*.s", count, gSpaces);
        }
#endif
    }
}

static void print_preprocessor_directive(PreprocessorDirective *dir) {
    switch(dir->kind) {
        case kPreprocessorDirectiveLine:
            {
                jkc99_assert(dir->u.line.file);
                jkc99_assert(dir->u.line.line);
                printf_("\n#line %u \"%s\"", dir->u.line.line, dir->u.line.file);
                gPrintSource.file = dir->u.line.file;
                gPrintSource.line = dir->u.line.line;
                gPrintSource.lineMarkerFlags = 0;
            } break;
        case kPreprocessorDirectiveGCCLineMarker:
            {
                jkc99_assert(dir->u.lineMarker.file);
                jkc99_assert(dir->u.lineMarker.line);
                jkc99_assert(gLineMarkerFlagString[dir->u.lineMarker.flags]);
                printf_("\n# %u \"%s\"%s", 
                        dir->u.lineMarker.line, 
                        dir->u.lineMarker.file,
                        gLineMarkerFlagString[dir->u.lineMarker.flags]);
                gPrintSource.file = dir->u.lineMarker.file;
                gPrintSource.line = dir->u.lineMarker.line;
                gPrintSource.lineMarkerFlags = dir->u.lineMarker.flags;
            } break;
        case kPreprocessorDirectivePragma:
            {
                jkc99_assert(dir->u.pragma);
                printf_("\n#pragma %s", dir->u.pragma);
            } break;
        default: jkc99_assert(false); break;
    }
}

static void print_attribute(Attribute *attr) {
    jkc99_assert(attr);
    int len = (int)(attr->strEnd - attr->strBegin);
    jkc99_assert(len);
    print_directives(attr);
    printf_("__attribute__%.*s", len, attr->strBegin);
}

static void print_declaration(Declaration *decl) {
    size_t i;

    print_directives(decl);

    if(decl->hasExtension) {
        printf_("__extension__ ");
    }

    for(i = 0; i < decl->specifierCount; ++i) {
        if(i) printf_(" ");
        print_declaration_specifier(decl->specifiers + i);
    }

    if(decl->initDeclaratorCount) {
        printf_(" ");
        for(i = 0; i < decl->initDeclaratorCount; ++i) {
            InitDeclarator *id = decl->initDeclarators + i;

            if(i) printf_(", ");

            print_declarator(id->declarator);
            if(id->initializer.kind) {
                printf_(" = ");
                print_initializer(&id->initializer);
            }
        }
    }

#if HAS_GCC_ASM
    if(decl->asmStmt) {
        jkc99_assert(decl->asmStmt->kind == kStmtAsm);
        printf_(" ");
        print_stmt(decl->asmStmt);
    }
#endif

    printf_(";");
}

static void print_struct_declaration(StructDeclaration *decl) {
    size_t i;

    print_directives(decl);

    if(decl->hasExtension) {
        printf_("__extension__ ");
    }

    for(i = 0; i < decl->specifierCount; ++i) {
        print_declaration_specifier(decl->specifiers + i);
        printf_(" ");
    }

    for(i = 0; i < decl->declaratorCount; ++i) {
        StructDeclarator *sd = decl->declarators + i;

        if(i) printf_(", ");

        if(sd->declarator) {
            print_declarator(sd->declarator);
        }
        if(sd->bitCountExpr) {
            printf_(" : ");
            print_expr(sd->bitCountExpr);
        }
    }

    printf_(";");
}

static void print_function_definition(FunctionDefinition *func) {
    size_t i;

    print_directives(func);

    if(func->hasExtension) {
        printf_("__extension__ ");
    }

    for(i = 0; i < func->specifierCount; ++i) {
        print_declaration_specifier(func->specifiers + i);
        printf_(" ");
    }

    print_declarator(func->declarator);

    for(i = 0; i < func->declarationCount; ++i) {
        print_declaration(func->declarations[i]);
    }

    printf_(" ");
    print_stmt(func->body);
}

static void print_parameter_declaration(ParameterDeclaration *decl) {
    size_t i;

    print_directives(decl);

    if(decl->hasExtension) {
        printf_("__extension__ ");
    }

    for(i = 0; i < da_count(decl->specifiers); ++i) {
        if(i) printf_(" ");
        print_declaration_specifier(decl->specifiers + i);
    }

    if(decl->declarator) {
        printf_(" ");
        print_declarator(decl->declarator);
    } else if(decl->abstractDeclarator) {
        printf_(" ");
        print_abstract_declarator(decl->abstractDeclarator);
    }
}

static inline void print_storage_class(StorageClass sc) {
    print_directives(&sc);

    switch(sc.kind) {
        case kStorageClassTypedef:      printf_("typedef");         break;
        case kStorageClassExtern:       printf_("extern");          break;
        case kStorageClassStatic:       printf_("static");          break;
        case kStorageClassAuto:         printf_("auto");            break;
        case kStorageClassRegister:     printf_("register");        break;
        default:                        jkc99_assert(false);           break;
    }
}

static void print_struct_or_union_specifier(StructOrUnionSpecifier *su, _Bool printTag) {
    size_t i;

    for(i = 0; i < su->attributeCount; ++i) {
        printf_(" ");
        print_attribute(su->attributes + i);
    }

    for(i = 0; i < su->declspecCount; ++i) {
        printf_(" ");
        print_declspec(su->declspecs + i);
    }

    if(printTag && su->identifier) {
        printf_(" %s", su->identifier);
    }
    if(su->memberDeclarationCount) {
        unsigned int line = gPrintSource.line;
        printf_(" {");
        print_indent_inc();
        for(i = 0; i < su->memberDeclarationCount; ++i) {
            print_newline(su->memberDeclarations[i]);
            print_struct_declaration(su->memberDeclarations[i]);
        }
        print_indent_dec();
        if(gPrintSource.line != line) {
            print_newline(NULL);
            printf_("}");
        } else {
            printf_(" }");
        }
    }
}

static void print_enum_specifier(EnumSpecifier *e) {
    size_t i;
    printf_("enum ");

    for(i = 0; i < e->attributeCount; ++i) {
        print_attribute(e->attributes + i);
        printf_(" ");
    }

    for(i = 0; i < e->declspecCount; ++i) {
        print_declspec(e->declspecs + i);
        printf_(" ");
    }

    if(e->identifier) {
        printf_("%s ", e->identifier);
    }
    if(e->enumeratorCount) {
        unsigned int line = gPrintSource.line;
        printf_("{");
        print_indent_inc();
        for(size_t i = 0; i < e->enumeratorCount; ++i) {
            if(i) printf_(",");
            print_newline(e->enumerators + i);
            print_directives(e->enumerators + i);
            printf_("%s", e->enumerators[i].identifier);
            if(e->enumerators[i].expr) {
                printf_(" = ");
                print_expr(e->enumerators[i].expr);
            }
        }
        print_indent_dec();
        if(gPrintSource.line != line) {
            print_newline(NULL);
            printf_("}");
        } else {
            printf_(" }");
        }
    }
}

static inline void print_type_specifier(TypeSpecifier *ts) {
    print_directives(ts);

    switch(ts->kind) {
        case kTypeSpecifierVoid:                printf_("void");                break;
        case kTypeSpecifierChar:                printf_("char");                break;
        case kTypeSpecifierShort:               printf_("short");               break;
        case kTypeSpecifierInt:                 printf_("int");                 break;
        case kTypeSpecifierLong:                printf_("long");                break;
        case kTypeSpecifierFloat:               printf_("float");               break;
        case kTypeSpecifierDouble:              printf_("double");              break;
        case kTypeSpecifierSigned:              printf_("signed");              break;
        case kTypeSpecifierUnsigned:            printf_("unsigned");            break;
        case kTypeSpecifier_Bool:               printf_("_Bool");               break;
        case kTypeSpecifier_Complex:            printf_("_Complex");            break;
        case kTypeSpecifierTypedefName:         printf_("%s", ts->u.t);         break;
        case kTypeSpecifierStruct:
            {
                printf_("struct");
                print_struct_or_union_specifier(&ts->u.su, true);
            } break;
        case kTypeSpecifierUnion:
            {
                printf_("union");
                print_struct_or_union_specifier(&ts->u.su, true);
            } break;
        case kTypeSpecifierEnum:
            {
                print_enum_specifier(&ts->u.e);
            } break;
        case kTypeSpecifier__int8:              printf_("__int8");              break;
        case kTypeSpecifier__int16:             printf_("__int16");             break;
        case kTypeSpecifier__int32:             printf_("__int32");             break;
        case kTypeSpecifier__int64:             printf_("__int64");             break;
        case kTypeSpecifierBuiltin: 
            {
#ifdef __clang__
                if(ts->u.t == kw[kw___builtin_va_list]) {
                    printf_("__builtin_va_list");
                    break;
                }
#endif
                jkc99_assert(false);
            } break;
        default:                                jkc99_assert(false);               break;
    }
}

static inline void print_type_qualifier(TypeQualifier *tq) {
    print_directives(tq);

    switch(tq->kind) {
        case kTypeQualifierConst:
            {
                printf_("const");
            } break;
        case kTypeQualifierRestrict:
            {
                switch(tq->format) {
                    case bTypeQualifierFormatQ:     printf_("restrict");       break;
                    case bTypeQualifierFormat__Q:   printf_("__restrict");     break;
                    case bTypeQualifierFormat__Q__: printf_("__restrict__");   break;
                    default:                        jkc99_assert(false);           break;
                }
            } break;
        case kTypeQualifierVolatile:
            {
                switch(tq->format) {
                    case bTypeQualifierFormatQ:     printf_("volatile");       break;
                    case bTypeQualifierFormat__Q:   printf_("__volatile");     break;
                    case bTypeQualifierFormat__Q__: printf_("__volatile__");   break;
                    default:                        jkc99_assert(false);           break;
                }
            } break;
        case kTypeQualifier__ptr32:
            {
                printf_("__ptr32");
            } break;
        case kTypeQualifier__ptr64:
            {
                printf_("__ptr64");
            } break;
        case kTypeQualifier__unaligned:
            {
                printf_("__unaligned");
            } break;
        default: jkc99_assert(false); break;
    }
}

static inline void print_function_specifier(FunctionSpecifier fs) {
    print_directives(&fs);

    switch(fs.kind) {
        case kFunctionSpecifierInline:
            {
                switch(fs.format) {
                    case kFunctionSpecifierFormatF:         printf_("inline"); break;
                    case kFunctionSpecifierFormat__F:       printf_("__inline"); break;
                    case kFunctionSpecifierFormat__F__:     printf_("__inline__"); break;
                    case kFunctionSpecifierFormat__forceF:  printf_("__forceinline"); break;
                    default: jkc99_assert(false); printf_("inline"); break;
                }
            } break;
        default: jkc99_assert(false); break;
    }
}

static inline void print_calling_convention(CallingConvention cc) {
    switch(cc) {
        case kCallingConventionNone:                                         break;
        case kCallingConventionCdecl:           printf_("__cdecl ");         break;
        case kCallingConventionClrcall:         printf_("__clrcall ");       break;
        case kCallingConventionStdcall:         printf_("__stdcall ");       break;
        case kCallingConventionFastcall:        printf_("__fastcall ");      break;
        case kCallingConventionThiscall:        printf_("__thiscall ");      break;
        case kCallingConventionVectorcall:      printf_("__vectorcall ");    break;
        default:                                jkc99_assert(false);           break;
    }
}

static void print_declspec(Declspec *ds) {
    print_directives(ds);

    printf_("__declspec(");
    for(size_t i=0; i < ds->modifierCount; ++i) {
        DeclspecModifier *m = ds->modifiers + i;
        if(i) printf_(" ");
        switch(m->kind) {
            case kDeclspecModifier_align:
                {
                    printf_("align(%u)", m->u.align);
                } break;
            case kDeclspecModifier_allocate:
                {
                    printf_("allocate(\"%s\")", m->u.segname);
                } break;
            case kDeclspecModifier_allocator:
                {
                    printf_("allocator");
                } break;
            case kDeclspecModifier_appdomain:
                {
                    printf_("appdomain");
                } break;
            case kDeclspecModifier_code_seg:
                {
                    printf_("code_seg(\"%s\")", m->u.segname);
                } break;
            case kDeclspecModifier_deprecated:
                {
                    printf_("deprecated(\"%s\")", m->u.deprecated);
                } break;
            case kDeclspecModifier_dllimport:
                {
                    printf_("dllimport");
                } break;
            case kDeclspecModifier_dllexport:
                {
                    printf_("dllexport");
                } break;
            case kDeclspecModifier_jitintrisic:
                {
                    printf_("jitintrinsic");
                } break;
            case kDeclspecModifier_naked:
                {
                    printf_("naked");
                } break;
            case kDeclspecModifier_noalias:
                {
                    printf_("noalias");
                } break;
            case kDeclspecModifier_noinline:
                {
                    printf_("noinline");
                } break;
            case kDeclspecModifier_noreturn:
                {
                    printf_("noreturn");
                } break;
            case kDeclspecModifier_nothrow:
                {
                    printf_("nothrow");
                } break;
            case kDeclspecModifier_novtable:
                {
                    printf_("novtable");
                } break;
            case kDeclspecModifier_process:
                {
                    printf_("process");
                } break;
            case kDeclspecModifier_property:
                {
                    printf_("property(%s%s%s%s%s)",
                            m->u.property.get ? "get=" : "",
                            m->u.property.get,
                            m->u.property.get && m->u.property.put ? ", " : "",
                            m->u.property.put ? "put=" : "",
                            m->u.property.put
                            );
                } break;
            case kDeclspecModifier_restrict:
                {
                    printf_("restrict");
                } break;
            case kDeclspecModifier_safebuffers:
                {
                    printf_("safebuffers");
                } break;
            case kDeclspecModifier_selectany:
                {
                    printf_("selectany");
                } break;
            case kDeclspecModifier_spectre:
                {
                    printf_("spectre(nomitigation)");
                } break;
            case kDeclspecModifier_thread:
                {
                    printf_("thread");
                } break;
            case kDeclspecModifier_uuid:
                {
                    printf_("uuid(\"%s\")", m->u.uuid);
                } break;
            default: jkc99_assert(false);
        }
    }
    printf_(") ");
}

static void print_declaration_specifier(DeclarationSpecifier *spec) {
    switch(spec->kind) {
        case kDeclarationSpecifierStorageClassSpecifier:
            {
                print_storage_class(spec->u.storageClass);
            } break;
        case kDeclarationSpecifierTypeSpecifier:
            {
                print_type_specifier(&spec->u.typeSpecifier);
            } break;
        case kDeclarationSpecifierTypeQualifier:
            {
                print_type_qualifier(&spec->u.typeQualifier);
            } break;
        case kDeclarationSpecifierFunctionSpecifier:
            {
                print_function_specifier(spec->u.functionSpecifier);
            } break;
        case kDeclarationSpecifierAttribute:
            {
                print_attribute(&spec->u.attribute);
            } break;
        case kDeclarationSpecifierDeclspec:
            {
                print_declspec(&spec->u.declspec);
            } break;
        case kDeclarationSpecifierCallingConvention:
            {
                print_calling_convention(spec->u.callingConvention);
            } break;
        case kDeclarationSpecifierExtension:
            {
                printf_("__extension__ ");
            } break;
        default:
            {
                jkc99_assert(false);
            } break;
    }
}

static void print_type_qualifier_list(size_t count, TypeQualifier *qualifiers) {
    for(size_t i = 0; i < count; ++i) {
        print_type_qualifier(qualifiers + i);
        printf_(" ");
    }
}

static void print_direct_declarator(DirectDeclarator *d) {
    print_directives(d);

    switch(d->kind) {
        case kDirectDeclaratorIdentifier:
            {
                printf_("%s", d->u.identifier.str);
            } break;
        case kDirectDeclaratorDeclarator:
            {
                printf_("(");
                print_declarator(d->u.declarator);
                printf_(")");
            } break;
        case kDirectDeclaratorArray:
            {
                print_direct_declarator(d->u.array.direct);
                printf_("[");
                print_type_qualifier_list(d->u.array.qualifierCount, d->u.array.qualifiers);
                if(d->u.array.isVLA) {
                    printf_("*");
                } else {
                    if(d->u.array.isStatic) {
                        printf_("static ");
                    }
                    //TODO Evaluate size for []
                    if(d->u.array.expr) {
                        print_expr(d->u.array.expr);
                    }
                }
                printf_("]");
            } break;
        case kDirectDeclaratorFunc:
            {
                size_t i;
                print_calling_convention(d->u.func.callingConvention);
                print_direct_declarator(d->u.func.direct);
                printf_("(");
                print_indent_inc();
                if(d->u.func.hasParameterList) {
                    for(i = 0; i < d->u.func.count; ++i) {
                        if(i) printf_(", ");
                        print_parameter_declaration(d->u.func.u.parameters[i]);
                    }
                } else {
                    for(i = 0; i < d->u.func.count; ++i) {
                        if(i) printf_(", ");
                        printf_("%s", d->u.func.u.identifiers[i]);
                    }
                }
                if(d->u.func.hasEllipsis) {
                    printf_(", ...)");
                } else {
                    printf_(")");
                }
                print_indent_dec();
            } break;
        default:
            {
                jkc99_assert(false);
            } break;
    }
}

static void print_declarator(Declarator *d) {
    size_t i;

    print_directives(d);

    for(i = 0; i < d->beforeAttributeCount; ++i) {
        print_attribute(d->beforeAttributes + i);
        printf_(" ");
    }

    for(i = 0; i < d->pointerCount; ++i) {
        Pointer *ptr = d->pointers + i;
        if(ptr->qualifierCount) {
            printf_("* ");
            print_type_qualifier_list(ptr->qualifierCount, ptr->qualifiers);
        } else {
            printf_("*");
        }
    }

    if(d->direct) {
        print_direct_declarator(d->direct);
    }

    for(i = 0; i < d->afterAttributeCount; ++i) {
        printf_(" ");
        print_attribute(d->afterAttributes + i);
    }
}

static void print_direct_abstract_declarator(DirectAbstractDeclarator *d) {
    print_directives(d);

    switch(d->kind) {
        case kDirectAbstractDeclaratorDeclarator:
            {
                printf_("(");
                print_abstract_declarator(d->u.declarator);
                printf_(")");
            } break;
        case kDirectAbstractDeclaratorArray:
            {
                if(d->u.array.direct) {
                    print_direct_abstract_declarator(d->u.array.direct);
                }
                printf_("[");
                print_type_qualifier_list(d->u.array.qualifierCount, d->u.array.qualifiers);
                if(d->u.array.isVLA) {
                    printf_("*");
                } else {
                    if(d->u.array.isStatic) {
                        printf_("static ");
                    }
                    if(d->u.array.expr) {
                        print_expr(d->u.array.expr);
                    }
                }
                printf_("]");
            } break;
        case kDirectAbstractDeclaratorFunc:
            {
                size_t i;
                print_calling_convention(d->u.func.callingConvention);
                if(d->u.func.direct) {
                    print_direct_abstract_declarator(d->u.func.direct);
                }
                printf_("(");
                print_indent_inc();
                if(d->u.func.hasParameterList) {
                    for(i = 0; i < d->u.func.count; ++i) {
                        if(i) printf_(", ");
                        print_parameter_declaration(d->u.func.u.parameters[i]);
                    }
                } else {
                    for(i = 0; i < d->u.func.count; ++i) {
                        if(i) printf_(", ");
                        printf_("%s", d->u.func.u.identifiers[i]);
                    }
                }
                if(d->u.func.hasEllipsis) {
                    printf_(", ...)");
                } else {
                    printf_(")");
                }
                print_indent_dec();
            } break;
        default:
            {
                jkc99_assert(false);
            } break;
    }
}

static void print_abstract_declarator(AbstractDeclarator *d) {
    size_t i;

    print_directives(d);

    for(i = 0; i < d->beforeAttributeCount; ++i) {
        print_attribute(d->beforeAttributes + i);
        printf_(" ");
    }

    for(i = 0; i < d->pointerCount; ++i) {
        Pointer *ptr = d->pointers + i;
        if(ptr->qualifierCount) {
            printf_("* ");
            print_type_qualifier_list(ptr->qualifierCount, ptr->qualifiers);
        } else {
            printf_("*");
        }
    }

    if(d->direct) {
        print_direct_abstract_declarator(d->direct);
    }

    for(i = 0; i < d->afterAttributeCount; ++i) {
        printf_(" ");
        print_attribute(d->afterAttributes + i);
    }
}

static void print_type_name(TypeName *typeName) {
    size_t i;

    print_directives(typeName);

    for(i = 0; i < typeName->specifierCount; i++) {
        if(i) printf_(" ");
        print_declaration_specifier(typeName->specifiers + i);
    }
    if(typeName->abstractDeclarator) {
        print_abstract_declarator(typeName->abstractDeclarator);
    }
}

static void print_initializer(Initializer *init) {
    switch(init->kind) {
        case kInitializerExpr:
            {
                print_expr(init->u.expr);
            } break;
        case kInitializerList:
            {
                unsigned int line = gPrintSource.line;
                printf_("{");
                print_indent_inc();
                print_initializer_list(&init->u.list);
                print_indent_dec();
                if(gPrintSource.line != line) {
                    print_newline(NULL);
                    printf_("}");
                } else {
                    printf_(" }");
                }
            } break;
        default:    jkc99_assert(false);   break;
    }
}

static void print_initializer_list(InitializerList *list) {
    size_t i, j;

    print_directives(list);
    
    for(i = 0; i < list->count; ++i) {
        Designation *d = list->designations + i;
        if(i) printf_(",");
        if(d->count) {
            for(j = 0; j < d->count; ++j) {
                print_directives(d->list + j);
                switch(d->list[j].kind) {
                    case kDesignatorIndex:
                        {
                            printf_("[");
                            print_expr(d->list[j].u.indexExpr);
                            printf_("]");
                        } break;
                    case kDesignatorField:
                        {
                            printf_(".%s", d->list[j].u.field);
                        } break;
                    default:        jkc99_assert(false);   break;
                }
            }
            printf_(" = ");
        }
        print_initializer(list->initializers + i);
    }
}

static void print_expr(Expr *expr) {
    print_directives(expr);

    switch(expr->kind) {
        case kExprPrimary:
            {
                ExprPrimary *e = &expr->u.primary;
                switch(e->kind) {
                    case kExprPrimaryIdentifier:
                        {
                            printf_("%s", e->u.identifier);
                        } break;
                    case kExprPrimaryConstant:
                        {
                            Constant *c = &e->u.constant;
                            switch(c->kind) {
                                case kConstantInteger:
                                    {
                                        if(c->str) {
                                            printf_("%s", c->str);
                                        } else {
                                            switch(c->representation) {
                                                case kConstantDecimal:
                                                    printf_("%llu%.3s", c->u.intVal, c->suffix);
                                                    break;
                                                case kConstantHex:
                                                    printf_("0x%llx%.3s", c->u.intVal, c->suffix);
                                                    break;
                                                case kConstantOctal:
                                                    printf_("%s%llo%.3s", c->u.intVal ? "0" : "", c->u.intVal, c->suffix);
                                                    break;
                                                default: jkc99_assert(false); break;

                                            }
                                        }
                                    } break;
                                case kConstantFloat:
                                    {
                                        if(c->str) {
                                            printf_("%s", c->str);
                                        } else {
                                            /* TODO We need to actually ensure we don't introduce changes to values here. At the moment values are just printed at a reasonable precision but there's no guarantees. */
                                            if(c->suffix[0] == 'f' || c->suffix[0] == 'F') {
                                                switch(c->representation) {
                                                    case kConstantDecimal:
                                                    case kConstantDecimalWithExponent:
                                                        printf_("%.9gF", (float)c->u.floatVal);
                                                        break;
                                                    case kConstantHex:
                                                        printf_("%aF", (float)c->u.floatVal);
                                                        break;
                                                    default: jkc99_assert(false); break;
                                                }
                                            } else if(c->suffix[0] == 'l' || c->suffix[0] == 'L') {
                                                switch(c->representation) {
                                                    case kConstantDecimal:
                                                    case kConstantDecimalWithExponent:
                                                        printf_("%.21lg", c->u.floatVal);
                                                        break;
                                                    case kConstantHex:
                                                        printf_("%la", c->u.floatVal);
                                                        break;
                                                    default: jkc99_assert(false); break;
                                                }
                                            } else {
                                                switch(c->representation) {
                                                    case kConstantDecimal:
                                                    case kConstantDecimalWithExponent:
                                                        printf_("%.17g", c->u.floatVal);
                                                        break;
                                                    case kConstantHex:
                                                        printf_("%a", c->u.floatVal);
                                                        break;
                                                    default: jkc99_assert(false); break;
                                                }
                                            }
                                        }
                                    } break;
                                case kConstantEnumeration:
                                    {
                                        jkc99_assert(false); //TODO
                                    } break;
                                case kConstantCharacter:
                                    {
                                        switch(c->u.charVal) {
                                            case '\\':  printf_("'\\\\'");    break;
                                            case '\'':  printf_("'\\\''");    break;
                                            case '\t':  printf_("'\\t'");     break;
                                            case '\f':  printf_("'\\f'");     break;
                                            case '\n':  printf_("'\\n'");     break;
                                            case '\r':  printf_("'\\r'");     break;
                                            case '\v':  printf_("'\\v'");     break;
                                            case '\b':  printf_("'\\b'");     break;
                                            case '\0':  printf_("'\\0'");     break;
                                            default:    printf_("'%c'", c->u.charVal);    break;
                                        }
                                    } break;
                            }
                        } break;
                    case kExprPrimaryStringLiteral:
                        {
                            /* TODO Support UTF-8 */
                            const char *p = e->u.str;
                            printf_("\"");
                            while(*p) {
                                switch(*p) {
                                    case '\\' : printf_("\\\\");    break;
                                    case '\'' : printf_("\\\'");    break;
                                    case '"':   printf_("\\\"");    break;
                                    case '\t':  printf_("\\t");     break;
                                    case '\f':  printf_("\\f");     break;
                                    case '\n':  printf_("\\n");     break;
                                    case '\r':  printf_("\\r");     break;
                                    case '\v':  printf_("\\v");     break;
                                    case '\b':  printf_("\\b");     break;
                                    case '\0':  printf_("\\0");     break;
                                    default:    printf_("%c", *p);  break;
                                }
                                p++;
                            }
                            printf_("\"");
                        } break;
                    case kExprPrimaryExpr:
                        {
                            printf_("(");
                            print_expr(e->u.expr);
                            printf_(")");
                        } break;
                    default: jkc99_assert(false); break;
                }
            } break;
        case kExprPostfix:
            {
                ExprPostfix *e = &expr->u.postfix;
                switch(e->kind) {
                    case kExprPostfixIndex:
                        {
                            print_expr(e->expr);
                            printf_("[");
                            print_expr(e->u.indexExpr);
                            printf_("]");
                        } break;
                    case kExprPostfixCall:
                        {
                            size_t i;
                            print_expr(e->expr);
                            printf_("(");
                            print_indent_inc();
                            for(i = 0; i < da_count(e->u.callArgs); ++i) {
                                if(i) printf_(", ");
                                print_expr(e->u.callArgs[i]);
                            }
                            print_indent_dec();
                            printf_(")");
                        } break;
                    case kExprPostfixDot:
                        {
                            print_expr(e->expr);
                            printf_(".%s", e->u.dot);
                        } break;
                    case kExprPostfixArrow:
                        {
                            print_expr(e->expr);
                            printf_("->%s", e->u.arrow);
                        } break;
                    case kExprPostfixIncrement:
                        {
                            print_expr(e->expr);
                            printf_("++");
                        } break;
                    case kExprPostfixDecrement:
                        {
                            print_expr(e->expr);
                            printf_("--");
                        } break;
                    case kExprPostfixCompound:
                        {
                            unsigned int line = gPrintSource.line;
                            printf_("(");
                            print_type_name(e->u.compound.typeName);
                            printf_("){");
                            print_indent_inc();
                            print_initializer_list(&e->u.compound.initializerList);
                            print_indent_dec();
                            if(gPrintSource.line != line) {
                                print_newline(NULL);
                                printf_("}");
                            } else {
                                printf_(" }");
                            }
                        } break;
#ifdef __clang__
                    case kExprPostfixVaArg:
                        {
                            print_expr(e->expr);
                            printf_("(");
                            print_expr(e->u.va_arg.list);
                            printf_(", ");
                            print_type_name(e->u.va_arg.typeName);
                            printf_(")");
                        } break;
#endif
                    default: jkc99_assert(false); break;
                }
            } break;
        case kExprUnary:
            {
                ExprUnary *e = &expr->u.unary;
                switch(e->kind) {
                    case kExprUnaryIncrement:   printf_("++"); print_expr(e->u.expr); break;
                    case kExprUnaryDecrement:   printf_("--"); print_expr(e->u.expr); break;
                    case kExprUnaryAddressOf:   printf_("&");  print_expr(e->u.expr); break;
                    case kExprUnaryDeref:       printf_("*");  print_expr(e->u.expr); break;
                    case kExprUnaryPlus:        printf_("+");  print_expr(e->u.expr); break;
                    case kExprUnaryMinus:       printf_("-");  print_expr(e->u.expr); break;
                    case kExprUnaryBitwiseNeg:  printf_("~");  print_expr(e->u.expr); break;
                    case kExprUnaryLogicalNot:  printf_("!");  print_expr(e->u.expr); break;
                    case kExprUnarySizeofExpr:
                        {
                            printf_("sizeof(");
                            print_expr(e->u.expr);
                            printf_(")");
                        } break;
                    case kExprUnarySizeofType:
                        {
                            printf_("sizeof(");
                            print_type_name(e->u.typeName);
                            printf_(")");
                        } break;
                    case kExprUnaryOffsetof:
                        {
                            printf_("__builtin_offsetof(");
                            print_type_name(e->u.offsetof.typeName);
                            printf_(", ");
                            printf_("%s)", e->u.offsetof.field);
                        } break;
                    default: jkc99_assert(false); break;
                }
            } break;
        case kExprCast:
            {
                ExprCast *e = &expr->u.cast;
                printf_("(");
                print_type_name(e->typeName);
                printf_(")");
                print_expr(e->expr);
            } break;
        case kExprMultiplicative:
            {
                ExprMultiplicative *e = &expr->u.multiplicative;
                print_expr(e->left);
                switch(e->kind) {
                    case kExprMultiplicativeMul:    printf_(" * ");    break;
                    case kExprMultiplicativeDiv:    printf_(" / ");    break;
                    case kExprMultiplicativeMod:    printf_(" %% ");    break;
                    default: jkc99_assert(false); break;
                }
                print_expr(e->right);
            } break;
        case kExprAdditive:
            {
                ExprAdditive *e = &expr->u.additive;
                print_expr(e->left);
                switch(e->kind) {
                    case kExprAdditiveAdd:          printf_(" + ");    break;
                    case kExprAdditiveSub:          printf_(" - ");    break;
                    default: jkc99_assert(false); break;
                }
                print_expr(e->right);
            } break;
        case kExprShift:
            {
                ExprShift *e = &expr->u.shift;
                print_expr(e->left);
                switch(e->kind) {
                    case kExprShiftLeft:            printf_(" << ");   break;
                    case kExprShiftRight:           printf_(" >> ");   break;
                    default: jkc99_assert(false); break;
                }
                print_expr(e->right);
            } break;
        case kExprRelational:
            {
                ExprRelational *e = &expr->u.relational;
                print_expr(e->left);
                switch(e->kind) {
                    case kExprRelationalLT:         printf_(" < ");    break;
                    case kExprRelationalGT:         printf_(" > ");    break;
                    case kExprRelationalLTE:        printf_(" <= ");   break;
                    case kExprRelationalGTE:        printf_(" >= ");   break;
                    default: jkc99_assert(false); break;
                }
                print_expr(e->right);
            } break;
        case kExprEquality:
            {
                ExprEquality *e = &expr->u.equality;
                print_expr(e->left);
                switch(e->kind) {
                    case kExprEqualityEQ:           printf_(" == ");   break;
                    case kExprEqualityNE:           printf_(" != ");   break;
                    default: jkc99_assert(false); break;
                }
                print_expr(e->right);
            } break;
        case kExprAnd:
            {
                ExprAnd *e = &expr->u.and;
                print_expr(e->left);
                printf_(" & ");
                print_expr(e->right);
            } break;
        case kExprExclusiveOr:
            {
                ExprExclusiveOr *e = &expr->u.exclusiveOr;
                print_expr(e->left);
                printf_(" ^ ");
                print_expr(e->right);
            } break;
        case kExprInclusiveOr:
            {
                ExprInclusiveOr *e = &expr->u.inclusiveOr;
                print_expr(e->left);
                printf_(" | ");
                print_expr(e->right);
            } break;
        case kExprLogicalAnd:
            {
                ExprLogicalAnd *e = &expr->u.logicalAnd;
                print_expr(e->left);
                printf_(" && ");
                print_expr(e->right);
            } break;
        case kExprLogicalOr:
            {
                ExprLogicalOr *e = &expr->u.logicalOr;
                print_expr(e->left);
                printf_(" || ");
                print_expr(e->right);
            } break;
        case kExprConditional:
            {
                ExprConditional *e = &expr->u.conditional;
                print_expr(e->cond);
                printf_(" ? ");
                print_expr(e->left);
                printf_(" : ");
                print_expr(e->right);
            } break;
        case kExprAssignment:
            {
                ExprAssignment *e = &expr->u.assignment;
                print_expr(e->left);
                switch(e->kind) {
                    case kExprAssignmentBasic:          printf_(" = ");      break;
                    case kExprAssignmentMul:            printf_(" *= ");     break;
                    case kExprAssignmentDiv:            printf_(" /= ");     break;
                    case kExprAssignmentMod:            printf_(" %%= ");    break;
                    case kExprAssignmentAdd:            printf_(" += ");     break;
                    case kExprAssignmentSub:            printf_(" -= ");     break;
                    case kExprAssignmentShiftLeft:      printf_(" <<= ");    break;
                    case kExprAssignmentShiftRight:     printf_(" >>= ");    break;
                    case kExprAssignmentAnd:            printf_(" &= ");     break;
                    case kExprAssignmentXor:            printf_(" ^= ");     break;
                    case kExprAssignmentOr:             printf_(" |= ");     break;
                    default: jkc99_assert(false); break;
                }
                print_expr(e->right);
            } break;
        case kExprComma:
            {
                ExprComma *e = &expr->u.comma;
                print_expr(e->left);
                printf_(", ");
                print_expr(e->right);
            } break;
        default: jkc99_assert(false); break;
    }
}

static void print_stmt(Stmt *stmt) {
    print_directives(stmt);

    switch(stmt->kind) {
        case kStmtLabeled:
            {
                switch(stmt->u.l.kind) {
                    case kStmtLabeledLabel:
                        {
                            printf_("%s: ", stmt->u.l.u.label.label);
                            for(size_t i = 0; i < stmt->u.l.u.label.attributeCount; ++i) {
                                print_attribute(stmt->u.l.u.label.attributes + i);
                                printf_(" ");
                            }
                            print_stmt(stmt->u.l.stmt);
                        } break;
                    case kStmtLabeledCase:
                        {
                            printf_("case ");
                            print_expr(stmt->u.l.u.caseExpr);
                            printf_(": ");
                            print_indent_inc();
                            print_stmt(stmt->u.l.stmt);
                            print_indent_dec();
                        } break;
                    case kStmtLabeledDefault:
                        {
                            printf_("default: ");
                            print_indent_inc();
                            print_stmt(stmt->u.l.stmt);
                            print_indent_dec();
                        } break;
                    default: 
                        {
                            jkc99_assert(false);
                        } break;
                }
            } break;
        case kStmtCompound:
            {
                unsigned int line = gPrintSource.line;
                printf_("{");
                print_indent_inc();
                for(size_t i = 0; i < stmt->u.c.count; ++i) {
                    BlockItem *item = stmt->u.c.items + i;
                    if(item->decl) {
                        jkc99_assert(!item->stmt);
                        print_newline(item->decl);
                        print_declaration(item->decl);
                    } else {
                        jkc99_assert(item->stmt);
                        print_newline(item->stmt);
                        print_stmt(item->stmt);
                        if(item->stmt->kind == kStmtAsm) {
                            printf_(";");
                        }
                    }
                }
                print_indent_dec();
                if(gPrintSource.line != line) {
                    print_newline(NULL);
                    printf_("}");
                } else {
                    printf_(" }");
                }
            } break;
        case kStmtExpression:
            {
                if(stmt->u.e.expr) {
                    print_expr(stmt->u.e.expr);
                } else {
                    for(size_t i = 0; i < stmt->u.e.attributeCount; ++i) {
                        print_attribute(stmt->u.e.attributes + i);
                        printf_(" ");
                    }
                }
                printf_(";");
            } break;
        case kStmtSelection:
            {
                switch(stmt->u.s.kind) {
                    case kStmtSelectionIf:
                        {
                            printf_("if(");
                            print_indent_inc();
                            print_expr(stmt->u.s.expr);
                            print_indent_dec();
                            printf_(") ");
                            print_stmt(stmt->u.s.stmt);
                            if(stmt->u.s.elseStmt) {
                                printf_(" else ");
                                print_stmt(stmt->u.s.elseStmt);
                            }
                        } break;
                    case kStmtSelectionSwitch:
                        {
                            printf_("switch(");
                            print_expr(stmt->u.s.expr);
                            printf_(") ");
                            print_stmt(stmt->u.s.stmt);
                        } break;
                    default: 
                        {
                            jkc99_assert(false);
                        } break;
                }
            } break;
        case kStmtIteration:
            {
                switch(stmt->u.i.kind) {
                    case kStmtIterationWhile:
                        {
                            printf_("while(");
                            print_indent_inc();
                            print_expr(stmt->u.i.condExpr);
                            print_indent_dec();
                            printf_(") ");
                            print_stmt(stmt->u.i.stmt);
                        } break;
                    case kStmtIterationDoWhile:
                        {
                            printf_("do ");
                            print_stmt(stmt->u.i.stmt);
                            printf_(" while (");
                            print_indent_inc();
                            print_expr(stmt->u.i.condExpr);
                            print_indent_dec();
                            printf_(");");
                        } break;
                    case kStmtIterationFor:
                        {
                            printf_("for(");
                            print_indent_inc();
                            if(stmt->u.i.initDecl) {
                                print_declaration(stmt->u.i.initDecl);
                                printf_(" ");
                            } else {
                                if(stmt->u.i.initExpr) {
                                    print_expr(stmt->u.i.initExpr);
                                }
                                printf_("; ");
                            }
                            if(stmt->u.i.condExpr) {
                                print_expr(stmt->u.i.condExpr);
                            }
                            printf_("; ");
                            if(stmt->u.i.iterExpr) {
                                print_expr(stmt->u.i.iterExpr);
                            }
                            print_indent_dec();
                            printf_(") ");
                            print_stmt(stmt->u.i.stmt);
                        } break;
                    default: 
                        {
                            jkc99_assert(false);
                        } break;
                }
            } break;
        case kStmtJump:
            {
                switch(stmt->u.j.kind) {
                    case kStmtJumpGoto:
                        {
                            printf_("goto %s;", stmt->u.j.u.identifier);
                        } break;
                    case kStmtJumpContinue:
                        {
                            printf_("continue;");
                        } break;
                    case kStmtJumpBreak:
                        {
                            printf_("break;");
                        } break;
                    case kStmtJumpReturn:
                        {
                            if(stmt->u.j.u.returnExpr) {
                                printf_("return ");
                                print_expr(stmt->u.j.u.returnExpr);
                                printf_(";");
                            } else {
                                printf_("return;");
                            }
                        } break;
                    default:
                        {
                            jkc99_assert(false);
                        } break;
                }
            } break;
#if HAS_GCC_ASM
        case kStmtAsm:
            {
                StmtAsm *a = &stmt->u.a;
                switch(a->kind) {
                    case kStmtAsmBasic:
                        {
                            printf_("__asm__ ");
                            if(a->qualifiers & kStmtAsmQualifierVolatile) {
                                printf_("__volatile__ ");
                            }
                            if(a->qualifiers & kStmtAsmQualifierInline) {
                                printf_("inline ");
                            }
                            printf_("(\"%s\")", a->instructions);
                        } break;
                    case kStmtAsmExtended:
                        {
                            printf_("__asm__ ");
                            if(a->qualifiers & kStmtAsmQualifierVolatile) {
                                printf_("__volatile__ ");
                            }
                            if(a->qualifiers & kStmtAsmQualifierInline) {
                                printf_("inline ");
                            }
                            if(a->qualifiers & kStmtAsmQualifierGoto) {
                                printf_("goto ");
                            }
                            printf_("(\"%s\"", a->instructions);
                            print_newline(NULL);
                            printf_(": ");
                            for(size_t i = 0; i < a->outputOperandCount; ++i) {
                                StmtAsmOutputOperand *out = a->outputOperands + i;
                                if(i) {
                                    printf_(", ");
                                }
                                if(out->symbolicName) {
                                    printf_("[%s] ", out->symbolicName);
                                }
                                printf_("\"%s\" (", out->constraints);
                                print_expr(out->expr);
                                printf_(")");
                            }
                            print_newline(NULL);
                            printf_(": ");
                            for(size_t i = 0; i < a->inputOperandCount; ++i) {
                                StmtAsmInputOperand *in = a->inputOperands + i;
                                if(i) {
                                    printf_(", ");
                                }
                                if(in->symbolicName) {
                                    printf_("[%s] ", in->symbolicName);
                                }
                                printf_("\"%s\" (", in->constraints);
                                print_expr(in->expr);
                                printf_(")");
                            }
                            print_newline(NULL);
                            printf_(": ");
                            for(size_t i = 0; i < a->clobberCount; ++i) {
                                if(i) {
                                    printf_(", ");
                                }
                                printf_("\"%s\"", a->clobbers[i]);
                            }
                            if(a->qualifiers & kStmtAsmQualifierGoto) {
                                print_newline(NULL);
                                printf_(": ");
                                for(size_t i = 0; i < a->gotoLabelCount; ++i) {
                                    if(i) {
                                        printf_(", ");
                                    }
                                    printf_("\"%s\"", a->gotoLabels[i]);
                                }
                            }
                            printf_(")");
                        } break;
                    default:
                        {
                            jkc99_assert(false);
                        } break;
                }
            } break;
#endif
        default:
            {
                jkc99_assert(false);
            } break;
    }
}

#if 1
UNUSED static void print_external_declaration(ExternalDeclaration *ed) {
    if(ed->decl) {
        jkc99_assert(!ed->func);
        print_declaration(ed->decl);
    } else {
        jkc99_assert(ed->func);
        print_function_definition(ed->func);
    }
    print_newline(NULL);
}
#else
UNUSED static void print_translation_unit_item(TranslationUnitItem *item) {
    switch(item->kind) {
        case kTranslationUnitItemPreprocessorDirective:
            {
                jkc99_assert(false); /* TODO */
            } break;
        case kTranslationUnitItemDeclaration: 
            {
                jkc99_assert(item->u.decl);
                print_declaration(item->u.decl);
                printf_("\n");
            } break;
        case kTranslationUnitItemFunctionDefinition:
            {
                jkc99_assert(item->u.func);
                print_function_definition(item->u.func);
                printf_("\n");
            } break;
        default:
            jkc99_assert(false);
            break;
    }
}
#endif
