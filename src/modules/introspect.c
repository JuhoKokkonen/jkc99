#include "../jkc99.h"
#include "print.h"

#ifndef JKC99_INTROSPECT_TYPE
#define JKC99_INTROSPECT_TYPE(name) "Introspect"#name
#endif

#ifndef JKC99_INTROSPECT_KIND
#define JKC99_INTROSPECT_KIND(name) "kIntrospect"#name
#endif

#ifndef JKC99_INTROSPECT_FLAG
#define JKC99_INTROSPECT_FLAG(name) "bIntrospect"#name
#endif

#ifndef JKC99_INTROSPECT_GLOBAL
#define JKC99_INTROSPECT_GLOBAL(name) "gIntrospect"#name
#endif

#ifndef JKC99_INTROSPECT_MACRO
#define JKC99_INTROSPECT_MACRO(name) "INTROSPECT_"#name
#endif

#ifdef HAS___ATTRIBUTE__
#define JKC99_INTROSPECT_UNUSED "__attribute__((unused)) "
#else
#define JKC99_INTROSPECT_UNUSED ""
#endif

#ifdef __clang__
#define JKC99_INTROSPECT_OFFSETOF "__builtin_offsetof"
#else
#error "offsetof not defined"
#endif

static const char *gIntrospectHeader =
    "typedef unsigned long long introspect_size_t;\n\n"
    //"#define "JKC99_INTROSPECT_MACRO(OFFSET_OF)"(type,name) __builtin_offsetof(type, name)\n\n"
    //"#define "JKC99_INTROSPECT_MACRO(OFFSET_OF)"(type,name) ((size_t)&((type){0}).name)\n\n"
    //"#define "JKC99_INTROSPECT_MACRO(UNUSED)" __attribute((unused))\n\n"
    "typedef enum "JKC99_INTROSPECT_TYPE(TypeQualifier)" {\n"
    "    "JKC99_INTROSPECT_FLAG(TypeQualifierConst)"     = 1 << 0,\n"
    "    "JKC99_INTROSPECT_FLAG(TypeQualifierRestrict)"  = 1 << 1,\n"
    "    "JKC99_INTROSPECT_FLAG(TypeQualifierVolatile)"  = 1 << 2\n"
    "} "JKC99_INTROSPECT_TYPE(TypeQualifier)";\n\n"
    "typedef enum "JKC99_INTROSPECT_TYPE(TypeKind)" {\n"
    "    "JKC99_INTROSPECT_KIND(TypeUnknown)",\n"
    "    "JKC99_INTROSPECT_KIND(TypeBuiltin)",\n"
    "    "JKC99_INTROSPECT_KIND(TypeVoid)",\n"
    "    "JKC99_INTROSPECT_KIND(TypeChar)",\n"
    "    "JKC99_INTROSPECT_KIND(TypeSignedChar)",\n"
    "    "JKC99_INTROSPECT_KIND(TypeSignedShort)",\n"
    "    "JKC99_INTROSPECT_KIND(TypeSignedInt)",\n"
    "    "JKC99_INTROSPECT_KIND(TypeSignedLong)",\n"
    "    "JKC99_INTROSPECT_KIND(TypeSignedLongLong)",\n"
    "    "JKC99_INTROSPECT_KIND(Type_Bool)",\n"
    "    "JKC99_INTROSPECT_KIND(TypeUnsignedChar)",\n"
    "    "JKC99_INTROSPECT_KIND(TypeUnsignedShort)",\n"
    "    "JKC99_INTROSPECT_KIND(TypeUnsignedInt)",\n"
    "    "JKC99_INTROSPECT_KIND(TypeUnsignedLong)",\n"
    "    "JKC99_INTROSPECT_KIND(TypeUnsignedLongLong)",\n"
    "    "JKC99_INTROSPECT_KIND(TypeFloat)",\n"
    "    "JKC99_INTROSPECT_KIND(TypeDouble)",\n"
    "    "JKC99_INTROSPECT_KIND(TypeLongDouble)",\n"
    "    "JKC99_INTROSPECT_KIND(TypeEnum)",\n"
    "    "JKC99_INTROSPECT_KIND(TypeStruct)",\n"
    "    "JKC99_INTROSPECT_KIND(TypeUnion)",\n"
    "    "JKC99_INTROSPECT_KIND(TypeQualified)",\n"
    "    "JKC99_INTROSPECT_KIND(TypeArray)",\n"
    "    "JKC99_INTROSPECT_KIND(TypePointer)",\n"
    "    "JKC99_INTROSPECT_KIND(TypeBitfield)",\n"
    "    "JKC99_INTROSPECT_KIND(TypeFunction)",\n"
    "    "JKC99_INTROSPECT_KIND(TypeVector)"\n"
    "} "JKC99_INTROSPECT_TYPE(TypeKind)";\n\n"
    "typedef introspect_size_t "JKC99_INTROSPECT_TYPE(TypeHandle)";\n\n"
    "typedef struct "JKC99_INTROSPECT_TYPE(TypeInfo)" "JKC99_INTROSPECT_TYPE(TypeInfo)";\n\n"
/*
    "typedef struct "JKC99_INTROSPECT_TYPE(Enumerator)" {\n"
    "    const char             *name;\n"
    "    int                    value;\n"
    "} "JKC99_INTROSPECT_TYPE(Enumerator)";\n\n"
*/
    "typedef struct "JKC99_INTROSPECT_TYPE(Member)" {\n"
    "    const char             *name;\n"
    "    introspect_size_t      offset;\n"
    "    "JKC99_INTROSPECT_TYPE(TypeHandle)"   type;\n"
    "} "JKC99_INTROSPECT_TYPE(Member)";\n\n"
    "struct "JKC99_INTROSPECT_TYPE(TypeInfo)" {\n"
    "    "JKC99_INTROSPECT_TYPE(TypeKind)" kind;\n"
    "    const char             *name;\n"
    "    introspect_size_t      size;\n"
    "    introspect_size_t      userCount;\n"
    "    introspect_size_t      *users;\n"
    "    union {\n"
    "        struct {\n"
    "            "JKC99_INTROSPECT_TYPE(TypeHandle)"   base;\n"
    "            int                    qualifiers;\n"
    "        } q;\n"
    "        struct {\n"
    "            "JKC99_INTROSPECT_TYPE(TypeHandle)"   base;\n"
    "            introspect_size_t      count;\n"
    "        } a;\n"
    "        struct {\n"
    "            "JKC99_INTROSPECT_TYPE(TypeHandle)"   base;\n"
    "        } p;\n"
    "        struct {\n"
    "            introspect_size_t      enumeratorCount;\n"
    "            const char             **names;\n"
    "            int                    *values;\n"
    //"            "JKC99_INTROSPECT_TYPE(Enumerator)"   *enumerators;\n"
    "        } e;\n"
    "        struct {\n"
    "            introspect_size_t      memberCount;\n"
    "            "JKC99_INTROSPECT_TYPE(Member)"   *members;\n"
    "        } su;\n"
    "        struct {\n"
    "            "JKC99_INTROSPECT_TYPE(TypeHandle)"   returnType;\n"
    "            _Bool                  hasVarargs;\n"
    "            introspect_size_t      parameterCount;\n"
    "            "JKC99_INTROSPECT_TYPE(TypeHandle)"   *parameters;\n"
    "        } f;\n"
    "        struct {\n"
    "            "JKC99_INTROSPECT_TYPE(TypeHandle)"   base;\n"
    "            int                    bytes;\n"
    "        } v;\n"
    "    } u;\n"
    "};\n\n"
    "typedef enum "JKC99_INTROSPECT_TYPE(SymbolKind)" {\n"
    "    "JKC99_INTROSPECT_KIND(SymbolType)",\n"
    "    "JKC99_INTROSPECT_KIND(SymbolGlobal)",\n"
    "    "JKC99_INTROSPECT_KIND(SymbolFunction)",\n"
    "    "JKC99_INTROSPECT_KIND(SymbolEnumerator)"\n"
    "} "JKC99_INTROSPECT_TYPE(SymbolKind)";\n\n"
    "typedef struct "JKC99_INTROSPECT_TYPE(Symbol)" {\n"
    "    "JKC99_INTROSPECT_TYPE(SymbolKind)"          kind;\n"
    "    const char                    *name;\n"
    "    "JKC99_INTROSPECT_TYPE(TypeHandle)"          type;\n"
    "    union {\n"
    "        void                      *global;\n"
    "        struct {\n"
    "            void                  (*ptr)(void);\n"
    "            const char            **parameterNames;\n"
    "        } func;\n"
    "        int                       value;\n"
    "    } u;\n"
    "} "JKC99_INTROSPECT_TYPE(Symbol)";\n\n"
    JKC99_INTROSPECT_UNUSED"extern "JKC99_INTROSPECT_TYPE(TypeInfo)" *"JKC99_INTROSPECT_GLOBAL(TypeInfo)";\n"
    JKC99_INTROSPECT_UNUSED"extern introspect_size_t "JKC99_INTROSPECT_GLOBAL(TypeInfoCount)";\n"
    JKC99_INTROSPECT_UNUSED"extern "JKC99_INTROSPECT_TYPE(Symbol)" *"JKC99_INTROSPECT_GLOBAL(Symbols)";\n"
    JKC99_INTROSPECT_UNUSED"extern introspect_size_t "JKC99_INTROSPECT_GLOBAL(SymbolCount)";\n\n\n";

typedef struct IntrospectSymbolExclusion {
    const char *identifier;
} IntrospectSymbolExclusion;

static struct {
    const char                  *str_type_info;
    const char                  *str_gIntrospectTypeInfo;
    size_t                      symbolExclusionCount;
    IntrospectSymbolExclusion   *symbolExclusions;
} gIntrospectModule;

static const char *introspect_check_symbol_exclusion(const char *str) {
    /* TODO Optimise */
    if(str) {
        for(size_t i = 0; i < gIntrospectModule.symbolExclusionCount; ++i) {
            if(strcmp(str, gIntrospectModule.symbolExclusions[i].identifier) == 0) {
                str = NULL;
                break;
            }
        }
    }

    return str;
}

#define generate_type_declaration_specifiers(ctx, handle) generate_type_declaration_specifiers_((ctx), (handle), "", true)
static char *generate_type_declaration_specifiers_(ParseContext *ctx, TypeHandle handle, const char *base, bool goingRight);

static void generate_struct_or_union_specifier(ParseContext *ctx, StructOrUnionSpecifier *su) {
    size_t i;

    for(i = 0; i < su->attributeCount; ++i) {
        printf_(" ");
        print_attribute(su->attributes + i);
    }

    for(i = 0; i < su->declspecCount; ++i) {
        printf_(" ");
        print_declspec(su->declspecs + i);
    }

    printf_(" {");
    print_indent_inc();
    for(i = 0; i < su->memberDeclarationCount; ++i) {
        StructDeclaration *decl = su->memberDeclarations[i];

        for(size_t j = 0; j < decl->declaratorCount; ++j) {
            StructDeclarator *sd = decl->declarators + j;

            print_newline(NULL);
            print_directives(decl);

            if(decl->hasExtension) {
                printf_("__extension__ ");
            }

#if 0
            for(j = 0; j < decl->attributeCount; ++j) {
                if(decl->attributes[j].position == kAttributePositionBefore) {
                    print_attribute(decl->attributes + j);
                    printf_(" ");
                }
            }
#endif

#if 0
            for(j = 0; j < decl->specifierCount; ++j) {
                print_declaration_specifier(decl->specifiers + j);
                printf_(" ");
            }
#endif



            if(sd->declarator) {
                const char *name;
                TypeHandle type = jkc99_declarator_get_type_and_name(ctx, sd->declarator, decl->type, &name, NULL);
                jkc99_assert(JKC99_TYPE_HANDLE_NOT_INVALID(type));
                jkc99_assert(name);
                char *str = generate_type_declaration_specifiers_(ctx, type, name, true);
                printf_("%s", str);
            } else {
                char *str = generate_type_declaration_specifiers(ctx, decl->type);
                jkc99_assert(str);
                printf_("%s", str);
            }

            if(sd->bitCountExpr) {
                printf_(" : ");
                print_expr(sd->bitCountExpr);
            }

#if 0
            for(j = 0; j < decl->attributeCount; ++j) {
                if(decl->attributes[j].position == kAttributePositionAfter) {
                    printf_(" ");
                    print_attribute(decl->attributes + j);
                }
            }
#endif

            printf_(";");
        }
    }
    print_indent_dec();
    printf_("}");
}

/* TODO We could probably stand to be a bit more efficient here, at the moment we 
 * use da_printf for everything and don't really free or reuse anything */
static char *generate_type_declaration_specifiers_(ParseContext *ctx, TypeHandle handle, const char *base, bool goingRight) {
    char **oldBuffer;
    char *buf = NULL;
    JKC99Type t = {0};
    jkc99_type_get(ctx, handle, &t);
    const char *space = base[0] ? " " : "";

    gPrintMinify = true;

    switch(t.kind) {
        case kTypeUnknown:
            {
                da_printf(buf, "<Unknown type>%s%s", space, base);
            } break;
        case kTypeBuiltin:
            {
                da_printf(buf, "%s%s%s", t.u.builtin.name, space, base);
            } break;
        case kTypeVoid:
            {
                da_printf(buf, "void%s%s", space, base);
            } break;
        case kTypeChar:
            {
                da_printf(buf, "char%s%s", space, base);
            } break;
        case kTypeSignedChar:
            {
                da_printf(buf, "signed char%s%s", space, base);
            } break;
        case kTypeSignedShort:
            {
                da_printf(buf, "short%s%s", space, base);
            } break;
        case kTypeSignedInt:
            {
                da_printf(buf, "int%s%s", space, base);
            } break;
        case kTypeSignedLong:
            {
                da_printf(buf, "long%s%s", space, base);
            } break;
        case kTypeSignedLongLong:
            {
                da_printf(buf, "long long%s%s", space, base);
            } break;
        case kType_Bool:
            {
                da_printf(buf, "_Bool%s%s", space, base);
            } break;
        case kTypeUnsignedChar:
            {
                da_printf(buf, "unsigned char%s%s", space, base);
            } break;
        case kTypeUnsignedShort:
            {
                da_printf(buf, "unsigned short%s%s", space, base);
            } break;
        case kTypeUnsignedInt:
            {
                da_printf(buf, "unsigned int%s%s", space, base);
            } break;
        case kTypeUnsignedLong:
            {
                da_printf(buf, "unsigned long%s%s", space, base);
            } break;
        case kTypeUnsignedLongLong:
            {
                da_printf(buf, "unsigned long long%s%s", space, base);
            } break;
        case kTypeFloat:
            {
                da_printf(buf, "float%s%s", space, base);
            } break;
        case kTypeDouble:
            {
                da_printf(buf, "double%s%s", space, base);
            } break;
        case kTypeLongDouble:
            {
                da_printf(buf, "long double%s%s", space, base);
            } break;

        case kTypeFloat_Complex:
            {
                da_printf(buf, "float _Complex%s%s", space, base);
            } break;

        case kTypeDouble_Complex:
            {
                da_printf(buf, "double _Complex%s%s", space, base);
            } break;

        case kTypeLongDouble_Complex:
            {
                da_printf(buf, "long double _Complex%s%s", space, base);
            } break;

        case kTypeQualified:
            {
                jkc99_assert(t.u.q.qualifiers);
                if(t.u.q.qualifiers & kTypeQualifierConst) {
                    da_printf(buf, "const%s", space);
                    space = " ";
                }
                if(t.u.q.qualifiers & kTypeQualifierRestrict) {
                    da_printf(buf, "restrict%s", space);
                    space = " ";
                }
                if(t.u.q.qualifiers & kTypeQualifierVolatile) {
                    da_printf(buf, "volatile%s", space);
                    space = " ";
                }
                if(t.u.q.qualifiers & kTypeQualifier__ptr32) {
                    da_printf(buf, "__ptr32%s", space);
                    space = " ";
                }
                if(t.u.q.qualifiers & kTypeQualifier__ptr64) {
                    da_printf(buf, "__ptr64%s", space);
                    space = " ";
                }
                if(t.u.q.qualifiers & kTypeQualifier__unaligned) {
                    da_printf(buf, "__unaligned%s", space);
                    space = " ";
                }
                da_printf(buf, "%s", base);
                buf = generate_type_declaration_specifiers_(ctx, t.u.q.base, buf, false);
            } break;

        case kTypeArray:
            {
                const char *pl = "";
                const char *pr = "";
                if(base[0] && !goingRight) {
                    pl = "(";
                    pr = ")";
                }
                if(t.u.a.isVLA) {
                    da_printf(buf, "%s%s%s[*]", pl, base, pr);
                } else {
                    if(t.u.a.count) {
                        da_printf(buf, "%s%s%s[%s%zu]", pl, base, pr, t.u.a.isStatic ? "static " : "", t.u.a.count);
                    } else if(t.u.a.countExpr) {
                        char *countExpr = NULL;
                        char **prevBuf = gPrintBuffer;
                        gPrintBuffer = &countExpr;
                        print_expr(t.u.a.countExpr);
                        gPrintBuffer = prevBuf;
                        jkc99_assert(countExpr && countExpr[0]);
                        da_printf(buf, "%s%s%s[%s%s]", pl, base, pr, t.u.a.isStatic ? "static " : "", countExpr);
                    } else {
                        size_t i = 0;
                        jkc99_assert(t.u.a.init && t.u.a.init->kind == kInitializerList);
                        InitializerList *list = &t.u.a.init->u.list;
                        for(i = 0; i < list->count; ++i) {
                            if(list->designations[i].count) {
                                break;
                            }
                        }

                        if(i == list->count) {
                            da_printf(buf, "%s%s%s[%s%zu]", pl, base, pr, t.u.a.isStatic ? "static " : "", list->count);
                        } else {
                            //TODO 
                            jkc99_assert(false);
                            da_printf(buf, "%s%s%s[%s]", pl, base, pr, t.u.a.isStatic ? "static " : "");
                        }
                    }
                }
                buf = generate_type_declaration_specifiers_(ctx, t.u.a.base, buf, true);
            } break;
        case kTypePointer:
            {
                da_printf(buf, "*%s", base);
                buf = generate_type_declaration_specifiers_(ctx, t.u.p.base, buf, false);
            } break;
        case kTypeBitField:
            {
                jkc99_assert(false); //TODO
            } break;
        case kTypeEnum:
            {
                if(t.u.e.tag && t.u.e.scope == 0) {
                    da_printf(buf, "enum %s%s%s", t.u.e.tag, space, base);
                } else {
                    for(size_t i=0; i < t.userCount; ++i) {
                        Symbol *sym = jkc99_sym_get_global_index(ctx, t.users[i]);
                        if(sym->kind == kSymbolType) {
                            da_printf(buf, "%s%s%s", sym->identifier, space, base);
                            break;
                        }
                    } 
                    if(!buf) {
#if 0
                        jkc99_assert(false);
                        oldBuffer = gPrintBuffer;
                        gPrintBuffer = &buf;
                        jkc99_assert(t.u.e.specifier);
                        print_enum_specifier(t.u.e.specifier);
                        gPrintBuffer = oldBuffer;
                        da_printf(buf, "%s", base);
#endif
                    }
                }
            } break;
        case kTypeStruct:
            {
                if(t.u.su.tag && t.u.su.scope == 0) {
                    da_printf(buf, "struct %s%s%s", t.u.su.tag, space, base);
                } else {
                    for(size_t i=0; i < t.userCount; ++i) {
                        Symbol *sym = jkc99_sym_get_global_index(ctx, t.users[i]);
                        if(sym->kind == kSymbolType) {
                            da_printf(buf, "%s%s%s", sym->identifier, space, base);
                            break;
                        }
                    } 
                    if(!buf) {
                        oldBuffer = gPrintBuffer;
                        gPrintBuffer = &buf;
                        printf_("struct");
                        goto anonymous_struct_or_union;
                    }
                }
            } break;
        case kTypeUnion:
            {
                if(t.u.su.tag && t.u.su.scope == 0) {
                    da_printf(buf, "union %s%s%s", t.u.su.tag, space, base);
                } else {
                    for(size_t i=0; i < t.userCount; ++i) {
                        Symbol *sym = jkc99_sym_get_global_index(ctx, t.users[i]);
                        if(sym->kind == kSymbolType) {
                            da_printf(buf, "%s%s%s", sym->identifier, space, base);
                            break;
                        }
                    } 
                    if(!buf) {
#if 0
                        oldBuffer = gPrintBuffer;
                        gPrintBuffer = &buf;
                        printf_("union");
anonymous_struct_or_union:
                        jkc99_assert(t.u.su.specifier);
                        print_struct_or_union_specifier(t.u.su.specifier, false);
                        gPrintBuffer = oldBuffer;
                        da_printf(buf, "%s", base);
#else
                        oldBuffer = gPrintBuffer;
                        gPrintBuffer = &buf;
                        printf_("union");
anonymous_struct_or_union:
                        jkc99_assert(t.u.su.specifier);
                        generate_struct_or_union_specifier(ctx, t.u.su.specifier);
                        gPrintBuffer = oldBuffer;
                        da_printf(buf, "%s", base);
#endif
                    }
                }
            } break;
        case kTypeFunc:
            {
                const char *pl = "";
                const char *pr = "";
                if(base[0] && !goingRight) {
                    pl = "(";
                    pr = ")";
                }
                da_printf(buf, "%s%s%s(", pl, base, pr);
                if(t.u.f.paramCount) {
                    da_printf(buf, "%s", generate_type_declaration_specifiers_(ctx, t.u.f.paramTypes[0], "", true));
                }
                for(size_t i = 1; i < t.u.f.paramCount; ++i) {
                    da_printf(buf, ", %s", generate_type_declaration_specifiers_(ctx, t.u.f.paramTypes[i], "", true));
                }
                if(t.u.f.hasEllipsis) {
                    da_printf(buf, ", ...)");
                } else {
                    da_printf(buf, ")");
                }
                buf = generate_type_declaration_specifiers_(ctx, t.u.f.returnType, buf, true);
            } break;
        case kTypeVector:
            {
                da_printf(buf, "__attribute__((__vector_size__(%d))) %s", t.u.v.bytes, base);
                buf = generate_type_declaration_specifiers_(ctx, t.u.v.base, buf, false);
            } break;

        default:
            {
                jkc99_assert(false);
            } break;
    }

    gPrintMinify = false;
    return buf;
}

static inline void print_type_users(JKC99Type *t) {
    if(t->userCount) {
        printf_(", .userCount=%zu, .users=(introspect_size_t[%zu]){ ", t->userCount, t->userCount);
        printf_("%zu", t->users[0]);
        for(size_t j=1; j < t->userCount; ++j) {
            printf_(", %zu", t->users[j]);
        }
        printf_(" }");
    }
}

JKC99_HOOK(generate_introspect_h) {
    TypeHandle typeHandle;
    int digitCount = 0;
    char *buf = NULL;
    size_t typeCount = jkc99_type_count(ctx);
    size_t symbolCount = jkc99_sym_count_global(ctx);

    gPrintBuffer = &buf;

    for(size_t num = typeCount; num; num /= 10) {
        digitCount++;
    }

#ifdef __clang__
    printf_("\n\n\n#pragma clang diagnostic push\n");
    printf_("#pragma clang diagnostic ignored \"-Wmissing-braces\"\n\n");
#endif

    printf_(JKC99_INTROSPECT_UNUSED"static "JKC99_INTROSPECT_TYPE(TypeInfo)" "JKC99_INTROSPECT_GLOBAL(TypeInfo_)"[] = {\n");

    gPrintDirectives = false;
    typeHandle = jkc99_type_first(ctx);

    for(size_t i = 0; i < typeCount; i++, typeHandle = jkc99_type_next(ctx, typeHandle, 1)) {
        char *typeName = NULL;
        JKC99Type type;
        jkc99_type_get(ctx, typeHandle, &type);
        JKC99Type *t = &type;
        printf_("    [%*zu] = ", digitCount, i);
        switch(t->kind) {
            case kTypeUnknown:
                {
                    jkc99_assert(i == 0);
                    printf_("{ .name=\"<Unknown type>\", .kind="JKC99_INTROSPECT_KIND(TypeUnknown)" },\n");
                } break;
            case kTypeBuiltin:
                {
                    jkc99_assert(t->u.builtin.name);
                    typeName = (char*)t->u.builtin.name;
                    printf_("{ .name=\"%s\", .kind="JKC99_INTROSPECT_KIND(TypeBuiltin)", .size=sizeof(%s) ", typeName, typeName);
                    print_type_users(t);
                    printf_(" },\n");
                } break;
            case kTypeVoid:
                {
                    printf_("{ .name=\"void\", .kind="JKC99_INTROSPECT_KIND(TypeVoid)" },\n");
                } break;
            case kTypeChar:
                {
                    printf_("{ .name=\"char\", .kind="JKC99_INTROSPECT_KIND(TypeChar)", .size=sizeof(char) ");
                    print_type_users(t);
                    printf_(" },\n");
                } break;
            case kTypeSignedChar:
                {
                    printf_("{ .name=\"signed char\", .kind="JKC99_INTROSPECT_KIND(TypeSignedChar)", .size=sizeof(signed char) ");
                    print_type_users(t);
                    printf_(" },\n");
                } break;
            case kTypeSignedShort:
                {
                    printf_("{ .name=\"short\", .kind="JKC99_INTROSPECT_KIND(TypeSignedShort)", .size=sizeof(short) ");
                    print_type_users(t);
                    printf_(" },\n");
                } break;
            case kTypeSignedInt:
                {
                    printf_("{ .name=\"int\", .kind="JKC99_INTROSPECT_KIND(TypeSignedInt)", .size=sizeof(int) ");
                    print_type_users(t);
                    printf_(" },\n");
                } break;
            case kTypeSignedLong:
                {
                    printf_("{ .name=\"long\", .kind="JKC99_INTROSPECT_KIND(TypeSignedLong)", .size=sizeof(long) ");
                    print_type_users(t);
                    printf_(" },\n");
                } break;
            case kTypeSignedLongLong:
                {
                    printf_("{ .name=\"long long\", .kind="JKC99_INTROSPECT_KIND(TypeSignedLongLong)", .size=sizeof(long long) ");
                    print_type_users(t);
                    printf_(" },\n");
                } break;
            case kType_Bool:
                {
                    printf_("{ .name=\"_Bool\", .kind="JKC99_INTROSPECT_KIND(Type_Bool)", .size=sizeof(_Bool) ");
                    print_type_users(t);
                    printf_(" },\n");
                } break;
            case kTypeUnsignedChar:
                {
                    printf_("{ .name=\"unsigned char\", .kind="JKC99_INTROSPECT_KIND(TypeUnsignedChar)", .size=sizeof(unsigned char) ");
                    print_type_users(t);
                    printf_(" },\n");
                } break;
            case kTypeUnsignedShort:
                {
                    printf_("{ .name=\"unsigned short\", .kind="JKC99_INTROSPECT_KIND(TypeUnsignedShort)", .size=sizeof(unsigned short) ");
                    print_type_users(t);
                    printf_(" },\n");
                } break;
            case kTypeUnsignedInt:
                {
                    printf_("{ .name=\"unsigned int\", .kind="JKC99_INTROSPECT_KIND(TypeUnsignedInt)", .size=sizeof(unsigned int) ");
                    print_type_users(t);
                    printf_(" },\n");
                } break;
            case kTypeUnsignedLong:
                {
                    printf_("{ .name=\"unsigned long\", .kind="JKC99_INTROSPECT_KIND(TypeUnsignedLong)", .size=sizeof(unsigned long) ");
                    print_type_users(t);
                    printf_(" },\n");
                } break;
            case kTypeUnsignedLongLong:
                {
                    printf_("{ .name=\"unsigned long long\", .kind="JKC99_INTROSPECT_KIND(TypeUnsignedLongLong)", .size=sizeof(unsigned long long) ");
                    print_type_users(t);
                    printf_(" },\n");
                } break;
            case kTypeFloat:
                {
                    printf_("{ .name=\"float\", .kind="JKC99_INTROSPECT_KIND(TypeFloat)", .size=sizeof(float) ");
                    print_type_users(t);
                    printf_(" },\n");
                } break;
            case kTypeDouble:
                {
                    printf_("{ .name=\"double\", .kind="JKC99_INTROSPECT_KIND(TypeDouble)", .size=sizeof(double) ");
                    print_type_users(t);
                    printf_(" },\n");
                } break;
            case kTypeLongDouble:
                {
                    printf_("{ .name=\"long double\", .kind="JKC99_INTROSPECT_KIND(TypeLongDouble)", .size=sizeof(long double) ");
                    print_type_users(t);
                    printf_(" },\n");
                } break;

            case kTypeFloat_Complex:
            case kTypeDouble_Complex:
            case kTypeLongDouble_Complex:
                {
                    jkc99_assert(false); /* TODO */
                } break;

            case kTypeQualified:
                {
                    typeName = generate_type_declaration_specifiers(ctx, JKC99_INDEX_TO_TYPE_HANDLE(i));
                    if(JKC99_TYPE_HANDLE_EQ(t->u.q.base, jkc99_type_void(ctx))) {
                        printf_("{ .name=\"%s\", .kind="JKC99_INTROSPECT_KIND(TypeQualified)", .size=0", typeName);
                    } else {
                        printf_("{ .name=\"%s\", .kind="JKC99_INTROSPECT_KIND(TypeQualified)", .size=sizeof(%s)", typeName, typeName);
                    }
                    print_type_users(t);
                    printf_(", .u.q.base=%zu },\n", JKC99_TYPE_HANDLE_TO_INDEX(t->u.q.base));
                } break;

            case kTypeArray:
                {
                    typeName = generate_type_declaration_specifiers(ctx, JKC99_INDEX_TO_TYPE_HANDLE(i));
                    printf_("{ .name=\"%s\", .kind="JKC99_INTROSPECT_KIND(TypeArray)", .size=sizeof(%s)",
                        typeName,
                        typeName
                        );
                    print_type_users(t);
                    printf_(", .u.a = { .base=%zu, .count=",
                        JKC99_TYPE_HANDLE_TO_INDEX(t->u.a.base)
                        );
                    if(t->u.a.count) {
                        printf_("%zu", t->u.a.count);
                    } else if(t->u.a.countExpr) {
                        printf_("(");
                        print_expr(t->u.a.countExpr);
                        printf_(")");
                    } else {
                        size_t i = 0;
                        jkc99_assert(t->u.a.init && t->u.a.init->kind == kInitializerList);
                        InitializerList *list = &t->u.a.init->u.list;
                        for(i = 0; i < list->count; ++i) {
                            if(list->designations[i].count) {
                                break;
                            }
                        }

                        if(i == list->count) {
                            printf_("%zu", list->count);
                        } else {
                            jkc99_assert(false); /* TODO */
                        }
                    }
                    printf_(" } },\n");
                } break;

            case kTypePointer:
                {
                    typeName = generate_type_declaration_specifiers(ctx, JKC99_INDEX_TO_TYPE_HANDLE(i));
                    
                    printf_("{ .name=\"%s\", .kind="JKC99_INTROSPECT_KIND(TypePointer)", .size=sizeof(%s)", typeName, typeName);
                    print_type_users(t);
                    printf_(", .u.p.base=%zu },\n", JKC99_TYPE_HANDLE_TO_INDEX(t->u.p.base));
                } break;

            case kTypeBitField:
                {
                    jkc99_assert(false); /* TODO */
                } break;

            case kTypeEnum:
                {
                    if(t->u.e.tag && t->u.e.scope == 0) {
                        printf_("{ .name=\"enum %s\", .kind="JKC99_INTROSPECT_KIND(TypeEnum)", .size=sizeof(enum %s)", t->u.e.tag, t->u.e.tag);
                    } else {
                        typeName = generate_type_declaration_specifiers(ctx, JKC99_INDEX_TO_TYPE_HANDLE(i));
                        if(typeName) {
                            printf_("{ .name=\"%s\", .kind="JKC99_INTROSPECT_KIND(TypeEnum)", .size=sizeof(%s)", typeName, typeName);
                        } else {
                            printf_("{ .name=\"<Anonymous enum>\", .kind="JKC99_INTROSPECT_KIND(TypeEnum)", .size=sizeof(int)");
                        }
                    }
                    print_type_users(t);
#if 0
                    printf_(", .u.e = { .enumeratorCount=%zu, .enumerators=("JKC99_INTROSPECT_TYPE(Enumerator)"[%zu]){\n", t->u.e.enumeratorCount, t->u.e.enumeratorCount);
                    for(size_t j=0; j < t->u.e.enumeratorCount; ++j) {
                        Enumerator *e = t->u.e.enumerators + j;
                        printf_("            { .name=\"%s\", .value=", e->identifier);
                        if(e->expr) {
                            jkc99_assert(e->exprOffset == 0);
                            if(e->exprResult.kind == kExprResultNotResolved) {
                                e->exprResult = eval_expr(ctx, type_signed_int(ctx), e->expr);
                                if(e->exprResult.kind == kExprResultSuccess) {
                                    jkc99_assert(JKC99_TYPE_HANDLE_EQ(e->exprResult.type, type_signed_int(ctx)));
                                    printf_("%d", e->exprResult.u.signed_int_val);
                                } else {
                                    jkc99_assert(e->exprResult.kind == kExprResultUnableToResolveAtCompileTime);
                                    print_expr(e->exprResult.u.expr);
                                }
                            }
                        } else {
                            Enumerator *ref;
                            jkc99_assert(e->exprOffset <= (int)j);

                            ref = t->u.e.enumerators + (j - e->exprOffset);

                            if(ref == t->u.e.enumerators && !ref->expr) {
                                printf_("%d", e->exprOffset);
                            } else {
                                if(ref->exprResult.kind == kExprResultSuccess) {
                                    jkc99_assert(JKC99_TYPE_HANDLE_EQ(ref->exprResult.type, type_signed_int(ctx)));
                                    printf_("%d", ref->exprResult.u.signed_int_val + (int)e->exprOffset);
                                } else {
                                    jkc99_assert(ref->exprResult.kind == kExprResultUnableToResolveAtCompileTime);
                                    printf_("(");
                                    print_expr(ref->exprResult.u.expr);
                                    printf_(") + %d", e->exprOffset);
                                }
                            }
                        }
                        printf_(" },\n");
                    }
                    printf_("        } } },\n");
#else
                    printf_(", .u.e = { .enumeratorCount=%zu, ", t->u.e.enumeratorCount);
                    if(t->u.e.enumeratorCount) {
                        printf_(".names = (const char*[%zu]){\n", t->u.e.enumeratorCount);
                        for(size_t j=0; j < t->u.e.enumeratorCount; ++j) {
                            Enumerator *e = t->u.e.enumerators + j;
                            printf_("                \"%s\",\n", e->identifier);
                        }
                        printf_("              }, .values = (int[%zu]){\n", t->u.e.enumeratorCount);
                        for(size_t j=0; j < t->u.e.enumeratorCount; ++j) {
                            Enumerator *e = t->u.e.enumerators + j;
                            if(e->expr) {
                                jkc99_assert(e->exprOffset == 0);
                                if(e->exprResult.kind == kExprResultNotResolved) {
                                    e->exprResult = jkc99_eval_expr(ctx, jkc99_type_signed_int(ctx), e->expr);
                                    if(e->exprResult.kind == kExprResultSuccess) {
                                        jkc99_assert(JKC99_TYPE_HANDLE_EQ(e->exprResult.type, jkc99_type_signed_int(ctx)));
                                        printf_("                %d,\n", e->exprResult.u.signed_int_val);
                                    } else {
                                        jkc99_assert(e->exprResult.kind == kExprResultUnableToResolveAtCompileTime);
                                        printf_("                (");
                                        print_expr(e->exprResult.u.expr);
                                        printf_("),\n");
                                    }
                                }
                            } else {
                                Enumerator *ref;
                                jkc99_assert(e->exprOffset <= (int)j);

                                ref = t->u.e.enumerators + (j - e->exprOffset);

                                if(ref == t->u.e.enumerators && !ref->expr) {
                                    printf_("                %d,\n", e->exprOffset);
                                } else {
                                    if(ref->exprResult.kind == kExprResultSuccess) {
                                        jkc99_assert(JKC99_TYPE_HANDLE_EQ(ref->exprResult.type, jkc99_type_signed_int(ctx)));
                                        printf_("                %d,\n", ref->exprResult.u.signed_int_val + (int)e->exprOffset);
                                    } else {
                                        jkc99_assert(ref->exprResult.kind == kExprResultUnableToResolveAtCompileTime);
                                        printf_("                (");
                                        print_expr(ref->exprResult.u.expr);
                                        printf_(") + %d,\n", e->exprOffset);
                                    }
                                }
                            }
                        }
                        printf_("            } } },\n");
                    } else {
                        printf_("} },\n");
                    }
#endif
                } break;

            case kTypeStruct:
                {
                    if(t->u.su.tag && t->u.su.scope == 0) {
                        da_printf(typeName, "struct %s", t->u.su.tag);
                        //typeName = generate_type_declaration_specifiers(ctx, JKC99_INDEX_TO_TYPE_HANDLE(i));
                        if(t->u.su.memberCount) {
                            printf_("{ .name=\"%s\", .kind="JKC99_INTROSPECT_KIND(TypeStruct)", .size=sizeof(%s)", typeName, typeName);
                        } else {
                            printf_("{ .name=\"%s\", .kind="JKC99_INTROSPECT_KIND(TypeStruct), typeName);
                            print_type_users(t);
                            printf_(" },\n");
                            break;
                        }
                        goto struct_or_union_members;
                    } else {
                        typeName = generate_type_declaration_specifiers(ctx, JKC99_INDEX_TO_TYPE_HANDLE(i));
                        const char *n = strchr(typeName, '\n');
                        if(n) {
                            const char *pos = typeName;
                            printf_("{ .name=\n");
                            while(n) {
                                printf_("\"%.*s\"\n", (int)(n-pos), pos);
                                pos = n+1;
                                n = strchr(pos, '\n');
                            }
                            printf_("\"%s\",\n", pos);
                        } else {
                            printf_("{ .name=\"%s\", ", typeName);
                        }
                        printf_(".kind="JKC99_INTROSPECT_KIND(TypeStruct)", .size=sizeof(%s)", typeName);
                        goto struct_or_union_members;
                    }
                } break;
            case kTypeUnion:
                {
                    if(t->u.su.tag && t->u.su.scope == 0) {
                        da_printf(typeName, "union %s", t->u.su.tag);
                        //typeName = generate_type_declaration_specifiers(ctx, JKC99_INDEX_TO_TYPE_HANDLE(i));
                        if(t->u.su.memberCount) {
                            printf_("{ .name=\"%s\", .kind="JKC99_INTROSPECT_KIND(TypeUnion)", .size=sizeof(%s)", typeName, typeName);
                        } else {
                            printf_("{ .name=\"%s\", .kind="JKC99_INTROSPECT_KIND(TypeUnion), typeName);
                            print_type_users(t);
                            printf_(" },\n");
                            break;
                        }
                    } else {
                        typeName = generate_type_declaration_specifiers(ctx, JKC99_INDEX_TO_TYPE_HANDLE(i));
                        const char *n = strchr(typeName, '\n');
                        if(n) {
                            const char *pos = typeName;
                            printf_("{ .name=\n");
                            while(n) {
                                printf_("\"%.*s\"\n", (int)(n-pos), pos);
                                pos = n+1;
                                n = strchr(pos, '\n');
                            }
                            printf_("\"%s\",\n", pos);
                        } else {
                            printf_("{ .name=\"%s\", ", typeName);
                        }
                        printf_(".kind="JKC99_INTROSPECT_KIND(TypeUnion)", .size=sizeof(%s)", typeName);
                    }
struct_or_union_members:
                    print_type_users(t);
                    print_indent_inc();
                    print_indent_inc();
                    print_newline(NULL);
                    printf_(", .u.su = { .memberCount=%zu, .members=("JKC99_INTROSPECT_TYPE(Member)"[%zu]){", t->u.su.memberCount, t->u.su.memberCount);
                    print_indent_inc();
                    print_newline(NULL);
                    jkc99_assert(t->u.su.memberCount);
                    for(size_t j = 0; j < t->u.su.memberCount; ++j) {
                        JKC99Type type;
                        /* TODO Handle bitfields */
                        jkc99_type_get(ctx, t->u.su.members[j].type, &type);
                        jkc99_assert(type.kind != kTypeBitField);
                        print_newline(NULL);
                        //printf_("{ .name=\"%s\", .type=%zu, .offset=((size_t)&((%s){0}).%s) },",
                        /* TODO Offsetof for other compilers */
                        printf_("{ .name=\"%s\", .type=%zu, .offset="JKC99_INTROSPECT_OFFSETOF"(%s, %s) },",
                            t->u.su.members[j].identifier,
                            JKC99_TYPE_HANDLE_TO_INDEX(t->u.su.members[j].type),
                            typeName,
                            t->u.su.members[j].identifier
                            );
                    }
                    print_indent_dec();
                    print_newline(NULL);
                    printf_(" } } },\n");
                    print_indent_dec();
                    print_indent_dec();
                } break;

            case kTypeFunc:
                {
                    typeName = generate_type_declaration_specifiers(ctx, JKC99_INDEX_TO_TYPE_HANDLE(i));
                    printf_("{ .name=\"%s\", .kind="JKC99_INTROSPECT_KIND(TypeFunction), typeName);
                    print_type_users(t);
                    printf_(", .u.f = { .returnType=%zu, .hasVarargs=%d, .parameterCount=%zu, .parameters=("JKC99_INTROSPECT_TYPE(TypeHandle)"[%zu]){", 
                            JKC99_TYPE_HANDLE_TO_INDEX(t->u.f.returnType), 
                            t->u.f.hasEllipsis ? 1 : 0, 
                            t->u.f.paramCount,
                            t->u.f.paramCount
                            );
                    if(t->u.f.paramCount) {
                        printf_("%zu", JKC99_TYPE_HANDLE_TO_INDEX(t->u.f.paramTypes[0]));
                    }
                    for(size_t j=1; j < t->u.f.paramCount; j++) {
                        printf_(",%zu", JKC99_TYPE_HANDLE_TO_INDEX(t->u.f.paramTypes[j]));
                    }
                    printf_("} } },\n");
                } break;

            case kTypeVector:
                {
                    typeName = generate_type_declaration_specifiers(ctx, JKC99_INDEX_TO_TYPE_HANDLE(i));
                    printf_("{ .name=\"%s\", .kind="JKC99_INTROSPECT_KIND(TypeVector)", .u.v = { .base=%zu, .bytes=%d } },\n", 
                            typeName,
                            JKC99_TYPE_HANDLE_TO_INDEX(t->u.v.base),
                            t->u.v.bytes
                            );
                } break;

            default:
                {
                    jkc99_assert(false);
                } break;
        }
    }
    printf_("};\n\n");
#ifdef __clang__
    printf_("#pragma clang diagnostic pop\n\n");
#endif

    digitCount = 0;
    for(size_t num = symbolCount; num; num /= 10) {
        digitCount++;
    }

#ifdef __clang__
    printf_("#pragma clang diagnostic push\n");
    printf_("#pragma clang diagnostic ignored \"-Wdeprecated-declarations\"\n\n");
#endif

    printf_(JKC99_INTROSPECT_UNUSED"static "JKC99_INTROSPECT_TYPE(Symbol)" "JKC99_INTROSPECT_GLOBAL(Symbols_)"[] = {\n");
    for(size_t i = 0; i < symbolCount; ++i) {
        Symbol *s = jkc99_sym_get_global_index(ctx, i);
        printf_("    [%*zu] = ", digitCount, i);
        switch(s->kind) {
            case kSymbolType:
                {
                    printf_("{ .name = \"%s\", .kind = "JKC99_INTROSPECT_KIND(SymbolType)", .type = %zu },\n", 
                            s->identifier, 
                            JKC99_TYPE_HANDLE_TO_INDEX(s->type)
                            );
                } break;
            case kSymbolFunction:
                {
                    Declarator *d;
                    const char *ptrStr = s->identifier;

                    if(s->u.functionDefinition) {
                        FunctionDefinition *fd = s->u.functionDefinition;
                        jkc99_assert(fd->declarator);
                        jkc99_assert(fd->declarator->direct);
                        jkc99_assert(fd->declarator->direct->kind == kDirectDeclaratorFunc);
                        jkc99_assert(fd->declarator->direct->u.func.hasParameterList);
                        d = fd->declarator;
                        for(size_t j = 0; j < fd->specifierCount; j++) {
                            DeclarationSpecifier *spec = fd->specifiers + j;
                            if(spec->kind == kDeclarationSpecifierDeclspec) {
                                Declspec *ds = &spec->u.declspec;
                                for(size_t k = 0; k < ds->modifierCount; k++) {
                                    if(ds->modifiers[k].kind == kDeclspecModifier_deprecated) {
                                        ptrStr = NULL;
                                        goto finish_deprecated_lookup_fd;
                                    }
                                }
                            }
                        }
finish_deprecated_lookup_fd:;
                    } else {
                        Declaration *decl = s->declarations[0].declaration;
                        jkc99_assert(s->declarations);
                        jkc99_assert(s->declarations[0].declaration->initDeclaratorCount > s->declarations[0].declaratorIndex);
                        InitDeclarator *id = s->declarations[0].declaration->initDeclarators + s->declarations[0].declaratorIndex;
                        jkc99_assert(id);
                        jkc99_assert(id->declarator);
                        d = id->declarator;
                        if(decl->storageClass == kStorageClassStatic) {
                            ptrStr = NULL;
                            goto finish_deprecated_lookup_decl;
                        }
                        /* TODO Should we be checking every declaration? */
                        for(size_t j = 0; j < decl->specifierCount; j++) {
                            DeclarationSpecifier *spec = decl->specifiers + j;
                            if(spec->kind == kDeclarationSpecifierDeclspec) {
                                Declspec *ds = &spec->u.declspec;
                                for(size_t k = 0; k < ds->modifierCount; k++) {
                                    if(ds->modifiers[k].kind == kDeclspecModifier_deprecated) {
                                        ptrStr = NULL;
                                        goto finish_deprecated_lookup_decl;
                                    }
                                }
                            }
                        }
finish_deprecated_lookup_decl:;
                        jkc99_assert(d->direct);
                        jkc99_assert(d->direct->kind == kDirectDeclaratorFunc);
                        jkc99_assert(d->direct->u.func.hasParameterList);
                    }

                    if(s->identifier[0] == '_') { // && s->identifier[1] == '_') {
                        ptrStr = NULL;
                    }

                    ptrStr = introspect_check_symbol_exclusion(ptrStr);

                    printf_("{ .name = \"%s\", .kind = "JKC99_INTROSPECT_KIND(SymbolFunction)", .type = %zu%s",
                            s->identifier, 
                            JKC99_TYPE_HANDLE_TO_INDEX(s->type),
                            (ptrStr || d->direct->u.func.count) ? ", .u.func = { " : "");
                    if(ptrStr) {
                        printf_(".ptr = (void (*)(void))%s%s", 
                                ptrStr,
                                d->direct->u.func.count ? ", " : " "
                               );
                    }
                    if(d->direct->u.func.count) {
                        printf_(".parameterNames = (const char *[]){ ");
                        for(size_t j=0; j < d->direct->u.func.count; j++) {
                            ParameterDeclaration *pd = d->direct->u.func.u.parameters[j];
                            if(pd->declarator) {
                                const char *name;
                                jkc99_declarator_get_type_and_name(ctx, pd->declarator, pd->type, &name, NULL);
                                printf_("\"%s\", ", name);
                            } else {
                                printf_("\"\", ");
                            }
                        }
                    }
                    printf_("%s},\n", (d->direct->u.func.count) ? "} } " : (ptrStr ? "} " : ""));
                } break;
            case kSymbolVariable:
                {
                    const char *ptrStr = s->identifier;
                    if(!s->u.definition.declaration) {
                        ptrStr = NULL;
                    }
                    ptrStr = introspect_check_symbol_exclusion(ptrStr);
                    printf_("{ .name = \"%s\", .kind = "JKC99_INTROSPECT_KIND(SymbolGlobal)", .type = %zu",
                            s->identifier, 
                            JKC99_TYPE_HANDLE_TO_INDEX(s->type)
                            );
                    if(ptrStr) {
                        printf_(", .u.global = (void*)&%s },\n", ptrStr);
                    } else {
                        printf_(" },\n");
                    }
                } break;
            case kSymbolEnumerator:
                {
                    printf_("{ .name = \"%s\", .kind = "JKC99_INTROSPECT_KIND(SymbolEnumerator)", .type = %zu, .u.value = %s},\n", 
                            s->identifier, 
                            JKC99_TYPE_HANDLE_TO_INDEX(s->type),
                            s->identifier
                            );
                } break;
            default: jkc99_assert(false); break;
        }
    }
    printf_("};\n\n");

#ifdef __clang__
    printf_("#pragma clang diagnostic pop\n\n");
#endif

    printf_(JKC99_INTROSPECT_UNUSED""JKC99_INTROSPECT_TYPE(TypeInfo)" *"JKC99_INTROSPECT_GLOBAL(TypeInfo)" = "JKC99_INTROSPECT_GLOBAL(TypeInfo_)";\n");
    printf_(JKC99_INTROSPECT_UNUSED"introspect_size_t "JKC99_INTROSPECT_GLOBAL(TypeInfoCount)" = %zu;\n", typeCount);
    printf_(JKC99_INTROSPECT_UNUSED""JKC99_INTROSPECT_TYPE(Symbol)" *"JKC99_INTROSPECT_GLOBAL(Symbols)" = "JKC99_INTROSPECT_GLOBAL(Symbols_)";\n");
    printf_(JKC99_INTROSPECT_UNUSED"introspect_size_t "JKC99_INTROSPECT_GLOBAL(SymbolCount)" = %zu;\n", symbolCount);

    //printf("%s", buf);
    ParseBuffer newbuf = jkc99_buffer_new(ctx, jkc99_str_intern(ctx, ""), da_count(buf), buf);
#if 0
    ParseBuffer oldbuf = jkc99_buffer_get(ctx);
    jkc99_buffer_set(ctx, newbuf);
    parse(ctx);
    jkc99_buffer_set(ctx, oldbuf);
#else
    jkc99_buffer_parse(ctx, newbuf);
#endif

    return false;
}

JKC99_HOOK(parse_type_info) {
    Expr **expr = (Expr**)ptr;
    Source src = {0};

    jkc99_lexer_source(ctx, &src);

    if(jkc99_lexer_match_id(ctx, gIntrospectModule.str_type_info)) {
        if(jkc99_lexer_match(ctx, '(')) {
            TypeHandle type = JKC99_INVALID_TYPE_HANDLE;
            Source parenSrc;
            TypeName *typeName;

            jkc99_lexer_source(ctx, &parenSrc);
            typeName = jkc99_parse_type_name(ctx);
            if(typeName) {
                jkc99_assert(JKC99_TYPE_HANDLE_NOT_INVALID(typeName->type));
                type = typeName->type;
            } else {
                /* TODO Support arbitrary expressions */
                jkc99_lexer_rollback(ctx, &parenSrc);
                Expr *typeInfoExpr = jkc99_parse_expr_primary(ctx);
                if(typeInfoExpr) {
                    if(typeInfoExpr->u.primary.kind == kExprPrimaryIdentifier) {
                        const char *id = typeInfoExpr->u.primary.u.identifier;
                        Symbol *sym = jkc99_sym_get(ctx, id);
                        if(sym) {
                            jkc99_assert(JKC99_TYPE_HANDLE_NOT_INVALID(sym->type));
                            type = sym->type;
                        } else {
                            jkc99_log_error_src(ctx, &parenSrc, "Unknown identifier '%s'.", id);
                        }
                    } else {
                        jkc99_log_error_src(ctx, &parenSrc, "Unexpected expression type in type_index(expr). Note that currently only identifiers are supported.");
                    }
                } else {
                    jkc99_log_error_src(ctx, &parenSrc, "Unable to parse the expression in type_index(expr). Note that currently only identifiers are supported.");
                }
            }
            if(JKC99_TYPE_HANDLE_NOT_INVALID(type)) {
                char str[64];
                size_t index = JKC99_TYPE_HANDLE_TO_INDEX(type);
                sprintf(str, "%zu", index);
                *expr = 
                    jkc99_expr_expr(ctx, &src, jkc99_expr_additive(ctx, &parenSrc, kExprAdditiveAdd,
                        jkc99_expr_identifier(ctx, &parenSrc, gIntrospectModule.str_gIntrospectTypeInfo),
                        jkc99_expr_int(ctx, &parenSrc, jkc99_str_intern(ctx, str), index, kConstantDecimal, 3, "ULL")
                        ));
            }
            jkc99_lexer_require(ctx, ')');
        } else {
            jkc99_log_error_src(ctx, &src, "Expected '(' after type_index.");
        }
        return true;
    }
    return false;
}

OPTION_FUNC(exclude) {
    size_t len;
    IntrospectSymbolExclusion *exclusions = NULL;

    if(!value) {
        fprintf(stderr, "The file name of symbol exclusion file is missing from the exclude option\n");
        return false;
    }

    const char *buf = jkc99_os_read_entire_file(value, &len);
    if(!buf) {
        fprintf(stderr, "Unable to read or empty symbol exclusion file\n");
        return false;
    }

    char *str = (char*)buf;

    while(*str) {
        IntrospectSymbolExclusion e = {0};
        while(*str && isspace(*str)) {
            str++;
        }
        e.identifier = str;
        while(*str && !isspace(*str)) {
            str++;
        }
        if(*str) {
            *str = '\0';
            str++;
        }
        da_push(exclusions, e);
    }

    gIntrospectModule.symbolExclusionCount = da_count(exclusions);
    gIntrospectModule.symbolExclusions = exclusions;

    return true;
}

static Option gOptions[] = {
    OPTION('e', exclude, "Specify symbol exclusion file")
};

#if 1
EXPORT JKC99_MODULE_GET_OPTIONS(jkc99_module_get_options) {
    *count = jkc99_array_count(gOptions);
    return gOptions;
}
#endif

EXPORT JKC99_MODULE_INIT(jkc99_module_init) {
    _Bool carryOn = true;
    gIntrospectModule.str_type_info = jkc99_str_intern(ctx, "type_info");
    gIntrospectModule.str_gIntrospectTypeInfo = jkc99_str_intern(ctx, JKC99_INTROSPECT_GLOBAL(TypeInfo));

#if 0
    for(int i = 0; i < argc && argv[i][0] == '-'; ++i) {
        if(argv[i][1] == '-') {
            bool found = false;
            size_t len;
            const char *val = NULL;
            for(len = 2; ; ++len) {
                if(argv[i][len] == '=') {
                    val = argv[i] + len + 1;
                    break;
                } else if(argv[i][len] == '\0') {
                    break;
                }
            }
            len -= 2;
            for(size_t j = 0; j < jkc99_array_count(gOptions); ++j) {
                if(gOptions[j].nameLen == len && strncmp(gOptions[j].name, argv[i] + 2, len) == 0) {
                    carryOn = gOptions[j].func(ctx, val);
                    found = true;
                    break;
                }
            }
            if(!found) {
                fprintf(stderr, "Unrecognised option %s\n", argv[i]);
            }
        } else if(argv[i][1]) {
            option_func *func = gOptions[(int)argv[i][1]].func;
            if(func) {
                carryOn = func(ctx, argv[i][2] == '=' ? argv[i] + 3 : NULL);
            } else {
                fprintf(stderr, "Unrecognised option -%c\n", argv[i][1]);
            }
        }
    }
#endif

    ParseBuffer buffer = jkc99_buffer_new(ctx, "", strlen(gIntrospectHeader), gIntrospectHeader);
    jkc99_buffer_set(ctx, buffer);
    jkc99_parse(ctx);
    jkc99_install_hook(ctx, kHookParseExprUnaryNone, parse_type_info, NULL);
    jkc99_install_hook(ctx, kHookParseComplete, generate_introspect_h, NULL);

    return carryOn;
}
