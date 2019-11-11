#include "jkc99.h"

#define JK_DYNAMIC_ARRAY_ASSERT jkc99_assert
#define JK_DYNAMIC_ARRAY_SIZE_T size_t
#include "dynamic_array.h"


#define JK_ARENA_IMPLEMENTATION
#define JK_ARENA_BLOCK_SIZE 8388608
#define JK_ARENA_ASSERT jkc99_assert
#include "arena.h"

typedef enum TagKind {
    kTagNone,
    kTagStruct,
    kTagUnion,
    kTagEnum
} TagKind;

typedef struct Tag {
    TagKind                     kind;
    const char                  *identifier;
    TypeHandle                  type;
} Tag;

typedef struct JKC99Scope {
    //TODO Hash table lookups for tags and symbols
    struct Tag                  *tags;
    struct Symbol               *symbols;
} JKC99Scope;

struct ParseContext {
    bool                            parsing;
    const char                      *filename;
    Arena                           arena;
    Strings                         strings;
    stb_lexer                       *buffers;
    stb_lexer                       *lexer;
    Hook                            *hooks[kHookCount];
    JKC99Scope                      *scopes;
    JKC99Type                       *types;
    _Bool                           signedChar;
    size_t                          currentDirectiveCount;
    PreprocessorDirective           *directives;
    ExternalDeclaration             *externalDeclarations;
    TypeHandle                      type_void;
    TypeHandle                      type_char;
    TypeHandle                      type_signed_char;
    TypeHandle                      type_signed_short;
    TypeHandle                      type_signed_int;
    TypeHandle                      type_signed_long;
    TypeHandle                      type_signed_long_long;
    TypeHandle                      type__Bool;
    TypeHandle                      type_unsigned_char;
    TypeHandle                      type_unsigned_short;
    TypeHandle                      type_unsigned_int;
    TypeHandle                      type_unsigned_long;
    TypeHandle                      type_unsigned_long_long;
    TypeHandle                      type_float;
    TypeHandle                      type_double;
    TypeHandle                      type_long_double;
    TypeHandle                      type_float__Complex;
    TypeHandle                      type_double__Complex;
    TypeHandle                      type_long_double__Complex;
    TypeHandle                      type_size_t;
#ifdef __clang__
    TypeHandle                      type___builtin_va_list;
#endif
};

#ifdef JKC99_TEST_BUILD
#include "modules/test.h"
#endif

JKC99_API const char *jkc99_str_intern_range(ParseContext *ctx, const char *str, size_t len) {
    size_t i;

    /* TODO Hash table */
    for(i = 0; i < da_count(ctx->strings.strings); ++i) {
        StringIntern *s = ctx->strings.strings + i;
        if(len == s->len && strncmp(str, s->str, len) == 0) {
            return s->str;
        }
    }

    {
        char *buf;
        StringIntern newstr = {0};
        newstr.len = len;
        buf = arena_alloc(&ctx->arena, len+1);
        sprintf(buf, "%.*s", (int)len, str);
        newstr.str = buf;
        da_push(ctx->strings.strings, newstr);
        return newstr.str;
    }
}

JKC99_API inline const char *jkc99_str_intern(ParseContext *ctx, const char *str) {
    return jkc99_str_intern_range(ctx, str, strlen(str));
}

JKC99_API void jkc99_log_error(ParseContext *ctx, const char *format, ...) {
    UNUSED unsigned int col = 0;
    va_list va;
    jkc99_assert(ctx);
    jkc99_assert(format);
    va_start(va, format);
    col = (ctx->lexer->where_firstchar > ctx->lexer->lineBegin) ? ctx->lexer->where_firstchar-ctx->lexer->lineBegin + 1 : 0;
    fprintf(stderr, "%s(%u) : error: ", ctx->lexer->file, ctx->lexer->line);
    vfprintf(stderr, format, va);
    fprintf(stderr, "\n");
}

JKC99_API void jkc99_log_error_src(ParseContext *ctx, Source *src, const char *format, ...) {
    va_list va;
    jkc99_assert(ctx);
    jkc99_assert(src);
    jkc99_assert(format);
    va_start(va, format);
    fprintf(stderr, "%s(%u) : error: ", src->file, src->line);
    vfprintf(stderr, format, va);
    fprintf(stderr, "\n");
}

JKC99_API void jkc99_log_error_unexpected(ParseContext *ctx) {
    if(ctx->lexer->token == CLEX_eof) {
        fprintf(stderr, "Unexpected EOF\n");
    } else{
        int len = (int)(ctx->lexer->where_lastchar-ctx->lexer->where_firstchar+1);
        jkc99_assert(len > 0);
        fprintf(stderr, "Unexpected token %*.s", len, ctx->lexer->where_firstchar);
    }
}

static void move_directives(void *fromPtr, void *toPtr) {
    ASTCommon *from = fromPtr;
    ASTCommon *to = toPtr;
    to->directiveCount = from->directiveCount;
    to->directives = from->directives;
    from->directiveCount = 0;
    from->directives = NULL;
}

static void jkc99_parse_empty_directives(ParseContext *ctx, void *ptr) {
    size_t newDirectiveCount = da_count(ctx->directives) - ctx->currentDirectiveCount;
    ASTCommon *node = ptr;
    PreprocessorDirective *newDirectives = NULL;

    jkc99_assert(da_count(ctx->directives) >= ctx->currentDirectiveCount);
    jkc99_assert(!ctx->currentDirectiveCount || ctx->directives);

    node->directiveCount = ctx->currentDirectiveCount;
    node->directives = ctx->directives;

    if(newDirectiveCount) {
        da_grow(newDirectives, newDirectiveCount);
        for(size_t i = 0; i < newDirectiveCount; i++) {
            newDirectives[i] = ctx->directives[ctx->currentDirectiveCount + i];
        }
    }

    ctx->currentDirectiveCount = newDirectiveCount;
    ctx->directives = newDirectives;
}

static char* skip_preprocessing_directive(ParseContext *ctx) {
    stb_lexer *lexer = ctx->lexer;
    char *p = lexer->where_lastchar;
    while(p != lexer->eof && *p != '\r' && *p != '\n') {
        if(*p == '\\') {
            ++p;
            if(p != lexer->eof) {
                if(*p == '\r') {
                    ++p;
                    if(p != lexer->eof && *p == '\n') {
                        ++p;
                    }
                    lexer->line++;
                    lexer->lineBegin = p;
                } else if(*p == '\n') {
                    ++p;
                    if(p != lexer->eof && *p == '\r') {
                        ++p;
                    }
                    lexer->line++;
                    lexer->lineBegin = p;
                }
            }
        } else {
            ++p;
        }
    }
    lexer->parse_point = p;
    stb_c_lexer_get_token(lexer);
    return p;
}

JKC99_API void jkc99_lexer_source(ParseContext *ctx, Source *src) {
    src->file = ctx->lexer->file;
    src->line = ctx->lexer->line;
    src->lineMarkerFlags = ctx->lexer->lineMarkerFlags;
    //jkc99_assert(ctx->lexer->where_firstchar >= ctx->lexer->lineBegin);
    //jkc99_assert((ctx->lexer->where_firstchar - ctx->lexer->lineBegin) < UINT_MAX);
    src->column = (ctx->lexer->where_firstchar >= ctx->lexer->lineBegin) ?
                    (unsigned int)(ctx->lexer->where_firstchar - ctx->lexer->lineBegin) : 0U;
    src->from = ctx->lexer->where_firstchar;
}

#define source_end(ctx,src)

JKC99_API void jkc99_lexer_rollback(ParseContext *ctx, Source *src) {
    ctx->lexer->file = src->file;
    ctx->lexer->line = src->line;
    ctx->lexer->lineBegin = src->from - src->column;
    ctx->lexer->lineMarkerFlags = src->lineMarkerFlags;
    ctx->lexer->parse_point = (char*)src->from;
    //lexer_next(ctx);
    stb_c_lexer_get_token(ctx->lexer);
}

static void jkc99_lexer_skip_until(ParseContext *ctx, size_t count, long *tokens) {
    while(!jkc99_lexer_is(ctx, CLEX_eof)) {
        for(size_t i = 0; i < count; ++i) {
            if(ctx->lexer->token == tokens[i]) {
                return;
            }
        }
        jkc99_lexer_next(ctx);
    }
}

#define jkc99_lexer_skip_until_array(ctx,arr) jkc99_lexer_skip_until((ctx), jkc99_array_count((arr)), (arr))

static void jkc99_parse_preprocessor_skip_whitespace(ParseContext *ctx) {
    while(ctx->lexer->parse_point != ctx->lexer->eof &&
            (   *ctx->lexer->parse_point == ' '
            ||  *ctx->lexer->parse_point == '\t'
            ||  *ctx->lexer->parse_point == '\v'
            ||  *ctx->lexer->parse_point == '\f')) {
        ctx->lexer->parse_point++;
        if(ctx->lexer->parse_point != ctx->lexer->eof &&
            ctx->lexer->parse_point[0] == '\\' &&
            ctx->lexer->parse_point[1] == '\n') {
            ctx->lexer->parse_point += 2;
        }
    }
}

static unsigned int jkc99_parse_preprocessor_number(ParseContext *ctx) {
    unsigned long i = 0;
    char *start = ctx->lexer->parse_point;
    while(ctx->lexer->parse_point != ctx->lexer->eof &&
            *ctx->lexer->parse_point >= '0' &&
            *ctx->lexer->parse_point <= '9') {
        ctx->lexer->parse_point++;
    }
    i = strtoul(start, &ctx->lexer->parse_point, 10);
    jkc99_assert(i < UINT_MAX);
    return (unsigned int)i;
}

static const char *jkc99_parse_preprocessor_string(ParseContext *ctx) {
    const char *str = NULL;
    char *p = ctx->lexer->parse_point;

    if(*p == '"') {
        p++;
        while(p != ctx->lexer->eof) {
            if(*p == '\\' && p[1] == '"') {
                p += 2;
            } else if(*p == '"') {
                p++;
                break;
            } else {
                p++;
            }
        }
        str = jkc99_str_intern_range(ctx, ctx->lexer->parse_point+1, (size_t)(p-ctx->lexer->parse_point-2));
        ctx->lexer->parse_point = p;
    }


    return str;
}

static void jkc99_parse_preprocessor_finish(ParseContext *ctx) {
    while(ctx->lexer->parse_point != ctx->lexer->eof) {
        if(*ctx->lexer->parse_point == '\n' || *ctx->lexer->parse_point == '\r') {
            ctx->lexer->parse_point += (ctx->lexer->parse_point[0] + ctx->lexer->parse_point[1]) == ('\n' + '\r') ? 2 : 1;
            break;
        } else {
            ctx->lexer->parse_point++;
        }
    }
}

JKC99_API void jkc99_lexer_next(ParseContext *ctx) {
    ctx->currentDirectiveCount = da_count(ctx->directives);
    stb_c_lexer_get_token(ctx->lexer);

    while(jkc99_lexer_is(ctx, '#')) {
        PreprocessorDirective dir = {0};

        jkc99_lexer_source(ctx, &dir.src);

        if((ctx->lexer->eof - ctx->lexer->where_firstchar) >= 2 &&
            ctx->lexer->where_firstchar[1] == ' ' &&
            isdigit(ctx->lexer->where_firstchar[2])) {
            unsigned int flag = 0;
            dir.kind = kPreprocessorDirectiveGCCLineMarker;
            dir.u.lineMarker.line = jkc99_parse_preprocessor_number(ctx);
            jkc99_parse_preprocessor_skip_whitespace(ctx);
            dir.u.lineMarker.file = jkc99_parse_preprocessor_string(ctx);
            do { 
                jkc99_parse_preprocessor_skip_whitespace(ctx);
                flag = jkc99_parse_preprocessor_number(ctx);
                jkc99_assert(flag <= 4);
                if(flag) {
                    dir.u.lineMarker.flags |= (1 << flag);
                }
            } while(flag);

            jkc99_parse_preprocessor_finish(ctx);

            jkc99_assert(dir.u.lineMarker.file);
            jkc99_assert(dir.u.lineMarker.line);
            jkc99_assert(dir.u.lineMarker.flags <= (bLineMarkerNewFile | bLineMarkerReturnToFile | bLineMarkerSystemHeader | bLineMarkerExternC));

            ctx->lexer->file = dir.u.lineMarker.file;
            ctx->lexer->line = dir.u.lineMarker.line;
            ctx->lexer->lineBegin = ctx->lexer->parse_point;
            ctx->lexer->lineMarkerFlags = dir.u.lineMarker.flags;

            stb_c_lexer_get_token(ctx->lexer);
        } else {
            stb_c_lexer_get_token(ctx->lexer);

            /* TODO The parsing here is a bit sloppy as we're using the tokens.
             * Should actually check properly without the whitespace skip etc. */
            if(ctx->lexer->token == CLEX_id) {
                if(jkc99_str_intern(ctx, ctx->lexer->string) == kw[kw_line]) {
                    stb_c_lexer_get_token(ctx->lexer);

                    dir.kind = kPreprocessorDirectiveLine;
                    jkc99_parse_preprocessor_skip_whitespace(ctx);
                    dir.u.line.line = jkc99_parse_preprocessor_number(ctx);
                    jkc99_parse_preprocessor_skip_whitespace(ctx);
                    dir.u.line.file = jkc99_parse_preprocessor_string(ctx);
                    jkc99_parse_preprocessor_finish(ctx);

                    jkc99_assert(dir.u.line.file);
                    jkc99_assert(dir.u.line.line);

                    ctx->lexer->file = dir.u.line.file;
                    ctx->lexer->line = dir.u.line.line;
                    ctx->lexer->lineBegin = ctx->lexer->parse_point;
                    ctx->lexer->lineMarkerFlags = 0;

                    stb_c_lexer_get_token(ctx->lexer);
                } else if(jkc99_str_intern(ctx, ctx->lexer->string) == kw[kw_pragma]) {
                    stb_c_lexer_get_token(ctx->lexer);
                    dir.kind = kPreprocessorDirectivePragma;
                    dir.u.pragma = ctx->lexer->where_firstchar;
                    dir.u.pragma = jkc99_str_intern_range(ctx, dir.u.pragma, (size_t)(skip_preprocessing_directive(ctx) - dir.u.pragma));
                    //stb_c_lexer_get_token(ctx->lexer);
                } else {
                    fprintf(stderr, "Aborting: Unexpected preprocessing token encountered, please make sure the target has been preprocessed. Only #line and #pragma preprocessing directives are allowed.\n");
                    jkc99_assert(false);
                    ctx->parsing = false;
                }
            }
        }
        if(dir.kind != kPreprocessorDirectiveNone) {
            da_push(ctx->directives, dir);
        }
    }
}

JKC99_API bool jkc99_lexer_is(ParseContext *ctx, long type) {
    if(ctx->lexer->token == type) {
        return true;
    }
    return false;
}

JKC99_API bool jkc99_lexer_is_id(ParseContext *ctx, const char *id) {
    if(ctx->lexer->token == CLEX_id && jkc99_str_intern(ctx, ctx->lexer->string) == id) {
        return true;
    } else {
        return false;
    }
}

JKC99_API bool jkc99_lexer_match(ParseContext *ctx, long type) {
    if(ctx->lexer->token == type) {
        jkc99_lexer_next(ctx);
        return true;
    }
    return false;
}

JKC99_API bool jkc99_lexer_match_id(ParseContext *ctx, const char *id) {
    if(ctx->lexer->token == CLEX_id && jkc99_str_intern(ctx, ctx->lexer->string) == id) {
        jkc99_lexer_next(ctx);
        return true;
    }
    return false;
}

JKC99_API bool jkc99_lexer_require(ParseContext *ctx, long type) {
    if(ctx->lexer->token == type) {
        jkc99_lexer_next(ctx);
        return true;
    } else {
        char got[64];
        jkc99_assert(ctx->lexer->where_lastchar >= ctx->lexer->where_firstchar);
        jkc99_assert((ctx->lexer->where_lastchar - ctx->lexer->where_firstchar) < INT_MAX);
        int len = (int)(ctx->lexer->where_lastchar - ctx->lexer->where_firstchar) + 1;

        snprintf(got, sizeof(got), ", got %.*s", len, ctx->lexer->where_firstchar);

        switch(type) {
            case CLEX_intlit:       jkc99_log_error(ctx, "Expected int literal%s", got);        break;
            case CLEX_floatlit:     jkc99_log_error(ctx, "Expected float literal%s", got);      break;
            case CLEX_id:           jkc99_log_error(ctx, "Expected identifier%s", got);         break;
            case CLEX_dqstring:     jkc99_log_error(ctx, "Expected string%s", got);             break;
            case CLEX_charlit:      jkc99_log_error(ctx, "Expected character literal%s", got);  break;
            case CLEX_eq:           jkc99_log_error(ctx, "Expected ==%s", got);                 break;
            case CLEX_noteq:        jkc99_log_error(ctx, "Expected !=%s", got);                 break;
            case CLEX_lesseq:       jkc99_log_error(ctx, "Expected <=%s", got);                 break;
            case CLEX_greatereq:    jkc99_log_error(ctx, "Expected >=%s", got);                 break;
            case CLEX_andand:       jkc99_log_error(ctx, "Expected &&%s", got);                 break;
            case CLEX_oror:         jkc99_log_error(ctx, "Expected ||%s", got);                 break;
            case CLEX_shl:          jkc99_log_error(ctx, "Expected <<%s", got);                 break;
            case CLEX_shr:          jkc99_log_error(ctx, "Expected >>%s", got);                 break;
            case CLEX_plusplus:     jkc99_log_error(ctx, "Expected ++%s", got);                 break;
            case CLEX_minusminus:   jkc99_log_error(ctx, "Expected --%s", got);                 break;
            case CLEX_pluseq:       jkc99_log_error(ctx, "Expected +=%s", got);                 break;
            case CLEX_minuseq:      jkc99_log_error(ctx, "Expected -=%s", got);                 break;
            case CLEX_muleq:        jkc99_log_error(ctx, "Expected *=%s", got);                 break;
            case CLEX_diveq:        jkc99_log_error(ctx, "Expected /=%s", got);                 break;
            case CLEX_modeq:        jkc99_log_error(ctx, "Expected %=%s", got);                 break;
            case CLEX_andeq:        jkc99_log_error(ctx, "Expected &=%s", got);                 break;
            case CLEX_oreq:         jkc99_log_error(ctx, "Expected |=%s", got);                 break;
            case CLEX_xoreq:        jkc99_log_error(ctx, "Expected ^=%s", got);                 break;
            case CLEX_arrow:        jkc99_log_error(ctx, "Expected ->%s", got);                 break;
            case CLEX_shleq:        jkc99_log_error(ctx, "Expected <<=%s", got);                break;
            case CLEX_shreq:        jkc99_log_error(ctx, "Expected >>=%s", got);                break;
            default: 
                {
                    jkc99_assert(type < 256);
                    jkc99_log_error(ctx, "Expected %c%s", (char) type, got);
                } break;
        }

        unsigned int scope = 0;

        do {
            if(ctx->lexer->token == '{') {
                scope++;
            } else if(ctx->lexer->token == '}') {
                if(scope) {
                    scope--;
                } else {
                    break;
                }
            }
            stb_c_lexer_get_token(ctx->lexer);
        } while(!(ctx->lexer->token == type || ctx->lexer->token == CLEX_eof));
    }
    return false;
}

JKC99_API bool jkc99_lexer_require_id(ParseContext *ctx, const char *id) {
    if(ctx->lexer->token == CLEX_id && jkc99_str_intern(ctx, ctx->lexer->string) == id) {
        jkc99_lexer_next(ctx);
        return true;
    } else {
        jkc99_log_error(ctx, "Expected %s", id);
    }
    return false;
}

static void hook_execute(ParseContext *ctx, HookKind kind, void *ptr) {
    size_t i;
    jkc99_assert(kind < kHookCount);

    for(i = 0; i < da_count(ctx->hooks[kind]); ++i) {
        Hook *h = ctx->hooks[kind] + i;
        if(h->func(ctx, h->ptr, ptr)) {
            break;
        }
    }
}

#if HAS___ATTRIBUTE__
JKC99_API bool jkc99_parse_attribute(ParseContext *ctx, Attribute *attr) {
    Source src;

    jkc99_lexer_source(ctx, &src);

    if(jkc99_lexer_match_kw(ctx, __attribute__)) {
        *attr = (Attribute){0};
        attr->strBegin = ctx->lexer->where_firstchar;
        //TODO Proper parsing. For now we just attempt to skip the attribute list.
        if(jkc99_lexer_match(ctx, '(')) {
            unsigned int parenCount = 1;
            while(parenCount && !jkc99_lexer_match(ctx, CLEX_eof)) {
                attr->strEnd = ctx->lexer->where_firstchar+1;
                if(jkc99_lexer_match(ctx,'(')) {
                    parenCount++;
                } else if(jkc99_lexer_match(ctx,')')) {
                    parenCount--;
                } else {
                    jkc99_lexer_next(ctx);
                }
            }
        }
        jkc99_assert(attr->strEnd > attr->strBegin);
        source_end(ctx, &src);
        attr->src = src;
        jkc99_parse_empty_directives(ctx, attr);
        return true;
    }
    return false;
}

static inline int jkc99_parse_attribute_list_(ParseContext *ctx, Attribute **list, size_t *count) {
    Attribute attr;
    size_t preCount = da_count(*list);

    while(jkc99_parse_attribute(ctx, &attr)) {
        da_push(*list, attr);
    }
    *count = da_count(*list);

    return (*count - preCount);
}

JKC99_API Attribute *jkc99_parse_attribute_list(ParseContext *ctx, size_t *count) {
    Attribute *list = NULL;

    jkc99_parse_attribute_list_(ctx, &list, count);

    return list;
}

#else
#define jkc99_parse_attribute(ctx,attr) false
#define jkc99_parse_attribute_list_(ctx,list,count) 0
#define jkc99_parse_attribute_list(ctx,count) NULL
#endif

JKC99_API Stmt *jkc99_stmt_alloc(ParseContext *ctx, Source *src, StmtKind kind) {
    Stmt *stmt = arena_alloc(&ctx->arena, sizeof(Stmt));
    stmt->kind = kind;
    stmt->src = src ? *src : (Source){0};
    jkc99_parse_empty_directives(ctx, stmt);
    return stmt;
}

JKC99_API Stmt *jkc99_stmt_label(ParseContext *ctx, Source *src, size_t attrCount, Attribute *attrs, const char *label, Stmt *stmt) {
    Stmt *res = jkc99_stmt_alloc(ctx, src, kStmtLabeled);
    res->u.l = (StmtLabeled) {
        .kind = kStmtLabeledLabel,
        .stmt = stmt,
        .u.label = {
            .label = label,
            .attributeCount = attrCount,
            .attributes = attrs
        }
    };

    return res;
}

JKC99_API Stmt *jkc99_stmt_case(ParseContext *ctx, Source *src, Expr *expr, Stmt *stmt) {
    Stmt *res = jkc99_stmt_alloc(ctx, src, kStmtLabeled);
    res->u.l = (StmtLabeled) {
        .kind = kStmtLabeledCase,
        .stmt = stmt,
        .u.caseExpr = expr
    };

    return res;
}

JKC99_API Stmt *jkc99_stmt_default(ParseContext *ctx, Source *src, Stmt *stmt) {
    Stmt *res = jkc99_stmt_alloc(ctx, src, kStmtLabeled);
    res->u.l = (StmtLabeled) {
        .kind = kStmtLabeledDefault,
        .stmt = stmt
    };

    return res;
}

JKC99_API Stmt *jkc99_stmt_compound(ParseContext *ctx, Source *src, size_t count, BlockItem *items) {
    Stmt *res = jkc99_stmt_alloc(ctx, src, kStmtCompound);
    res->u.c = (StmtCompound) {
        .count = count,
        .items = items
    };

    return res;
}

JKC99_API Stmt *jkc99_stmt_compound_push(UNUSED ParseContext *ctx, Stmt *compound, BlockItem blockItem) {
    da_push(compound->u.c.items, blockItem);
    compound->u.c.count = da_count(compound->u.c.items);

    return compound;
}

JKC99_API Stmt *jkc99_stmt_expr(ParseContext *ctx, Source *src, size_t attributeCount, Attribute *attributes, Expr *expr) {
    Stmt *res = jkc99_stmt_alloc(ctx, src, kStmtExpression);
    res->u.e.expr = expr;
    res->u.e.attributeCount = attributeCount;
    res->u.e.attributes = attributes;

    jkc99_assert(!attributeCount || !expr);

    return res;
}

JKC99_API Stmt *jkc99_stmt_if(ParseContext *ctx, Source *src, Expr *expr, Stmt *stmt, Stmt *elseStmt) {
    Stmt *res = jkc99_stmt_alloc(ctx, src, kStmtSelection);
    res->u.s = (StmtSelection) {
        .kind = kStmtSelectionIf,
        .expr = expr,
        .stmt = stmt,
        .elseStmt = elseStmt
    };

    return res;
}

JKC99_API Stmt *jkc99_stmt_switch(ParseContext *ctx, Source *src, Expr *expr, Stmt *stmt) {
    Stmt *res = jkc99_stmt_alloc(ctx, src, kStmtSelection);
    res->u.s = (StmtSelection) {
        .kind = kStmtSelectionSwitch,
        .expr = expr,
        .stmt = stmt
    };

    return res;
}

JKC99_API Stmt *jkc99_stmt_while(ParseContext *ctx, Source *src, Expr *expr, Stmt *stmt) {
    Stmt *res = jkc99_stmt_alloc(ctx, src, kStmtIteration);
    res->u.i = (StmtIteration) {
        .kind = kStmtIterationWhile,
        .condExpr = expr,
        .stmt = stmt
    };

    return res;
}

JKC99_API Stmt *jkc99_stmt_do_while(ParseContext *ctx, Source *src, Expr *expr, Stmt *stmt) {
    Stmt *res = jkc99_stmt_alloc(ctx, src, kStmtIteration);
    res->u.i = (StmtIteration) {
        .kind = kStmtIterationDoWhile,
        .condExpr = expr,
        .stmt = stmt
    };

    return res;
}

JKC99_API Stmt *jkc99_stmt_for(ParseContext *ctx, Source *src, Declaration *initDecl, Expr *initExpr, Expr *condExpr, Expr *iterExpr, Stmt *stmt) {
    Stmt *res = jkc99_stmt_alloc(ctx, src, kStmtIteration);
    res->u.i = (StmtIteration) {
        .kind = kStmtIterationFor,
        .initDecl = initDecl,
        .initExpr = initExpr,
        .condExpr = condExpr,
        .iterExpr = iterExpr,
        .stmt = stmt
    };

    return res;
}

JKC99_API Stmt *jkc99_stmt_goto(ParseContext *ctx, Source *src, const char *identifier) {
    Stmt *res = jkc99_stmt_alloc(ctx, src, kStmtJump);
    res->u.j = (StmtJump) {
        .kind = kStmtJumpGoto,
        .u.identifier = identifier
    };

    return res;
}

JKC99_API Stmt *jkc99_stmt_continue(ParseContext *ctx, Source *src) {
    Stmt *res = jkc99_stmt_alloc(ctx, src, kStmtJump);
    res->u.j = (StmtJump) {
        .kind = kStmtJumpContinue
    };

    return res;
}

JKC99_API Stmt *jkc99_stmt_break(ParseContext *ctx, Source *src) {
    Stmt *res = jkc99_stmt_alloc(ctx, src, kStmtJump);
    res->u.j = (StmtJump) {
        .kind = kStmtJumpBreak
    };

    return res;
}

JKC99_API Stmt *jkc99_stmt_return(ParseContext *ctx, Source *src, Expr *expr) {
    Stmt *res = jkc99_stmt_alloc(ctx, src, kStmtJump);
    res->u.j = (StmtJump) {
        .kind = kStmtJumpReturn,
        .u.returnExpr = expr
    };

    return res;
}

#if HAS_GCC_ASM
JKC99_API Stmt *jkc99_stmt_asm_basic(ParseContext *ctx, Source *src, int qualifiers, const char *instructions) {
    Stmt *res = jkc99_stmt_alloc(ctx, src, kStmtAsm);
    res->u.a = (StmtAsm) {
        .kind = kStmtAsmBasic,
        .qualifiers = qualifiers,
        .instructions = instructions
    };
    return res;
}

JKC99_API Stmt *jkc99_stmt_asm_extended(ParseContext *ctx, Source *src, int qualifiers, const char *instructions, StmtAsmOutputOperand *outputOperands, StmtAsmInputOperand *inputOperands, StmtAsmClobber *clobbers, const char **gotoLabels) {
    Stmt *res = jkc99_stmt_alloc(ctx, src, kStmtAsm);
    res->u.a = (StmtAsm) {
        .kind = kStmtAsmExtended,
        .qualifiers = qualifiers,
        .instructions = instructions,
        .outputOperandCount = da_count(outputOperands),
        .outputOperands = outputOperands,
        .inputOperandCount = da_count(inputOperands),
        .inputOperands = inputOperands,
        .clobberCount = da_count(clobbers),
        .clobbers = clobbers,
        .gotoLabelCount = da_count(gotoLabels),
        .gotoLabels = gotoLabels
    };
    return res;
}
#endif

JKC99_API Stmt *jkc99_parse_stmt_compound(ParseContext *ctx) {
    Source src;
    Stmt *stmt = NULL;

    jkc99_lexer_source(ctx, &src);

    if(jkc99_lexer_match(ctx, '{')) {
        BlockItem *items = NULL;

        jkc99_scope_push(ctx);

        while(!(jkc99_lexer_is(ctx, '}') || jkc99_lexer_is(ctx, CLEX_eof))) {
            BlockItem item = {0};
            item.decl = jkc99_parse_declaration(ctx);
            if(!item.decl) {
                item.stmt = jkc99_parse_stmt(ctx);
                if(!item.stmt) {
                    break;
                }
            }
            da_push(items, item);
        }

        stmt = jkc99_stmt_compound(ctx, &src, da_count(items), items);
        if(jkc99_lexer_require(ctx, '}')) {
            jkc99_scope_pop(ctx);
        }
    }

    return stmt;
}

#if HAS___DECLSPEC
/* https://docs.microsoft.com/en-us/cpp/cpp/declspec?view=vs-2019 */
JKC99_API int jkc99_parse_declspec(ParseContext *ctx, Declspec *ds) {
    Source src = {0};
    jkc99_assert(ds && ds->modifierCount == 0 && ds->modifiers == NULL);

    jkc99_lexer_source(ctx, &src);
    if(jkc99_lexer_match_kw(ctx, __declspec)) {
        if(jkc99_lexer_require(ctx, '(')) {
            while(!(jkc99_lexer_match(ctx, ')') || jkc99_lexer_match(ctx, CLEX_eof))) {
                DeclspecModifier mod = {0};
                if(jkc99_lexer_match_kw(ctx, align)) {
                    mod.kind = kDeclspecModifier_align;
                    if(jkc99_lexer_require(ctx, '(')) {
                        if(jkc99_lexer_is(ctx, CLEX_intlit)) {
                            mod.u.align = ctx->lexer->int_number;
                            jkc99_lexer_next(ctx);
                        } else {
                            jkc99_log_error(ctx, "Expected int literal for __declspec(align)");
                            jkc99_lexer_next(ctx);
                        }
                        jkc99_lexer_require(ctx, ')');
                    }
                } else if(jkc99_lexer_match_kw(ctx, allocate)) {
                    mod.kind = kDeclspecModifier_allocate;
                    if(jkc99_lexer_require(ctx, '(')) {
                        const char *str = jkc99_parse_string(ctx);
                        if(str) {
                            mod.u.segname = str;
                        } else {
                            jkc99_log_error(ctx, "Expected string segname for __declspec(allocate)");
                        }
                        jkc99_lexer_require(ctx, ')');
                    }
                } else if(jkc99_lexer_match_kw(ctx, allocator)) {
                    mod.kind = kDeclspecModifier_allocator;
                } else if(jkc99_lexer_match_kw(ctx, appdomain)) {
                    mod.kind = kDeclspecModifier_appdomain;
                } else if(jkc99_lexer_match_kw(ctx, code_seg)) {
                    mod.kind = kDeclspecModifier_code_seg;
                    if(jkc99_lexer_require(ctx, '(')) {
                        const char *str = jkc99_parse_string(ctx);
                        if(str) {
                            mod.u.segname = str;
                        } else {
                            jkc99_log_error(ctx, "Expected string segname for __declspec(code_seg)");
                        }
                        jkc99_lexer_require(ctx, ')');
                    }
                } else if(jkc99_lexer_match_kw(ctx, deprecated)) {
                    mod.kind = kDeclspecModifier_deprecated;
                    if(jkc99_lexer_match(ctx, '(')) {
                        mod.u.deprecated = jkc99_parse_string(ctx);
                        jkc99_lexer_require(ctx, ')');
                    }
                } else if(jkc99_lexer_match_kw(ctx, dllimport)) {
                    mod.kind = kDeclspecModifier_dllimport;
                } else if(jkc99_lexer_match_kw(ctx, dllexport)) {
                    mod.kind = kDeclspecModifier_dllexport;
                } else if(jkc99_lexer_match_kw(ctx, jitintrinsic)) {
                    mod.kind = kDeclspecModifier_jitintrisic;
                } else if(jkc99_lexer_match_kw(ctx, naked)) {
                    mod.kind = kDeclspecModifier_naked;
                } else if(jkc99_lexer_match_kw(ctx, noalias)) {
                    mod.kind = kDeclspecModifier_noalias;
                } else if(jkc99_lexer_match_kw(ctx, noinline)) {
                    mod.kind = kDeclspecModifier_noinline;
                } else if(jkc99_lexer_match_kw(ctx, noreturn)) {
                    mod.kind = kDeclspecModifier_noreturn;
                } else if(jkc99_lexer_match_kw(ctx, nothrow)) {
                    mod.kind = kDeclspecModifier_nothrow;
                } else if(jkc99_lexer_match_kw(ctx, novtable)) {
                    mod.kind = kDeclspecModifier_novtable;
                } else if(jkc99_lexer_match_kw(ctx, process)) {
                    mod.kind = kDeclspecModifier_process;
                } else if(jkc99_lexer_match_kw(ctx, property)) {
                    mod.kind = kDeclspecModifier_property;
                    if(jkc99_lexer_require(ctx, '(')) {
                        if(jkc99_lexer_match_kw(ctx, get)) {
                            jkc99_lexer_require(ctx, '=');
                            mod.u.property.get = jkc99_parse_identifier(ctx);
                            if(jkc99_lexer_match(ctx, ',')) {
                                if(jkc99_lexer_match_kw(ctx, put)) {
                                    jkc99_lexer_require(ctx, '=');
                                    mod.u.property.put = jkc99_parse_identifier(ctx);
                                }
                            }
                        } else if(jkc99_lexer_match_kw(ctx, put)) {
                            jkc99_lexer_require(ctx, '=');
                            mod.u.property.put = jkc99_parse_identifier(ctx);
                        } else {
                            jkc99_log_error(ctx, "Expected either get or put for __declspec(property)");
                        }
                        jkc99_lexer_require(ctx, ')');
                    }
                } else if(jkc99_lexer_match_kw(ctx, restrict)) {
                    mod.kind = kDeclspecModifier_restrict;
                } else if(jkc99_lexer_match_kw(ctx, safebuffers)) {
                    mod.kind = kDeclspecModifier_safebuffers;
                } else if(jkc99_lexer_match_kw(ctx, selectany)) {
                    mod.kind = kDeclspecModifier_selectany;
                } else if(jkc99_lexer_match_kw(ctx, spectre)) {
                    mod.kind = kDeclspecModifier_spectre;
                    if(jkc99_lexer_require(ctx, '(')) {
                        jkc99_lexer_require_kw(ctx, nomitigation);
                        jkc99_lexer_require(ctx, ')');
                    }
                } else if(jkc99_lexer_match_kw(ctx, thread)) {
                    mod.kind = kDeclspecModifier_thread;
                } else if(jkc99_lexer_match_kw(ctx, uuid)) {
                    mod.kind = kDeclspecModifier_uuid;
                    if(jkc99_lexer_require(ctx, '(')) {
                        mod.u.uuid = jkc99_parse_string(ctx);
                        jkc99_lexer_require(ctx, ')');
                    }
                } else {
                    jkc99_log_error(ctx, "Unknown __declspec modifier");
                    jkc99_lexer_next(ctx);
                }
                if(mod.kind) {
                    da_push(ds->modifiers, mod);
                } else {
                    jkc99_assert(false);
                }
            }
            ds->modifierCount = da_count(ds->modifiers);
        }
        source_end(ctx, &src);
        ds->src = src;
        jkc99_parse_empty_directives(ctx, ds);
    }

    return ds->modifierCount;
}

static int jkc99_parse_declspec_list_(ParseContext *ctx, Declspec **list, size_t *count) {
    Declspec ds = {0};
    size_t preCount = da_count(*list);
    while(jkc99_parse_declspec(ctx, &ds)) {
        da_push(*list, ds);
        ds = (Declspec){0};
    }
    *count = da_count(*list);
    jkc99_assert(*count >= preCount);
    return (*count - preCount);
}
#else
#define jkc99_parse_declspec(ctx) 0
#define jkc99_parse_declspec_list_(ctx) 0
#endif

#if HAS___EXTENSION__
JKC99_API int jkc99_parse_extension(ParseContext *ctx) {
    if(jkc99_lexer_match_kw(ctx, __extension__)) {
        return 1;
    }
    return 0;
}
#else
#define jkc99_parse_extension(ctx) 0
#endif

#if HAS_GCC_ASM
JKC99_API StmtAsmOutputOperand *jkc99_parse_asm_output_operands(ParseContext *ctx) {
    StmtAsmOutputOperand *operands = NULL;

    do {
        StmtAsmOutputOperand operand = {0};
        if(jkc99_lexer_is(ctx, '[')) {
            operand.symbolicName = jkc99_parse_identifier(ctx);
            jkc99_lexer_require(ctx, ']');
        }
        operand.constraints = jkc99_parse_string(ctx);
        if(operand.constraints) {
            if(jkc99_lexer_require(ctx, '(')) {
                //TODO Make sure expression is lvalue
                operand.expr = jkc99_parse_expr(ctx);
                jkc99_lexer_require(ctx, ')');
            }
            da_push(operands, operand);
        } else {
            jkc99_assert(operands == NULL); //TODO Not sure whether trailing comma is valid syntax
            break;
        }
    } while(jkc99_lexer_match(ctx, ','));

    return operands;
}

JKC99_API StmtAsmInputOperand *jkc99_parse_asm_input_operands(ParseContext *ctx) {
    StmtAsmInputOperand *operands = NULL;

    do {
        StmtAsmInputOperand operand = {0};
        if(jkc99_lexer_is(ctx, '[')) {
            operand.symbolicName = jkc99_parse_identifier(ctx);
            jkc99_lexer_require(ctx, ']');
        }
        operand.constraints = jkc99_parse_string(ctx);
        if(operand.constraints) {
            if(jkc99_lexer_require(ctx, '(')) {
                operand.expr = jkc99_parse_expr(ctx);
                jkc99_lexer_require(ctx, ')');
            }
            da_push(operands, operand);
        } else {
            jkc99_assert(operands == NULL); //TODO Not sure whether trailing comma is valid syntax
            break;
        }
    } while(jkc99_lexer_match(ctx, ','));

    return operands;
}

JKC99_API StmtAsmClobber *jkc99_parse_asm_clobbers(ParseContext *ctx) {
    StmtAsmClobber *clobbers = NULL;

    do {
        StmtAsmClobber clobber = jkc99_parse_string(ctx);
        if(clobber) {
            da_push(clobbers, clobber);
        } else {
            break;
        }
    } while(jkc99_lexer_match(ctx, ','));

    return clobbers;
}

JKC99_API Stmt *jkc99_parse_stmt_asm(ParseContext *ctx) {
    Source src;
    Stmt *stmt = NULL;

    jkc99_lexer_source(ctx, &src);

    if(jkc99_lexer_match_kw(ctx, __asm__)) {
        StmtAsmKind kind = kStmtAsmBasic;
        int qualifiers = 0;
        const char *instructions = NULL;
        StmtAsmOutputOperand *outputOperands = NULL;
        StmtAsmInputOperand *inputOperands = NULL;
        StmtAsmClobber *clobbers = NULL;
        const char **gotoLabels = NULL;

        while(1) {
            if(jkc99_lexer_match_kw(ctx, __volatile__) || jkc99_lexer_match_kw(ctx, volatile)) {
                qualifiers |= kStmtAsmQualifierVolatile;
            } else if(jkc99_lexer_match_kw(ctx, __inline__) || jkc99_lexer_match_kw(ctx, inline)) {
                qualifiers |= kStmtAsmQualifierInline;
            } else if(jkc99_lexer_match_kw(ctx, __goto__) || jkc99_lexer_match_kw(ctx, goto)) {
                qualifiers |= kStmtAsmQualifierGoto;
            } else {
                break;
            }
        }

        if(jkc99_lexer_match(ctx, '(')) {
            instructions = jkc99_parse_string(ctx);
            jkc99_assert(instructions);
            if(qualifiers & kStmtAsmQualifierGoto) {
                kind = kStmtAsmExtended;
                if(jkc99_lexer_match(ctx, ':')) {
                    jkc99_lexer_require(ctx, ':');
                    inputOperands = jkc99_parse_asm_input_operands(ctx);
                    jkc99_lexer_require(ctx, ':');
                    clobbers = jkc99_parse_asm_clobbers(ctx);
                    jkc99_lexer_require(ctx, ':');
                    do {
                        const char *label = jkc99_parse_identifier(ctx);
                        if(label) {
                            da_push(gotoLabels, label);
                        } else {
                            break;
                        }
                    } while(jkc99_lexer_match(ctx, ','));
                }
            } else if(jkc99_lexer_match(ctx, ':')) {
                kind = kStmtAsmExtended;
                outputOperands = jkc99_parse_asm_output_operands(ctx);
                if(jkc99_lexer_match(ctx, ':')) {
                    inputOperands = jkc99_parse_asm_input_operands(ctx);
                    if(jkc99_lexer_match(ctx, ':')) {
                        clobbers = jkc99_parse_asm_clobbers(ctx);
                    }
                }
            }
            jkc99_lexer_require(ctx, ')');
        }

        if(kind == kStmtAsmBasic) {
            stmt = jkc99_stmt_asm_basic(ctx, &src, qualifiers, instructions);
        } else {
            jkc99_assert(kind == kStmtAsmExtended);
            stmt = jkc99_stmt_asm_extended(ctx, &src, qualifiers, instructions, outputOperands, inputOperands, clobbers, gotoLabels);
        }
    }

    return stmt;
}
#endif

JKC99_API Stmt *jkc99_parse_stmt(ParseContext *ctx) {
    Source src;
    Stmt *stmt = NULL;

    jkc99_lexer_source(ctx, &src);

    /* TODO */
    jkc99_parse_extension(ctx);
    if(jkc99_lexer_is(ctx, '{')) {
        stmt = jkc99_parse_stmt_compound(ctx);
    } else if(jkc99_lexer_match_kw(ctx, if)) {
        if(jkc99_lexer_require(ctx, '(')) {
            Expr *expr;
            Stmt *ifStmt;
            Stmt *elseStmt = NULL;

            expr = jkc99_parse_expr(ctx);
            jkc99_lexer_require(ctx, ')');
            ifStmt = jkc99_parse_stmt(ctx);
            if(jkc99_lexer_match_kw(ctx, else)) {
                elseStmt = jkc99_parse_stmt(ctx);
            }

            stmt = jkc99_stmt_if(ctx, &src, expr, ifStmt, elseStmt);
        } else {
            jkc99_assert(false); //TODO
        }
    } else if(jkc99_lexer_match_kw(ctx, switch)) {
        if(jkc99_lexer_require(ctx, '(')) {
            Expr *expr;

            expr = jkc99_parse_expr(ctx);
            jkc99_lexer_require(ctx, ')');
            stmt = jkc99_stmt_switch(ctx, &src, expr, jkc99_parse_stmt(ctx));
        } else {
            jkc99_assert(false); //TODO
        }
    } else if(jkc99_lexer_match_kw(ctx, while)) {
        if(jkc99_lexer_require(ctx, '(')) {
            Expr *expr;

            expr = jkc99_parse_expr(ctx);
            jkc99_lexer_require(ctx, ')');
            stmt = jkc99_stmt_while(ctx, &src, expr, jkc99_parse_stmt(ctx));
        } else {
            jkc99_assert(false); //TODO
        }
    } else if(jkc99_lexer_match_kw(ctx, do)) {
        Expr *expr;

        stmt = jkc99_parse_stmt(ctx);
        jkc99_lexer_require_kw(ctx, while);
        jkc99_lexer_require(ctx, '(');
        expr = jkc99_parse_expr(ctx);
        jkc99_lexer_require(ctx, ')');
        jkc99_lexer_require(ctx, ';');

        stmt = jkc99_stmt_do_while(ctx, &src, expr, stmt);
    } else if(jkc99_lexer_match_kw(ctx, for)) {
        if(jkc99_lexer_require(ctx, '(')) {
            Expr *initExpr = NULL;
            Expr *condExpr = NULL;
            Declaration *initDecl = NULL;
            Expr *iterExpr = NULL;

            jkc99_scope_push(ctx);

            initDecl = jkc99_parse_declaration(ctx);
            if(!initDecl) {
                initExpr = jkc99_parse_expr(ctx);
                jkc99_lexer_require(ctx, ';');
            }
            condExpr = jkc99_parse_expr(ctx);
            jkc99_lexer_require(ctx, ';');
            iterExpr = jkc99_parse_expr(ctx);
            jkc99_lexer_require(ctx, ')');
            stmt = jkc99_parse_stmt(ctx);
            
            jkc99_scope_pop(ctx);

            stmt = jkc99_stmt_for(ctx, &src, initDecl, initExpr, condExpr, iterExpr, stmt);
        } else {
            jkc99_assert(false); //TODO
        }
    } else if(jkc99_lexer_match_kw(ctx, goto)) {
        stmt = jkc99_stmt_goto(ctx, &src, jkc99_parse_identifier(ctx));
        jkc99_lexer_require(ctx, ';');
    } else if(jkc99_lexer_match_kw(ctx, continue)) {
        stmt = jkc99_stmt_continue(ctx, &src);
        jkc99_lexer_require(ctx, ';');
    } else if(jkc99_lexer_match_kw(ctx, break)) {
        stmt = jkc99_stmt_break(ctx, &src);
        jkc99_lexer_require(ctx, ';');
    } else if(jkc99_lexer_match_kw(ctx, return)) {
        stmt = jkc99_stmt_return(ctx, &src, jkc99_parse_expr(ctx));
        jkc99_lexer_require(ctx, ';');
    } else if(jkc99_lexer_match_kw(ctx, case)) {
        Expr *expr;

        expr = jkc99_parse_expr(ctx);
        jkc99_lexer_require(ctx, ':');
        stmt = jkc99_parse_stmt(ctx);
        stmt = jkc99_stmt_case(ctx, &src, expr, stmt);
    } else if(jkc99_lexer_match_kw(ctx, default)) {
        jkc99_lexer_require(ctx, ':');
        stmt = jkc99_parse_stmt(ctx);
        stmt = jkc99_stmt_default(ctx, &src, stmt);
#if HAS_GCC_ASM
    } else if(jkc99_lexer_is_kw(ctx, __asm__)) {
        stmt = jkc99_parse_stmt_asm(ctx);
        jkc99_lexer_require(ctx, ';');
#endif
    } else if(jkc99_lexer_is_kw(ctx, __attribute__)) {
        Attribute *attrs = NULL;
        size_t attrCount = 0;
        attrs = jkc99_parse_attribute_list(ctx, &attrCount);
        stmt = jkc99_stmt_expr(ctx, &src, attrCount, attrs, NULL);
        jkc99_lexer_require(ctx, ';');
    } else {
        Expr *expr;
        expr = jkc99_parse_expr(ctx);
        if(expr && expr->kind == kExprPrimary && expr->u.primary.kind == kExprPrimaryIdentifier && jkc99_lexer_match(ctx, ':')) {
            Attribute *attrs = NULL;
            size_t attrCount = 0;
            attrs = jkc99_parse_attribute_list(ctx, &attrCount);
            stmt = jkc99_parse_stmt(ctx);
            stmt = jkc99_stmt_label(ctx, &src, attrCount, attrs, expr->u.primary.u.identifier, stmt);
        } else {
            stmt = jkc99_stmt_expr(ctx, &src, 0, NULL, expr);
            jkc99_lexer_require(ctx, ';');
        }
    }

    return stmt;
}

static Expr *expr_result_to_expr(ParseContext *ctx, ExprResult *res) {
    /* TODO Preserve source from original expr */
    Source src = {0};

    Expr *expr = NULL;
    if(EXPR_RESULT_SUCCESS(res)) {
        const char *str = NULL;
        if(JKC99_TYPE_HANDLE_EQ(res->type, ctx->type__Bool)) {
            expr = jkc99_expr_int(ctx, &src, str, res->u._Bool_val, kConstantDecimal, 0, "");
        } else if(JKC99_TYPE_HANDLE_EQ(res->type, ctx->type_char)) {
            expr = jkc99_expr_char(ctx, &src, res->u.char_val);
        } else if(JKC99_TYPE_HANDLE_EQ(res->type, ctx->type_signed_char)) {
            expr = jkc99_expr_int(ctx, &src, str, res->u.signed_char_val, kConstantDecimal, 0, "");
        } else if(JKC99_TYPE_HANDLE_EQ(res->type, ctx->type_signed_short)) {
            expr = jkc99_expr_int(ctx, &src, str, res->u.signed_short_val, kConstantDecimal, 0, "");
        } else if(JKC99_TYPE_HANDLE_EQ(res->type, ctx->type_signed_int)) {
            expr = jkc99_expr_int(ctx, &src, str, res->u.signed_int_val, kConstantDecimal, 0, "");
        } else if(JKC99_TYPE_HANDLE_EQ(res->type, ctx->type_signed_long)) {
            expr = jkc99_expr_int(ctx, &src, str, res->u.signed_long_val, kConstantDecimal, 1, "L");
        } else if(JKC99_TYPE_HANDLE_EQ(res->type, ctx->type_signed_long_long)) {
            expr = jkc99_expr_int(ctx, &src, str, res->u.signed_long_long_val, kConstantDecimal, 2, "LL");
        } else if(JKC99_TYPE_HANDLE_EQ(res->type, ctx->type_unsigned_char)) {
            expr = jkc99_expr_int(ctx, &src, str, res->u.unsigned_char_val, kConstantDecimal, 1, "U");
        } else if(JKC99_TYPE_HANDLE_EQ(res->type, ctx->type_unsigned_short)) {
            expr = jkc99_expr_int(ctx, &src, str, res->u.unsigned_short_val, kConstantDecimal, 1, "U");
        } else if(JKC99_TYPE_HANDLE_EQ(res->type, ctx->type_unsigned_int)) {
            expr = jkc99_expr_int(ctx, &src, str, res->u.unsigned_int_val, kConstantDecimal, 1, "U");
        } else if(JKC99_TYPE_HANDLE_EQ(res->type, ctx->type_unsigned_long)) {
            expr = jkc99_expr_int(ctx, &src, str, res->u.unsigned_long_val, kConstantDecimal, 2, "UL");
        } else if(JKC99_TYPE_HANDLE_EQ(res->type, ctx->type_unsigned_long_long)) {
            expr = jkc99_expr_int(ctx, &src, str, res->u.unsigned_long_long_val, kConstantDecimal, 3, "ULL");
        } else if(JKC99_TYPE_HANDLE_EQ(res->type, ctx->type_float)) {
            expr = jkc99_expr_float(ctx, &src, str, res->u.float_val, kConstantDecimal, 1, "F");
        } else if(JKC99_TYPE_HANDLE_EQ(res->type, ctx->type_double)) {
            expr = jkc99_expr_float(ctx, &src, str, res->u.double_val, kConstantDecimal, 0, "");
        } else if(JKC99_TYPE_HANDLE_EQ(res->type, ctx->type_long_double)) {
            expr = jkc99_expr_float(ctx, &src, str, res->u.long_double_val, kConstantDecimal, 1, "L");
        } else {
            jkc99_assert(false);
        }
    } else {
        jkc99_assert(EXPR_RESULT_UNABLE(res));
        expr = res->u.expr;
    }
    return expr;
}

static inline _Bool eval_scalar_to_bool(ParseContext *ctx, ExprResult res) {
    if(EXPR_RESULT_SUCCESS(&res) && JKC99_TYPE_HANDLE_NOT_INVALID(res.type)) {
        JKC99Type t;
        jkc99_type_get(ctx, res.type, &t);
        switch(t.kind) {
            case kType_Bool:                return res.u._Bool_val;                break;
            case kTypeChar:                 return res.u.char_val;                 break;
            case kTypeSignedChar:           return res.u.signed_char_val;          break;
            case kTypeSignedShort:          return res.u.signed_short_val;         break;
            case kTypeEnum:
            case kTypeSignedInt:            return res.u.signed_int_val;           break;
            case kTypeSignedLong:           return res.u.signed_long_val;          break;
            case kTypeSignedLongLong:       return res.u.signed_long_long_val;     break;
            case kTypeUnsignedChar:         return res.u.unsigned_char_val;        break;
            case kTypeUnsignedShort:        return res.u.unsigned_short_val;       break;
            case kTypeUnsignedInt:          return res.u.unsigned_int_val;         break;
            case kTypeUnsignedLong:         return res.u.unsigned_long_val;        break;
            case kTypeUnsignedLongLong:     return res.u.unsigned_long_long_val;   break;
            case kTypeFloat:                return res.u.float_val;                break;
            case kTypeDouble:               return res.u.double_val;               break;
            case kTypeLongDouble:           return res.u.long_double_val;          break;
            case kTypeFloat_Complex:        return float_complex_to_bool(res.u.float_complex_val);       break;
            case kTypeDouble_Complex:       return double_complex_to_bool(res.u.double_complex_val);      break;
            case kTypeLongDouble_Complex:   return long_double_complex_to_bool(res.u.long_double_complex_val); break;
            case kTypeArray:                return res.u.ptr;                      break;
            case kTypePointer:              return res.u.ptr;                      break;
            default:                        jkc99_assert(false);                      break;
        }
    } else {
        jkc99_assert(false);
    }
    return false;
}

#define EVAL_CMP_INNER(rtype,res,op,l,r)                                            \
            if(JKC99_TYPE_HANDLE_EQ((l).type, ctx->type_##rtype)) {                       \
                (res)->u.signed_int_val = (l).u.rtype##_val op (r).u.rtype##_val;   \
            }

//TODO Extended types
//TODO Complex types
#define EVAL_CMP(res,op,l,r)                                                    \
        {                                                                       \
            eval_convert_arithmetic(ctx, &(l), &(r));                           \
            EXPR_RESULT_SET_SUCCESS((res));                                     \
            (res)->type = ctx->type_signed_int;                                 \
            jkc99_assert(JKC99_TYPE_HANDLE_EQ((l).type, (r).type));                      \
            EVAL_CMP_INNER(_Bool,res,op,l,r)                                    \
            else EVAL_CMP_INNER(char,res,op,l,r)                                \
            else EVAL_CMP_INNER(signed_char,res,op,l,r)                         \
            else EVAL_CMP_INNER(signed_short,res,op,l,r)                        \
            else EVAL_CMP_INNER(signed_int,res,op,l,r)                          \
            else EVAL_CMP_INNER(signed_long,res,op,l,r)                         \
            else EVAL_CMP_INNER(signed_long_long,res,op,l,r)                    \
            else EVAL_CMP_INNER(unsigned_char,res,op,l,r)                       \
            else EVAL_CMP_INNER(unsigned_short,res,op,l,r)                      \
            else EVAL_CMP_INNER(unsigned_int,res,op,l,r)                        \
            else EVAL_CMP_INNER(unsigned_long,res,op,l,r)                       \
            else EVAL_CMP_INNER(unsigned_long_long,res,op,l,r)                  \
            else EVAL_CMP_INNER(float,res,op,l,r)                               \
            else EVAL_CMP_INNER(double,res,op,l,r)                              \
            else EVAL_CMP_INNER(long_double,res,op,l,r)                         \
            else {                                                              \
                jkc99_assert(false);                                               \
            }                                                                   \
        }

#define EVAL_BINARY_INNER(rtype,res,op,l,r)                                     \
            if(JKC99_TYPE_HANDLE_EQ((l).type, ctx->type_##rtype)) {                   \
                (res)->u.rtype##_val = (l).u.rtype##_val op (r).u.rtype##_val;  \
            }

//TODO Extended types
//TODO Enum
#define EVAL_BINARY_OP_INTEGER(res,op,l,r)                                      \
        {                                                                       \
            eval_convert_arithmetic(ctx, &(l), &(r));                           \
            EXPR_RESULT_SET_SUCCESS((res));                                     \
            (res)->type = (l).type;                                             \
            jkc99_assert(JKC99_TYPE_HANDLE_EQ((l).type, (r).type));                      \
            EVAL_BINARY_INNER(_Bool,res,op,l,r)                                 \
            else EVAL_BINARY_INNER(char,res,op,l,r)                             \
            else EVAL_BINARY_INNER(signed_char,res,op,l,r)                      \
            else EVAL_BINARY_INNER(signed_short,res,op,l,r)                     \
            else EVAL_BINARY_INNER(signed_int,res,op,l,r)                       \
            else EVAL_BINARY_INNER(signed_long,res,op,l,r)                      \
            else EVAL_BINARY_INNER(signed_long_long,res,op,l,r)                 \
            else EVAL_BINARY_INNER(unsigned_char,res,op,l,r)                    \
            else EVAL_BINARY_INNER(unsigned_short,res,op,l,r)                   \
            else EVAL_BINARY_INNER(unsigned_int,res,op,l,r)                     \
            else EVAL_BINARY_INNER(unsigned_long,res,op,l,r)                    \
            else EVAL_BINARY_INNER(unsigned_long_long,res,op,l,r)               \
            else {                                                              \
                jkc99_assert(false);                                               \
            }                                                                   \
        }

//TODO Extended types
//TODO Enum
//TODO Complex types
#define EVAL_BINARY_OP(res,op,l,r)                                              \
        {                                                                       \
            eval_convert_arithmetic(ctx, &(l), &(r));                           \
            EXPR_RESULT_SET_SUCCESS((res));                                     \
            (res)->type = (l).type;                                             \
            jkc99_assert(JKC99_TYPE_HANDLE_EQ((l).type, (r).type));                      \
            EVAL_BINARY_INNER(_Bool,res,op,l,r)                                 \
            else EVAL_BINARY_INNER(char,res,op,l,r)                             \
            else EVAL_BINARY_INNER(signed_char,res,op,l,r)                      \
            else EVAL_BINARY_INNER(signed_short,res,op,l,r)                     \
            else EVAL_BINARY_INNER(signed_int,res,op,l,r)                       \
            else EVAL_BINARY_INNER(signed_long,res,op,l,r)                      \
            else EVAL_BINARY_INNER(signed_long_long,res,op,l,r)                 \
            else EVAL_BINARY_INNER(unsigned_char,res,op,l,r)                    \
            else EVAL_BINARY_INNER(unsigned_short,res,op,l,r)                   \
            else EVAL_BINARY_INNER(unsigned_int,res,op,l,r)                     \
            else EVAL_BINARY_INNER(unsigned_long,res,op,l,r)                    \
            else EVAL_BINARY_INNER(unsigned_long_long,res,op,l,r)               \
            else EVAL_BINARY_INNER(float,res,op,l,r)                            \
            else EVAL_BINARY_INNER(double,res,op,l,r)                           \
            else EVAL_BINARY_INNER(long_double,res,op,l,r)                      \
            else {                                                              \
                jkc99_assert(false);                                               \
            }                                                                   \
        }


//TODO Handle error case
//TODO Handle complex types
#define EVAL_CONVERT_TO_INNER(rtype)                                            \
        if(JKC99_TYPE_HANDLE_EQ(r->type, ctx->type_long_double)) {                    \
            r->u.rtype##_val = r->u.long_double_val;                            \
        } else if(JKC99_TYPE_HANDLE_EQ(r->type, ctx->type_double)) {                  \
            r->u.rtype##_val = r->u.double_val;                                 \
        } else if(JKC99_TYPE_HANDLE_EQ(r->type, ctx->type_float)) {                   \
            r->u.rtype##_val = r->u.float_val;                                  \
        } else if(JKC99_TYPE_HANDLE_EQ(r->type, ctx->type_char)) {                    \
            r->u.rtype##_val = r->u.char_val;                                   \
        } else if(JKC99_TYPE_HANDLE_EQ(r->type, ctx->type_signed_char)) {             \
            r->u.rtype##_val = r->u.signed_char_val;                            \
        } else if(JKC99_TYPE_HANDLE_EQ(r->type, ctx->type_signed_short)) {            \
            r->u.rtype##_val = r->u.signed_short_val;                           \
        } else if(JKC99_TYPE_HANDLE_EQ(r->type, ctx->type_signed_int)) {              \
            r->u.rtype##_val = r->u.signed_int_val;                             \
        } else if(JKC99_TYPE_HANDLE_EQ(r->type, ctx->type_signed_long)) {             \
            r->u.rtype##_val = r->u.signed_long_val;                            \
        } else if(JKC99_TYPE_HANDLE_EQ(r->type, ctx->type_signed_long_long)) {        \
            r->u.rtype##_val = r->u.signed_long_long_val;                       \
        } else if(JKC99_TYPE_HANDLE_EQ(r->type, ctx->type_unsigned_char)) {           \
            r->u.rtype##_val = r->u.unsigned_char_val;                          \
        } else if(JKC99_TYPE_HANDLE_EQ(r->type, ctx->type_unsigned_short)) {          \
            r->u.rtype##_val = r->u.unsigned_short_val;                         \
        } else if(JKC99_TYPE_HANDLE_EQ(r->type, ctx->type_unsigned_int)) {            \
            r->u.rtype##_val = r->u.unsigned_int_val;                           \
        } else if(JKC99_TYPE_HANDLE_EQ(r->type, ctx->type_unsigned_long)) {           \
            r->u.rtype##_val = r->u.unsigned_long_val;                          \
        } else if(JKC99_TYPE_HANDLE_EQ(r->type, ctx->type_unsigned_long_long)) {      \
            r->u.rtype##_val = r->u.unsigned_long_long_val;                     \
        } else if(JKC99_TYPE_HANDLE_EQ(r->type, ctx->type__Bool)) {                   \
            r->u.rtype##_val = r->u._Bool_val;                                  \
        } else {                                                                \
            jkc99_assert(false);                                                   \
            EXPR_RESULT_SET_UNABLE(r);                                          \
            return;                                                             \
        }                                                                       \
        r->type = ctx->type_##rtype

#define EVAL_CONVERT_TO(rtype)                                                  \
    if(JKC99_TYPE_HANDLE_EQ(type, ctx->type_##rtype)) {                               \
        EVAL_CONVERT_TO_INNER(rtype);                                           \
    }


static inline void eval_convert_to_type(ParseContext *ctx, ExprResult *r, TypeHandle type) {
    if(EXPR_RESULT_SUCCESS(r) && JKC99_TYPE_HANDLE_NEQ(r->type, type)) {
                EVAL_CONVERT_TO(_Bool)
        else    EVAL_CONVERT_TO(char)
        else    EVAL_CONVERT_TO(signed_char)
        else    EVAL_CONVERT_TO(signed_short)
        else    EVAL_CONVERT_TO(signed_int)
        else    EVAL_CONVERT_TO(signed_long)
        else    EVAL_CONVERT_TO(signed_long_long)
        else    EVAL_CONVERT_TO(unsigned_char)
        else    EVAL_CONVERT_TO(unsigned_short)
        else    EVAL_CONVERT_TO(unsigned_int)
        else    EVAL_CONVERT_TO(unsigned_long)
        else    EVAL_CONVERT_TO(unsigned_long_long)
        else    EVAL_CONVERT_TO(float)
        else    EVAL_CONVERT_TO(double)
        else    EVAL_CONVERT_TO(long_double)
        else {
            jkc99_assert(false);
        }
    }
}

static inline int jkc99_type_conversion_rank(ParseContext *ctx, TypeHandle type) {
    int rank = 0;
    JKC99Type rtype;
    
    jkc99_type_get(ctx, type, &rtype);

    if(rtype.kind >= kTypeFirstIntegerType && rtype.kind <= kTypeLastIntegerType) {
        rank = rtype.u.i.rank;
    } else {
        //TODO Enum
        jkc99_assert(false);
    }

#if 0
    if(type == ctx->type__Bool) {
        rank = 1;
    } else if(type == ctx->type_char || type == ctx->type_signed_char || type == ctx->type_unsigned_char) {
        rank = 2;
    } else if(type == ctx->type_signed_short || type == ctx->type_unsigned_short) {
        rank = 3;
    } else if(type == ctx->type_signed_int || type == ctx->type_unsigned_int) {
        rank = 4;
    } else if(type == ctx->type_signed_long || type == ctx->type_unsigned_long) {
        rank = 5;
    } else if(type == ctx->type_signed_long_long || type == ctx->type_unsigned_long_long) {
        rank = 6;
    } else {

        if(rtype->kind == kTypeSignedExtended || rtype->kind == kTypeUnsignedExtended) {
            rank = rtype.u.i.rank;
        } else {
            //TODO Enum
            jkc99_assert(false);
        }
    }
#endif

    return rank;
}

static inline void eval_integer_promotion(ParseContext *ctx, ExprResult *r) {
    if(!(JKC99_TYPE_HANDLE_EQ(r->type, ctx->type_signed_int) || JKC99_TYPE_HANDLE_EQ(r->type, ctx->type_unsigned_int))) {
        JKC99Type intType, uintType;
        JKC99Type rtype;

        jkc99_type_get(ctx, ctx->type_signed_int, &intType);
        jkc99_type_get(ctx, ctx->type_unsigned_int, &uintType);
        jkc99_type_get(ctx, r->type, &rtype);

        jkc99_assert(intType.u.i.rank == uintType.u.i.rank);

        //TODO A bit field of type _Bool, int, signed int or unsigned int (ISO/IEC 9899:TC3 6.3.1.1)
        if((rtype.kind >= kTypeFirstIntegerType && rtype.kind <= kTypeLastIntegerType)
            &&  rtype.u.i.rank <= intType.u.i.rank) {
            if(rtype.u.i.min >= intType.u.i.min && rtype.u.i.max <= intType.u.i.max) {
                eval_convert_to_type(ctx, r, ctx->type_signed_int);
            } else {
                eval_convert_to_type(ctx, r, ctx->type_unsigned_int);
            }
        }
    }
}

//ISO/IEC 9899:TC3 6.3.1.8
static inline void eval_convert_arithmetic(ParseContext *ctx, ExprResult *l, ExprResult *r) {
    if(EXPR_RESULT_SUCCESS(l) && EXPR_RESULT_SUCCESS(r)) {
        //TODO _Complex types
        if(JKC99_TYPE_HANDLE_EQ(l->type, ctx->type_long_double)) {
            eval_convert_to_type(ctx, r, ctx->type_long_double);
        } else if(JKC99_TYPE_HANDLE_EQ(r->type, ctx->type_long_double)) {
            eval_convert_to_type(ctx, l, ctx->type_long_double);
        } else if(JKC99_TYPE_HANDLE_EQ(l->type, ctx->type_double)) {
            eval_convert_to_type(ctx, r, ctx->type_double);
        } else if(JKC99_TYPE_HANDLE_EQ(r->type, ctx->type_double)) {
            eval_convert_to_type(ctx, l, ctx->type_double);
        } else if(JKC99_TYPE_HANDLE_EQ(l->type, ctx->type_float)) {
            eval_convert_to_type(ctx, r, ctx->type_float);
        } else if(JKC99_TYPE_HANDLE_EQ(r->type, ctx->type_float)) {
            eval_convert_to_type(ctx, l, ctx->type_float);
        } else {
            eval_integer_promotion(ctx, l);
            eval_integer_promotion(ctx, r);
            
            if(JKC99_TYPE_HANDLE_NEQ(l->type, r->type)) {
                _Bool lsigned, rsigned;
                int lrank, rrank;

                lsigned = jkc99_type_is_signed(ctx, l->type);
                rsigned = jkc99_type_is_signed(ctx, r->type);

                lrank = jkc99_type_conversion_rank(ctx, l->type);
                rrank = jkc99_type_conversion_rank(ctx, r->type);

                if(lrank && rrank) {
                    if(lsigned == rsigned) {
                        if(lrank < rrank) {
                            eval_convert_to_type(ctx, l, r->type);
                        } else {
                            jkc99_assert(rrank < lrank);
                            eval_convert_to_type(ctx, r, l->type);
                        }
                    } else {
                        //Swap so l is always the unsigned one
                        if(lsigned) {
                            ExprResult *t;
                            int trank;
                            t = l;
                            l = r;
                            r = t;
                            trank = lrank;
                            lrank = rrank;
                            rrank = trank;
                        }

                        if(lrank >= rrank) {
                            eval_convert_to_type(ctx, r, l->type);
                        } else {
                            JKC99Type ltype, rtype;

                            jkc99_type_get(ctx, l->type, &ltype);
                            jkc99_type_get(ctx, r->type, &rtype);

                            if(rtype.u.i.max >= ltype.u.i.max) {
                                eval_convert_to_type(ctx, l, r->type);
                            } else {
                                TypeHandle targetType = rtype.u.i.correspondingType;

                                eval_convert_to_type(ctx, l, targetType);
                                eval_convert_to_type(ctx, r, targetType);
                            }
                        }
                    }
                }
            }
        }
    }
}

//TODO Support complex
#define EVAL_BINARY(l,op,r) \
    if(l.success && r.success) { \
        if(l.type == ctx->type_long_double) { \
        } else if(r.type == ctx->type_long_double) { \
        } else if(l.type == ctx->type_double) { \
        } else if(r.type == ctx->type_double) { \
        } else if(l.type == ctx->type_float) { \
        } else if(r.type == ctx->type_float) { \
        } else { \
            if(l.type != r.type) { \
            } \
        } \
    }

//TODO 
#define EVAL_ERROR_OPERAND_NOT(op,type)

static inline int jkc99_type_is_integer(ParseContext *ctx, TypeHandle handle) {
    JKC99Type type;

    jkc99_type_get(ctx, handle, &type);
    return (type.kind >= kTypeFirstIntegerType && type.kind <= kTypeLastIntegerType);
}

static inline int jkc99_type_is_floating(ParseContext *ctx, TypeHandle handle) {
    JKC99Type type;

    jkc99_type_get(ctx, handle, &type);
    return (type.kind >= kTypeFirstFloatingType && type.kind <= kTypeLastFloatingType);
}

static inline int jkc99_type_is_real_floating(ParseContext *ctx, TypeHandle handle) {
    JKC99Type type;

    jkc99_type_get(ctx, handle, &type);
    return (type.kind >= kTypeFirstRealFloatingType && type.kind <= kTypeLastRealFloatingType);
}

static inline int jkc99_type_is_real(ParseContext *ctx, TypeHandle handle) {
    return (jkc99_type_is_integer(ctx, handle) || jkc99_type_is_real_floating(ctx, handle));
}

static inline int jkc99_type_is_pointer_to_object(ParseContext *ctx, TypeHandle handle ) {
    JKC99Type type;

    jkc99_type_get(ctx, handle, &type);
    if(type.kind == kTypePointer) {
        JKC99Type base;
        jkc99_type_get(ctx, type.u.p.base, &base);
        if(base.kind && base.kind != kTypeFunc) {
            return 1;
        }
    }
    return 0;
}

UNUSED static inline int jkc99_type_is_pointer_to_func(ParseContext *ctx, TypeHandle handle) {
    JKC99Type type;

    jkc99_type_get(ctx, handle, &type);
    if(type.kind == kTypePointer) {
        JKC99Type base;
        jkc99_type_get(ctx, type.u.p.base, &base);
        if(base.kind && base.kind == kTypeFunc) {
            return 1;
        }
    }
    return 0;
}

static inline int jkc99_type_is_arithmetic(ParseContext *ctx, TypeHandle handle) {
    return (jkc99_type_is_integer(ctx, handle) || jkc99_type_is_floating(ctx, handle));
}

static inline int jkc99_type_is_pointer(ParseContext *ctx, TypeHandle handle) {
    JKC99Type type;

    jkc99_type_get(ctx, handle, &type);
    return (type.kind == kTypePointer);
}

static inline int jkc99_type_is_scalar(ParseContext *ctx, TypeHandle handle) {
    return (jkc99_type_is_arithmetic(ctx, handle) || jkc99_type_is_pointer(ctx, handle));
}

UNUSED static inline int jkc99_type_is_aggregate(ParseContext *ctx, TypeHandle handle) {
    JKC99Type type;

    jkc99_type_get(ctx, handle, &type);
    return (type.kind == kTypeArray || type.kind == kTypeStruct);
}

static int jkc99_type_are_compatible(ParseContext *ctx, TypeHandle l, TypeHandle r) {
    JKC99Type ltype, rtype;

    if(JKC99_TYPE_HANDLE_EQ(l, r)) {
        return 1;
    }

    jkc99_type_get(ctx, l, &ltype);
    jkc99_type_get(ctx, r, &rtype);

    //TODO Cache results
    if(ltype.kind == rtype.kind) {
        if(ltype.kind == kTypePointer) {
            //TODO Fix handling of qualified types
            jkc99_assert(false);
#if 0
            if(ltype.qualifiers == rtype.qualifiers && type_are_compatible(ctx, ltype.u.p.base, rtype.u.p.base)) {
                return 1;
            }
#endif
        } else if(ltype.kind == kTypeArray) {
            if(jkc99_type_are_compatible(ctx, ltype.u.a.base, rtype.u.a.base)) {
                //TODO What to do here? Do we actually need compatibility for arrays? At the moment we can't determine count expr since we don't have access to sizeof at compile time
                if(ltype.u.a.countExpr && rtype.u.a.countExpr) {
                    if(ltype.u.a.countExpr == rtype.u.a.countExpr) {
                        return 1;
                    } else {
                        return 0;
                    }
                } else {
                    return 1;
                }
            }
        } else if(ltype.kind == kTypeFunc) {
            //TODO This really need another look. Relevant section in ISO/IEC 9899:TC3 is 6.7.5.3 15
            if(jkc99_type_are_compatible(ctx, ltype.u.f.returnType, rtype.u.f.returnType)) {
                size_t i;
                if(((ltype.u.f.paramCount && ltype.u.f.paramCount == rtype.u.f.paramCount))
                    && ltype.u.f.hasEllipsis == rtype.u.f.hasEllipsis) {
                    for(i = 0; i < ltype.u.f.paramCount; i++) {
                        if(!jkc99_type_are_compatible(ctx, ltype.u.f.paramTypes[i], rtype.u.f.paramTypes[i])) {
                            return 0;
                        }
                    }
                } else if(  (ltype.u.f.paramCount && !rtype.u.f.paramCount && !ltype.u.f.hasEllipsis)
                    ||      (rtype.u.f.paramCount && !ltype.u.f.paramCount && !rtype.u.f.hasEllipsis)) {
                    return 1;
                }
            }
        } else if(ltype.kind == kTypeStruct) {
            if((ltype.u.su.tag || rtype.u.su.tag) && ltype.u.su.tag != rtype.u.su.tag) {
                return 0;
            }

            if(ltype.u.su.memberCount && rtype.u.su.memberCount) {
                if(ltype.u.su.memberCount == rtype.u.su.memberCount) {
                    size_t i;
                    for(i = 0; i < ltype.u.su.memberCount; i++) {
                        StructUnionMember *lm, *rm;
                        lm = ltype.u.su.members + i;
                        rm = rtype.u.su.members + i;
                        if(lm->identifier != rm->identifier) {
                            return 0;
                        }
                        if(!jkc99_type_are_compatible(ctx, lm->type, rm->type)) {
                            return 0;
                        }
                    }
                    return 1;
                }
            } else {
                return 1;
            }
        } else if(ltype.kind == kTypeUnion) {
            if((ltype.u.su.tag || rtype.u.su.tag) && ltype.u.su.tag != rtype.u.su.tag) {
                return 0;
            }

            if(ltype.u.su.memberCount && rtype.u.su.memberCount) {
                if(ltype.u.su.memberCount == rtype.u.su.memberCount) {
                    size_t i;
                    for(i = 0; i < ltype.u.su.memberCount; i++) {
                        size_t j;
                        for(j = 0; j < rtype.u.su.memberCount; j++) {
                            StructUnionMember *lm, *rm;
                            lm = ltype.u.su.members + i;
                            rm = rtype.u.su.members + j;
                            if(lm->identifier == rm->identifier) {
                                if(jkc99_type_are_compatible(ctx, lm->type, rm->type)) {
                                    break;
                                } else {
                                    return 0;
                                }
                            }
                        }
                        if(j == rtype.u.su.memberCount) {
                            return 0;
                        }
                    }
                    return 1;
                }
            } else {
                return 1;
            }
        } else if(ltype.kind == kTypeEnum) {
            jkc99_assert(false); //TODO 
#if 0
            EnumSpecifier *ls, *rs;

            if((ltype.u.su.tag || rtype.u.su.tag) && ltype.u.su.tag != rtype.u.su.tag) {
                return 0;
            }

            ls = ltype.u.e.specifier;
            rs = rtype.u.e.specifier;

            if(ls->enumeratorCount && rs->enumeratorCount) {
                if(ls->enumeratorCount == rs->enumeratorCount) {
                    size_t i;
                    for(i = 0; i < ls->enumeratorCount; i++) {
                        size_t j;
                        for(j = 0; j < rs->enumeratorCount; j++) {
                            Enumerator *le, *re;
                            le = ls->enumerators + i;
                            re = rs->enumerators + j;

                            if(le->identifier == re->identifier) {
                                if(le->value == re->value) {
                                    break;
                                } else {
                                    return 0;
                                }
                            }
                        }
                        if(j == rs->enumeratorCount) {
                            return 0;
                        }
                    }
                    return 1;
                }
            }
#endif
        }
    } else if(ltype.kind == kTypeEnum || rtype.kind == kTypeEnum) {
        jkc99_assert(false);
        if(rtype.kind == kTypeEnum) {
            //switch so ltype is always the enum
            JKC99Type ttype;
            ttype = ltype;
            ltype = rtype;
            rtype = ttype;
        }
        //TODO  Not sure how to actually handle this? 
        //      We might need configuration options to match the enum to integer type compability strategy of the target compiler and platform.
        //      However that also seems very risky as an optimising compiler could make the decision based on any number of factor other than just the enumerators.
        //      I feel like most compilers probably just use int but not sure.
        //      Reference: ISO/IEC9899:TC3 6.7.2.2
    }

    return 0;
}

static inline int jkc99_type_are_pointers_to_compatible_objects(ParseContext *ctx, TypeHandle l, TypeHandle r) {
    JKC99Type ltype, rtype;

    if(JKC99_TYPE_HANDLE_EQ(l, r)) {
        return 1;
    }

    jkc99_type_get(ctx, l, &ltype);
    jkc99_type_get(ctx, r, &rtype);

    if(ltype.kind == kTypePointer && rtype.kind == kTypePointer) {
        if(JKC99_TYPE_HANDLE_EQ(ltype.u.p.base, rtype.u.p.base)) {
            return 1;
        } else {
            return jkc99_type_are_compatible(ctx, ltype.u.p.base, rtype.u.p.base);
        }
    }

    return 0;
}

#define EVAL_EXPR_SET_BINARY_PARTIAL_RESULT(funcName,memName,srcExpr,res,l,r) \
    if(EXPR_RESULT_SUCCESS(l)) { \
        if(EXPR_RESULT_UNABLE(r)) { \
            EXPR_RESULT_SET_UNABLE(res); \
            (res)->u.expr = jkc99_expr_##funcName(ctx, &expr->src, expr_result_to_expr(ctx, l), (srcExpr)->u.memName.right); \
        } else { \
            jkc99_assert(EXPR_RESULT_ERROR(r)); \
            EXPR_RESULT_SET_ERROR(res); \
            (res)->u.expr = srcExpr; \
        } \
    } else if(EXPR_RESULT_SUCCESS(r)) { \
        if(EXPR_RESULT_UNABLE(l)) { \
            EXPR_RESULT_SET_UNABLE(res); \
            (res)->u.expr = jkc99_expr_##funcName(ctx, &expr->src, (srcExpr)->u.memName.left, expr_result_to_expr(ctx, r)); \
        } else { \
            jkc99_assert(EXPR_RESULT_ERROR(r)); \
            EXPR_RESULT_SET_ERROR(res); \
            (res)->u.expr = srcExpr; \
        } \
    } else { \
        if(EXPR_RESULT_ERROR(l) || EXPR_RESULT_ERROR(r)) { \
            EXPR_RESULT_SET_ERROR(res); \
        } else { \
            jkc99_assert(EXPR_RESULT_UNABLE(l) && EXPR_RESULT_UNABLE(r)); \
            EXPR_RESULT_SET_UNABLE(res); \
        } \
        (res)->u.expr = srcExpr; \
    }

#define EVAL_EXPR_SET_BINARY_PARTIAL_RESULT_WITH_KIND(binKind,subKind,srcExpr,res,l,r) \
    if(EXPR_RESULT_SUCCESS(l)) { \
        if(EXPR_RESULT_UNABLE(r)) { \
            EXPR_RESULT_SET_UNABLE(res); \
            (res)->u.expr = jkc99_expr_##binKind(ctx, &expr->src, subKind, expr_result_to_expr(ctx, l), (srcExpr)->u.binKind.right); \
        } else { \
            jkc99_assert(EXPR_RESULT_ERROR(r)); \
            EXPR_RESULT_SET_ERROR(res); \
            (res)->u.expr = srcExpr; \
        } \
    } else if(EXPR_RESULT_SUCCESS(r)) { \
        if(EXPR_RESULT_UNABLE(l)) { \
            EXPR_RESULT_SET_UNABLE(res); \
            (res)->u.expr = jkc99_expr_##binKind(ctx, &expr->src, subKind, (srcExpr)->u.binKind.left, expr_result_to_expr(ctx, r)); \
        } else { \
            jkc99_assert(EXPR_RESULT_ERROR(r)); \
            EXPR_RESULT_SET_ERROR(res); \
            (res)->u.expr = srcExpr; \
        } \
    } else { \
        if(EXPR_RESULT_ERROR(l) || EXPR_RESULT_ERROR(r)) { \
            EXPR_RESULT_SET_ERROR(res); \
        } else { \
            jkc99_assert(EXPR_RESULT_UNABLE(l) && EXPR_RESULT_UNABLE(r)); \
            EXPR_RESULT_SET_UNABLE(res); \
        } \
        (res)->u.expr = srcExpr; \
    }

JKC99_API ExprResult jkc99_eval_expr(ParseContext *ctx, TypeHandle expected, Expr *expr) {
    ExprResult res = {0};
    
    //jkc99_assert(false); //TODO Major sections TBD

    switch(expr->kind) {

        case kExprPrimary:
            {
                ExprPrimary *e = &expr->u.primary;
                switch(e->kind) {
                    case kExprPrimaryIdentifier:
                        {
                            Symbol *sym = jkc99_sym_get(ctx, e->u.identifier);
                            if(sym && sym->kind == kSymbolEnumerator) {
                                Enumerator *enumerator, *refEnum;
                                JKC99Type type;
                                jkc99_type_get(ctx, sym->type, &type);
                                jkc99_assert(type.kind == kTypeEnum);
                                jkc99_assert(sym->u.enumeratorIndex < type.u.e.enumeratorCount);
                                jkc99_assert((unsigned int)type.u.e.enumerators[sym->u.enumeratorIndex].exprOffset <= sym->u.enumeratorIndex);

                                enumerator = type.u.e.enumerators + sym->u.enumeratorIndex;
                                refEnum = type.u.e.enumerators + (sym->u.enumeratorIndex - enumerator->exprOffset);
                                if(refEnum->expr) {
                                    if(refEnum->exprResult.kind == kExprResultNotResolved) {
                                        refEnum->exprResult = jkc99_eval_expr(ctx, ctx->type_signed_int, refEnum->expr);
                                    }
                                    if(refEnum->exprResult.kind == kExprResultSuccess) {
                                        jkc99_assert(JKC99_TYPE_HANDLE_EQ(refEnum->exprResult.type, ctx->type_signed_int));
                                        res.type = ctx->type_signed_int;
                                        res.u.signed_int_val = refEnum->exprResult.u.signed_int_val + enumerator->exprOffset;
                                        EXPR_RESULT_SET_SUCCESS(&res);
                                    } else {
                                        /* TODO Proper source string */
                                        res.u.expr = jkc99_expr_additive(ctx, &expr->src, kExprAdditiveAdd, refEnum->exprResult.u.expr, jkc99_expr_int(ctx, &expr->src, NULL, enumerator->exprOffset, kConstantDecimal, 0, ""));
                                        if(EXPR_RESULT_UNABLE(&refEnum->exprResult)) {
                                            EXPR_RESULT_SET_UNABLE(&res);
                                        } else {
                                            jkc99_assert(EXPR_RESULT_ERROR(&refEnum->exprResult));
                                            EXPR_RESULT_SET_UNABLE(&res);
                                        }
                                    }
                                } else {
                                    res.type = ctx->type_signed_int;
                                    res.u.signed_int_val = enumerator->exprOffset;
                                    EXPR_RESULT_SET_SUCCESS(&res);
                                }
                            } else {
                                res.u.expr = expr;
                                EXPR_RESULT_SET_ERROR(&res);
                                jkc99_assert(false);
                            }
                        } break;
                    case kExprPrimaryConstant:
                        {
                            switch(e->u.constant.kind) {
                                case kConstantInteger:
                                    {
                                        bool hasU = false;
                                        bool hasL = false;
                                        bool hasLL = false;

                                        if(e->u.constant.suffix[0] == 'u' || e->u.constant.suffix[0] == 'U') {
                                            hasU = true;
                                            if(e->u.constant.suffix[1] == 'l') {
                                                if(e->u.constant.suffix[2] == 'l') {
                                                    hasLL = true;
                                                } else {
                                                    hasL = true;
                                                }
                                            } else if(e->u.constant.suffix[1] == 'L') {
                                                if(e->u.constant.suffix[2] == 'L') {
                                                    hasLL = true;
                                                } else {
                                                    hasL = true;
                                                }
                                            }
                                        } else if(e->u.constant.suffix[0] == 'l') {
                                            if(e->u.constant.suffix[1] == 'l') {
                                                hasLL = true;
                                                if(e->u.constant.suffix[2] == 'u' || e->u.constant.suffix[2] == 'U') {
                                                    hasU = true;
                                                }
                                            } else {
                                                hasL = true;
                                                if(e->u.constant.suffix[1] == 'u' || e->u.constant.suffix[1] == 'U') {
                                                    hasU = true;
                                                }
                                            }
                                        } else if(e->u.constant.suffix[0] == 'L') {
                                            if(e->u.constant.suffix[1] == 'L') {
                                                hasLL = true;
                                                if(e->u.constant.suffix[2] == 'u' || e->u.constant.suffix[2] == 'U') {
                                                    hasU = true;
                                                }
                                            } else {
                                                hasL = true;
                                                if(e->u.constant.suffix[1] == 'u' || e->u.constant.suffix[1] == 'U') {
                                                    hasU = true;
                                                }
                                            }
                                        }
                                        //TODO Support extended types
                                        if(hasU) {
                                            JKC99Type u, ul, ull;

                                            jkc99_type_get(ctx, ctx->type_unsigned_int, &u);
                                            jkc99_type_get(ctx, ctx->type_unsigned_long, &ul);
                                            jkc99_type_get(ctx, ctx->type_unsigned_long_long, &ull);

                                            if(hasLL) {
                                                goto uint_type_ll;
                                            } else if(hasL) {
                                                goto uint_type_l;
                                            }

                                            if(e->u.constant.u.intVal <= u.u.i.max) {
                                                EXPR_RESULT_SET_SUCCESS(&res);
                                                res.type = ctx->type_unsigned_int;
                                                res.u.unsigned_int_val = (unsigned int)e->u.constant.u.intVal;
                                                goto uint_type_end;
                                            }
uint_type_l:
                                            if(e->u.constant.u.intVal <= ul.u.i.max) {
                                                EXPR_RESULT_SET_SUCCESS(&res);
                                                res.type = ctx->type_unsigned_long;
                                                res.u.unsigned_long_val = (unsigned long)e->u.constant.u.intVal;
                                                goto uint_type_end;
                                            }
uint_type_ll:
                                            if(e->u.constant.u.intVal <= ull.u.i.max) {
                                                EXPR_RESULT_SET_SUCCESS(&res);
                                                res.type = ctx->type_unsigned_long_long;
                                                res.u.unsigned_long_long_val = (unsigned long long)e->u.constant.u.intVal;
                                                goto uint_type_end;
                                            }
uint_type_end:;
                                        } else {
                                            bool doU = (e->u.constant.representation == kConstantHex || e->u.constant.representation == kConstantOctal) ? true : false;
                                            JKC99Type u, ul, ull;
                                            JKC99Type s, sl, sll;

                                            jkc99_type_get(ctx, ctx->type_signed_int, &s);
                                            jkc99_type_get(ctx, ctx->type_signed_long, &sl);
                                            jkc99_type_get(ctx, ctx->type_signed_long_long, &sll);
                                            if(doU) {
                                                jkc99_type_get(ctx, ctx->type_unsigned_int, &u);
                                                jkc99_type_get(ctx, ctx->type_unsigned_long, &ul);
                                                jkc99_type_get(ctx, ctx->type_unsigned_long_long, &ull);
                                            }

                                            if(hasLL) {
                                                goto int_type_ll;
                                            } else if(hasL) {
                                                goto int_type_l;
                                            }

                                            if(e->u.constant.u.intVal <= s.u.i.max) {
                                                EXPR_RESULT_SET_SUCCESS(&res);
                                                res.type = ctx->type_signed_int;
                                                res.u.signed_int_val = (signed int)e->u.constant.u.intVal;
                                                goto int_type_end;
                                            } else if(doU && e->u.constant.u.intVal <= u.u.i.max) {
                                                EXPR_RESULT_SET_SUCCESS(&res);
                                                res.type = ctx->type_unsigned_int;
                                                res.u.unsigned_int_val = (unsigned int)e->u.constant.u.intVal;
                                                goto int_type_end;
                                            }

int_type_l:
                                            if(e->u.constant.u.intVal <= sl.u.i.max) {
                                                EXPR_RESULT_SET_SUCCESS(&res);
                                                res.type = ctx->type_signed_long;
                                                res.u.signed_long_val = (signed long)e->u.constant.u.intVal;
                                                goto int_type_end;
                                            } else if(doU && e->u.constant.u.intVal <= ul.u.i.max) {
                                                EXPR_RESULT_SET_SUCCESS(&res);
                                                res.type = ctx->type_unsigned_long;
                                                res.u.unsigned_long_val = (unsigned long)e->u.constant.u.intVal;
                                                goto int_type_end;
                                            }

int_type_ll:
                                            if(e->u.constant.u.intVal <= sll.u.i.max) {
                                                EXPR_RESULT_SET_SUCCESS(&res);
                                                res.type = ctx->type_signed_long_long;
                                                res.u.signed_long_long_val = (signed long long)e->u.constant.u.intVal;
                                                goto int_type_end;
                                            } else if(doU && e->u.constant.u.intVal <= ull.u.i.max) {
                                                EXPR_RESULT_SET_SUCCESS(&res);
                                                res.type = ctx->type_unsigned_long_long;
                                                res.u.unsigned_long_long_val = (unsigned long long)e->u.constant.u.intVal;
                                                goto int_type_end;
                                            }
int_type_end:;
                                        }
                                    } break;
                                case kConstantFloat:
                                    jkc99_assert(false); break;
                                case kConstantEnumeration:
                                    jkc99_assert(false); break;
                                case kConstantCharacter:
                                    jkc99_assert(false); break;
                                default: 
                                    jkc99_assert(false); break;
                            }
                        } break;
                    case kExprPrimaryStringLiteral:
                        {
                            jkc99_assert(false); /* TODO */
                        } break;
                    case kExprPrimaryExpr:
                        {
                            res = jkc99_eval_expr(ctx, expected, e->u.expr);
                        } break;
                    default:    jkc99_assert(false); break;
                }
            } break;
        case kExprPostfix:
            {
                ExprPostfix *e = &expr->u.postfix;
                switch(e->kind) {
                    case kExprPostfixIndex:
                        {
                            jkc99_assert(false); /* TODO */
                        } break;
                    case kExprPostfixCall:
                        {
                            jkc99_assert(false); /* TODO */
                        } break;
                    case kExprPostfixDot:
                        {
                            jkc99_assert(false); /* TODO */
                        } break;
                    case kExprPostfixArrow:
                        {
                            jkc99_assert(false); /* TODO */
                        } break;
                    case kExprPostfixIncrement:
                        {
                            jkc99_assert(false); /* TODO */
                        } break;
                    case kExprPostfixDecrement:
                        {
                            jkc99_assert(false); /* TODO */
                        } break;
                    case kExprPostfixCompound:
                        {
                            jkc99_assert(false); /* TODO */
                        } break;
                    default:    jkc99_assert(false); break;
                }
            } break;
        case kExprUnary:
            {
                ExprUnary *e = &expr->u.unary;
                switch(e->kind) {
                    case kExprUnaryIncrement:
                        {
                            jkc99_assert(false); /* TODO */
                        } break;
                    case kExprUnaryDecrement:
                        {
                            jkc99_assert(false); /* TODO */
                        } break;
                    case kExprUnaryAddressOf:
                        {
                            jkc99_assert(false); /* TODO */
                        } break;
                    case kExprUnaryDeref:
                        {
                            jkc99_assert(false); /* TODO */
                        } break;
                    case kExprUnaryPlus:
                        {
                            jkc99_assert(false); /* TODO */
                        } break;
                    case kExprUnaryMinus:
                        {
                            ExprUnary *e = &expr->u.unary;
                            ExprResult r;

                            r = jkc99_eval_expr(ctx, JKC99_INVALID_TYPE_HANDLE, e->u.expr);
                            if(EXPR_RESULT_SUCCESS(&r)) {
                                TypeHandle t = r.type;

#define EVAL_EXPR_UNARY(tu,op) \
                                res.type = ctx->type_##tu; \
                                res.u.tu##_val = op r.u.tu##_val; \
                                EXPR_RESULT_SET_SUCCESS(&res)

                                if(JKC99_TYPE_HANDLE_EQ(t, ctx->type__Bool)) {
                                    EVAL_EXPR_UNARY(_Bool, -);
                                } else if(JKC99_TYPE_HANDLE_EQ(t, ctx->type_char)) {
                                    EVAL_EXPR_UNARY(char, -);
                                } else if(JKC99_TYPE_HANDLE_EQ(t, ctx->type_signed_char)) {
                                    EVAL_EXPR_UNARY(signed_char, -);
                                } else if(JKC99_TYPE_HANDLE_EQ(t, ctx->type_signed_short)) {
                                    EVAL_EXPR_UNARY(signed_short, -);
                                } else if(JKC99_TYPE_HANDLE_EQ(t, ctx->type_signed_int)) {
                                    EVAL_EXPR_UNARY(signed_int, -);
                                } else if(JKC99_TYPE_HANDLE_EQ(t, ctx->type_signed_long)) {
                                    EVAL_EXPR_UNARY(signed_long, -);
                                } else if(JKC99_TYPE_HANDLE_EQ(t, ctx->type_signed_long_long)) {
                                    EVAL_EXPR_UNARY(signed_long_long, -);
                                } else if(JKC99_TYPE_HANDLE_EQ(t, ctx->type_unsigned_char)) {
                                    EVAL_EXPR_UNARY(unsigned_char, -);
                                } else if(JKC99_TYPE_HANDLE_EQ(t, ctx->type_unsigned_short)) {
                                    EVAL_EXPR_UNARY(unsigned_short, -);
                                } else if(JKC99_TYPE_HANDLE_EQ(t, ctx->type_unsigned_int)) {
                                    EVAL_EXPR_UNARY(unsigned_int, -);
                                } else if(JKC99_TYPE_HANDLE_EQ(t, ctx->type_unsigned_long)) {
                                    EVAL_EXPR_UNARY(unsigned_long, -);
                                } else if(JKC99_TYPE_HANDLE_EQ(t, ctx->type_unsigned_long_long)) {
                                    EVAL_EXPR_UNARY(unsigned_long_long, -);
                                } else if(JKC99_TYPE_HANDLE_EQ(t, ctx->type_float)) {
                                    EVAL_EXPR_UNARY(float, -);
                                } else if(JKC99_TYPE_HANDLE_EQ(t, ctx->type_double)) {
                                    EVAL_EXPR_UNARY(double, -);
                                } else if(JKC99_TYPE_HANDLE_EQ(t, ctx->type_long_double)) {
                                    EVAL_EXPR_UNARY(long_double, -);
                                } else {
                                    jkc99_assert(false); /* TODO */
                                    EXPR_RESULT_SET_ERROR(&res);
                                }
                            } else if(EXPR_RESULT_UNABLE(&r)) {
                                EXPR_RESULT_SET_UNABLE(&res);
                                res.type = r.type;
                                res.u.expr = jkc99_expr_unary(ctx, &expr->src, kExprUnaryMinus, r.u.expr);
                            } else {
                                EXPR_RESULT_SET_ERROR(&res);
                                jkc99_assert(false);
                            }
                        } break;
                    case kExprUnaryBitwiseNeg:
                        {
                            jkc99_assert(false); /* TODO */
                        } break;
                    case kExprUnaryLogicalNot:
                        {
                            jkc99_assert(false); /* TODO */
                        } break;

                    /* TODO Maybe we should try to resolve sizes where possible for sizeof */
                    case kExprUnarySizeofExpr:
                        {
#if 0
                            ExprResult eres;
                            eres = jkc99_eval_expr(ctx, JKC99_INVALID_TYPE_HANDLE, e->u.expr);
                            if(eres.kind == kExprResultSuccess) {
                                Type t;
                                jkc99_assert(JKC99_TYPE_HANDLE_NOT_INVALID(eres.type));
                                jkc99_type_get(ctx, eres.type, &t);
                                EXPR_RESULT_SUCCESS(&res);
                                res.type = ctx->type_size_t;
                            }
#else
                            EXPR_RESULT_SET_UNABLE(&res);
                            res.type = ctx->type_size_t;
                            res.u.expr = expr;
#endif
                        } break;
                    case kExprUnarySizeofType:
                        {
                            /* This is something we can't really do reliably as we're parsing. */
                            EXPR_RESULT_SET_UNABLE(&res);
                            res.type = ctx->type_size_t;
                            res.u.expr = expr;
                        } break;
                    default:    jkc99_assert(false); break;
                }
            } break;
        case kExprCast:
            {
                ExprCast *e = &expr->u.cast;
                TypeHandle t = e->typeName->type;
                ExprResult r;

                r = jkc99_eval_expr(ctx, JKC99_INVALID_TYPE_HANDLE, e->expr);

                jkc99_assert(JKC99_TYPE_HANDLE_NOT_INVALID(t));

                if(EXPR_RESULT_SUCCESS(&r)) {
                    if(JKC99_TYPE_HANDLE_EQ(t, ctx->type__Bool)) {
#define EVAL_EXPR_CAST_INNER(tval,ttype) \
                        if(JKC99_TYPE_HANDLE_EQ(r.type, ctx->type__Bool)) { \
                            res.u.tval##_val = (ttype)r.u._Bool_val; \
                            EXPR_RESULT_SET_SUCCESS(&res); \
                        } else if(JKC99_TYPE_HANDLE_EQ(r.type, ctx->type_signed_char)) { \
                            res.u.tval##_val = (ttype)r.u.signed_char_val; \
                            EXPR_RESULT_SET_SUCCESS(&res); \
                        } else if(JKC99_TYPE_HANDLE_EQ(r.type, ctx->type_signed_short)) { \
                            res.u.tval##_val = (ttype)r.u.signed_short_val; \
                            EXPR_RESULT_SET_SUCCESS(&res); \
                        } else if(JKC99_TYPE_HANDLE_EQ(r.type, ctx->type_signed_int)) { \
                            res.u.tval##_val = (ttype)r.u.signed_int_val; \
                            EXPR_RESULT_SET_SUCCESS(&res); \
                        } else if(JKC99_TYPE_HANDLE_EQ(r.type, ctx->type_signed_long)) { \
                            res.u.tval##_val = (ttype)r.u.signed_long_val; \
                            EXPR_RESULT_SET_SUCCESS(&res); \
                        } else if(JKC99_TYPE_HANDLE_EQ(r.type, ctx->type_signed_long_long)) { \
                            res.u.tval##_val = (ttype)r.u.signed_long_long_val; \
                            EXPR_RESULT_SET_SUCCESS(&res); \
                        } else if(JKC99_TYPE_HANDLE_EQ(r.type, ctx->type_unsigned_char)) { \
                            res.u.tval##_val = (ttype)r.u.unsigned_char_val; \
                            EXPR_RESULT_SET_SUCCESS(&res); \
                        } else if(JKC99_TYPE_HANDLE_EQ(r.type, ctx->type_unsigned_short)) { \
                            res.u.tval##_val = (ttype)r.u.unsigned_short_val; \
                            EXPR_RESULT_SET_SUCCESS(&res); \
                        } else if(JKC99_TYPE_HANDLE_EQ(r.type, ctx->type_unsigned_int)) { \
                            res.u.tval##_val = (ttype)r.u.unsigned_int_val; \
                            EXPR_RESULT_SET_SUCCESS(&res); \
                        } else if(JKC99_TYPE_HANDLE_EQ(r.type, ctx->type_unsigned_long)) { \
                            res.u.tval##_val = (ttype)r.u.unsigned_long_val; \
                            EXPR_RESULT_SET_SUCCESS(&res); \
                        } else if(JKC99_TYPE_HANDLE_EQ(r.type, ctx->type_unsigned_long_long)) { \
                            res.u.tval##_val = (ttype)r.u.unsigned_long_long_val; \
                            EXPR_RESULT_SET_SUCCESS(&res); \
                        } else if(JKC99_TYPE_HANDLE_EQ(r.type, ctx->type_float)) { \
                            res.u.tval##_val = (ttype)r.u.float_val; \
                            EXPR_RESULT_SET_SUCCESS(&res); \
                        } else if(JKC99_TYPE_HANDLE_EQ(r.type, ctx->type_double)) { \
                            res.u.tval##_val = (ttype)r.u.double_val; \
                            EXPR_RESULT_SET_SUCCESS(&res); \
                        } else if(JKC99_TYPE_HANDLE_EQ(r.type, ctx->type_long_double)) { \
                            res.u.tval##_val = (ttype)r.u.long_double_val; \
                            EXPR_RESULT_SET_SUCCESS(&res); \
                        } else { \
                            EXPR_RESULT_SET_ERROR(&res); \
                            jkc99_assert(false); /* TODO */ \
                        }
                        EVAL_EXPR_CAST_INNER(_Bool,_Bool);
                    } else if(JKC99_TYPE_HANDLE_EQ(t, ctx->type_signed_char)) {
                        EVAL_EXPR_CAST_INNER(signed_char, signed char);
                    } else if(JKC99_TYPE_HANDLE_EQ(t, ctx->type_signed_short)) {
                        EVAL_EXPR_CAST_INNER(signed_short, signed short);
                    } else if(JKC99_TYPE_HANDLE_EQ(t, ctx->type_signed_int)) {
                        EVAL_EXPR_CAST_INNER(signed_int, signed int);
                    } else if(JKC99_TYPE_HANDLE_EQ(t, ctx->type_signed_long)) {
                        EVAL_EXPR_CAST_INNER(signed_long, signed long);
                    } else if(JKC99_TYPE_HANDLE_EQ(t, ctx->type_signed_long_long)) {
                        EVAL_EXPR_CAST_INNER(signed_long_long, signed long long);
                    } else if(JKC99_TYPE_HANDLE_EQ(t, ctx->type_unsigned_char)) {
                        EVAL_EXPR_CAST_INNER(unsigned_char, unsigned char);
                    } else if(JKC99_TYPE_HANDLE_EQ(t, ctx->type_unsigned_short)) {
                        EVAL_EXPR_CAST_INNER(unsigned_short, unsigned short);
                    } else if(JKC99_TYPE_HANDLE_EQ(t, ctx->type_unsigned_int)) {
                        EVAL_EXPR_CAST_INNER(unsigned_int, unsigned int);
                    } else if(JKC99_TYPE_HANDLE_EQ(t, ctx->type_unsigned_long)) {
                        EVAL_EXPR_CAST_INNER(unsigned_long, unsigned long);
                    } else if(JKC99_TYPE_HANDLE_EQ(t, ctx->type_unsigned_long_long)) {
                        EVAL_EXPR_CAST_INNER(unsigned_long_long, unsigned long long);
                    } else if(JKC99_TYPE_HANDLE_EQ(t, ctx->type_float)) {
                        EVAL_EXPR_CAST_INNER(float, float);
                    } else if(JKC99_TYPE_HANDLE_EQ(t, ctx->type_double)) {
                        EVAL_EXPR_CAST_INNER(double, double);
                    } else if(JKC99_TYPE_HANDLE_EQ(t, ctx->type_long_double)) {
                        EVAL_EXPR_CAST_INNER(long_double, long double);
                    } else {
                        jkc99_assert(false); /* TODO */
                        EXPR_RESULT_SET_ERROR(&res);
                    }
                } else if(EXPR_RESULT_UNABLE(&r)) {
                    EXPR_RESULT_SET_UNABLE(&res);
                    res.u.expr = jkc99_expr_cast(ctx, &expr->src, e->typeName, r.u.expr);
                } else {
                    EXPR_RESULT_SET_ERROR(&res);
                }
                res.type = t;
            } break;
        case kExprMultiplicative:
            {
                ExprResult l,r;
                ExprMultiplicative *e = &expr->u.multiplicative;

                l = jkc99_eval_expr(ctx, JKC99_INVALID_TYPE_HANDLE, e->left);
                r = jkc99_eval_expr(ctx, JKC99_INVALID_TYPE_HANDLE, e->right);

                if(EXPR_RESULT_SUCCESS(&l) && EXPR_RESULT_SUCCESS(&r)) {
                    switch(e->kind) {
                        case kExprMultiplicativeMul:
                            {
                                if(jkc99_type_is_arithmetic(ctx, l.type) && jkc99_type_is_arithmetic(ctx, r.type)) {
                                    EVAL_BINARY_OP(&res,*,l,r);
                                }
                            } break;
                        case kExprMultiplicativeDiv:
                            {
                                if(jkc99_type_is_arithmetic(ctx, l.type) && jkc99_type_is_arithmetic(ctx, r.type)) {
                                    /* TODO Handle zero on right side */
                                    jkc99_assert(l.u.unsigned_long_long_val);
                                    EVAL_BINARY_OP(&res,/,l,r);
                                }
                            } break;
                        case kExprMultiplicativeMod:
                            {
                                if(jkc99_type_is_arithmetic(ctx, l.type) && jkc99_type_is_arithmetic(ctx, r.type)) {
                                    /* TODO Handle zero on right side */
                                    jkc99_assert(l.u.unsigned_long_long_val);
                                    EVAL_BINARY_OP_INTEGER(&res,%,l,r);
                                }
                            } break;
                        default:    jkc99_assert(false); break;
                    }
                } else EVAL_EXPR_SET_BINARY_PARTIAL_RESULT_WITH_KIND(multiplicative, e->kind, expr, &res, &l, &r);
            } break;

        case kExprAdditive:
            {
                ExprResult l,r;
                ExprAdditive *e = &expr->u.additive;

                l = jkc99_eval_expr(ctx, JKC99_INVALID_TYPE_HANDLE, e->left);
                r = jkc99_eval_expr(ctx, JKC99_INVALID_TYPE_HANDLE, e->right);

                if(EXPR_RESULT_SUCCESS(&l) && EXPR_RESULT_SUCCESS(&r)) {
                    switch(e->kind) {
                        case kExprAdditiveAdd:
                            {
                                if(jkc99_type_is_arithmetic(ctx, l.type) && jkc99_type_is_arithmetic(ctx, r.type)) {
                                    EVAL_BINARY_OP(&res,+,l,r);
                                } else if(
                                   (jkc99_type_is_pointer_to_object(ctx, l.type) && jkc99_type_is_integer(ctx, r.type)) ||
                                   (jkc99_type_is_integer(ctx, l.type) && jkc99_type_is_pointer_to_object(ctx, r.type))) {
                                    jkc99_assert(false); /* TODO */
                                } else {
                                    jkc99_assert(false);
                                }
                            } break;
                        case kExprAdditiveSub:
                            {
                                if(jkc99_type_is_arithmetic(ctx, l.type) && jkc99_type_is_arithmetic(ctx, r.type)) {
                                    EVAL_BINARY_OP(&res,-,l,r);
                                } else if(
                                   (jkc99_type_are_pointers_to_compatible_objects(ctx, l.type, r.type)) ||
                                   (jkc99_type_is_pointer_to_object(ctx, l.type) && jkc99_type_is_integer(ctx, r.type))) {
                                    jkc99_assert(false); /* TODO */
                                } else {
                                    jkc99_assert(false);
                                }
                            } break;
                        default:    jkc99_assert(false); break;
                    }
                } else EVAL_EXPR_SET_BINARY_PARTIAL_RESULT_WITH_KIND(additive, e->kind, expr, &res, &l, &r);
            } break;
        case kExprShift:
            {
                ExprResult l,r;
                ExprShift *e = &expr->u.shift;

                l = jkc99_eval_expr(ctx, JKC99_INVALID_TYPE_HANDLE, e->left);
                r = jkc99_eval_expr(ctx, JKC99_INVALID_TYPE_HANDLE, e->right);

                if(EXPR_RESULT_SUCCESS(&l) && EXPR_RESULT_SUCCESS(&r)) {
                    if(jkc99_type_is_integer(ctx, l.type) && jkc99_type_is_integer(ctx, r.type)) {
                        JKC99Type ltype, rtype;

                        eval_integer_promotion(ctx, &l);
                        eval_integer_promotion(ctx, &r);

                        jkc99_type_get(ctx, l.type, &ltype);
                        jkc99_type_get(ctx, r.type, &rtype);

                        //TODO Extended types
                        //TODO Enum
#define EVAL_SHIFT_INNER(restype,op) \
                        { \
                            EXPR_RESULT_SET_SUCCESS(&res); \
                            res.type = l.type; \
                            switch(rtype.kind) \
                            { \
                                case kTypeChar:             res.u.restype##_val = l.u.restype##_val op r.u.char_val;                break; \
                                case kTypeSignedChar:       res.u.restype##_val = l.u.restype##_val op r.u.signed_char_val;         break; \
                                case kTypeSignedShort:      res.u.restype##_val = l.u.restype##_val op r.u.signed_short_val;        break; \
                                case kTypeSignedInt:        res.u.restype##_val = l.u.restype##_val op r.u.signed_int_val;          break; \
                                case kTypeSignedLong:       res.u.restype##_val = l.u.restype##_val op r.u.signed_long_val;         break; \
                                case kTypeSignedLongLong:   res.u.restype##_val = l.u.restype##_val op r.u.signed_long_long_val;    break; \
                                case kType_Bool:            res.u.restype##_val = l.u.restype##_val op r.u._Bool_val;               break; \
                                case kTypeUnsignedChar:     res.u.restype##_val = l.u.restype##_val op r.u.unsigned_char_val;       break; \
                                case kTypeUnsignedShort:    res.u.restype##_val = l.u.restype##_val op r.u.unsigned_short_val;      break; \
                                case kTypeUnsignedInt:      res.u.restype##_val = l.u.restype##_val op r.u.unsigned_int_val;        break; \
                                case kTypeUnsignedLong:     res.u.restype##_val = l.u.restype##_val op r.u.unsigned_long_val;       break; \
                                case kTypeUnsignedLongLong: res.u.restype##_val = l.u.restype##_val op r.u.unsigned_long_long_val;  break; \
                                case kTypeSignedExtended: \
                                case kTypeUnsignedExtended: \
                                case kTypeEnum: \
                                default: jkc99_assert(false); break; \
                            } \
                        }

                        //TODO Extended types
                        //TODO Enum
#define EVAL_SHIFT(op) \
                        { \
                            switch(ltype.kind) \
                            { \
                                case kTypeChar:             EVAL_SHIFT_INNER(char,op);                  break; \
                                case kTypeSignedChar:       EVAL_SHIFT_INNER(signed_char,op);           break; \
                                case kTypeSignedShort:      EVAL_SHIFT_INNER(signed_short,op);          break; \
                                case kTypeSignedInt:        EVAL_SHIFT_INNER(signed_int,op);            break; \
                                case kTypeSignedLong:       EVAL_SHIFT_INNER(signed_long,op);           break; \
                                case kTypeSignedLongLong:   EVAL_SHIFT_INNER(signed_long_long,op);      break; \
                                case kType_Bool:            EVAL_SHIFT_INNER(_Bool,op);                 break; \
                                case kTypeUnsignedChar:     EVAL_SHIFT_INNER(unsigned_char,op);         break; \
                                case kTypeUnsignedShort:    EVAL_SHIFT_INNER(unsigned_short,op);        break; \
                                case kTypeUnsignedInt:      EVAL_SHIFT_INNER(unsigned_int,op);          break; \
                                case kTypeUnsignedLong:     EVAL_SHIFT_INNER(unsigned_long,op);         break; \
                                case kTypeUnsignedLongLong: EVAL_SHIFT_INNER(unsigned_long_long,op);    break; \
                                case kTypeSignedExtended: \
                                case kTypeUnsignedExtended: \
                                case kTypeEnum: \
                                default: jkc99_assert(false); break; \
                            } \
                        }

                        switch(e->kind) {
                            case kExprShiftLeft:
                                {
                                    EVAL_SHIFT(<<);
                                } break;
                            case kExprShiftRight:
                                {
                                    EVAL_SHIFT(>>);
                                } break;
                            default:    jkc99_assert(false); break;
                        }
                    }
                } else EVAL_EXPR_SET_BINARY_PARTIAL_RESULT_WITH_KIND(shift, e->kind, expr, &res, &l, &r);

            } break;
        case kExprRelational:
            {
                ExprResult l,r;
                ExprRelational *e = &expr->u.relational;

                l = jkc99_eval_expr(ctx, JKC99_INVALID_TYPE_HANDLE, e->left);
                r = jkc99_eval_expr(ctx, JKC99_INVALID_TYPE_HANDLE, e->right);

                if(EXPR_RESULT_SUCCESS(&l) && EXPR_RESULT_SUCCESS(&r)) {
                    if(jkc99_type_is_real(ctx, l.type) && jkc99_type_is_real(ctx, r.type)) {
                        switch(e->kind) {
                            case kExprRelationalLT:
                                {
                                    EVAL_CMP(&res,<,l,r);
                                } break;
                            case kExprRelationalGT:
                                {
                                    EVAL_CMP(&res,>,l,r);
                                } break;
                            case kExprRelationalLTE:
                                {
                                    EVAL_CMP(&res,<=,l,r);
                                } break;
                            case kExprRelationalGTE:
                                {
                                    EVAL_CMP(&res,>=,l,r);
                                } break;
                            default:    jkc99_assert(false); break;
                        }
                    } else if(jkc99_type_are_pointers_to_compatible_objects(ctx, l.type, r.type)) {
                        jkc99_assert(false); //TODO TODO TODO
                    //} else if(jkc99_type_are_pointers_to_compatible_incomplete_types(ctx, l.type, r.type)) {
                    //    jkc99_assert(false); //TODO TODO TODO
                    }
                } else EVAL_EXPR_SET_BINARY_PARTIAL_RESULT_WITH_KIND(relational, e->kind, expr, &res, &l, &r);
            } break;
        case kExprEquality:
            {
                ExprResult l,r;
                ExprEquality *e = &expr->u.equality;

                l = jkc99_eval_expr(ctx, JKC99_INVALID_TYPE_HANDLE, e->left);
                r = jkc99_eval_expr(ctx, JKC99_INVALID_TYPE_HANDLE, e->right);


                if(EXPR_RESULT_SUCCESS(&l) && EXPR_RESULT_SUCCESS(&r)) {
                    if(jkc99_type_is_arithmetic(ctx, l.type) && jkc99_type_is_arithmetic(ctx, r.type)) {
                        if(e->kind == kExprEqualityEQ) {
                            EVAL_CMP(&res,==,l,r);
                        } else {
                            jkc99_assert(e->kind == kExprEqualityNE);
                            EVAL_CMP(&res,!=,l,r);
                        }
                    } else {
                        JKC99Type ltype, rtype;
                        _Bool proceed = false;

                        jkc99_type_get(ctx, l.type, &ltype);
                        jkc99_type_get(ctx, r.type, &rtype);

                        if(ltype.kind == kTypePointer && rtype.kind == kTypePointer) {
                            if(jkc99_type_are_compatible(ctx, ltype.u.p.base, rtype.u.p.base)) {
                                proceed = true;
                            } else {
                                JKC99Type lbase, rbase;
                                jkc99_type_get(ctx, ltype.u.p.base, &lbase);
                                jkc99_type_get(ctx, rtype.u.p.base, &rbase);
                                if(lbase.kind == kTypeVoid || rbase.kind == kTypeVoid) {
                                    proceed = true;
                                }
                            }
                        }

                        if(proceed) {
                            EXPR_RESULT_SET_SUCCESS(&res);
                            res.type = ctx->type_signed_int;
                            if(e->kind == kExprEqualityEQ) {
                                res.u.signed_int_val = l.u.ptr == r.u.ptr;
                            } else {
                                jkc99_assert(e->kind == kExprEqualityNE);
                                res.u.signed_int_val = l.u.ptr != r.u.ptr;
                            }
                        }
                    }
                } else EVAL_EXPR_SET_BINARY_PARTIAL_RESULT_WITH_KIND(equality, e->kind, expr, &res, &l, &r);
            } break;
        case kExprAnd:
            {
                ExprResult l,r;
                ExprAnd *e = &expr->u.and;

                l = jkc99_eval_expr(ctx, JKC99_INVALID_TYPE_HANDLE, e->left);
                r = jkc99_eval_expr(ctx, JKC99_INVALID_TYPE_HANDLE, e->right);

                if(EXPR_RESULT_SUCCESS(&l) && EXPR_RESULT_SUCCESS(&r)) {
                    if(jkc99_type_is_integer(ctx, l.type)) {
                        if(jkc99_type_is_integer(ctx, r.type)) {
                            EVAL_BINARY_OP_INTEGER(&res,&,l,r)
                        } else {
                            EVAL_ERROR_OPERAND_NOT(right,integer);
                        }
                    } else {
                        EVAL_ERROR_OPERAND_NOT(left,integer);
                    }
                } else EVAL_EXPR_SET_BINARY_PARTIAL_RESULT(and, and, expr, &res, &l, &r);
            } break;
        case kExprExclusiveOr:
            {
                ExprResult l,r;
                ExprExclusiveOr *e = &expr->u.exclusiveOr;

                l = jkc99_eval_expr(ctx, JKC99_INVALID_TYPE_HANDLE, e->left);
                r = jkc99_eval_expr(ctx, JKC99_INVALID_TYPE_HANDLE, e->right);

                if(EXPR_RESULT_SUCCESS(&l) && EXPR_RESULT_SUCCESS(&r)) {
                    if(jkc99_type_is_integer(ctx, l.type)) {
                        if(jkc99_type_is_integer(ctx, r.type)) {
                            EVAL_BINARY_OP_INTEGER(&res,^,l,r)
                        } else {
                            EVAL_ERROR_OPERAND_NOT(right,integer);
                        }
                    } else {
                        EVAL_ERROR_OPERAND_NOT(left,integer);
                    }
                } else EVAL_EXPR_SET_BINARY_PARTIAL_RESULT(exclusive_or, exclusiveOr, expr, &res, &l, &r);
            } break;
        case kExprInclusiveOr:
            {
                ExprResult l,r;
                ExprInclusiveOr *e = &expr->u.inclusiveOr;

                l = jkc99_eval_expr(ctx, JKC99_INVALID_TYPE_HANDLE, e->left);
                r = jkc99_eval_expr(ctx, JKC99_INVALID_TYPE_HANDLE, e->right);

                if(EXPR_RESULT_SUCCESS(&l) && EXPR_RESULT_SUCCESS(&r)) {
                    if(jkc99_type_is_integer(ctx, l.type)) {
                        if(jkc99_type_is_integer(ctx, r.type)) {
                            EVAL_BINARY_OP_INTEGER(&res,|,l,r)
                        } else {
                            EVAL_ERROR_OPERAND_NOT(right,integer);
                        }
                    } else {
                        EVAL_ERROR_OPERAND_NOT(left,integer);
                    }
                } else EVAL_EXPR_SET_BINARY_PARTIAL_RESULT(inclusive_or, inclusiveOr, expr, &res, &l, &r);
            } break;
        case kExprLogicalAnd:
            {
                ExprResult l,r;
                ExprLogicalAnd *e = &expr->u.logicalAnd;

                l = jkc99_eval_expr(ctx, JKC99_INVALID_TYPE_HANDLE, e->left);
                if(EXPR_RESULT_SUCCESS(&l)) {
                    if(jkc99_type_is_scalar(ctx, l.type)) {
                        r = jkc99_eval_expr(ctx, JKC99_INVALID_TYPE_HANDLE, e->right);
                        if(EXPR_RESULT_SUCCESS(&r)) {
                            if(jkc99_type_is_scalar(ctx, r.type)) {
                                EXPR_RESULT_SET_SUCCESS(&res);
                                res.type = ctx->type_signed_int;
                                res.u.signed_int_val = eval_scalar_to_bool(ctx, l) && eval_scalar_to_bool(ctx, r);
                            } else {
                                EVAL_ERROR_OPERAND_NOT(right,scalar);
                            }
                        }
                    } else {
                        EVAL_ERROR_OPERAND_NOT(left,scalar);
                    }
                } else EVAL_EXPR_SET_BINARY_PARTIAL_RESULT(logical_and, logicalAnd, expr, &res, &l, &r);
            } break;
        case kExprLogicalOr:
            {
                ExprResult l,r;
                ExprLogicalOr *e = &expr->u.logicalOr;

                l = jkc99_eval_expr(ctx, JKC99_INVALID_TYPE_HANDLE, e->left);
                if(EXPR_RESULT_SUCCESS(&l)) {
                    if(jkc99_type_is_scalar(ctx, l.type)) {
                        if(eval_scalar_to_bool(ctx, l)) {
                            EXPR_RESULT_SET_SUCCESS(&res);
                            res.type = ctx->type_signed_int;
                            res.u.signed_int_val = 1;
                        } else {
                            r = jkc99_eval_expr(ctx, JKC99_INVALID_TYPE_HANDLE, e->right);
                            if(EXPR_RESULT_SUCCESS(&r)) {
                                if(jkc99_type_is_scalar(ctx, r.type)) {
                                    EXPR_RESULT_SET_SUCCESS(&res);
                                    res.type = ctx->type_signed_int;
                                    res.u.signed_int_val = eval_scalar_to_bool(ctx, r) ? 1 : 0;
                                } else {
                                    EVAL_ERROR_OPERAND_NOT(right,scalar);
                                }
                            }
                        }
                    } else {
                        EVAL_ERROR_OPERAND_NOT(left,scalar);
                    }
                } else EVAL_EXPR_SET_BINARY_PARTIAL_RESULT(logical_or, logicalOr, expr, &res, &l, &r);
            } break;
        case kExprConditional:
            {
                ExprResult cond;
                _Bool condVal;
                ExprConditional *e = &expr->u.conditional;

                cond = jkc99_eval_expr(ctx, ctx->type__Bool, e->cond);
                if(EXPR_RESULT_SUCCESS(&cond)) {
                    if(jkc99_type_is_scalar(ctx, cond.type)) {
                        condVal = eval_scalar_to_bool(ctx, cond);
                        if(condVal) {
                            res = jkc99_eval_expr(ctx, expected, e->left);
                        } else {
                            res = jkc99_eval_expr(ctx, expected, e->right);
                        }
                    } else {
                        EVAL_ERROR_OPERAND_NOT(condition,scalar);
                    }
                } else if(EXPR_RESULT_UNABLE(&cond)) {
                    EXPR_RESULT_SET_UNABLE(&res);
                    res.u.expr = expr;
                }
            } break;

        default:
            {
                //TODO Assignment and comma expressions
                jkc99_assert(false);
            } break;
    }

    jkc99_assert(EXPR_RESULT_SUCCESS(&res) || (EXPR_RESULT_UNABLE(&res) && res.u.expr));
    return res;
}

JKC99_API TypeHandle   jkc99_type_void(ParseContext *ctx) {
    return ctx->type_void;
}

JKC99_API TypeHandle   jkc99_type_char(ParseContext *ctx) {
    return ctx->type_char;
}

JKC99_API TypeHandle   jkc99_type_signed_char(ParseContext *ctx) {
    return ctx->type_signed_char;
}

JKC99_API TypeHandle   jkc99_type_signed_short(ParseContext *ctx) {
    return ctx->type_signed_short;
}

JKC99_API TypeHandle   jkc99_type_signed_int(ParseContext *ctx) {
    return ctx->type_signed_int;
}

JKC99_API TypeHandle   jkc99_type_signed_long(ParseContext *ctx) {
    return ctx->type_signed_long;
}

JKC99_API TypeHandle   jkc99_type_signed_long_long(ParseContext *ctx) {
    return ctx->type_signed_long_long;
}

JKC99_API TypeHandle   jkc99_type__Bool(ParseContext *ctx) {
    return ctx->type__Bool;
}

JKC99_API TypeHandle   jkc99_type_unsigned_char(ParseContext *ctx) {
    return ctx->type_unsigned_char;
}

JKC99_API TypeHandle   jkc99_type_unsigned_short(ParseContext *ctx) {
    return ctx->type_unsigned_short;
}

JKC99_API TypeHandle   jkc99_type_unsigned_int(ParseContext *ctx) {
    return ctx->type_unsigned_int;
}

JKC99_API TypeHandle   jkc99_type_unsigned_long(ParseContext *ctx) {
    return ctx->type_unsigned_long;
}

JKC99_API TypeHandle   jkc99_type_unsigned_long_long(ParseContext *ctx) {
    return ctx->type_unsigned_long_long;
}

JKC99_API TypeHandle   jkc99_type_float(ParseContext *ctx) {
    return ctx->type_float;
}

JKC99_API TypeHandle   jkc99_type_double(ParseContext *ctx) {
    return ctx->type_double;
}

JKC99_API TypeHandle   jkc99_type_long_double(ParseContext *ctx) {
    return ctx->type_long_double;
}

JKC99_API TypeHandle   jkc99_type_float__Complex(ParseContext *ctx) {
    return ctx->type_float__Complex;
}

JKC99_API TypeHandle   jkc99_type_double__Complex(ParseContext *ctx) {
    return ctx->type_double__Complex;
}

JKC99_API TypeHandle   jkc99_type_long_double__Complex(ParseContext *ctx) {
    return ctx->type_long_double__Complex;
}

JKC99_API TypeHandle   jkc99_type_size_t(ParseContext *ctx) {
    return ctx->type_size_t;
}

#ifdef __clang__
JKC99_API TypeHandle   jkc99_type___builtin_va_list(ParseContext *ctx) {
    return ctx->type___builtin_va_list;
}
#endif

JKC99_API TypeHandle jkc99_type_struct(ParseContext *ctx, const char *tag) {
    JKC99Scope *scope;
    size_t i;
    JKC99Type type = {0};
    Tag newTag = {.kind = kTagStruct};

    if(tag) {
        for(scope = &da_last(ctx->scopes); scope >= ctx->scopes; --scope) {
            for(i = 0; i < da_count(scope->tags); ++i) {
                Tag *t = scope->tags + i;
                if(t->identifier == tag) {
                    if(t->kind == kTagStruct) {
                        return t->type;
                    } else {
                        jkc99_log_error(ctx, "Tag type mismatch for tag %s", tag);
                    }
                }
            }
        }
    }

    type.kind = kTypeStruct;
    type.u.su.tag = tag;
    type.u.su.scope = da_count(ctx->scopes)-1;
    da_push(ctx->types, type);

    newTag.type = JKC99_INDEX_TO_TYPE_HANDLE(da_count(ctx->types)-1);
    if(tag) {
        scope = &da_last(ctx->scopes);
        newTag.identifier = tag;
        da_push(scope->tags, newTag);
    }

    return newTag.type;
}

JKC99_API TypeHandle jkc99_type_union(ParseContext *ctx, const char *tag) {
    JKC99Scope *scope;
    size_t i;
    JKC99Type type = {0};
    Tag newTag = {.kind = kTagUnion};

    if(tag) {
        for(scope = &da_last(ctx->scopes); scope >= ctx->scopes; --scope) {
            for(i = 0; i < da_count(scope->tags); ++i) {
                Tag *t = scope->tags + i;
                if(t->identifier == tag) {
                    if(t->kind == kTagUnion) {
                        return t->type;
                    } else {
                        jkc99_log_error(ctx, "Tag type mismatch for tag %s", tag);
                    }
                }
            }
        }
    }

    type.kind = kTypeUnion;
    type.u.su.tag = tag;
    type.u.su.scope = da_count(ctx->scopes)-1;
    da_push(ctx->types, type);

    newTag.type = JKC99_INDEX_TO_TYPE_HANDLE(da_count(ctx->types)-1);
    if(tag) {
        scope = &da_last(ctx->scopes);
        newTag.identifier = tag;
        da_push(scope->tags, newTag);
    }

    return newTag.type;
}

JKC99_API TypeHandle jkc99_type_enum(ParseContext *ctx, const char *tag) {
    JKC99Scope *scope;
    size_t i;
    JKC99Type type = {0};
    Tag newTag = {.kind = kTagEnum};

    if(tag) {
        for(scope = &da_last(ctx->scopes); scope >= ctx->scopes; --scope) {
            for(i = 0; i < da_count(scope->tags); ++i) {
                Tag *t = scope->tags + i;
                if(t->identifier == tag) {
                    if(t->kind == kTagEnum) {
                        return t->type;
                    } else {
                        jkc99_log_error(ctx, "Tag type mismatch for tag %s", tag);
                    }
                }
            }
        }
    }

    type.kind = kTypeEnum;
    type.u.e.tag = tag;
    type.u.e.scope = da_count(ctx->scopes)-1;
    da_push(ctx->types, type);

    newTag.type = JKC99_INDEX_TO_TYPE_HANDLE(da_count(ctx->types)-1);
    if(tag) {
        scope = &da_last(ctx->scopes);
        newTag.identifier = tag;
        da_push(scope->tags, newTag);
    }

    return newTag.type;
}


JKC99_API TypeHandle jkc99_type_qualified(ParseContext *ctx, TypeHandle base, int qualifiers) {
    size_t i;
    JKC99Type type = {0};

    jkc99_assert(JKC99_TYPE_HANDLE_NOT_INVALID(base));

    for(i = 0; i < da_count(ctx->types); i++) {
        JKC99Type *t = ctx->types + i;
        if(t->kind == kTypeQualified && JKC99_TYPE_HANDLE_EQ(t->u.q.base, base) && t->u.q.qualifiers == qualifiers) {
            return JKC99_INDEX_TO_TYPE_HANDLE(i);
        }
    }

    type.kind = kTypeQualified;
    type.u.q.base = base;
    type.u.q.qualifiers = qualifiers;
    da_push(ctx->types, type);

    return JKC99_INDEX_TO_TYPE_HANDLE(da_count(ctx->types)-1);
}

JKC99_API TypeHandle jkc99_type_ptr(ParseContext *ctx, TypeHandle base) {
    size_t i;
    JKC99Type type = {0};

    jkc99_assert(JKC99_TYPE_HANDLE_NOT_INVALID(base));

    for(i = 0; i < da_count(ctx->types); i++) {
        JKC99Type *t = ctx->types + i;
        if(t->kind == kTypePointer && JKC99_TYPE_HANDLE_EQ(t->u.p.base, base)) {
            return JKC99_INDEX_TO_TYPE_HANDLE(i);
        }
    }

    type.kind = kTypePointer;
    type.u.p.base = base;
    da_push(ctx->types, type);

    return JKC99_INDEX_TO_TYPE_HANDLE(da_count(ctx->types)-1);
}

JKC99_API _Bool jkc99_type_is_signed(ParseContext *ctx, TypeHandle type) {
    if(     JKC99_TYPE_HANDLE_EQ(type, ctx->type_signed_char)
        ||  JKC99_TYPE_HANDLE_EQ(type, ctx->type_signed_short)
        ||  JKC99_TYPE_HANDLE_EQ(type, ctx->type_signed_int)
        ||  JKC99_TYPE_HANDLE_EQ(type, ctx->type_signed_long)
        ||  JKC99_TYPE_HANDLE_EQ(type, ctx->type_signed_long_long)
        || (JKC99_TYPE_HANDLE_EQ(type, ctx->type_char) && ctx->signedChar)) {
        return true;
    } else {
        jkc99_assert(JKC99_TYPE_HANDLE_EQ(type, ctx->type__Bool)
                ||JKC99_TYPE_HANDLE_EQ(type, ctx->type_unsigned_char)
                ||JKC99_TYPE_HANDLE_EQ(type, ctx->type_unsigned_short)
                ||JKC99_TYPE_HANDLE_EQ(type, ctx->type_unsigned_int)
                ||JKC99_TYPE_HANDLE_EQ(type, ctx->type_unsigned_long)
                ||JKC99_TYPE_HANDLE_EQ(type, ctx->type_unsigned_long_long));
        return false;
    }
}


/* TODO The approach we've taken with this leaves all sorts of issues in figuring out counts for arrays:
 * int foo1[] = { [3] = 3, 5 };                             Doable
 * int foo2[] = { ['a'+2] = 4, 7 };                         Bit more complex
 * int foo3[] = { [sizeof(void*) + 9] = 10, 11];            Impossible?
 * int foo3[] = { [sizeof(Foo)] = 10, [sizeof(Bar)] = 11];  Flat out impossible?
 */
JKC99_API TypeHandle jkc99_type_array(ParseContext *ctx, TypeHandle base, Expr *countExpr, struct Initializer *init) {
    //TODO Optimise
    size_t count = 0;
    size_t i;
    JKC99Type type = {0};

    jkc99_assert(JKC99_TYPE_HANDLE_NOT_INVALID(base));

    if(countExpr) {
        ExprResult countRes;

        countRes = jkc99_eval_expr(ctx, JKC99_INVALID_TYPE_HANDLE, countExpr);
        if(countRes.kind == kExprResultSuccess) {
            if(JKC99_TYPE_HANDLE_EQ(countRes.type, ctx->type_char)) {
                count = countRes.u.char_val;
            } else if(JKC99_TYPE_HANDLE_EQ(countRes.type, ctx->type_signed_char)) {
                count = countRes.u.signed_char_val;
            } else if(JKC99_TYPE_HANDLE_EQ(countRes.type, ctx->type_signed_short)) {
                count = countRes.u.signed_short_val;
            } else if(JKC99_TYPE_HANDLE_EQ(countRes.type, ctx->type_signed_int)) {
                count = countRes.u.signed_int_val;
            } else if(JKC99_TYPE_HANDLE_EQ(countRes.type, ctx->type_signed_long)) {
                count = countRes.u.signed_long_val;
            } else if(JKC99_TYPE_HANDLE_EQ(countRes.type, ctx->type_signed_long_long)) {
                count = countRes.u.signed_long_long_val;
            } else if(JKC99_TYPE_HANDLE_EQ(countRes.type, ctx->type_unsigned_char)) {
                count = countRes.u.unsigned_char_val;
            } else if(JKC99_TYPE_HANDLE_EQ(countRes.type, ctx->type_unsigned_short)) {
                count = countRes.u.unsigned_short_val;
            } else if(JKC99_TYPE_HANDLE_EQ(countRes.type, ctx->type_unsigned_int)) {
                count = countRes.u.unsigned_int_val;
            } else if(JKC99_TYPE_HANDLE_EQ(countRes.type, ctx->type_unsigned_long)) {
                count = countRes.u.unsigned_long_val;
            } else if(JKC99_TYPE_HANDLE_EQ(countRes.type, ctx->type_unsigned_long_long)) {
                count = countRes.u.unsigned_long_long_val;
            } else {
                jkc99_assert(false);
            }
        }
    } else if(init) {
        jkc99_assert(init->kind == kInitializerList);

        InitializerList *list = &init->u.list;
        for(i = 0; i < list->count; ++i) {
            if(list->designations[i].count) {
                break;
            }
        }

        if(i == list->count) {
            count = list->count;
        }
    }

    for(i = 0; i < da_count(ctx->types); i++) {
        JKC99Type *t = ctx->types + i;
        if(t->kind == kTypeArray && JKC99_TYPE_HANDLE_EQ(t->u.a.base, base)) {
            if((countExpr && t->u.a.countExpr == countExpr) || (count && t->u.a.count == count)) {
                return JKC99_INDEX_TO_TYPE_HANDLE(i);
            }
        }
    }

    type.kind = kTypeArray;
    type.u.a.base = base;
    type.u.a.countExpr = countExpr;
    type.u.a.count = count;
    type.u.a.init = init;
    da_push(ctx->types, type);

    return JKC99_INDEX_TO_TYPE_HANDLE(da_count(ctx->types)-1);
}

JKC99_API TypeHandle jkc99_type_array_vla(ParseContext *ctx, TypeHandle base, Expr *countExpr) {
    //TODO Optimise
    size_t i;
    JKC99Type type = {0};

    jkc99_assert(JKC99_TYPE_HANDLE_NOT_INVALID(base));

    for(i = 0; i < da_count(ctx->types); i++) {
        JKC99Type *t = ctx->types + i;
        if(t->kind == kTypeArray && JKC99_TYPE_HANDLE_EQ(t->u.a.base, base) && t->u.a.isVLA) {
            return JKC99_INDEX_TO_TYPE_HANDLE(i);
        }
    }

    type.kind = kTypeArray;
    type.u.a.isVLA = true;
    type.u.a.base = base;
    type.u.a.countExpr = countExpr;
    da_push(ctx->types, type);

    return JKC99_INDEX_TO_TYPE_HANDLE(da_count(ctx->types)-1);
}

JKC99_API TypeHandle   jkc99_type_bitfield(ParseContext *ctx, TypeHandle base, Expr *bitCountExpr) {
    //TODO Optimise
    size_t i;
    JKC99Type type = {0};
    int bcount = 0;
    ExprResult bitCountRes = {0};

    jkc99_assert(bitCountExpr);
    jkc99_assert(jkc99_type_is_integer(ctx, base));

    bitCountRes = jkc99_eval_expr(ctx, JKC99_INVALID_TYPE_HANDLE, bitCountExpr);
    if(bitCountRes.kind == kExprResultSuccess) {
        unsigned long long count = 0;

        jkc99_assert(jkc99_type_is_integer(ctx, bitCountRes.type));
        if(JKC99_TYPE_HANDLE_EQ(bitCountRes.type, ctx->type_char)) {
            count = bitCountRes.u.char_val;
        } else if(JKC99_TYPE_HANDLE_EQ(bitCountRes.type, ctx->type_signed_char)) {
            count = bitCountRes.u.signed_char_val;
        } else if(JKC99_TYPE_HANDLE_EQ(bitCountRes.type, ctx->type_signed_short)) {
            count = bitCountRes.u.signed_short_val;
        } else if(JKC99_TYPE_HANDLE_EQ(bitCountRes.type, ctx->type_signed_int)) {
            count = bitCountRes.u.signed_int_val;
        } else if(JKC99_TYPE_HANDLE_EQ(bitCountRes.type, ctx->type_signed_long)) {
            count = bitCountRes.u.signed_long_val;
        } else if(JKC99_TYPE_HANDLE_EQ(bitCountRes.type, ctx->type_signed_long_long)) {
            count = bitCountRes.u.signed_long_long_val;
        } else if(JKC99_TYPE_HANDLE_EQ(bitCountRes.type, ctx->type_unsigned_char)) {
            count = bitCountRes.u.unsigned_char_val;
        } else if(JKC99_TYPE_HANDLE_EQ(bitCountRes.type, ctx->type_unsigned_short)) {
            count = bitCountRes.u.unsigned_short_val;
        } else if(JKC99_TYPE_HANDLE_EQ(bitCountRes.type, ctx->type_unsigned_int)) {
            count = bitCountRes.u.unsigned_int_val;
        } else if(JKC99_TYPE_HANDLE_EQ(bitCountRes.type, ctx->type_unsigned_long)) {
            count = bitCountRes.u.unsigned_long_val;
        } else if(JKC99_TYPE_HANDLE_EQ(bitCountRes.type, ctx->type_unsigned_long_long)) {
            count = bitCountRes.u.unsigned_long_long_val;
        } else {
            jkc99_assert(false);
        }
        jkc99_assert(count <= INT_MAX);

        bcount = (int)count;
        for(i = 0; i < da_count(ctx->types); i++) {
            JKC99Type *t = ctx->types + i;
            if(t->kind == kTypeBitField && JKC99_TYPE_HANDLE_EQ(t->u.b.base, base) && !t->u.b.bitCountExpr && t->u.b.bitCount == bcount) {
                return JKC99_INDEX_TO_TYPE_HANDLE(i);
            }
        }

        bitCountExpr = NULL;
    } else if(bitCountRes.kind == kExprResultUnableToResolveAtCompileTime) {
        for(i = 0; i < da_count(ctx->types); i++) {
            JKC99Type *t = ctx->types + i;
            if(t->kind == kTypeBitField && JKC99_TYPE_HANDLE_EQ(t->u.b.base, base) && t->u.b.bitCountExpr == bitCountExpr) {
                return JKC99_INDEX_TO_TYPE_HANDLE(i);
            }
        }
    } else {
        jkc99_log_error_src(ctx, &bitCountExpr->src, "Unable to evaluate bit count expression");
    }

    type.kind = kTypeBitField;
    type.u.b.base = base;
    type.u.b.bitCountExpr = bitCountExpr;
    type.u.b.bitCount = bcount;

    da_push(ctx->types, type);

    return JKC99_INDEX_TO_TYPE_HANDLE(da_count(ctx->types)-1);
}

JKC99_API TypeHandle jkc99_type_func(ParseContext *ctx, TypeHandle returnType, size_t paramCount, TypeHandle *paramTypes, bool hasEllipsis) {
    //TODO Optimise
    size_t i;
    JKC99Type type = {0};

    jkc99_assert(JKC99_TYPE_HANDLE_NOT_INVALID(returnType));

    for(i = 0; i < da_count(ctx->types); i++) {
        JKC99Type *t = ctx->types + i;
        if(t->kind == kTypeFunc && JKC99_TYPE_HANDLE_EQ(t->u.f.returnType, returnType) && t->u.f.paramCount == paramCount && t->u.f.hasEllipsis == hasEllipsis) {
            size_t j;
            for(j = 0; j < t->u.f.paramCount; j++) {
                if(JKC99_TYPE_HANDLE_NEQ(t->u.f.paramTypes[j], paramTypes[j])) {
                    break;
                }
            }
            if(j == t->u.f.paramCount) {
                return JKC99_INDEX_TO_TYPE_HANDLE(i);
            }
        }
    }

    type.kind = kTypeFunc;
    type.u.f.returnType = returnType;
    type.u.f.hasEllipsis = hasEllipsis;
    type.u.f.paramCount = paramCount;
    type.u.f.paramTypes = arena_alloc(&ctx->arena, paramCount*sizeof(TypeHandle));
    memcpy(type.u.f.paramTypes, paramTypes, paramCount*sizeof(TypeHandle));

    da_push(ctx->types, type);

    return JKC99_INDEX_TO_TYPE_HANDLE(da_count(ctx->types)-1);
}

JKC99_API TypeHandle   jkc99_type_vector(ParseContext *ctx, TypeHandle base, int bytes) {
    //TODO Optimise
    size_t i;
    JKC99Type type = {0};

    jkc99_assert(JKC99_TYPE_HANDLE_NOT_INVALID(base));
    jkc99_assert(jkc99_type_is_arithmetic(ctx, base));

    for(i = 0; i < da_count(ctx->types); i++) {
        JKC99Type *t = ctx->types + i;
        if(t->kind == kTypeVector && JKC99_TYPE_HANDLE_EQ(t->u.v.base, base) && t->u.v.bytes == bytes) {
            return JKC99_INDEX_TO_TYPE_HANDLE(i);
        }
    }

    type.kind = kTypeVector;
    type.u.v.base = base;
    type.u.v.bytes = bytes;

    da_push(ctx->types, type);

    return JKC99_INDEX_TO_TYPE_HANDLE(da_count(ctx->types)-1);
}

JKC99_API size_t       jkc99_type_count(ParseContext *ctx) {
    return da_count(ctx->types);
}

JKC99_API TypeHandle   jkc99_type_first(UNUSED ParseContext *ctx) {
    return JKC99_INDEX_TO_TYPE_HANDLE(0);
}

JKC99_API TypeHandle   jkc99_type_next(ParseContext *ctx, TypeHandle ref, size_t count) {
    size_t index = JKC99_TYPE_HANDLE_TO_INDEX(ref) + count;
    jkc99_assert(JKC99_TYPE_HANDLE_TO_INDEX(ref) < da_count(ctx->types));
    return (index < da_count(ctx->types) ? JKC99_INDEX_TO_TYPE_HANDLE(index) : JKC99_INVALID_TYPE_HANDLE);
}


JKC99_API void jkc99_type_add_user(ParseContext *ctx, TypeHandle handle, size_t index) {
    JKC99Type t;
    jkc99_assert(JKC99_TYPE_HANDLE_NOT_INVALID(handle));
    jkc99_type_get(ctx, handle, &t);
    da_push(t.users, index);
    t.userCount = da_count(t.users);
    jkc99_type_set(ctx, handle, &t);
}

JKC99_API void jkc99_type_get(ParseContext *ctx, TypeHandle handle, JKC99Type *type) {
    jkc99_assert(JKC99_TYPE_HANDLE_TO_INDEX(handle) <= da_count(ctx->types));
    *type = ctx->types[JKC99_TYPE_HANDLE_TO_INDEX(handle)];
}

JKC99_API void jkc99_type_set(ParseContext *ctx, TypeHandle handle, JKC99Type *type) {
    jkc99_assert(JKC99_TYPE_HANDLE_NOT_INVALID(handle) && JKC99_TYPE_HANDLE_TO_INDEX(handle) <= da_count(ctx->types));
    ctx->types[JKC99_TYPE_HANDLE_TO_INDEX(handle)] = *type;
}




JKC99_API void jkc99_scope_push(ParseContext *ctx) {
    da_push(ctx->scopes, (JKC99Scope){0});
}

JKC99_API void jkc99_scope_pop(ParseContext *ctx) {
    da_pop(ctx->scopes);
}

static Symbol *jkc99_parse_scope_sym_get(JKC99Scope *scope, const char *identifier) {
    //TODO Hash map
    size_t i;
    for(i = 0; i < da_count(scope->symbols); ++i) {
        if(scope->symbols[i].identifier == identifier) {
            return scope->symbols + i;
        }
    }
    return NULL;
}

JKC99_API Symbol *jkc99_sym_get_current_scope(ParseContext *ctx, const char *identifier) {
    return jkc99_parse_scope_sym_get((ctx)->scopes + (da_count((ctx)->scopes)-1), (identifier));
}

JKC99_API Symbol *jkc99_sym_get_global_index(ParseContext *ctx, size_t index) {
    Symbol *sym = NULL;
    if(da_count(ctx->scopes) && index < da_count(ctx->scopes[0].symbols)) {
        sym = ctx->scopes[0].symbols + index;
    }
    jkc99_assert(sym);
    return sym;
}

JKC99_API size_t jkc99_sym_count_global(ParseContext *ctx) {
    size_t count = 0;
    if(da_count(ctx->scopes)) {
        count = da_count(ctx->scopes[0].symbols);
    }
    return count;
}

JKC99_API void jkc99_sym_push(ParseContext *ctx, Symbol *sym) {
    size_t index = da_count(ctx->scopes) - 1;
    jkc99_assert(da_count(ctx->scopes));
    if(sym->kind != kSymbolEnumerator && index == 0) {
        jkc99_type_add_user(ctx, sym->type, da_count(ctx->scopes[index].symbols));
    }
    hook_execute(ctx, kHookNewSymbol, sym);
    if(sym) {
        da_push(ctx->scopes[index].symbols, *sym);
    }
}

JKC99_API Symbol *jkc99_sym_get(ParseContext *ctx, const char *identifier) {
    size_t i;

    for(i = da_count(ctx->scopes); i;) {
        Symbol *sym = jkc99_parse_scope_sym_get(ctx->scopes + --i, identifier);
        if(sym) {
            return sym;
        }
    }

    return NULL;
}

static TypeHandle get_derived_type_declspec(UNUSED ParseContext *ctx, TypeHandle type, UNUSED size_t declspecCount, UNUSED Declspec *declspecs) {
    /* TODO */
    return type;
}

static const char *skip_whitespace(const char *str) {
    while(*str && (
                *str == ' '  ||
                *str == '\t' ||
                *str == '\n' ||
                *str == '\r' ||
                *str == '\v' ||
                *str == '\f' )) {
        ++str;
    }
    return str;
}

static TypeHandle get_derived_type_attribute(ParseContext *ctx, TypeHandle type, size_t attributeCount, Attribute *attributes) {
    if(JKC99_TYPE_HANDLE_INVALID(type) || !attributeCount) {
        return type;
    }

    for(size_t i = 0; i < attributeCount; ++i) {
        Attribute *attr = attributes + i;
        const char *str = attr->strBegin;
        str = skip_whitespace(str);
        while(*str == '(') {
            str++;
        }
        while(str < attr->strEnd) {
            str = skip_whitespace(str);
            if(str[0] == '_' && str[1] == '_') {
                str += 2;
            }
            if(strncmp(str, "vector_size", 11) == 0) {
                str += 11;
                if(str[0] == '_' && str[1] == '_') {
                    str += 2;
                }
                str = skip_whitespace(str);
                if(*str == '(') {
                    int bytes;
                    str++;
                    str = skip_whitespace(str);
                    bytes = atoi(str);
                    if(bytes > 0) {
                        int b = bytes;
                        _Bool isPowerOfTwo = true;
                        while(b != 1) {
                            if((b%2) != 0) {
                                isPowerOfTwo = false;
                                break;
                            }
                            b /= 2;
                        }
                        if(isPowerOfTwo) {
                            if(jkc99_type_is_arithmetic(ctx, type)) {
                                type = jkc99_type_vector(ctx, type, bytes);
                            } else {
                                jkc99_log_error(ctx, "Base type for vector_size is not a scalar type");
                            }
                        } else {
                            jkc99_log_error(ctx, "Byte count for vector_size is not a power of two");
                        }
                    } else {
                        jkc99_log_error(ctx, "Unable to parse byte count for vector_size");
                    }

                    while(str < attr->strEnd && *str != ')') {
                        str++;
                    }
                } else {
                    jkc99_log_error(ctx, "Expected '(' after vector_size");
                }
            } else {
                while(str < attr->strEnd && *str != ',') {
                    str++;
                }
                str++;
            }
        }
    }

    return type;
}


JKC99_API void jkc99_declaration_specifiers_get_type_storage_class_and_function_specifier(ParseContext *ctx, size_t specifierCount, DeclarationSpecifier *specifiers, TypeHandle *handle, StorageClassKind *sclass, FunctionSpecifier *fspec) {
    size_t i;
    int qual = 0;
    *handle = JKC99_INVALID_TYPE_HANDLE;
    if(sclass) {
        *sclass = 0;
    }
    if(fspec) {
        *fspec = (FunctionSpecifier){0};
    }

    for(i = 0; i < specifierCount; ++i) {
        DeclarationSpecifier *spec = specifiers + i;
        if(spec->kind == kDeclarationSpecifierStorageClassSpecifier) {
            jkc99_assert(sclass && *sclass == kStorageClassNone);
            *sclass = spec->u.storageClass.kind;
        } else if(spec->kind == kDeclarationSpecifierTypeSpecifier) {
            TypeSpecifier *tspec = &spec->u.typeSpecifier;
            switch(tspec->kind) {
                case kTypeSpecifierBuiltin:
                    {
#ifdef __clang__
                        if(tspec->u.t == kw[kw___builtin_va_list]) {
                            *handle = ctx->type___builtin_va_list;
                        } else {
                            jkc99_assert(false);
                        }
#else
#error "Builtin types not handled for this compiler"
#endif
                    } break;
                case kTypeSpecifierVoid:
                    {
                        *handle = ctx->type_void;
                    } break;
                case kTypeSpecifierChar:
                    {
                        if(JKC99_TYPE_HANDLE_EQ(*handle, ctx->type_signed_int)) {
                            *handle = ctx->type_signed_char;
                        } else if(JKC99_TYPE_HANDLE_EQ(*handle, ctx->type_unsigned_int)) {
                            *handle = ctx->type_unsigned_char;
                        } else {
                            jkc99_assert(JKC99_TYPE_HANDLE_INVALID(*handle));
                            *handle = ctx->type_char;
                        }
                    } break;
                case kTypeSpecifierShort:
                    {
                        if(JKC99_TYPE_HANDLE_EQ(*handle, ctx->type_signed_int) || JKC99_TYPE_HANDLE_INVALID(*handle)) {
                            *handle = ctx->type_signed_short;
                        } else if(JKC99_TYPE_HANDLE_EQ(*handle, ctx->type_unsigned_int)) {
                            *handle = ctx->type_unsigned_short;
                        } else {
                            jkc99_assert(false);
                        }
                    } break;
                case kTypeSpecifierInt:
                    {
                        if(JKC99_TYPE_HANDLE_EQ(*handle, ctx->type_signed_int) || JKC99_TYPE_HANDLE_INVALID(*handle)) {
                            *handle = ctx->type_signed_int;
                        } else if(JKC99_TYPE_HANDLE_EQ(*handle, ctx->type_unsigned_int)) {
                            *handle = ctx->type_unsigned_int;
                        } else {
                            jkc99_assert(JKC99_TYPE_HANDLE_EQ(*handle, ctx->type_signed_short)
                                    ||JKC99_TYPE_HANDLE_EQ(*handle, ctx->type_unsigned_short)
                                    ||JKC99_TYPE_HANDLE_EQ(*handle, ctx->type_signed_long)
                                    ||JKC99_TYPE_HANDLE_EQ(*handle, ctx->type_unsigned_long)
                                    ||JKC99_TYPE_HANDLE_EQ(*handle, ctx->type_signed_long_long)
                                    ||JKC99_TYPE_HANDLE_EQ(*handle, ctx->type_unsigned_long_long)
                                    );
                        }
                    } break;
                case kTypeSpecifierLong:
                    {
                        if(JKC99_TYPE_HANDLE_EQ(*handle, ctx->type_signed_long)) {
                            *handle = ctx->type_signed_long_long;
                        } else if(JKC99_TYPE_HANDLE_EQ(*handle, ctx->type_unsigned_long)) {
                            *handle = ctx->type_unsigned_long_long;
                        } else if(JKC99_TYPE_HANDLE_EQ(*handle, ctx->type_signed_int) || JKC99_TYPE_HANDLE_INVALID(*handle)) {
                            *handle = ctx->type_signed_long;
                        } else if(JKC99_TYPE_HANDLE_EQ(*handle, ctx->type_unsigned_int)) {
                            *handle = ctx->type_unsigned_long;
                        } else if(JKC99_TYPE_HANDLE_EQ(*handle, ctx->type_double)) {
                            *handle = ctx->type_long_double;
                        } else if(JKC99_TYPE_HANDLE_EQ(*handle, ctx->type_double__Complex)) {
                            /* TODO We should actually check if the double _Complex is explicit (error) or by default */
                            *handle = ctx->type_long_double__Complex;
                        } else {
                            jkc99_assert(false);
                        }
                    } break;
                case kTypeSpecifierFloat:
                    {
                        if(JKC99_TYPE_HANDLE_INVALID(*handle) ||
                                JKC99_TYPE_HANDLE_EQ(*handle, ctx->type_float)) {
                            *handle = ctx->type_float;
                        } else if(JKC99_TYPE_HANDLE_EQ(*handle, ctx->type_double__Complex)) {
                            /* TODO We should actually check if the double _Complex is explicit (error) or by default */
                            *handle = ctx->type_float__Complex;
                        } else {
                            jkc99_assert(false); /* TODO Error */
                        }
                    } break;
                case kTypeSpecifierDouble:
                    {
                        if(JKC99_TYPE_HANDLE_EQ(*handle, ctx->type_signed_long)) {
                            *handle = ctx->type_long_double;
                        } else if(JKC99_TYPE_HANDLE_INVALID(*handle) || 
                                JKC99_TYPE_HANDLE_EQ(*handle, ctx->type_double)) {
                            *handle = ctx->type_double;
                        } else if(JKC99_TYPE_HANDLE_EQ(*handle, ctx->type_double__Complex)) {
                            *handle = ctx->type_double__Complex;
                        } else {
                            jkc99_assert(false); /* TODO Error */
                        }
                    } break;
                case kTypeSpecifierSigned:
                    {
                        if(JKC99_TYPE_HANDLE_INVALID(*handle)) {
                            *handle = ctx->type_signed_int;
                        } else if(JKC99_TYPE_HANDLE_EQ(*handle, ctx->type_char)) {
                            *handle = ctx->type_signed_char;
                        } else {
                            jkc99_assert(false);
                        }
                    } break;
                case kTypeSpecifierUnsigned:
                    {
                        if(JKC99_TYPE_HANDLE_INVALID(*handle)) {
                            *handle = ctx->type_unsigned_int;
                        } else {
                            if(jkc99_type_is_signed(ctx, *handle)) {
                                JKC99Type t;
                                jkc99_type_get(ctx, *handle, &t);
                                *handle = t.u.i.correspondingType;
                                jkc99_assert(JKC99_TYPE_HANDLE_NOT_INVALID(*handle));
                            } else if(JKC99_TYPE_HANDLE_EQ(*handle, ctx->type_char)) {
                                *handle = ctx->type_unsigned_char;
                            } else {
                                jkc99_assert(false);
                            }
                        }
                    } break;
                case kTypeSpecifier_Bool:
                    {
                        jkc99_assert(JKC99_TYPE_HANDLE_INVALID(*handle));
                        *handle = ctx->type__Bool;
                    } break;
                case kTypeSpecifier_Complex:
                    {
                        if(JKC99_TYPE_HANDLE_EQ(*handle, ctx->type_long_double)) {
                            *handle = ctx->type_long_double__Complex;
                        } else if(JKC99_TYPE_HANDLE_EQ(*handle, ctx->type_float)) {
                            *handle = ctx->type_float__Complex;
                        } else if(JKC99_TYPE_HANDLE_EQ(*handle, ctx->type_double)) {
                            jkc99_assert(JKC99_TYPE_HANDLE_INVALID(*handle) || JKC99_TYPE_HANDLE_EQ(*handle, ctx->type_double)); // TODO Error
                            *handle = ctx->type_double__Complex;
                        }
                    } break;
                case kTypeSpecifierStruct:
                    {
                        jkc99_assert(JKC99_TYPE_HANDLE_INVALID(*handle));
                        *handle = jkc99_type_struct(ctx, tspec->u.su.identifier);
                        goto define_struct_or_union;
                    } break;
                case kTypeSpecifierUnion:
                    {
                        jkc99_assert(JKC99_TYPE_HANDLE_INVALID(*handle));
                        *handle = jkc99_type_union(ctx, tspec->u.su.identifier);
define_struct_or_union:
                        if(tspec->u.su.definedHere) {
                            JKC99Type type;
                            jkc99_type_get(ctx, *handle, &type);
                            jkc99_assert((type.kind == kTypeStruct && tspec->kind == kTypeSpecifierStruct)
                                    || (type.kind == kTypeUnion && tspec->kind == kTypeSpecifierUnion));
                            if(type.u.su.memberCount) {
                                //TODO Handle previously defined
                                jkc99_assert(false);
                            } else {
                                size_t i;
                                jkc99_assert(type.u.su.members == NULL);
                                for(i = 0; i < tspec->u.su.memberDeclarationCount; i++) {
                                    size_t j;
                                    StructDeclaration *mdecl;

                                    mdecl = tspec->u.su.memberDeclarations[i];
                                    for(j = 0; j < mdecl->declaratorCount; j++) {
                                        StructUnionMember m = {0};
                                        StructDeclarator *sd = mdecl->declarators + j;

                                        if(sd->declarator) {
                                            m.type = jkc99_declarator_get_type_and_name(ctx, sd->declarator, mdecl->type, &m.identifier, NULL);
                                        } else {
                                            m.type = mdecl->type;
                                            m.identifier = NULL;
                                        }
                                        if(sd->bitCountExpr) {
                                            m.type = jkc99_type_bitfield(ctx, m.type, sd->bitCountExpr);
                                        }

                                        da_push(type.u.su.members, m);
                                    }
                                    type.u.su.memberCount = da_count(type.u.su.members);
                                    type.u.su.specifier = &tspec->u.su;
                                    jkc99_type_set(ctx, *handle, &type);
                                }
                            }
                            if(tspec->kind == kTypeSpecifierStruct) {
                                hook_execute(ctx, kHookNewTypeStruct, handle);
                            } else {
                                jkc99_assert(tspec->kind == kTypeSpecifierUnion);
                                hook_execute(ctx, kHookNewTypeUnion, handle);
                            }
                        }
                    } break;
                case kTypeSpecifierEnum:
                    {
                        jkc99_assert(JKC99_TYPE_HANDLE_INVALID(*handle));

                        *handle = jkc99_type_enum(ctx, tspec->u.e.identifier);

                        if(tspec->u.e.definedHere) {
                            JKC99Type type;
                            jkc99_type_get(ctx, *handle, &type);
                            jkc99_assert(type.kind == kTypeEnum && tspec->kind == kTypeSpecifierEnum);
                            if(type.u.e.enumeratorCount) {
                                //TODO Handle previously defined/error
                                jkc99_assert(false);
                            } else {
                                type.u.e.tag = tspec->u.e.identifier;
                                type.u.e.enumeratorCount = tspec->u.e.enumeratorCount;
                                type.u.e.enumerators = tspec->u.e.enumerators;

                                for(size_t j = 0; j < type.u.e.enumeratorCount; j++) {
                                    Enumerator *e;
                                    Symbol *oldSym;

                                    e = type.u.e.enumerators + j;
                                    oldSym = jkc99_sym_get_current_scope(ctx, e->identifier);

                                    if(oldSym) {
                                        jkc99_log_error_src(ctx, &e->src, "Redefinition of %s", e->identifier);
                                    } else {
                                        Symbol sym = { .kind = kSymbolEnumerator };
                                        sym.identifier = e->identifier;
                                        sym.type = *handle;
                                        sym.u.enumeratorIndex = j;

                                        jkc99_sym_push(ctx, &sym);
                                        //da_push(scope->symbols, sym);
                                    }
                                }

                                type.u.e.specifier = &tspec->u.e;
                                jkc99_type_set(ctx, *handle, &type);
                            }
                        }
                    } break;
                case kTypeSpecifierTypedefName:
                    {
                        Symbol *sym;
                        sym = jkc99_sym_get(ctx, tspec->u.t);
                        jkc99_assert(sym->kind == kSymbolType);
                        *handle = sym->type;
                    } break;
                case kTypeSpecifier__int8:
                    {
                        if(JKC99_TYPE_HANDLE_EQ(*handle, ctx->type_unsigned_int)) {
                            *handle = ctx->type_unsigned_char;
                        } else if(JKC99_TYPE_HANDLE_EQ(*handle, ctx->type_signed_int) || JKC99_TYPE_HANDLE_INVALID(*handle)) {
                            *handle = ctx->type_signed_char;
                        } else {
                            jkc99_log_error(ctx, "Unexpected __int8");
                        }
                    } break;
                case kTypeSpecifier__int16:
                    {
                        if(JKC99_TYPE_HANDLE_EQ(*handle, ctx->type_unsigned_int)) {
                            *handle = ctx->type_unsigned_short;
                        } else if(JKC99_TYPE_HANDLE_EQ(*handle, ctx->type_signed_int) || JKC99_TYPE_HANDLE_INVALID(*handle)) {
                            *handle = ctx->type_signed_short;
                        } else {
                            jkc99_log_error(ctx, "Unexpected __int16");
                        }
                    } break;
                case kTypeSpecifier__int32:
                    {
                        if(JKC99_TYPE_HANDLE_EQ(*handle, ctx->type_unsigned_int)) {
                            *handle = ctx->type_unsigned_int;
                        } else if(JKC99_TYPE_HANDLE_EQ(*handle, ctx->type_signed_int) || JKC99_TYPE_HANDLE_INVALID(*handle)) {
                            *handle = ctx->type_signed_int;
                        } else {
                            jkc99_log_error(ctx, "Unexpected __int32");
                        }
                    } break;
                case kTypeSpecifier__int64:
                    {
                        if(JKC99_TYPE_HANDLE_EQ(*handle, ctx->type_unsigned_int)) {
                            *handle = ctx->type_unsigned_long_long;
                        } else if(JKC99_TYPE_HANDLE_EQ(*handle, ctx->type_signed_int) || JKC99_TYPE_HANDLE_INVALID(*handle)) {
                            *handle = ctx->type_signed_long_long;
                        } else {
                            jkc99_log_error(ctx, "Unexpected __int64");
                        }
                    } break;
                default: jkc99_assert(false); break;
            }
        } else if(spec->kind == kDeclarationSpecifierTypeQualifier) {
            qual |= spec->u.typeQualifier.kind;
        } else if(spec->kind == kDeclarationSpecifierFunctionSpecifier) {
            *fspec = spec->u.functionSpecifier;
        } else if(spec->kind == kDeclarationSpecifierAttribute) {
            *handle = get_derived_type_attribute(ctx, *handle, 1, &spec->u.attribute);
        } else if(spec->kind == kDeclarationSpecifierDeclspec) {
            *handle = get_derived_type_declspec(ctx, *handle, 1, &spec->u.declspec);
        } else if(spec->kind == kDeclarationSpecifierCallingConvention) {
            /* TODO */
        } else {
            jkc99_assert(false);
        }
    }

    if(JKC99_TYPE_HANDLE_NOT_INVALID(*handle) && qual) {
        *handle = jkc99_type_qualified(ctx, *handle, qual);
    }
}

static int collapse_qualifier_list(size_t count, TypeQualifier *qualifiers) {
    int qual = 0;
    for(size_t i = 0; i < count; ++i) {
        qual |= qualifiers->kind;
    }
    return qual;
}

JKC99_API TypeHandle jkc99_abstract_declarator_get_type(ParseContext *ctx, AbstractDeclarator *d, TypeHandle type) {
    size_t i;

    jkc99_assert(JKC99_TYPE_HANDLE_NOT_INVALID(type));

    for(i = 0; i < d->pointerCount; i++) {
        Pointer *ptr = d->pointers + i;
        type = jkc99_type_ptr(ctx, type);
        if(ptr->qualifierCount) {
            type = jkc99_type_qualified(ctx, type, collapse_qualifier_list(ptr->qualifierCount, ptr->qualifiers));
        }
    }

    if(d->direct) {
        type = jkc99_direct_abstract_declarator_get_type(ctx, d->direct, type);
    }

    if(d->beforeAttributeCount) {
        type = get_derived_type_attribute(ctx, type, d->beforeAttributeCount, d->beforeAttributes);
    }
    if(d->afterAttributeCount) {
        type = get_derived_type_attribute(ctx, type, d->afterAttributeCount, d->afterAttributes);
    }

    return type;
}

JKC99_API TypeHandle jkc99_direct_abstract_declarator_get_type(ParseContext *ctx, DirectAbstractDeclarator *dd, TypeHandle typeHandle) {
    jkc99_assert(dd);

    switch(dd->kind) {
        case kDirectAbstractDeclaratorDeclarator:
            {
                //if(dd->u.declarator && dd->u.declarator->direct) {
                if(dd->u.declarator) {
                    return jkc99_abstract_declarator_get_type(ctx, dd->u.declarator, typeHandle);
                }
            } break;
        case kDirectAbstractDeclaratorArray:
            {
                if(dd->u.array.isVLA) {
                    typeHandle = jkc99_type_array_vla(ctx, typeHandle, NULL);
                } else {
                    /* TODO Determine if count expression is integer constant expression */
                    typeHandle = jkc99_type_array(ctx, typeHandle, dd->u.array.expr, NULL);
                }
                if(dd->u.array.direct) {
                    return jkc99_direct_abstract_declarator_get_type(ctx, dd->u.array.direct, typeHandle);
                } else {
                    return typeHandle;
                }
            } break;
        case kDirectAbstractDeclaratorFunc:
            {
                TypeHandle *paramTypes = NULL;

                for(size_t i=0; i < dd->u.func.count; ++i) {
                    ParameterDeclaration *p = dd->u.func.u.parameters[i];
                    TypeHandle paramType = p->type;
                    if(p->declarator) {
                        paramType = jkc99_declarator_get_type_and_name(ctx, p->declarator, p->type, NULL, NULL);
                    } else if(p->abstractDeclarator) {
                        paramType = jkc99_abstract_declarator_get_type(ctx, p->abstractDeclarator, p->type);
                    }
                    da_push(paramTypes, paramType);
                }

                jkc99_assert(da_count(paramTypes) == dd->u.func.count);
                typeHandle = jkc99_type_func(ctx, typeHandle, dd->u.func.count, paramTypes, dd->u.func.hasEllipsis);
                da_free(paramTypes);

                if(dd->u.func.direct) {
                    return jkc99_direct_abstract_declarator_get_type(ctx, dd->u.func.direct, typeHandle);
                } else {
                    return typeHandle;
                }
            } break;
        default: jkc99_assert(false); break;
    }

    return JKC99_INVALID_TYPE_HANDLE;
}


JKC99_API TypeName *jkc99_type_name(ParseContext *ctx, Source *src, size_t specifierCount, DeclarationSpecifier *specifiers, AbstractDeclarator *declarator) {
    TypeHandle handle = JKC99_INVALID_TYPE_HANDLE;
    StorageClassKind storageClass = 0;
    FunctionSpecifier fspec = {0};

    TypeName *tn = arena_alloc(&ctx->arena, sizeof(TypeName));

    tn->src = src ? *src : (Source){0};
    jkc99_parse_empty_directives(ctx, tn);
    tn->specifierCount = specifierCount;
    tn->specifiers = specifiers;
    tn->abstractDeclarator = declarator;

    jkc99_declaration_specifiers_get_type_storage_class_and_function_specifier(ctx, tn->specifierCount, tn->specifiers, &handle, &storageClass, &fspec);

    jkc99_assert(storageClass == kStorageClassNone);
    jkc99_assert(fspec.kind == kFunctionSpecifierNone);
    jkc99_assert(JKC99_TYPE_HANDLE_NOT_INVALID(handle));

    tn->type = handle;

    if(JKC99_TYPE_HANDLE_NOT_INVALID(tn->type) && declarator)  {
        tn->type = jkc99_abstract_declarator_get_type(ctx, declarator, tn->type);
    }

    return tn;
}

JKC99_API DirectAbstractDeclarator *jkc99_direct_abstract_declarator_alloc(ParseContext *ctx, Source *src, DirectAbstractDeclaratorKind kind) {
    DirectAbstractDeclarator *direct = arena_alloc(&ctx->arena, sizeof(DirectAbstractDeclarator));
    direct->kind = kind;
    direct->src = src ? *src : (Source){0};
    jkc99_parse_empty_directives(ctx, direct);
    return direct;
}

JKC99_API DirectAbstractDeclarator *jkc99_direct_abstract_declarator_declarator(ParseContext *ctx, Source *src, AbstractDeclarator *declarator) {
    DirectAbstractDeclarator *d = jkc99_direct_abstract_declarator_alloc(ctx, src, kDirectAbstractDeclaratorDeclarator);
    d->u.declarator = declarator;
    return d;
}

JKC99_API DirectAbstractDeclarator *jkc99_direct_abstract_declarator_array_vla(ParseContext *ctx, Source *src, DirectAbstractDeclarator *direct) {
    DirectAbstractDeclarator *d = jkc99_direct_abstract_declarator_alloc(ctx, src, kDirectAbstractDeclaratorArray);
    d->u.array.direct = direct;
    d->u.array.isVLA = true;
    return d;
}

JKC99_API DirectAbstractDeclarator *jkc99_direct_abstract_declarator_array(ParseContext *ctx, Source *src, size_t attributeCount, Attribute *attributes, DirectAbstractDeclarator *direct, size_t qualifierCount, TypeQualifier *qualifiers, bool isStatic, Expr *expr) {
    DirectAbstractDeclarator *d = jkc99_direct_abstract_declarator_alloc(ctx, src, kDirectAbstractDeclaratorArray);
    d->u.array.direct = direct;
    d->u.array.attributeCount = attributeCount;
    d->u.array.attributes = attributes;
    d->u.array.qualifierCount = qualifierCount;
    d->u.array.qualifiers = qualifiers;
    d->u.array.isStatic = isStatic;
    d->u.array.expr = expr;
    return d;
}

JKC99_API DirectAbstractDeclarator *jkc99_direct_abstract_declarator_func_params(ParseContext *ctx, Source *src, DirectAbstractDeclarator *direct, CallingConvention callingConvention, size_t paramCount, ParameterDeclaration **params, bool hasEllipsis) {
    DirectAbstractDeclarator *d = jkc99_direct_abstract_declarator_alloc(ctx, src, kDirectAbstractDeclaratorFunc);
    d->u.func.direct = direct;
    d->u.func.callingConvention = callingConvention;
    d->u.func.hasEllipsis = hasEllipsis;
    d->u.func.hasParameterList = true;
    d->u.func.count = paramCount;
    d->u.func.u.parameters = params;
    return d;
}


static bool jkc99_parse_type_qualifier(ParseContext *ctx, TypeQualifier *qual);

static inline size_t jkc99_parse_type_qualifier_list_(ParseContext *ctx, TypeQualifier **list, size_t *count) {
    TypeQualifier qual = {0};
    size_t preCount = da_count(*list);

    while(jkc99_parse_type_qualifier(ctx, &qual)) {
        da_push(*list, qual);
        qual = (TypeQualifier){0};
    }
    *count = da_count(*list);

    return (*count - preCount);
}

JKC99_API TypeQualifier *jkc99_parse_type_qualifier_list(ParseContext *ctx, size_t *count) {
    TypeQualifier *list = NULL;
    
    jkc99_parse_type_qualifier_list_(ctx, &list, count);

    return list;
}

/* TODO Adjust parameter declarators! */
JKC99_API DirectAbstractDeclarator *jkc99_parse_direct_abstract_declarator(ParseContext *ctx) {
    Source src;
    CallingConvention callingConvention = 0;
    DirectAbstractDeclarator *direct = NULL;

    jkc99_lexer_source(ctx, &src);

    if(jkc99_lexer_match(ctx, '(')) {
        AbstractDeclarator *declarator = NULL;
        jkc99_assert(callingConvention == kCallingConventionNone);
        callingConvention = jkc99_parse_calling_convention(ctx);
        declarator = jkc99_parse_abstract_declarator(ctx);
        if(declarator) {
            jkc99_lexer_require(ctx, ')');
            source_end(ctx, &src);
            direct = jkc99_direct_abstract_declarator_declarator(ctx, &src, declarator);
        } else {
            jkc99_lexer_rollback(ctx, &src);
        }
    }

    do {
        if(jkc99_lexer_match(ctx, '[')) {
            size_t attributeCount = 0;
            Attribute *attributes = NULL;
            size_t qualifierCount = 0;
            TypeQualifier *qualifiers = NULL;
            bool isStaticSize = false;

            if(jkc99_lexer_match_kw(ctx, static)) {
                isStaticSize = true;
            }

            while(jkc99_parse_type_qualifier_list_(ctx, &qualifiers, &qualifierCount) ||
                    jkc99_parse_attribute_list_(ctx, &attributes, &attributeCount));

            if(qualifierCount) {
                if(jkc99_lexer_match_kw(ctx, static)) {
                    isStaticSize = true;
                }
            }

            if(jkc99_lexer_match(ctx, '*')) {
                jkc99_lexer_require(ctx, ']');
                source_end(ctx, &src);
                direct = jkc99_direct_abstract_declarator_array_vla(ctx, &src, direct);
            } else {
                Expr *expr = jkc99_parse_expr_assignment(ctx);
                jkc99_lexer_require(ctx, ']');
                source_end(ctx, &src);
                direct = jkc99_direct_abstract_declarator_array(ctx, &src, attributeCount, attributes, direct, qualifierCount, qualifiers, isStaticSize, expr);
            }
        } else if(jkc99_lexer_match(ctx, '(')) {
            size_t count;
            bool hasEllipsis = false;
            ParameterDeclaration **parameterList = NULL;
            bool hasParameterList = jkc99_parse_parameter_list(ctx, &count, &parameterList, &hasEllipsis);
            jkc99_assert(hasParameterList);
            jkc99_lexer_require(ctx, ')');
            source_end(ctx, &src);
            direct = jkc99_direct_abstract_declarator_func_params(ctx, &src, direct, callingConvention, count, parameterList, hasEllipsis);
        } else {
            break;
        }
    } while(direct);

    if(!direct) {
        jkc99_lexer_rollback(ctx, &src);
    }

    return direct;
}

JKC99_API AbstractDeclarator *jkc99_abstract_declarator(ParseContext *ctx, Source *src, size_t beforeAttrCount, Attribute *beforeAttrs, size_t afterAttrCount, Attribute *afterAttrs, size_t ptrCount, Pointer *pointers, DirectAbstractDeclarator *dd) {
    AbstractDeclarator *d = arena_alloc(&ctx->arena, sizeof(AbstractDeclarator));

    d->src = src ? *src : (Source){0};
    jkc99_parse_empty_directives(ctx, d);
    d->pointerCount = ptrCount;
    d->pointers = pointers;
    d->direct = dd;
    d->beforeAttributeCount = beforeAttrCount;
    d->beforeAttributes = beforeAttrs;
    d->afterAttributeCount = afterAttrCount;
    d->afterAttributes = afterAttrs;

    return d;
}

JKC99_API AbstractDeclarator *jkc99_parse_abstract_declarator(ParseContext *ctx) {
    Source src = {0};
    AbstractDeclarator *ad = NULL;
    Pointer *pointers = NULL;
    size_t pointerCount;
    size_t beforeAttrCount = 0;
    size_t afterAttrCount = 0;
    DirectAbstractDeclarator *direct = NULL;

    jkc99_lexer_source(ctx, &src);

    Attribute *beforeAttrs = jkc99_parse_attribute_list(ctx, &beforeAttrCount);

    pointers = jkc99_parse_pointer(ctx, pointers);
    pointerCount = da_count(pointers);

    direct = jkc99_parse_direct_abstract_declarator(ctx);

    Attribute *afterAttrs = jkc99_parse_attribute_list(ctx, &afterAttrCount);
    if(pointerCount || direct || beforeAttrCount || afterAttrCount) {
        source_end(ctx, &src);
        ad = jkc99_abstract_declarator(ctx, &src, beforeAttrCount, beforeAttrs, afterAttrCount, afterAttrs, pointerCount, pointers, direct);
    } else {
        jkc99_lexer_rollback(ctx, &src);
    }

    return ad;
}

JKC99_API TypeName *jkc99_parse_type_name(ParseContext *ctx) {
    Source src = {0};
    TypeName *typeName = NULL;
    size_t specifierCount;
    DeclarationSpecifier *specifiers = NULL;

    jkc99_lexer_source(ctx, &src);

    specifiers = jkc99_parse_specifier_qualifier_list(ctx, &specifierCount);
    if(specifiers) {
        AbstractDeclarator *declarator;

        declarator = jkc99_parse_abstract_declarator(ctx);
        source_end(ctx, &src);
        typeName = jkc99_type_name(ctx, &src, specifierCount, specifiers, declarator);
    } else {
        jkc99_lexer_rollback(ctx, &src);
    }

    return typeName;
}


JKC99_API Designation jkc99_parse_designation(ParseContext *ctx) {
    Designation d = {0};
    do {
        Designator dr = { 0 };
        jkc99_lexer_source(ctx, &dr.src);
        if(jkc99_lexer_match(ctx, '[')) {
            dr.kind = kDesignatorIndex;
            dr.u.indexExpr = jkc99_parse_expr_constant(ctx);
            jkc99_lexer_require(ctx, ']');
            source_end(ctx, &dr.src);
            move_directives(dr.u.indexExpr, &dr);
        } else if(jkc99_lexer_match(ctx, '.')) {
            dr.kind = kDesignatorField;
            dr.u.field = jkc99_parse_identifier(ctx);
            source_end(ctx, &dr.src);
        } else {
            break;
        }
        jkc99_assert(dr.kind);
        da_push(d.list, dr);
    } while(1);

    d.count = da_count(d.list);
    if(d.count) {
        jkc99_lexer_require(ctx, '=');
    }

    return d;
}



JKC99_API const char *jkc99_parse_identifier(ParseContext *ctx) {
    const char *identifier = NULL;

    if(jkc99_lexer_is(ctx, CLEX_id)) {
        identifier = jkc99_str_intern(ctx, ctx->lexer->string);
        jkc99_lexer_next(ctx);
    }

    return identifier;
}

JKC99_API const char *jkc99_parse_string(ParseContext *ctx) {
    const char *str = NULL;
    if(jkc99_lexer_is(ctx, CLEX_dqstring)) {
        str = jkc99_str_intern(ctx, ctx->lexer->string);
        jkc99_lexer_next(ctx);
    }

    return str;
}

JKC99_API Expr *jkc99_expr_alloc(ParseContext *ctx, Source *src, ExprKind kind) {
    Expr *expr = arena_alloc(&ctx->arena, sizeof(Expr));
    expr->kind = kind;
    expr->src = src ? *src : (Source){0};
    jkc99_parse_empty_directives(ctx, expr);
    return expr;
}

JKC99_API Expr *jkc99_expr_expr(ParseContext *ctx, Source *src, Expr *expr) {
    Expr *e = jkc99_expr_alloc(ctx, src, kExprPrimary);
    e->u.primary.kind = kExprPrimaryExpr;
    e->u.primary.u.expr = expr;
    return e;
}

JKC99_API Expr *jkc99_expr_string(ParseContext *ctx, Source *src, const char *str) {
    Expr *e = jkc99_expr_alloc(ctx, src, kExprPrimary);
    e->u.primary.kind = kExprPrimaryStringLiteral;
    e->u.primary.u.str = str;
    return e;
}

static void constant_set_suffix(Constant *c, size_t suffixLen, const char *suffix) {
    c->suffix[0] = '\0';
    c->suffix[1] = '\0';
    c->suffix[2] = '\0';

    jkc99_assert(suffixLen <= 3);
    for(size_t i = 0; i < suffixLen; i++) {
        c->suffix[i] = suffix[i];
    }
}

//TODO Different types for literals
JKC99_API Expr *jkc99_expr_int(ParseContext *ctx, Source *src, const char *str, unsigned long long intlit, ConstantRepresentation repr, size_t suffixLen, const char *suffix) {
    Expr *e = jkc99_expr_alloc(ctx, src, kExprPrimary);
    e->u.primary.kind = kExprPrimaryConstant;
    e->u.primary.u.constant.kind = kConstantInteger;
    e->u.primary.u.constant.str = str;
    e->u.primary.u.constant.u.intVal = intlit; 
    e->u.primary.u.constant.representation = repr;
    constant_set_suffix(&e->u.primary.u.constant, suffixLen, suffix);
    return e;
}

//TODO Different types for literals
JKC99_API Expr *jkc99_expr_float(ParseContext *ctx, Source *src, const char *str, double floatlit, ConstantRepresentation repr, size_t suffixLen, const char *suffix) {
    Expr *e = jkc99_expr_alloc(ctx, src, kExprPrimary);
    e->u.primary.kind = kExprPrimaryConstant;
    e->u.primary.u.constant.kind = kConstantFloat;
    e->u.primary.u.constant.str = str;
    e->u.primary.u.constant.u.floatVal = floatlit;
    e->u.primary.u.constant.representation = repr;
    constant_set_suffix(&e->u.primary.u.constant, suffixLen, suffix);
    return e;
}

JKC99_API Expr *jkc99_expr_char(ParseContext *ctx, Source *src, char charlit) {
    Expr *e = jkc99_expr_alloc(ctx, src, kExprPrimary);
    e->u.primary.kind = kExprPrimaryConstant;
    e->u.primary.u.constant.kind = kConstantCharacter;
    e->u.primary.u.constant.u.charVal = charlit;
    return e;
}

JKC99_API Expr *jkc99_expr_identifier(ParseContext *ctx, Source *src, const char *identifier) {
    Expr *e = jkc99_expr_alloc(ctx, src, kExprPrimary);
    e->u.primary.kind = kExprPrimaryIdentifier;
    e->u.primary.u.identifier = identifier;
    return e;
}

JKC99_API Expr *jkc99_expr_compound(ParseContext *ctx, Source *src, TypeName *typeName, InitializerList initializerList) {
    Expr *e = jkc99_expr_alloc(ctx, src, kExprPostfix);
    e->u.postfix.kind = kExprPostfixCompound;
    e->u.postfix.u.compound.typeName = typeName;
    e->u.postfix.u.compound.initializerList = initializerList;
    return e;
}

JKC99_API Expr *jkc99_expr_index(ParseContext *ctx, Source *src, Expr *expr, Expr *indexExpr) {
    Expr *e = jkc99_expr_alloc(ctx, src, kExprPostfix);
    e->u.postfix.expr = expr;
    e->u.postfix.kind = kExprPostfixIndex;
    e->u.postfix.u.indexExpr = indexExpr;
    return e;
}

#ifdef __clang__
JKC99_API Expr *jkc99_expr_va_arg(ParseContext *ctx, Source *src, Expr *expr, Expr *list, TypeName *typeName) {
    Expr *e = jkc99_expr_alloc(ctx, src, kExprPostfix);
    e->u.postfix.expr = expr;
    e->u.postfix.kind = kExprPostfixVaArg;
    e->u.postfix.u.va_arg.list = list;
    e->u.postfix.u.va_arg.typeName = typeName;
    return e;
}
#endif

JKC99_API Expr *jkc99_expr_call(ParseContext *ctx, Source *src, Expr *expr, Expr **args) {
    Expr *e = jkc99_expr_alloc(ctx, src, kExprPostfix);
    e->u.postfix.expr = expr;
    e->u.postfix.kind = kExprPostfixCall;
    e->u.postfix.u.callArgs = args;
    return e;
}

JKC99_API Expr *jkc99_expr_dot(ParseContext *ctx, Source *src, Expr *expr, const char *identifier) {
    Expr *e = jkc99_expr_alloc(ctx, src, kExprPostfix);
    e->u.postfix.expr = expr;
    e->u.postfix.kind = kExprPostfixDot;
    e->u.postfix.u.dot = identifier;
    return e;
}

JKC99_API Expr *jkc99_expr_arrow(ParseContext *ctx, Source *src, Expr *expr, const char *identifier) {
    Expr *e = jkc99_expr_alloc(ctx, src, kExprPostfix);
    e->u.postfix.expr = expr;
    e->u.postfix.kind = kExprPostfixArrow;
    e->u.postfix.u.arrow = identifier;
    return e;
}

JKC99_API Expr *jkc99_expr_increment(ParseContext *ctx, Source *src, Expr *expr) {
    Expr *e = jkc99_expr_alloc(ctx, src, kExprPostfix);
    e->u.postfix.expr = expr;
    e->u.postfix.kind = kExprPostfixIncrement;
    return e;
}

JKC99_API Expr *jkc99_expr_decrement(ParseContext *ctx, Source *src, Expr *expr) {
    Expr *e = jkc99_expr_alloc(ctx, src, kExprPostfix);
    e->u.postfix.expr = expr;
    e->u.postfix.kind = kExprPostfixDecrement;
    return e;
}

JKC99_API Expr *jkc99_expr_unary(ParseContext *ctx, Source *src, ExprUnaryKind kind, Expr *expr) {
    Expr *e = jkc99_expr_alloc(ctx, src, kExprUnary);
    e->u.unary.kind = kind;
    e->u.unary.u.expr = expr;
    return e;
}

JKC99_API Expr *jkc99_expr_sizeof_type(ParseContext *ctx, Source *src, TypeName *typeName) {
    Expr *e = jkc99_expr_alloc(ctx, src, kExprUnary);
    e->u.unary.kind = kExprUnarySizeofType;
    e->u.unary.u.typeName = typeName;
    return e;
}

JKC99_API Expr *jkc99_expr_offsetof(ParseContext *ctx, Source *src, TypeName *typeName, const char *field) {
    Expr *e = jkc99_expr_alloc(ctx, src, kExprUnary);
    e->u.unary.kind = kExprUnaryOffsetof;
    e->u.unary.u.offsetof.typeName = typeName;
    e->u.unary.u.offsetof.field = field;
    return e;
}

JKC99_API Expr *jkc99_expr_cast(ParseContext *ctx, Source *src, TypeName *typeName, Expr *expr) {
    Expr *e = jkc99_expr_alloc(ctx, src, kExprCast);
    e->u.cast.typeName = typeName;
    e->u.cast.expr = expr;
    return e;
}

JKC99_API Expr *jkc99_expr_multiplicative(ParseContext *ctx, Source *src, ExprMultiplicativeKind kind, Expr *left, Expr *right) {
    Expr *e = jkc99_expr_alloc(ctx, src, kExprMultiplicative);
    e->u.multiplicative.kind = kind;
    e->u.multiplicative.left = left;
    e->u.multiplicative.right = right;
    return e;
}

JKC99_API Expr *jkc99_expr_additive(ParseContext *ctx, Source *src, ExprAdditiveKind kind, Expr *left, Expr *right) {
    Expr *e = jkc99_expr_alloc(ctx, src, kExprAdditive);
    e->u.additive.kind = kind;
    e->u.additive.left = left;
    e->u.additive.right = right;
    return e;
}

JKC99_API Expr *jkc99_expr_shift(ParseContext *ctx, Source *src, ExprShiftKind kind, Expr *left, Expr *right) {
    Expr *e = jkc99_expr_alloc(ctx, src, kExprShift);
    e->u.shift.kind = kind;
    e->u.shift.left = left;
    e->u.shift.right = right;
    return e;
}

JKC99_API Expr *jkc99_expr_relational(ParseContext *ctx, Source *src, ExprRelationalKind kind, Expr *left, Expr *right) {
    Expr *e = jkc99_expr_alloc(ctx, src, kExprRelational);
    e->u.relational.kind = kind;
    e->u.relational.left = left;
    e->u.relational.right = right;
    return e;
}

JKC99_API Expr *jkc99_expr_equality(ParseContext *ctx, Source *src, ExprEqualityKind kind, Expr *left, Expr *right) {
    Expr *e = jkc99_expr_alloc(ctx, src, kExprEquality);
    e->u.equality.kind = kind;
    e->u.equality.left = left;
    e->u.equality.right = right;
    return e;
}

JKC99_API Expr *jkc99_expr_and(ParseContext *ctx, Source *src, Expr *left, Expr *right) {
    Expr *e = jkc99_expr_alloc(ctx, src, kExprAnd);
    e->u.and.left = left;
    e->u.and.right = right;
    return e;
}

JKC99_API Expr *jkc99_expr_exclusive_or(ParseContext *ctx, Source *src, Expr *left, Expr *right) {
    Expr *e = jkc99_expr_alloc(ctx, src, kExprExclusiveOr);
    e->u.exclusiveOr.left = left;
    e->u.exclusiveOr.right = right;
    return e;
}

JKC99_API Expr *jkc99_expr_inclusive_or(ParseContext *ctx, Source *src, Expr *left, Expr *right) {
    Expr *e = jkc99_expr_alloc(ctx, src, kExprInclusiveOr);
    e->u.inclusiveOr.left = left;
    e->u.inclusiveOr.right = right;
    return e;
}

JKC99_API Expr *jkc99_expr_logical_and(ParseContext *ctx, Source *src, Expr *left, Expr *right) {
    Expr *e = jkc99_expr_alloc(ctx, src, kExprLogicalAnd);
    e->u.logicalAnd.left = left;
    e->u.logicalAnd.right = right;
    return e;
}

JKC99_API Expr *jkc99_expr_logical_or(ParseContext *ctx, Source *src, Expr *left, Expr *right) {
    Expr *e = jkc99_expr_alloc(ctx, src, kExprLogicalOr);
    e->u.logicalOr.left = left;
    e->u.logicalOr.right = right;
    return e;
}


JKC99_API Expr *jkc99_expr_conditional(ParseContext *ctx, Source *src, Expr *cond, Expr *left, Expr *right) {
    Expr *e = jkc99_expr_alloc(ctx, src, kExprConditional);
    e->u.conditional.cond = cond;
    e->u.conditional.left = left;
    e->u.conditional.right = right;
    return e;
}

JKC99_API Expr *jkc99_expr_assign(ParseContext *ctx, Source *src, ExprAssignmentKind kind, Expr *left, Expr *right) {
    Expr *e = jkc99_expr_alloc(ctx, src, kExprAssignment);
    e->u.assignment.kind = kind;
    e->u.assignment.left = left;
    e->u.assignment.right = right;
    return e;
}

JKC99_API Expr *jkc99_expr_comma(ParseContext *ctx, Source *src, Expr *left, Expr *right) {
    Expr *e = jkc99_expr_alloc(ctx, src, kExprComma);
    e->u.comma.left = left;
    e->u.comma.right = right;
    return e;
}

JKC99_API Expr *jkc99_parse_expr_primary(ParseContext *ctx) {
    Source src;
    Expr *expr = NULL;

    jkc99_lexer_source(ctx, &src);

    if(jkc99_lexer_match(ctx, '(')) {
        expr = jkc99_expr_expr(ctx, &src, jkc99_parse_expr(ctx));
        jkc99_lexer_require(ctx, ')');
    } else if(jkc99_lexer_is(ctx, CLEX_dqstring)) {
        char *buf = NULL;
        da_pushn(buf, ctx->lexer->string_len+1);
        memcpy(buf, ctx->lexer->string, ctx->lexer->string_len);
        buf[ctx->lexer->string_len] = '\0';

        while(jkc99_lexer_next(ctx), jkc99_lexer_is(ctx, CLEX_dqstring)) {
            da_pushn(buf, ctx->lexer->string_len);
            char *str = buf + (da_count(buf)-(ctx->lexer->string_len+1));
            memcpy(str, ctx->lexer->string, ctx->lexer->string_len);
            str[ctx->lexer->string_len] = '\0';
        }

        expr = jkc99_expr_string(ctx, &src, jkc99_str_intern(ctx, buf));

        da_free(buf);
    } else {
        if(jkc99_lexer_is(ctx, CLEX_intlit)) {
            ConstantRepresentation repr = kConstantDecimal;
            if(ctx->lexer->where_firstchar[0] == '0') {
                if( ctx->lexer->where_firstchar[1] == 'x' ||
                    ctx->lexer->where_firstchar[1] == 'X') {
                    repr = kConstantHex;
                } else {
                    repr = kConstantOctal;
                }
            }
            jkc99_assert(ctx->lexer->where_lastchar >= ctx->lexer->where_firstchar);
            expr = jkc99_expr_int(ctx, &src, jkc99_str_intern_range(ctx, ctx->lexer->where_firstchar, (size_t)(ctx->lexer->where_lastchar - ctx->lexer->where_firstchar + 1)), ctx->lexer->int_number, repr, ctx->lexer->string_len, ctx->lexer->string);
            jkc99_lexer_next(ctx);
        } else if(jkc99_lexer_is(ctx, CLEX_floatlit)) {
            ConstantRepresentation repr = kConstantDecimal;
            if(ctx->lexer->where_firstchar[0] == '0' &&
               (ctx->lexer->where_firstchar[1] == 'x' ||
                ctx->lexer->where_firstchar[1] == 'X')) {
                repr = kConstantHex;
            } else {
                repr = kConstantDecimal;
                for(size_t i = 0; i < (size_t)(ctx->lexer->where_lastchar - ctx->lexer->where_firstchar); i++) {
                    if(ctx->lexer->where_firstchar[i] == 'e' || ctx->lexer->where_firstchar[i] == 'E') {
                        repr = kConstantDecimalWithExponent;
                        break;
                    }
                }
            }
            jkc99_assert(ctx->lexer->where_lastchar >= ctx->lexer->where_firstchar);
            expr = jkc99_expr_float(ctx, &src, jkc99_str_intern_range(ctx, ctx->lexer->where_firstchar, (size_t)(ctx->lexer->where_lastchar - ctx->lexer->where_firstchar + 1)), ctx->lexer->real_number, repr, ctx->lexer->string_len, ctx->lexer->string);
            jkc99_lexer_next(ctx);
        } else if(jkc99_lexer_is(ctx, CLEX_charlit)) {
            expr = jkc99_expr_char(ctx, &src, ctx->lexer->int_number);
            jkc99_lexer_next(ctx);
        } else if(jkc99_lexer_is(ctx, CLEX_id)) {
            expr = jkc99_expr_identifier(ctx, &src, jkc99_str_intern(ctx, ctx->lexer->string));
            jkc99_lexer_next(ctx);
        }
    }

    return expr;
}

JKC99_API Initializer jkc99_parse_initializer(ParseContext *ctx) {
    Initializer init = {0};

    if(jkc99_lexer_match(ctx, '{')) {
        init.kind = kInitializerList;
        init.u.list = jkc99_parse_initializer_list(ctx);
        jkc99_lexer_require(ctx, '}');
    } else {
        Expr *expr = jkc99_parse_expr_assignment(ctx);
        if(expr) {
            init.kind = kInitializerExpr;
            init.u.expr = expr;
        }
    }

    return init;
}

JKC99_API InitializerList jkc99_parse_initializer_list(ParseContext *ctx) {
    InitializerList list = {0};

    jkc99_lexer_source(ctx, &list.src);

    do {
        Designation d;
        Initializer i;

        d = jkc99_parse_designation(ctx);
        i = jkc99_parse_initializer(ctx);

        if(i.kind) {
            da_push(list.designations, d);
            da_push(list.initializers, i);
        } else {
            jkc99_log_error(ctx, "Expected initializer");
        }
    } while(jkc99_lexer_match(ctx, ',') && !jkc99_lexer_is(ctx, '}') && !jkc99_lexer_is(ctx, CLEX_eof));

    jkc99_assert(da_count(list.designations) == da_count(list.initializers));
    list.count = da_count(list.initializers);

    source_end(ctx, &list.src);

    return list;
}

static Expr **jkc99_parse_argument_expression_list(ParseContext *ctx) {
    Expr **list = NULL;

    do {
        Expr *e = jkc99_parse_expr_assignment(ctx);
        if(e) {
            da_push(list, e);
        } else {
            break;
        }
    } while(jkc99_lexer_match(ctx, ','));

    return list;
}

JKC99_API Expr *jkc99_parse_expr_postfix(ParseContext *ctx) {
    Source src;
    Expr *expr = NULL;

    jkc99_lexer_source(ctx, &src);

    if(jkc99_lexer_match(ctx, '(')) {
        TypeName *typeName = jkc99_parse_type_name(ctx);
        if(typeName && jkc99_lexer_match(ctx, ')') && jkc99_lexer_match(ctx, '{')) {
            expr = jkc99_expr_compound(ctx, &src, typeName, jkc99_parse_initializer_list(ctx));
            jkc99_lexer_require(ctx, '}');
        } else {
            jkc99_lexer_rollback(ctx, &src);
        }
    }

    if(!expr) {
        expr = jkc99_parse_expr_primary(ctx);
    }

    if(expr) {
        do {
            Source postfixSrc;
            jkc99_lexer_source(ctx, &postfixSrc);
            if(jkc99_lexer_match(ctx, '[')) {
                expr = jkc99_expr_index(ctx, &postfixSrc, expr, jkc99_parse_expr(ctx));
                jkc99_lexer_require(ctx, ']');
            } else if(jkc99_lexer_match(ctx, '(')) {
#ifdef __clang__
                if(expr && expr->kind == kExprPrimary && expr->u.primary.kind == kExprPrimaryIdentifier && expr->u.primary.u.identifier == kw[kw___builtin_va_arg]) {
                    Expr *list = NULL;
                    TypeName *type = NULL;
                    list = jkc99_parse_expr_assignment(ctx);
                    jkc99_lexer_require(ctx, ',');
                    type = jkc99_parse_type_name(ctx);
                    expr = jkc99_expr_va_arg(ctx, &postfixSrc, expr, list, type);
                    jkc99_lexer_require(ctx, ')');
                    continue;
                }
#endif
                expr = jkc99_expr_call(ctx, &postfixSrc, expr, jkc99_parse_argument_expression_list(ctx));
                jkc99_lexer_require(ctx, ')');
            } else if(jkc99_lexer_match(ctx, '.')) {
                expr = jkc99_expr_dot(ctx, &postfixSrc, expr, jkc99_parse_identifier(ctx));
            } else if(jkc99_lexer_match(ctx, CLEX_arrow)) {
                expr = jkc99_expr_arrow(ctx, &postfixSrc, expr, jkc99_parse_identifier(ctx));
            } else if(jkc99_lexer_match(ctx, CLEX_plusplus)) {
                expr = jkc99_expr_increment(ctx, &postfixSrc, expr);
            } else if(jkc99_lexer_match(ctx, CLEX_minusminus)) {
                expr = jkc99_expr_decrement(ctx, &postfixSrc, expr);
            } else {
                break;
            }
        } while(true);
    }

    return expr;
}

JKC99_API Expr *jkc99_parse_expr_unary(ParseContext *ctx) {
    Source src;
    Expr *expr = NULL;

    jkc99_lexer_source(ctx, &src);

    if(jkc99_lexer_match(ctx, CLEX_plusplus)) {
        expr = jkc99_expr_unary(ctx, &src, kExprUnaryIncrement, jkc99_parse_expr_unary(ctx));
    } else if(jkc99_lexer_match(ctx, CLEX_minusminus)) {
        expr = jkc99_expr_unary(ctx, &src, kExprUnaryDecrement, jkc99_parse_expr_unary(ctx));
    } else if(jkc99_lexer_match(ctx, '&')) {
        expr = jkc99_expr_unary(ctx, &src, kExprUnaryAddressOf, jkc99_parse_expr_cast(ctx));
    } else if(jkc99_lexer_match(ctx, '*')) {
        expr = jkc99_expr_unary(ctx, &src, kExprUnaryDeref, jkc99_parse_expr_cast(ctx));
    } else if(jkc99_lexer_match(ctx, '+')) {
        expr = jkc99_expr_unary(ctx, &src, kExprUnaryPlus, jkc99_parse_expr_cast(ctx));
    } else if(jkc99_lexer_match(ctx, '-')) {
        expr = jkc99_expr_unary(ctx, &src, kExprUnaryMinus, jkc99_parse_expr_cast(ctx));
    } else if(jkc99_lexer_match(ctx, '~')) {
        expr = jkc99_expr_unary(ctx, &src, kExprUnaryBitwiseNeg, jkc99_parse_expr_cast(ctx));
    } else if(jkc99_lexer_match(ctx, '!')) {
        expr = jkc99_expr_unary(ctx, &src, kExprUnaryLogicalNot, jkc99_parse_expr_cast(ctx));
    } else {
        if(jkc99_lexer_match_kw(ctx, sizeof)) {
            if(jkc99_lexer_match(ctx, '(')) {
                Source sizeofSrc;
                TypeName *typeName;

                jkc99_lexer_source(ctx, &sizeofSrc);
                typeName = jkc99_parse_type_name(ctx);

                if(typeName) {
                    expr = jkc99_expr_sizeof_type(ctx, &src, typeName);
                } else {
                    jkc99_lexer_rollback(ctx, &sizeofSrc);
                    expr = jkc99_parse_expr_unary(ctx);
                    if(expr) {
                        expr = jkc99_expr_unary(ctx, &src, kExprUnarySizeofExpr, expr);
                    } else {
                        jkc99_log_error(ctx, "Expected type name or expression for sizeof()");
                    }
                }
                jkc99_lexer_require(ctx, ')');
            } else {
                expr = jkc99_parse_expr_unary(ctx);
                if(expr) {
                    expr = jkc99_expr_unary(ctx, &src, kExprUnarySizeofExpr, expr);
                } else {
                    jkc99_log_error(ctx, "Expected expression for sizeof");
                }
            }
        } else if(jkc99_lexer_match_kw(ctx, __builtin_offsetof)) {
            TypeName *typeName;
            const char *field;

            if(jkc99_lexer_require(ctx, '(')) {
                typeName = jkc99_parse_type_name(ctx);
                if(!typeName) {
                    jkc99_log_error(ctx, "Expected type name for offsetof");
                }
                jkc99_lexer_require(ctx, ',');
                field = jkc99_parse_identifier(ctx);
                if(!field) {
                    jkc99_log_error(ctx, "Expected field name for offsetof");
                }
                expr = jkc99_expr_offsetof(ctx, &src, typeName, field);
                jkc99_lexer_require(ctx, ')');
            } else {
                jkc99_log_error(ctx, "Expected '(' after offsetof");
            }
        }
    }

    if(!expr) {
        hook_execute(ctx, kHookParseExprUnaryNone, &expr);
        if(!expr) {
            expr = jkc99_parse_expr_postfix(ctx);
        }
    }

    return expr;
}

JKC99_API Expr *jkc99_parse_expr_cast(ParseContext *ctx) {
    Expr *expr = NULL;
    Source src;

    jkc99_lexer_source(ctx, &src);

    if(jkc99_lexer_match(ctx, '(')) {
        TypeName *typeName = jkc99_parse_type_name(ctx);
        if(typeName && jkc99_lexer_match(ctx, ')') && !jkc99_lexer_match(ctx, '{')) {
            Expr *castExpr = jkc99_parse_expr_cast(ctx);
            if(!castExpr) {
                jkc99_log_error(ctx, "Unable to parse expression for cast");
            }
            expr = jkc99_expr_cast(ctx, &src, typeName, castExpr);
        } else {
            jkc99_lexer_rollback(ctx, &src);
        }
    }
    
    if(!expr) {
        expr = jkc99_parse_expr_unary(ctx);
    }

    return expr;
}

JKC99_API Expr *jkc99_parse_expr_multiplicative(ParseContext *ctx) {
    Source src;
    Expr *expr = NULL;

    jkc99_lexer_source(ctx, &src);

    expr = jkc99_parse_expr_cast(ctx);
    while(expr) {
        if(jkc99_lexer_match(ctx, '*')) {
            expr = jkc99_expr_multiplicative(ctx, &src, kExprMultiplicativeMul, expr, jkc99_parse_expr_cast(ctx));
        } else if(jkc99_lexer_match(ctx, '/')) {
            expr = jkc99_expr_multiplicative(ctx, &src, kExprMultiplicativeDiv, expr, jkc99_parse_expr_cast(ctx));
        } else if(jkc99_lexer_match(ctx, '%')) {
            expr = jkc99_expr_multiplicative(ctx, &src, kExprMultiplicativeMod, expr, jkc99_parse_expr_cast(ctx));
        } else {
            break;
        }
    }

    return expr;
}

JKC99_API Expr *jkc99_parse_expr_additive(ParseContext *ctx) {
    Source src;
    Expr *expr = NULL;

    jkc99_lexer_source(ctx, &src);

    expr = jkc99_parse_expr_multiplicative(ctx);
    while(expr) {
        if(jkc99_lexer_match(ctx, '+')) {
            expr = jkc99_expr_additive(ctx, &src, kExprAdditiveAdd, expr, jkc99_parse_expr_multiplicative(ctx));
        } else if(jkc99_lexer_match(ctx, '-')) {
            expr = jkc99_expr_additive(ctx, &src, kExprAdditiveSub, expr, jkc99_parse_expr_multiplicative(ctx));
        } else {
            break;
        }
    }

    return expr;
}

JKC99_API Expr *jkc99_parse_expr_shift(ParseContext *ctx) {
    Source src;
    Expr *expr = NULL;

    jkc99_lexer_source(ctx, &src);

    expr = jkc99_parse_expr_additive(ctx);
    while(expr) {
        if(jkc99_lexer_match(ctx, CLEX_shl)) {
            expr = jkc99_expr_shift(ctx, &src, kExprShiftLeft, expr, jkc99_parse_expr_additive(ctx));
        } else if(jkc99_lexer_match(ctx, CLEX_shr)) {
            expr = jkc99_expr_shift(ctx, &src, kExprShiftRight, expr, jkc99_parse_expr_additive(ctx));
        } else {
            break;
        }
    }

    return expr;
}

JKC99_API Expr *jkc99_parse_expr_relational(ParseContext *ctx) {
    Source src;
    Expr *expr = NULL;

    jkc99_lexer_source(ctx, &src);

    expr = jkc99_parse_expr_shift(ctx);
    while(expr) {
        if(jkc99_lexer_match(ctx, '<')) {
            expr = jkc99_expr_relational(ctx, &src, kExprRelationalLT, expr, jkc99_parse_expr_shift(ctx));
        } else if(jkc99_lexer_match(ctx, '>')) {
            expr = jkc99_expr_relational(ctx, &src, kExprRelationalGT, expr, jkc99_parse_expr_shift(ctx));
        } else if(jkc99_lexer_match(ctx, CLEX_lesseq)) {
            expr = jkc99_expr_relational(ctx, &src, kExprRelationalLTE, expr, jkc99_parse_expr_shift(ctx));
        } else if(jkc99_lexer_match(ctx, CLEX_greatereq)) {
            expr = jkc99_expr_relational(ctx, &src, kExprRelationalGTE, expr, jkc99_parse_expr_shift(ctx));
        } else {
            break;
        }
    }

    return expr;
}

JKC99_API Expr *jkc99_parse_expr_equality(ParseContext *ctx) {
    Source src;
    Expr *expr = NULL;

    jkc99_lexer_source(ctx, &src);

    expr = jkc99_parse_expr_relational(ctx);
    while(expr) {
        if(jkc99_lexer_match(ctx, CLEX_eq)) {
            expr = jkc99_expr_equality(ctx, &src, kExprEqualityEQ, expr, jkc99_parse_expr_relational(ctx));
        } else if(jkc99_lexer_match(ctx, CLEX_noteq)) {
            expr = jkc99_expr_equality(ctx, &src, kExprEqualityNE, expr, jkc99_parse_expr_relational(ctx));
        } else {
            break;
        }
    }

    return expr;
}

JKC99_API Expr *jkc99_parse_expr_and(ParseContext *ctx) {
    Source src;
    Expr *expr = NULL;

    jkc99_lexer_source(ctx, &src);

    expr = jkc99_parse_expr_equality(ctx);
    while(expr && jkc99_lexer_match(ctx, '&')) {
        expr = jkc99_expr_and(ctx, &src, expr, jkc99_parse_expr_equality(ctx));
    }

    return expr;
}

JKC99_API Expr *jkc99_parse_expr_exclusive_or(ParseContext *ctx) {
    Source src;
    Expr *expr = NULL;

    jkc99_lexer_source(ctx, &src);

    expr = jkc99_parse_expr_and(ctx);
    while(expr && jkc99_lexer_match(ctx, '^')) {
        expr = jkc99_expr_exclusive_or(ctx, &src, expr, jkc99_parse_expr_and(ctx));
    }

    return expr;
}

JKC99_API Expr *jkc99_parse_expr_inclusive_or(ParseContext *ctx) {
    Source src;
    Expr *expr = NULL;

    jkc99_lexer_source(ctx, &src);

    expr = jkc99_parse_expr_exclusive_or(ctx);
    while(expr && jkc99_lexer_match(ctx, '|')) {
        expr = jkc99_expr_inclusive_or(ctx, &src, expr, jkc99_parse_expr_exclusive_or(ctx));
    }

    return expr;
}

JKC99_API Expr *jkc99_parse_expr_logical_and(ParseContext *ctx) {
    Source src;
    Expr *expr = NULL;

    jkc99_lexer_source(ctx, &src);

    expr = jkc99_parse_expr_inclusive_or(ctx);
    while(expr && jkc99_lexer_match(ctx, CLEX_andand)) {
        expr = jkc99_expr_logical_and(ctx, &src, expr, jkc99_parse_expr_inclusive_or(ctx));
    }

    return expr;
}

JKC99_API Expr *jkc99_parse_expr_logical_or(ParseContext *ctx) {
    Source src;
    Expr *expr = NULL;

    jkc99_lexer_source(ctx, &src);
    
    expr = jkc99_parse_expr_logical_and(ctx);
    while(expr && jkc99_lexer_match(ctx, CLEX_oror)) {
        expr = jkc99_expr_logical_or(ctx, &src, expr, jkc99_parse_expr_logical_and(ctx));
    }

    return expr;
}

JKC99_API Expr *jkc99_parse_expr_conditional(ParseContext *ctx) {
    Source src;
    Expr *expr = NULL;

    jkc99_lexer_source(ctx, &src);

    expr = jkc99_parse_expr_logical_or(ctx);
    if(expr && jkc99_lexer_match(ctx, '?')) {
        Expr *tb = jkc99_parse_expr(ctx);
        if(tb) {
            jkc99_lexer_require(ctx, ':');
            expr = jkc99_expr_conditional(ctx, &src, expr, tb, jkc99_parse_expr_conditional(ctx));
        }
    }

    return expr;
}

JKC99_API Expr *jkc99_parse_expr_constant(ParseContext *ctx) {
    return jkc99_parse_expr_conditional(ctx);
}

JKC99_API Expr *jkc99_parse_expr_assignment(ParseContext *ctx) {
    Source src;
    Expr *expr = NULL;

    jkc99_lexer_source(ctx, &src);

    expr = jkc99_parse_expr_conditional(ctx);
    if(expr && expr->kind <= kExprUnary) {
        if(jkc99_lexer_match(ctx, '=')) {
            expr = jkc99_expr_assign(ctx, &src, kExprAssignmentBasic, expr, jkc99_parse_expr_assignment(ctx));
        } else if(jkc99_lexer_match(ctx, CLEX_muleq)) {
            expr = jkc99_expr_assign(ctx, &src, kExprAssignmentMul, expr, jkc99_parse_expr_assignment(ctx));
        } else if(jkc99_lexer_match(ctx, CLEX_diveq)) {
            expr = jkc99_expr_assign(ctx, &src, kExprAssignmentDiv, expr, jkc99_parse_expr_assignment(ctx));
        } else if(jkc99_lexer_match(ctx, CLEX_modeq)) {
            expr = jkc99_expr_assign(ctx, &src, kExprAssignmentMod, expr, jkc99_parse_expr_assignment(ctx));
        } else if(jkc99_lexer_match(ctx, CLEX_pluseq)) {
            expr = jkc99_expr_assign(ctx, &src, kExprAssignmentAdd, expr, jkc99_parse_expr_assignment(ctx));
        } else if(jkc99_lexer_match(ctx, CLEX_minuseq)) {
            expr = jkc99_expr_assign(ctx, &src, kExprAssignmentSub, expr, jkc99_parse_expr_assignment(ctx));
        } else if(jkc99_lexer_match(ctx, CLEX_shleq)) {
            expr = jkc99_expr_assign(ctx, &src, kExprAssignmentShiftLeft, expr, jkc99_parse_expr_assignment(ctx));
        } else if(jkc99_lexer_match(ctx, CLEX_shreq)) {
            expr = jkc99_expr_assign(ctx, &src, kExprAssignmentShiftRight, expr, jkc99_parse_expr_assignment(ctx));
        } else if(jkc99_lexer_match(ctx, CLEX_andeq)) {
            expr = jkc99_expr_assign(ctx, &src, kExprAssignmentAnd, expr, jkc99_parse_expr_assignment(ctx));
        } else if(jkc99_lexer_match(ctx, CLEX_xoreq)) {
            expr = jkc99_expr_assign(ctx, &src, kExprAssignmentXor, expr, jkc99_parse_expr_assignment(ctx));
        } else if(jkc99_lexer_match(ctx, CLEX_oreq)) {
            expr = jkc99_expr_assign(ctx, &src, kExprAssignmentOr, expr, jkc99_parse_expr_assignment(ctx));
        }
    }

    return expr;
}

JKC99_API Expr *jkc99_parse_expr(ParseContext *ctx) {
    Source src;
    Expr *expr = NULL;

    jkc99_lexer_source(ctx, &src);

    jkc99_parse_extension(ctx);
    expr = jkc99_parse_expr_assignment(ctx);
    if(expr) {
        while(jkc99_lexer_match(ctx, ',')) {
            expr = jkc99_expr_comma(ctx, &src, expr, jkc99_parse_expr_assignment(ctx));
        }
    }

    return expr;
}



static inline bool jkc99_parse_storage_class_specifier(ParseContext *ctx, StorageClass *storageClass) {
    bool res = false;

    jkc99_assert(storageClass->kind == kStorageClassNone);

    if(jkc99_lexer_is(ctx, CLEX_id)) {
        Source src;

        jkc99_lexer_source(ctx, &src);

        if(jkc99_lexer_match_kw(ctx, typedef)) {
            *storageClass = (StorageClass){ .kind = kStorageClassTypedef };
        } else if(jkc99_lexer_match_kw(ctx, extern)) {
            *storageClass = (StorageClass){ .kind = kStorageClassExtern };
        } else if(jkc99_lexer_match_kw(ctx, static)) {
            *storageClass = (StorageClass){ .kind = kStorageClassStatic };
        } else if(jkc99_lexer_match_kw(ctx, auto)) {
            *storageClass = (StorageClass){ .kind = kStorageClassAuto };
        } else if(jkc99_lexer_match_kw(ctx, register)) {
            *storageClass = (StorageClass){ .kind = kStorageClassRegister };
        }

        if(storageClass->kind != kStorageClassNone) {
            source_end(ctx, &src);
            storageClass->src = src;
            jkc99_parse_empty_directives(ctx, storageClass);
            res = true;
        }
    }

    return res;
}

static StructDeclaration *jkc99_parse_struct_declaration(ParseContext *ctx);

static void jkc99_parse_struct_or_union_specifier(ParseContext *ctx, StructOrUnionSpecifier *spec) {
    if(jkc99_lexer_is(ctx, CLEX_id)) {
        spec->identifier = jkc99_str_intern(ctx, ctx->lexer->string);
        jkc99_lexer_next(ctx);
    } 

    if(jkc99_lexer_match(ctx, '{')) {
        StructDeclaration **list = NULL;
        spec->definedHere = true;

        while(!(jkc99_lexer_match(ctx, '}') || jkc99_lexer_is(ctx, CLEX_eof))) {
            StructDeclaration *decl;
            decl = jkc99_parse_struct_declaration(ctx);
            if(decl) {
                da_push(list, decl);
            }
        }

        jkc99_parse_attribute_list_(ctx, &spec->attributes, &spec->attributeCount);

        spec->memberDeclarations = list;
        spec->memberDeclarationCount = da_count(list);
    } else {
        spec->definedHere = false;
    }
}

static Enumerator *jkc99_parse_enumerator_list_(ParseContext *ctx, Enumerator *list, size_t exprOffset) {
    Enumerator enumerator = {0};

    jkc99_lexer_source(ctx, &enumerator.src);

    enumerator.identifier = jkc99_parse_identifier(ctx);
    if(enumerator.identifier) {
        jkc99_parse_empty_directives(ctx, &enumerator);
        if(jkc99_lexer_match(ctx, '=')) {
            enumerator.expr = jkc99_parse_expr_constant(ctx);
        }
        if(enumerator.expr) {
            exprOffset = 0;
        }
        enumerator.exprOffset = exprOffset++;
        source_end(ctx, &enumerator);
        da_push(list, enumerator);

        if(jkc99_lexer_match(ctx, ',')) {
            list = jkc99_parse_enumerator_list_(ctx, list, exprOffset);
        }
    }

    return list;
}

JKC99_API Enumerator *jkc99_parse_enumerator_list(ParseContext *ctx, size_t *count) {
    Enumerator *list = jkc99_parse_enumerator_list_(ctx,NULL, 0);
    *count = da_count(list);
    return list;
}

static bool jkc99_parse_is_typedef_name(ParseContext *ctx, const char *identifier) {
    Symbol *s = jkc99_sym_get(ctx, identifier);
    return (s && s->kind == kSymbolType ? true : false);
}

static inline bool jkc99_parse_type_specifier(ParseContext *ctx, TypeSpecifier *spec, _Bool typeSpecifierFound) {
    if(jkc99_lexer_is(ctx, CLEX_id)) {
        Source src;

        jkc99_assert(spec->kind == kTypeSpecifierNone);
        jkc99_lexer_source(ctx, &src);
        if(jkc99_lexer_match_kw(ctx, void)) {
            spec->kind = kTypeSpecifierVoid;
        } else if(jkc99_lexer_match_kw(ctx, char)) {
            spec->kind = kTypeSpecifierChar;
        } else if(jkc99_lexer_match_kw(ctx, short)) {
            spec->kind = kTypeSpecifierShort;
        } else if(jkc99_lexer_match_kw(ctx, int)) {
            spec->kind = kTypeSpecifierInt;
        } else if(jkc99_lexer_match_kw(ctx, long)) {
            spec->kind = kTypeSpecifierLong;
        } else if(jkc99_lexer_match_kw(ctx, float)) {
            spec->kind = kTypeSpecifierFloat;
        } else if(jkc99_lexer_match_kw(ctx, double)) {
            spec->kind = kTypeSpecifierDouble;
        } else if(jkc99_lexer_match_kw(ctx, signed)) {
            spec->kind = kTypeSpecifierSigned;
        } else if(jkc99_lexer_match_kw(ctx, unsigned)) {
            spec->kind = kTypeSpecifierUnsigned;
        } else if(jkc99_lexer_match_kw(ctx, _Bool)) {
            spec->kind = kTypeSpecifier_Bool;
        } else if(jkc99_lexer_match_kw(ctx, _Complex)) {
            spec->kind = kTypeSpecifier_Complex;
#if 1
        } else if(jkc99_lexer_match_id(ctx, jkc99_str_intern(ctx, "__int8"))) {
            spec->kind = kTypeSpecifier__int8;
        } else if(jkc99_lexer_match_id(ctx, jkc99_str_intern(ctx, "__int16"))) {
            spec->kind = kTypeSpecifier__int16;
        } else if(jkc99_lexer_match_id(ctx, jkc99_str_intern(ctx, "__int32"))) {
            spec->kind = kTypeSpecifier__int32;
        } else if(jkc99_lexer_match_id(ctx, jkc99_str_intern(ctx, "__int64"))) {
            spec->kind = kTypeSpecifier__int64;
#endif
        } else if(jkc99_lexer_match_kw(ctx, struct)) {
            jkc99_parse_empty_directives(ctx, spec);
            while(jkc99_parse_attribute_list_(ctx, &spec->u.su.attributes, &spec->u.su.attributeCount) || jkc99_parse_declspec_list_(ctx, &spec->u.su.declspecs, &spec->u.su.declspecCount));
            spec->kind = kTypeSpecifierStruct;
            jkc99_parse_struct_or_union_specifier(ctx, &spec->u.su);
        } else if(jkc99_lexer_match_kw(ctx, union)) {
            jkc99_parse_empty_directives(ctx, spec);
            while(jkc99_parse_attribute_list_(ctx, &spec->u.su.attributes, &spec->u.su.attributeCount) || jkc99_parse_declspec_list_(ctx, &spec->u.su.declspecs, &spec->u.su.declspecCount));
            spec->kind = kTypeSpecifierUnion;
            jkc99_parse_struct_or_union_specifier(ctx, &spec->u.su);
        } else if(jkc99_lexer_match_kw(ctx, enum)) {
            jkc99_parse_empty_directives(ctx, spec);
            while(jkc99_parse_attribute_list_(ctx, &spec->u.e.attributes, &spec->u.e.attributeCount) || jkc99_parse_declspec_list_(ctx, &spec->u.su.declspecs, &spec->u.su.declspecCount));
            spec->kind = kTypeSpecifierEnum;

            if(jkc99_lexer_is(ctx, CLEX_id)) {
                spec->u.e.identifier = jkc99_str_intern(ctx, ctx->lexer->string);
                jkc99_lexer_next(ctx);
            }

            if(jkc99_lexer_match(ctx, '{')) {
                spec->u.e.definedHere = true;
                spec->u.e.enumerators = jkc99_parse_enumerator_list(ctx, &spec->u.e.enumeratorCount);
                jkc99_lexer_require(ctx, '}');
                jkc99_parse_attribute_list_(ctx, &spec->u.e.attributes, &spec->u.e.attributeCount);
            }
            if(spec->u.e.enumeratorCount) {
                hook_execute(ctx, kHookNewEnumSpecifier, &spec->u.e);
            }
#ifdef __clang__
        } else if(jkc99_lexer_match_kw(ctx, __builtin_va_list)) {
            spec->kind = kTypeSpecifierBuiltin;
            spec->u.t = kw[kw___builtin_va_list];
#endif
        } else if(!typeSpecifierFound) {
            const char *str = jkc99_str_intern(ctx, ctx->lexer->string);
            if(jkc99_parse_is_typedef_name(ctx, str)) {
                spec->kind = kTypeSpecifierTypedefName;
                spec->u.t = str;
            }
        }

        if(spec->kind != kTypeSpecifierNone) {
            source_end(ctx, &src);
            spec->src = src;

            if(!(spec->kind == kTypeSpecifierStruct || spec->kind == kTypeSpecifierUnion || spec->kind == kTypeSpecifierEnum)) {
                jkc99_parse_empty_directives(ctx, spec);
            }
                
            if(spec->kind == kTypeSpecifierTypedefName) {
                jkc99_lexer_next(ctx);
            }
        }
    }

#if 0
    if(spec->kind != kTypeSpecifierNone) {
        decl->typeSpecifiers |= res;
    }
#endif

    return (spec->kind != kTypeSpecifierNone ? true : false);
}


static inline bool jkc99_parse_function_specifier(ParseContext *ctx, FunctionSpecifier *functionSpecifier) {
    bool res = false;

    if(jkc99_lexer_is(ctx, CLEX_id)) {
        Source src;

        jkc99_assert(functionSpecifier->kind == kFunctionSpecifierNone);
        jkc99_lexer_source(ctx, &src);
        if(jkc99_lexer_match_kw(ctx, inline)) {
            *functionSpecifier = (FunctionSpecifier){
                .kind = kFunctionSpecifierInline,
                .format = kFunctionSpecifierFormatF
            };
        } else if(jkc99_lexer_match_kw(ctx, __inline)) {
            *functionSpecifier = (FunctionSpecifier){
                .kind = kFunctionSpecifierInline,
                .format = kFunctionSpecifierFormat__F
            };
        } else if(jkc99_lexer_match_kw(ctx, __inline__)) {
            *functionSpecifier = (FunctionSpecifier){
                .kind = kFunctionSpecifierInline,
                .format = kFunctionSpecifierFormat__F__
            };
        } else if(jkc99_lexer_match_kw(ctx, __forceinline)) {
            *functionSpecifier = (FunctionSpecifier){
                .kind = kFunctionSpecifierInline,
                .format = kFunctionSpecifierFormat__forceF
            };
        }

        if(functionSpecifier->kind != kFunctionSpecifierNone) {
            source_end(ctx, &src);
            functionSpecifier->src = src;
            jkc99_parse_empty_directives(ctx, functionSpecifier);
            res = true;
        }
    }
    return res;
}

JKC99_API inline CallingConvention jkc99_parse_calling_convention(ParseContext *ctx) {
    CallingConvention cc = 0;

    if(jkc99_lexer_is(ctx, CLEX_id)) {
        if(jkc99_lexer_match_kw(ctx, __cdecl)) {
            cc = kCallingConventionCdecl;
        } else if(jkc99_lexer_match_kw(ctx, __clrcall)) {
            cc = kCallingConventionClrcall;
        } else if(jkc99_lexer_match_kw(ctx, __stdcall)) {
            cc = kCallingConventionStdcall;
        } else if(jkc99_lexer_match_kw(ctx, __fastcall)) {
            cc = kCallingConventionFastcall;
        } else if(jkc99_lexer_match_kw(ctx, __thiscall)) {
            cc = kCallingConventionThiscall;
        } else if(jkc99_lexer_match_kw(ctx, __vectorcall)) {
            cc = kCallingConventionVectorcall;
        }
    }
    return cc;
}


static bool jkc99_parse_type_qualifier(ParseContext *ctx, TypeQualifier *qual) {
    if(jkc99_lexer_is(ctx, CLEX_id)) {
        Source src;

        jkc99_assert(qual->kind == kTypeQualifierNone);
        jkc99_lexer_source(ctx, &src);

        if(jkc99_lexer_match_kw(ctx, const)) {
            *qual = (TypeQualifier){ 
                .kind = kTypeQualifierConst
            };
        } else if(jkc99_lexer_match_kw(ctx, restrict)) {
            *qual = (TypeQualifier){
                .kind = kTypeQualifierRestrict,
                .format = bTypeQualifierFormatQ
            };
        } else if(jkc99_lexer_match_kw(ctx, __restrict)) {
            *qual = (TypeQualifier){
                .kind = kTypeQualifierRestrict,
                .format = bTypeQualifierFormat__Q
            };
        } else if(jkc99_lexer_match_kw(ctx, __restrict__)) {
            *qual = (TypeQualifier){
                .kind = kTypeQualifierRestrict,
                .format = bTypeQualifierFormat__Q__
            };
        } else if(jkc99_lexer_match_kw(ctx, volatile)) {
            *qual = (TypeQualifier){
                .kind = kTypeQualifierVolatile,
                .format = bTypeQualifierFormatQ
            };
        } else if(jkc99_lexer_match_kw(ctx, __volatile)) {
            *qual = (TypeQualifier){
                .kind = kTypeQualifierVolatile,
                .format = bTypeQualifierFormat__Q
            };
        } else if(jkc99_lexer_match_kw(ctx, __volatile__)) {
            *qual = (TypeQualifier){
                .kind = kTypeQualifierVolatile,
                .format = bTypeQualifierFormat__Q__
            };
        } else if(jkc99_lexer_match_kw(ctx, __ptr32)) {
            *qual = (TypeQualifier){
                .kind = kTypeQualifier__ptr32,
                .format = bTypeQualifierFormat__Q
            };
        } else if(jkc99_lexer_match_kw(ctx, __ptr64)) {
            *qual = (TypeQualifier){
                .kind = kTypeQualifier__ptr64,
                .format = bTypeQualifierFormat__Q
            };
        } else if(jkc99_lexer_match_kw(ctx, __unaligned)) {
            *qual = (TypeQualifier){
                .kind = kTypeQualifier__unaligned,
                .format = bTypeQualifierFormat__Q
            };
        }

        if(qual->kind != kTypeQualifierNone) {
            source_end(ctx, &src);
            qual->src = src;
            jkc99_parse_empty_directives(ctx, qual);
            return true;
        }
    }

    return false;
}

static DeclarationSpecifier *jkc99_parse_specifier_qualifier_list_(ParseContext *ctx, DeclarationSpecifier *list, _Bool typeSpecifierFound) {
    DeclarationSpecifier specifier = {0};

    if(jkc99_parse_type_specifier(ctx, &specifier.u.typeSpecifier, typeSpecifierFound)) {
        specifier.kind = kDeclarationSpecifierTypeSpecifier;
        da_push(list, specifier);
        typeSpecifierFound = true;
        list = jkc99_parse_specifier_qualifier_list_(ctx, list, typeSpecifierFound);
    } else if(jkc99_parse_type_qualifier(ctx, &specifier.u.typeQualifier)) {
        specifier.kind = kDeclarationSpecifierTypeQualifier;
        da_push(list, specifier);
        list = jkc99_parse_specifier_qualifier_list_(ctx, list, typeSpecifierFound);
    } else if(jkc99_parse_attribute(ctx, &specifier.u.attribute)) {
        specifier.kind = kDeclarationSpecifierAttribute;
        da_push(list, specifier);
        list = jkc99_parse_specifier_qualifier_list_(ctx, list, typeSpecifierFound);
    }

    return list;
}

JKC99_API DeclarationSpecifier *jkc99_parse_specifier_qualifier_list(ParseContext *ctx, size_t *count) {
    DeclarationSpecifier *specs = jkc99_parse_specifier_qualifier_list_(ctx,NULL,false);
    *count = da_count(specs);
    return specs;
}

static StructDeclarator *jkc99_parse_struct_declarator_list_(ParseContext *ctx, StructDeclarator *list) {
    StructDeclarator sd = {0};

    sd.declarator = jkc99_parse_declarator(ctx);
    if(jkc99_lexer_match(ctx, ':')) {
        sd.bitCountExpr = jkc99_parse_expr_constant(ctx);
    }

    if(sd.declarator || sd.bitCountExpr) {
        da_push(list, sd);
        if(jkc99_lexer_match(ctx, ',')) {
            list = jkc99_parse_struct_declarator_list_(ctx, list);
        }
    }

    return list;
}

static inline StructDeclarator *jkc99_parse_struct_declarator_list(ParseContext *ctx, size_t *count) {
    StructDeclarator *d = jkc99_parse_struct_declarator_list_(ctx,NULL);
    *count = da_count(d);
    return d;
}

static StructDeclaration *jkc99_parse_struct_declaration(ParseContext *ctx) {
    bool hasExtension = false;
    Source src;
    StructDeclaration *decl = NULL;
    size_t specifierCount;
    DeclarationSpecifier *specifiers;

    jkc99_lexer_source(ctx, &src);

    hasExtension = jkc99_parse_extension(ctx);
    specifiers = jkc99_parse_specifier_qualifier_list(ctx, &specifierCount);
    if(specifiers) {
        size_t declaratorCount;
        StructDeclarator *declarators;

        declarators = jkc99_parse_struct_declarator_list(ctx, &declaratorCount);
        jkc99_lexer_require(ctx, ';');
        source_end(ctx, &src);
        decl = jkc99_declaration_struct(ctx, &src, specifierCount, specifiers, da_count(declarators), declarators);
        decl->hasExtension = hasExtension;
    } else {
        long tokens[] = {';','}'};

        jkc99_log_error_src(ctx, &src, "Expected member declaration specifiers");
        jkc99_lexer_skip_until_array(ctx, tokens);
        jkc99_lexer_match(ctx, ';');
    }

    return decl;
}

static DeclarationSpecifier *jkc99_parse_declaration_specifiers_(ParseContext *ctx, DeclarationSpecifier *list, _Bool typeSpecifierFound) {
    DeclarationSpecifier spec = {0};

    if(jkc99_parse_storage_class_specifier(ctx, &spec.u.storageClass)) {
        spec.kind = kDeclarationSpecifierStorageClassSpecifier;
        da_push(list, spec);
        list = jkc99_parse_declaration_specifiers_(ctx, list, typeSpecifierFound);
    } else if(jkc99_parse_type_qualifier(ctx, &spec.u.typeQualifier)) {
        spec.kind = kDeclarationSpecifierTypeQualifier;
        da_push(list, spec);
        list = jkc99_parse_declaration_specifiers_(ctx, list, typeSpecifierFound);
    } else if(jkc99_parse_function_specifier(ctx, &spec.u.functionSpecifier)) {
        spec.kind = kDeclarationSpecifierFunctionSpecifier;
        da_push(list, spec);
        list = jkc99_parse_declaration_specifiers_(ctx, list, typeSpecifierFound);
    } else if(jkc99_parse_attribute(ctx, &spec.u.attribute)) {
        spec.kind = kDeclarationSpecifierAttribute;
        da_push(list, spec);
        list = jkc99_parse_declaration_specifiers_(ctx, list, typeSpecifierFound);
#if HAS___DECLSPEC
    } else if(jkc99_parse_declspec(ctx, &spec.u.declspec)) {
        spec.kind = kDeclarationSpecifierDeclspec;
        da_push(list, spec);
        list = jkc99_parse_declaration_specifiers_(ctx, list, typeSpecifierFound);
#endif
    } else if(jkc99_parse_type_specifier(ctx, &spec.u.typeSpecifier, typeSpecifierFound)) {
        spec.kind = kDeclarationSpecifierTypeSpecifier;
        da_push(list, spec);
        typeSpecifierFound = true;
        list = jkc99_parse_declaration_specifiers_(ctx, list, typeSpecifierFound);
    } else {
        CallingConvention cc = jkc99_parse_calling_convention(ctx);
        if(cc != kCallingConventionNone) {
            spec.kind = kDeclarationSpecifierCallingConvention;
            spec.u.callingConvention = cc;
            da_push(list, spec);
            list = jkc99_parse_declaration_specifiers_(ctx, list, typeSpecifierFound);
        }
    }

    return list;
}

static inline DeclarationSpecifier *jkc99_parse_declaration_specifiers(ParseContext *ctx, size_t *count) {
    DeclarationSpecifier *specs = jkc99_parse_declaration_specifiers_(ctx,NULL,false);
    *count = da_count(specs);
    //NOTE This is meant to make it possible to handle multiple typedefs of the same type. Note that this is actually forbidden by the spec but it seems that most compilers allow it.
    if(*count >= 3 && specs[0].kind == kDeclarationSpecifierStorageClassSpecifier && specs[0].u.storageClass.kind == kStorageClassTypedef) {
        size_t last = *count-1;
        if(specs[last].kind == kDeclarationSpecifierTypeSpecifier && specs[last].u.typeSpecifier.kind == kTypeSpecifierTypedefName) {
            size_t i;
            for(i = 1; i < last; i++) {
                if(specs[i].kind == kDeclarationSpecifierTypeSpecifier) {
                    size_t len = strlen(specs[last].u.typeSpecifier.u.t);
                    ctx->lexer->parse_point -= len;
                    while(strncmp(ctx->lexer->parse_point, specs[last].u.typeSpecifier.u.t, len) != 0) {
                        ctx->lexer->parse_point--;
                    }
                    ctx->lexer->parse_point--;
                    jkc99_lexer_next(ctx);
                    *count -= 1;
                    break;
                }
            }
        }
    }
    return specs;
}

JKC99_API Pointer *jkc99_parse_pointer(ParseContext *ctx, Pointer *list) {
    Source src;

    jkc99_lexer_source(ctx, &src);
    /* TODO Handle calling conventions properly */
    jkc99_parse_calling_convention(ctx);
    if(jkc99_lexer_match(ctx, '*')) {
        Pointer ptr = {0};
        ptr.qualifiers = jkc99_parse_type_qualifier_list(ctx, &ptr.qualifierCount);
        da_push(list, ptr);
        list = jkc99_parse_pointer(ctx, list);
    } else {
        jkc99_lexer_rollback(ctx, &src);
    }

    return list;
}


JKC99_API DirectDeclarator *jkc99_direct_declarator_alloc(ParseContext *ctx, Source *src, DirectDeclaratorKind kind) {
    DirectDeclarator *direct = arena_alloc(&ctx->arena, sizeof(DirectDeclarator));
    direct->kind = kind;
    direct->src = src ? *src : (Source){0};
    jkc99_parse_empty_directives(ctx, direct);
    return direct;
}

JKC99_API DirectDeclarator *jkc99_direct_declarator_identifier(ParseContext *ctx, Source *src, const char *identifier) {
    DirectDeclarator *d = jkc99_direct_declarator_alloc(ctx, src, kDirectDeclaratorIdentifier);
    d->u.identifier.str = identifier;
    return d;
}

JKC99_API DirectDeclarator *jkc99_direct_declarator_declarator(ParseContext *ctx, Source *src, Declarator *declarator) {
    DirectDeclarator *d = jkc99_direct_declarator_alloc(ctx, src, kDirectDeclaratorDeclarator);
    d->u.declarator = declarator;
    return d;
}

JKC99_API DirectDeclarator *jkc99_direct_declarator_array_vla(ParseContext *ctx, Source *src, size_t attributeCount, Attribute *attributes, DirectDeclarator *direct, size_t qualifierCount, TypeQualifier *qualifiers) {
    DirectDeclarator *d = jkc99_direct_declarator_alloc(ctx, src, kDirectDeclaratorArray);
    d->u.array.direct = direct;
    d->u.array.attributeCount = attributeCount;
    d->u.array.attributes = attributes;
    d->u.array.qualifierCount = qualifierCount;
    d->u.array.qualifiers = qualifiers;
    d->u.array.isVLA = true;
    return d;
}

JKC99_API DirectDeclarator *jkc99_direct_declarator_array(ParseContext *ctx, Source *src, size_t attributeCount, Attribute *attributes, DirectDeclarator *direct, size_t qualifierCount, TypeQualifier *qualifiers, bool isStatic, Expr *expr) {
    DirectDeclarator *d = jkc99_direct_declarator_alloc(ctx, src, kDirectDeclaratorArray);
    d->u.array.direct = direct;
    d->u.array.attributeCount = attributeCount;
    d->u.array.attributes = attributes;
    d->u.array.qualifierCount = qualifierCount;
    d->u.array.qualifiers = qualifiers;
    d->u.array.isStatic = isStatic;
    d->u.array.expr = expr;
    return d;
}

JKC99_API DirectDeclarator *jkc99_direct_declarator_func_params(ParseContext *ctx, Source *src, DirectDeclarator *direct, CallingConvention callingConvention, size_t paramCount, ParameterDeclaration **params, bool hasEllipsis) {
    DirectDeclarator *d = jkc99_direct_declarator_alloc(ctx, src, kDirectDeclaratorFunc);
    d->u.func.direct = direct;
    d->u.func.callingConvention = callingConvention;
    d->u.func.hasEllipsis = hasEllipsis;
    d->u.func.hasParameterList = true;
    d->u.func.count = paramCount;
    d->u.func.u.parameters = params;
    return d;
}

JKC99_API DirectDeclarator *jkc99_direct_declarator_func_identifiers(ParseContext *ctx, Source *src, DirectDeclarator *direct, CallingConvention callingConvention, size_t idCount, const char **identifiers, bool hasEllipsis) {
    DirectDeclarator *d = jkc99_direct_declarator_alloc(ctx, src, kDirectDeclaratorFunc);
    d->u.func.direct = direct;
    d->u.func.callingConvention = callingConvention;
    d->u.func.hasEllipsis = hasEllipsis;
    d->u.func.count = idCount;
    d->u.func.u.identifiers = identifiers;
    return d;
}


JKC99_API void *jkc99_declaration_alloc(ParseContext *ctx, Source *src, size_t size, size_t specifierCount, DeclarationSpecifier *specifiers) {
    DeclarationCommon *decl = arena_alloc(&ctx->arena, size);
    decl->src = src ? *src : (Source){0};
    //jkc99_assert(ctx->currentDirectiveCount == 0);
    jkc99_assert(specifierCount);
    move_directives(&specifiers[0].u, decl);
    decl->specifierCount = specifierCount;
    decl->specifiers = specifiers;
    return decl;
}




JKC99_API TypeHandle jkc99_declarator_get_type_and_name(ParseContext *ctx, Declarator *d, TypeHandle type, const char **name, Initializer *initializer) {
    size_t i;

    jkc99_assert(JKC99_TYPE_HANDLE_NOT_INVALID(type));

    for(i = 0; i < d->pointerCount; i++) {
        Pointer *ptr = d->pointers + i;
        type = jkc99_type_ptr(ctx, type);
        if(ptr->qualifierCount) {
            type = jkc99_type_qualified(ctx, type, collapse_qualifier_list(ptr->qualifierCount, ptr->qualifiers));
        }
    }

    if(d->direct) {
        type = jkc99_direct_declarator_get_type_and_name(ctx, d->direct, type, name, initializer);
    }

    if(d->beforeAttributeCount) {
        type = get_derived_type_attribute(ctx, type, d->beforeAttributeCount, d->beforeAttributes);
    }
    if(d->afterAttributeCount) {
        type = get_derived_type_attribute(ctx, type, d->afterAttributeCount, d->afterAttributes);
    }

    return type;
}

JKC99_API TypeHandle jkc99_direct_declarator_get_type_and_name(ParseContext *ctx, DirectDeclarator *dd, TypeHandle typeHandle, const char **name, Initializer *initializer) {
    jkc99_assert(dd);

    switch(dd->kind) {
        case kDirectDeclaratorIdentifier:
            {
                if(name) {
                    *name = dd->u.identifier.str;
                }

                return typeHandle;
            } break;
        case kDirectDeclaratorDeclarator:
            {
                if(dd->u.declarator && dd->u.declarator->direct) {
                    return jkc99_declarator_get_type_and_name(ctx, dd->u.declarator, typeHandle, name, initializer);
                }
            } break;
        case kDirectDeclaratorArray:
            {
                if(dd->u.array.isVLA) {
                    typeHandle = jkc99_type_array_vla(ctx, typeHandle, NULL);
                } else {
                    /* TODO Determine if count expression is integer constant expression */
                    typeHandle = jkc99_type_array(ctx, typeHandle, dd->u.array.expr, initializer);
                }
                return jkc99_direct_declarator_get_type_and_name(ctx, dd->u.array.direct, typeHandle, name, initializer);
            } break;
        case kDirectDeclaratorFunc:
            {
                if(dd->u.func.hasParameterList) {
                    size_t i;
                    TypeHandle *paramTypes = NULL;

                    for(i = 0; i < dd->u.func.count; i++) {
                        ParameterDeclaration *p = dd->u.func.u.parameters[i];
                        TypeHandle paramType = p->type;
                        if(p->declarator) {
                            paramType = jkc99_declarator_get_type_and_name(ctx, p->declarator, p->type, NULL, NULL);
                        } else if(p->abstractDeclarator) {
                            paramType = jkc99_abstract_declarator_get_type(ctx, p->abstractDeclarator, p->type);
                        }
                        da_push(paramTypes, paramType);
                    }

                    jkc99_assert(da_count(paramTypes) == dd->u.func.count);

                    typeHandle = jkc99_type_func(ctx, typeHandle, dd->u.func.count, paramTypes, dd->u.func.hasEllipsis);

                    da_free(paramTypes);

                    return jkc99_direct_declarator_get_type_and_name(ctx, dd->u.func.direct, typeHandle, name, initializer);
                } else {
                    jkc99_assert(dd->u.func.count == 0); //TODO Identifier list on functions
                    jkc99_assert(!dd->u.func.hasEllipsis);

                    typeHandle = jkc99_type_func(ctx, typeHandle, 0, NULL, false);
                    return jkc99_direct_declarator_get_type_and_name(ctx, dd->u.func.direct, typeHandle, name, initializer);
                }
            } break;
        default: jkc99_assert(false); break;
    }

    return typeHandle;
}

JKC99_API Declaration *jkc99_declaration(ParseContext *ctx, Source *src, size_t specifierCount, DeclarationSpecifier *specifiers, size_t declaratorCount, InitDeclarator *declarators) {
    Declaration *d = jkc99_declaration_alloc(ctx, src, sizeof(Declaration), specifierCount, specifiers);

    d->initDeclaratorCount = declaratorCount;
    d->initDeclarators = declarators;

    jkc99_declaration_specifiers_get_type_storage_class_and_function_specifier(ctx, d->specifierCount, d->specifiers, &d->type, &d->storageClass, &d->functionSpecifier);

    jkc99_assert(JKC99_TYPE_HANDLE_NOT_INVALID(d->type)); //TODO Error

    return d;
}

JKC99_API StructDeclaration *jkc99_declaration_struct(ParseContext *ctx, Source *src, size_t specifierCount, DeclarationSpecifier *specifiers, size_t declaratorCount, StructDeclarator *declarators) {
    StructDeclaration *d = jkc99_declaration_alloc(ctx, src, sizeof(StructDeclaration), specifierCount, specifiers);

    d->declaratorCount = declaratorCount;
    d->declarators = declarators;

    jkc99_declaration_specifiers_get_type_storage_class_and_function_specifier(ctx, d->specifierCount, d->specifiers, &d->type, NULL, NULL);

    jkc99_assert(JKC99_TYPE_HANDLE_NOT_INVALID(d->type)); //TODO Error

    return d;
}

JKC99_API ParameterDeclaration *jkc99_declaration_parameter(ParseContext *ctx, Source *src, size_t specifierCount, DeclarationSpecifier *specifiers, Declarator *declarator, AbstractDeclarator *abstractDeclarator) {
    ParameterDeclaration *d = jkc99_declaration_alloc(ctx, src, sizeof(ParameterDeclaration), specifierCount, specifiers);

    d->declarator = declarator;
    d->abstractDeclarator = abstractDeclarator;

    jkc99_declaration_specifiers_get_type_storage_class_and_function_specifier(ctx, d->specifierCount, d->specifiers, &d->type, &d->storageClass, &d->functionSpecifier);

    jkc99_assert(JKC99_TYPE_HANDLE_NOT_INVALID(d->type));

    return d;
}

static void jkc99_sym_push_function(ParseContext *ctx, const char *name, TypeHandle type, FunctionDefinition *f) {
    Symbol *oldSym;
    oldSym = jkc99_sym_get_current_scope(ctx, name);
    if(oldSym) {
        jkc99_assert(oldSym->identifier == name);
        if(oldSym->kind == kSymbolFunction && JKC99_TYPE_HANDLE_EQ(oldSym->type, type)) {
            if(oldSym->u.functionDefinition == NULL) {
                oldSym->u.functionDefinition = f;
            } else {
                jkc99_log_error(ctx, "Redefining a previously defined function %s", name);
            }
        } else {
            jkc99_log_error(ctx, "Type of function %s conflicts with a previous declaration", name);
        }
    } else {
        Symbol sym = {0};
        //jkc99_assert(storageClass != kStorageClassTypedef);
        //sym.kind = storageClass == kStorageClassTypedef ? kSymbolType : kSymbolFunction;
        sym.kind = kSymbolFunction;
        sym.identifier = name;
        sym.u.functionDefinition = f;
        sym.type = type;
        jkc99_sym_push(ctx, &sym);
        //da_push(scope->symbols, sym);
    }
}

JKC99_API FunctionDefinition *function_definition(ParseContext *ctx, Source *src, size_t specifierCount, DeclarationSpecifier *specifiers, Declarator *declarator, size_t declarationCount, Declaration **declarations, Stmt *stmt, const char *name, TypeHandle type) {
    jkc99_assert(specifierCount && specifiers);
    jkc99_assert(declarator);
    jkc99_assert(declarationCount == 0 || declarations);
    jkc99_assert(stmt);

    FunctionDefinition *f = jkc99_declaration_alloc(ctx, src, sizeof(FunctionDefinition), specifierCount, specifiers);

    f->declarator = declarator;
    f->declarationCount = declarationCount;
    f->declarations = declarations;
    f->body = stmt;
    f->name = name;
    f->type = type;

    hook_execute(ctx, kHookNewFunctionDefinition, &f);

    if(f) {
        jkc99_sym_push_function(ctx, name, type, f);
    }

    return f;
}

static _Bool jkc99_parse_parameter_list_(ParseContext *ctx, size_t index, ParameterDeclaration ***list, bool *hasEllipsis);

static void jkc99_parse_parameter_list_skip(ParseContext *ctx, size_t index, Source *src, ParameterDeclaration ***list, bool *hasEllipsis) {
    long tokens[] = { ',', ')' };
    jkc99_log_error_src(ctx, src, "Unable to parse parameter number %zu", index);
    jkc99_lexer_skip_until_array(ctx, tokens);
    if(jkc99_lexer_match(ctx, ',')) {
        jkc99_parse_parameter_list_(ctx, index+1, list, hasEllipsis);
    } else {
        *hasEllipsis = false;
    }
}

static _Bool jkc99_parse_parameter_list_(ParseContext *ctx, size_t index, ParameterDeclaration ***list, bool *hasEllipsis) {
    Source src;
    size_t specifierCount;
    DeclarationSpecifier *specifiers = NULL;

    jkc99_lexer_source(ctx, &src);
    
    specifiers = jkc99_parse_declaration_specifiers(ctx, &specifierCount);
    if(specifiers) {
        ParameterDeclaration *decl = NULL;
        Declarator *declarator = NULL;

        declarator = jkc99_parse_declarator(ctx);
        source_end(ctx, &src);
        if(declarator) {
            decl = jkc99_declaration_parameter(ctx, &src, specifierCount, specifiers, declarator, NULL);
        } else {
            decl = jkc99_declaration_parameter(ctx, &src, specifierCount, specifiers, NULL, jkc99_parse_abstract_declarator(ctx));
        }
        da_push(*list, decl);
        if(jkc99_lexer_match(ctx, ',')) {
            jkc99_parse_parameter_list_(ctx, index+1, list, hasEllipsis);
        }
    } else {
        if(index > 1) {
            if(jkc99_lexer_is(ctx, ')') || jkc99_lexer_is(ctx, CLEX_eof)) {
                return true;
            } else {
                Source src2;
                jkc99_lexer_source(ctx, &src2);
                if(jkc99_lexer_match(ctx, '.') && jkc99_lexer_match(ctx, '.') && jkc99_lexer_match(ctx, '.')) { 
                    *hasEllipsis = true;
                    return true;
                } else {
                    jkc99_parse_parameter_list_skip(ctx, index, &src, list, hasEllipsis);
                }
            }
        } else {
            if(jkc99_lexer_is(ctx, ')') || jkc99_lexer_is(ctx, CLEX_eof)) {
                return false;
            } else if(jkc99_lexer_match(ctx, CLEX_id)) {
                if(jkc99_lexer_is(ctx, ',')) {
                    jkc99_lexer_rollback(ctx, &src);
                    return false;
                }
            }
            jkc99_parse_parameter_list_skip(ctx, index, &src, list, hasEllipsis);
        }
    }

    return true;
}

JKC99_API _Bool jkc99_parse_parameter_list(ParseContext *ctx, size_t *count, ParameterDeclaration ***list, bool *hasEllipsis) {
    *hasEllipsis = false;
    _Bool hasParameterList = jkc99_parse_parameter_list_(ctx,1,list,hasEllipsis);
    *count = da_count(*list);
    return hasParameterList;
}

static const char **jkc99_parse_identifier_list_(ParseContext *ctx, const char **list, bool *hasEllipsis) {
    const char *id = NULL;

    id = jkc99_parse_identifier(ctx);

    if(id) {
        da_push(list, id);
        if(jkc99_lexer_match(ctx, ',')) {
            list = jkc99_parse_identifier_list_(ctx,list, hasEllipsis);
        }
    } else if(jkc99_lexer_match(ctx, '.') && jkc99_lexer_match(ctx, '.') && jkc99_lexer_match(ctx, '.')) {
        //TODO Should probably parse this in a way that does not consume on partial match
        *hasEllipsis = true;
    }

    return list;
}

JKC99_API const char **jkc99_parse_identifier_list(ParseContext *ctx, size_t *count, bool *hasEllipsis) {
    const char **ids;
    *hasEllipsis = false;
    ids = jkc99_parse_identifier_list_(ctx,NULL,hasEllipsis);
    *count = da_count(ids);
    return ids;
}

/* TODO Adjust parameter declarators! */
JKC99_API DirectDeclarator *jkc99_parse_direct_declarator(ParseContext *ctx) {
    CallingConvention callingConvention = 0;
    DirectDeclarator *direct = NULL;
    Source src;

    jkc99_lexer_source(ctx, &src);

    callingConvention = jkc99_parse_calling_convention(ctx);

    if(jkc99_lexer_is(ctx, CLEX_id)) {
        const char *id = jkc99_parse_identifier(ctx);
        source_end(ctx, &src);
        direct = jkc99_direct_declarator_identifier(ctx, &src, id);
    } else if(jkc99_lexer_match(ctx, '(')) {
        Declarator *declarator = NULL;
        jkc99_assert(callingConvention == kCallingConventionNone);
        callingConvention = jkc99_parse_calling_convention(ctx);
        declarator = jkc99_parse_declarator(ctx);
        if(declarator) {
            jkc99_lexer_require(ctx, ')');
            source_end(ctx, &src);
            direct = jkc99_direct_declarator_declarator(ctx, &src, declarator);
        } else {
            jkc99_lexer_rollback(ctx, &src);
        }
    }

    while(direct) {
        if(jkc99_lexer_match(ctx, '[')) {
            size_t attributeCount = 0;
            Attribute *attributes = NULL;
            size_t qualifierCount = 0;
            TypeQualifier *qualifiers = NULL;
            bool isStaticSize = false;

            if(jkc99_lexer_match_kw(ctx, static)) {
                isStaticSize = true;
            }

            while(jkc99_parse_type_qualifier_list_(ctx, &qualifiers, &qualifierCount) ||
                    jkc99_parse_attribute_list_(ctx, &attributes, &attributeCount));

            if(jkc99_lexer_match_kw(ctx, static)) {
                isStaticSize = true;
            }

            if(jkc99_lexer_match(ctx, '*')) {
                jkc99_lexer_require(ctx, ']');
                source_end(ctx, &src);
                direct = jkc99_direct_declarator_array_vla(ctx, &src, attributeCount, attributes, direct, qualifierCount, qualifiers);
            } else {
                Expr *expr = jkc99_parse_expr_assignment(ctx);
                jkc99_lexer_require(ctx, ']');
                source_end(ctx, &src);
                direct = jkc99_direct_declarator_array(ctx, &src, attributeCount, attributes, direct, qualifierCount, qualifiers, isStaticSize, expr);
            }
        } else if(jkc99_lexer_match(ctx, '(')) {
            size_t count;
            bool hasEllipsis = false;
            ParameterDeclaration **parameterList = NULL;
            bool hasParameterList = jkc99_parse_parameter_list(ctx, &count, &parameterList, &hasEllipsis);
            if(hasParameterList) {
                jkc99_lexer_require(ctx, ')');
                source_end(ctx, &src);
                direct = jkc99_direct_declarator_func_params(ctx, &src, direct, callingConvention, count, parameterList, hasEllipsis);
            } else {
                const char **ids = jkc99_parse_identifier_list(ctx, &count, &hasEllipsis);
                jkc99_lexer_require(ctx, ')');
                source_end(ctx, &src);
                direct = jkc99_direct_declarator_func_identifiers(ctx, &src, direct, callingConvention, count, ids, hasEllipsis);
            }
        } else {
            break;
        }
    }

    if(!direct) {
        jkc99_lexer_rollback(ctx, &src);
    }

    return direct;
}

JKC99_API Declarator *declarator(ParseContext *ctx, Source *src, size_t beforeAttrCount, Attribute *beforeAttrs, size_t afterAttrCount, Attribute *afterAttrs, size_t ptrCount, Pointer *pointers, DirectDeclarator *dd) {
    Declarator *d = arena_alloc(&ctx->arena, sizeof(Declarator));

    d->src = src ? *src : (Source){0};
    jkc99_parse_empty_directives(ctx, d);
    d->pointerCount = ptrCount;
    d->pointers = pointers;
    d->direct = dd;
    d->beforeAttributeCount = beforeAttrCount;
    d->beforeAttributes = beforeAttrs;
    d->afterAttributeCount = afterAttrCount;
    d->afterAttributes = afterAttrs;

    return d;
}

JKC99_API Declarator *jkc99_parse_declarator(ParseContext *ctx) {
    Declarator *d = NULL;
    Pointer *pointers = NULL;
    Source src;
    DirectDeclarator *direct = NULL;
    Attribute *beforeAttributes = NULL;
    size_t beforeAttributeCount = 0;
    Attribute *afterAttributes = NULL;
    size_t afterAttributeCount = 0;

    jkc99_lexer_source(ctx, &src);
    beforeAttributes = jkc99_parse_attribute_list(ctx, &beforeAttributeCount);
    pointers = jkc99_parse_pointer(ctx, pointers);
    direct = jkc99_parse_direct_declarator(ctx);
    if(direct) {
        afterAttributes = jkc99_parse_attribute_list(ctx, &afterAttributeCount);
        source_end(ctx, &src);
        d = declarator(ctx, &src, beforeAttributeCount, beforeAttributes, afterAttributeCount, afterAttributes, da_count(pointers), pointers, direct);
    } else {
        jkc99_lexer_rollback(ctx, &src);
    }

    return d;
}

static InitDeclarator *jkc99_parse_init_declarator_list_(ParseContext *ctx, InitDeclarator *list) {
    InitDeclarator init = {0};

    init.declarator = jkc99_parse_declarator(ctx);
    if(init.declarator) {
        if(jkc99_lexer_match(ctx, '=')) {
            init.initializer = jkc99_parse_initializer(ctx);
        }
        da_push(list, init);

        if(jkc99_lexer_match(ctx, ',')) {
            list = jkc99_parse_init_declarator_list_(ctx,list);
        }
    }

    return list;
}

JKC99_API InitDeclarator *jkc99_parse_init_declarator_list(ParseContext *ctx, size_t *count) {
    InitDeclarator *inits = jkc99_parse_init_declarator_list_(ctx,NULL);
    *count = da_count(inits);
    return inits;
}


static Declaration *jkc99_parse_declaration_internal(ParseContext *ctx, Source *src, size_t specifierCount, DeclarationSpecifier *specifiers) {
    Declaration *decl = NULL;

    if(specifierCount) {
        Stmt *asmStmt = NULL;
        size_t initCount;
        InitDeclarator *inits = jkc99_parse_init_declarator_list(ctx, &initCount);

        jkc99_assert(specifiers);

#ifdef HAS_GCC_ASM
        /* TODO Handle this properly */
        asmStmt = jkc99_parse_stmt_asm(ctx);
        if(initCount && inits[initCount-1].declarator) {
            Declarator *d = inits[initCount-1].declarator;
            jkc99_parse_attribute_list_(ctx, &d->afterAttributes, &d->afterAttributeCount);
        }
#endif
        jkc99_lexer_require(ctx, ';');
        source_end(ctx,src);
        decl = jkc99_declaration(ctx, src, specifierCount, specifiers, initCount, inits);
        decl->asmStmt = asmStmt;

        //Symbols
        if(JKC99_TYPE_HANDLE_NOT_INVALID(decl->type)) {
            SymbolDeclaration symDecl = {0};
            symDecl.declaration = decl;
            for(size_t i = 0; i < decl->initDeclaratorCount; ++i) {
                InitDeclarator *init = decl->initDeclarators + i;
                symDecl.declaratorIndex = i;

                if(init->declarator)  {
                    TypeHandle type;
                    const char *name = NULL;

                    type = jkc99_declarator_get_type_and_name(ctx, init->declarator, decl->type, &name, &init->initializer);
                    jkc99_assert(JKC99_TYPE_HANDLE_NOT_INVALID(type));

                    if(name) {
                        Symbol *oldSym;
                        oldSym = jkc99_sym_get_current_scope(ctx, name);

                        if(decl->storageClass == kStorageClassTypedef) {
                            if(oldSym) {
                                jkc99_assert(oldSym->kind == kSymbolType); //TODO Error
                                /*TODO  This is a problem we have doing this outside of the actual compiler framework. I don't think we have any obvious way to know which types are compatible with builtin types but maybe there's a better way to do this. Needs looking into.
                                */
                                if(JKC99_TYPE_HANDLE_NEQ(type, oldSym->type)) {
                                    JKC99Type newType = {0};
                                    JKC99Type oldType = {0};
                                    jkc99_type_get(ctx, type, &newType);
                                    jkc99_type_get(ctx, oldSym->type, &oldType);
                                    if(newType.kind != kTypeBuiltin && oldType.kind != kTypeBuiltin) {
                                        jkc99_log_error_src(ctx, src, "Incompatible redeclaration of type %s", name);
                                    }
                                }
                            } else {
                                Symbol sym = {0};
                                sym.kind = kSymbolType;
                                sym.identifier = name;
                                sym.u.definition = symDecl;
                                sym.type = type;
                                jkc99_sym_push(ctx, &sym);
                                //da_push(scope->symbols, sym);
                            }
                        } else {
                            JKC99Type t;
                            jkc99_type_get(ctx, type, &t);
                            if(oldSym) {
                                jkc99_assert(oldSym->identifier == name);
                                //jkc99_assert(JKC99_TYPE_HANDLE_EQ(oldSym->type, type)); //TODO Error

#if _WIN32
                                if(jkc99_type_are_compatible(ctx, type, oldSym->type)
                                        || oldSym->identifier == jkc99_str_intern(ctx, "__swprintf_l")
                                        || oldSym->identifier == jkc99_str_intern(ctx, "__vswprintf_l")
                                        || oldSym->identifier == jkc99_str_intern(ctx, "_swprintf")
                                        || oldSym->identifier == jkc99_str_intern(ctx, "_vswprintf")
                                        || oldSym->identifier == jkc99_str_intern(ctx, "sprintf")
                                        || oldSym->identifier == jkc99_str_intern(ctx, "vsprintf")
                                        || oldSym->identifier == jkc99_str_intern(ctx, "_snprintf")
                                        || oldSym->identifier == jkc99_str_intern(ctx, "_vsnprintf")
                                        ) {
#else
                                if(jkc99_type_are_compatible(ctx, type, oldSym->type)) {
#endif
                                    if(decl->storageClass == kStorageClassExtern || t.kind == kTypeFunc) {
                                        da_push(oldSym->declarations, symDecl);
                                    } else {
                                        if(oldSym->u.definition.declaration == NULL) {
                                            oldSym->u.definition = symDecl;
                                        } else {
                                            jkc99_log_error_src(ctx, src, "Redefinition of %s", name);
                                        }
                                    }
                                } else {
                                    jkc99_log_error_src(ctx, src, "Incompatible redeclaration of %s", name);
                                }
                            } else {
                                Symbol sym = {0};
                                if(t.kind == kTypeFunc) {
                                    sym.kind = kSymbolFunction;
                                } else {
                                    sym.kind = kSymbolVariable;
                                }
                                sym.identifier = name;
                                sym.type = type;
                                if(decl->storageClass == kStorageClassExtern || sym.kind == kSymbolFunction) {
                                    da_push(sym.declarations, symDecl);
                                } else {
                                    sym.u.definition = symDecl;
                                }
                                jkc99_sym_push(ctx, &sym);
                            }
                        }
                    }
                }
            }
        }
    }

    return decl;
}

JKC99_API Declaration *jkc99_parse_declaration(ParseContext *ctx) {
    bool hasExtension = false;
    Declaration *decl = NULL;
    Source src;
    size_t specifierCount = 0;
    DeclarationSpecifier *specifiers = NULL;

    jkc99_lexer_source(ctx, &src);

    hasExtension = jkc99_parse_extension(ctx);
    specifiers = jkc99_parse_declaration_specifiers(ctx, &specifierCount);

    if(specifiers) {
        decl = jkc99_parse_declaration_internal(ctx, &src, specifierCount, specifiers);
        decl->hasExtension = hasExtension;
    } else {
        jkc99_lexer_rollback(ctx, &src);
    }

    return decl;
}


JKC99_API ExternalDeclaration jkc99_parse_external_declaration(ParseContext *ctx) {
    bool hasExtension = false;
    Source src;
    size_t specifierCount;
    DeclarationSpecifier *specifiers = NULL;
    ExternalDeclaration extdecl = {0};

    jkc99_lexer_source(ctx, &src);

    hasExtension = jkc99_parse_extension(ctx);
    specifiers = jkc99_parse_declaration_specifiers(ctx, &specifierCount);
    if(specifiers) {
        Source src2;
        Declarator *declarator;
        Declaration **declarations = NULL;
        Stmt *stmt = NULL;

        jkc99_lexer_source(ctx, &src2);
        
        declarator = jkc99_parse_declarator(ctx);
        if(declarator && declarator->direct && declarator->direct->kind == kDirectDeclaratorFunc) {
            TypeHandle returnType = JKC99_INVALID_TYPE_HANDLE;
            TypeHandle type = JKC99_INVALID_TYPE_HANDLE;
            StorageClassKind storageClass = 0;
            FunctionSpecifier fspec = {0};
            const char *name = NULL;

            {
                DirectDeclarator *dir = declarator->direct->u.func.direct;
                while(dir->kind != kDirectDeclaratorIdentifier) {
                    if(dir->kind == kDirectDeclaratorDeclarator && dir->u.declarator->pointerCount == 0 && dir->u.declarator->direct) {
                        dir = dir->u.declarator->direct;
                    } else {
                        goto parse_function_definition_end;
                    }
                }
            }
            while(1) {
                Declaration *decl = jkc99_parse_declaration(ctx);
                if(decl) {
                    da_push(declarations, decl);
                } else {
                    break;
                }
            }

            //parse___attribute__(ctx, kAttributePositionAfter);

            jkc99_scope_push(ctx);
            //NOTE This is just to create the symbol for the function
            {
                jkc99_declaration_specifiers_get_type_storage_class_and_function_specifier(ctx, specifierCount, specifiers, &returnType, &storageClass, &fspec);
                if(JKC99_TYPE_HANDLE_INVALID(returnType)) {
                    goto parse_function_definition_end;
                } else if(storageClass == kStorageClassTypedef) {
                    goto parse_function_definition_end;
                }

                type = jkc99_declarator_get_type_and_name(ctx, declarator, returnType, &name, NULL);

                if(!name) {
                    goto parse_function_definition_end;
                } else if(JKC99_TYPE_HANDLE_INVALID(type)) {
                    goto parse_function_definition_end;
                }

                jkc99_sym_push_function(ctx, name, type, NULL);
            }
            jkc99_assert(declarator->direct->u.func.hasParameterList || declarator->direct->u.func.count == 0);
            for(size_t i=0; i < declarator->direct->u.func.count; ++i) {
                ParameterDeclaration *p = declarator->direct->u.func.u.parameters[i];
                if(p->declarator) {
                    Symbol sym = { .kind=kSymbolVariable };
                    sym.type = jkc99_declarator_get_type_and_name(ctx, p->declarator, p->type, &sym.identifier, NULL);
                    jkc99_assert(JKC99_TYPE_HANDLE_NOT_INVALID(sym.type));
                    if(sym.identifier) {
                        jkc99_sym_push(ctx, &sym);
                    }
                }
            }

            stmt = jkc99_parse_stmt_compound(ctx);

            if(stmt) {
                jkc99_assert(declarator->direct->kind == kDirectDeclaratorFunc);
                jkc99_scope_pop(ctx);
                source_end(ctx, &src);
                extdecl.func = function_definition(ctx, &src, specifierCount, specifiers, declarator, da_count(declarations), declarations, stmt, name, type);
                if(extdecl.func) {
                    extdecl.func->hasExtension = hasExtension;
                } else {
                    jkc99_assert(false);
                }
            }
        }

parse_function_definition_end:

        if(!extdecl.func) {
            jkc99_lexer_rollback(ctx, &src2);
            if(da_count(ctx->scopes) > 1) {
                jkc99_assert(da_count(ctx->scopes) == 2);
                jkc99_scope_pop(ctx);
            }
            extdecl.decl = jkc99_parse_declaration_internal(ctx, &src, specifierCount, specifiers);
            extdecl.decl->hasExtension = hasExtension;
        }
    } else {
        jkc99_lexer_rollback(ctx, &src);
    }

    return extdecl;
}

UNUSED static void jkc99_parse_init(ParseContext *ctx, const char *filename) {
    ctx->filename = filename;
    
    jkc99_scope_push(ctx);

    jkc99_assert(da_count(ctx->types) == 0);
    da_push(ctx->types, (JKC99Type){ .kind = kTypeUnknown }); /* Type index 0 is invalid */

    //TODO We need to cater to the possibility of the target compiler or platform being different to the compiler used for the parser itself
    {
        size_t index = da_count(ctx->types);

        ctx->type_void = JKC99_INDEX_TO_TYPE_HANDLE(index++);
        da_push(ctx->types, (JKC99Type){ .kind = kTypeVoid });

        ctx->type_char = JKC99_INDEX_TO_TYPE_HANDLE(index++);
        da_push(ctx->types, (JKC99Type){ .kind = kTypeChar, .u.i = { .name = "char", .size = sizeof(char), .min = CHAR_MIN, .max = CHAR_MAX }});

        ctx->type_signed_char = JKC99_INDEX_TO_TYPE_HANDLE(index++);
        da_push(ctx->types, (JKC99Type){ .kind = kTypeSignedChar, .u.i = { .name = "signed char", .size = sizeof(signed char), .min = SCHAR_MIN, .max = SCHAR_MAX }});

        ctx->type_signed_short = JKC99_INDEX_TO_TYPE_HANDLE(index++);
        da_push(ctx->types, (JKC99Type){ .kind = kTypeSignedShort, .u.i = { .name = "signed short", .size = sizeof(signed short), .min = SHRT_MIN, .max = SHRT_MAX }});

        ctx->type_signed_int = JKC99_INDEX_TO_TYPE_HANDLE(index++);
        da_push(ctx->types, (JKC99Type){ .kind = kTypeSignedInt, .u.i = { .name = "signed int", .size = sizeof(signed int), .min = INT_MIN, .max = INT_MAX }});

        ctx->type_signed_long = JKC99_INDEX_TO_TYPE_HANDLE(index++);
        da_push(ctx->types, (JKC99Type){ .kind = kTypeSignedLong, .u.i = { .name = "signed long", .size = sizeof(signed long), .min = LONG_MIN, .max = LONG_MAX }});

        ctx->type_signed_long_long = JKC99_INDEX_TO_TYPE_HANDLE(index++);
        da_push(ctx->types, (JKC99Type){ .kind = kTypeSignedLongLong, .u.i = { .name = "signed long long", .size = sizeof(signed long long), .min = LLONG_MIN, .max = LLONG_MAX }});

        ctx->type_unsigned_char = JKC99_INDEX_TO_TYPE_HANDLE(index++);
        da_push(ctx->types, (JKC99Type){ .kind = kTypeUnsignedChar, .u.i = { .name = "unsigned char", .size = sizeof(unsigned char), .min = 0, .max = UCHAR_MAX }});

        ctx->type_unsigned_short = JKC99_INDEX_TO_TYPE_HANDLE(index++);
        da_push(ctx->types, (JKC99Type){ .kind = kTypeUnsignedShort, .u.i = { .name = "unsigned short", .size = sizeof(unsigned short), .min = 0, .max = USHRT_MAX }});

        ctx->type_unsigned_int = JKC99_INDEX_TO_TYPE_HANDLE(index++);
        da_push(ctx->types, (JKC99Type){ .kind = kTypeUnsignedInt, .u.i = { .name = "unsigned int", .size = sizeof(unsigned int), .min = 0, .max = UINT_MAX }});

        ctx->type_unsigned_long = JKC99_INDEX_TO_TYPE_HANDLE(index++);
        da_push(ctx->types, (JKC99Type){ .kind = kTypeUnsignedLong, .u.i = { .name = "unsigned long", .size = sizeof(unsigned long), .min = 0, .max = ULONG_MAX }});

        ctx->type_unsigned_long_long = JKC99_INDEX_TO_TYPE_HANDLE(index++);
        da_push(ctx->types, (JKC99Type){ .kind = kTypeUnsignedLongLong, .u.i = { .name = "unsigned long long", .size = sizeof(unsigned long long), .min = 0, .max = ULLONG_MAX }});

#define SET_CORRESPONDING_INTEGER_TYPE(ctx,type) \
        do { \
            JKC99Type t; \
            jkc99_type_get((ctx),(ctx)->type_signed_##type,&t); \
            t.u.i.correspondingType = (ctx)->type_unsigned_##type; \
            jkc99_type_set((ctx),(ctx)->type_signed_##type,&t); \
            jkc99_type_get((ctx),(ctx)->type_unsigned_##type,&t); \
            t.u.i.correspondingType = (ctx)->type_signed_##type; \
            jkc99_type_set((ctx),(ctx)->type_unsigned_##type,&t); \
        } while(0)

        SET_CORRESPONDING_INTEGER_TYPE(ctx,char);
        SET_CORRESPONDING_INTEGER_TYPE(ctx,short);
        SET_CORRESPONDING_INTEGER_TYPE(ctx,int);
        SET_CORRESPONDING_INTEGER_TYPE(ctx,long);
        SET_CORRESPONDING_INTEGER_TYPE(ctx,long_long);

#undef SET_CORRESPONDING_INTEGER_TYPE

        ctx->type__Bool = JKC99_INDEX_TO_TYPE_HANDLE(index++);
        //TODO AFAIK there is no min/max macros for _Bool?
        da_push(ctx->types, (JKC99Type){ .kind = kType_Bool, .u.i = { .name = "_Bool", .size = sizeof(_Bool), .min = 0, .max = 1 }});

        ctx->type_float = JKC99_INDEX_TO_TYPE_HANDLE(index++);
        da_push(ctx->types, (JKC99Type){ .kind = kTypeFloat, .u.flt = { .name = "float", .size = sizeof(float), .min = FLT_MIN, .max = FLT_MAX }});

        ctx->type_double = JKC99_INDEX_TO_TYPE_HANDLE(index++);
        da_push(ctx->types, (JKC99Type){ .kind = kTypeDouble, .u.flt = { .name = "double", .size = sizeof(double), .min = DBL_MIN, .max = DBL_MAX }});

        ctx->type_long_double = JKC99_INDEX_TO_TYPE_HANDLE(index++);
        da_push(ctx->types, (JKC99Type){ .kind = kTypeLongDouble, .u.flt = { .name = "long double", .size = sizeof(long double), .min = LDBL_MIN, .max = LDBL_MAX }});

#ifdef JKC99_HAS_COMPLEX
        ctx->type_float__Complex = JKC99_INDEX_TO_TYPE_HANDLE(index++);
        da_push(ctx->types, (JKC99Type){ .kind = kTypeFloat_Complex, .u.c = { .name = "float _Complex", .size = sizeof(Float_Complex), .correspondingType = ctx->type_float }});

        ctx->type_double__Complex = JKC99_INDEX_TO_TYPE_HANDLE(index++);
        da_push(ctx->types, (JKC99Type){ .kind = kTypeDouble_Complex, .u.c = { .name = "double _Complex", .size = sizeof(Double_Complex), .correspondingType = ctx->type_double }});

        ctx->type_long_double__Complex = JKC99_INDEX_TO_TYPE_HANDLE(index++);
        da_push(ctx->types, (JKC99Type){ .kind = kTypeLongDouble_Complex, .u.c = { .name = "long double _Complex", .size = sizeof(LongDouble_Complex), .correspondingType = ctx->type_long_double }});
#endif


        //Compiler dependent builtin types
#ifdef __clang__
        {
            Symbol sym = {0};
            JKC99Scope *scope = &da_last(ctx->scopes);

            ctx->type___builtin_va_list = JKC99_INDEX_TO_TYPE_HANDLE(index++);
            da_push(ctx->types, (JKC99Type){ .kind = kTypeBuiltin, .u.builtin.name = jkc99_str_intern(ctx, "__builtin_va_list")}); 

            sym.kind = kSymbolType;
            sym.identifier = jkc99_str_intern(ctx, "__builtin_va_list");
            sym.type = ctx->type___builtin_va_list;
            da_push(scope->symbols, sym);
        }
#endif

    }

    ctx->parsing = true;
}

JKC99_API ParseBuffer jkc99_buffer_new(ParseContext *ctx, const char *filename, size_t bufSize, const char *buffer) {
    stb_lexer *lexer = arena_alloc(&ctx->arena, sizeof(stb_lexer));
    char *stringStorage = arena_alloc(&ctx->arena, STRING_STORAGE_SIZE);
    memset(lexer, 0, sizeof(stb_lexer));

    stb_c_lexer_init(lexer, buffer, buffer + bufSize, stringStorage, STRING_STORAGE_SIZE);
    lexer->file = jkc99_str_intern(ctx, filename);
    lexer->line = 1;
    lexer->lineBegin = lexer->input_stream;
    lexer->lineMarkerFlags = 0;

    return (ParseBuffer)lexer;
}

JKC99_API void jkc99_buffer_set(ParseContext *ctx, ParseBuffer lexer) {
    ctx->lexer = (stb_lexer*)lexer;

    if(!ctx->lexer->initialized) {
        jkc99_lexer_next(ctx);
        ctx->lexer->initialized = true;
    }
}

JKC99_API ParseBuffer jkc99_buffer_get(ParseContext *ctx) {
    return (ParseBuffer)ctx->lexer;
}

JKC99_API const char    *jkc99_buffer_file(UNUSED ParseContext *ctx, ParseBuffer buf) {
    return ((stb_lexer*)buf)->file;
}

JKC99_API void jkc99_buffer_parse(ParseContext *ctx, ParseBuffer buf) {
    ParseBuffer oldbuf = jkc99_buffer_get(ctx);
    jkc99_buffer_set(ctx, buf);
    jkc99_parse(ctx);
    jkc99_buffer_set(ctx, oldbuf);
}

JKC99_API size_t                jkc99_ext_decl_count(ParseContext *ctx) {
    return da_count(ctx->externalDeclarations);
}

JKC99_API ExternalDeclaration   *jkc99_ext_decl_first(ParseContext *ctx) {
    return ctx->externalDeclarations;
}

JKC99_API ExternalDeclaration   *jkc99_ext_decl_next(ParseContext *ctx, ExternalDeclaration *ref, size_t count) {
    size_t index = ref - ctx->externalDeclarations + count;
    jkc99_assert(ref >= ctx->externalDeclarations);
    return (index < da_count(ctx->externalDeclarations) ? ctx->externalDeclarations + index : NULL);
}

void jkc99_parse(ParseContext *ctx) {
    ctx->parsing = true;
    while(ctx->parsing) {
        Source src;
        jkc99_lexer_source(ctx, &src);
        switch(ctx->lexer->token) {
            case CLEX_parse_error:
                {
                    jkc99_assert(false);
                    ctx->parsing = false;
                } break;
            case CLEX_eof:
                {
                    ctx->parsing = false;
                    return;
                } break;
            case ';':
                {
                    jkc99_lexer_next(ctx);
                } break;
            default:
                {
                    if(ctx->lexer->token == CLEX_id && jkc99_lexer_match_kw(ctx, __pragma)) {
                        if(jkc99_lexer_match(ctx, '(')) {
                            unsigned int level = 1;
                            while(!jkc99_lexer_is(ctx, CLEX_eof) && level) {
                                if(jkc99_lexer_match(ctx, '(')) {
                                    ++level;
                                } else if(jkc99_lexer_match(ctx, ')')) {
                                    --level;
                                } else {
                                    jkc99_lexer_next(ctx);
                                }
                            }
                        }
                    } else {
                        ExternalDeclaration extDecl;

                        extDecl = jkc99_parse_external_declaration(ctx);
                        if(extDecl.decl || extDecl.func) {
                            hook_execute(ctx, kHookNewExternalDeclaration, &extDecl);
                            if(extDecl.decl || extDecl.func) {
                                da_push(ctx->externalDeclarations, extDecl);
                            }
                        } else {
                            jkc99_assert(false);
                            jkc99_log_error_unexpected(ctx);
                            jkc99_lexer_next(ctx);
                        }
                    }
                } break;
        }
    }
}

JKC99_API JKC99_INSTALL_HOOK(jkc99_install_hook) {
    Hook hook = {0};
    hook.func = func;
    hook.ptr = ptr;
    if(kind < kHookCount) {
        da_push(ctx->hooks[kind], hook);
    } else {
        fprintf(stderr, "Unable install hook on index %d, only %d hooks available. The module requires a newer version of JKC99\n", kind, kHookCount);
    }
}


static size_t gModuleCount = 0;
static JKC99Module *gModules = NULL;

OPTION_FUNC(help);
OPTION_FUNC(list);

static Option gOptions[] = {
    OPTION('h', help, "Display available options"),
    OPTION('l', list, "List available modules")
};

static void jkc99_print_option_help(size_t optionCount, Option *options) {
    for(size_t i = 0; i < optionCount; ++i) {
        Option *opt = options + i;
        if(opt->func) {
            int pos;
            if(opt->name) {
                pos = printf("  -%c --%s", opt->letter, opt->name);
            } else {
                pos = printf("  -%c", opt->letter);
            }
            printf("%*.s %s\n", (int)(22 - pos), "                                              ", opt->helpString);
        }
    }
}


OPTION_FUNC(help) {
    printf("Usage: jkc99 [options] <target> <<module-name> [module-options]>...\n\n");

    printf("Options:\n");
    jkc99_print_option_help(jkc99_array_count(gOptions), gOptions);
    //printf("%s", jkc99_get_option_help(jkc99_array_count(gOptions), gOptions));
    printf("\n");

    for(size_t i = 0; i < gModuleCount; ++i) {
        JKC99Module *mod = gModules + i;
        printf("Module %s:\n", mod->name);
        if(mod->optionCount) {
            jkc99_print_option_help(mod->optionCount, mod->options);
        } else {
            printf("  This module has no options specified\n");
        }
#if 0
        if(gModules[i].get_help) {
            printf("%s", gModules[i].get_help());
        } else {
            printf("  No help information provided for this module\n");
        }
#endif
        printf("\n");
    }

    return false;
}

OPTION_FUNC(list) {
    printf("Available modules:\n");
    for(size_t i = 0; i < gModuleCount; ++i) {
        printf("  %s\n", gModules[i].name);
    }
    return false;
}

#define STR_MODULE_INIT_NOT_FOUND "Function jkc99_module_init not found in module %s\n"
#define STR_GET_VERSION_NOT_FOUND "Function jkc99_get_version not found in module %s\n"
#define STR_UNABLE_TO_RESOLVE_PATH 

#if defined(_WIN32)

#include <windows.h>
#include <tchar.h>

#ifdef UNICODE
static inline void char_to_tchar(TCHAR *dest, const char *src, size_t len) {
    mbstowcs(dest, src, len);
}
static inline void tchar_to_char(char *dest, const TCHAR *src, size_t len) {
    wcstombs(dest, src, len);
}
#else
static inline void char_to_tchar(TCHAR *dest, const char *src, size_t len) {
    strncpy(dest, src, len);
}
static inline void tchar_to_char(char *dest, const TCHAR *src, size_t len) {
    strncpy(dest, src, len);
}
#endif

#if (NTDDI_VERSION >= NTDDI_WIN8) && defined(UNICODE)
#include <pathcch.h>
#pragma comment(lib, "pathcch.lib")
static void remove_file_specifier(TCHAR *str, size_t len) {
    PathCchRemoveFileSpec(str, len);
}
#else 
#include <shlwapi.h>
#pragma comment(lib, "shlwapi.lib")
static void remove_file_specifier(TCHAR *str, size_t len) {
    jkc99_assert(len <= MAX_PATH);
    PathRemoveFileSpec(str);
}
#endif

int win32_compare_file_name(const void *vl, const void *vr) {
    WIN32_FIND_DATAA *l, *r;
    l = (WIN32_FIND_DATAA*)vl;
    r = (WIN32_FIND_DATAA*)vr;

    return strcmp(l->cFileName, r->cFileName);
}

JKC99Module *jkc99_os_load_modules(size_t *moduleCount) {
    JKC99Module *modules = NULL;
    HMODULE exeHandle;
    TCHAR modulePath[MAX_PATH] = TEXT("");
    DWORD modulePathLen = 0;
    DWORD attr;

    *moduleCount = 0;

    exeHandle = GetModuleHandle(NULL);
    if(exeHandle == NULL) {
        jkc99_assert(false); /* TODO Error */
        return NULL;
    }

    modulePathLen = GetModuleFileName(exeHandle, modulePath, jkc99_array_count(modulePath));
    if(modulePathLen == 0) {
        jkc99_assert(false); /* TODO Error */
        return NULL;
    }

    remove_file_specifier(modulePath, modulePathLen);

    attr = GetFileAttributes(modulePath);
    if(attr == INVALID_FILE_ATTRIBUTES) {
        _ftprintf(stderr, TEXT("Unable to resolve module path.\n"));
        jkc99_assert(false);
    } else if(attr & FILE_ATTRIBUTE_DIRECTORY) {
        size_t len;
        WIN32_FIND_DATA find = {0};
        HANDLE findHandle;

        len = _tcslen(modulePath);
        jkc99_assert((len+6) < jkc99_array_count(modulePath));
        if(!(modulePath[len-1] == '\\' || modulePath[len-1] == '/')) {
            modulePath[len++] = '\\';
        }
        modulePath[len++] = '*';
        modulePath[len++] = '.';
        modulePath[len++] = 'd';
        modulePath[len++] = 'l';
        modulePath[len++] = 'l';
        modulePath[len]   = '\0';

        findHandle = FindFirstFile(modulePath, &find);
        if(findHandle == INVALID_HANDLE_VALUE) {
            DWORD err = GetLastError();
            if(err == ERROR_FILE_NOT_FOUND) {
                _ftprintf(stderr, TEXT("No modules found using path %s\n"), modulePath);
            } else {
                jkc99_assert(false); /* TODO Error */
            }
        } else {
            size_t i;
            WIN32_FIND_DATA *files = NULL;

            do {
                da_push(files, find);
            } while(FindNextFile(findHandle, &find));

            qsort(files, da_count(files), sizeof(files[0]), win32_compare_file_name);

            for(i = 0; i < da_count(files); ++i) {
                HMODULE dll;
                dll = LoadLibrary(files[i].cFileName);
                if(dll == NULL) {
                    jkc99_assert(false); /* TODO Error */
                } else {
                    jkc99_get_version_func *module_version = (jkc99_get_version_func*)GetProcAddress(dll, "jkc99_get_version");
                    if(module_version) {
                        jkc99_module_init_func *module_init = (jkc99_module_init_func*)GetProcAddress(dll, "jkc99_module_init");
                        if(module_init) {
                            jkc99_module_get_options_func *module_get_options = (jkc99_module_get_options_func*)GetProcAddress(dll, "jkc99_module_get_options");
                            JKC99Module module = {0};
                            size_t len = _tcslen(files[i].cFileName);
                            char moduleName[MAX_PATH] = {0};

                            tchar_to_char(moduleName, files[i].cFileName, len-4);
                            //module.name = jkc99_str_intern_range(ctx, moduleName, len-4);
                            module.name = malloc(len-3);
                            memcpy((void*)module.name, moduleName, len-3);
                            module.get_version = module_version;
                            module.init = module_init;
                            module.get_options = module_get_options;
                            if(module.get_options) {
                                module.options = module.get_options(&module.optionCount);
                            } else {
                                module.optionCount = 0;
                                module.options = NULL;
                            }
                            da_push(modules, module);
                        } else {
                            _ftprintf(stderr, TEXT(STR_MODULE_INIT_NOT_FOUND), files[i].cFileName);
                        }
                    } else {
                        _ftprintf(stderr, TEXT(STR_GET_VERSION_NOT_FOUND), files[i].cFileName);
                        jkc99_assert(false); /* TODO Error */
                    }
                } 
            }

        }
    } else {
        _ftprintf(stderr, TEXT("Unable to resolve module path.\n"));
        jkc99_assert(false);
    }

    *moduleCount = da_count(modules);
    return modules;
}

JKC99_API JKC99_READ_ENTIRE_FILE(jkc99_os_read_entire_file) {
    TCHAR filenameT[MAX_PATH];
    char *buf = NULL;
    HANDLE fileHandle;

    jkc99_assert(len);

    char_to_tchar(filenameT, filename, jkc99_array_count(filenameT));

    fileHandle = CreateFile(filenameT, GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, 0, NULL);

    if(fileHandle == INVALID_HANDLE_VALUE) {
        printf("Unable to find file using path: %s\n", filename);
    } else {
        DWORD read = 0;
        DWORD size;

        size = GetFileSize(fileHandle, NULL);
        if(size == INVALID_FILE_SIZE) {
            printf("Error on GetFileSize for %s\n", filename);
        } else {
            buf = malloc(size+1); //TODO VirtualAlloc?
            if(buf) {
                if(ReadFile(fileHandle, buf, size, &read, 0)) {
                    jkc99_assert(read == size); //TODO
                    buf[size] = 0;
                    *len = size;
                    CloseHandle(fileHandle);
                } else {
                    free(buf);
                    buf = NULL;
                }
            } else {
                printf("Unable to allocate %lu bytes\n", size+1);
            }
        }
    }
    return buf;
}

JKC99_API JKC99_WRITE_BUFFER_TO_FILE(jkc99_os_write_buffer_to_file) {
    int returnVal = 0;
    TCHAR filenameT[MAX_PATH];
    HANDLE fileHandle;
    DWORD written;

    jkc99_assert(filename);
    jkc99_assert(buf);
    jkc99_assert(len);
    jkc99_assert(len <= ULONG_MAX);

    char_to_tchar(filenameT, filename, jkc99_array_count(filenameT));

    fileHandle = CreateFile(filenameT, GENERIC_WRITE, 0, NULL, CREATE_NEW, FILE_ATTRIBUTE_NORMAL, NULL);

    if(fileHandle == INVALID_HANDLE_VALUE) {
        fprintf(stderr, "Unable to open %s for write\n", filename);
        returnVal = 1;
    }

    if(!WriteFile(fileHandle, buf, (DWORD)len, &written, NULL)) {
        fprintf(stderr, "Unable to write %zu bytes to %s\n", len, filename);
        returnVal = 2;
    }

    CloseHandle(fileHandle);

    return returnVal;
} 

#elif defined(__linux__)

#include <sys/stat.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <dirent.h>
#include <dlfcn.h>
#include <linux/limits.h>

JKC99Module *jkc99_os_load_modules(size_t *moduleCount) {
    char *lastSlash;
    JKC99Module *modules = NULL;
    char modulePath[PATH_MAX] = {0};

    readlink("/proc/self/exe", modulePath, sizeof(modulePath));
    lastSlash = strrchr(modulePath, '/');
    if(lastSlash) {
        *lastSlash = '\0';
    }

    jkc99_assert(modulePath[0]);
    DIR *dir = opendir(modulePath);

    if(dir) {
        struct dirent *dent = readdir(dir);
        while(dent) {
            size_t len = strlen(dent->d_name);
            if(dent->d_name[len-3] == '.' && dent->d_name[len-2] == 's' && dent->d_name[len-1] == 'o') {
                void *so;
                char filePath[PATH_MAX];
                snprintf(filePath, sizeof(filePath), "%s/%s", modulePath, dent->d_name);

                so = dlopen(filePath, RTLD_LAZY);
                if(so) {
                    jkc99_get_version_func *module_version = (jkc99_get_version_func*)dlsym(so, "jkc99_get_version");
                    if(module_version) {
                        jkc99_module_init_func *module_init = (jkc99_module_init_func*)dlsym(so, "jkc99_module_init");
                        if(module_init) {
                            jkc99_module_get_options_func *module_get_options = (jkc99_module_get_options_func*)dlsym(so, "jkc99_module_get_options");
                            JKC99Module module = {0};

                            module.name = malloc(len-2);
                            memcpy((char*)module.name, dent->d_name, len-3);
                            *(((char*)module.name) + len-3) = 0;
                            module.get_version = module_version;
                            module.init = module_init;
                            module.get_options = module_get_options;
                            if(module.get_options) {
                                module.options = module.get_options(&module.optionCount);
                            } else {
                                module.optionCount = 0;
                                module.options = NULL;
                            }
                            da_push(modules, module);
                        } else {
                            fprintf(stderr, STR_MODULE_INIT_NOT_FOUND, filePath);
                        }
                    } else {
                        fprintf(stderr, STR_GET_VERSION_NOT_FOUND, filePath);
                    }
                } else {
                    fprintf(stderr, "%s\n", dlerror());
                }
            }
            dent = readdir(dir);
        }

        closedir(dir);
    } else {
        char errStr[PATH_MAX + 100];
        snprintf(errStr, sizeof(errStr), "Unable to open module directory %s", modulePath);
        perror(errStr);
    }

    *moduleCount = da_count(modules);
    return modules;
}

JKC99_API JKC99_READ_ENTIRE_FILE(jkc99_os_read_entire_file) {
    char *ptr = NULL;
    int file;

    *len = 0;
    file = open(filename, O_RDONLY);

    if(file >= 0) {
        struct stat st = {0};
        int statRet = fstat(file, &st);

        if(statRet == 0) {
            if(st.st_size > 0) {
                ptr = mmap(NULL, st.st_size, PROT_READ, MAP_PRIVATE, file, 0);
                if(ptr == MAP_FAILED) {
                    char errStr[PATH_MAX + 100];
                    snprintf(errStr, sizeof(errStr), "Unable to mmap file %s", filename);
                    perror(errStr);
                    ptr = NULL;
                } else {
                    *len = st.st_size;
                }
            } else {
                jkc99_assert(st.st_size == 0);
            }
        } else {
            char errStr[PATH_MAX + 100];
            snprintf(errStr, sizeof(errStr), "Unable to stat file %s", filename);
            perror(errStr);
        }
    } else {
        char errStr[PATH_MAX + 100];
        snprintf(errStr, sizeof(errStr), "Unable to read file %s", filename);
        perror(errStr);
    }

    return ptr;
}

JKC99_API JKC99_WRITE_BUFFER_TO_FILE(jkc99_os_write_buffer_to_file) {
    int returnVal = 0;

    if(len) {
        int file;

        jkc99_assert(buf);
        file = open(filename, O_WRONLY | O_CREAT | O_TRUNC);

        if(file >= 0) {
            const int maxZero = 5;
            const int maxEINTR = 5;
            int EINTRCount = 0;
            int zeroCount = 0;
            size_t written = 0;

            while(written < len) {
                ssize_t ret = write(file, buf + written, len - written);

                if(ret > 0) {
                    written += (size_t)ret;
                    if(written == len) {
                        break;
                    }
                    jkc99_assert(written < len);
                } else if(ret == 0) {
                    if(++zeroCount >= maxZero) {
                        char errStr[PATH_MAX + 100];
                        snprintf(errStr, sizeof(errStr), "Maximum zero bytes written count exceeded on file %s", filename);
                        perror(errStr);
                        returnVal = 1;
                        break;
                    }
                } else {
                    if(errno == EINTR) {
                        if(++EINTRCount >= maxEINTR) {
                            char errStr[PATH_MAX + 100];
                            snprintf(errStr, sizeof(errStr), "Maximum interrupt count exceeded trying to write file %s", filename);
                            perror(errStr);
                            returnVal = 2;
                            break;
                        }
                    } else {
                        char errStr[PATH_MAX + 100];
                        snprintf(errStr, sizeof(errStr), "Unable to write file %s", filename);
                        perror(errStr);
                        returnVal = 3;
                        break;
                    }
                }
            }
        } else {
            char errStr[PATH_MAX + 100];
            snprintf(errStr, sizeof(errStr), "Unable to write file %s", filename);
            perror(errStr);
            returnVal = 4;
        }
    }
    return returnVal;
}

#else
#error "Unsupported operating system"
#endif

UNUSED static bool jkc99_execute_options(ParseContext *ctx, size_t optionCount, Option *options, int argc, char **argv) {
    bool carryOn = true;
    int i = 0;
    while(i < argc) {
        jkc99_assert(argv[i][0] == '-');
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
            for(size_t j = 0; j < optionCount; ++j) {
                if(gOptions[j].nameLen == len && strncmp(options[j].name, argv[i] + 2, len) == 0) {
                    bool res = options[j].func(ctx, val);
                    carryOn = carryOn ? res : false;
                    found = true;
                    break;
                }
            }
            if(!found) {
                fprintf(stderr, "Unrecognised option %s\n", argv[i]);
                carryOn = false;
            }
        } else if(argv[i][1]) {
            bool found = false;
            for(size_t j = 0; j < optionCount; ++j) {
                if(options[j].letter == argv[i][1]) {
                    found = true;
                    option_func *func = options[j].func;
                    if(func) {
                        bool res = func(ctx, argv[i][2] == '=' ? argv[i] + 3 : NULL);
                        carryOn = carryOn ? res : false;
                    } else {
                        fprintf(stderr, "Unrecognised option -%c\n", argv[i][1]);
                        carryOn = false;
                    }
                }
            }
            if(!found) {
                fprintf(stderr, "Unrecognised option -%c\n", argv[i][1]);
                carryOn = false;
            }
        }
        ++i;
    }

    return carryOn;
}

#ifdef JKC99_TEST_BUILD

#include "unit_tests.c"
int main(UNUSED int argc, UNUSED char **argv) {
    return jkc99_test_run();
}

#else /* JKC99_TEST_BUILD */

int main(int argc, char **argv) {
    ParseContext ctx = {0};
    int returnVal = 0;
#define X(keyword) kw[kw_##keyword] = jkc99_str_intern(&ctx, #keyword);
#include "keywords.h"

    gModules = jkc99_os_load_modules(&gModuleCount);

    if(gModules) {
        bool carryOn = true;
        int i = 1;
        while(i < argc && argv[i][0] == '-') {
            ++i;
        }
        carryOn = jkc99_execute_options(&ctx, jkc99_array_count(gOptions), gOptions, i-1, argv + 1);
        if(carryOn) {
            if(i < argc) {
                size_t bufferSize;
                const char *buffer;
                const char *targetName = argv[i];

                buffer = jkc99_os_read_entire_file(targetName, &bufferSize);
                if(buffer) {
                    jkc99_parse_init(&ctx, targetName);
                    //gJKC99.push_buffer(&ctx, targetName, bufferSize, buffer);
                    ParseBuffer parseBuf = jkc99_buffer_new(&ctx, targetName, bufferSize, buffer);

                    ++i;
                    if(i < argc) {
                        while(i < argc) {
                            size_t j = 0;
                            const char *moduleName = jkc99_str_intern(&ctx, argv[i++]);
                            for(; j < gModuleCount; ++j) {
                                JKC99Module *mod = gModules + j;
                                if(strcmp(mod->name, moduleName) == 0) {
                                    JKC99Version mVersion = {0};
                                    int moduleArgc = 0;
                                    char **moduleArgv = argv + i;

                                    while(i < argc && argv[i][0] == '-') {
                                        ++moduleArgc;
                                        ++i;
                                    }

                                    /* NOTE This is not really meant to be a robust version system.
                                     * It's mainly there to catch cases where main program and modules get accidentally out of sync. */
                                    mVersion = mod->get_version();
                                    if(mVersion.size == gVersion.size
                                        &&  mVersion.keywordCount <= gVersion.keywordCount
                                        &&  mVersion.keywords
                                        &&  mVersion.compiler == gVersion.compiler
                                        &&  mVersion.hookCount <= gVersion.hookCount
                                        ) {
                                        for(size_t j = 0; j < mVersion.keywordCount; ++j) {
                                            mVersion.keywords[j] = kw[j];
                                        }
                                        carryOn = jkc99_execute_options(&ctx, mod->optionCount, mod->options, moduleArgc, moduleArgv);
                                        if(carryOn) {
                                            if(!mod->init(&ctx)) {
                                                returnVal = 1;
                                            }
                                        } else {
                                            returnVal = 1;
                                        }
                                    } else {
                                        fprintf(stderr, "Module %s is incompatible with the host application, please make sure the versions are kept in sync.\n", mod->name);
                                        returnVal = 1;
                                    }
                                    break;
                                }
                            }
                            if(j == gModuleCount) {
                                fprintf(stderr, "Module %s not found!\n", moduleName);
                                returnVal = 1;
                            }
                        }
                    } else {
                        fprintf(stderr, "No modules specified.\n");
                        returnVal = 1;
                    }

                    if(returnVal == 0) {
                        jkc99_buffer_set(&ctx, parseBuf);
                        jkc99_parse(&ctx);
                        hook_execute(&ctx, kHookParseComplete, NULL);
                    }
                } else {
                    fprintf(stderr, "Unable to read target %s\n", targetName);
                    returnVal = 1;
                }
            } else {
                option_help(&ctx, NULL);
            }
        }
    } else {
        printf("No modules found!\n");
        returnVal = 1;
    }
    arena_free(&ctx.arena);

    return returnVal;
}

#endif /* JKC99_TEST_BUILD */
