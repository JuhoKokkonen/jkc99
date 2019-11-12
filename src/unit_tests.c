#include <string.h>
#include <wchar.h>


static ParseContext *jkc99_test_context(void) {
    static ParseContext *ctx = NULL;
    if(!ctx) {
        ctx = malloc(sizeof(ParseContext));
        memset(ctx, 0, sizeof(ParseContext));
        jkc99_parse_init(ctx, "");
    }
    return ctx;
}

#define TEST_CTX_BUF(str) \
    ParseContext *ctx = jkc99_test_context(); \
    ParseBuffer buf = jkc99_buffer_new(ctx, "", strlen(str), str); \
    jkc99_buffer_set(ctx, buf)

JKC99_TEST(expr_int_basic) {
    TEST_CTX_BUF("12345");
    Expr *e = jkc99_parse_expr(ctx);
    JKC99T(e);
    JKC99T(e->kind == kExprPrimary);
    JKC99T(e->u.primary.kind == kExprPrimaryConstant);
    JKC99T(e->u.primary.u.constant.kind == kConstantInteger);
    JKC99T(e->u.primary.u.constant.representation == kConstantDecimal);
    JKC99T(e->u.primary.u.constant.suffix[0] == '\0');
    JKC99T(e->u.primary.u.constant.u.intVal == 12345);
}

JKC99_TEST(expr_int_suffix) {
    TEST_CTX_BUF("123456789012ull");
    Expr *e = jkc99_parse_expr(ctx);
    JKC99T(e);
    JKC99T(e->kind == kExprPrimary);
    JKC99T(e->u.primary.kind == kExprPrimaryConstant);
    JKC99T(e->u.primary.u.constant.kind == kConstantInteger);
    JKC99T(e->u.primary.u.constant.representation == kConstantDecimal);
    JKC99T(strncmp(e->u.primary.u.constant.suffix, "ull", 3) == 0);
    JKC99T(e->u.primary.u.constant.u.intVal == 123456789012ull);
}

JKC99_TEST(expr_string) {
    const char *str = "\"Test123\a\b\v\f\t\n\r\"";
    TEST_CTX_BUF(str);
    Expr *e = jkc99_parse_expr(ctx);
    JKC99T(e);
    JKC99T(e->kind == kExprPrimary);
    JKC99T(e->u.primary.kind == kExprPrimaryStringLiteral);
    JKC99T(strncmp(e->u.primary.u.str.str, str+1, strlen(str)-2) == 0);
}

JKC99_TEST(expr_identifier) {
    const char *str = "foobar";
    TEST_CTX_BUF(str);
    Expr *e = jkc99_parse_expr(ctx);
    JKC99T(e);
    JKC99T(e->kind == kExprPrimary);
    JKC99T(e->u.primary.kind == kExprPrimaryIdentifier);
    JKC99T(strcmp(e->u.primary.u.identifier, str) == 0);
}

JKC99_TEST(expr_postfix_index) {
    TEST_CTX_BUF("x[1337]");
    Expr *e = jkc99_parse_expr(ctx);
    JKC99T(e);
    JKC99T(e->kind == kExprPostfix);
    JKC99T(e->u.postfix.kind == kExprPostfixIndex);
    JKC99T(e->u.postfix.expr);
    JKC99T(e->u.postfix.expr->kind == kExprPrimary);
    JKC99T(e->u.postfix.expr->u.primary.kind == kExprPrimaryIdentifier);
    JKC99T(strcmp(e->u.postfix.expr->u.primary.u.identifier, "x") == 0);
    JKC99T(e->u.postfix.u.indexExpr);
    JKC99T(e->u.postfix.u.indexExpr->kind == kExprPrimary);
    JKC99T(e->u.postfix.u.indexExpr->u.primary.kind == kExprPrimaryConstant);
    JKC99T(e->u.postfix.u.indexExpr->u.primary.u.constant.kind == kConstantInteger);
    JKC99T(e->u.postfix.u.indexExpr->u.primary.u.constant.u.intVal == 1337);
}

JKC99_TEST(expr_postfix_call) {
    TEST_CTX_BUF("printf(\"Hello World %d\n\", 123)");
    Expr *e = jkc99_parse_expr(ctx);
    JKC99T(e);
    JKC99T(e->kind == kExprPostfix);
    JKC99T(e->u.postfix.kind == kExprPostfixCall);
    JKC99T(e->u.postfix.expr);
    JKC99T(e->u.postfix.expr->kind == kExprPrimary);
    JKC99T(e->u.postfix.expr->u.primary.kind == kExprPrimaryIdentifier);
    JKC99T(strcmp(e->u.postfix.expr->u.primary.u.identifier, "printf") == 0);
    JKC99T(e->u.postfix.u.callArgs);
    JKC99T(e->u.postfix.u.callArgs[0]->kind == kExprPrimary);
    JKC99T(e->u.postfix.u.callArgs[0]->u.primary.kind == kExprPrimaryStringLiteral);
    JKC99T(strcmp(e->u.postfix.u.callArgs[0]->u.primary.u.str.str, "Hello World %d\n") == 0);
    JKC99T(e->u.postfix.u.callArgs[1]->kind == kExprPrimary);
    JKC99T(e->u.postfix.u.callArgs[1]->u.primary.kind == kExprPrimaryConstant);
    JKC99T(e->u.postfix.u.callArgs[1]->u.primary.u.constant.kind == kConstantInteger);
    JKC99T(e->u.postfix.u.callArgs[1]->u.primary.u.constant.u.intVal == 123);
}
