#include "../jkc99.h"
#include "../dynamic_array.h"


static struct {
    const char      *funcPrefix;
    size_t          funcPrefixLen;
    TypeHandle      funcType;
    int             testCount;
    char            *run_tests;
} gTestModule = {0};

JKC99_HOOK(parse_test_function) {
    FunctionDefinition *func = *(FunctionDefinition**)ptr;

    if(JKC99_TYPE_HANDLE_EQ(func->type, gTestModule.funcType) &&
         strncmp(func->name, gTestModule.funcPrefix, gTestModule.funcPrefixLen) == 0) {
        da_printf(gTestModule.run_tests, "    errCount += %s(1);\n", func->name);
        gTestModule.testCount++;

        BlockItem item = {0};
        Expr **printfArgs = NULL;

        da_push(printfArgs, jkc99_expr_string(ctx, NULL, jkc99_str_intern(ctx, "    %-48s SUCCESS\n")));
        da_push(printfArgs, 
                jkc99_expr_additive(ctx, NULL, kExprAdditiveAdd, 
                    jkc99_expr_identifier(ctx, NULL, jkc99_str_intern(ctx, "__func__")),
                    jkc99_expr_int(ctx, NULL, NULL, (int)gTestModule.funcPrefixLen, kConstantDecimal, 0, "")));
        item.stmt = jkc99_stmt_expr(ctx, NULL, 0, NULL, 
                jkc99_expr_call(ctx, NULL, 
                    jkc99_expr_identifier(ctx, NULL, jkc99_str_intern(ctx, "printf")),
                    printfArgs));
        jkc99_stmt_compound_push(ctx, func->body, item);
        item.stmt = jkc99_stmt_return(ctx, NULL, jkc99_expr_int(ctx, NULL, jkc99_str_intern(ctx, "0"), 0, kConstantDecimal, 0, ""));
        jkc99_stmt_compound_push(ctx, func->body, item);
    }

    return false;
}

JKC99_HOOK(generate_run_tests) {
    if(gTestModule.testCount) {
        da_printf(gTestModule.run_tests, 
                "    printf(\"----------------------------------------------------------------------------\n\");"
                "        printf(\"(%%d/%d) \", (%d - errCount));"
                "    if(!errCount) {\n"
                "        printf(\"All tests were successful\n\");"
                "    } else {\n"
                "        printf(\"%%d Failure%%s!\n\n\", errCount, errCount > 1 ? \"s\" : \"\");"
                "    }\n\n"
                "    return errCount;\n"
                "}\n\n"
                , gTestModule.testCount
                , gTestModule.testCount
                );
    } else {
        da_printf(gTestModule.run_tests, 
                "    printf(\"No tests defined!\n\n\");\n"
                "    return errCount;\n"
                "}\n\n"
                );
    }
    ParseBuffer buffer = jkc99_buffer_new(ctx, "", da_count(gTestModule.run_tests), gTestModule.run_tests);
    jkc99_buffer_parse(ctx, buffer);

    return false;
}

OPTION_FUNC(prefix) {
    if(value) {
        gTestModule.funcPrefix = jkc99_str_intern(ctx, value);
        gTestModule.funcPrefixLen = strlen(gTestModule.funcPrefix);
    } else {
        fprintf(stderr, "Test: Expected prefix\n");
        return false;
    }
    return true;
}

static Option gOptions[] = {
    OPTION('p', prefix, "Specify test function prefix")
};


EXPORT JKC99_MODULE_GET_OPTIONS(jkc99_module_get_options) {
    *count = jkc99_array_count(gOptions);
    return gOptions;
}

EXPORT JKC99_MODULE_INIT(jkc99_module_init) {
    da_printf(gTestModule.run_tests,
            "static int jkc99_test_run(void) {\n"
            "    int errCount = 0;\n\n"
            "    printf(\"Executing unit tests...\n\");\n"
            "    printf(\"----------------------------------------------------------------------------\n\");\n"
            );

    if(!gTestModule.funcPrefix) {
        gTestModule.funcPrefix = jkc99_str_intern(ctx, "jkc99t_");
        gTestModule.funcPrefixLen = strlen(gTestModule.funcPrefix);
    }

    gTestModule.funcType = jkc99_type_func(ctx, jkc99_type_int(ctx), 1, (TypeHandle[]){ jkc99_type_int(ctx) }, false);


    jkc99_install_hook(ctx, kHookNewFunctionDefinition, parse_test_function, NULL);
    jkc99_install_hook(ctx, kHookParseComplete, generate_run_tests, NULL);

    return true;
}
