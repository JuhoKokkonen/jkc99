#include "../jkc99.h"
#include "../dynamic_array.h"
#include "print.h"

JKC99_HOOK(print_external_declarations) {
    ExternalDeclaration *extdecl = jkc99_ext_decl_first(ctx);
    while(extdecl) {
        print_external_declaration(extdecl);
        extdecl = jkc99_ext_decl_next(ctx, extdecl, 1);
    }

    return false;
}

EXPORT JKC99_MODULE_INIT(jkc99_module_init) {
    jkc99_install_hook(ctx, kHookParseComplete, print_external_declarations, NULL);
    return true;
}
