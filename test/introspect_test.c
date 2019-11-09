#include <stdio.h>

typedef struct TestStruct {
    char        a;
    int         b;
    const int   *c;
} TestStruct;

TestStruct foo,bar;

typedef enum {
    Monday,
    Tuesday,
    Wednesday,
    Thursday = Tuesday + 58,
    Friday = Thursday + sizeof(int),
    Saturday,
    Sunday
} Weekday;

void print_type_info(IntrospectTypeInfo *info);

int main(int argc, char **argv) {
    struct TestLocalStruct {
        char *str;
        double d;
        int *(*func)(TestStruct, char**);
    } testMembers __attribute__((__unused__));

    printf("Introspection info available for %lu types and %lu symbols\n\n", 
            gIntrospectTypeInfoCount, gIntrospectSymbolCount);

    print_type_info(type_info(TestStruct));
    print_type_info(type_info(Weekday));
    print_type_info(type_info(testMembers));
#ifdef _WIN32
    print_type_info(type_info(int (const char *const, ...)));
#else
    print_type_info(type_info(int (const char * __restrict, ...)));
#endif
}

void print_type_info(IntrospectTypeInfo *info) {
    printf("%s\n", info->name);
    switch(info->kind) {
        case kIntrospectTypeStruct:
            {
                if(info->u.su.memberCount) {
                    printf("  Members:\n");
                    for(size_t i = 0; i < info->u.su.memberCount; ++i) {
                        IntrospectMember *m = info->u.su.members + i;
                        IntrospectTypeInfo *mInfo = gIntrospectTypeInfo + m->type;
                        printf("    name: %-14s type: %-36s offset: %zu\n", 
                                m->name, mInfo->name, m->offset);
                    }
                    if(info->userCount) {
                        printf("  Globals:\n");
                        for(size_t i = 0; i < info->userCount; i++) {
                            IntrospectSymbol *sym = 
                                gIntrospectSymbols + info->users[i];
                            if(sym->kind == kIntrospectSymbolGlobal) {
                                printf("    name: %-14s address: 0x%p\n", 
                                        sym->name, sym->u.global);
                            }
                        }
                    }
                }
            } break;
        case kIntrospectTypeEnum:
            {
                printf("  Enumerators:\n");
                for(size_t i = 0; i < info->u.e.enumeratorCount; ++i) {
                    printf("    name: %-14s value: %d\n",
                            info->u.e.names[i], info->u.e.values[i]);
                }
            } break;
        case kIntrospectTypeFunction:
            {
                printf("  Functions:\n");
                for(size_t i = 0; i < info->userCount; i++) {
                    IntrospectSymbol *sym = gIntrospectSymbols + info->users[i];
                    if(sym->kind == kIntrospectSymbolFunction) {
                        printf("    name: %-14s address: 0x%p\n", 
                                sym->name, (void*)sym->u.func.ptr);
                    }
                }
            } break;
        default: break;
    }
    printf("\n");
}
