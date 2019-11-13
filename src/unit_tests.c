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

JKC99_TEST(expr_char_basic) {
    const char *input[] = {
        "\' \'", "\'!\'", "\'~\'", "\'#\'", "\'$\'", "\'%\'", "\'&\'", "\'(\'",
        "\')\'", "\'*\'", "\'+\'", "\',\'", "\'-\'", "\'.\'", "\'/\'", "\'0\'", 
        "\'1\'", "\'2\'", "\'3\'", "\'4\'", "\'5\'", "\'6\'", "\'7\'", "\'8\'", 
        "\'9\'", "\':\'", "\';\'", "\'<\'", "\'=\'", "\'>\'", "\'?\'", "\'@\'", 
        "\'[\'", "\']\'", "\'^\'", "\'_\'", "\'`\'", "\'{\'", "\'|\'", "\'}\'",
        "\'\"\'",

        "\'A\'", "\'B\'", "\'C\'", "\'D\'", "\'E\'", "\'F\'", "\'G\'", "\'H\'",  
        "\'I\'", "\'J\'", "\'K\'", "\'L\'", "\'M\'", "\'N\'", "\'O\'", "\'P\'",
        "\'Q\'", "\'R\'", "\'S\'", "\'T\'", "\'U\'", "\'V\'", "\'W\'", "\'X\'",  
        "\'Y\'", "\'Z\'", 

        "\'a\'", "\'b\'", "\'c\'", "\'d\'", "\'e\'", "\'f\'", "\'g\'", "\'h\'",  
        "\'i\'", "\'j\'", "\'k\'", "\'l\'", "\'m\'", "\'n\'", "\'o\'", "\'p\'",
        "\'q\'", "\'r\'", "\'s\'", "\'t\'", "\'u\'", "\'v\'", "\'w\'", "\'x\'",  
        "\'y\'", "\'z\'",
    };
    for(size_t i = 0; i < jkc99_array_count(input); ++i) {
        TEST_CTX_BUF(input[i]);
        Expr *e = jkc99_parse_expr(ctx);
        JKC99T(e);
        JKC99T(e->kind == kExprPrimary);
        JKC99T(e->u.primary.kind == kExprPrimaryConstant);
        JKC99T(e->u.primary.u.constant.kind == kConstantCharacter);
        JKC99T(e->u.primary.u.constant.u.charVal == (unsigned char)(input[i][1]));
        JKC99T(e->u.primary.u.constant.representation == kConstantRepresentationCharacter);
    }
}

JKC99_TEST(expr_char_simple_escape) {
    struct {
        const char      *str;
        unsigned char   val;
    } inputs[] = {
        { .str = "\'\\\'\'", .val = '\'' },
        { .str = "\'\\\"\'", .val = '\"' }, 
        { .str = "\'\\?\'",  .val = '\?' },
        { .str = "\'\\\\\'", .val = '\\' },
        { .str = "\'\\a\'",  .val = '\a' },
        { .str = "\'\\b\'",  .val = '\b' },
        { .str = "\'\\f\'",  .val = '\f' },
        { .str = "\'\\n\'",  .val = '\n' },
        { .str = "\'\\r\'",  .val = '\r' },
        { .str = "\'\\t\'",  .val = '\t' },
        { .str = "\'\\v\'",  .val = '\v' },
    };

    for(size_t i = 0; i < jkc99_array_count(inputs); ++i) {
        TEST_CTX_BUF(inputs[i].str);
        Expr *e = jkc99_parse_expr(ctx);
        JKC99T(e);
        JKC99T(e->kind == kExprPrimary);
        JKC99T(e->u.primary.kind == kExprPrimaryConstant);
        JKC99T(e->u.primary.u.constant.kind == kConstantCharacter);
        JKC99T(e->u.primary.u.constant.u.charVal == inputs[i].val);
        JKC99T(e->u.primary.u.constant.representation == kConstantRepresentationCharacter);
    }
}

JKC99_TEST(expr_char_octal) {
    const char *input[] = {
        "\'\\00\'",  "\'\\01\'",  "\'\\02\'",  "\'\\03\'",  "\'\\04\'",  "\'\\05\'",  "\'\\06\'",  "\'\\07\'",
        "\'\\10\'",  "\'\\11\'",  "\'\\12\'",  "\'\\13\'",  "\'\\14\'",  "\'\\15\'",  "\'\\16\'",  "\'\\17\'",
        "\'\\20\'",  "\'\\21\'",  "\'\\22\'",  "\'\\23\'",  "\'\\24\'",  "\'\\25\'",  "\'\\26\'",  "\'\\27\'",
        "\'\\30\'",  "\'\\31\'",  "\'\\32\'",  "\'\\33\'",  "\'\\34\'",  "\'\\35\'",  "\'\\36\'",  "\'\\37\'",
        "\'\\40\'",  "\'\\41\'",  "\'\\42\'",  "\'\\43\'",  "\'\\44\'",  "\'\\45\'",  "\'\\46\'",  "\'\\47\'",
        "\'\\50\'",  "\'\\51\'",  "\'\\52\'",  "\'\\53\'",  "\'\\54\'",  "\'\\55\'",  "\'\\56\'",  "\'\\57\'",
        "\'\\60\'",  "\'\\61\'",  "\'\\62\'",  "\'\\63\'",  "\'\\64\'",  "\'\\65\'",  "\'\\66\'",  "\'\\67\'",
        "\'\\70\'",  "\'\\71\'",  "\'\\72\'",  "\'\\73\'",  "\'\\74\'",  "\'\\75\'",  "\'\\76\'",  "\'\\77\'",
        "\'\\100\'", "\'\\101\'", "\'\\102\'", "\'\\103\'", "\'\\104\'", "\'\\105\'", "\'\\106\'", "\'\\107\'",
        "\'\\110\'", "\'\\111\'", "\'\\112\'", "\'\\113\'", "\'\\114\'", "\'\\115\'", "\'\\116\'", "\'\\117\'",
        "\'\\120\'", "\'\\121\'", "\'\\122\'", "\'\\123\'", "\'\\124\'", "\'\\125\'", "\'\\126\'", "\'\\127\'",
        "\'\\130\'", "\'\\131\'", "\'\\132\'", "\'\\133\'", "\'\\134\'", "\'\\135\'", "\'\\136\'", "\'\\137\'",
        "\'\\140\'", "\'\\141\'", "\'\\142\'", "\'\\143\'", "\'\\144\'", "\'\\145\'", "\'\\146\'", "\'\\147\'",
        "\'\\150\'", "\'\\151\'", "\'\\152\'", "\'\\153\'", "\'\\154\'", "\'\\155\'", "\'\\156\'", "\'\\157\'",
        "\'\\160\'", "\'\\161\'", "\'\\162\'", "\'\\163\'", "\'\\164\'", "\'\\165\'", "\'\\166\'", "\'\\167\'",
        "\'\\170\'", "\'\\171\'", "\'\\172\'", "\'\\173\'", "\'\\174\'", "\'\\175\'", "\'\\176\'", "\'\\177\'",
        "\'\\200\'", "\'\\201\'", "\'\\202\'", "\'\\203\'", "\'\\204\'", "\'\\205\'", "\'\\206\'", "\'\\207\'",
        "\'\\210\'", "\'\\211\'", "\'\\212\'", "\'\\213\'", "\'\\214\'", "\'\\215\'", "\'\\216\'", "\'\\217\'",
        "\'\\220\'", "\'\\221\'", "\'\\222\'", "\'\\223\'", "\'\\224\'", "\'\\225\'", "\'\\226\'", "\'\\227\'",
        "\'\\230\'", "\'\\231\'", "\'\\232\'", "\'\\233\'", "\'\\234\'", "\'\\235\'", "\'\\236\'", "\'\\237\'",
        "\'\\240\'", "\'\\241\'", "\'\\242\'", "\'\\243\'", "\'\\244\'", "\'\\245\'", "\'\\246\'", "\'\\247\'",
        "\'\\250\'", "\'\\251\'", "\'\\252\'", "\'\\253\'", "\'\\254\'", "\'\\255\'", "\'\\256\'", "\'\\257\'",
        "\'\\260\'", "\'\\261\'", "\'\\262\'", "\'\\263\'", "\'\\264\'", "\'\\265\'", "\'\\266\'", "\'\\267\'",
        "\'\\270\'", "\'\\271\'", "\'\\272\'", "\'\\273\'", "\'\\274\'", "\'\\275\'", "\'\\276\'", "\'\\277\'",
        "\'\\300\'", "\'\\301\'", "\'\\302\'", "\'\\303\'", "\'\\304\'", "\'\\305\'", "\'\\306\'", "\'\\307\'",
        "\'\\310\'", "\'\\311\'", "\'\\312\'", "\'\\313\'", "\'\\314\'", "\'\\315\'", "\'\\316\'", "\'\\317\'",
        "\'\\320\'", "\'\\321\'", "\'\\322\'", "\'\\323\'", "\'\\324\'", "\'\\325\'", "\'\\326\'", "\'\\327\'",
        "\'\\330\'", "\'\\331\'", "\'\\332\'", "\'\\333\'", "\'\\334\'", "\'\\335\'", "\'\\336\'", "\'\\337\'",
        "\'\\340\'", "\'\\341\'", "\'\\342\'", "\'\\343\'", "\'\\344\'", "\'\\345\'", "\'\\346\'", "\'\\347\'",
        "\'\\350\'", "\'\\351\'", "\'\\352\'", "\'\\353\'", "\'\\354\'", "\'\\355\'", "\'\\356\'", "\'\\357\'",
        "\'\\360\'", "\'\\361\'", "\'\\362\'", "\'\\363\'", "\'\\364\'", "\'\\365\'", "\'\\366\'", "\'\\367\'",
        "\'\\370\'", "\'\\371\'", "\'\\372\'", "\'\\373\'", "\'\\374\'", "\'\\375\'", "\'\\376\'", "\'\\377\'",
    };
    for(size_t i = 0; i < jkc99_array_count(input); ++i) {
        TEST_CTX_BUF(input[i]);
        Expr *e = jkc99_parse_expr(ctx);
        JKC99T(e);
        JKC99T(e->kind == kExprPrimary);
        JKC99T(e->u.primary.kind == kExprPrimaryConstant);
        JKC99T(e->u.primary.u.constant.kind == kConstantCharacter);
        JKC99T(e->u.primary.u.constant.u.charVal == (unsigned char)i);
        JKC99T(e->u.primary.u.constant.representation == kConstantRepresentationOctal);
    }
}

JKC99_TEST(expr_char_hex) {
    const char *input[] = {
        "\'\\x00\'", "\'\\x01\'", "\'\\x02\'", "\'\\x03\'", "\'\\x04\'", "\'\\x05\'", "\'\\x06\'", "\'\\x07\'",
        "\'\\x08\'", "\'\\x09\'", "\'\\x0a\'", "\'\\x0b\'", "\'\\x0c\'", "\'\\x0d\'", "\'\\x0e\'", "\'\\x0f\'",
        "\'\\x10\'", "\'\\x11\'", "\'\\x12\'", "\'\\x13\'", "\'\\x14\'", "\'\\x15\'", "\'\\x16\'", "\'\\x17\'",
        "\'\\x18\'", "\'\\x19\'", "\'\\x1a\'", "\'\\x1b\'", "\'\\x1c\'", "\'\\x1d\'", "\'\\x1e\'", "\'\\x1f\'",
        "\'\\x20\'", "\'\\x21\'", "\'\\x22\'", "\'\\x23\'", "\'\\x24\'", "\'\\x25\'", "\'\\x26\'", "\'\\x27\'",
        "\'\\x28\'", "\'\\x29\'", "\'\\x2a\'", "\'\\x2b\'", "\'\\x2c\'", "\'\\x2d\'", "\'\\x2e\'", "\'\\x2f\'",
        "\'\\x30\'", "\'\\x31\'", "\'\\x32\'", "\'\\x33\'", "\'\\x34\'", "\'\\x35\'", "\'\\x36\'", "\'\\x37\'",
        "\'\\x38\'", "\'\\x39\'", "\'\\x3a\'", "\'\\x3b\'", "\'\\x3c\'", "\'\\x3d\'", "\'\\x3e\'", "\'\\x3f\'",
        "\'\\x40\'", "\'\\x41\'", "\'\\x42\'", "\'\\x43\'", "\'\\x44\'", "\'\\x45\'", "\'\\x46\'", "\'\\x47\'",
        "\'\\x48\'", "\'\\x49\'", "\'\\x4a\'", "\'\\x4b\'", "\'\\x4c\'", "\'\\x4d\'", "\'\\x4e\'", "\'\\x4f\'",
        "\'\\x50\'", "\'\\x51\'", "\'\\x52\'", "\'\\x53\'", "\'\\x54\'", "\'\\x55\'", "\'\\x56\'", "\'\\x57\'",
        "\'\\x58\'", "\'\\x59\'", "\'\\x5a\'", "\'\\x5b\'", "\'\\x5c\'", "\'\\x5d\'", "\'\\x5e\'", "\'\\x5f\'",
        "\'\\x60\'", "\'\\x61\'", "\'\\x62\'", "\'\\x63\'", "\'\\x64\'", "\'\\x65\'", "\'\\x66\'", "\'\\x67\'",
        "\'\\x68\'", "\'\\x69\'", "\'\\x6a\'", "\'\\x6b\'", "\'\\x6c\'", "\'\\x6d\'", "\'\\x6e\'", "\'\\x6f\'",
        "\'\\x70\'", "\'\\x71\'", "\'\\x72\'", "\'\\x73\'", "\'\\x74\'", "\'\\x75\'", "\'\\x76\'", "\'\\x77\'",
        "\'\\x78\'", "\'\\x79\'", "\'\\x7a\'", "\'\\x7b\'", "\'\\x7c\'", "\'\\x7d\'", "\'\\x7e\'", "\'\\x7f\'",
        "\'\\x80\'", "\'\\x81\'", "\'\\x82\'", "\'\\x83\'", "\'\\x84\'", "\'\\x85\'", "\'\\x86\'", "\'\\x87\'",
        "\'\\x88\'", "\'\\x89\'", "\'\\x8a\'", "\'\\x8b\'", "\'\\x8c\'", "\'\\x8d\'", "\'\\x8e\'", "\'\\x8f\'",
        "\'\\x90\'", "\'\\x91\'", "\'\\x92\'", "\'\\x93\'", "\'\\x94\'", "\'\\x95\'", "\'\\x96\'", "\'\\x97\'",
        "\'\\x98\'", "\'\\x99\'", "\'\\x9a\'", "\'\\x9b\'", "\'\\x9c\'", "\'\\x9d\'", "\'\\x9e\'", "\'\\x9f\'",
        "\'\\xa0\'", "\'\\xa1\'", "\'\\xa2\'", "\'\\xa3\'", "\'\\xa4\'", "\'\\xa5\'", "\'\\xa6\'", "\'\\xa7\'",
        "\'\\xa8\'", "\'\\xa9\'", "\'\\xaa\'", "\'\\xab\'", "\'\\xac\'", "\'\\xad\'", "\'\\xae\'", "\'\\xaf\'",
        "\'\\xb0\'", "\'\\xb1\'", "\'\\xb2\'", "\'\\xb3\'", "\'\\xb4\'", "\'\\xb5\'", "\'\\xb6\'", "\'\\xb7\'",
        "\'\\xb8\'", "\'\\xb9\'", "\'\\xba\'", "\'\\xbb\'", "\'\\xbc\'", "\'\\xbd\'", "\'\\xbe\'", "\'\\xbf\'",
        "\'\\xc0\'", "\'\\xc1\'", "\'\\xc2\'", "\'\\xc3\'", "\'\\xc4\'", "\'\\xc5\'", "\'\\xc6\'", "\'\\xc7\'",
        "\'\\xc8\'", "\'\\xc9\'", "\'\\xca\'", "\'\\xcb\'", "\'\\xcc\'", "\'\\xcd\'", "\'\\xce\'", "\'\\xcf\'",
        "\'\\xd0\'", "\'\\xd1\'", "\'\\xd2\'", "\'\\xd3\'", "\'\\xd4\'", "\'\\xd5\'", "\'\\xd6\'", "\'\\xd7\'",
        "\'\\xd8\'", "\'\\xd9\'", "\'\\xda\'", "\'\\xdb\'", "\'\\xdc\'", "\'\\xdd\'", "\'\\xde\'", "\'\\xdf\'",
        "\'\\xe0\'", "\'\\xe1\'", "\'\\xe2\'", "\'\\xe3\'", "\'\\xe4\'", "\'\\xe5\'", "\'\\xe6\'", "\'\\xe7\'",
        "\'\\xe8\'", "\'\\xe9\'", "\'\\xea\'", "\'\\xeb\'", "\'\\xec\'", "\'\\xed\'", "\'\\xee\'", "\'\\xef\'",
        "\'\\xf0\'", "\'\\xf1\'", "\'\\xf2\'", "\'\\xf3\'", "\'\\xf4\'", "\'\\xf5\'", "\'\\xf6\'", "\'\\xf7\'",
        "\'\\xf8\'", "\'\\xf9\'", "\'\\xfa\'", "\'\\xfb\'", "\'\\xfc\'", "\'\\xfd\'", "\'\\xfe\'", "\'\\xff\'",
    };
    for(size_t i = 0; i < jkc99_array_count(input); ++i) {
        TEST_CTX_BUF(input[i]);
        Expr *e = jkc99_parse_expr(ctx);
        JKC99T(e);
        JKC99T(e->kind == kExprPrimary);
        JKC99T(e->u.primary.kind == kExprPrimaryConstant);
        JKC99T(e->u.primary.u.constant.kind == kConstantCharacter);
        JKC99T(e->u.primary.u.constant.u.charVal == (unsigned char)i);
        JKC99T(e->u.primary.u.constant.representation == kConstantRepresentationHex);
    }
}

JKC99_TEST(expr_wchar_basic) {
    const char *input[] = {
        "L\' \'", "L\'!\'", "L\'~\'", "L\'#\'", "L\'$\'", "L\'%\'", "L\'&\'", "L\'(\'",
        "L\')\'", "L\'*\'", "L\'+\'", "L\',\'", "L\'-\'", "L\'.\'", "L\'/\'", "L\'0\'", 
        "L\'1\'", "L\'2\'", "L\'3\'", "L\'4\'", "L\'5\'", "L\'6\'", "L\'7\'", "L\'8\'", 
        "L\'9\'", "L\':\'", "L\';\'", "L\'<\'", "L\'=\'", "L\'>\'", "L\'?\'", "L\'@\'", 
        "L\'[\'", "L\']\'", "L\'^\'", "L\'_\'", "L\'`\'", "L\'{\'", "L\'|\'", "L\'}\'",
        "L\'\"\'",

        "L\'A\'", "L\'B\'", "L\'C\'", "L\'D\'", "L\'E\'", "L\'F\'", "L\'G\'", "L\'H\'",  
        "L\'I\'", "L\'J\'", "L\'K\'", "L\'L\'", "L\'M\'", "L\'N\'", "L\'O\'", "L\'P\'",
        "L\'Q\'", "L\'R\'", "L\'S\'", "L\'T\'", "L\'U\'", "L\'V\'", "L\'W\'", "L\'X\'",  
        "L\'Y\'", "L\'Z\'", 

        "L\'a\'", "L\'b\'", "L\'c\'", "L\'d\'", "L\'e\'", "L\'f\'", "L\'g\'", "L\'h\'",  
        "L\'i\'", "L\'j\'", "L\'k\'", "L\'l\'", "L\'m\'", "L\'n\'", "L\'o\'", "L\'p\'",
        "L\'q\'", "L\'r\'", "L\'s\'", "L\'t\'", "L\'u\'", "L\'v\'", "L\'w\'", "L\'x\'",  
        "L\'y\'", "L\'z\'",
    };
    for(size_t i = 0; i < jkc99_array_count(input); ++i) {
        TEST_CTX_BUF(input[i]);
        Expr *e = jkc99_parse_expr(ctx);
        JKC99T(e);
        JKC99T(e->kind == kExprPrimary);
        JKC99T(e->u.primary.kind == kExprPrimaryConstant);
        JKC99T(e->u.primary.u.constant.kind == kConstantCharacter);
        JKC99T(e->u.primary.u.constant.u.charVal == (wint_t)(input[i][2]));
        JKC99T(e->u.primary.u.constant.representation == kConstantRepresentationWideCharacter);
    }
}

JKC99_TEST(expr_wchar_simple_escape) {
    struct {
        const char      *str;
        wint_t          val;
    } inputs[] = {
        { .str = "L\'\\\'\'", .val = L'\'' },
        { .str = "L\'\\\"\'", .val = L'\"' }, 
        { .str = "L\'\\?\'",  .val = L'\?' },
        { .str = "L\'\\\\\'", .val = L'\\' },
        { .str = "L\'\\a\'",  .val = L'\a' },
        { .str = "L\'\\b\'",  .val = L'\b' },
        { .str = "L\'\\f\'",  .val = L'\f' },
        { .str = "L\'\\n\'",  .val = L'\n' },
        { .str = "L\'\\r\'",  .val = L'\r' },
        { .str = "L\'\\t\'",  .val = L'\t' },
        { .str = "L\'\\v\'",  .val = L'\v' },
    };

    for(size_t i = 0; i < jkc99_array_count(inputs); ++i) {
        TEST_CTX_BUF(inputs[i].str);
        Expr *e = jkc99_parse_expr(ctx);
        JKC99T(e);
        JKC99T(e->kind == kExprPrimary);
        JKC99T(e->u.primary.kind == kExprPrimaryConstant);
        JKC99T(e->u.primary.u.constant.kind == kConstantCharacter);
        JKC99T(e->u.primary.u.constant.u.charVal == (inputs[i].val));
        JKC99T(e->u.primary.u.constant.representation == kConstantRepresentationWideCharacter);
    }
}

JKC99_TEST(expr_wchar_octal) {
    const char *input[] = {
        "L\'\\00\'",  "L\'\\01\'",  "L\'\\02\'",  "L\'\\03\'",  "L\'\\04\'",  "L\'\\05\'",  "L\'\\06\'",  "L\'\\07\'",
        "L\'\\10\'",  "L\'\\11\'",  "L\'\\12\'",  "L\'\\13\'",  "L\'\\14\'",  "L\'\\15\'",  "L\'\\16\'",  "L\'\\17\'",
        "L\'\\20\'",  "L\'\\21\'",  "L\'\\22\'",  "L\'\\23\'",  "L\'\\24\'",  "L\'\\25\'",  "L\'\\26\'",  "L\'\\27\'",
        "L\'\\30\'",  "L\'\\31\'",  "L\'\\32\'",  "L\'\\33\'",  "L\'\\34\'",  "L\'\\35\'",  "L\'\\36\'",  "L\'\\37\'",
        "L\'\\40\'",  "L\'\\41\'",  "L\'\\42\'",  "L\'\\43\'",  "L\'\\44\'",  "L\'\\45\'",  "L\'\\46\'",  "L\'\\47\'",
        "L\'\\50\'",  "L\'\\51\'",  "L\'\\52\'",  "L\'\\53\'",  "L\'\\54\'",  "L\'\\55\'",  "L\'\\56\'",  "L\'\\57\'",
        "L\'\\60\'",  "L\'\\61\'",  "L\'\\62\'",  "L\'\\63\'",  "L\'\\64\'",  "L\'\\65\'",  "L\'\\66\'",  "L\'\\67\'",
        "L\'\\70\'",  "L\'\\71\'",  "L\'\\72\'",  "L\'\\73\'",  "L\'\\74\'",  "L\'\\75\'",  "L\'\\76\'",  "L\'\\77\'",
        "L\'\\100\'", "L\'\\101\'", "L\'\\102\'", "L\'\\103\'", "L\'\\104\'", "L\'\\105\'", "L\'\\106\'", "L\'\\107\'",
        "L\'\\110\'", "L\'\\111\'", "L\'\\112\'", "L\'\\113\'", "L\'\\114\'", "L\'\\115\'", "L\'\\116\'", "L\'\\117\'",
        "L\'\\120\'", "L\'\\121\'", "L\'\\122\'", "L\'\\123\'", "L\'\\124\'", "L\'\\125\'", "L\'\\126\'", "L\'\\127\'",
        "L\'\\130\'", "L\'\\131\'", "L\'\\132\'", "L\'\\133\'", "L\'\\134\'", "L\'\\135\'", "L\'\\136\'", "L\'\\137\'",
        "L\'\\140\'", "L\'\\141\'", "L\'\\142\'", "L\'\\143\'", "L\'\\144\'", "L\'\\145\'", "L\'\\146\'", "L\'\\147\'",
        "L\'\\150\'", "L\'\\151\'", "L\'\\152\'", "L\'\\153\'", "L\'\\154\'", "L\'\\155\'", "L\'\\156\'", "L\'\\157\'",
        "L\'\\160\'", "L\'\\161\'", "L\'\\162\'", "L\'\\163\'", "L\'\\164\'", "L\'\\165\'", "L\'\\166\'", "L\'\\167\'",
        "L\'\\170\'", "L\'\\171\'", "L\'\\172\'", "L\'\\173\'", "L\'\\174\'", "L\'\\175\'", "L\'\\176\'", "L\'\\177\'",
        "L\'\\200\'", "L\'\\201\'", "L\'\\202\'", "L\'\\203\'", "L\'\\204\'", "L\'\\205\'", "L\'\\206\'", "L\'\\207\'",
        "L\'\\210\'", "L\'\\211\'", "L\'\\212\'", "L\'\\213\'", "L\'\\214\'", "L\'\\215\'", "L\'\\216\'", "L\'\\217\'",
        "L\'\\220\'", "L\'\\221\'", "L\'\\222\'", "L\'\\223\'", "L\'\\224\'", "L\'\\225\'", "L\'\\226\'", "L\'\\227\'",
        "L\'\\230\'", "L\'\\231\'", "L\'\\232\'", "L\'\\233\'", "L\'\\234\'", "L\'\\235\'", "L\'\\236\'", "L\'\\237\'",
        "L\'\\240\'", "L\'\\241\'", "L\'\\242\'", "L\'\\243\'", "L\'\\244\'", "L\'\\245\'", "L\'\\246\'", "L\'\\247\'",
        "L\'\\250\'", "L\'\\251\'", "L\'\\252\'", "L\'\\253\'", "L\'\\254\'", "L\'\\255\'", "L\'\\256\'", "L\'\\257\'",
        "L\'\\260\'", "L\'\\261\'", "L\'\\262\'", "L\'\\263\'", "L\'\\264\'", "L\'\\265\'", "L\'\\266\'", "L\'\\267\'",
        "L\'\\270\'", "L\'\\271\'", "L\'\\272\'", "L\'\\273\'", "L\'\\274\'", "L\'\\275\'", "L\'\\276\'", "L\'\\277\'",
        "L\'\\300\'", "L\'\\301\'", "L\'\\302\'", "L\'\\303\'", "L\'\\304\'", "L\'\\305\'", "L\'\\306\'", "L\'\\307\'",
        "L\'\\310\'", "L\'\\311\'", "L\'\\312\'", "L\'\\313\'", "L\'\\314\'", "L\'\\315\'", "L\'\\316\'", "L\'\\317\'",
        "L\'\\320\'", "L\'\\321\'", "L\'\\322\'", "L\'\\323\'", "L\'\\324\'", "L\'\\325\'", "L\'\\326\'", "L\'\\327\'",
        "L\'\\330\'", "L\'\\331\'", "L\'\\332\'", "L\'\\333\'", "L\'\\334\'", "L\'\\335\'", "L\'\\336\'", "L\'\\337\'",
        "L\'\\340\'", "L\'\\341\'", "L\'\\342\'", "L\'\\343\'", "L\'\\344\'", "L\'\\345\'", "L\'\\346\'", "L\'\\347\'",
        "L\'\\350\'", "L\'\\351\'", "L\'\\352\'", "L\'\\353\'", "L\'\\354\'", "L\'\\355\'", "L\'\\356\'", "L\'\\357\'",
        "L\'\\360\'", "L\'\\361\'", "L\'\\362\'", "L\'\\363\'", "L\'\\364\'", "L\'\\365\'", "L\'\\366\'", "L\'\\367\'",
        "L\'\\370\'", "L\'\\371\'", "L\'\\372\'", "L\'\\373\'", "L\'\\374\'", "L\'\\375\'", "L\'\\376\'", "L\'\\377\'",
    };
    for(size_t i = 0; i < jkc99_array_count(input); ++i) {
        TEST_CTX_BUF(input[i]);
        Expr *e = jkc99_parse_expr(ctx);
        JKC99T(e);
        JKC99T(e->kind == kExprPrimary);
        JKC99T(e->u.primary.kind == kExprPrimaryConstant);
        JKC99T(e->u.primary.u.constant.kind == kConstantCharacter);
        JKC99T(e->u.primary.u.constant.u.charVal == (wint_t)i);
        JKC99T(e->u.primary.u.constant.representation == kConstantRepresentationWideOctal);
    }
}

JKC99_TEST(expr_wchar_hex) {
    const char *input[] = {
        "L\'\\x00\'", "L\'\\x01\'", "L\'\\x02\'", "L\'\\x03\'", "L\'\\x04\'", "L\'\\x05\'", "L\'\\x06\'", "L\'\\x07\'",
        "L\'\\x08\'", "L\'\\x09\'", "L\'\\x0a\'", "L\'\\x0b\'", "L\'\\x0c\'", "L\'\\x0d\'", "L\'\\x0e\'", "L\'\\x0f\'",
        "L\'\\x10\'", "L\'\\x11\'", "L\'\\x12\'", "L\'\\x13\'", "L\'\\x14\'", "L\'\\x15\'", "L\'\\x16\'", "L\'\\x17\'",
        "L\'\\x18\'", "L\'\\x19\'", "L\'\\x1a\'", "L\'\\x1b\'", "L\'\\x1c\'", "L\'\\x1d\'", "L\'\\x1e\'", "L\'\\x1f\'",
        "L\'\\x20\'", "L\'\\x21\'", "L\'\\x22\'", "L\'\\x23\'", "L\'\\x24\'", "L\'\\x25\'", "L\'\\x26\'", "L\'\\x27\'",
        "L\'\\x28\'", "L\'\\x29\'", "L\'\\x2a\'", "L\'\\x2b\'", "L\'\\x2c\'", "L\'\\x2d\'", "L\'\\x2e\'", "L\'\\x2f\'",
        "L\'\\x30\'", "L\'\\x31\'", "L\'\\x32\'", "L\'\\x33\'", "L\'\\x34\'", "L\'\\x35\'", "L\'\\x36\'", "L\'\\x37\'",
        "L\'\\x38\'", "L\'\\x39\'", "L\'\\x3a\'", "L\'\\x3b\'", "L\'\\x3c\'", "L\'\\x3d\'", "L\'\\x3e\'", "L\'\\x3f\'",
        "L\'\\x40\'", "L\'\\x41\'", "L\'\\x42\'", "L\'\\x43\'", "L\'\\x44\'", "L\'\\x45\'", "L\'\\x46\'", "L\'\\x47\'",
        "L\'\\x48\'", "L\'\\x49\'", "L\'\\x4a\'", "L\'\\x4b\'", "L\'\\x4c\'", "L\'\\x4d\'", "L\'\\x4e\'", "L\'\\x4f\'",
        "L\'\\x50\'", "L\'\\x51\'", "L\'\\x52\'", "L\'\\x53\'", "L\'\\x54\'", "L\'\\x55\'", "L\'\\x56\'", "L\'\\x57\'",
        "L\'\\x58\'", "L\'\\x59\'", "L\'\\x5a\'", "L\'\\x5b\'", "L\'\\x5c\'", "L\'\\x5d\'", "L\'\\x5e\'", "L\'\\x5f\'",
        "L\'\\x60\'", "L\'\\x61\'", "L\'\\x62\'", "L\'\\x63\'", "L\'\\x64\'", "L\'\\x65\'", "L\'\\x66\'", "L\'\\x67\'",
        "L\'\\x68\'", "L\'\\x69\'", "L\'\\x6a\'", "L\'\\x6b\'", "L\'\\x6c\'", "L\'\\x6d\'", "L\'\\x6e\'", "L\'\\x6f\'",
        "L\'\\x70\'", "L\'\\x71\'", "L\'\\x72\'", "L\'\\x73\'", "L\'\\x74\'", "L\'\\x75\'", "L\'\\x76\'", "L\'\\x77\'",
        "L\'\\x78\'", "L\'\\x79\'", "L\'\\x7a\'", "L\'\\x7b\'", "L\'\\x7c\'", "L\'\\x7d\'", "L\'\\x7e\'", "L\'\\x7f\'",
        "L\'\\x80\'", "L\'\\x81\'", "L\'\\x82\'", "L\'\\x83\'", "L\'\\x84\'", "L\'\\x85\'", "L\'\\x86\'", "L\'\\x87\'",
        "L\'\\x88\'", "L\'\\x89\'", "L\'\\x8a\'", "L\'\\x8b\'", "L\'\\x8c\'", "L\'\\x8d\'", "L\'\\x8e\'", "L\'\\x8f\'",
        "L\'\\x90\'", "L\'\\x91\'", "L\'\\x92\'", "L\'\\x93\'", "L\'\\x94\'", "L\'\\x95\'", "L\'\\x96\'", "L\'\\x97\'",
        "L\'\\x98\'", "L\'\\x99\'", "L\'\\x9a\'", "L\'\\x9b\'", "L\'\\x9c\'", "L\'\\x9d\'", "L\'\\x9e\'", "L\'\\x9f\'",
        "L\'\\xa0\'", "L\'\\xa1\'", "L\'\\xa2\'", "L\'\\xa3\'", "L\'\\xa4\'", "L\'\\xa5\'", "L\'\\xa6\'", "L\'\\xa7\'",
        "L\'\\xa8\'", "L\'\\xa9\'", "L\'\\xaa\'", "L\'\\xab\'", "L\'\\xac\'", "L\'\\xad\'", "L\'\\xae\'", "L\'\\xaf\'",
        "L\'\\xb0\'", "L\'\\xb1\'", "L\'\\xb2\'", "L\'\\xb3\'", "L\'\\xb4\'", "L\'\\xb5\'", "L\'\\xb6\'", "L\'\\xb7\'",
        "L\'\\xb8\'", "L\'\\xb9\'", "L\'\\xba\'", "L\'\\xbb\'", "L\'\\xbc\'", "L\'\\xbd\'", "L\'\\xbe\'", "L\'\\xbf\'",
        "L\'\\xc0\'", "L\'\\xc1\'", "L\'\\xc2\'", "L\'\\xc3\'", "L\'\\xc4\'", "L\'\\xc5\'", "L\'\\xc6\'", "L\'\\xc7\'",
        "L\'\\xc8\'", "L\'\\xc9\'", "L\'\\xca\'", "L\'\\xcb\'", "L\'\\xcc\'", "L\'\\xcd\'", "L\'\\xce\'", "L\'\\xcf\'",
        "L\'\\xd0\'", "L\'\\xd1\'", "L\'\\xd2\'", "L\'\\xd3\'", "L\'\\xd4\'", "L\'\\xd5\'", "L\'\\xd6\'", "L\'\\xd7\'",
        "L\'\\xd8\'", "L\'\\xd9\'", "L\'\\xda\'", "L\'\\xdb\'", "L\'\\xdc\'", "L\'\\xdd\'", "L\'\\xde\'", "L\'\\xdf\'",
        "L\'\\xe0\'", "L\'\\xe1\'", "L\'\\xe2\'", "L\'\\xe3\'", "L\'\\xe4\'", "L\'\\xe5\'", "L\'\\xe6\'", "L\'\\xe7\'",
        "L\'\\xe8\'", "L\'\\xe9\'", "L\'\\xea\'", "L\'\\xeb\'", "L\'\\xec\'", "L\'\\xed\'", "L\'\\xee\'", "L\'\\xef\'",
        "L\'\\xf0\'", "L\'\\xf1\'", "L\'\\xf2\'", "L\'\\xf3\'", "L\'\\xf4\'", "L\'\\xf5\'", "L\'\\xf6\'", "L\'\\xf7\'",
        "L\'\\xf8\'", "L\'\\xf9\'", "L\'\\xfa\'", "L\'\\xfb\'", "L\'\\xfc\'", "L\'\\xfd\'", "L\'\\xfe\'", "L\'\\xff\'",
    };
    for(size_t i = 0; i < jkc99_array_count(input); ++i) {
        TEST_CTX_BUF(input[i]);
        Expr *e = jkc99_parse_expr(ctx);
        JKC99T(e);
        JKC99T(e->kind == kExprPrimary);
        JKC99T(e->u.primary.kind == kExprPrimaryConstant);
        JKC99T(e->u.primary.u.constant.kind == kConstantCharacter);
        JKC99T(e->u.primary.u.constant.u.wcharVal == (wint_t)i);
        JKC99T(e->u.primary.u.constant.representation == kConstantRepresentationWideHex);
    }
}

JKC99_TEST(expr_int_basic) {
    TEST_CTX_BUF("12345");
    Expr *e = jkc99_parse_expr(ctx);
    JKC99T(e);
    JKC99T(e->kind == kExprPrimary);
    JKC99T(e->u.primary.kind == kExprPrimaryConstant);
    JKC99T(e->u.primary.u.constant.kind == kConstantInteger);
    JKC99T(e->u.primary.u.constant.representation == kConstantRepresentationDecimal);
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
    JKC99T(e->u.primary.u.constant.representation == kConstantRepresentationDecimal);
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

JKC99_TEST(expr_string_concatenation) {
    TEST_CTX_BUF("\"Hello\" \"World!\"");
    Expr *e = jkc99_parse_expr(ctx);
    JKC99T(e);
    JKC99T(e->kind == kExprPrimary);
    JKC99T(e->u.primary.kind == kExprPrimaryStringLiteral);
    JKC99T(strcmp(e->u.primary.u.str.str, "HelloWorld!") == 0);
}

JKC99_TEST(expr_wstring) {
    wchar_t *wstr = L"Test123\a\b\v\f\t\n\r";
    size_t wstrLen = wcslen(wstr);
    size_t len = wcstombs(NULL, wstr, wstrLen) + 4;
    char *str = malloc(len * sizeof(char));
    //wcstombs(str, wstr, len);
    snprintf(str, len, "L\"%ls\"", wstr);
    TEST_CTX_BUF(str);
    Expr *e = jkc99_parse_expr(ctx);
    JKC99T(e);
    JKC99T(e->kind == kExprPrimary);
    JKC99T(e->u.primary.kind == kExprPrimaryStringLiteral);
    JKC99T(wcscmp(e->u.primary.u.str.wstr, wstr) == 0);
}

JKC99_TEST(expr_wstring_concatenation) {
    TEST_CTX_BUF("L\"Hello\" L\"World!\"");
    Expr *e = jkc99_parse_expr(ctx);
    JKC99T(e);
    JKC99T(e->kind == kExprPrimary);
    JKC99T(e->u.primary.kind == kExprPrimaryStringLiteral);
    JKC99T(wcscmp(e->u.primary.u.str.wstr, L"HelloWorld!") == 0);
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
