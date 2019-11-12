enum {
    bLineMarkerNewFile          = 1 << 1,
    bLineMarkerReturnToFile     = 1 << 2,
    bLineMarkerSystemHeader     = 1 << 3,
    bLineMarkerExternC          = 1 << 4
};

typedef struct {
    const char *file;
    unsigned int line;
    const char *lineBegin;
    int lineMarkerFlags;

    char *input_stream;
    char *eof;
    char *parse_point;
    char *string_storage;
    int string_storage_len;

    char *where_firstchar;
    char *where_lastchar;

    long token;
    double real_number;
    long int_number;
    char *string;
    int string_len;

    _Bool initialized;
} stb_lexer;

typedef struct {
    int line_number;
    int line_offset;
} stb_lex_location;

extern void stb_c_lexer_init(stb_lexer *lexer, const char *input_stream, const char *input_stream_end, char *string_store, int store_length);
extern int stb_c_lexer_get_token(stb_lexer *lexer);
extern void stb_c_lexer_get_location(const stb_lexer *lexer, const char *where, stb_lex_location *loc);

enum {
    CLEX_eof = 256,
    CLEX_parse_error,

    CLEX_intlit,
    CLEX_floatlit,
    CLEX_id,
    CLEX_dqstring,
    CLEX_charlit,
    CLEX_eq,
    CLEX_noteq,
    CLEX_lesseq,
    CLEX_greatereq,
    CLEX_andand,
    CLEX_oror,
    CLEX_shl,
    CLEX_shr,
    CLEX_plusplus,
    CLEX_minusminus,
    CLEX_pluseq,
    CLEX_minuseq,
    CLEX_muleq,
    CLEX_diveq,
    CLEX_modeq,
    CLEX_andeq,
    CLEX_oreq,
    CLEX_xoreq,
    CLEX_arrow,
    CLEX_shleq,
    CLEX_shreq,

    CLEX_first_unused_token
};

void stb_c_lexer_init(stb_lexer *lexer, const char *input_stream, const char *input_stream_end, char *string_store, int store_length) {
    lexer->input_stream = (char *) input_stream;
    lexer->eof = (char *) input_stream_end;
    lexer->parse_point = (char *) input_stream;
    lexer->string_storage = string_store;
    lexer->string_storage_len = store_length;
}

void stb_c_lexer_get_location(const stb_lexer *lexer, const char *where, stb_lex_location *loc) {
    char *p = lexer->input_stream;
    int line_number = 1;
    int char_offset = 0;
    while (*p && p < where) {
        if (*p == '\n' || *p == '\r') {
            p += (p[0]+p[1] == '\r'+'\n' ? 2 : 1);
            line_number += 1;
            char_offset = 0;
        } else {
            ++p;
            ++char_offset;
        }
    }
    loc->line_number = line_number;
    loc->line_offset = char_offset;
}

static int stb__clex_token(stb_lexer *lexer, int token, char *start, char *end) {
    lexer->token = token;
    lexer->where_firstchar = start;
    lexer->where_lastchar = end;
    lexer->parse_point = end+1;
    return 1;
}

static int stb__clex_eof(stb_lexer *lexer) {
    lexer->token = CLEX_eof;
    return 0;
}

static int stb__clex_iswhite(int x) {
    return x == ' ' || x == '\t' || x == '\r' || x == '\n' || x == '\f';
}

static const char *stb__strchr(const char *str, int ch) {
    for (; *str; ++str) {
        if (*str == ch) {
            return str;
        }
    }
    return 0;
}

static int stb__clex_parse_suffixes(stb_lexer *lexer, long tokenid, char *start, char *cur, const char *suffixes) {
    lexer->string = lexer->string_storage;
    lexer->string_len = 0;

    while ((*cur >= 'a' && *cur <= 'z') || (*cur >= 'A' && *cur <= 'Z')) {
        if (stb__strchr(suffixes, *cur) == 0) {
            return stb__clex_token(lexer, CLEX_parse_error, start, cur);
        }
        if (lexer->string_len+1 >= lexer->string_storage_len) {
            return stb__clex_token(lexer, CLEX_parse_error, start, cur);
        }
        lexer->string[lexer->string_len++] = *cur++;
    }

    return stb__clex_token(lexer, tokenid, start, cur-1);
}

static int stb__clex_parse_char(char *p, char **q) {
    if (*p == '\\') {
        *q = p+2;
        switch(p[1]) {
            case '\\': return '\\';
            case '\'': return '\'';
            case '"': return '"';
            case 't': return '\t';
            case 'f': return '\f';
            case 'n': return '\n';
            case 'r': return '\r';
            case 'v': return '\v';
            case 'a': return '\a';
            case 'b': return '\b';
            case '?': return '\?';
            case '0': return '\0';
            case 'x': case 'X': return -1;
            case 'u': return -1;
        }
    }
    *q = p+1;
    return (unsigned char) *p;
}

#if 0
static int stb__clex_parse_wchar(char *p, char **q) {
    if (*p == '\\') {
        *q = p+2;
        switch(p[1]) {
            case '\\': return '\\';
            case '\'': return '\'';
            case '"': return '"';
            case 't': return '\t';
            case 'f': return '\f';
            case 'n': return '\n';
            case 'r': return '\r';
            case 'v': return '\v';
            case 'a': return '\a';
            case 'b': return '\b';
            case '?': return '\?';
            case '0': return '\0';
            case 'x': case 'X': return -1;
            case 'u': return -1;
        }
    }
    *q = p+1;
    return (unsigned char) *p;
}
#endif

static int stb__clex_skip_whitespace_and_comments(stb_lexer *lexer, char **p) {
    for (;;) {
        while (*p != lexer->eof && stb__clex_iswhite(**p)) {
            if (**p == '\n' || **p == '\r') {
                *p += ((*p)[0]+(*p)[1] == '\r'+'\n' ? 2 : 1);
                lexer->line++;
                lexer->lineBegin = *p;
            } else {
                ++*p;
            }
        }

        if (*p != lexer->eof && (*p)[0] == '/' && (*p)[1] == '/') { 
            while (*p != lexer->eof && **p != '\r' && **p != '\n') {
                ++*p;
            }
            continue; 
        }

        if (*p != lexer->eof && (*p)[0] == '/' && (*p)[1] == '*') { 
            char *start = *p;
            *p += 2;
            while (*p != lexer->eof && ((*p)[0] != '*' || (*p)[1] != '/')) {
                if ((*p)[0] == '\n' || (*p)[0] == '\r') {
                    *p += ((*p)[0]+(*p)[1] == '\r'+'\n' ? 2 : 1);
                    lexer->line++;
                    lexer->lineBegin = *p;
                } else {
                    ++*p;
                }
            }
            if (*p == lexer->eof) {
                return stb__clex_token(lexer, CLEX_parse_error, start, (*p)-1); 
            }
            *p += 2; 
            continue; 
        }
        break;
    }

    return 0;
}

static int stb__clex_parse_string(stb_lexer *lexer, char *p, int type) {
    char *start = p;
    char delim = *p++;
    char *out = lexer->string_storage;
    char *outend = lexer->string_storage + lexer->string_storage_len;
    while(1) {
        while (*p != delim) {
            int n;
            if (*p == '\\') {
                char *q;
                n = stb__clex_parse_char(p, &q);
                if (n < 0) {
                    return stb__clex_token(lexer, CLEX_parse_error, start, q);
                }
                p = q;
            } else {

                n = (unsigned char) *p++;
            }
            if (out+1 > outend) {
                return stb__clex_token(lexer, CLEX_parse_error, start, p);
            }

            *out++ = (char) n;
        }
        {
            char *pos = p+1;
            unsigned int line = lexer->line;
            const char *lineBegin = lexer->lineBegin;

            if(stb__clex_skip_whitespace_and_comments(lexer, &pos)) {
                break;
            } else {
                if(*pos != delim) {
                    lexer->line = line;
                    lexer->lineBegin = lineBegin;
                    break;
                } else {
                    p = pos;
                    p++;
                }
            }
        }
    }
    *out = 0;
    lexer->string = lexer->string_storage;
    lexer->string_len = (int) (out - lexer->string_storage);
    return stb__clex_token(lexer, type, start, p);
}

int stb_c_lexer_get_token(stb_lexer *lexer) {
    char *p = lexer->parse_point;
    int skipError;

    skipError = stb__clex_skip_whitespace_and_comments(lexer, &p);
    if(skipError) {
        return skipError;
    }

    if (p == lexer->eof) {
        return stb__clex_eof(lexer);
    }

    switch (*p) {
#if 0
        case 'L':
            {
                char *start = p;
                if(p[1] == '\'' && (p+2) < lexer->eof) {
                    jkc99_assert(sizeof(lexer->int_number) >= sizeof(wchar_t));
                    p += 2;
                    if (*p == '\\') {
                        *q = p+2;
                        switch(p[1]) {
                            case '\\': return '\\';
                            case '\'': return '\'';
                            case '"': return '"';
                            case 't': return '\t';
                            case 'f': return '\f';
                            case 'n': return '\n';
                            case 'r': return '\r';
                            case 'v': return '\v';
                            case 'a': return '\a';
                            case 'b': return '\b';
                            case '?': return '\?';
                            case '0': return '\0';
                            case 'x': case 'X': return -1;
                            case 'u': return -1;
                        }
                    }
                    *q = p+1;
                    return (unsigned char) *p;
                    if (lexer->int_number < 0)
                        return stb__clex_token(lexer, CLEX_parse_error, start,start);
                    if (p == lexer->eof || *p != '\'')
                        return stb__clex_token(lexer, CLEX_parse_error, start,p);
                    return stb__clex_token(lexer, CLEX_charlit, start, p);
                } else if(p[1] == '\"') {
                }
            } /* Fall through */
#endif

        default:
            if ( (*p >= 'a' && *p <= 'z')
                    || (*p >= 'A' && *p <= 'Z')
                    || *p == '_' || (unsigned char) *p >= 128) {
                int n = 0;
                lexer->string = lexer->string_storage;
                lexer->string_len = n;
                do {
                    if (n+1 >= lexer->string_storage_len) {
                        return stb__clex_token(lexer, CLEX_parse_error, p, p+n);
                    }
                    lexer->string[n] = p[n];
                    ++n;
                } while (
                        (p[n] >= 'a' && p[n] <= 'z')
                        || (p[n] >= 'A' && p[n] <= 'Z')
                        || (p[n] >= '0' && p[n] <= '9')
                        || p[n] == '_' || (unsigned char) p[n] >= 128);
                lexer->string[n] = 0;
                return stb__clex_token(lexer, CLEX_id, p, p+n-1);
            }

single_char:
            return stb__clex_token(lexer, *p, p, p);

        case '+':
            if (p+1 != lexer->eof) {
                if (p[1] == '+') return stb__clex_token(lexer, CLEX_plusplus, p,p+1);
                if (p[1] == '=') return stb__clex_token(lexer, CLEX_pluseq , p,p+1);
            }
            goto single_char;
        case '-':
            if (p+1 != lexer->eof) {
                if (p[1] == '-') return stb__clex_token(lexer, CLEX_minusminus, p,p+1);
                if (p[1] == '=') return stb__clex_token(lexer, CLEX_minuseq , p,p+1);
                if (p[1] == '>') return stb__clex_token(lexer, CLEX_arrow , p,p+1);
            }
            goto single_char;
        case '&':
            if (p+1 != lexer->eof) {
                if (p[1] == '&') return stb__clex_token(lexer, CLEX_andand, p,p+1);
                if (p[1] == '=') return stb__clex_token(lexer, CLEX_andeq , p,p+1);
            }
            goto single_char;
        case '|':
            if (p+1 != lexer->eof) {
                if (p[1] == '|') return stb__clex_token(lexer, CLEX_oror, p,p+1);
                if (p[1] == '=') return stb__clex_token(lexer, CLEX_oreq, p,p+1);
            }
            goto single_char;
        case '=':
            if (p+1 != lexer->eof) {
                if (p[1] == '=') return stb__clex_token(lexer, CLEX_eq, p,p+1);

            }
            goto single_char;
        case '!':
            if (p+1 != lexer->eof && p[1] == '=') 
                return stb__clex_token(lexer, CLEX_noteq, p,p+1);
            goto single_char;
        case '^':
            if (p+1 != lexer->eof && p[1] == '=') 
                return stb__clex_token(lexer, CLEX_xoreq, p,p+1);
            goto single_char;
        case '%':
            if (p+1 != lexer->eof && p[1] == '=') 
                return stb__clex_token(lexer, CLEX_modeq, p,p+1);
            goto single_char;
        case '*':
            if (p+1 != lexer->eof && p[1] == '=') 
                return stb__clex_token(lexer, CLEX_muleq, p,p+1);
            goto single_char;
        case '/':
            if (p+1 != lexer->eof && p[1] == '=')
                return stb__clex_token(lexer, CLEX_diveq, p,p+1);
            goto single_char;
        case '<':
            if (p+1 != lexer->eof) {
                if (p[1] == '=') 
                    return stb__clex_token(lexer, CLEX_lesseq, p,p+1);
                if (p[1] == '<') { 
                    if (p+2 != lexer->eof && p[2] == '=') 
                        return stb__clex_token(lexer, CLEX_shleq, p,p+2); 
                    return stb__clex_token(lexer, CLEX_shl, p,p+1); 
                }
            }
            goto single_char;
        case '>':
            if (p+1 != lexer->eof) {
                if (p[1] == '=') 
                    return stb__clex_token(lexer, CLEX_greatereq, p,p+1);
                if (p[1] == '>') {
                    if (p+2 != lexer->eof && p[2] == '=') 
                        return stb__clex_token(lexer, CLEX_shreq, p,p+2);
                    return stb__clex_token(lexer, CLEX_shr, p,p+1);
                }
            }
            goto single_char;

        case '"':
            return stb__clex_parse_string(lexer, p, CLEX_dqstring);
            goto single_char;
        case '\'':
            { 
                char *start = p;
                lexer->int_number = stb__clex_parse_char(p+1, &p);
                if (lexer->int_number < 0)
                    return stb__clex_token(lexer, CLEX_parse_error, start,start);
                if (p == lexer->eof || *p != '\'')
                    return stb__clex_token(lexer, CLEX_parse_error, start,p);
                return stb__clex_token(lexer, CLEX_charlit, start, p);
            }
            goto single_char;

        case '0':
            if (p+1 != lexer->eof) {
                if (p[1] == 'x' || p[1] == 'X') {
                    char *q;
                    for (q=p+2; q != lexer->eof && ((*q >= '0' && *q <= '9') || (*q >= 'a' && *q <= 'f') || (*q >= 'A' && *q <= 'F')); ++q);
                    if (q != lexer->eof) {
                        if (*q == '.' || *q == 'p' || *q == 'P') {
                            lexer->real_number = strtod((char *) p, (char**) &q);
                            if (p == q)
                                return stb__clex_token(lexer, CLEX_parse_error, p,q);
                            return stb__clex_parse_suffixes(lexer, CLEX_floatlit, p,q, "flFL");
                        }
                    }

                    lexer->int_number = strtol((char *) p, (char **) &q, 16);
                    if (q == p+2)
                        return stb__clex_token(lexer, CLEX_parse_error, p-2,p-1);
                    return stb__clex_parse_suffixes(lexer, CLEX_intlit, p,q, "uUlL");

                }
            }
            /* Fall through */

        case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9':
            {
                char *q = p;
                while (q != lexer->eof && (*q >= '0' && *q <= '9'))
                    ++q;
                if (q != lexer->eof) {
                    if (*q == '.' || *q == 'e' || *q == 'E') {
                        lexer->real_number = strtod((char *) p, (char**) &q);
                        return stb__clex_parse_suffixes(lexer, CLEX_floatlit, p,q, "flFL");
                    }
                }
            }

            if (p[0] == '0') {
                char *q = p;
                lexer->int_number = strtol((char *) p, (char **) &q, 8);
                return stb__clex_parse_suffixes(lexer, CLEX_intlit, p,q, "uUlL");
            }

            {
                char *q = p;
                lexer->int_number = strtol((char *) p, (char **) &q, 10);
                return stb__clex_parse_suffixes(lexer, CLEX_intlit, p,q, "uUlL");
            }

            goto single_char;
    }
}
