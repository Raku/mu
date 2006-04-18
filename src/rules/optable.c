#include <stdlib.h>
#include <optable.h>

ParseMode sctable (Str in) {
    if (strncmp(in, "term:", 5)          ) { return PGE_SCTABLE_TERM; }
    if (strncmp(in, "postfix:", 8)       ) { return PGE_SCTABLE_POSTFIX; }
    if (strncmp(in, "close:", 6)         ) { return PGE_SCTABLE_CLOSE; }
    if (strncmp(in, "prefix:", 7)        ) { return PGE_SCTABLE_PREFIX; }
    if (strncmp(in, "prelist:", 8)       ) { return PGE_SCTABLE_PRELIST; }
    if (strncmp(in, "infix:", 6)         ) { return PGE_SCTABLE_INFIX; }
    if (strncmp(in, "ternary:", 8)       ) { return PGE_SCTABLE_TERNARY; }
    if (strncmp(in, "postcircumfix:", 14)) { return PGE_SCTABLE_POSTCIRCUMFIX; }
    if (strncmp(in, "circumfix:", 10)    ) { return PGE_SCTABLE_CIRCUMFIX; }
}

OpParser new_parser () {
    OpParser p = {NULL, NULL, NULL, NULL};
    return p;
}

void free_parser (OpParser p) {
    HashFree(p.tokens);
    HashFree(p.keys);
    HashFree(p.klens);
}

void add_token (
    OpParser    p,
    Str         name,       /* infix:moose */
    Str         relation,   /* <postfix:antler */
    Bool        nows,       /* no whitespace allowed before this */
    Bool        nullterm,   /* is this a null term? */
    Callback    parsed,     /* dynamic parsing */
    Extras      extras      /* extra stuff to return on match */
) {
}

OpMatch parse (OpParser p, Str in) {
}

int main () {
    OpParser p = new_parser();
    free_parser(p);
}
