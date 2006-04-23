#include <stdlib.h>
#include <string.h>
#include <optable.h>

#define REG_SCTABLE(s,v) \
    if (0 == strncmp(in, s, strlen(s))) { return v; }

ParseMode sctable (Str in) {
    REG_SCTABLE("term:",             PGE_SCTABLE_TERM)
    REG_SCTABLE("postfix:",          PGE_SCTABLE_POSTFIX)
    REG_SCTABLE("close:",            PGE_SCTABLE_CLOSE)
    REG_SCTABLE("prefix:",           PGE_SCTABLE_PREFIX)
    REG_SCTABLE("prelist:",          PGE_SCTABLE_PRELIST)
    REG_SCTABLE("infix:",            PGE_SCTABLE_INFIX)
    REG_SCTABLE("ternary:",          PGE_SCTABLE_TERNARY)
    REG_SCTABLE("postcircumfix:",    PGE_SCTABLE_POSTCIRCUMFIX)
    REG_SCTABLE("circumfix:",        PGE_SCTABLE_CIRCUMFIX)
    /* XXX: default return */
}

OpParser new_parser () {
    OpParser p = {HashNew(), HashNew(), HashNew(), HashNew()};
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

OpMatch parse (OpParser p, Str target, Str stop_token) {
}

int main () {
    OpParser p = new_parser();
    free_parser(p);
}
