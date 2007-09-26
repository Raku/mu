#include "../lib/KindaPerl6/Runtime/C/match.h"
#include "tokenc-rules.c"
int main() {
    dump_match(simple("simple",6,0));
    dump_match(foo("xfoobaz",7,1));
    dump_match(foo("xyc",3,0));
}
