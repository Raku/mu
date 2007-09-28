#include "../lib/KindaPerl6/Runtime/C/match.h"
#include "tokenc-rules.c"
int main() {
    dump_match(simple("simple",0));
    dump_match(foo("xfoobaz",1));
    dump_match(foo("xyc",0));
    dump_match(outer("outer.inner",0));
}
