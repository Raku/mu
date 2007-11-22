#include "../lib/KindaPerl6/Runtime/C/match.h"
#include "tokenc-rules.c"
int main() {
    dump_match(simple("simple",0));
    dump_match(foo("xfoobaz",1));
    dump_match(foo("xyc",0));
    dump_match(outer("outer.inner",0));
}

/*


AUTHORS

The Pugs Team perl6-compiler@perl.org.

SEE ALSO

The Perl 6 homepage at http://dev.perl.org/perl6.

The Pugs homepage at http://pugscode.org/.

COPYRIGHT

Copyright 2007 by Flavio Soibelmann Glock and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See http://www.perl.com/perl/misc/Artistic.html

*/