BEGIN { %ENV = () }

use strict;
use re::override-PCRE;

print "1..2\n";
if ("Hello, world" =~ /(?<=Hello|Hi), (world)/) {
    print "ok 1\n";
}

no re::override-PCRE;
if (eval q[ "Hello, world" =~ /(?<=Hello|Hi), (world)/ ]) {
    print "not ";
}
print "ok 2\n";
