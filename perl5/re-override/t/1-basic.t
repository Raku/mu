BEGIN { %ENV = () }

use strict;
use re::override;

print "1..2\n";
if ("Hello, world" =~ /(?<=Hello|Hi), (world)/) {
    print "ok 1\n";
}

no re::override-pcre;
if (eval q[ "Hello, world" =~ /(?<=Hello|Hi), (world)/ ]) {
    print "not ";
}
print "ok 2\n";
