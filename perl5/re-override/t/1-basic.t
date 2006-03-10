BEGIN { %ENV = () }

use strict;
use re::override-pcre;

print "1..1\n";
if ("Hello, world" =~ /(?<=Hello|Hi), (world)/) {
    print "ok 1\n";
}
