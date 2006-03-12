use t::TestModuleCompile tests => 1;

filters {
    perl => 'eval',
    pm => 'compile_pm',
};

run_is pm => 'pmc';

sub compile_pm {
    Indented->pmc_process(shift);
}

__DATA__

=== Nested Compilers
--- perl
package Indented;
use Module::Compile -base;

sub compile {
    s/^(def \w+):(\n(?: +.*?\n)*)/$1 {$2}\n/g;
    PySubs->compile();
}

package PySubs;

sub compile {
    s/^def (\w+ \{)/sub $1/g;
}

package Y2K;
use Module::Compile -base;
sub compile { s/y/k/g }

--- pm
package Foo;
use strict;
use warnings;
# sample main script
drink();
use Indented;
def drink:
    print "Drink ";
    use Y2K;
    print "yool aid, ";
    no Y2K;
    print "yo!"

--- pmc
package Foo;
use strict;
use warnings;
# sample main script
drink();
sub drink {
    print "Drink ";
    print "kool aid, ";
    print "yo!"
}

